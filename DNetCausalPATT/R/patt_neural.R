#' Train compliance model using neural networks
#'
#' @description
#' Train model using group exposed to treatment with compliance as binary
#' outcome variable and covariates.
#'
#' @param complier.formula formula for complier variable as outcome and
#' covariates (c ~ x)
#' @param exp.data `data.frame` for experimental data.
#' @param treat.var string for treatment variable.
#' @param algorithm string for algorithm for training neural networks.
#' Default set to the Resilient back propagation with weight backtracking
#' (rprop+). Other algorithms include backprop', rprop-', 'sag', or 'slr'
#' (see \code{neuralnet} package).
#' @param hidden.layer vector for specifying hidden layers and number of neurons.
#' @param ID string for identifier variable
#' @param stepmax maximum number of steps.
#'
#' @return trained complier model object
#' @export
#'
#' @examples
neuralnet_complier_mod<-function(complier.formula,
                            exp.data,
                            treat.var,
                            algorithm = "rprop+",
                            hidden.layer=c(4,2),
                            ID=NULL,
                            stepmax=1e+05){
  if (!is.null(ID)){
    id=ID
  }
  complier.formula <- as.formula(complier.formula)
  exp.data.vars <- na.omit(exp.data[,c(all.vars(complier.formula),treat.var)])
  neuralnetdataT <- exp.data.vars[which(exp.data.vars[,treat.var]==1),]
  compvar <- all.vars(complier.formula)[1]
  neuralnetdataT[[compvar]] <- as.factor(neuralnetdataT[[compvar]])
  neuralnet.complier.mod <- neuralnet::neuralnet(complier.formula,
                                            data=neuralnetdataT,
                                            algorithm = algorithm,
                                            hidden = hidden.layer,
                                            linear.output = FALSE,
                                            stepmax = stepmax)
  return(neuralnet.complier.mod)
}

#' Predicting Compliance from experimental data
#'
#' @description
#' Predicting Compliance from control group experimental data
#'
#' @param neuralnet.complier.mod results from \code{neuralnet_complier_mod}
#' @param exp.data `data.frame` of experimental data
#' @param treat.var string for treatment variable
#' @param compl.var string for compliance variable
#'
#' @return `data.frame` object with true compliers, predicted compliers in the
#' control group, and all compliers (actual + predicted).
#' @export
#'
#' @examples
neuralnet_predict<-function(neuralnet.complier.mod,
                            exp.data,treat.var,
                            compl.var){

  neuralnetpredict<-predict(neuralnet.complier.mod,exp.data)
  neuralnetpredict.max<-max.col(neuralnetpredict)

  neuralnet.compliers<-data.frame("treatment"=exp.data[,treat.var],
                             "real_complier"=exp.data[,compl.var],
                             "NC.pscore"=neuralnetpredict[,1],
                             "C.pscore"=neuralnetpredict[,2]
  )

  neuralnet.compliers$predicted_complier<-ifelse(neuralnet.compliers$treatment==0 &
                                              neuralnetpredict.max==2,1,0)
  neuralnet.compliers$compliers_all<-as.numeric(as.character(neuralnet.compliers$real_complier))+
    neuralnet.compliers$predicted_complier
  return(neuralnet.compliers)
}

#' Modeling Responses from experimental data Using Deep NN
#'
#' @description Model Responses from all compliers (actual + predicted)
#' in experimental data using neural network.
#'
#' @param response.formula formula for response variable and covariates (y ~ x)
#' @param exp.data `data.frame` of experimental data.
#' @param neuralnet.compliers `data.frame` of compliers (actual + predicted)
#' from \code{neuralnet_predict}.
#' @param compl.var string of compliance variable
#' @param algorithm neural network algorithm, default set to `"rprop+"`.
#' @param hidden.layer vector specifying hidden layers and number of neurons.
#' @param stepmax maximum number of steps for training model.
#'
#' @return trained response model object
#' @export
#'
#' @examples
neuralnet_response_model <- function(response.formula,
                                     exp.data,
                                     neuralnet.compliers,
                                     compl.var,
                                     algorithm = "rprop+",
                                     hidden.layer=c(4,2),
                                     stepmax=1e+05){

  variables<-all.vars(response.formula)
  responsevar<-variables[1]
  covariates<-variables[-1]

  .formula <- as.formula(paste0(paste0(responsevar," ~",compl.var," + "),
                                paste0(covariates,collapse = " + ")))

  exp.data <- exp.data[,all.vars(.formula)]
  exp.data[[responsevar]] <- as.factor(exp.data[[responsevar]])
  exp.compliers<-exp.data[which(neuralnet.compliers$compliers_all==1),]
  neuralnet.response.mod <- neuralnet::neuralnet(.formula,
                                            data=exp.compliers,
                                            hidden=hidden.layer,
                                            algorithm = algorithm,
                                            linear.output = FALSE,
                                            stepmax = stepmax)
  return(neuralnet.response.mod)
}


#' Assess Population Data counterfactuals
#'
#' @description
#' Create counterfactual datasets in the population for compliers and
#' noncompliers. Then predict potential outcomes using trained model from
#' \code{neuralnet_response_model}.
#'
#' @param pop.data population data.
#' @param neuralnet.response.mod trained model from.
#' \code{neuralnet_response_model}.
#' @param ID string for identifier variable.
#' @param cluster string for clustering variable (currently unused).
#'
#' @return `data.frame` of predicted outcomes of response variable from
#' counterfactuals.
#' @export
#'
#' @examples
neuralnet_pattc_counterfactuals <- function (pop.data,
                                       neuralnet.response.mod,
                                       ID=NULL,
                                       cluster=NULL){

  compl.var <- pop_prep$compl_var
  covariates <- all.vars(pop_prep$response_formula)[-1]
  popdata <- pop_prep$popdata
  popdata$c <- popdata[,compl.var]

  pop.tr.counterfactual <- cbind( 1, popdata[which(popdata$c==1),covariates])
  colnames(pop.tr.counterfactual) <- c(compl.var,covariates)
  pop.ctrl.counterfactual <- cbind(0,popdata[which(popdata$c==1),covariates])
  colnames(pop.ctrl.counterfactual)<-c(compl.var,covariates)

  Y.hat.0 <- predict(neuralnet.response.mod, pop.ctrl.counterfactual)
  Y.hat.1 <- predict(neuralnet.response.mod, pop.tr.counterfactual)

  neuralnetpredict.max.0<-max.col(Y.hat.0)-1
  neuralnetpredict.max.1<-max.col(Y.hat.1)-1

  if (!is.null(cluster)){
    clustervar <- pop.data[,cluster]
    Y.hats <- data.frame( Y_hat0 = neuralnetpredict.max.0,
                          Y_hat1 = neuralnetpredict.max.1,
                          cluster=clustervar)
  }
  else
  {Y.hats <- data.frame(Y_hat0 = neuralnetpredict.max.0,
                        Y_hat1 = neuralnetpredict.max.1)
  }
  return(Y.hats)

}

#' Estimate PATT_C using Deep NN
#'
#' @description
#' estimates the Population Average Treatment Effect
#' of the Treated from experimental data with noncompliers using Deep Neural
#' Networks.
#'
#' @param response.formula formula of response variable as outcome and
#' covariates (y ~ x)
#' @param exp.data `data.frame` of experimental data. Must include binary
#' treatment and compliance variables.
#' @param pop.data `data.frame` of population data. Must include binary
#' compliance variable
#' @param treat.var string for treatment variable.
#' @param compl.var string for compliance variable
#' @param compl.algorithm string for algorithim to train neural network for
#' compliance model. Default set to `"rprop+"`. See (`neuralnet` package for
#' available algorithms).
#' @param response.algorithm string for algorithim to train neural network for
#' response model. Default set to `"rprop+"`. See (`neuralnet` package for
#' available algorithms).
#' @param compl.hidden.layer vector for specifying hidden layers and number of
#' neurons in complier model.
#' @param response.hidden.layer vector for specifying hidden layers and number of
#' neurons in response model.
#' @param compl.stepmax maximum number of steps for complier model
#' @param response.stepmax maximum number of steps for response model
#' @param ID string for identifier variable
#' @param cluster string for cluster variable.
#' @param bootse logical for bootstrapped standard erros.
#' @param bootp logical for bootstrapped p values.
#' @param bootn logical for number of bootstraps.
#'
#' @return results of weighted t test as PATTC estimate.
#' @export
#'
#' @examples
patt_deep_nn <- function(response.formula,
                         exp.data,
                         pop.data,
                         treat.var,
                         compl.var,
                         compl.algorithm = "rprop+",
                         response.algorithm = "rprop+",
                         compl.hidden.layer = c(4,2),
                         response.hidden.layer = c(4,2),
                         compl.stepmax = 1e+05,
                         response.stepmax = 1e+05,
                         ID = NULL,
                         cluster = NULL,
                         bootse = FALSE,
                         bootp = FALSE,
                         bootn = 999)

{
  expdata<- expcall(response.formula,
                     treat.var = treat.var,
                     compl.var = compl.var,
                     data= exp.data, ID=ID)

  popdata<-popcall(response.formula,
                   compl.var = compl.var,
                   data= exp.data, ID=ID)
  covariates <- all.vars(response.formula)[-1]
  compl.formula<- paste0(compl.var," ~ ", paste0(covariates,collapse = " + "))
  compl.mod<-neuralnet_complier_mod(complier.formula = compl.formula,
                                    exp.data =expdata$expdata,
                                    treat.var = treat.var,
                                    stepmax = compl.stepmax)

  compliers<-neuralnet_predict(compl.mod,exp.data =expdata$expdata,
                               treat.var = "trt1",compl.var ="compl1" )

  response.mod <- neuralnet_response_model(response.formula = exp_prep$response_formula,
                                           compl.var,
                                           exp.data = exp_prep$expdata,
                                           neuralnet.compliers = compliers,
                                           stepmax = response.stepmax)

  counterfactuals<-neuralnet_pattc_counterfactuals(pop_prep,nnet_response_model)

  pattc<-WtC(x=counterfactuals$Y_hat1,
             y=counterfactuals$Y_hat0,
             bootse=bootse,
             bootp = bootp,
             bootn = bootn,
             samedata=FALSE,
             equivalence = FALSE)

  return(pattc)
}

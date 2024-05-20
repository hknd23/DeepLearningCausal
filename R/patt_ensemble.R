#' PATT_C SL Ensemble
#'
#' @description
#' \code{PATT_C_SL_Ensemble} estimates the Population Average Treatment Effect of the Treated from experimental data with noncompliers
#' using the super learner ensemble that includes extreme gradient boosting, glmnet (elastic net regression), random forest and neural nets.
#' @param data list object of data
#' @param ID
#' @param SL.library. Employs extreme gradient boosting, elastic net regression, random forest, and neural nets.
#'
#' @return
#' @export
#'
#' @examples
complier_mod <- function(data,ID=NULL,SL.library=NULL) {
  if (!is.null(ID)){
    id=ID
  }
  if (is.null(SL.library))
  {
    SL.library.class <- define.SL.class.library()
  }
  expdata <- data$expdata
  covariates <- all.vars(data$response_formula)[-1]
  compl.var <- data$compl_var
  treat.var <- data$treat_var

  Ycompl<-expdata[which(expdata[,treat.var]==1),compl.var]
  Xcompl<-expdata[which(expdata[,treat.var]==1),covariates]

  complier.mod <- SuperLearner::SuperLearner(Y=Ycompl,
                               X=Xcompl,
                               SL.library=SL.library.class,
                               id=ID,
                               family="binomial")
  return(complier.mod)
}

#' complier_predict
#' @description
#' Predict Compliance from experimental data
#' @param complier.mod
#' @param exp.data
#'
#' @return
#' @export
#'
#' @examples
complier_predict <- function(complier.mod,exp.data) {
  message("1")
  covdata <- exp.data$expdata
  message("2")
  covariates.names <- all.vars(expdata$response_formula)[-1]
  message("3")
  covariates <- covdata[,covariates.names]
  message("4")
  treat.var<-exp.data$treat_var
  message("5")
  compl.var<-exp.data$compl_var
  message("6")
  C.pscore <- predict(complier.mod, covariates, onlySL=TRUE)
  message("7")
  rct.compliers <- data.frame("treatment"=covdata[,treat.var],
                              "real_complier"=covdata[,compl.var],
                              "C.pscore"=C.pscore$pred,
                              row.names = rownames(covdata))
  message("8")
  pred.compliers <- ROCR::prediction(rct.compliers$C.pscore[rct.compliers$
                                                          treatment==1],
                                 rct.compliers$real_complier[rct.compliers$
                                                               treatment==1])
  cost.perf <- ROCR::performance(pred.compliers, "cost")
  opt.cut <- pred.compliers@cutoffs[[1]][which.min(cost.perf@y.values[[1]])]

  rct.compliers$predicted_complier<-ifelse(rct.compliers$treatment==0 &
                                         rct.compliers$C.pscore>opt.cut,1,0)
  rct.compliers$compliers_all<-rct.compliers$real_complier+
  rct.compliers$predicted_complier
  return(rct.compliers)
  }

#' Model Responses from experimental data using SL ensemble
#' @description
#'
#' @param exp.data
#' @param exp.compliers
#' @param family
#' @param ID
#' @param SL.library
#'
#' @return
#' @export
#'
#' @examples
response_model<-function(exp.data,
                         exp.compliers,
                         family="binomial",
                         ID=NULL,
                         SL.library=NULL){

  if (family=="binomial" & is.null(SL.library) )  {
    SL.library <- define.SL.class.library()
  }
  response.var <- all.vars(exp.data$response_formula)[1]
  covariates <- all.vars(exp.data$response_formula)[-1]

  treat.var <- exp.data$treat_var
  compl.var <- exp.data$compl_var
  response.data <- exp.data$expdata

  Y.exp.response <- response.data[,response.var][which(exp.compliers$compliers_all==1)]
  X.exp.response <- data.frame("complier"=response.data[,compl.var][which(exp.compliers$compliers_all==1)],
                               response.data[which(exp.compliers$complier==1),covariates])
  colnames(X.exp.response)<-c(compl.var,covariates)

  response.mod <- SuperLearner::SuperLearner(Y=Y.exp.response,
                               X=X.exp.response,
                               SL.library=SL.library,
                               family=family,
                               id=ID)
  return(response.mod)
}


#' Assess Population Data counterfactuals
#' @description
#'
#' @param pop.data
#' @param response.mod
#' @param id
#' @param cluster
#' @param cut.point
#'
#' @return
#' @export
#'
#' @examples
pattc_counterfactuals<- function (pop.data,
                                  response.mod,
                                  id=NULL,
                                  cluster=NULL,
                                  cut.point=.5){
  compl.var<-pop.data$compl_var
  covariates<-all.vars(pop.data$response_formula)[-1]

  popdata<-pop.data$popdata
  popdata$c<-popdata[,compl.var]


  pop.tr.counterfactual <- cbind( 1, popdata[which(popdata$c==1),covariates])
  colnames(pop.tr.counterfactual)<-c(compl.var,covariates)
  pop.ctrl.counterfactual <- cbind(0,popdata[which(popdata$c==1),covariates])
  colnames(pop.ctrl.counterfactual)<-c(compl.var,covariates)

  Y.pred.1 <- predict(response.mod, pop.tr.counterfactual, onlySL = T)$pred
  Y.pred.0 <- predict(response.mod, pop.ctrl.counterfactual, onlySL = T)$pred

  if (potential.outcome) {
    Y.hat.1 <- ifelse(Y.pred.1>cut.point,1,0)
    Y.hat.0 <- ifelse(Y.pred.0>cut.point,1,0)
  } else if (!potential.outcome) {
    Y.hat.1 <- Y.pred.1
    Y.hat.0 <- Y.pred.0
  }

  if (!is.null(cluster)){
    clustervar <- pop.data[,cluster]
    Y.hats <- data.frame(Y_hat0 = Y.hat.0, Y_hat1 = Y.hat.1, cluster=clustervar)
  } else  {
    Y.hats <- data.frame(Y_hat0 = Y.hat.0, Y_hat1 = Y.hat.1)
  }
  return(Y.hats)
}

#' PATT_C SL Ensemble
#'
#' @description
#' \code{PATT_C_SL_Ensemble} estimates the Population Average Treatment Effect
#' of the Treated from experimental data with noncompliers
#' using the super learner ensemble that includes extreme gradient boosting,
#' glmnet (elastic net regression), random forest and neural nets.
#'
#' @param response.formula formula for the effects of covariates on outcome
#' variable (y ~ x).
#' @param exp.data `data.frame` object for experimental data. Must include
#' binary treatment and compliance variable.
#' @param pop.data `data.frame` object for population data. Must include
#' binary compliance variable.
#' @param treat.var string for binary treatment variable.
#' @param compl.var string for binary compliance variable.
#' @param createSL logical. If `TRUE` will call on \code{create.SL} to create
#' SL wrappers.
#' @param ID string for name of identifier.
#' @param cluster string for name of cluster variable.
#' @param bootse
#' @param bootp
#' @param bootn
#' @param samedata
#' @param equivalence
#'
#' @return
#' @export
#'
#' @examples
patt_ensemble <- function(response.formula,
                        exp.data,
                        pop.data,
                        treat.var,
                        compl.var,
                        createSL=TRUE,
                        ID=NULL,
                        cluster=NULL,
                        bootse=FALSE,
                        bootp = FALSE,
                        bootn = 999,
                        samedata=FALSE,
                        equivalence = FALSE)

{
  if (createSL) {
    create.SL()
  }

  expdata<- expcall(response.formula,
                    treat.var = treat.var,
                    compl.var = compl.var,
                    data= exp.data, ID=ID)

  popdata<-popcall(response.formula,
                   compl.var = compl.var,
                   data= exp.data, ID=ID)
  covariates <- all.vars(response.formula)[-1]
  compl.formula<- paste0(compl.var," ~ ",paste0(covariates,collapse = " + "))
  message("Training complier model")
  compl.mod<-complier_mod(expdata,ID=NULL,SL.library=NULL)
  message("predicting")
#CHECKHERE
  compliers<-complier_predict(complier.mod=compl.mod,
                              exp.data =expdata)
  message("Training response model")
  response.mod <-  response_model(exp.data=expdata,
                                  exp.compliers=compliers,
                                  family="binomial",
                                  ID=NULL,
                                  SL.library=NULL)

  message("Estimating pattc")
  counterfactuals<-pattc_counterfactuals(pop.data=popdata,
                                         response.mod=response.mod,
                                         id=NULL,
                                         cluster=NULL,
                                         cut.point=.5)

  pattc<-WtC(x=counterfactuals$Y_hat1,
             y=counterfactuals$Y_hat0,
             bootse=bootse,
             bootp = bootp,
             bootn = bootn,
             samedata=samedata,
             equivalence = equivalence)

  return(pattc)
}





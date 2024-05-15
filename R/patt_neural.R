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
  complier.formula<-as.formula(complier.formula)
  exp.data.vars<-na.omit(exp.data[,c(all.vars(complier.formula),treat.var)])
  neuralnetdataT<-exp.data.vars[which(exp.data.vars[,treat.var]==1),]
  compvar<-all.vars(complier.formula)[1]
  neuralnetdataT[[compvar]]<-as.factor(neuralnetdataT[[compvar]])
  neuralnet.complier.mod <- neuralnet::neuralnet(complier.formula,
                                            data=neuralnetdataT,
                                            algorithm = algorithm,
                                            hidden=hidden.layer,
                                            linear.output = FALSE,
                                            stepmax = stepmax)
  return(neuralnet.complier.mod)
}

neuralnet_predict<-function(neuralnet.complier.mod,exp.data,treat.var,compl.var){

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


  exp.data<-exp.data[,all.vars(.formula)]
  exp.data[[responsevar]]<-as.factor(exp.data[[responsevar]])
  exp.compliers<-exp.data[which(neuralnet.compliers$compliers_all==1),]
  neuralnet.response.mod <- neuralnet::neuralnet(.formula,
                                            data=exp.compliers,
                                            hidden=hidden.layer,
                                            algorithm = algorithm,
                                            linear.output = FALSE,
                                            stepmax = stepmax)
  return(neuralnet.response.mod)
}


neuralnet_pattc_counterfactuals<- function (pop.data,
                                       neuralnet.response.mod,
                                       ID=NULL,
                                       cluster=NULL){
  compl.var<-pop_prep$compl_var
  covariates<-all.vars(pop_prep$response_formula)[-1]
  popdata<-pop_prep$popdata
  popdata$c<-popdata[,compl.var]
  pop.tr.counterfactual <- cbind( 1, popdata[which(popdata$c==1),covariates])
  colnames(pop.tr.counterfactual)<-c(compl.var,covariates)
  pop.ctrl.counterfactual <- cbind(0,popdata[which(popdata$c==1),covariates])
  colnames(pop.ctrl.counterfactual)<-c(compl.var,covariates)

  Y.hat.0 <- predict(neuralnet.response.mod, pop.ctrl.counterfactual)
  Y.hat.1 <- predict(neuralnet.response.mod, pop.tr.counterfactual)

  neuralnetpredict.max.0<-max.col(Y.hat.0)-1
  neuralnetpredict.max.1<-max.col(Y.hat.1)-1

  if (!is.null(cluster)){
    clustervar <- pop.data[,cluster]
    Y.hats <- data.frame( Y_hat0 = neuralnetpredict.max.0, Y_hat1 = neuralnetpredict.max.1,
                          cluster=clustervar)
  }
  else
  {Y.hats <- data.frame(Y_hat0 = neuralnetpredict.max.0, Y_hat1 = neuralnetpredict.max.1)
  }
  return(Y.hats)

}

patt_neural <- function(response.formula,
                        exp.data,
                        pop.data,
                        treat.var,
                        compl.var,
                        compl.algorithm = "rprop+",
                        response.algorithm = "rprop+",
                        compl.hidden.layer=c(4,2),
                        response.hidden.layer=c(4,2),
                        compl.stepmax=1e+05,
                        response.stepmax=1e+05,
                        ID=NULL,
                        cluster=NULL,
                        bootse=FALSE,
                        bootp = FALSE,
                        bootn = 999,
                        samedata=FALSE,
                        equivalence = FALSE)

{
  expdata<- expcall(response.formula,
                     treat.var = treat.var,
                     compl.var = compl.var,
                     data= exp.data, ID=ID)

  popdata<-popcall(response.formula,
                   compl.var = compl.var,
                   data= exp.data, ID=ID)
  covariates <- all.vars(response.formula)[-1]
  compl.formula<- paste0(compl.var," ~ ",paste0(covariates,collapse = " + "))
  compl.mod<-neuralnet_complier_mod(complier.formula = compl.formula,
                                    exp.data =expdata$expdata,
                                    treat.var = treat.var,
                                    stepmax = compl.stepmax)

  compliers<-neuralnet_predict(compl.mod,exp.data =expdata$expdata,
                               treat.var = "trt1",compl.var ="compl1" )

  response.mod <- neuralnet_response_model(response.formula=exp_prep$response_formula,
                                           compl.var,
                                           exp.data=exp_prep$expdata,
                                           neuralnet.compliers=compliers,
                                           stepmax = response.stepmax)

  counterfactuals<-neuralnet_pattc_counterfactuals(pop_prep,nnet_response_model)

  pattc<-WtC(x=counterfactuals$Y_hat1,
             y=counterfactuals$Y_hat0,
             bootse=bootse,
             bootp = bootp,
             bootn = bootn,
             samedata=samedata,
             equivalence = equivalence)

  return(pattc)
}

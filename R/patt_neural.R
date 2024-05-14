neuralnet_complier_mod<-function(complier.formula,
                            expdata,
                            treat.var,
                            algorithm = "rprop+",
                            hidden.layer=c(4,2),
                            ID=NULL,
                            stepmax=1e+05){
  if (!is.null(ID)){
    id=ID
  }
  complier.formula<-as.formula(complier.formula)
  expdata.vars<-na.omit(expdata[,c(all.vars(complier.formula),treat.var)])
  neuralnetdataT<-expdata.vars[which(expdata.vars[,treat.var]==1),]
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

neuralnet_predict<-function(neuralnet.complier.mod,expdata,treat.var,compl.var){

  neuralnetpredict<-predict(neuralnet.complier.mod,expdata)
  neuralnetpredict.max<-max.col(neuralnetpredict)

  neuralnet.compliers<-data.frame("treatment"=expdata[,treat.var],
                             "real_complier"=expdata[,compl.var],
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


  expdata<-exp.data[,all.vars(.formula)]
  expdata[[responsevar]]<-as.factor(expdata[[responsevar]])
  exp.compliers<-expdata[which(neuralnet.compliers$compliers_all==1),]
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
                                       id=NULL,
                                       cluster=NULL){

  pop.tr.counterfactual <- cbind("compl1" = 1,
                                 pop.data$Xpop[which(pop.data$Cpop==1),])
  pop.ctrl.counterfactual <- cbind("compl1" = 0,
                                   pop.data$Xpop[which(pop.data$Cpop==1),])

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
  {Y.hats <- data.frame(Y_hat0 = neuralnetpredict.max.0, Y_hat1 = neuralnetpredict.max.1,)
  }
  return(Y.hats)

}



nnet_complier_mod<-function(complier.formula,
                            expdata,
                            treat.var,
                            ID=NULL,
                            stepmax=1e+05){
  if (!is.null(ID)){
    id=ID
  }
  complier.formula<-as.formula(complier.formula)
  nnetdataT<-na.omit(expdata[which(expdata[,treat.var]==1),])
  compvar<-all.vars(complier.formula)[1]
  nnetdataT[[compvar]]<-as.factor(nnetdataT[[compvar]])
  nnet.complier.mod <- neuralnet::neuralnet(complier.formula,
                                    data=nnetdataT,
                                    hidden=c(4,2),
                                    linear.output = FALSE,
                                    stepmax = stepmax)
  return(nnet.complier.mod)
}

nnet_predict<-function(nnet.complier.mod,expdata){

  nnetpredict<-predict(nnet.complier.mod,expdata)
  nnetpredict.max<-max.col(nnetpredict)

  nnet.compliers<-data.frame("treatment"=nnetdata$trt1,
                             "real_complier"=nnetdata$compl,
                             "NC.pscore"=nnetpredict[,1],
                             "C.pscore"=nnetpredict[,2]
  )

  nnet.compliers$predicted_complier<-ifelse(nnet.compliers$treatment==0 &
                                              nnetpredict.max==2,1,0)
  nnet.compliers$compliers_all<-as.numeric(as.character(nnet.compliers$real_complier))+
    nnet.compliers$predicted_complier
  return(nnet.compliers)
}

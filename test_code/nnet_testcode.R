#test code for neural network models
sapply(paste0("R/",list.files("R/")), source)
library(nnet)
library(neuralnet)
library(keras)
library(tensorflow)
library(readr)
library(tidyverse)
library(neuralnet)
source("R/nnet_complier.R")
expdata <- read_csv("data/expdata0502.csv")
popdata <- read_csv("data/popdata0502.csv")


exp_prep<- expcall(outcome ~ trt1,
                   ~age + male + income + education +
                     employed + married + Hindu + job_worry,
                   ~compl1,
                   data= expdata, ID="l")

pop_prep<-popcall(outcome1~ age + male +
                    income + education +
                    employed + married +
                    Hindu + job_worry,
                  ~compl1,data=popdata,
                  cluster = "year")


expdata<-data.frame("compl"=as.matrix(exp_prep$Cexp),
                     exp_prep$Xexp,treat=exp_prep$Texp,
                     outcome=exp_prep$Yexp)

treat.var="trt1"
compl.formula=  compl1~age + male +
  income + education +
  employed + married +
  Hindu + job_worry




complier.formula=compl.formula

nnet.compl.mod<-nnet_complier_mod(compl.formula,expdata,
                                  treat.var = "trt1",
                                  stepmax = 1e+07)

nnetcompliers<-nnet_predict(nnet.compl.mod,expdata,
                            treat.var = "trt1",compl.var ="compl1" )

expdata<-nnetdata
nnet.complier.mod<-nnetcompliers
response.formula=outcome~age + male +
  income + education +
  employed + married +
  Hindu + job_worry
stepmax=1e+06
nnet.compliers<-nnet.complier.mod
compl.formula=~. + compl1
nnet_response_model<-function(response.formula,
                         exp.data,
                         nnet.compliers,
                         compl.formula,
                         stepmax=1e+05){

  .formula<-update.formula(response.formula,compl.formula)
  variables<-all.vars(.formula)
  responsevar<-variables[1]
  expdata<-exp.data[,variables]
  expdata[[responsevar]]<-as.factor(expdata[[responsevar]])
  exp.compliers<-expdata[which(nnet.compliers$compliers_all==1),]
  nnet.response.mod <- neuralnet::neuralnet(.formula,
                                            data=exp.compliers,
                                            hidden=c(4,2),
                                            linear.output = FALSE,
                                            stepmax = stepmax)
  return(nnet.response.mod)
}


nnet_pattc_counterfactuals<- function (pop.data,nnet.response.mod,id=NULL,cluster=NULL){

  pop.tr.counterfactual <- cbind("compl1" = 1,
                                 pop.data$Xpop[which(pop.data$Cpop==1),])
  pop.ctrl.counterfactual <- cbind("compl1" = 0,
                                   pop.data$Xpop[which(pop.data$Cpop==1),])
  Y.hat.1 <- predict(nnet.response.mod, pop.tr.counterfactual)
  nnetpredict.max.1<-max.col(Y.hat.1)
  Y.hat.0 <- predict(nnet.response.mod, pop.ctrl.counterfactual)
  nnetpredict.max.0<-max.col(Y.hat.1)
  if (!is.null(cluster)){
    clustervar <- pop.data[,cluster]
    Y.hats <- data.frame(Y_hat1 = Y.hat.1, Y_hat0 = Y.hat.0,cluster=clustervar)
  }
  else
  {Y.hats <- data.frame(Y_hat1 = Y.hat.1, Y_hat0 = Y.hat.0)
  }
  return(Y.hats)

}


nnet.response.mod <- nnet_response_model(response.formula=response.formula,
                                         compl.formula=~. + compl1,
                                         exp.data=exp.data,
                                         nnet.compliers=nnetcompliers,stepmax = 1e+08)
##continue work here##continue work here##continue work here##continue work here
##continue work here##continue work here##continue work here##continue work here

nnet_pattc_counterfactuals(pop_prep,nnet_response_model)

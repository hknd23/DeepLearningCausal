#test code for neural network models

library(nnet)
library(neuralnet)
library(keras)
library(tensorflow)
library(readr)
library(tidyverse)
library(neuralnet)

expdata <- read_csv("data/expdata0502.csv")
popdata <- read_csv("data/popdata0502.csv")

sapply(paste0("R/",list.files("R/")), source)

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

exp_prep$Cexp<-as.factor(exp_prep$Cexp)

nnetdata<-data.frame(compl=exp_prep$Cexp,
                     exp_prep$Xexp,treat=exp_prep$Texp)
nnetdataT<-nnetdata[which(nnetdata$trt1==1),]
nnetmodel <- neuralnet(
  compl~age + male +
    income + education +
    employed + married +
    Hindu + job_worry,
  data=nnetdataT,
  hidden=c(4,2),
  linear.output = FALSE,stepmax = 1e+07
)
nnetpredict<-predict(nnetmodel,nnetdata)

#nnetpredict<-as.data.frame(predict(nnetmodel,nnetdata))
nnetpredict.max<-max.col(nnetpredict)

nnetdata$predicted_compl<-nnetpredict.max-1

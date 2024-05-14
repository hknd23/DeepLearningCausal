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


exp_prep<- expcall(outcome ~ age + male + income + education +
                     employed + married + Hindu + job_worry,
                   treat.var = "trt1",
                   compl.var = "compl1",
                   data= expdata, ID="l")

pop_prep<-popcall(outcome1~ age + male +
                    income + education +
                    employed + married +
                    Hindu + job_worry,
                  compl.var = "compl1",
                  data=popdata,
                  cluster = "year")



nnet.compl.mod<-nnet_complier_mod(compl.formula,expdata,
                                  treat.var = "trt1",
                                  stepmax = 1e+07)

nnetcompliers<-nnet_predict(nnet.compl.mod,expdata,
                            treat.var = "trt1",compl.var ="compl1" )

exp.data<-expdata
nnet.complier.mod<-nnetcompliers
response.formula=outcome~age + male +
  income + education +
  employed + married +
  Hindu + job_worry
stepmax=1e+06
nnet.compliers<-nnet.complier.mod
compl.formula=~. + compl1



nnet.response.mod <- nnet_response_model(response.formula=response.formula,
                                         compl.var,
                                         exp.data=exp.data,
                                         nnet.compliers=nnetcompliers,stepmax = 1e+08)
##continue work here##continue work here##continue work here##continue work here
##continue work here##continue work here##continue work here##continue work here

nnet_pattc_counterfactuals(pop_prep,nnet_response_model)

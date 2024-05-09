#https://github.com/xnie/rlearner/blob/master/R/sboost.R
#https://github.com/QuantLet/Meta_learner-for-Causal-ML/blob/main/S-learner/S-learner.R

vec.pac= c("SuperLearner", "gbm", "glmnet","ranger","nnet","caret")
source("R/meta_learners.R")


lapply(vec.pac, require, character.only = TRUE)

expdata <- read_csv("data/expdata0502.csv")
popdata <- read_csv("data/popdata0502.csv")
popdata_nonegs<-popdata %>% naniar::replace_with_na_all(condition = ~.x<0)

exp_prep<- expcall(outcome ~ trt1,
                   ~age + male + income + education +
                     employed + married + Hindu + job_worry,
                   ~compl1,
                   data= expdata, ID="l")

pop_prep<-popcall(outcome1~ age + male +
                    income + education +
                    employed + married +
                    Hindu + job_worry,
                  ~compl1,data=popdata_nonegs,
                  cluster = "year")

#Learner Library:
learners <- c( "SL.glmnet","SL.xgboost", "SL.ranger","SL.nnet")

#CV Control for the SuperLearner
control <- SuperLearner::SuperLearner.CV.control(V=5)

exp.frame<-data.frame(outcome=exp_prep$Yexp,
                      exp_prep$Texp,exp_prep$Xexp)
data=exp.frame
cov.formula =outcome  ~ age + male +
  income + education +
  employed + married +
  Hindu + job_worry
learners=learners


expS <- meta_learner(cov.formula = outcome  ~ age + male +
                  income + education +
                  employed + married +
                  Hindu + job_worry,
                data = expdata,
                control = control,
                meta.learner.type="S.Learner",
                treat.var = "trt1")



expT <- meta_learner(cov.formula = outcome  ~ age + male +
                       income + education +
                       employed + married +
                       Hindu + job_worry,
                     data = expdata,
                     control = control,
                     meta.learner.type="T.Learner",
                     treat.var = "trt1")



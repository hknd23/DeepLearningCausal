#https://stackoverflow.com/questions/72985863/how-do-you-install-r-packages-from-a-private-git-repo-with-a-pat
#https://cran.r-project.org/web/packages/submission_checklist.html
#source all functions
sapply(paste0("R/",list.files("R/")), source)

#import datasets
IND_exp_data <- read.csv("test_code/IND_exp_data.csv")
IND_pop_data <- read.csv("test_code/IND_pop_data.csv")

#IND_exp_ex <- IND_exp_data[sample(nrow(IND_exp_data), nrow(IND_exp_data)/2),]
#IND_pop_ex <- IND_pop_data[which(IND_pop_data$year %in% c(2022)),]
#IND_pop_ex2<-IND_pop_ex[sample(nrow(IND_pop_ex), nrow(IND_pop_ex)/2),]
expdata <- IND_exp_data
popdata <- IND_pop_data

#popdata$trt1 <- popdata$exp1_trt
#popdata$outcome <- popdata$outcome1



library(SuperLearner)

#########
#Example Code for ensemble/SL pattc
#######
#####
#Example code for neural network pattc
#####
set.seed(123456)
pattc_ensemble <- patt_ensemble(response.formula = outcome ~ age +
                                  income + education +
                                  employed + job_worry,
                            exp.data = expdata,
                            pop.data = popdata,
                            treat.var="trt1",
                            compl.var="compl1",
                            createSL=TRUE,
                            SL.library = c("SL.gbm.adaboost",
                                           "SL.gbm.bernoulli",
                                           "SL.glmnet"),
                            ID=NULL,
                            cluster=NULL,
                            bootse=FALSE,
                            bootp = FALSE,
                            bootn = 999)


######
#Example code for neural network pattc
#####
set.seed(123456)
pattc_neural <- patt_deep_nn(response.formula = outcome ~ age + male +
                              income + education +
                              employed + married +
                              Hindu + job_worry,
                            exp.data = expdata,
                            pop.data = popdata,
                            treat.var = "trt1",
                            compl.var = "compl1",
                            compl.algorithm = "rprop+",
                            response.algorithm = "rprop+",
                            compl.hidden.layer=c(4,2),
                            response.hidden.layer=c(4,2),
                            compl.stepmax = 1e+09,
                            response.stepmax = 1e+09,
                            ID=NULL,
                            cluster=NULL,
                            bootse=FALSE,
                            bootp = FALSE,
                            bootn = 999)


#####
#Example code for ensemble based S and T Learners
#####
control <- SuperLearner::SuperLearner.CV.control(V=5)

slearner <- ST_learner_ensemble(cov.formula = outcome ~ age +
                                  income  +
                                  employed  + job_worry,
                               data = expdata,
                                treat.var = "trt1",
                                meta.learner.type = "S.Learner",
                           learners = c("SL.glmnet","SL.xgboost"),
                                  nfolds = 5)

tlearner <- ST_learner_ensemble(cov.formula = outcome ~ age +
                                  income  +
                                  employed  + job_worry,
                                data = expdata,
                                treat.var = "trt1",
                                meta.learner.type = "T.Learner",
                                learners = c("SL.glmnet","SL.xgboost"),
                                nfolds = 5)

#####
#Example code for neural based S and T Learners
#####

slearner_nn <- ST_learner_DeepNN(cov.formula = outcome ~ age +
                                income  +
                                employed  + job_worry,
                             data = expdata,
                             treat.var = "trt1",
                             meta.learner.type="S.Learner",
                             stepmax=1e+9,
                             nfolds=5,
                             algorithm = "rprop+",
                             hidden.layer=c(4,2),
                             linear.output = FALSE)

tlearner_nn <- ST_learner_DeepNN(cov.formula = outcome ~ age +
                                income  +
                                employed  + job_worry,
                              data = expdata,
                              treat.var = "trt1",
                              meta.learner.type="T.Learner",
                              stepmax = 1e+9,
                              nfolds = 5,
                              algorithm = "rprop+",
                              hidden.layer=c(2,1),
                              linear.output = FALSE)

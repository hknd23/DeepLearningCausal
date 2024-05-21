
#source all functions
sapply(paste0("R/",list.files("R/")), source)

#import datasets
expdata <- read.csv("data/expdata0502.csv")
popdata <- read.csv("data/popdata0502.csv")

eprep<-expcall(response.formula = compl1~ age + male +
                 income + education +
                 employed + married +
                 Hindu + job_worry,
               data=expdata,treat.var = "trt1",compl.var = "compl1")

popprep<-popcall(response.formula = compl1~ age + male +
                 income + education +
                 employed + married +
                 Hindu + job_worry,
               data=popdata,compl.var = "compl1")



nnet.compl.mod<-neuralnet_complier_mod(compl1~ age + male +
                                    income + education +
                                    employed + married +
                                    Hindu + job_worry,
                                  exp_prep$expdata,
                                  treat.var = "trt1",
                                  stepmax = 1e+08)

nnetcompliers<-neuralnet_predict(nnet.compl.mod,expdata,
                            treat.var = "trt1",compl.var ="compl1" )



nnet_response_model <- neuralnet_response_model(response.formula=exp_prep$response_formula,
                                         compl.var="compl1",
                                         exp.data=exp_prep$expdata,
                                         neuralnet.compliers=nnetcompliers,stepmax = 1e+08)


neuralnet_pattc_counterfactuals(pop_prep,nnet_response_model)


response.formula=outcome ~ age + male + income + education +
  employed + married + Hindu + job_worry
exp.data=expdata
pop.data=popdata
treat.var="trt1"
compl.var="compl1"
compl.algorithm = "rprop+"
response.algorithm = "rprop+"
compl.hidden.layer=c(4,2)
response.hidden.layer=c(4,2)
compl.stepmax=1e+08
response.stepmax=1e+08
ID=NULL
cluster=NULL
bootse=FALSE
bootp = FALSE
bootn = 999
samedata=FALSE
equivalence = FALSE


#########
#Example Code for ensemble/SL pattc
#######
#####
#Example code for neural network pattc
#####
pattc_ensemble <- patt_ensemble(response.formula=outcome ~ age + male +
                              income + education +
                              employed + married +
                              Hindu + job_worry,
                            exp.data=expdata,
                            pop.data=popdata,
                            treat.var="trt1",
                            compl.var="compl1",
                            createSL=TRUE,
                            ID=NULL,
                            cluster=NULL,
                            bootse=FALSE,
                            bootp = FALSE,
                            bootn = 999,
                            samedata=FALSE,
                            equivalence = FALSE)



######
#Example code for neural network pattc
#####
pattc_neural <- patt_neural(response.formula=outcome ~ age + male +
                              income + education +
                              employed + married +
                              Hindu + job_worry,
                            exp.data=expdata,
                            pop.data=popdata,
                            treat.var="trt1",
                            compl.var="compl1",
                            compl.algorithm = "rprop+",
                            response.algorithm = "rprop+",
                            compl.hidden.layer=c(4,2),
                            response.hidden.layer=c(4,2),
                            compl.stepmax=1e+08,
                            response.stepmax=1e+08,
                            ID=NULL,
                            cluster=NULL,
                            bootse=FALSE,
                            bootp = FALSE,
                            bootn = 999,
                            samedata=FALSE,
                            equivalence = FALSE)


#####
#Example code for ensemble based S and T Learners
#####
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





#####
#Example code for neural based S and T Learners
#####
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


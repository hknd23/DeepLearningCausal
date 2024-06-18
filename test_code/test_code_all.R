#https://stackoverflow.com/questions/72985863/how-do-you-install-r-packages-from-a-private-git-repo-with-a-pat
#https://cran.r-project.org/web/packages/submission_checklist.html
#load all functions
devtools::load_all()
#import datasets
#IND_exp_data <- read.csv("test_code/IND_exp_data.csv")
#IND_pop_data <- read.csv("test_code/IND_pop_data.csv")

#IND_exp_ex <- IND_exp_data[sample(nrow(IND_exp_data), nrow(IND_exp_data)/2),]
#IND_pop_ex <- IND_pop_data[which(IND_pop_data$year %in% c(2022)),]
#IND_pop_ex2<-IND_pop_ex[sample(nrow(IND_pop_ex), nrow(IND_pop_ex)/2),]
#expdata <- IND_exp_data
#popdata <- IND_pop_data

#popdata$trt1 <- popdata$exp1_trt
#popdata$outcome <- popdata$outcome1



library(SuperLearner)

#########
#Example Code for ensemble/SL pattc
#######
data(exp_data) #experimental data
data(pop_data) #population data
data(exp_data_full) #experimental data
data(pop_data_full) #population data
 set.seed(123456)
 #specify models and estimate PATTC
 pattc_ensemble <- pattc_ensemble(response.formula = support_war ~ age + income +
                                 education + employed + job_loss,
                                 exp.data = exp_data_full,
                                 pop.data = pop_data,
                                 treat.var = "strong_leader",
                                 compl.var = "compliance",
                                 createSL = TRUE,
                                 SL.library = c(NULL),
                                 ID = NULL,
                                 cluster = NULL,
                                 bootse = FALSE,
                                 bootp = FALSE,
                                 bootn = 999)


#####
#Example code for neural network pattc
#####
 # load datasets
 data(exp_data) #experimental data
 data(pop_data) #population data
 # specify models and estimate PATTC
 set.seed(123456)
 pattc_neural <- pattc_deepneural(response.formula = support_war ~ age + female +
                                income + education +  employed + married +
                                hindu + job_loss,
                                exp.data = exp_data,
                                pop.data = pop_data,
                                treat.var = "strong_leader",
                                compl.var = "compliance",
                                compl.algorithm = "rprop+",
                                response.algorithm = "rprop+",
                                compl.hidden.layer = c(4,2),
                                response.hidden.layer = c(4,2),
                                compl.stepmax = 1e+09,
                                response.stepmax = 1e+09,
                                ID = NULL,
                                cluster = NULL,
                                bootse = FALSE,
                                bootp = FALSE,
                                bootn = 999)

 summary(pattc_neural)


#####
#Example code for ensemble based S and T Learners
#####
  # load dataset
  data(exp_data)
  #load SuperLearner package
  library(SuperLearner)
  # estimate CATEs with S Learner
  control <- SuperLearner::SuperLearner.CV.control(V=5)
  # estimate CATEs with S Learner
  set.seed(123456)
  slearner <- metalearner_ensemble(cov.formula = support_war ~ age +
                                    income + employed + job_loss,
                                  data = exp_data,
                                  treat.var = "strong_leader",
                                  meta.learner.type = "S.Learner",
                                  learners = c("SL.glmnet","SL.xgboost"),
                                  nfolds = 5)

  # estimate CATEs with T Learner

  set.seed(123456)
  tlearner <- metalearner_ensemble(cov.formula = support_war ~ age + income +
                                    employed  + job_loss,
                                    data = exp_data,
                                    treat.var = "strong_leader",
                                    meta.learner.type = "T.Learner",
                                    learners = c("SL.glmnet","SL.xgboost"),
                                    nfolds = 5)

#####
#Example code for neural based S and T Learners
#####
 # load dataset
 data(exp_data)
 # estimate CATEs with S Learner
 set.seed(123456)
 slearner_nn <- metalearner_deepneural(cov.formula = support_war ~ age + income +
                                    + job_loss,
                                    data = exp_data,
                                    treat.var = "strong_leader",
                                    meta.learner.type = "S.Learner",
                                    stepmax = 4e+9,
                                    nfolds = 5,
                                    algorithm = "rprop+",
                                    hidden.layer = c(1),
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

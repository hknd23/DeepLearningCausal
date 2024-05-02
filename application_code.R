library(readr)
library(dplyr)
library(tidyr)


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


set.seed(123456)
create.SL()
#SL.library.class <- define.SL.class.library()
#SL.library.reg <- define.SL.reg.library()



exp_compliers<-exp_complier_predict(exp_prep)

response_model<-exp_response(exp.data = exp_prep,exp.compliers = exp_compliers,
                             family = "binomial")

counterfactuals<-pattc_counterfactuals(pop.data=pop_prep,
                                       response.mod=response_model)

t.patt <-  WtC(x=counterfactuals$Y_hat1, 
               y=counterfactuals$Y_hat0,
               bootse=FALSE,
               bootp = FALSE,
               bootn = 999, samedata=FALSE,
               equivalence = FALSE)



library(readr)
library(dplyr)
library(tidyr)
library(ROCR)
expdata <- read_csv("C:/Users/nguye/My Drive/pattcdata/India_svy_methpaper_APR 17 2024.csv")
popdata <- read_csv("C:/Users/nguye/My Drive/pattcdata/WVS_Covariates_0426.csv")
summary(expdata)

###########


#religion 5-muslim, 6 Hindu
rownames(expdata)<-expdata$l
expdata$Hindu<-ifelse(expdata$religion==6,1,0)
expdata$married<-ifelse(expdata$marital_stat==1,1,0)
expdata$employed<-ifelse(expdata$empl_status==1,1,0)
expdata$male<-ifelse(expdata$gender==1,1,0)
expdata$outcome<-ifelse(expdata$exp1_dv1==1,1,0)   ###ASKFOR CODING 1 yes 2 know

expdata$compl1<-ifelse(expdata$exp1trt1_nc==1,0,1)
expdata$compl1<- tidyr::replace_na(expdata$compl1,0)
expdata$compl2<-ifelse(expdata$exp1trt2_nc==1,0,1)
expdata$compl2<- tidyr::replace_na(expdata$compl2,0)

expdata$trt1<-ifelse(expdata$exp1_trt==1,1,0)
expdata$trt2<-ifelse(expdata$exp1_trt==2,1,0)

expdata[,c("exp1_trt","compl1","compl2")]
summary(expdata[,c("exp1_trt","compl1","compl2")])

expdata<-as.data.frame(expdata)

######prep popdata
popdata$Hindu<-ifelse(popdata$Religious_Denom=="Hindu",1,0)
popdata$married<-ifelse(popdata$marital==1,1,0)
popdata$employed<-ifelse(popdata$empl_status<4,1,0)
popdata$male<-ifelse(popdata$gender==1,1,0)


popdata$outcome1<-ifelse(popdata$exp1_dv<3,1,0)
popdata$outcome2<-ifelse(popdata$exp1_dv2<0,NA,ifelse(popdata$exp1_dv2>2,0,1))


popdata$compl1<-ifelse(popdata$strong_leader_good==1,1,0)
popdata$compl2<-ifelse(popdata$strong_leader_good==1,0,1)

popdata$compl1<- tidyr::replace_na(popdata$compl1,0)
popdata$compl2<- tidyr::replace_na(popdata$compl2,0)

##################
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
pop_prep2<-popcall(outcome1~ age + male + 
                    income + education + 
                    employed + married + 
                    Hindu + job_worry,
                  ~compl1,data=popdata)


source("C:/Users/nguye/My Drive/newproject/R/superlearners.R")
set.seed(42)
create.SL()
SL.library.class <- define.SL.class.library()
SL.library.reg <- define.SL.reg.library()
#recheck here
data=exp_prep



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
                 
                                     

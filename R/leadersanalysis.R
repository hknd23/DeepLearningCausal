library(readr)
library(dplyr)
library(tidyr)
library(ROCR)
expdata <- read_csv("C:/Users/nguye/My Drive/pattcdata/India_svy_methpaper_APR 17 2024.csv")
popdata <- read_csv("C:/Users/nguye/My Drive/pattcdata/WVS_Covariates_0426.csv")
summary(expdata)

###########
# FUNC1
# expcall()
#Prepare dataset



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











"outcome~trt1|"

rownames(expdata)<-expdata$l
rct.treat<-expdata$trt1
rct.compliance<-expdata$compl1
rct.compliance2<-expdata$compl2
outcome<-expdata$outcome
#formula
compliance<-"compl1"
treat<-"trt1"
response<-"outcome"
#response ~ treatment|compliance|covariates
testformula<-formula("response ~ treatment|compliance|covariates")

testformula2<-formula("response ~ treatment|covariates")
expformula<-testformula2
popformula<-NULL
compliance<-"compl1"
##########
formula1 <- as.formula(testformula2)
formula1 <- popformula

variables<-unique(c(all.vars(expformula),compliance))

expdata<-expdata
populationdata<-NULL 

cov.names<-c("age","male","income","education","employed","married","Hindu","job_worry")
covs<-expdata[,cov.names]
response.formula<-as.formula(paste(response,"~",treat))
cov.formula<-as.formula(paste("~",paste(cov.names,collapse=" +")))
compl.formula<-as.formula(paste("~",compliance))
obsID<-"l"

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

Z <- model.matrix(attr(mf2, "terms"), data = mf2)
Y <- as.matrix(model.response(mf1))
C <- as.matrix(model.response(mf2))






Y=rct.compliance[which(rct.treat==1)]
Y2=
Y3= exp_prep[which(exp_prep$Texp==1)]$Cexp

source("C:/Users/nguye/My Drive/newproject/R/superlearners.R")
set.seed(42)
create.SL()
SL.library.class <- define.SL.class.library()
SL.library.reg <- define.SL.reg.library()
#recheck here
data=exp_prep


exp_complier_predict <- function(data,ID=NULL,SL.library=NULL) {
if (!is.null(ID)){
  id=ID
}
if (is.null(SL.library))
{
  SL.library.class <- define.SL.class.library()
}
expdata <- data
covs <- as.data.frame(expdata$Xexp)
complier.mod <- SuperLearner(Y=exp_prep$Cexp[which(exp_prep$Texp==1)], 
                             X=covs[which(exp_prep$Texp==1),], 
                             SL.library=SL.library.class,
                             id=ID,
                             family="binomial")

C.pscore <- predict(complier.mod, covs, onlySL=TRUE)
rct.compliers <- data.frame("treatment"=as.data.frame(exp_prep$Texp)[,1],
                            "real_complier"=rct.compliance,
                            "C.pscore"=C.pscore$pred,row.names = rownames(exp_prep$Yexp))
pred.compliers <- ROCR::prediction(rct.compliers$C.pscore[rct.compliers$
                                                            treatment==1],
                                   rct.compliers$real_complier[rct.compliers$
                                                                 treatment==1]) 
cost.perf <- ROCR::performance(pred.compliers, "cost")
opt.cut <- pred.compliers@cutoffs[[1]][which.min(cost.perf@y.values[[1]])]

rct.compliers$predicted_complier<-ifelse(rct.compliers$treatment==0 & 
                                           rct.compliers$C.pscore>opt.cut,1,0)
rct.compliers$compliers_all<-rct.compliers$real_complier+
                            rct.compliers$predicted_complier
return(rct.compliers)
}

exp_compliers<-exp_complier_predict(exp_prep)
ID=NULL

family="binomial"
SL.library=SL.library.class

exp_response<-function(exp.data,exp.compliers,family,ID=NULL,SL.library=NULL){

if (family=="binomial" & is.null(SL.library) )  {
  SL.library <- define.SL.class.library()
}
  
Y.exp.response <- exp.data$Yexp[which(exp.compliers$compliers_all==1)]
X.exp.response <- data.frame("complier"=exp.data$Cexp[which(exp.compliers$compliers_all==1)],
                             exp.data$Xexp[which(exp.compliers$complier==1),])
response.mod <- SuperLearner(Y=Y.exp.response, 
                            X=X.exp.response, 
                            SL.library=SL.library,
                            family=family,
                            id=ID)
return(response.mod)
}

response_model<-exp_response(exp.data = exp_prep,exp.compliers = exp_compliers,
                             family = "binomial")
pop.data=pop_prep
pop.data

pattc_counterfactuals<- function(pop.data,response.mod,id=NULL,cluster=NULL){
if(!is.null(cluster)){
  clustervar<-
}
pop.tr.counterfactual <- cbind("complier" = 1,
                               pop.data$Xpop[which(pop.data$Cpop==1),])
pop.ctrl.counterfactual <- cbind("complier" = 0,
                                 pop.data$Xpop[which(pop.data$Cpop==1),])
Y.hat.1 <- predict(response_model, pop.tr.counterfactual, onlySL = T)$pred 
Y.hat.0 <- predict(response_model, pop.ctrl.counterfactual, onlySL = T)$pred 

Y.hats<-data.frame(Y_hat1 = Y.hat.1, Y_hat0 = Y.hat.0)

return(Y.hats)
}

counterfactuals<-pattc_counterfactuals(pop.data=pop_prep,
                                       response.mod=response_model)

counterfactuals$Y_hat0


t.patt <-  WtC(x=counterfactuals$Y_hat1, 
                                         y=counterfactuals$Y_hat0,
                                         bootse=FALSE,
                                         bootp = FALSE,
                                         bootn = 999, samedata=FALSE,
                                         equivalence = FALSE)
                 
                                        # weight = nhis.weights[which(insurance.nhis == 1)],
                                       #  weighty=nhis.weights[which(insurance.nhis==1)], 
                                       #  cluster = nhis.hhid[which(insurance.nhis == 1)],
                                       #  clustery=nhis.hhid[which(insurance.nhis==1)], 
                                      #   samedata=FALSE,
                                      #   equivalence = FALSE)) 




# Compute CACE Booot then

rct.cace <-  WtC(x=as.matrix(outcome[which(rct.treat==1)]), 
                 y=as.matrix(outcome[which(rct.treat==0)]), 
                 c=as.matrix(rct.compliers$compliers_all[which(treatment==1)]), 
                 bootse=FALSE,
                 bootp = FALSE,
                 bootn = 999,
                 samedata=FALSE,
                 weight = rct.treat[which(rct.treat==1)], 
                 weighty= rct.treat[which(rct.treat==0)], 
                 weightc=rct.compliers$complier[which(treatment==1)], 
                 cluster = rep(1,length(rct.treat[which(rct.treat == 1)])), 
                 clustery=rep(1,length(rct.treat[which(rct.treat==0)])), 
                 clusterc=rep(1,length(rct.compliers$complier[which(treatment==1)])),
                 equivalence = FALSE)

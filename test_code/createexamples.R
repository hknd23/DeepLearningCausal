setwd("C:/Users/nguye/My Drive/newproject")

IND_exp_data <- read.csv("test_code/IND_exp_data_full.csv")
IND_pop_data <- read.csv("test_code/IND_pop_data_full.csv")

vasrs<-c("trt1", "outcome", "compl1", "exp1_dv1", "male", "age", "income", "imp_rel", "Hindu", "religion", "education",
         "ideol_lr","empl_status", "employed", "marital_stat","married", "job_worry")
vasrs2<-c("exp1_trt", "outcome1", "compl1", "exp1_dv", "male", "age", "income", "imp_rel", "Hindu", "religion", "education",
          "ideol","empl_status", "employed", "marital","married", "job_worry","year")
IND_exp_data2<-IND_exp_data[,c(vasrs)]
IND_pop_data2<-IND_pop_data[,c(vasrs2)]

IND_exp_data2$female<-ifelse(IND_exp_data2$male==1,0,1)
IND_pop_data2$female<-ifelse(IND_pop_data2$male==1,0,1)

IND_pop_data2$exp1_trtA <- ifelse(IND_pop_data2$exp1_trt<0,NA,
                                 ifelse(IND_pop_data2$exp1_trt < 3,1,0))

vasrsa<-c("trt1", "outcome", "compl1", "female", "age", "income","religion","Hindu", "imp_rel",
         "education", "ideol_lr","empl_status", "employed", "marital_stat","married", "job_worry")
vasrsap<-c("exp1_trtA", "outcome1", "compl1", "female", "age", "income","religion","Hindu", "imp_rel",
          "education", "ideol","empl_status", "employed", "marital","married", "job_worry","year")

IND_exp_data3<-IND_exp_data2[,vasrsa]
IND_pop_data3<-IND_pop_data2[,vasrsap]

varnames<-c("strong_leader", "support_war", "compliance", "female", "age", "income",
            "religion", "hindu", "practicing_religion", "education", "political_ideology",
            "employment", "employed", "marital_status", "married", "job_loss")
varnames2<-c("strong_leader", "support_war", "compliance", "female", "age", "income",
            "religion", "hindu", "practicing_religion", "education", "political_ideology",
            "employment", "employed", "marital_status", "married", "job_loss","year")
colnames(IND_exp_data3) <- varnames
colnames(IND_pop_data3) <- varnames2

IND_exp_ex <- IND_exp_data3[sample(nrow(IND_exp_data3), nrow(IND_exp_data3)/2),]
IND_pop_ex <- IND_pop_data3[which(IND_pop_data3$year %in% c(2022)),]
IND_pop_ex2<-IND_pop_ex[sample(nrow(IND_pop_ex), nrow(IND_pop_ex)/2),]

Exp_data_full <- IND_exp_data3
Pop_data_full <- IND_pop_data3

expdata <- IND_exp_ex
popdata <- IND_pop_ex2

Exp_data <- expdata
Pop_data <- popdata

save(Exp_data_full, file = "test_code/Exp_Data_Full.rda")
save(Pop_data_full, file = "test_code/Pop_Data_Full.rda")
save(Exp_data, file = "test_code/Exp_Data.rda")
save(Pop_data, file = "test_code/Pop_Data.rda")

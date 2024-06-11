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


vasrsa<-c("trt1", "outcome", "compl1", "female", "age", "income","religion","Hindu", "imp_rel",
         "education", "ideol_lr","empl_status", "employed", "marital_stat","married", "job_worry")
vasrsap<-c("exp1_trt", "outcome1", "compl1", "female", "age", "income","religion","Hindu", "imp_rel",
          "education", "ideol","empl_status", "employed", "marital","married", "job_worry","year")

IND_exp_data3<-IND_exp_data2[,vasrsa]
IND_pop_data3<-IND_pop_data2[,vasrsap]

varnames<-c("support_war", "strong_leader", "compliance", "female", "age", "income",
            "religion", "hindu", "practicing_religion", "education", "political_ideology",
            "employment", "employed", "marital_status", "married", "job_loss")
varnames2<-c("support_war", "strong_leader", "compliance", "female", "age", "income",
            "religion", "hindu", "practicing_religion", "education", "political_ideology",
            "employment", "employed", "marital_status", "married", "job_loss","year")
colnames(IND_exp_data3) <- varnames
colnames(IND_pop_data3) <- varnames2

IND_exp_ex <- IND_exp_data3[sample(nrow(IND_exp_data3), nrow(IND_exp_data3)/2),]
IND_pop_ex <- IND_pop_data3[which(IND_pop_data3$year %in% c(2022)),]
IND_pop_ex2<-IND_pop_ex[sample(nrow(IND_pop_ex), nrow(IND_pop_ex)/2),]

exp_data_full <- IND_exp_data3
pop_data_full <- IND_pop_data3

expdata <- IND_exp_ex
popdata <- IND_pop_ex2

exp_data <- expdata
pop_data <- popdata


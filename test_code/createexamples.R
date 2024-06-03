IND_exp_data <- read.csv("test_code/IND_exp_data_full.csv")
IND_pop_data <- read.csv("test_code/IND_pop_data_full.csv")

vasrs<-c("trt1", "outcome", "compl1", "exp1_dv1", "male", "age", "income", "imp_rel", "Hindu", "religion", "education",
         "ideol_lr","empl_status", "employed", "marital_stat","married", "job_worry")
vasrs2<-c("exp1_trt", "outcome1", "compl1", "exp1_dv", "male", "age", "income", "imp_rel", "Hindu", "religion", "education",
          "ideol","empl_status", "employed", "marital","married", "job_worry","year")
IND_exp_data2<-IND_exp_data[,c(vasrs)]
IND_pop_data2<-IND_pop_data[,c(vasrs2)]


IND_exp_ex <- IND_exp_data2[sample(nrow(IND_exp_data2), nrow(IND_exp_data2)/2),]
IND_pop_ex <- IND_pop_data2[which(IND_pop_data2$year %in% c(2022)),]
IND_pop_ex2<-IND_pop_ex[sample(nrow(IND_pop_ex), nrow(IND_pop_ex)/2),]

expdata <- IND_exp_ex
popdata <- IND_pop_ex2

popdata$trt1 <- popdata$exp1_trt
popdata$outcome <- popdata$outcome1

IND_exp_data <- expdata
IND_pop_data <- popdata


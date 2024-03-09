#Code for estimating PATT PATT-C and CACE
###Data preestimation processing
#I##########
#Check colnames for datasets
if (sum(colnames(rctdata)==colnames(popdata))!=
    length(colnames(rctdata)))
{
  print("Covariates do not match")
} else
#########


#loading testing dataset
load(paste0(getwd(),"/data/prepare-analysis.RData"))


#general formula Y ~ X|D where Y= Outcome, X= Covariates, D=Compliance (in the population)
##### Tobe added: formula code
RCTDATA <- data.frame(treatment,
                         insurance,
                         "num.visit"=num.visit,# need to omit rows containing any NA
                         "num.out"=num.out,
                       ohie.hhid,
                      ohie.weights,
                         n.hh,  # need to omit rows containing any NA
                         n.children,
                         gender,
                         age.19to49,
                         age.50to64,
                         white,
                         black,
                         asian,
                         aian,
                         race.other,
                         hisp,
                         diabetes,
                         asthma,
                         bp,
                         copd,
                         heart,
                         education,
                         income,
                         partner,
                         employed,
                         wave,
                         wave.interact)
covs<-colnames(RCT2)[7:length(colnames(RCT2))]
treat.var<-"treatment"
comp.var<-"insurance"
outcome.var<-c("num.visit", "num.out")
RCT2<-na.omit(RCTDATA)
experiment.data<-RCTDATA
population.data<-RCTDATA


  rct.data<-na.omit(experiment.data)
pop.data<-na.omit(population.data)

############
#create dataframe for covariates

x.rct<-rct.data[,covs]
x.pop<-pop.data[,covs]

# Create vectors for treatment and compliance
treat.rct <- rct.data[,treat.var]

compl.rct <- rct.data[,comp.var]
compl.pop <- pop.data[,comp.var]

# Create dfs for outcomes
Y.rct <- rct.data[,outcome.var]
Y.pop <- pop.data[,outcome.var]
rct.id<-rct.data[,"ohie.hhid"]
rct.weights<-rct.data[,"ohie.weights"]

compl.rct[which(treat.rct==1)]
#ComplierMod
# Predict who is a complier in the control group
set.seed(42)
source(paste0(getwd(),"/R/superlearners.R"))


complier.mod <- SuperLearner(Y=compl.rct[which(treat.rct==1)],
                             X=x.rct[which(treat.rct == 1),],
                             SL.library=SL.library.class,
                             family="binomial",
                             id=rct.id[which(treat.rct == 1)],# force observations in the same cluster to be in the same validation fold
                             obsWeights = rct.weights[which(treat.rct == 1)]) # observation weights

summary(complier.mod)

# Store predictions
comp.pred <- predict(complier.mod, x.rct, onlySL=TRUE)


#save(complier.mod, file=paste0(getwd(),"/data/complier-mod.rda"))
rct.compliers <- data.frame("treatment"=treat.rct,
                            "compliance"=compl.rct,
                            "comppredicted"=comp.pred$pred,
                            "complier"=ifelse(treat.rct==1 & compl.rct==1,1,0),
                            "weights"=rct.weights,
                            "cluster"=rct.id)

# put in ROCR object # predicted vs. actual compliers
pred.compliers <- ROCR::prediction(rct.compliers$comppredicted[rct.compliers$treatment==1],
                                   rct.compliers$complier[rct.compliers$treatment==1])

# Get optimal cut-point
cost.perf <- performance(pred.compliers, "cost")
opt.cut <- pred.compliers@cutoffs[[1]][which.min(cost.perf@y.values[[1]])]
opt.cut
 # predicted compliers from the control group

rct.compliers$complier_control<-ifelse(rct.compliers$treatment==0 &
                                         rct.compliers$comppredicted>opt.cut,1,0)
rct.compliers$complier_predicted<-rct.compliers$complier+rct.compliers$complier_control



# Fit a regression to the compliers in the RCT
y.col <- seq_along(Y.rct)
Y.rct.response <- Y.rct[which(rct.compliers$complier_predicted==1),]
X.rct.response <- data.frame("complier"=compl.rct[which(rct.compliers$complier_predicted==1)],
                              x.rct[which(rct.compliers$complier_predicted==1),])

# For computing unadjusted PATT <-PATT FOR RCT
#Y.rct.response.unadj <- Y.rct[which(rct.compliers$complier_predicted==1 | rct.compliers$complier_predicted==0),]
Y.rct.response.unadj <- Y.rct
X.rct.response.unadj <- data.frame("complier"=compl.rct,
                                    x.rct)

# Run response model
set.seed(42)
response.mod <- lapply(y.col, function (i) SuperLearner(Y=Y.rct.response[,i],
                                                        X=X.rct.response,
                                                        SL.library=SL.library.reg,
                                                        family="gaussian",
                                                        id=rct.id[which(rct.compliers$complier_predicted==1)],
                                                        obsWeights = rct.weights[which(rct.compliers$complier_predicted==1)]))

names(response.mod) <- colnames(Y.ohie.response)[y.col] # name each element of list

response.mod # summarize




#roc
roc.perf <- performance(pred.compliers, measure = "tpr", x.measure = "fpr") # plot ROC curve
plot(roc.perf)
abline(a=0, b= 1)

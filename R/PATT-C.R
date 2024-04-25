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
#load functions
source(paste0(getwd(),"/R/superlearners.R"))
source(paste0(getwd(),"/R/wtc.R"))
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
covs<-colnames(RCTDATA)[7:length(colnames(RCTDATA))]
treat.var<-"treatment"
comp.var<-"insurance"
outcome.var<-c("num.visit", "num.out")
weight<-"ohie.weights"
cluster.id<-"ohie.hhid"
experiment.data<-RCTDATA
population.data<-RCTDATA


#rct.data<-na.omit(experiment.data)
#pop.data<-na.omit(population.data)

rct.data<-experiment.data
pop.data<-population.data
############
#create dataframe for covariates

x.rct<-na.omit(rct.data[,covs])
x.pop<-na.omit(pop.data[,covs])

# Create vectors for treatment and compliance
treat.rct <- rct.data[,treat.var][as.numeric(rownames(x.rct))]

compl.rct <- rct.data[,comp.var][as.numeric(rownames(x.rct))]
compl.pop <- pop.data[,comp.var][as.numeric(rownames(x.pop))]

# Create dfs for outcomes
Y.rct <- na.omit(rct.data[,outcome.var])
Y.pop <- na.omit(pop.data[,outcome.var])


rct.id<-rct.data[,cluster.id]
rct.weights<-rct.data[,weight]

#compl.rct[which(treat.rct==1)]
#ComplierMod
# Predict who is a complier in the control group
set.seed(42)
#Checked same, so rerun this (clear code?)
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
                            "weights"=rct.weights[as.numeric(rownames(x.rct))],
                            "cluster"=rct.id[as.numeric(rownames(x.rct))])

# put in ROCR object # predicted vs. actual compliers
pred.compliers <- ROCR::prediction(rct.compliers$comppredicted[rct.compliers$treatment==1],
                                   rct.compliers$complier[rct.compliers$treatment==1])

# Get optimal cut-point
cost.perf <- ROCR::performance(pred.compliers, "cost")
opt.cut <- pred.compliers@cutoffs[[1]][which.min(cost.perf@y.values[[1]])]
opt.cut
 # predicted compliers from the control group

rct.compliers$complier_control<-ifelse(rct.compliers$treatment==0 &
                                         rct.compliers$comppredicted>opt.cut,1,0)
rct.compliers$complier_predicted<-rct.compliers$complier+rct.compliers$complier_control
y.col <- seq_along(Y.rct)


####CACE
# Compute CACE #continue here
rct.cace <- lapply(y.col, function (i) WtC(x=as.matrix(Y.rct[[i]][which(treat.rct==1)]),
                                           y=as.matrix(Y.rct[[i]][which(treat.rct==0)]),
                                           c=as.matrix(rct.compliersX$complier[which(treatment.ohie==1)]),
                                           bootse=TRUE,
                                           bootp = TRUE,
                                           bootn = 999,
                                           weight = ohie.weights[which(treatment.ohie == 1)],
                                           weighty= ohie.weights[which(treatment.ohie == 0)],
                                           weightc=rct.compliersX$weights[which(treatment.ohie == 1)],
                                           cluster = ohie.hhid[which(treatment.ohie == 1)],
                                           clustery=ohie.hhid[which(treatment.ohie==0)],
                                           clusterc=rct.compliersX$cluster[which(treatment.ohie == 1)],
                                           samedata=FALSE,
                                           equivalence = FALSE))

#######
#Resonse Model
#######
# Fit a regression to the compliers in the RCT
Y.rct.response <- Y.rct[which(rct.compliers$complier_predicted==1),]
X.rct.response <- data.frame("complier"=compl.rct[which(rct.compliers$complier_predicted==1)],
                              x.rct[which(rct.compliers$complier_predicted==1),])

# Run response model
set.seed(42)
response.mod <- lapply(y.col, function (i) SuperLearner(Y=Y.rct.response[,i],
                                                        X=X.rct.response,
                                                        SL.library=SL.library.reg,
                                                        family="gaussian",
                                                        id=rct.id[which(rct.compliers$complier_predicted==1)],
                                                        obsWeights = rct.weights[which(rct.compliers$complier_predicted==1)]))
#save(response.mod, file="data/response.mod0136.rda")
names(response.mod) <- colnames(Y.rct.response)[y.col] # name each element of list



# Use response model to estimate potential outcomes for population "compliers" on medicaid
nrt.tr.counterfactual <- cbind("insurance" = rep(1, length(which(compl.pop==1))),
                               x.pop[which(compl.pop==1),])
nrt.ctrl.counterfactual <- cbind("insurance" = rep(0, length(which(compl.pop==1))),
                                 x.pop[which(compl.pop==1),])


Y.hat.1 <- lapply(colnames(Y.rct),
                  function (i) predict(response.mod[[i]],
                  nrt.tr.counterfactual, onlySL = T)$pred) # extract SL predictions
Y.hat.0 <- lapply(colnames(Y.rct),
                  function (i) predict(response.mod[[i]],
                  nrt.ctrl.counterfactual, onlySL = T)$pred)





# For computing unadjusted PATT <-PATT FOR RCT
#Y.rct.response.unadj <- Y.rct[which(rct.compliers$complier_predicted==1 | rct.compliers$complier_predicted==0),]
Y.rct.response.unadj <- Y.rct
X.rct.response.unadj <- data.frame("complier"=compl.rct,
                                    x.rct)



set.seed(42)
response.mod.patt <- lapply(y.col, function (i) SuperLearner(Y=Y.rct.response.unadj[,i],
                                                             X=X.rct.response.unadj,
                                                             SL.library=SL.library.reg,
                                                             family="gaussian",
                                                             id=rct.id,
                                                             obsWeights = rct.weights))

# Use response model to estimate potential outcomes for population "compliers" on medicaid



Y.hat.1.unadj <- lapply(colnames(Y.pop), function (i) predict(response.mod.patt[[i]], nrt.tr.counterfactual, onlySL = T)$pred)
Y.hat.0.unadj <- lapply(colnames(Y.pop), function (i) predict(response.mod.patt[[i]], nrt.ctrl.counterfactual, onlySL = T)$pred)


rct.id<-rct.data[,cluster.id]
rct.weights<-rct.data[,weight]

pop.id<-pop.data[,cluster.id]
pop.weights<-pop.data[,weight]
#PATT
set.seed(42)
t.patt.unadj <- lapply(y.col, function (i) WtC(x=Y.hat.1.unadj[[i]],
                                               y=Y.hat.0.unadj[[i]],
                                               bootse=TRUE,
                                               bootp = TRUE,
                                               bootn = 999,
                                               weight = pop.weights[which(compl.pop == 1)],
                                               weighty=pop.weights[which(compl.pop==1)],
                                               cluster = pop.id[which(compl.pop == 1)],
                                               clustery=pop.id[which(compl.pop==1)],
                                               samedata=FALSE,
                                               equivalence = FALSE))
#roc
roc.perf <- performance(pred.compliers, measure = "tpr", x.measure = "fpr") # plot ROC curve
plot(roc.perf)
abline(a=0, b= 1)

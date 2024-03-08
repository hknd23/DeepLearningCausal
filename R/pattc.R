#Code for estimating PATT PATT-C and CACE

#loading testing dataset
load(paste0(getwd(),"/data/prepare-analysis.RData"))
#general formula Y ~ X|D where Y= Outcome, X= Covariates, D=Compliance (in the population)
##### Tobe added: formula code
RCTDATA <- data.frame(treatment,
                         insurance,
                         "num.visit"=num.visit,# need to omit rows containing any NA
                         "num.out"=num.out,
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
covs<-colnames(RCT2)[5:length(colnames(RCT2))]
treat.var<-"treatment"
comp.var<-"insurance"
outcome.var<-c("num.visit", "num.out")
RCT2<-na.omit(RCTDATA)
experiment.data<-RCTDATA
population.data<-RCTDATA
outcome.var<-
###Data preestimation processing
#I##########
#Check colnames for datasets
if (sum(colnames(rctdata)==colnames(popdata))!=
    length(colnames(rctdata)))
{
  print("Covariates do not match")
} else

  rct.data<-na.omit(experiment.data)
pop.data<-na.omit(population.data)

############
#create dataframe for covariates
x.rct<-rct.data[,covs]
x.pop<-pop.data[,covs]

# Create vectors for treatment and compliance
treatment <- rct.data[,treat.var]

compl.rct <- rct.data[,comp.var]
compl.pop <- pop.data[,comp.var]

# Create dfs for outcomes
Y.rct <- rct.data[,outcome.var]
Y.pop <- pop.data[,outcome.var]



#ComplierMod
# Predict who is a complier in the control group
set.seed(42)
source(paste0(getwd(),"/R/superlearners.R"))

complier.mod <- SuperLearner(Y=compl.rct[which(treatment.ohie==1)],
                             X=X.ohie[which(treatment.ohie == 1),],
                             SL.library=SL.library.class,
                             family="binomial",
                             id=ohie.hhid[which(treatment.ohie == 1)],# force observations in the same cluster to be in the same validation fold
                             obsWeights = ohie.weights[which(treatment.ohie == 1)]) # observation weights

summary(complier.mod)

# Store predictions
C.pscore <- predict(complier.mod, X.ohie, onlySL=TRUE)

#Code for estimating PATT PATT-C and CACE

#loading testing dataset

#general formula Y ~ X|D where Y= Outcome, X= Covariates, D=Compliance (in the population)
##### Tobe added: formula code

###Data preestimation processing
#Check colnames for datasets
if (sum(colnames(rctdata)==colnames(popdata))!=
    length(colnames(rctdata)))
{
  print("Covariates do not match")
} else

  rct.data<-na.omit(experiment.data)
pop.data<-na.omit(population.data)


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

complier.mod <- SuperLearner(Y=insurance.ohie[which(treatment.ohie==1)],
                             X=X.ohie[which(treatment.ohie == 1),],
                             SL.library=SL.library.class,
                             family="binomial",
                             id=ohie.hhid[which(treatment.ohie == 1)],# force observations in the same cluster to be in the same validation fold
                             obsWeights = ohie.weights[which(treatment.ohie == 1)]) # observation weights

summary(complier.mod)

# Store predictions
C.pscore <- predict(complier.mod, X.ohie, onlySL=TRUE)

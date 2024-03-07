#Code for estimating PATT PATT-C and CACE

library(haven)
testda <- read_dta("C:/Users/nguye/Downloads/Contr Game Feb 21 2024/Contr Game Feb 21 2024/CONTR Game Data 15 Aug 23.dta")
testda2 <- testda

#general formula Y ~ X|D where Y= Outcome, X= Covariates, D=Compliance (in the population)


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

compl.rct <- insurance[as.numeric(rownames(X.ohie))]
compl.pop <- medicaid[as.numeric(rownames(X.nhis))]

# Create dfs for outcomes 
Y.ohie <- na.omit(data.frame("num.visit"=num.visit,# need to omit rows containing any NA
                             "num.out"=num.out))

Y.nhis <- na.omit(data.frame("num.visit"=nhis.num.visit,# need to omit rows containing any NA
                             "num.out"=nhis.num.out))




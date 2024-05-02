expcall <- function(response.formula,
                    cov.formula,
                    compl.formula,
                    data,
                    weights=NULL,
                    cluster=NULL,
                    ID=NULL)
{
  if (is.null(ID ==FALSE)){
    rownames(data)<-data[,ID]
  }
  response.formula <- as.formula(response.formula)
  cov.formula <- as.formula(cov.formula)
  compl.formula<-as.formula(compl.formula)
  variables <- unique(c(all.vars(response.formula),
                        all.vars(cov.formula),
                        all.vars(compl.formula)))
  newdata <- na.omit(data[,variables])

  expmf <- model.frame(response.formula,data=newdata)
  covmf <- model.frame(cov.formula,data=newdata)
  complmf <- model.frame(compl.formula,data=newdata)

  Tterm <- terms(expmf)
  attr(Tterm, "intercept") <- 0
  Covterms <- terms(covmf)
  attr(Covterms, "intercept") <- 0
  Complterms <- terms(complmf)
  attr(Complterms, "intercept") <- 0

  Texp <- model.matrix(Tterm, data = expmf)
  Yexp <- as.matrix(model.response(expmf))
  Xexp <- model.matrix(Covterms, data = covmf)
  Cexp <- model.matrix(Complterms, data = complmf)

  expl<-list(Yexp= Yexp, Texp = Texp, Xexp= Xexp,
             Cexp = Cexp,expdata= newdata,
             response_formula=response.formula,
             covariates=cov.formula,
             compliance=compl.formula, type="Experiment")

  return(expl)
}

popcall <- function(response.formula,
                    compl.formula,
                    data,
                    weights=NULL,
                    cluster=NULL,
                    ID=NULL)
{
  if (!is.null(ID)){
    rownames(data)<-data[,ID]
  }
  if (!is.null(weights)){
    weights<-data[,weights]
  }
  if (!is.null(cluster)){
    clustervar<-data[,cluster]
  } else {clustervar<-NULL}
  response.formula <- as.formula(response.formula)
  compl.formula<-as.formula(compl.formula)
  variables <- unique(c(all.vars(response.formula),
                        all.vars(compl.formula)))
  newdata <- na.omit(data[,variables])


  popmf <- model.frame(response.formula,data=newdata)
  complmf <- model.frame(compl.formula,data=newdata)

  Covterms <- terms(popmf)
  attr(Covterms, "intercept") <- 0
  Complterms <- terms(complmf)
  attr(Complterms, "intercept") <- 0

  Ypop <- as.matrix(model.response(popmf))
  Xpop <- model.matrix(Covterms, data = popmf)
  Cpop <- model.matrix(Complterms, data = complmf)

  popl<-list(Ypop= Ypop, Xpop= Xpop,
             Cpop = Cpop,popdata= newdata,
             response_formula=response.formula,
             compliance=compl.formula,
             weights=weights,
             cluster=clustervar,
             type="Population")

  return(popl)
}

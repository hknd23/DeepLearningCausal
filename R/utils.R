expcall <- function(response.formula,
                    treat.var,
                    compl.var,
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
  variables <- unique(c(all.vars(response.formula),
                        treat.var,
                        compl.var))
  newdata <- na.omit(data[,variables])

  responsevar<-variables[1]
  covariates<-variables[-1]

  .formula <- as.formula(paste0(paste0(responsevar," ~",treat.var, " + ",
                                       compl.var, " + "),
                                paste0(covariates,collapse = " + ")))


  expmf <- model.frame(.formula,data=newdata)

  expl<-list(expdata= expmf,
             response_formula=response.formula,
             treat_var=treat.var,
             compl_var=compl.var, type="Experiment")

  return(expl)
}

popcall <- function(response.formula,
                    compl.var,
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
  variables <- unique(c(all.vars(response.formula),
                        compl.var))
  newdata <- na.omit(data[,variables])

  responsevar<-variables[1]
  covariates<-variables[-1]

  .formula <- as.formula(paste0(paste0(responsevar," ~",
                                       compl.var, " + "),
                                paste0(covariates,collapse = " + ")))

  popmf <- model.frame(.formula,data=newdata)

  popl<-list(popdata= popmf,
             response_formula=response.formula,
             compl_var=compl.var,
             weights=weights,
             cluster=clustervar,
             type="Population")

  return(popl)
}

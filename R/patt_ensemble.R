#' @export
#'
complier_mod <- function(data,ID=NULL,SL.library=NULL) {
  if (!is.null(ID)){
    id=ID
  }
  if (is.null(SL.library))
  {
    SL.library.class <- define.SL.class.library()
  }
  expdata <- data$expdata
  covariates <- all.vars(data$response_formula)[-1]
  compl.var <- data$compl_var

  Ycompl<-expdata[which(expdata[,treat.var]==1),compl.var]
  Xcompl<-expdata[which(expdata[,treat.var]==1),covariates]

  complier.mod <- SuperLearner::SuperLearner(Y=Ycompl,
                               X=Xcompl,
                               SL.library=SL.library.class,
                               id=ID,
                               family="binomial")
  return(complier.mod)
}

#' @export
#'
complier_predict <- function(complier.mod,exp.data) {

  covdata <- exp.data$expdata
  covariates.names <- all.vars(expdata$response_formula)[-1]
  covariates <- covdata[,covariates.names]
  treat.var<-exp.data$treat_var
  compl.var<-exp.data$compl_var

  C.pscore <- predict(complier.mod, covariates, onlySL=TRUE)

  rct.compliers <- data.frame("treatment"=covdata[,treat.var],
                              "real_complier"=covdata[,compl.var],
                              "C.pscore"=C.pscore$pred,
                              row.names = rownames(covdata))

  pred.compliers <- ROCR::prediction(rct.compliers$C.pscore[rct.compliers$
                                                          treatment==1],
                                 rct.compliers$real_complier[rct.compliers$
                                                               treatment==1])
  cost.perf <- ROCR::performance(pred.compliers, "cost")
  opt.cut <- pred.compliers@cutoffs[[1]][which.min(cost.perf@y.values[[1]])]

  rct.compliers$predicted_complier<-ifelse(rct.compliers$treatment==0 &
                                         rct.compliers$C.pscore>opt.cut,1,0)
  rct.compliers$compliers_all<-rct.compliers$real_complier+
  rct.compliers$predicted_complier
  return(rct.compliers)
  }

#' @export

response_model<-function(exp.data,
                         exp.compliers,
                         family="binomial",
                         ID=NULL,
                         SL.library=NULL){

  if (family=="binomial" & is.null(SL.library) )  {
    SL.library <- define.SL.class.library()
  }
  response.var <- all.vars(exp.data$response_formula)[1]
  covariates <- all.vars(exp.data$response_formula)[-1]

  treat.var <- exp.data$treat_var
  compl.var <- exp.data$compl_var
  response.data <- exp.data$expdata

  Y.exp.response <- response.data[,response.var][which(exp.compliers$compliers_all==1)]
  X.exp.response <- data.frame("complier"=response.data[,compl.var][which(exp.compliers$compliers_all==1)],
                               response.data[which(exp.compliers$complier==1),covariates])
  colnames(X.exp.response)<-c(compl.var,covariates)

  response.mod <- SuperLearner::SuperLearner(Y=Y.exp.response,
                               X=X.exp.response,
                               SL.library=SL.library,
                               family=family,
                               id=ID)
  return(response.mod)
}

#' @export

pattc_counterfactuals<- function (pop.data,
                                  response.mod,
                                  id=NULL,
                                  cluster=NULL,
                                  cut.point=.5){
  compl.var<-pop.data$compl_var
  covariates<-all.vars(pop.data$response_formula)[-1]

  popdata<-pop.data$popdata
  popdata$c<-popdata[,compl.var]


  pop.tr.counterfactual <- cbind( 1, popdata[which(popdata$c==1),covariates])
  colnames(pop.tr.counterfactual)<-c(compl.var,covariates)
  pop.ctrl.counterfactual <- cbind(0,popdata[which(popdata$c==1),covariates])
  colnames(pop.ctrl.counterfactual)<-c(compl.var,covariates)

  Y.pred.1 <- predict(response.mod, pop.tr.counterfactual, onlySL = T)$pred
  Y.pred.0 <- predict(response.mod, pop.ctrl.counterfactual, onlySL = T)$pred

  if (potential.outcome) {
    Y.hat.1 <- ifelse(Y.pred.1>cut.point,1,0)
    Y.hat.0 <- ifelse(Y.pred.0>cut.point,1,0)
  } else if (!potential.outcome) {
    Y.hat.1 <- Y.pred.1
    Y.hat.0 <- Y.pred.0
  }

  if (!is.null(cluster)){
    clustervar <- pop.data[,cluster]
    Y.hats <- data.frame(Y_hat0 = Y.hat.0, Y_hat1 = Y.hat.1, cluster=clustervar)
  } else  {
    Y.hats <- data.frame(Y_hat0 = Y.hat.0, Y_hat1 = Y.hat.1)
  }
  return(Y.hats)
}

patt_ensemble <- function(response.formula,
                        exp.data,
                        pop.data,
                        treat.var,
                        compl.var,
                        createSL=TRUE,
                        ID=NULL,
                        cluster=NULL,
                        bootse=FALSE,
                        bootp = FALSE,
                        bootn = 999,
                        samedata=FALSE,
                        equivalence = FALSE)

{
  if (createSL) {
    create.SL()
  }

  expdata<- expcall(response.formula,
                    treat.var = treat.var,
                    compl.var = compl.var,
                    data= exp.data, ID=ID)

  popdata<-popcall(response.formula,
                   compl.var = compl.var,
                   data= exp.data, ID=ID)
  covariates <- all.vars(response.formula)[-1]
  compl.formula<- paste0(compl.var," ~ ",paste0(covariates,collapse = " + "))

  compl.mod<-complier_mod(expdata,ID=NULL,SL.library=NULL)

  compliers<-complier_predict(compl.mod,exp.data =expdata)

  response.mod <-  response_model(exp.data=expdata,
                                  exp.compliers=compliers,
                                  family="binomial",
                                  ID=NULL,
                                  SL.library=NULL)


  counterfactuals<-pattc_counterfactuals(pop.data=popdata,
                                         response.mod=response.mod,
                                         id=NULL,
                                         cluster=NULL,
                                         cut.point=.5)

  pattc<-WtC(x=counterfactuals$Y_hat1,
             y=counterfactuals$Y_hat0,
             bootse=bootse,
             bootp = bootp,
             bootn = bootn,
             samedata=samedata,
             equivalence = equivalence)

  return(pattc)
}





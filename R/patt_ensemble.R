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
  expdata <- data
  covariates <- as.data.frame(expdata$Xexp)
  complier.mod <- SuperLearner(Y=exp_prep$Cexp[which(exp_prep$Texp==1)],
                               X=covariates[which(exp_prep$Texp==1),],
                               SL.library=SL.library.class,
                               id=ID,
                               family="binomial")
  return(complier.mod)
}

#' @export
#'
complier_predict <- function(complier.mod,expdata) {
covariates <- as.data.frame(expdata$Xexp)
C.pscore <- predict(complier.mod, covariates, onlySL=TRUE)
rct.compliers <- data.frame("treatment"=as.data.frame(expdata$Texp)[,1],
                            "real_complier"=as.data.frame(expdata$Cexp)[,1],
                            "C.pscore"=C.pscore$pred,row.names = rownames(expdata$Yexp))
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
                         family,ID=NULL,
                         SL.library=NULL){

  if (family=="binomial" & is.null(SL.library) )  {
    SL.library <- define.SL.class.library()
  }

  Y.exp.response <- exp.data$Yexp[which(exp.compliers$compliers_all==1)]
  X.exp.response <- data.frame("complier"=exp.data$Cexp[which(exp.compliers$compliers_all==1)],
                               exp.data$Xexp[which(exp.compliers$complier==1),])
  response.mod <- SuperLearner(Y=Y.exp.response,
                               X=X.exp.response,
                               SL.library=SL.library,
                               family=family,
                               id=ID)
  return(response.mod)
}

#' @export

pattc_counterfactuals<- function (pop.data,response.mod,
                                  id=NULL,
                                  cluster=NULL){
  pop.tr.counterfactual <- cbind("complier" = 1,
                                 pop.data$Xpop[which(pop.data$Cpop==1),])
  pop.ctrl.counterfactual <- cbind("complier" = 0,
                                   pop.data$Xpop[which(pop.data$Cpop==1),])
  Y.hat.1 <- predict(response.mod, pop.tr.counterfactual, onlySL = T)$pred
  Y.hat.0 <- predict(response.mod, pop.ctrl.counterfactual, onlySL = T)$pred
  if (!is.null(cluster)){
    clustervar <- pop.data[,cluster]
    Y.hats <- data.frame(Y_hat1 = Y.hat.1, Y_hat0 = Y.hat.0,cluster=clustervar)
  }
  else
  {Y.hats <- data.frame(Y_hat1 = Y.hat.1, Y_hat0 = Y.hat.0)
  }
  return(Y.hats)
}


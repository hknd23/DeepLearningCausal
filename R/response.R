#' @export

response_model<-function(exp.data,exp.compliers,family,ID=NULL,SL.library=NULL){

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

pattc_counterfactuals<- function (pop.data,response.mod,id=NULL,cluster=NULL){
  if (!is.null(cluster)){
    clustervar <- pop.data[,cluster]
  } else {clustervar <- NULL}
  pop.tr.counterfactual <- cbind("complier" = 1,
                                 pop.data$Xpop[which(pop.data$Cpop==1),])
  pop.ctrl.counterfactual <- cbind("complier" = 0,
                                   pop.data$Xpop[which(pop.data$Cpop==1),])
  Y.hat.1 <- predict(response_model, pop.tr.counterfactual, onlySL = T)$pred
  Y.hat.0 <- predict(response_model, pop.ctrl.counterfactual, onlySL = T)$pred

  Y.hats <- data.frame(Y_hat1 = Y.hat.1, Y_hat0 = Y.hat.0,cluster=clustervar)

  return(Y.hats)
}

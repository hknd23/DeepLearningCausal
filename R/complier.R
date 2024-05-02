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
  covs <- as.data.frame(expdata$Xexp)
  complier.mod <- SuperLearner(Y=exp_prep$Cexp[which(exp_prep$Texp==1)],
                               X=covs[which(exp_prep$Texp==1),],
                               SL.library=SL.library.class,
                               id=ID,
                               family="binomial")
  return(complier.mod)
}

#' @export
#'
complier_predict <- function(complier.mod,expdata) {
covs <- as.data.frame(expdata$Xexp)
C.pscore <- predict(complier.mod, covs, onlySL=TRUE)
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

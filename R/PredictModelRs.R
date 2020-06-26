# Predict for liver injury and clinical status of six-category scale scores
#
# This is a function named 'PredictModelRs' for predicting liver injury after admission and clinical status level performed as six-category scale scores
#
# Author:Liang Yuan
# Data:2020/6/24

PredictModelRs <- function(data,outrs=F) {
  #load models
  data(SSS_model)
  data(li_model)
  #readRDS("data/SSS_model.RDS")->SSS_model
  #readRDS("data/liverinjury_model.RDS")->li_model

  #data preprocessing -> dat1 dat2
  data<-as.data.frame(data)
  SSS_index<-c("AST","DBIL","TT","LDH","NEUT#","CRP")
  dat1<-matrix(nrow = dim(data)[1])
  for(i in SSS_index){
    a<-data[,which(colnames(data)==i)]
    cbind(dat1,a)->dat1
  }
  dat1[,-1]->dat1
  colnames(dat1)<-SSS_index
  rownames(dat1)<-rownames(data)

  li_index<-c("ALT","CRP","LDH")
  dat2<-matrix(nrow = dim(data)[1])
  for(i in li_index){
    b<-data[,which(colnames(data)==i)]
    cbind(dat2,b)->dat2
  }
  dat2[,-1]->dat2
  colnames(dat2)<-li_index
  rownames(dat2)<-rownames(data)

  #predict
  sapply(rownames(dat1),function(x){
    predict(SSS_model,data[which(rownames(data)==x),])
  })->pred_SSS
  names(pred_SSS)<-rownames(dat1)
  sapply(rownames(dat2),function(x){
    predict(li_model,data[which(rownames(data)==x),])
  })->pred_li
  names(pred_li)<-rownames(dat2)

  rs<-data.frame(Pred_li=pred_li,Pred_SSS=pred_SSS)
  ifelse(rs$Pred_li>=0.5,"liver injury","non liver injury")->rs$Pred_li
  ifelse(rs$Pred_SSS>=0.5,"clinical status level high","clinical status level low")->rs$Pred_SSS

  #output
  if(outrs==F){
  sapply(rownames(rs),function(x){
    chara<-paste(x," patient may perform ",rs$Pred_li[which(rownames(rs)==x)]," and ",rs$Pred_SSS[which(rownames(rs)==x)],sep="")
    cat(chara,'\n')
  })
  }

  if(outrs==T) {
    print(rs)
  }
}

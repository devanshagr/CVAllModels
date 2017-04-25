cross_validate<-function(df,relation,n_iter,sr, model)
{
  df <- as.data.frame(df)
  library(MASS)
  library(randomForest)
  relation<-as.formula(relation)
  #We will compare two models- predictor
  mean_subset<-c();
  mean_all<-c();
  dep<-all.vars(terms(relation))[1]
  relation_all=as.formula(paste(dep,'.',sep="~"))
  relation_subset<-relation
  if(model=='lda'){
    for(i in 1:n_iter){
      sample <- sample.int(n = nrow(df), size = floor(sr*nrow(df)), replace = F)
      train <- df[sample, ]
      testing  <- df[-sample, ]
      first.lda<-lda(relation_subset, data=train)
      second.lda<-lda(relation_all, data=train)
      pred1.lda<-predict(first.lda,newdata=testing)$class
      pred2.lda<-predict(second.lda, newdata=testing)$class
      pred1.lda<-as.numeric(pred1.lda)
      pred2.lda<-as.numeric(pred2.lda)
      mean1<-mean((pred1.lda-testing[,dep])^2)
      mean2<-mean((pred2.lda-testing[,dep])^2)
      mean_subset<-c(mean_subset,mean1)
      mean_all<-c(mean_all,mean2)
    }
  }
  else if(model=='rf'){
    for(i in 1:n_iter){
      sample <- sample.int(n = nrow(df), size = floor(sr*nrow(df)), replace = F)
      train <- df[sample, ]
      testing  <- df[-sample, ]
      first.rf<-randomForest(relation_subset, data=train)
      second.rf<-randomForest(relation_all, data=train)
      pred1.rf<-predict(first.rf,newdata=testing)
      pred2.rf<-predict(second.rf, newdata=testing)
      pred1.rf<-as.numeric(pred1.rf)
      pred2.rf<-as.numeric(pred2.rf)
      mean1<-mean((pred1.rf-testing[,dep])^2)
      mean2<-mean((pred2.rf-testing[,dep])^2)
      mean_subset<-c(mean_subset,mean1)
      mean_all<-c(mean_all,mean2)
    }
  }
  else{
    print("Please enter correct model")
  }

  return (data.frame(accuracy_subset=mean_subset,accuracy_all= mean_all))
}

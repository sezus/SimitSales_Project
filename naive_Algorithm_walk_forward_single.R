naive_func_wf_single<-function(dataset,config='unistep',seasonal,size,freq){
  data_predict<-vector()
  dataset<- msts(dataset,start=c(2014,6), seasonal.periods = freq)
  #if(seasonal==FALSE){
  #  fit<-naive(dataset,size)
  #  data_predict<-fit$forecast[1:size]
  #  print("seasonala FALSE")
    
  #}
  #else{
    if (config=="multistep"){
      #for(i in 1:size){
        #fit<-snaive(dataset,h=1)
        fit<-snaive(dataset,h=12)
        print(fit)
        dataset<-c(dataset,fit$mean[1])
        #dataset<- msts(dataset,start=c(2014,6), seasonal.periods = freq)
      }

      data_predict<-dataset[(length(dataset)-(size-1)):length(dataset)]
      #print(dataset)
    #}else if (config=='unistep'){
    #  fit<-snaive(dataset, size)
    #  data_predict<-fit$mean[1:size]
      #print(dataset)
    #}
  #}
  return(data_predict)
}

#Model ve tahmin
walk_forward_validation_naive_wf_single<-function(dataset,cfg,h){
  ndiff=12
  lend<-length(dataset)
  train_set = dataset[1:(lend-ndiff)]
  len_train<-length(train_set)
  print("train")
  print(train_set)
  test_set = dataset[(lend-(ndiff*2)):lend]
  print("test_set")
  print(test_set)
  #print(test_set)
  pred=vector()
  for (i in c(0:(h-1))){
    data_forecast=naive_func_wf_single(train_set[1:(len_train-h)],as.character(cfg$y[1]),cfg$x[1],1,as.numeric(cfg$freq[1]))
    
    pred[i+1]<-as.numeric(data_forecast)
    print(i+1)
    print("test_set[i+1]")
    print(test_set[i+1])
    print(rmse(as.numeric(data_forecast),test_set[i+1]))
  }
  print("pred")
  print(pred)
  print("test")
  print(test_set[1:h])
  print((lend-ndiff):lend)
  plot(1:lend,dataset,type='l', main=cfg)
  lines(((lend-(ndiff-1)):lend) ,test_set[1:h],col = 'red')
  lines(((lend-(ndiff-1)):lend) ,pred,col = 'blue')
  rmse_<-rmse(pred,test_set[1:h])
  mse_<-mse(pred,test_set[1:h])
  mae_<-mae(pred,test_set[1:h])
  mape_<-mape(pred,test_set[1:h])
  smape_<-smape(pred,test_set[1:h])
  error<-data.frame(rmse=rmse_,mse=mse_,mae=mae_,mape=mape_,smape=smape_)
  #error<-rmse(pred,test_set[1:h])
  print("error")
  print(error)
  return(list(error,pred))
}

naive_configs<-function(freq){
  cfg<-merge(rbind(merge(c(TRUE),c("multistep","unistep")),c(FALSE,"unistep")),data.frame(freq))
  return(cfg)
}

#verilen farklÃ„Â± config kombinasyonlarÃ„Â±na gÃƒÂ¶re tek tek hata mesajÃ„Â±ni hesaplar
gridsearch_naive_wf_single<-function(dataset,cfg_list,h){
  #rmse_error<-1:dim(cfg_list)[1]
  rmse_error<-data.frame()
  pred<-data.frame()  
  for (i in 1:dim(cfg_list)[1]){
    #rmse_error[i]<-walk_forward_validation_naive_wf_single(dataset,cfg_list[i,],h)
    rmse_error<-rbind(rmse_error,walk_forward_validation_naive_wf_single(dataset,cfg_list[i,],h)[[1]])
    pred<-rbind(pred,walk_forward_validation_naive_wf_single(dataset,cfg_list[i,],h)[[2]])
    
  }
  return(list(cbind(cfg_list,rmse_error),cbind(cfg_list,pred)))
  
}
scores_model_naive_wf_single<-function(dataset,freq,tsize=0.7){
  current<-data.frame()
  if(sum(tsize<1)!=0){
    
    for(m in 1:length(tsize)){
      result<-gridsearch_naive(simit_Week$SalesCount,naive_configs(freq),tsize[m])
      result<-merge(as.numeric(result),as.numeric(tsize[m]))
      current<-rbind(current,result)
      #print(current)
    }  
    return(current)  
  }else{
    print("tsize 1'den kucuk deÃ„Å¸erler olmali dataset'in % kaci train kaci test'e ayrÃ„Â±lacak")
    return(tsize)
  }  
  
}



Control<-function(dataset,x,y){
  in1<-which(names(dataset) == x)
  in2<-which(names(dataset) == y)
  in3<-which(names(dataset) == "StoreNum")
  a<-vector()
  
  for(i in 1:length(unique(dataset[,in1]))){
    #Birinci deðiþkenin listesi
    il=unique(dataset[,in1])[i]
    print(il)
    print(length(dataset[dataset[,in1]==il,in2]))
    #O deðiþkene ait Store sayýsý
    print(length(unique(dataset[dataset[,in1]==il,in3])))
    #n<-length(unique(dataset[dataset[,in1]==il,in3]))
    a[i]=length(dataset[dataset[,in1]==il,in2])
    #print(length(dataset[dataset[,in1]==unique(dataset[,in1])[i],in2]))
    #print(a)
  }
  if(length(unique(as.numeric(a)))>1){
    print(unique(as.numeric(a)))
    
    return(print("hata var"))
  }else{
    print(unique(as.numeric(a)))
    return(print("hata yok"))
  }
}
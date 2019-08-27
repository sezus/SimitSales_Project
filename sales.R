sales<-function(xx){
  xx<-gsub("0.000","",xx)
  xx<-gsub("\\.","",xx)
  #print(sum(is.na(as.numeric(xx))))
  xx<-as.numeric(xx)
  return(xx)
}
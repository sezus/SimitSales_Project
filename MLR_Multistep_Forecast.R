y=names(ilsimit)[c(3,4,5,14,16,20:22)]
h=c(1:12)
errorsum<-vector(length=length(y))
print(a)
error<-vector(length=length(y))
f<-formula(paste0("SalesCount~",paste(y, collapse = "+" )))  
print(f)
pred<-vector()
len<-length(ilsimit$Weeks)
for (i in h){
  #print(i)
  model_lm<-lm(f,data=ilsimit[1:(len-i),])
  #print(summary(model_lm))
  #print(summary(lmodel)$coefficients[,4][order(summary(lmodel)$coefficients[,4])])
  error[i]<-summary(model_lm)$adj.r.squared
  data_forecast<-forecast(model_lm,ilsimit[(len-i+1):(len-i+1),c(3,4,5,14,16,20:22)],h=1)
  print(data_forecast$mean)
  pred[i]<-as.numeric(data_forecast$mean)
}
errorsum[a]=mean(error)
print(errorsum)
print(rmse(pred[12:1],ilsimit[(len-11):len,15]))
print(mse(pred[12:1],ilsimit[(len-11):len,15]))
print(mae(pred[12:1],ilsimit[(len-11):len,15]))
print(mape(pred[12:1],ilsimit[(len-11):len,15]))

plot(c(1:112),ilsimit$SalesCount[1:112],type="l")
lines(model_lm$fitted.values,col="red")
lines(c(101:112),pred[12:1],col="green")




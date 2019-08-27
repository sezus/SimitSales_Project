WeatherCombine_ilce<-function(main,weather){
  
  prepare_weather<-function(weather){
    options(digits = 1)
    weather$snow<-as.double(weather$snow)
    weather[is.na(weather$snow),]$snow<-rep(0,sum(is.na(weather$snow)))
    weather$temp<-as.double(weather$temp)
    weather$Max_temp<-as.double(weather$Max_temp)
    weather$Min_temp<-as.double(weather$Min_temp)
    weather$precip<-as.double(weather$precip)
    weather$gunes<-as.double(weather$gunes)
    return(weather)
  }
  
  
  prepare_sub_weather_ilce<-function(weather,main){
    sub_weather<-weather[(weather$StartDT %in% main$StartDT) & (tolower(weather$ilce) %in% tolower(main$ilce)),]
    return(sub_weather)
  }
  
  prepare_sub_weather_params_ilce<-function(sub_weather){
    print("sub_weather$temp_beforearrange")
    print(sub_weather$temp)
    sub_weather<-sub_weather[order(sub_weather$StartDT),]
    print(names(sub_weather))
    print(sub_weather)
    #sub_weather<-arrange(sub_weather,desc(sub_weather$StartDT))
    options(digits = 1)
    print("sub_weather$temp_afterarrange")
    print(sum(is.na(sub_weather$temp)))
    sub_weather[is.na(sub_weather$Max_temp),]$Max_temp<-as.double(sub_weather[is.na(sub_weather$Max_temp),]$temp)
    sub_weather[is.na(sub_weather$Min_temp),]$Min_temp<-as.double(sub_weather[is.na(sub_weather$Min_temp),]$temp)
    sub_weather[is.na(sub_weather$precip),]$precip<-as.double(mean(sub_weather[!is.na(sub_weather$precip),]$precip)) #imputation yapmak lazım
    sub_weather[is.na(sub_weather$gunes),]$gunes<-as.double(mean(sub_weather[!is.na(sub_weather$gunes),]$gunes)) #imputation yapmak lazım
    print(sub_weather)
    return(sub_weather)
  }
  
  source("Agg_Columns.R")
  main_ilce<-main[tolower(main$ilce) %in% tolower(weather$ilce),]
  weather$ilce<-tolower(weather$ilce)
  weather<-prepare_weather(weather)
  sub_weather<-data.frame()
  main_ilce$StartDT<-as.Date(main_ilce$StartDT)
  sub_weather<-prepare_sub_weather_ilce(weather,main_ilce)
  sub_weather<-prepare_sub_weather_params_ilce(sub_weather)

  temp<-Agg_Columns(sub_weather,c("temp","Max_temp","Min_temp","snow","precip","gunes"),c("ilce","StartDT"),mean)
  print(temp[is.na(temp$temp),]$ilce)
  main_ilce$StartDT<-as.character(main_ilce$StartDT)
  temp$StartDT<-as.character(temp$StartDT)
  
  weather_result<-merge(main_ilce,temp,by= c("ilce","StartDT"),all.x=TRUE)
  print("weather_result")
  print(unique(weather_result[is.na(weather_result$temp),]$ilce))    
  print(unique(main[!tolower(main$ilce) %in% tolower(weather$ilce),]$ilce))
  if(length(unique(weather_result[is.na(weather_result$temp),]$ilce))>0){
    print(unique(weather_result[is.na(weather_result$temp),]$il))
    list<-unique(weather_result[is.na(weather_result$temp),]$ilce)
    print(unique(main[tolower(main$ilce) %in% tolower(list),]$ilce))
    weather_result_il<-WeatherCombine(main[tolower(main$ilce) %in% tolower(list),],weather)
    weather_result<-rbind(weather_result[!is.na(weather_result$temp),],weather_result_il)
    print("weather_result")
    print(summary(weather_result))   
  }

  weather_result$StartDT<-as.Date(weather_result$StartDT)
  return(weather_result)
  
}
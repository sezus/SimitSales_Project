fillMissing<-function(store_df,support_df){
  #Eksik günü bul
  all <- seq(as.Date(min(store_df$StartDT)),as.Date(max(store_df$StartDT)),by="day")
  missingpart<-all[!all %in% store_df$StartDT]
  #boþ bir df oluþtur
  support_miss<-data.frame()
  for(i in 1:length(missingpart)){
    miss<-subset(support_df,support_df$StartDT==missingpart[i])
    support_miss<-rbind(support_miss,miss)
  }
  support_miss<-arrange(support_miss,support_miss$StartDT)
  m<-length(missingpart)
  #print(min(missingpart))
  n<-which((store_df$StartDT==min(missingpart)-1))
  n<-max(n)
  cat("support_miss$Pazar")

  missing_df <- data.frame(StartDT = rep(missingpart),
                           IsWeekend = support_miss$IsWeekend,
                           Pazartesi = support_miss$Pazartesi,
                           Sali = support_miss$Sali,
                           Carsamba = support_miss$Carsamba,
                           Persembe = support_miss$Persembe,
                           Cuma = support_miss$Cuma,
                           Cumartesi = support_miss$Cumartesi,
                           Pazar = support_miss$Pazar,
                           D1_Lt=c(store_df$SalesCount[n],rep(NA,(m-1))),
                           D2_Lt=c(store_df$SalesCount[n],rep(NA,(m-1))),
                           D3_Lt=c(store_df$SalesCount[n],rep(NA,(m-1))),
                           D4_Lt=c(store_df$SalesCount[n],rep(NA,(m-1))),
                           D5_Lt=c(store_df$SalesCount[n],rep(NA,(m-1))),
                           D6_Lt=c(store_df$SalesCount[n],rep(NA,(m-1))),
                           MagazaGrubu = rep(unique(store_df$MagazaGrubu),m),
                           MagazaTanimi = rep(unique(store_df$MagazaTanimi),m),
                           Urun = rep(unique(store_df$Urun),m),
                           #UrunGrubu = rep(unique(store_df$UrunGrubu),m),
                           SalesCount = rep(NA,m),
                           IsNationalHoliday =support_miss$IsNationalHoliday,
                           Ramazan=support_miss$Ramazan,
                           WeekDays = support_miss$WeekDays,
                           il = rep(unique(store_df$il),m),
                           ilce = rep(unique(store_df$ilce),m),
                           StoreNum = rep(unique(store_df$StoreNum),m),
                           Weeks = cut(rep(missingpart), breaks="weeks") ,
                           Year =cut(rep(missingpart), breaks="year"),
                           Month = cut(rep(missingpart), breaks="month"),
                           MonthDT = months.Date(as.POSIXct(missingpart,format = "%m")),
                           #ItemType = rep(unique(store_df$ItemType),m),
                           Price=support_miss$Price,
                           Change=support_miss$Change,
                           stringsAsFactors=FALSE)

  if(n>=6){
    missing_df$D2_Lt<-c(store_df$SalesCount[(n-1)],rep(NA,(m-1)))
    missing_df$D3_Lt<-c(store_df$SalesCount[(n-2)],rep(NA,(m-1)))
    missing_df$D4_Lt<-c(store_df$SalesCount[(n-3)],rep(NA,(m-1)))
    missing_df$D5_Lt<-c(store_df$SalesCount[(n-4)],rep(NA,(m-1)))
    missing_df$D6_Lt<-c(store_df$SalesCount[(n-5)],rep(NA,(m-1)))
  }
  
  if(m>=2 & n>1){
    missing_df$D2_Lt<-c(store_df$SalesCount[(n-1):n],rep(NA,(m-2)))
  }

  if(dim(store_df)[2]!=dim(missing_df)[2]){
    cat("fillMissing: names(store_df)")
    cat("fillMissing: names(missing_df)")
  }

  return(missing_df)
}
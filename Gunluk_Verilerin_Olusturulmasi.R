#1yıllık ve 2 yıllık magaza başına gunluk toplam satışlar
simitIMPUT_il_1 <-
  read.csv(
    "simitIMPUT_il_1.csv",
    sep = ",",
    header = TRUE,
    stringsAsFactors = FALSE
  )
simitIMPUT_il_1$X <- NULL
simitIMPUT_il_2 <-
  read.csv(
    "simitIMPUT_il_2.csv",
    sep = ",",
    header = TRUE,
    stringsAsFactors = FALSE
  )
simitIMPUT_il_2$X <- NULL
#simitIMPUT_il_1<-simitIMPUT_il_1_load[order(simitIMPUT_il_1$StartDT),]
simitIMPUT_il_1$Weeks <- as.factor(simitIMPUT_il_1$Weeks)
simitIMPUT_il_1$Year <- as.factor(simitIMPUT_il_1$Year)
simitIMPUT_il_1$Month <- as.factor(simitIMPUT_il_1$Month)
simitIMPUT_il_2$Weeks <- as.factor(simitIMPUT_il_2$Weeks)
simitIMPUT_il_2$Year <- as.factor(simitIMPUT_il_2$Year)
simitIMPUT_il_2$Month <- as.factor(simitIMPUT_il_2$Month)

source("Gunluk.R")
#1yıllık ve 2 yıllık il bazında gunluk toplam satışlar
#simitRF1$temp<-as.character(simitRF1$temp)
unique_isim<-c("Year","Month","Weeks","StartDT")
mean_isim<-c("D1_Lt","D2_Lt","D3_Lt","D4_Lt","D5_Lt","D6_Lt","Price","SalesCount","temp","Max_temp","Min_temp","snow","precip","gunes") 
any_isim<-c("IsNationalHoliday","Ramazan") 
agg_isim<-c("StartDT","il")
simitday_il1<-Gunluk(simitIMPUT_il_1,unique_isim,mean_isim,any_isim,agg_isim)
simitday_il2<-Gunluk(simitIMPUT_il_2,unique_isim,mean_isim,any_isim,agg_isim)
write.csv(simitday_il1, file = "simitday_il1.csv")
write.csv(simitday_il2, file = "simitday_il2.csv")

#1yıllık ve 2 yıllık gunluk toplam satışlar
unique_isim<-c("Year","Month","Weeks","StartDT") 
mean_isim<-c("D1_Lt","D2_Lt","D3_Lt","D4_Lt","D5_Lt","D6_Lt","Price","SalesCount","temp","Max_temp","Min_temp","snow","precip","gunes") 
any_isim<-c("IsNationalHoliday","Ramazan") 
agg_isim<-c("StartDT")
simitday_1<-Gunluk(simitIMPUT_il_1,unique_isim,mean_isim,any_isim,agg_isim)
simitday_2<-Gunluk(simitIMPUT_il_2,unique_isim,mean_isim,any_isim,agg_isim)
write.csv(simitday_1, file = "simitday_1.csv")
write.csv(simitday_2, file = "simitday_2.csv")


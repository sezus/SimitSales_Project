directory <- "/Users/sezgi.sener/Desktop/R_Veri_Temizleme_Kodlar"
setwd(directory)

#ReadFile
source("ReadData.R")
source("Libraries.R")
ss <- ReadData(directory, "ss.csv")
tumsatis <- ss
summary(ss)

#Yeni Özellikler
source("featureEngineering.R")
tumsatis <- featureEngineering(tumsatis)
tumsatis <- subset(tumsatis, tumsatis$StartDT >= "2014-01-01")


####### PROMOSYON + DUZ simit satis degerleri
simit <-
  tumsatis[tumsatis$Urun %in% unique(tumsatis$Urun)[c(grep("SMT", unique(tumsatis$Urun)), grep("SIM", unique(tumsatis$Urun)))], ]
#simit_withpromotion<-tumsatis[tumsatis$Urun %in% unique(tumsatis$Urun)[c(grep("SMT",unique(tumsatis$Urun)),grep("SIM",unique(tumsatis$Urun)))],]

simit$Urun <- "SIMIT"
simit$ItemType <- NULL
simit$UrunGrubu <- NULL

simit <-
  data.frame(
    simit %>% group_by(
      StartDT,
      IsWeekend,
      Pazartesi,
      Sali,
      Carsamba,
      Persembe,
      Cuma,
      Cumartesi,
      Pazar,
      MagazaGrubu,
      MagazaTanimi,
      Urun,
      IsNationalHoliday,
      Ramazan,
      WeekDays,
      il,
      ilce,
      StoreNum,
      Weeks,
      Year,
      Month,
      MonthDT,
      Price,
      Change
    ) %>% summarise(SalesCount = sum(SalesCount))
  )

#Promosyonda satilan simit verilerini de ekledigimiz isin
#Lag degerlerini yeniden hesaplatman gerekiyor
simit$D1_Lt <- lag(simit$SalesCount, 1)
simit$D2_Lt <- lag(simit$SalesCount, 2)
simit$D3_Lt <- lag(simit$SalesCount, 3)
simit$D4_Lt <- lag(simit$SalesCount, 4)
simit$D5_Lt <- lag(simit$SalesCount, 5)
simit$D6_Lt <- lag(simit$SalesCount, 6)

simit <- simit[, c(1:9, 26:31, 10:25)]

# [1] "StartDT"           "IsWeekend"         "Pazartesi"         "Sali"
# [5] "Carsamba"          "Persembe"          "Cuma"              "Cumartesi"
# [9] "Pazar"             "D1_Lt"             "D2_Lt"             "D3_Lt"
# [13] "D4_Lt"             "D5_Lt"             "D6_Lt"             "MagazaGrubu"
# [17] "MagazaTanimi"      "Urun"              "IsNationalHoliday" "Ramazan"
# [21] "WeekDays"          "il"                "ilce"              "StoreNum"
# [25] "Weeks"             "Year"              "Month"             "MonthDT"
# [29] "Price"             "Change"            "SalesCount"
write.csv(simit, file = "simit_all.csv")

##Missing Data
##Missing Data çalýþmasý yapýlmýþtýr
source("supportMissing.R")
source("Missing.R")

#Bos bir data frame olusturuyoruz.
support_df <- supportMissing(simit)
#Simit verisinde eksik gunleri olan magazalar belirleniyor.
Totalsimit <- missing(simit)
#Sonrasinda bu magazalara eksik gun sayisina göre siraliyoruz
Totalsimit <- arrange(Totalsimit, desc(Totalsimit$StoreDayCount))

#kayip gunsayisi 104den kucuk olan magazalar isin Kayip veri tahmini yaptiriyoruz
ttbir <- Totalsimit[Totalsimit$missing <= 104 &
                      Totalsimit$missing != 0, ]

source("imputeSimitRpart.R")
simitIMPUT <- imputeSimitRpart(simit, support_df, ttbir)
simitIMPUT$MagazaTanimi <- tolower(simitIMPUT$MagazaTanimi)

ikiYil <-
  Totalsimit[Totalsimit$totalDay > 730 &
               Totalsimit$missing < 150 & Totalsimit$missing < 150, ]
ikiYil <-
  ikiYil[ikiYil$totalDay - (ikiYil$lastDay - min(ikiYil$lastDay)) > 730, ]

##Max bir yillik magaza almak isin
birYil <-
  Totalsimit[Totalsimit$totalDay > 365 &
               Totalsimit$missing < 150 &
               Totalsimit$lastDay > as.Date("2016-05-05"), ]
birYil <-
  birYil[birYil$totalDay - (birYil$lastDay - min(birYil$lastDay)) > 365, ]
ikiYil <- ikiYil[ikiYil$id != 82, ]

simitIMPUT2 <-
  simitIMPUT[simitIMPUT$MagazaTanimi %in% ikiYil$storeName &
               simitIMPUT$StartDT <= min(ikiYil$lastDay) &
               simitIMPUT$StartDT >= max(ikiYil$firstDay), ]
simitIMPUT1 <-
  simitIMPUT[simitIMPUT$MagazaTanimi %in% birYil$storeName &
               simitIMPUT$StartDT <= min(birYil$lastDay) &
               simitIMPUT$StartDT >= max(birYil$firstDay), ]

#Hava Durumu
source("weatherCondition.R")
havax <- weatherCondition()
hava <- havax

#1 senelik ve 2 senelik datasetleri hava durumu verisiyle birlestirdik
source("lower_il.R")
source("WeatherCombine.R")
#bir yillik
simitIMPUT1 <- lower_il_ilce(simitIMPUT1)
simitIMPUT_il_1 <- WeatherCombine(simitIMPUT1, havax)

#iki yillik
simitIMPUT2 <- lower_il_ilce(simitIMPUT2)
simitIMPUT_il_2 <- WeatherCombine(simitIMPUT2, havax)

source("Control.R")
Control(simitIMPUT2, "StoreNum", "StartDT")
Control(simitIMPUT1, "StoreNum", "StartDT")#Normal olabilir kontrol etmek lazim
Control(simitIMPUT, "StoreNum", "StartDT")#tarihler esitlenmedi ondan farkli cikmasi normal

write.csv(simitIMPUT_il_1, file = "simitIMPUT_il_1.csv")
write.csv(simitIMPUT_il_2, file = "simitIMPUT_il_2.csv")

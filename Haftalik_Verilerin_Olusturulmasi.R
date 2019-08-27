#1yýllýk ve 2 yýllýk magaza baþýna toplam satýþlar
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

source("Haftalik.R")
#1yýllýk ve 2 yýllýk il bazýna haftalýk toplam satýþ
#simitRF1$temp<-as.character(simitRF1$temp)
unique_isim <- c("Year", "Month", "MonthDT")
mean_isim <-
  c(
    "D1_Lt",
    "D2_Lt",
    "D3_Lt",
    "D4_Lt",
    "D5_Lt",
    "D6_Lt",
    "Price",
    "SalesCount",
    "temp",
    "Max_temp",
    "Min_temp",
    "snow",
    "precip",
    "gunes"
  )
any_isim <- c("IsNationalHoliday", "Ramazan")
agg_isim <- c("Weeks", "il")
simithaftalik_il1 <-
  Haftalik(simitIMPUT_il_1, unique_isim, mean_isim, any_isim, agg_isim)
simithaftalik_il2 <-
  Haftalik(simitIMPUT_il_2, unique_isim, mean_isim, any_isim, agg_isim)
write.csv(simithaftalik_il1, file = "simithaftalik_il1.csv")
write.csv(simithaftalik_il2, file = "simithaftalik_il2.csv")
ilsimit <- simithaftalik_il2[simithaftalik_il2$il == "istanbul", ]
write.csv(ilsimit, file = "istanbul.csv")


#1yýllýk ve 2 yýllýk haftalýk toplam satýþ
unique_isim <- c("Year", "Month", "MonthDT")
mean_isim <-
  c(
    "D1_Lt",
    "D2_Lt",
    "D3_Lt",
    "D4_Lt",
    "D5_Lt",
    "D6_Lt",
    "Price",
    "SalesCount",
    "temp",
    "Max_temp",
    "Min_temp",
    "snow",
    "precip",
    "gunes"
  )
any_isim <- c("IsNationalHoliday", "Ramazan")
agg_isim <- c("Weeks")
simithaftalik_1 <-
  Haftalik(simitIMPUT_il_1, unique_isim, mean_isim, any_isim, agg_isim)
simithaftalik_2 <-
  Haftalik(simitIMPUT_il_2, unique_isim, mean_isim, any_isim, agg_isim)
simithaftalik_1 <- simithaftalik_1[order(simithaftalik_1$Weeks), ]
simithaftalik_2 <- simithaftalik_2[order(simithaftalik_2$Weeks), ]

write.csv(simithaftalik_1, file = "simithaftalik_1.csv")
write.csv(simithaftalik_2, file = "simithaftalik_2.csv")


for (i in unique(simitIMPUT_il_2$StoreNum)) {
  print(i)
  plot(
    as.Date(simit[simit$StoreNum == i, ]$StartDT),
    simit[simit$StoreNum == i, ]$SalesCount,
    type = 'l',
    xlab = simit[simit$StoreNum == i, ]$MagazaTanimi,
    ylab = 'GÃƒÂ¼n',
    main = i
  )
  lines(as.Date(simitIMPUT_il_2[simitIMPUT_il_2$StoreNum == i, ]$StartDT),
        simitIMPUT_il_2[simitIMPUT_il_2$StoreNum == i, ]$SalesCount,
        col = 'red')
  lines(as.Date(simitIMPUT_il_2[simitIMPUT_il_2$StoreNum == i &
                                  simitIMPUT_il_2$Ramazan == 1, ]$StartDT),
        simitIMPUT_il_2[simitIMPUT_il_2$StoreNum == i &
                          simitIMPUT_il_2$Ramazan == 1, ]$SalesCount,
        col = 'blue')
  lines(as.Date(simitIMPUT_il_2[simitIMPUT_il_2$StoreNum == i &
                                  simitIMPUT_il_2$IsNationalHoliday > 0, ]$StartDT),
        simitIMPUT_il_2[simitIMPUT_il_2$StoreNum == i &
                          simitIMPUT_il_2$IsNationalHoliday > 0, ]$SalesCount,
        col = 'green')
}
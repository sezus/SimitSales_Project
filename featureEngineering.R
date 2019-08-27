featureEngineering <- function(ss) {
  source("sales.R")
  #Satýþ degerlerini integer haline dönüþtürüyoruz.
  ss$D1_Lt <- sales(ss$D1_Lt)
  ss$D2_Lt <- sales(ss$D2_Lt)
  ss$D3_Lt <- sales(ss$D3_Lt)
  ss$D4_Lt <- sales(ss$D4_Lt)
  ss$D5_Lt <- sales(ss$D5_Lt)
  ss$D6_Lt <- sales(ss$D6_Lt)
  
  #StartDT gün deðerlerini tutuyor. Onu saat formatýna çeviriyoruz
  ss$StartDT <- as.Date(ss$StartDT, format = "%Y.%m.%d")
  
  #Dosyada isholiday gözüken deger aslýnda haftasonu mu deðil mi amaçlý kullanýlýyor.
  colnames(ss)[colnames(ss) %in% "IsHoliday"] <- "IsWeekend"
  
  #Magaza tipini küçük harfli hale getiriyoruz.
  ss$MagazaTanimi <- tolower(ss$MagazaTanimi)
  
  #Özellikler
  #1.Ulusal Tatil Gunleri
  #2.Ramazan
  #3.Haftanýn Günleri
  #4.il/ilce
  #5.StoreNum
  #6.Hafta/Yýl/Ay
  #7.Ýçecek/Yiyecek
  #8.Dolar
  
  #1.Ulusal Tatil günleri
  print("Tatil Gunleri")
  tatilgunleri <-
    read.csv(
      "tatilgunleri.csv",
      sep = ";",
      header = TRUE,
      stringsAsFactors = FALSE
    )
  tatilgunleri$Tarih <- paste(tatilgunleri$AY, ".", tatilgunleri$YIL)
  tatilgunleri$Tarih <-
    as.Date(gsub(" ", "", tatilgunleri$Tarih), format = "%d.%m.%Y")
  tatilgunleri$AY <- NULL
  tatilgunleri$YIL <- NULL
  print("IsNationalHoliday")
  ss$IsNationalHoliday <- 0
  tatilgunleri <-
    tatilgunleri[unique(tatilgunleri$Tarih) %in% unique(ss$StartDT), ]
  for (i in 1:length(tatilgunleri$Tarih)) {
    ss[ss$StartDT == tatilgunleri$Tarih[i], ]$IsNationalHoliday <-
      tatilgunleri$X[i]
  }
  remove(tatilgunleri)
  
  #2.Ramazan
  print("Ramazan")
  ss$Ramazan <- 0
  all14 <- seq(as.Date("2014-06-28"), as.Date("2014-07-27"), by = "day")
  all15 <- seq(as.Date("2015-06-17"), as.Date("2015-07-16"), by = "day")
  all16 <- seq(as.Date("2016-06-06"), as.Date("2016-07-04"), by = "day")
  ss[ss$StartDT %in% all14, ]$Ramazan <- 1
  ss[ss$StartDT %in% all15, ]$Ramazan <- 1
  ss[ss$StartDT %in% all16, ]$Ramazan <- 1
  
  #3.Haftanýn günleri
  print("WeekDays")
  ss[ss$Sali == 1, ]$Sali <- 2
  ss[ss$Carsamba == 1, ]$Carsamba <- 3
  ss[ss$Persembe == 1, ]$Persembe <- 4
  ss[ss$Cuma == 1, ]$Cuma <- 5
  ss[ss$Cumartesi == 1, ]$Cumartesi <- 6
  print(unique(ss$Cumartesi))
  ss[ss$Pazar == 1, ]$Pazar <- 7
  # Haftanýn günlerini tek parametre/kolon olarak gösterip kalanlarýný silelim
  ss$WeekDays <-
    paste(
      ss$Pazartesi,
      ss$Sali,
      ss$Carsamba,
      ss$Persembe,
      ss$Cuma,
      ss$Cumartesi,
      ss$Pazar,
      sep = ""
    )
  ss$WeekDays <- gsub("0", "", ss$WeekDays)
  
  #4.il
  print("il")
  print("ilce")
  #Lokasyon verileri
  loc <-
    read.csv(
      "xx.csv",
      sep = ";",
      header = TRUE,
      stringsAsFactors = FALSE
    )
  loc <- loc[1:4]
  ss$MagazaTanimi <- tolower(ss$MagazaTanimi)
  loc$CompanyLocationName <- tolower(loc$CompanyLocationName)
  unique(ss$MagazaTanimi)[unique(ss$MagazaTanimi) %in% (loc$CompanyLocationName)]
  unique(loc$CompanyLocationName)[!(loc$CompanyLocationName) %in% unique(ss$MagazaTanimi)]
  ss$il <- ""
  ss$ilce <- ""
  for (i in 1:length(loc$CompanyLocationName)) {
    a <- ss$MagazaTanimi == loc$CompanyLocationName[i]
    ss[a, ]$il <- loc$il[i]
    ss[a, ]$ilce <- loc$ilce[i]
  }
  
  #5.StoreNum
  print("StoreNum")
  unique(as.numeric(as.factor(ss$MagazaTanimi)))
  ss$StoreNum <- as.numeric(as.factor(ss$MagazaTanimi))
  
  
  #6.Weeks/Years/Months
  print("Weekr")
  print("Month")
  print("Year")
  ss <- ss[order(ss$StartDT), ]
  ss$Weeks <- cut(ss$StartDT, breaks = "weeks")
  ss$Year <- cut(ss$StartDT, breaks = "years")
  ss$Month <- cut(ss$StartDT, breaks = "month")
  ss$MonthDT <- months.Date(as.POSIXct(ss$StartDT, format = "%m"))
  
  
  #7.Ýçecek Yiyecekleri ayýralým
  print("ItemType")
  drinks <- c(
    "AYRAN",
    "BÝTKÝ ÇAYI",
    "ÇAY",
    "FROZEN",
    "GAZOZ",
    "KAHVE",
    "KOLA",
    "LÝMONATA",
    "LITRELIK ICECEK",
    "MADEN SUYU",
    "MEYVE SUYU",
    "MEYVE SUYU-SIKMA",
    "MÝLKSHAKE",
    "ORALET",
    "PORTAKAL SUYU",
    "SOÐUK ÇAY",
    "SU",
    "SÜT"
  )
  
  ss$ItemType <- "b"
  ss[ss$UrunGrubu %in% drinks, ]$ItemType <- "a"
  
  #8.Dolar
  print("Dolar")
  #Dolar verileri
  dolar <-
    read.csv(
      "dolar2.csv",
      sep = ",",
      header = TRUE,
      stringsAsFactors = TRUE
    )
  dolar$StartDT <- ""
  dolar$StartDT <- as.Date(parse_date_time(dolar$Date, orders = "mdy"))
  #dolar<-dolar[-806,]
  dolar <-
    dolar[dolar$StartDT >= min(ss$StartDT) &
            dolar$StartDT <= max(ss$StartDT), ]
  ss <- merge(ss, dolar[, c(2, 6, 9)], by = "StartDT", all = TRUE)
  dolarCuma <- dolar[weekdays(dolar$StartDT) == 'Cuma', ]
  
  for (i in 1:length(dolarCuma$StartDT)) {
    #print(dolarCuma$StartDT[i])
    price5 <- dolar[dolar$StartDT == dolarCuma$StartDT[i], ]$Price
    change5 <- dolar[dolar$StartDT == dolarCuma$StartDT[i], ]$Change
    #print(price5)
    ss[ss$StartDT %in% (dolarCuma$StartDT[i] + 1), ]$Price <- price5
    ss[ss$StartDT %in% (dolarCuma$StartDT[i] + 2), ]$Price <- price5
    ss[ss$StartDT %in% (dolarCuma$StartDT[i] + 1), ]$Change <-
      change5
    ss[ss$StartDT %in% (dolarCuma$StartDT[i] + 2), ]$Change <-
      change5
  }
  return(ss)
}
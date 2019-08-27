supportMissing <- function(satis) {
  ##simit verisinin ilk günden son güne zamansal olarak gün bilgisini bir seriye alalým.
  all <-
    seq(as.Date(min(satis$StartDT)), as.Date(max(satis$StartDT)), by = "day")
  m <- length(all)
  print(m)
  #Sonrasýnda bu gün sayýsýna göre bir boþ data.frame oluþturalým.
  #Ýlerde bunu eksik günleri bulmak ve doldurmak için kullanacaðýz.
  #Her bir maðaza için ayrý oluþturaçaðýmýz için sadece güne baðýmlý deðiþkenlerden oluþturuyoruz.
  xxxx <- satis[!duplicated(satis$StartDT), ]
  print(dim(xxxx))
  support_df <- data.frame(
    StartDT = rep(all),
    IsWeekend = xxxx$IsWeekend,
    Pazartesi = xxxx$Pazartesi,
    Sali = xxxx$Sali,
    Carsamba = xxxx$Carsamba,
    Persembe = xxxx$Persembe,
    Cuma = xxxx$Cuma,
    Cumartesi = xxxx$Cumartesi,
    Pazar = xxxx$Pazar,
    IsNationalHoliday = xxxx$IsNationalHoliday,
    Ramazan = xxxx$Ramazan,
    WeekDays = xxxx$WeekDays,
    Weeks = cut(rep(all), breaks = "weeks") ,
    Year = cut(rep(all), breaks = "year"),
    Month = cut(rep(all), breaks = "month"),
    MonthDT = months.Date(as.POSIXct(all, format = "%m")),
    Price = xxxx$Price,
    Change = xxxx$Change,
    stringsAsFactors = FALSE
  )
  return(support_df)
}
supportMissing <- function(satis) {
  ##simit verisinin ilk g�nden son g�ne zamansal olarak g�n bilgisini bir seriye alal�m.
  all <-
    seq(as.Date(min(satis$StartDT)), as.Date(max(satis$StartDT)), by = "day")
  m <- length(all)
  print(m)
  #Sonras�nda bu g�n say�s�na g�re bir bo� data.frame olu�tural�m.
  #�lerde bunu eksik g�nleri bulmak ve doldurmak i�in kullanaca��z.
  #Her bir ma�aza i�in ayr� olu�tura�a��m�z i�in sadece g�ne ba��ml� de�i�kenlerden olu�turuyoruz.
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
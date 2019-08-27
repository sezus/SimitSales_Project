fact <- function(satis) {
  satis$IsNationalHoliday <- as.factor(satis$IsNationalHoliday)
  satis$IsWeekend <- as.factor(satis$IsWeekend)
  satis$WeekDays <- as.factor(satis$WeekDays)
  satis$Ramazan <- as.factor(satis$Ramazan)
  satis$Year <- as.factor(satis$Year)
  satis$Month <- as.factor(satis$Month)
  satis$MonthDT <- as.factor(satis$MonthDT)
  return(satis)
}

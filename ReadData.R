ReadData <- function(directory, filename) {
  setwd(directory)
  data <-
    read.csv(
      "ss.csv",
      sep = ";",
      header = TRUE,
      stringsAsFactors = FALSE
    )
  data <- data[, c(
    "StartDT",
    "IsHoliday",
    "Pazartesi",
    "Sali",
    "Carsamba",
    "Persembe",
    "Cuma",
    "Cumartesi",
    "Pazar",
    "D1_Lt",
    "D2_Lt",
    "D3_Lt",
    "D4_Lt",
    "D5_Lt",
    "D6_Lt",
    "MagazaGrubu",
    "MagazaTanimi",
    "Urun",
    "UrunGrubu",
    "SalesCount"
  )]
  data$StartDT <- as.Date(data$StartDT, format = "%d.%m.%Y")
  print(names(data))
  summary(data)
  print(sapply(data, function(x)
    length(unique(x))))
  return(data)
}
weatherCondition <- function() {
  source("excel_parse.R")
  path <- paste0("C:/Users/sezgi.sener/Desktop/Diger/Meteo")
  
  Av_temp <-
    excel_parse(path, "201704207507_Gunluk_Ortalama_Sicaklik.xlsx")
  Max_temp <-
    excel_parse(path, "201704207507_Gunluk_Maksimum_Sicaklik.xlsx")
  Min_temp <-
    excel_parse(path, "201704207507_Gunluk_Minimum_Sicaklik.xlsx")
  snow <- excel_parse(path, "201704207507_Gunluk_kar_su_es_degeri.xlsx")
  Av_precip <-
    excel_parse(path, "201704207507_Gunluk_Ortalama_nem_yuzde.xlsx")
  Av_gunes <-
    excel_parse(path,
                "201704207507_Gunluk_toplam_guneslenme_suresi_saat.xlsx")
  names(Av_temp) <-
    c("Gun",
      "month",
      "temp",
      "year",
      "station_name",
      "station_id",
      "StartDT")
  names(Max_temp) <-
    c("Gun",
      "month",
      "Max_temp",
      "year",
      "station_name",
      "station_id",
      "StartDT")
  names(Min_temp) <-
    c("Gun",
      "month",
      "Min_temp",
      "year",
      "station_name",
      "station_id",
      "StartDT")
  names(snow) <-
    c("Gun",
      "month",
      "snow",
      "year",
      "station_name",
      "station_id",
      "StartDT")
  names(Av_precip) <-
    c("Gun",
      "month",
      "precip",
      "year",
      "station_name",
      "station_id",
      "StartDT")
  names(Av_gunes) <-
    c("Gun",
      "month",
      "gunes",
      "year",
      "station_name",
      "station_id",
      "StartDT")
  havad <-
    merge(Av_temp[, c(3, 6, 7)],
          Max_temp[, c(3, 6, 7)],
          by = c("station_id", "StartDT"),
          all.x = TRUE)
  havad <-
    merge(havad,
          Min_temp[, c(3, 6, 7)],
          by = c("station_id", "StartDT"),
          all.x = TRUE)
  havad <-
    merge(havad,
          snow[, c(3, 6, 7)],
          by = c("station_id", "StartDT"),
          all.x = TRUE)
  havad <-
    merge(havad,
          Av_precip[, c(3, 6, 7)],
          by = c("station_id", "StartDT"),
          all.x = TRUE)
  havad <-
    merge(havad,
          Av_gunes[, c(3, 6, 7)],
          by = c("station_id", "StartDT"),
          all.x = TRUE)
  il_ilce_meteo <-
    read.csv(
      "Il_Ilce.csv",
      sep = ";",
      header = TRUE,
      stringsAsFactors = FALSE
    )
  names(il_ilce_meteo) <-
    c("station_id",
      "ICAO",
      "il",
      "ilce",
      "station",
      "Gozlem_Grubu",
      "Gozlem_Turu")
  havad <-
    merge(havad, il_ilce_meteo[, c(1, 3, 4)], by = "station_id", all.x = TRUE)
  havad$il <- tolower(havad$il)
  options(digits = 2)
  havad$temp <- as.double(as.character(havad$temp))
  havad$Max_temp <- as.double(as.character(havad$Max_temp))
  havad$Min_temp <- as.double(as.character(havad$Min_temp))
  havad$snow <- as.double(as.character(havad$snow))
  havad$precip <- as.double(as.character(havad$precip))
  havad$gunes <- as.double(as.character(havad$gunes))
  return(havad)
}
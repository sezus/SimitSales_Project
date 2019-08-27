WeatherCombine <- function(main, weather) {
  prepare_weather <- function(weather) {
    weather$snow <- as.double(weather$snow)
    weather[is.na(weather$snow), ]$snow <-
      rep(0, sum(is.na(weather$snow)))
    weather$temp <- as.numeric(weather$temp)
    weather$Max_temp <- as.numeric(weather$Max_temp)
    weather$Min_temp <- as.numeric(weather$Min_temp)
    weather$precip <- as.numeric(weather$precip)
    weather$gunes <- as.numeric(weather$gunes)
    return(weather)
  }
  
  prepare_sub_weather <- function(weather, main) {
    sub_weather <-
      weather[(weather$StartDT %in% main$StartDT) &
                (weather$il %in% main$il), ]
    return(sub_weather)
  }
  prepare_sub_weather_params <- function(sub_weather) {
    sub_weather <- arrange(sub_weather, desc(sub_weather$StartDT))
    sub_weather[is.na(sub_weather$Max_temp), ]$Max_temp <-
      sub_weather[is.na(sub_weather$Max_temp), ]$temp
    sub_weather[is.na(sub_weather$Min_temp), ]$Min_temp <-
      sub_weather[is.na(sub_weather$Min_temp), ]$temp
    sub_weather[is.na(sub_weather$precip), ]$precip <-
      as.numeric(mean(sub_weather[!is.na(sub_weather$precip), ]$precip)) #imputation yapmak lazım
    sub_weather[is.na(sub_weather$gunes), ]$gunes <-
      as.numeric(mean(sub_weather[!is.na(sub_weather$gunes), ]$gunes)) #imputation yapmak lazım
    return(sub_weather)
  }
  
  source("Agg_Columns.R")
  weather <- prepare_weather(weather)
  sub_weather <- data.frame()
  main$StartDT <- as.Date(main$StartDT)
  sub_weather <- prepare_sub_weather(weather, main)
  sub_weather <- prepare_sub_weather_params(sub_weather)
  temp <-
    Agg_Columns(
      sub_weather,
      c("temp", "Max_temp", "Min_temp", "snow", "precip", "gunes"),
      c("il", "StartDT"),
      mean
    )
  main$StartDT <- as.character(main$StartDT)
  temp$StartDT <- as.character(temp$StartDT)
  weather_result <- merge(main, temp, by = c("il", "StartDT"), all.x = TRUE)
  
  print("last")
  print(weather_result[is.na(weather_result$temp), ])
  if (length(weather_result[is.na(weather_result$temp), ]$il) > 0) {
    list <- unique(weather_result[is.na(weather_result$temp), ]$il)
    print(list)
    for (i in list) {
      ortalama_sicaklik <-
        mean(weather_result[!is.na(weather_result$temp) &
                              weather_result$il == i, ]$temp)
      print(weather_result[is.na(weather_result$temp) &
                             weather_result$il == i, ]$temp)
      weather_result[is.na(weather_result$temp) &
                       weather_result$il == i, ]$temp <- ortalama_sicaklik
      print(weather_result[is.na(weather_result$temp) &
                             weather_result$il == i, ]$temp)
      ortalama_max <-
        mean(weather_result[!is.na(weather_result$Max_temp) &
                              weather_result$il == i, ]$Max_temp)
      weather_result[is.na(weather_result$Max_temp) &
                       weather_result$il == i, ]$Max_temp <- ortalama_max
      weather_result[is.na(weather_result$Min_temp) &
                       weather_result$il == i, ]$Min_temp <-
        mean(weather_result[!is.na(weather_result$Min_temp) &
                              weather_result$il == i, ]$Min_temp)
      weather_result[is.na(weather_result$snow) &
                       weather_result$il == i, ]$snow <- 0
      weather_result[is.na(weather_result$precip) &
                       weather_result$il == i, ]$precip <-
        mean(weather_result[!is.na(weather_result$precip) &
                              weather_result$il == i, ]$precip)
      weather_result[is.na(weather_result$gunes) &
                       weather_result$il == i, ]$gunes <-
        mean(weather_result[!is.na(weather_result$gunes) &
                              weather_result$il == i, ]$gunes)
    }
  }
  weather_result$StartDT <- as.Date(weather_result$StartDT)
  return(weather_result)
}
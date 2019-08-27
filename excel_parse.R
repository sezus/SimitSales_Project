excel_parse <- function(directory, name) {
  library("readxl")
  library(stringr)
  library(tidyr)
  last_directory <- getwd()
  setwd(directory)
  my_data <- read_excel(name)
  my_data <- data.frame(my_data)
  my_data[is.na(my_data)] <- "-"
  my_data[substr(my_data$X__1, 1, 3) == "Yýl", ]$X__1 <-
    gsub("Ý", "i", my_data[substr(my_data$X__1, 1, 3) == "Yýl", ]$X__1)
  my_data[substr(my_data$X__1, 1, 3) == "Yýl", ]$X__1 <-
    gsub("Ö", "O", my_data[substr(my_data$X__1, 1, 3) == "Yýl", ]$X__1)
  my_data[substr(my_data$X__1, 1, 3) == "Yýl", ]$X__1 <-
    gsub("ý", "i", my_data[substr(my_data$X__1, 1, 3) == "Yýl", ]$X__1)
  ind <- which(substr(my_data$X__1, 1, 3) == "Yil")
  year <- rep("", length(ind))
  station_name <- rep("", length(ind))
  station_id <- rep("", length(ind))
  df_data_gather = data.frame(
    Gun = rep("", 1),
    month = rep("", 1),
    weather_cond = rep("", 1),
    year = rep("", 1),
    station_name = rep("", 1),
    station_id = rep("", 1)
  )
  
  for (i in 1:length(ind)) {
    year[i] <- str_match(my_data[ind[i], 1], "Yil: (.*?)  istasyon")[2]
    station_name[i] <- str_match(my_data[ind[i], 1], "No: (.*?)/")[2]
    station_id[i] <-
      str_match(my_data[ind[i], 1], "^Yil:.*istasyon Adi.*/No:.*/(.*?)$")[2]
    df_data <- data.frame(my_data[(ind[i] + 4):(ind[i] + 34), 1:13])
    names(df_data) <- c("Gun", 1:12)
    df_data <- df_data %>%
      gather(key = month, value = weather_cond,-Gun)
    df_data$year <- rep(year[i], 372)
    df_data$station_name <- rep(station_name[i], 372)
    df_data$station_id <- rep(station_id[i], 372)
    df_data_gather <- rbind(df_data_gather, df_data)
  }
  df_data_gather <- df_data_gather[df_data_gather$weather_cond != '-', ]
  df_data_gather <- df_data_gather[df_data_gather$weather_cond != "", ]
  df_data_gather$StartDT <-
    as.Date(with(df_data_gather, paste(year, month, Gun, sep = "-")), "%Y-%m-%d")
  df_data_gather <- droplevels(df_data_gather)
  setwd(last_directory)
  print(getwd())
  return(df_data_gather)
}
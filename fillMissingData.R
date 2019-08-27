fillMissingData <- function(urun_df, support_df, Magaza) {
  library(dplyr)
  source("fillMissing.R")
  source("fact.R")
  source("Predictions.R")
  store_df <- urun_df[urun_df$MagazaTanimi == Magaza, ]
  cat("pazar")
  pazar <- sum(as.numeric(store_df$Pazar))
  missing_df <- fillMissing(store_df, support_df)
  
  if (pazar == 0) {
    cat("Pazar Kapalý")
    missing_df[missing_df$Pazar == 7, ]$SalesCount <-
      rep(0, length(missing_df[missing_df$Pazar == 7, ]$Pazar))
    store_df <- rbind(store_df, missing_df[missing_df$Pazar == 7, ])
    missing_df <- missing_df[missing_df$Pazar != 7, ]
    cat("Pazar Kapalý")
  }
  
  #print(store_df[store_df$Pazar==7,])
  missing_df <- Predictions(store_df, missing_df)
  ##Prediction içinde ya da dýþýnda store_df missing_df dimension check yapmak lazým.
  #missing_df <- redict(store_df, missing_df, type = "vector")
  if (pazar == 0) {
    missing_df <- rbind(missing_df, store_df[store_df$Pazar == 7, ])
  }
  return(missing_df)
  
}
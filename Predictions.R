Predictions <- function(store_df, missing_df) {
  #library(imputeTS)
  r <- nrow(store_df)
  store_df <- arrange(store_df, store_df$StartDT)
  missing_df <- arrange(missing_df, missing_df$StartDT)
  Predictions <- numeric()
  if (sum(is.na(store_df$D1_Lt)) != 0) {
    cat("NA'lar var")
  }
  for (i in 1:nrow(missing_df)) {
    f <-
      SalesCount ~ StartDT + IsWeekend + Pazartesi + Sali + Carsamba + Persembe +
      Cuma + Cumartesi + Pazar + D1_Lt + D2_Lt + D3_Lt + D4_Lt + D5_Lt + D6_Lt +
      IsNationalHoliday + Ramazan + WeekDays + Weeks + Year + Month + MonthDT + Price + Change
    n <- which(store_df$StartDT == missing_df$StartDT[i] - 1)
    n <- max(n)
    fitsirkeci <-
      rpart(
        SalesCount ~ IsWeekend + StartDT + Pazartesi + Sali + Carsamba + Persembe + Cuma + Cumartesi + Pazar +
          IsNationalHoliday + Ramazan + WeekDays + Weeks +
          Year + Month + MonthDT + Price + Change + D1_Lt,
        data = store_df,
        control = rpart.control(
          minsplit = 10,
          maxdepth = 30,
          cp = 0.000001
        ),
        method = "anova",
        na.action = na.omit
      )
    names(missing_df)
    names(store_df)
    
    Predictions[i] <-
      predict(fitsirkeci, missing_df[i, ], type = "vector")
    print(predict(fitsirkeci, missing_df[i, ], type = "vector"))
    missing_df$SalesCount[i] <- Predictions[[i]]
    
    n <- which(store_df$StartDT == missing_df$StartDT[i] - 1)
    n <- max(n)
    
    cat("D1_Lt")
    missing_df[i, ]$D1_Lt <- store_df$SalesCount[n]
    missing_df[i, ]$D2_Lt <- store_df$D1_Lt[n]
    missing_df[i, ]$D3_Lt <- store_df$D2_Lt[n]
    missing_df[i, ]$D4_Lt <- store_df$D3_Lt[n]
    missing_df[i, ]$D5_Lt <- store_df$D4_Lt[n]
    missing_df[i, ]$D6_Lt <- store_df$D5_Lt[n]

    if (missing_df$SalesCount[i] != "") {
      cat("Sales Count: ")
      store_df <- rbind(store_df, missing_df[i, ])
      cat("store Sales Count: ")
      store_df <- arrange(store_df, store_df$StartDT)
    }
    else{
      cat("Missing Sales Count Empty")
    }
    
  }
  #Tahmin edilmiþ alanýn çizimleri
  plot(
    store_df$StartDT,
    store_df$SalesCount,
    type = "l",
    col = "red",
    main = unique(store_df$MagazaTanimi)
  )
  lines(missing_df$StartDT, missing_df$SalesCount, col = "green")
  return(missing_df)
}
imputeSimitRpart <- function(simit, support_df, ttbir) {
  source("fillMissingData.R")
  simitRF <- simit
  for (i in 1:length(ttbir$storeName)) {
    if (ttbir$missing[i] != 0) {
      missing_df <- fillMissingData(simit, support_df, ttbir$storeName[i])
      dimt = dim(missing_df)[1] + dim(simitRF)[1]
      simitRF <- rbind(simitRF, missing_df)
      if (dimt != dim(simitRF)[1]) {
        cat("esit degil")
        print(ttbir$missing[i])
        print(dim(missing_df)[1])
        print(dimt)
      }
      cat("sum")
      simitRF <- arrange(simitRF, simitRF$StartDT)
      remove(missing_df)
    }
  }
  return(simitRF)
}
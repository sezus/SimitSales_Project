missing <- function(set) {
  #Her Magazanın toplam kaç günlük verisinin olduguna bakalım
  loc <-
    read.csv(
      "xx.csv",
      sep = ";",
      header = TRUE,
      stringsAsFactors = FALSE
    )
  loc <- loc[1:4]
  
  if (length(unique(set$MagazaTanimi)) > 1) {
    set <- set[order(set$MagazaTanimi), ]
  }
  
  #1.StoreDayCount : Her magazanın toplam kac günlük verisi var
  StoreDayCount <-
    tapply(set$StartDT, set$MagazaTanimi, function(x) {
      length(unique(x))
    })
  
  #2.StoreSales : Her magazanın toplam satış rakamları
  StoreSales <-
    tapply(set$SalesCount, set$MagazaTanimi, function(x)
      sum(x))
  
  #3.StoreSalesLen : Her magazanın sipariş sayısı
  StoreSalesLen <-
    tapply(set$SalesCount, set$MagazaTanimi, function(x)
      length(x))
  
  #4.StoreItem : Her magazanın kac çeşit ürün siparişi var
  StoreItem <-
    tapply(set$Urun, set$MagazaTanimi, function(x)
      length(unique(x)))
  
  #5. StoreItemGroup : Her magazanın kac çeşit ürün grubu satısı var
  #StoreItemGroup<-tapply(set$UrunGrubu,set$MagazaTanimi,function(x) length(unique(x)))
  
  #6. lastDay,firstDay
  lastDay <-
    tapply(set$StartDT, set$MagazaTanimi, function(x) {
      max(unique(x))
    })
  firstDay <-
    tapply(set$StartDT, set$MagazaTanimi, function(x) {
      min(unique(x))
    })
  class(lastDay) <- "Date"
  class(firstDay) <- "Date"
  nrow(set$StoreNum)
  nrow(set$MagazaT)
  StoreNum <-
    tapply(set$StoreNum, set$MagazaTanimi, function(x)
      unique(x))
  
  CompanyLocationName <- unique(loc$CompanyLocationName)
  CompanyLocationName <-
    CompanyLocationName[order(CompanyLocationName)]
  il <- unique(loc$il)
  TotalStoreData <- data.frame(
    storeName = tolower(names(lastDay)),
    lastDay,
    firstDay,
    totalDay = lastDay - firstDay,
    StoreDayCount,
    StoreSales,
    StoreItem,
    #StoreItemGroup,
    id = StoreNum
  )
  
  
  #Lets check if any missing day information for each store and Product btw those days
  lst <- tapply(set$StartDT, set$MagazaTanimi, function(x) {
    a <- seq(min(unique(x)), max(unique(x)), by = 1)
    a[!a %in% x]
  })
  TotalStoreData <- TotalStoreData[order(TotalStoreData$storeName), ]
  TotalStoreData$missing <- sapply(lst, function(x)
    length(x))
  remove(lastDay,
         firstDay,
         StoreSales,
         StoreItem,
         StoreItemGroup,
         StoreDayCount)
  return(TotalStoreData)
}
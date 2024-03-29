missing <- function(set) {
  #Her Magazan�n toplam ka� g�nl�k verisinin olduguna bakal�m
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
  
  #1.StoreDayCount : Her magazan�n toplam kac g�nl�k verisi var
  StoreDayCount <-
    tapply(set$StartDT, set$MagazaTanimi, function(x) {
      length(unique(x))
    })
  
  #2.StoreSales : Her magazan�n toplam sat�� rakamlar�
  StoreSales <-
    tapply(set$SalesCount, set$MagazaTanimi, function(x)
      sum(x))
  
  #3.StoreSalesLen : Her magazan�n sipari� say�s�
  StoreSalesLen <-
    tapply(set$SalesCount, set$MagazaTanimi, function(x)
      length(x))
  
  #4.StoreItem : Her magazan�n kac �e�it �r�n sipari�i var
  StoreItem <-
    tapply(set$Urun, set$MagazaTanimi, function(x)
      length(unique(x)))
  
  #5. StoreItemGroup : Her magazan�n kac �e�it �r�n grubu sat�s� var
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
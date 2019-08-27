Agg_Columns <-
  function(agg_dataset,
           columns,
           columns_agg,
           func = mean,
           option = "bulk") {
    agg_d <- data.frame()
    agg_f <- c("")
    col_f <- c("")
    for (i in columns_agg) {
      if (columns_agg[length(columns_agg)] == i) {
        agg = paste0(i)
        agg_f <- paste0(agg_f, agg)
      } else{
        agg = paste0(i, " +")
        agg_f <- paste0(agg_f, agg)
      }
    }
    for (i in columns) {
      if (columns[length(columns)] == i) {
        col = paste0(i)
        col_f <- paste0(col_f, col)
      } else{
        col = paste0(i, " +")
        col_f <- paste0(col_f, col)
      }
    }
    if (option != "bulk") {
      for (i in columns) {
        col = paste0(i)
        col_f <- paste0(col_f, col)
        f <- formula(paste0(col, "~", agg_f))
      }
      print(f)
      agg_d <- aggregate(f, data = agg_dataset, func)
    }
    else{
      for (i in columns) {
        col = paste0(i)
        #col_f<-paste0(col_f,col)
        f <- formula(paste0(col, "~", agg_f))
        #print(f)
        #print(func)
        
        if (dim(agg_d)[1] == 0) {
          agg_d <- aggregate(f, data = agg_dataset, func)
        }
        print(dim(agg_d))
        print(f)
        print(dim(aggregate(f, data = agg_dataset, func)))
        if (dim(agg_d)[1] != dim(aggregate(f, data = agg_dataset, func))[1]) {
          print("dimensions are different!! Becareful!")
          #return("NA")
        }
        else{
          agg_d <- merge(agg_d, aggregate(f, data = agg_dataset, func), all = TRUE)
        }
      }
    }
    
    return(agg_d)
  }
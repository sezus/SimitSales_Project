lower_il_ilce <- function(simit, option = "il") {
  if (option == "il") {
    simit$il <- tolower(simit$il)
    simit$il <- gsub("Ý", "i",  simit$il)
    simit$il <- gsub("ü", "u", simit$il)
    simit$il <- gsub("ð", "g", simit$il)
    simit$il <- gsub("ç", "c", simit$il)
    simit$il <- gsub("þ", "s", simit$il)
    simit$il <- gsub("ý", "i",  simit$il)
    simit$il <- gsub("afyonkarahisar", "afyon",  simit$il)
  } else {
    simit$ilce <- tolower(simit$ilce)
    simit$ilce <- gsub("Ý", "i",  simit$ilce)
    simit$ilce <- gsub("ü", "u", simit$ilce)
    simit$ilce <- gsub("ð", "g", simit$ilce)
    simit$ilce <- gsub("ç", "c", simit$ilce)
    simit$ilce <- gsub("þ", "s", simit$ilce)
    simit$ilce <- gsub("ý", "i",  simit$ilce)
    simit$ilce <- gsub("afyonkarahisar", "afyon",  simit$ilce)
  }
  
  return(simit)
}

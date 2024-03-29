lower_il_ilce <- function(simit, option = "il") {
  if (option == "il") {
    simit$il <- tolower(simit$il)
    simit$il <- gsub("�", "i",  simit$il)
    simit$il <- gsub("�", "u", simit$il)
    simit$il <- gsub("�", "g", simit$il)
    simit$il <- gsub("�", "c", simit$il)
    simit$il <- gsub("�", "s", simit$il)
    simit$il <- gsub("�", "i",  simit$il)
    simit$il <- gsub("afyonkarahisar", "afyon",  simit$il)
  } else {
    simit$ilce <- tolower(simit$ilce)
    simit$ilce <- gsub("�", "i",  simit$ilce)
    simit$ilce <- gsub("�", "u", simit$ilce)
    simit$ilce <- gsub("�", "g", simit$ilce)
    simit$ilce <- gsub("�", "c", simit$ilce)
    simit$ilce <- gsub("�", "s", simit$ilce)
    simit$ilce <- gsub("�", "i",  simit$ilce)
    simit$ilce <- gsub("afyonkarahisar", "afyon",  simit$ilce)
  }
  
  return(simit)
}

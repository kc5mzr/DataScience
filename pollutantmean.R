pollutantmean <- function(directory, pollutant, id = 1:332)
{
  filenames=Sys.glob(file.path("specdata",sprintf("*%03i.csv",id)))
  polution_data <- data.frame(do.call(rbind,lapply(filenames, function(x){read.csv(file=x,header=T)})))
  pdmean <- mean(polution_data[,pollutant],na.rm = TRUE)
  sprintf("%1.3f",pdmean)
}
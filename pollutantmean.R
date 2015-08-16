	## 'directory' is a character vector of length 1 indicating
        ## the location of the CSV files

        ## 'pollutant' is a character vector of length 1 indicating
        ## the name of the pollutant for which we will calculate the
        ## mean; either "sulfate" or "nitrate".

        ## 'id' is an integer vector indicating the monitor ID numbers
        ## to be used

        ## Return the mean of the pollutant across all monitors list
        ## in the 'id' vector (ignoring NA values)
        ## NOTE: Do not round the result!

pollutantmean <- function(directory, pollutant, id = 1:332) {
  # data files is a character vector of filenames based on passed id
  datafiles <- c(datafiles, paste(directory, "/", formatC(id, width=3, flag="0"), ".csv", sep=""))
  
  # read all files specified above
  data <- lapply(datafiles, read.csv)
  data <- do.call(rbind, data)
  
  #display sulfate or nitrate mean
  if (pollutant == "sulfate") 
  {
    sulfate <- mean(data[,2], na.rm = TRUE)
    sulfate <- round(sulfate, digits = 3)
    return(sulfate)
  } 
  else
  {
    nitrate <- mean(data[,3], na.rm = TRUE)
    nitrate <- round(nitrate, digits = 3)
    return(nitrate) 
  }       
}

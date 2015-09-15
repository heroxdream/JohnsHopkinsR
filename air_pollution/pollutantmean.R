pollutantmean = function(directory, pollutant, id = 1:332) {
  
  sum = 0
  entries = 0
  for (i in c(id)){
    str_i = as.character(i)
    while(nchar(str_i) < 3){
      str_i = paste("0", str_i, sep = "")
    }
    file_name = paste(directory, "/", str_i, ".csv", sep = "")
    tble = read.csv(file_name)
    pollutant_values = tble[[pollutant]]
    sum = sum + sum(pollutant_values, na.rm = TRUE)
    entries = entries + sum(!is.na(pollutant_values))
  }
   sum / entries
}


#combine all csv files generated by the news article retrieval code
filenames <- list.files("Washingtonpost", pattern="*.csv", full.names=TRUE)
for (i in 1:length(filenames)){
  if(i == 1){
    dataCom <- read.csv(filenames[i])
  }
  else {
    temp <- read.csv(filenames[i])
    dataCom <- rbind(dataCom, temp)
  }
}

#removing the r generated first column
dataCom <- dataCom[-1]

file_name <- paste("CompleteDataFinal" , "csv", sep=".")
write.csv(dataCom , file = file_name)
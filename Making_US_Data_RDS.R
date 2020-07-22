# Loading Libraries -------------------------------------------------------
library(RSelenium)
library(lubridate)
library(tidyverse)
library(Rspotify)
library(purrr)
library(sjmisc)
library(readr)

#####################################################################
##YOU MUST MANUALLY MOVE THE DOWNLOADED CSVs TO THE Raw_CSVs folder before running
#the code below
#####################################################################
# Turning the CSVs into Dataframes -----------------------------------------

files <- vector(mode = "character", length = number_of_days)
for (i in 1:number_of_days) {
  files[i] <- paste("regional-us-daily-",dates[i,], ".csv", sep = "")
}

##Test is used for appending then is eliminated
test <- tibble(Position="a",`Track Name`="b", Artist="c",Streams="d",URL="e",date="f")
for (i in 1:number_of_days) {
  print(i)
  tryCatch({  
    append <- read_csv(paste("Raw_CSVs/",as.character(files[i]), sep = ""), col_names = FALSE)
    append <- append[-1,]
    colnames(append) <- append[1,] 
    append <- append[-1,]
    append <- append %>% dplyr::mutate(date = as.character(dates[i,]))
    test <- rbind(test,append)}, error=function(e){print(e)})
}
US_Data <- test[-1,] %>% rename("Date"=date)

#Getting URI column
US_Data <- US_Data %>% mutate(url2=URL) %>% separate(url2, into = c(1,2,3,4,"URI"), sep="/") %>%
  dplyr::select(Date, Position, `Track Name`,Artist, Streams,URL,URI) 

#Saving Data
saveRDS(US_Data,"Data/US_Data.rds")

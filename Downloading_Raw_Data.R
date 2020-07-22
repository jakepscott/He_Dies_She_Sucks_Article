
# Loading Libraries -------------------------------------------------------
library(RSelenium)
library(lubridate)
library(tidyverse)
library(Rspotify)
library(purrr)
library(sjmisc)
library(readr)


# Getting Dates -----------------------------------------------------------

current_date <- Sys.Date()
number_of_days <- ymd(current_date) - ymd("2020-07-11")

dates <- vector(mode = "character", length = number_of_days)
for (i in 1:number_of_days) {
  dates[i] <- as.character(ymd(current_date) - i)
}

dates <- as_tibble(dates)


# Getting the URLs for the Top 200 Pages ----------------------------------

urls <- vector(mode = "character", length = number_of_days)
for (i in 1:number_of_days) {
  urls[i] <- paste("https://spotifycharts.com/regional/us/daily/",dates[i,], sep = "")
}
urls <-tibble(urls)


# Downloading The Top 200 Songs -------------------------------------------

remDr <- rsDriver(
  port = 4557L,
  browser = c("chrome", "firefox", "phantomjs", "internet explorer"),
  chromever = "83.0.4103.39")
remDr$client$open()

##Downloading CSVs into Downloads folder, then move them Raw_CSVs folder manually
for (i in 1:number_of_days) {
  tryCatch({
    print(i)
    remDr$client$navigate(as.character(urls[i,]))
    webElem <- remDr$client$findElement(using = "class name", "header-csv")
    webElem$clickElement()
  }, error=function(e){test <- print(e)})
}


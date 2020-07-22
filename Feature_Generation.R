# Loading Libraries -------------------------------------------------------
library(RSelenium)
library(lubridate)
library(tidyverse)
library(Rspotify)
library(purrr)
library(sjmisc)
library(readr)


# Getting the Song Features Themselves ------------------------------------

#Sourcing a file hidden to github to protect my Spotify key
source("Spotify_Key.R")

#Getting a row for each song in the full data (all 200 songs every day back to 2017)
unique_US_songs <- US_Data %>% select(`Track Name`, Artist, URI) %>% distinct()

#Getting the unique identifying ids
URIs <- tibble(URI=unique_US_songs$URI)

##Download features about the tracks
US_Features <- as_tibble(getFeatures("0nbXyq5TXYPCO7pr3N8S4I",token = keys))
US_Features <- US_Features[-1,]

for(i in 1:nrow(unique_US_songs)){
  print(i)
  print(URIs[i,])
  Sys.sleep(.1)
  US_Features<-as_tibble(rbind(US_Features,getFeatures(URIs[i,],token=keys)))
}

US_Features <- US_Features %>% dplyr::rename("URI"=id)


# Joining the Full Data and the Features for Every Song -------------------
US_Data <- left_join(US_Data,US_Features, by = "URI")



# Combining Top 200 Songs Data and Track Data -----------------------------

unique_songs <- US_Data %>% select(`Track Name`, Artist, URI) %>% distinct()
source(file="Set_Up_Files/Track_Info_Code.R")
US_Data <- left_join(US_Data, track_info, by = "URI")


# Combining Top 200 Songs Data and Album Data -----------------------------
Album_URIs <- US_Data %>% select(album_id) %>% distinct() %>% mutate(album_id=as.character(album_id))
source(file="Set_Up_Files/Album_Info_Code.r")
US_Data <- left_join(US_Data, Album_info, by = "album_id")


# Combining Top 200 Songs Data and Artist Data ----------------------------

Artist_Songs_Combo <- US_Data %>% dplyr::select(Artist, URI) %>% unique(.)
source(file="Set_Up_Files/Artist_Info_Code.r")
US_Data <- left_join(US_Data, Artist_info, by="Artist")


# Fixing Album Release Dates  ---------------------------------------------

unique_album_rds <- US_Data %>% select(album_release_date) %>% distinct() %>%
  mutate(album_release_date=as.character(album_release_date), nchar=nchar(album_release_date)) %>% 
  mutate(album_rd_correct=case_when(nchar==10~album_release_date,
                                    nchar==7~paste(album_release_date, "1", sep = "-"),
                                    nchar==4~paste(album_release_date,"1","1",sep = "-"))) %>% 
  select(-nchar)

###Joining
US_Data <- left_join(US_Data,unique_album_rds, by=c("album_release_date"))

###Getting number of days since release
US_Data <- US_Data %>% mutate(album_rd_correct=date(album_rd_correct),
                              Days_Since_Release=as.numeric(Date-album_rd_correct))

# Making Genre Variable ---------------------------------------------------

##Making the columns themselves: This creates a true false column for every genre
US_Data <- US_Data %>% mutate(hip_hop=str_detect(US_Data$genres,pattern=c("hip", "hop")),
                              pop=str_detect(US_Data$genres,pattern="pop"),
                              rap=str_detect(US_Data$genres,pattern="rap"),
                              trap=str_detect(US_Data$genres,pattern="trap"),
                              teen=str_detect(US_Data$genres,pattern="teen"),
                              electronic=str_detect(US_Data$genres,pattern=c("electronic", "electro", "edm", "dub")),
                              rock=str_detect(US_Data$genres,pattern=c("rock", "metal", "alternative")),
                              sad_feelings=str_detect(US_Data$genres,pattern=c("blues", "jazz", "emo", "punk", "soul", "reggae")),
                              RandB=str_detect(US_Data$genres,pattern="R&B"),
                              conscious=str_detect(US_Data$genres,pattern=c("conscious", "progressive")),
                              country=str_detect(US_Data$genres,pattern="country"),
                              other_genre=ifelse(hip_hop==FALSE &
                                                   pop==FALSE &
                                                   rap==FALSE &
                                                   trap==FALSE &
                                                   teen==FALSE &
                                                   electronic==FALSE &
                                                   rock==FALSE &
                                                   sad_feelings==FALSE &
                                                   RandB==FALSE &
                                                   conscious==FALSE &
                                                   country==FALSE,yes = TRUE,no = FALSE))


# Final Misc Cleaning ----------------------------------------------------------
#Making Streams Numeric
US_Data <- US_Data %>% mutate(Streams=as.numeric(Streams),
                              Position=as.numeric(Position))

##Making a dummy that says whether song is from an album or a single/other
US_Data <- US_Data %>% mutate(album_dummy=if_else(album_type=="album",TRUE,FALSE)) 

##Making a time signature dummy that is 1 if time signature is 4 zero otherwise
US_Data <- US_Data %>% mutate(time_signature_dummy=if_else(time_signature==4,1,0)) 

##Making album release data a date object
US_Data <- US_Data %>% mutate(album_release_date=as.Date(album_release_date))

##Renaming `Track Name` to Track_Name
US_Data <- US_Data %>% dplyr::rename("Track_Name"=`Track Name`)

#Renaming genre column
US_Data <- US_Data %>% rename("genre"=genres)

##getting Year, Month, Date columns
US_Data <- US_Data %>% mutate(Date=date(Date),
                              Year=year(Date),
                              Month=month(Date), 
                              Day=day(Date), 
                              Week=week(Date))

# Saving the Data ---------------------------------------------------------
Song_Features <- US_Data

saveRDS(Song_Features, file = "Data/Song_Features.rds")

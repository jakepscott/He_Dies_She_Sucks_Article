# Loading Libraries and Raw Data ------------------------------------------


library(tidyverse)
library(readr)
library(haven)
library(Rspotify)
library(geniusr)
library(stringr)
library(rvest)
library(stringi)
library(remotes)
library(genius)
Lyrics <- read_rds("Data/Song_Features.rds")

# Getting distinct songs --------------------------------------------------
distinct_songs <- US_Data %>% distinct(Track_Name, Artist, .keep_all = TRUE) %>% 
  select(Track_Name, Artist)


# Getting lyrics from Genius using the genius::genius_lyrics function --------

#Attempt 1: Using full track names
distinct_songs <- distinct_songs %>% mutate(lyrics=1)
for (i in 1:nrow(distinct_songs)) {
  tryCatch({
    print(i)
    distinct_songs$lyrics[i] <- genius_lyrics(artist = distinct_songs$Artist[i],
                                              song = distinct_songs$Track_Name[i],
                                              info = "simple") %>% dplyr::select(lyric)
  }, error=function(e){print(e)}
  )
}

#Checking the ones that I missed
missed <- distinct_songs[1,]
missed <- missed[-1,]

for (i in 1:nrow(distinct_songs)) {
  print(i)
  if (!is.character(distinct_songs$lyrics[[i]])) {
    missed[i,] <- distinct_songs[i,]
  } else {
    missed[i,] <- NA
  }
}

missed <- missed %>% filter(!is.na(Track_Name))


# Attempt 2: Simplifying the Track Names -----------------------------------
#Simplifying track names
missed <- missed %>% mutate(Track_Name2=Track_Name) %>% 
  separate(col = Track_Name2, into = c("Track_Name2", "extra"), sep = " [(]") %>%
  select(-extra) %>% 
  separate(Track_Name2, into = c("Track_Name2", "extra"), sep = " -") %>% select(-extra) %>% 
  separate(Track_Name2, into = c("Track_Name2", "extra"), sep = "-") %>% select(-extra)

attempt2 <- missed
##Trying to get lyrics using simplified names, resulting in attempt2
for (i in 1:nrow(missed)) {
  tryCatch({
    print(i)
    attempt2$lyrics[i] <- genius_lyrics(artist = missed$Artist[i],
                                        song = missed$Track_Name2[i],
                                        info = "simple") %>% dplyr::select(lyric)
  }, error=function(e){print(e)}
  )
}

attempt2

##Figuring out which songs I did not catch the lyrics of in attempt2
missed2 <- missed[1,]
missed2 <- missed2[-1,]

for (i in 1:nrow(attempt2)) {
  print(i)
  if (!is.character(attempt2$lyrics[[i]])) {
    missed2[i,] <- attempt2[i,]
  }  else {
    missed2[i,] <- NA
  }
}

missed2 <- missed2 %>% filter(!is.na(Track_Name))



# Attempt 3: Removing Punctuation --------------------------------------------------------------
##3rd attempt to get all lyrics, this time removing punctuation from artist name and track name
attempt3 <- missed2 %>% mutate(Track_Name3=str_remove_all(string = Track_Name2, pattern = "[[:punct:]]"),
                               Artist2=str_remove_all(string = Artist, pattern = "[[:punct:]]"))

##Removing the + sign, replacing with hyphen
attempt3 <- attempt3 %>% mutate(Artist2=str_replace(Artist2, pattern = " //+ ", replacement = "-"))


##Tring to catch the songs I didn't before, in attempt3

for (i in 1:nrow(attempt3)) {
  tryCatch({
    print(i)
    attempt3$lyrics[i] <- genius_lyrics(artist = attempt3$Artist2[i],
                                        song = attempt3$Track_Name3[i],
                                        info = "simple") %>% dplyr::select(lyric)
  }, error=function(e){print(e)}
  )
}

##Figuring out which songs I did not catch the lyrics of in attempt3
missed3 <- attempt3[1,]
missed3 <- missed3[-1,]

for (i in 1:nrow(attempt3)) {
  print(i)
  if (!is.character(attempt3$lyrics[[i]])) {
    missed3[i,] <- attempt3[i,]
  }   else {
    missed3[i,] <- NA
  }
}

missed3 <- missed3%>% filter(!is.na(Track_Name))


# Attempt 4. Simplifying a lot- parentheses, hypens, punctuation --------
##Making a fourth attempt
attempt4 <- missed3 %>% select(Track_Name, Artist, lyrics)

##Throwing the kitchen sink to try to get it to work- this time ridding of non-english letters/accents
attempt4 <- attempt4 %>% mutate(Track_Name2=Track_Name) %>% 
  separate(col = Track_Name2, into = c("Track_Name2", "extra"), sep = " [(]") %>%
  select(-extra) %>% 
  separate(Track_Name2, into = c("Track_Name2", "extra"), sep = " -") %>% select(-extra) %>% 
  separate(Track_Name2, into = c("Track_Name2", "extra"), sep = "-") %>% select(-extra) %>%
  mutate(Track_Name2=str_remove_all(string = Track_Name2, pattern = "[[:punct:]]"),
         Artist2=str_remove_all(string = Artist, pattern = "[[:punct:]]")) %>%
  mutate(Track_Name2= stri_trans_general(str = Track_Name2, id = "Latin-ASCII"),
         Artist2= stri_trans_general(str = Artist2, id = "Latin-ASCII"))


##Trying to get lyrics using simplified names, resulting in attempt4
for (i in 1:nrow(attempt4)) {
  tryCatch({
    print(i)
    attempt4$lyrics[i] <- genius_lyrics(artist = attempt4$Artist2[i],
                                        song = attempt4$Track_Name2[i],
                                        info = "simple") %>% dplyr::select(lyric)
  }, error=function(e){print(e)}
  )
}

missed4 <- attempt4[1,]
missed4 <- missed4[-1,]
for (i in 1:nrow(attempt4)) {
  print(i)
  if (!is.character(attempt4$lyrics[[i]])) {
    missed4[i,] <- attempt4[i,]
  }   else {
    missed4[i,] <- NA
  }
}

missed4 <- missed4 %>% filter(!is.na(Track_Name))



# Joining all the attempts ------------------------------------------------

##Combining attempt 3 and attempt4_full
attempt3_Full <- attempt3 %>% select(Track_Name, lyrics)

##Combining attempt 2 and attempt3_full
attempt2_Full <- attempt2 %>% select(Track_Name, lyrics)
attempt2_Full <- left_join(attempt2_Full,attempt3_Full, by="Track_Name") %>% mutate(lyrics=lyrics.x)

##Combining lyrics..x with lyrics.y
for (i in 1:nrow(attempt2_Full)) {
  if (is.double(attempt2_Full$lyrics.x[[i]])) {
    attempt2_Full$lyrics[i] <- attempt2_Full$lyrics.y[i]
  }
}

attempt2_Full <-attempt2_Full %>% select(-lyrics.x,-lyrics.y)

##Combining distinctsongs and attempt2_full
distinct_songs_full <- distinct_songs %>% select(Track_Name, Artist, lyrics)
distinct_songs_full <- left_join(distinct_songs_full,attempt2_Full, by="Track_Name") %>% mutate(lyrics=lyrics.x)

##Combining lyrics..x with lyrics.y
for (i in 1:nrow(distinct_songs_full)) {
  if (is.double(distinct_songs_full$lyrics.x[[i]])) {
    distinct_songs_full$lyrics[i] <- distinct_songs_full$lyrics.y[i]
  }
}

distinct_songs_full <-distinct_songs_full %>% select(-lyrics.x,-lyrics.y)

distinct_songs_full <- distinct_songs_full %>% distinct(Track_Name, Artist, .keep_all = TRUE)

##Checking after all that how many I still missed
missed_full <- distinct_songs_full[1,]
missed_full <- missed_full[-1,]

for (i in 1:nrow(distinct_songs_full)) {
  print(i)
  if (!is.character(distinct_songs_full$lyrics[[i]])) {
    missed_full[i,] <- distinct_songs_full[i,]
  } else {
    missed_full[i,] <- NA
  }
}
missed_full <- missed_full %>% filter(!is.na(Track_Name))


# Saving ------------------------------------------------------------------
Song_Lyrics <- distinct_songs_full
saveRDS(Song_Lyrics, "Data/Song_Lyrics.rds")

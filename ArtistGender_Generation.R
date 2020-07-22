library(tidyverse)
library(readr)
Song_Lyrics <- read_rds("Data/Song_Lyrics.rds")

distinct_artists <- Song_Lyrics %>% distinct(Artist) %>% pull(Artist)

genders <- rep("Female",length(distinct_artists))

for (i in 1:length(distinct_artists)) {
  genders_temp <- readline(paste("Is ",distinct_artists[i],"a male? Input m for male, o for other."))
  genders[i] <- case_when(genders_temp=="m"~"Male",
                          genders_temp=="o"~"Other",
                          TRUE~"Female")
}



saveRDS(genders,"Data/gendersV1.rds")

artist_genders <- cbind(distinct_artists,genders)
artist_genders <- as_tibble(artist_genders) %>% dplyr::rename("Artist"=distinct_artists,
                                                              "Artist_Gender"=genders)
saveRDS(artist_genders,"Data/Artist_Genders.rds")




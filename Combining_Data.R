library(readr)


# Song Features and Lyrics ------------------------------------------------


Song_Features <- read_rds("Data/Song_Features.rds")
Song_Lyrics <- read_rds("Data/Song_Lyrics.rds")

Data <- left_join(Song_Features,Song_Lyrics)
saveRDS(Data,"Data/Data.rds")


# Artist Genders ----------------------------------------------------------
artist_genders <- read_rds("Data/artist_genders.rds")
Data <- left_join(Data,artist_genders)
saveRDS(Data,"Data/Data.rds")



# Getting Artist Genres ---------------------------------------------------

Artist_Genres <- Data %>% dplyr::select(Artist,hip_hop:other_genre) %>% distinct(Artist,.keep_all = T)
saveRDS(Artist_Genres,"Data/Artist_Genres.rds")

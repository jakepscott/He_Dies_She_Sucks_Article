##Getting Keys
source("Spotify_Key.R")



##Here I am taking the list of artists and the URIs for all the songs of their in the data, nesting by artist, and making 
##mini data frames of all the songs they have in the list. Then I am taking just the first one. I only need the first one so
##I can run getTrack on it to get information on the song, including the artist URI, which I can then use to get artist info:)
Artist_Songs_Combo <- Artist_Songs_Combo %>% group_by(Artist) %>% nest() %>% 
  mutate(song_uri=as.character(data[[1]][1,])) %>% ungroup()

##Songs_uri is the same as Artist_Songs_Combo, but only with the song URIs, which makes my for loop easier to run
Songs_URIs <- Artist_Songs_Combo %>% select(song_uri)

##So the Artist_Songs_Combo is a tibble containing the artist and one of their songs. Now I will use the Song URI to get the 
##Artist URI
track_info <- as_tibble(getTrack("0nbXyq5TXYPCO7pr3N8S4I",token = keys))
track_info <- track_info[-1,]

for(i in 1:nrow(Songs_URIs)){#for each song
  tryCatch({print(i)
    print(Songs_URIs[i,])
    Sys.sleep(.1)
    track_info<-as_tibble(rbind(track_info,getTrack(Songs_URIs[i,],token=keys)))
  }, error=function(e){print(e)})
}

##Now I have a datafram containing both the artist names (main and featured) and the artist ids. I only want the mains! 
##Going to use seperate to get them:) In the future I could do something with features (eg feat Gucci), but I don' think it
##will add enough info to justify the work
track_info <- track_info %>% separate(artists_id,into = c("main_id","feat_id"), sep = ";") %>% select(-feat_id) %>%
  separate(artists, into = c("main_artist", "feat_artist"), sep = ";") %>% select(-feat_artist)

#Getting just the artist IDs in one dataset, for for loops later
Artist_IDs_combo <- track_info %>% dplyr::select(main_artist, main_id)
Artist_IDs_only <- Artist_IDs_combo %>% dplyr::select(main_id)


##FINALLY I can actually get the information about the artists themselves, I will just use a for loop, pretty standard
#These just initialize the tibble I will be binding my newly created rows to in the for loop
Artist_info <- as_tibble(getArtist("757aE44tKEUQEqRuT6GnEB",token = keys))
Artist_info <- Artist_info[-1,]

for(i in 1:nrow(Artist_IDs_only)){#for each song
  tryCatch({print(i)
    print(Artist_IDs_only[i,])
    Sys.sleep(.1)
    Artist_info<-as_tibble(rbind(Artist_info,getArtist(Artist_IDs_only[i,],token=keys)))
  }, error=function(e){})
}


Artist_info <- Artist_info %>% rename("Artist"=name)

source("Spotify_Key.R")


URIs <- tibble(URI=unique_songs$URI)

##Download features about the tracks
track_info <- as_tibble(getTrack("0nbXyq5TXYPCO7pr3N8S4I",token = keys))
track_info <- track_info[-1,]

for(i in 1:nrow(URIs)){#for each song
  tryCatch({print(i)
    print(URIs[i,])
    Sys.sleep(.1)
    track_info<-as_tibble(rbind(track_info,getTrack(URIs[i,],token=keys)))
  }, error=function(e){})
}

track_info <- track_info %>% rename("URI"=track_id) %>% separate(artists_id,into = c("main_id","feat_id"), sep = ";") %>% select(-feat_id) %>%
  separate(artists, into = c("main_artist", "feat_artist"), sep = ";") %>% dplyr::select(-feat_artist) %>% 
  dplyr::select(-main_artist) %>% rename("artist_id"=main_id) %>% dplyr::select(-name) %>% mutate(URI=as.character(URI)) %>% 
  dplyr::select(-popularity)



track_info <- track_info 
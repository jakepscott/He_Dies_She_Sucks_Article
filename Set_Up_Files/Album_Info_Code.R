source("Spotify_Key.R")


Album_info <- as_tibble(getAlbumInfo("6eV8O8De1mibErM1XM1tEc",token = keys))
Album_info <- Album_info[-1,]

for(i in 1:nrow(Album_URIs)){
  tryCatch({print(i)
    print(URIs[i,])
    Sys.sleep(.1)
    Album_info<-as_tibble(rbind(Album_info,getAlbumInfo(Album_URIs[i,],token=keys)[1,]))
  }, error=function(e){})
}

Album_info <- Album_info %>% rename("album_id"=id, "album_popularity"=popularity, "album_release_date"=release_date, 
                                    "album_name" = name) %>% 
  dplyr::select(-artist) %>% dplyr::select(-album_name)

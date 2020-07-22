# He Dies She Sucks: An Analysis of Gender and Language in Songs on Spotify
This repo contains the code I used to conduct analysis and generate figures for a Medium article called [He Dies, She Sucks: An Analysis of Gender and Language in Songs on Spotify](https://medium.com/@jakepscott16/he-dies-she-sucks-b4a8ac18cb73). In this article, I explore how the words associated with male pronouns (he, him, his) are very different than those associated with female ones (she, her, her). Men are characterized and/or associated with violence, whereas women are mostly sexualized. However, there is heterogeneity in terms of artist gender: while male artists do indeed associate themselves with violence and women with sex, female artists do neither.

![image](https://user-images.githubusercontent.com/56490913/88132737-4fb17780-cbae-11ea-827a-c31501f0a058.png)


## Getting Started

To see the results of this code, check out the [Medium article](https://medium.com/@jakepscott16/he-dies-she-sucks-b4a8ac18cb73). 

If you want to run or edit the code on your own computer,  download the code and data from this repo, and follow the instructions below. Please note, you will have to run the code itself to get the data. Unfortunately, besides the gender data, the rest of the data was too large for me to upload.

* Step 1: Run Downloading_Raw_Data.R file to get raw CSVs from Spotify. You may have to adjust the `rsDriver()` function parameters to suite your browser of choice.
*	Step 2: Move CSVs to Raw_CSVs folder (a solution where rSelenium automatically downloads into the Raw_CSVs folder is something I'd love, and have not found yet)
*	Step 3: Run Making_US_Data_RDS.R file to get US_Data RDS out of the raw CSVs
*	Step 4: Run Feature_Generation.R file (Note: the code has `source("Spotify_Key.R")` in the code. This is to protect my app ID, client ID, and client secret. To run the code yourself, you will need to your own and place it in the `spotifyOAuth()` function from the [Rspotify](https://cran.r-project.org/web/packages/Rspotify/Rspotify.pdf) package.  
*	Step 5: Run Lyric_Generation.R file
*	Optional Step 5.5: Run ArtistGender_Generation if you want to manually record gender. I have already done so, so it should be useable for the near future.
*	Step 6: Run Combining_Data.R 
*	Step 7: Run Counting_He_Versus_She.R to get figure 1 in the article
*	Step 8: Run Male_Versus_Female_Presence.R to get figure 2
*	Step 9: Run He_Versus_She for he and she figures
*	Step 10: Run Him_Versus_Her for him and her figures
*	Step 11: Run His_Versus_Her for his and her figures

### Prerequisites

To run the code on your computer you need R and the following packages installed and loaded:

```
library(RSelenium)
library(lubridate)
library(tidyverse)
library(Rspotify)
library(purrr)
library(sjmisc)
library(readr)
library(haven)
library(geniusr)
library(stringr)
library(rvest)
library(stringi)
library(remotes)
library(genius)
```

### Data Sets and Notable Packages
This code downloads, cleans, and uses the top 200 data from [Spotify](https://spotifycharts.com/regional). It uses lyric information from [Genius](https://genius.com/). 

## Author

* **Jake Scott** - [Twitter](https://twitter.com/jakepscott2020), [Medium](https://medium

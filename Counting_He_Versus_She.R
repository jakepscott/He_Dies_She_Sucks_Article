
# Loading Libraries -------------------------------------------------------


library(tidyverse)
library(readr)
library(tidytext)
library(plyr)
library(zoo)
library(ggtext)
library(scales)

windowsFonts(`Roboto Condensed`=windowsFont("Roboto Condensed"))

Song_Lyrics <- read_rds("Dataframes/Song_Lyrics.rds")
Data <- read_rds("Dataframes/Data.rds")

# Counting He Versus She --------------------------------------------------
Number_of_He_She <- tibble(total_words=1L,she=1L,he=1L,Track_Name="test",Artist="test")
Number_of_He_She <- Number_of_He_She[-1,]
for (i in 1:nrow(Song_Lyrics)) {
  print(i)
  tryCatch({
    #Getting number of he and she for a given a song
    append <- Song_Lyrics$lyrics[[i]] %>% as_tibble() %>% 
      dplyr::rename("text"=value) %>% 
      unnest_tokens(word, text, token = "ngrams", n = 1) %>% 
      dplyr::mutate(total_words=nrow(.)) %>% 
      dplyr::group_by(word) %>% 
      dplyr::mutate(he=sum(word=="he"),
                    she=sum(word=="she"),
                    Track_Name=Song_Lyrics$Track_Name[[i]],
                    Artist=Song_Lyrics$Artist[[i]]) %>% 
      ungroup() %>% 
      mutate(he=max(he),
             she=max(she)) %>% #This is because for some reason the he/she value becomes 0 if the word is not he/she 
      head(1) %>% 
      select(-word)
    #Binding Given Song to Full Data
    Number_of_He_She <- rbind.fill(Number_of_He_She,append)
  }, error=function(e){print(paste(e,"test"))})
}
Number_of_He_She <- Number_of_He_She %>% as_tibble() 


#Joining with Song_Lyrics
combined <- left_join(Data,Number_of_He_She)

he_she_over_time <- combined %>%
  dplyr::group_by(Date) %>%
  dplyr::summarise(he=sum(he,na.rm = T),
                   she=sum(she,na.rm = T)) %>% 
  ungroup() %>%
  mutate(total_he_she=(he+she),
         percent_he=(he/total_he_she)*100,
         percent_she=(she/total_he_she)*100,
         rollmean_percent_he=rollmean(percent_he,k = 7,fill = NA, align = "right"),
         rollmean_percent_she=rollmean(percent_she,k = 7,fill = NA, align = "right")) %>% 
  pivot_longer(cols=c("percent_he","percent_she","rollmean_percent_he","rollmean_percent_she"),
               names_to="he_vs_she",values_to="percent")


# Graphing ----------------------------------------------------------------


# Absolute ----------------------------------------------------------------
he_she_over_time <- he_she_over_time %>%
  filter(he_vs_she %in% c("percent_he","percent_she")) %>% 
  mutate(label = case_when(Date==max(Date) & he_vs_she=="percent_he"~"He",
                           Date==max(Date) & he_vs_she=="percent_she"~"She")) 

he_she_over_time %>% 
  filter(he_vs_she %in% c("percent_he","percent_she")) %>% 
  ggplot(aes(x=Date,y=percent/100)) +
  geom_line(aes(color=he_vs_she),lwd=1) +
  scale_color_manual(values = c("#0071CC","#E26A89")) +
  scale_y_continuous(labels=percent) +
  coord_cartesian(xlim=c(as.Date("2017-01-01"),as.Date("2020-08-15"))) +
  labs(y=NULL,
       x=NULL,
       fill=NULL,
       title="Since 2017, <span style='color: #E26A89'>**she**</span> has been used in songs more often than <span style='color: #0071CC'>**he**</span>",
       subtitle = "Percent of pronouns that are she versus he",
       caption = "Plot: @jakepscott2020 | Data: Spotify and Genius") +
  theme_minimal(base_family = "Source Sans Pro",base_size = 12) +
  theme(panel.grid.minor = element_blank(),
        plot.title = element_markdown(face = "bold", size = rel(1.2),hjust = .5),
        plot.subtitle = element_text(face = "plain", size = rel(1), color = "grey70"),
        plot.caption = element_text(face = "italic", size = rel(0.8), 
                                    color = "grey70"),
        legend.position = "none",
        axis.text.x = element_text(size=rel(1)),
        axis.text.y = element_text(size=rel(1)),
        plot.title.position = "plot")

#ggsave("Figures/he_she_usage.png",dpi=600)

# By Gender ---------------------------------------------------------------
he_she_over_time <- combined %>%
  dplyr::group_by(Date,Artist_Gender) %>%
  dplyr::summarise(he=sum(he,na.rm = T),
                   she=sum(she,na.rm = T)) %>% 
  ungroup() %>%
  mutate(total_he_she=(he+she),
         percent_he=(he/total_he_she)*100,
         percent_she=(she/total_he_she)*100,
         rollmean_percent_he=rollmean(percent_he,k = 7,fill = NA, align = "right"),
         rollmean_percent_she=rollmean(percent_she,k = 7,fill = NA, align = "right")) %>% 
  pivot_longer(cols=c("percent_he","percent_she","rollmean_percent_he","rollmean_percent_she"),
               names_to="he_vs_she",values_to="percent")

he_she_over_time %>% 
  filter(he_vs_she =="percent_she",
         Artist_Gender!="Other") %>% 
  ggplot(aes(x=Date,y=percent/100)) +
  geom_hline(yintercept = .5, linetype="dashed",color="grey70")+
  geom_hline(aes(yintercept = (he_she_over_time %>% filter(Artist_Gender=="Female",
                                                           he_vs_she=="percent_she") %>% pull(percent) %>% median())/100),
             color="#E26A89",
             lwd=1,
             linetype="dashed")+
  geom_hline(aes(yintercept = (he_she_over_time %>% filter(Artist_Gender=="Male",
                                                           he_vs_she=="percent_she") %>% pull(percent) %>% median())/100),
             color="#0071CC",
             lwd=1,
             linetype="dashed")+
  geom_line(aes(color=Artist_Gender),lwd=1) +
  scale_color_manual(values = c("#E26A89","#0071CC")) +
  scale_y_continuous(labels=percent) +
  coord_cartesian(xlim=c(as.Date("2017-01-01"),as.Date("2020-08-15"))) +
  labs(y=NULL,
       x=NULL,
       fill=NULL,
       title="Use of the word  \"*she*\" by <span style='color: #0071CC'>**male**</span> versus <span style='color: #E26A89'>**female**</span> artists",
       subtitle = "Percent of pronouns that are she, dotted lines represent full sample median",
       caption = "Plot: @jakepscott2020 | Data: Spotify and Genius") +
  theme_minimal(base_family = "Source Sans Pro",base_size = 12) +
  theme(panel.grid.minor = element_blank(),
        plot.title = element_markdown(face = "bold", size = rel(1.2)),
        plot.subtitle = element_text(face = "plain", size = rel(1), color = "grey70"),
        plot.caption = element_text(face = "italic", size = rel(0.8), 
                                    color = "grey70"),
        legend.position = "none",
        axis.text.x = element_text(size=rel(1)),
        axis.text.y = element_text(size=rel(1)),
        plot.title.position = "plot")
#ggsave("Figures/he_she_usage_by_gender.png",dpi=600)


# Loading Libraries -------------------------------------------------------
library(tidyverse)
library(readr)
library(tidytext)
library(plyr)
library(zoo)
library(ggtext)
library(scales)
library(patchwork)
library(ggrepel)
library(RColorBrewer)
windowsFonts(`Roboto Condensed`=windowsFont("Roboto Condensed"))


# Loading Raw Data --------------------------------------------------------
Data <- read_rds("Dataframes/Data.rds")


# Cleaning Data -----------------------------------------------------------
Male_vs_Female_Artists <- Data %>% 
  dplyr::group_by(Date,Artist_Gender) %>% 
  dplyr::summarise(n=n()) %>% 
  pivot_wider(names_from = Artist_Gender,
              values_from = n) %>% 
  dplyr::select(-Other) %>% 
  mutate(Total=Female+Male,
         Female_Per=Female/Total,
         Male_Per=Male/Total,
         rollmean_percent_he=rollmean(Male_Per,k = 7,fill = NA, align = "right"),
         rollmean_percent_she=rollmean(Female_Per,k = 7,fill = NA, align = "right")) %>% 
  pivot_longer(cols=c("rollmean_percent_he","rollmean_percent_she"),
               names_to="Gender",values_to="Percent")


# Graphing ----------------------------------------------------------------
ggplot(Male_vs_Female_Artists,aes(x=Date,y=Percent)) +
  geom_area(aes(fill=Gender)) +
  geom_hline(yintercept = .5,linetype="dashed",color="grey20") +
  scale_y_continuous(expand = c(0,0),labels = percent) +
  scale_fill_manual(values = c("#0071CC","#E26A89")) +
  scale_x_date(expand = c(0,0)) +
  labs(y=NULL,
       x=NULL,
       fill=NULL,
       title="4 out of 5 artists on the Spotify top 200 <br>list are <span style='color: #0071CC'>**men**</span>",
       caption = "Plot: @jakepscott2020 | Data: Spotify and Genius") +
  theme_minimal(base_family = "Roboto Condensed",base_size = 12) +
  theme(panel.grid.minor = element_blank(),
        plot.title = element_markdown(face = "bold", size = rel(2),hjust=.5),
        plot.subtitle = element_text(face = "plain", size = rel(1), color = "grey70"),
        plot.caption = element_text(face = "italic", size = rel(0.8), 
                                    color = "grey70"),
        legend.position = "none",
        axis.text.x = element_text(size=rel(1)),
        axis.text.y = element_text(size=rel(1)),
        plot.title.position = "plot")
ggsave("Figures/percent_of_artist_by_gender.png",dpi=600)

Data %>% 
  dplyr::group_by(Date,Artist_Gender) %>% 
  dplyr::summarise(n=n()) %>% 
  pivot_wider(names_from = Artist_Gender,
              values_from = n) %>% 
  dplyr::select(-Other) %>% 
  mutate(Total=Female+Male,
         Female_Per=Female/Total,
         Male_Per=Male/Total,
         rollmean_percent_he=rollmean(Male_Per,k = 7,fill = NA, align = "right"),
         rollmean_percent_she=rollmean(Female_Per,k = 7,fill = NA, align = "right")) %>% 
  pull(Female_Per) %>% 
  median()

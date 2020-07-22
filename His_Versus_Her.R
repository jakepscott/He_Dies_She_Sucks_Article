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
Song_Lyrics <- read_rds("Dataframes/Song_Lyrics.rds")
Data <- read_rds("Dataframes/Data.rds")

# Only Including Bigrams That Start With He or She ------------------------
Song_Lyrics_Bigram <- read_rds("Dataframes/Song_Lyrics_Bigram.rds")
His_Her_Bigrams <- Song_Lyrics_Bigram
for (i in 1:nrow(Song_Lyrics_Bigram)) {
  print(i)
  tryCatch({
    His_Her_Bigrams$lyrics[[i]] <- Song_Lyrics_Bigram$lyrics[[i]] %>% separate(bigram, c("word1", "word2"), sep = " ") %>%
      # Only choose rows where the first word is he or she
      filter(word1 %in% c("his","her")) %>% 
      dplyr::group_by(word1,word2) %>% 
      dplyr::summarise(n=n()) %>% 
      ungroup() %>% filter(word1 %in% c("his","her"))
  }, error=function(e){print(e)})
}
artist_genders <- read_rds("Dataframes/artist_genders.rds")

His_Her_Bigrams <- left_join(His_Her_Bigrams,artist_genders)

saveRDS(His_Her_Bigrams,"DataFrames/His_Her_Bigrams.rds")

# Unrolling To Look at All He/She Bigrams Since 2017 -------------------
His_Her_Bigrams <- read_rds("Dataframes/His_Her_Bigrams.rds")
Unrolled_His_Her_Bigrams <- His_Her_Bigrams$lyrics[[1]]
for (i in 1:nrow(His_Her_Bigrams)) {
  print(i)
  tryCatch({
    Unrolled_His_Her_Bigrams <- rbind(Unrolled_His_Her_Bigrams,His_Her_Bigrams$lyrics[[i]])
  }, error=function(e){print(e)})
}
Unrolled_His_Her_Bigrams <- Unrolled_His_Her_Bigrams %>% filter(word1=="his" | word1=="her")
saveRDS(Unrolled_His_Her_Bigrams,"DataFrames/Unrolled_His_Her_Bigrams.rds")

# Prepping the Data -------------------------------------------------------
Unrolled_His_Her_Bigrams <- read_rds("Dataframes/Unrolled_His_Her_Bigrams.rds")

top_10_raw_words <- Unrolled_His_Her_Bigrams %>% 
  dplyr::group_by(word1,word2) %>% 
  dplyr::summarise(n=sum(n,na.rm = T)) %>% 
  ungroup() %>% 
  dplyr::group_by(word1) %>% 
  dplyr::mutate(total=sum(n)) %>% 
  ungroup() %>% 
  mutate(percent=n/total) %>% 
  arrange(word1,desc(percent)) %>% 
  dplyr::group_by(word1) %>% 
  slice(1:10)

Unrolled_His_Her_Bigrams <- read_rds("Dataframes/Unrolled_His_Her_Bigrams.rds")

top_10_raw_words <- Unrolled_His_Her_Bigrams %>% 
  dplyr::group_by(word1,word2) %>% 
  dplyr::summarise(n=sum(n,na.rm = T)) %>% 
  ungroup() %>% 
  dplyr::group_by(word1) %>% 
  dplyr::mutate(total=sum(n)) %>% 
  ungroup() %>% 
  mutate(percent=n/total) %>% 
  arrange(word1,desc(percent)) %>% 
  dplyr::group_by(word1) %>% 
  slice(1:10)

# Graphing Raw His Versus Her----------------------------------------------------------------
(a <- top_10_raw_words %>% filter(word1=="his") %>% 
   ggplot(aes(x=fct_reorder(word2,percent),y=percent)) +
   geom_col(fill="#0071CC") +
   scale_y_continuous(label=percent) +
   geom_text(aes(x=word2,y=percent,label=str_to_upper(word2)),
             color="white",
             hjust=1.2,
             family="Roboto Condensed",
             size=4) +
   coord_flip(ylim=(c(0,.06))) +
   labs(y=NULL,
        x=NULL,
        fill=NULL,
        title=NULL,
        subtitle =NULL,
        caption = "Plot: @jakepscott2020 | Data: Spotify and Genius") +
   theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
   theme(panel.grid = element_blank(),
         plot.title = element_markdown(face = "bold", size = rel(1.2)),
         plot.subtitle = element_text(face = "plain", size = rel(1), color = "grey70"),
         plot.caption = element_text(face = "italic", size = rel(0.8), 
                                     color = "grey70"),
         legend.position = "none",
         axis.text.x = element_text(size=rel(.8)),
         axis.text.y = element_blank(),
         plot.title.position = "plot"))

(b <- top_10_raw_words %>% filter(word1=="her") %>% 
    ggplot(aes(x=fct_reorder(word2,percent),y=percent)) +
    geom_col(fill="#E26A89") +
    geom_text(aes(x=word2,y=percent,label=str_to_upper(word2)),
              color="white",
              hjust=1.2,
              family="Roboto Condensed",
              size=4) +
    scale_y_continuous(label=percent) +
    coord_flip(ylim=(c(0,.06))) +
    labs(y=NULL,
         x=NULL,
         fill=NULL,
         title=NULL,
         subtitle =NULL) +
    theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
    theme(panel.grid = element_blank(),
          plot.subtitle = element_text(face = "plain", size = rel(1), color = "grey70"),
          legend.position = "none",
          axis.text.x = element_text(size=rel(.8)),
          axis.text.y = element_blank(),
          plot.title.position = "plot"))
b + a + 
  plot_layout(ncol = 1) +
  plot_annotation(title="The words associated with <span style='color: #E26A89'>**her**</span> and <span style='color: #0071CC'>**his**</span> are similar and boring <br>in absolute terms",
                  subtitle = "Measured as percent of total words associated with her and his respectively",
                  theme = theme(plot.title = element_markdown(face = "bold", size = rel(1.2)),
                                plot.subtitle = element_text(face = "plain", size = rel(1), color = "grey70")))

ggsave("Figures/Her_His_Words_Absolute.png",dpi=600)


# Removing Stop Words -----------------------------------------------------

Unrolled_His_Her_Bigrams <- read_rds("Dataframes/Unrolled_His_Her_Bigrams.rds")

stop_words_fixed <- stop_words %>% dplyr::rename("word2"=word)

Unrolled_His_Her_Bigrams <- anti_join(Unrolled_His_Her_Bigrams,stop_words_fixed,by="word2")

top_10_nonstop_words <- Unrolled_His_Her_Bigrams %>%
  filter(!(word2 %in% c("gon","wanna"))) %>% 
  dplyr::group_by(word1,word2) %>% 
  dplyr::summarise(n=sum(n,na.rm = T)) %>% 
  ungroup() %>% 
  dplyr::group_by(word1) %>% 
  dplyr::mutate(total=sum(n)) %>% 
  ungroup() %>% 
  mutate(percent=n/total) %>% 
  arrange(word1,desc(percent)) %>% 
  dplyr::group_by(word1) %>% 
  slice(1:10) %>% 
  dplyr::mutate(word2=case_when(word2=="fuck"~"f*ck",
                                TRUE~as.character(word2)))

(a <- top_10_nonstop_words %>% filter(word1=="his") %>% 
    ggplot(aes(x=fct_reorder(word2,percent),y=percent)) +
    geom_col(fill="#0071CC") +
    geom_text(aes(x=word2,y=percent,label=str_to_upper(word2)),
              color="white",
              hjust=1.2,
              family="Roboto Condensed",
              size=4) +
    scale_y_continuous(label=percent) +
    coord_flip(ylim=(c(0,.07))) +
    labs(y=NULL,
         x=NULL,
         fill=NULL,
         title=NULL,
         subtitle =NULL,
         caption = "Plot: @jakepscott2020 | Data: Spotify and Genius") +
    theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
    theme(panel.grid = element_blank(),
          plot.title = element_markdown(face = "bold", size = rel(1.2)),
          plot.subtitle = element_text(face = "plain", size = rel(1), color = "grey70"),
          plot.caption = element_text(face = "italic", size = rel(0.8), 
                                      color = "grey70"),
          legend.position = "none",
          axis.text.x = element_text(size=rel(.8)),
          axis.text.y = element_blank(),
          plot.title.position = "plot"))

(b <- top_10_nonstop_words %>% filter(word1=="her") %>% 
    ggplot(aes(x=fct_reorder(word2,percent),y=percent)) +
    geom_col(fill="#E26A89") +
    geom_text(aes(x=word2,y=percent,label=str_to_upper(word2)),
              color="white",
              hjust=1.2,
              family="Roboto Condensed",
              size=4) +
    scale_y_continuous(label=percent) +
    coord_flip(ylim=(c(0,.07))) +
    labs(y=NULL,
         x=NULL,
         fill=NULL,
         title=NULL,
         subtitle =NULL) +
    theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
    theme(panel.grid = element_blank(),
          plot.subtitle = element_text(face = "plain", size = rel(1), color = "grey70"),
          legend.position = "none",
          axis.text.x = element_text(size=rel(.8)),
          axis.text.y = element_blank(),
          plot.title.position = "plot"))
b + a + 
  plot_layout(ncol = 1) +
  plot_annotation(title="The words following <span style='color: #0071CC'>**his**</span> and <span style='color: #E26A89'>**her**</span> in songs <br>are quite different",
                  subtitle = "Measured as percent of total words associated with her and his respectively",
                  theme = theme(plot.title = element_markdown(face = "bold", size = rel(2), hjust = .5),
                                plot.subtitle = element_text(face = "plain", size = rel(1), color = "grey70",hjust=.5)))
ggsave("Figures/His_Her_Words_No_Stop_Words.png",dpi=600)


# Looking at Word Ratios -------------------------------------------------
Unrolled_His_Her_Bigrams <- read_rds("Dataframes/Unrolled_His_Her_Bigrams.rds")
stop_words_fixed <- stop_words %>% dplyr::rename("word2"=word)
Unrolled_His_Her_Bigrams <- anti_join(Unrolled_His_Her_Bigrams,stop_words_fixed,by="word2")

count <- Unrolled_His_Her_Bigrams %>%
  filter(!(word2 %in% c("gon","wanna"))) %>% 
  dplyr::group_by(word1,word2) %>% 
  dplyr::summarise(n=sum(n,na.rm = T)) %>% filter(n>10)

#Getting ratios
(word_ratios <- count %>%
  # Spread out the word1 column so that there's a column named "he" and one named "she"
  pivot_wider(names_from=word1, values_from= n, values_fill = 0) %>%
  # Add 1 to each number so that logs work (just in case any are zero)
  mutate_if(is.numeric, ~(. + 1) / sum(. + 1)) %>%
  # Create a new column that is the logged ratio of the she counts to he counts
  mutate(logratio = log2(her / his)) %>%
  # Sort by that ratio
  arrange(desc(logratio)) %>% 
  # This gets the words in the right order---we take the absolute value, select
  # only rows where the absolute value of the log ratio is bigger than 0, and then take the top 15 words
  mutate(abslogratio = abs(logratio)) %>%
  group_by(logratio < 0) %>%
  top_n(10, abslogratio) %>%
  ungroup() %>%
  dplyr::mutate(word = reorder(word2, logratio),
                word=case_when(word=="fuck"~"f*ck",
                               word=="fucked"~"f*cked",
                               word=="pussy"~"p*ssy",
                               word=="bitch"~"b*tch",
                               TRUE~as.character(word)))) 

#Graphing
(a <- ggplot(word_ratios, aes(y = fct_reorder(word,-logratio), x = logratio)) +
    geom_col(aes(fill=logratio<0)) +
    geom_text(data=filter(word_ratios,logratio>0),
              aes(x=logratio,y=word,label=str_to_upper(word)),
              color="white",
              hjust=1.2,
              family="Roboto Condensed",
              size=4) +
    geom_text(data=filter(word_ratios,logratio<0),
              aes(x=logratio,y=word,label=str_to_upper(word)),
              color="white",
              hjust=-.1,
              family="Roboto Condensed",
              size=4) +
    geom_vline(xintercept=0, color="grey10") +
    coord_cartesian(xlim=c(-6.5,6.5)) +
    scale_fill_manual(values = c("#E26A89","#0071CC")) +
    labs(x = "How much more/less likely", y = NULL,
         caption = "Plot: @jakepscott2020 | Data: Spotify and Genius") +
    scale_x_continuous(breaks = seq(-6, 6,2),
                       labels = c("32x", "16x","4x", "Same", "4x", "16x","32x")) +
    theme_bw(base_family = "Roboto Condensed",base_size = 12) +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y=element_blank(),
          plot.title = element_markdown(face = "bold", size = rel(1.2)),
          plot.subtitle = element_text(face = "plain", size = rel(1), color = "grey70"),
          plot.caption = element_text(face = "italic", size = rel(0.8), 
                                      color = "grey70"),
          legend.position = "none",
          axis.text.y = element_blank(),
          axis.text.x = element_text(size=rel(1.4)),
          axis.ticks.y=element_blank(),
          plot.title.position = "plot"))

blank <- tibble(x=-6:6,y=-6:6)

(b <- ggplot(blank,aes(x=-6:6),y=-1:1) +
    annotate(geom = "segment", x = .5, xend = 4, y = -1, yend = -1, color = "#E26A89", 
             arrow = arrow(angle = 30, length = unit(0.5, "lines")),
             size=2) +
    annotate(geom = "segment", x = -.5, xend = -4, y = -1, yend = -1, color = "#0071CC", 
             arrow = arrow(angle = 30, length = unit(0.5, "lines")),
             size=2) +
    annotate(geom="text",x=.95,y=-.8,label="HER",color="#E26A89",
             size=5) +
    annotate(geom="text",x=-.85,y=-.8,label="HIS",color="#0071CC",
             size=5) +
    coord_cartesian(ylim=c(-1.1,-.4),
                    xlim=c(-6,6)) +
    theme_void())


b + a + plot_layout(ncol=1,heights = c(1,8)) +
  plot_annotation(title="How much more likely a word is to follow \"his\" \nor \"her\" in songs",
                  theme=theme(plot.title = element_text(face = "bold", size = rel(2),
                                                        family = "Roboto Condensed",
                                                        hjust=.5)))
ggsave("Figures/His_Vs_Her_Likelihood.png",dpi=600)

# Word Ratios by Gender ---------------------------------------------------
His_Her_Bigrams <- read_rds("Dataframes/His_Her_Bigrams.rds")

Unrolled_His_Her_Bigrams <- His_Her_Bigrams$lyrics[[1]] %>% mutate(gender=His_Her_Bigrams$Artist_Gender[[1]])
for (i in 1:nrow(His_Her_Bigrams)) {
  print(i)
  tryCatch({
    Unrolled_His_Her_Bigrams <- rbind(Unrolled_His_Her_Bigrams,His_Her_Bigrams$lyrics[[i]] %>% mutate(gender=His_Her_Bigrams$Artist_Gender[[i]]))
  }, error=function(e){print(e)})
}

stop_words_fixed <- stop_words %>% dplyr::rename("word2"=word)
Unrolled_His_Her_Bigrams <- anti_join(Unrolled_His_Her_Bigrams,stop_words_fixed,by="word2")

count <- Unrolled_His_Her_Bigrams %>%
  filter(!(word2 %in% c("gon","wanna"))) %>% 
  dplyr::group_by(gender,word1,word2) %>% 
  dplyr::summarise(n=sum(n,na.rm = T)) %>% 
  ungroup()


# Female Singers ----------------------------------------------------------

#Getting ratios
#Have to get words associated with his and her seperately due to a number of ties
his_words <- count %>%
  filter(gender=="Female") %>% 
  # Spread out the word1 column so that there's a column named "he" and one named "her"
  pivot_wider(names_from=word1, values_from= n, values_fill = 0) %>%
  # Add 1 to each number so that logs work (just in case any are zero)
  dplyr::mutate_if(is.numeric, ~(. + 1) / sum(. + 1)) %>%
  # Create a new column that is the logged ratio of the her counts to he counts
  dplyr::mutate(logratio = log2(her / his)) %>%
  # Sort by that ratio
  arrange(desc(logratio)) %>% 
  # This gets the words in the right order---we take the absolute value, select
  # only rows where the absolute value of the log ratio is bigger than 0, and then take the top 15 words
  dplyr::mutate(abslogratio = abs(logratio)) %>% 
  filter(logratio<0) %>% 
  arrange(desc(abslogratio)) %>% 
  head(10)

her_words <- count %>%
  filter(gender=="Female") %>% 
  # Spread out the word1 column so that there's a column named "he" and one named "her"
  pivot_wider(names_from=word1, values_from= n, values_fill = 0) %>%
  # Add 1 to each number so that logs work (just in case any are zero)
  dplyr::mutate_if(is.numeric, ~(. + 1) / sum(. + 1)) %>%
  # Create a new column that is the logged ratio of the her counts to he counts
  dplyr::mutate(logratio = log2(her / his)) %>%
  # Sort by that ratio
  arrange(desc(logratio)) %>% 
  # This gets the words in the right order---we take the absolute value, select
  # only rows where the absolute value of the log ratio is bigger than 0, and then take the top 15 words
  dplyr::mutate(abslogratio = abs(logratio)) %>% 
  filter(logratio>0) %>% 
  arrange(desc(abslogratio)) %>% 
  head(10)

female_word_ratios <- rbind(his_words,her_words) %>% dplyr::mutate(word = reorder(word2, logratio),
                                                                   word=case_when(word=="fuck"~"f*ck",
                                                                                  word=="fucked"~"f*cked",
                                                                                  word=="pussy"~"p*ussy",
                                                                                  TRUE~as.character(word)))
#Graphing
(a <- ggplot(female_word_ratios, aes(y = fct_reorder(word,-logratio), x = logratio)) +
    geom_col(aes(fill=logratio<0)) +
    geom_text(data=filter(female_word_ratios,logratio>0),
              aes(x=logratio,y=word,label=str_to_upper(word)),
              color="white",
              hjust=1.2,
              family="Roboto Condensed",
              size=4) +
    geom_text(data=filter(female_word_ratios,logratio<0),
              aes(x=logratio,y=word,label=str_to_upper(word)),
              color="white",
              hjust=-.1,
              family="Roboto Condensed",
              size=4) +
    geom_vline(xintercept=0, color="grey10") +
    coord_cartesian(xlim=c(-3.5,3.5)) +
    scale_fill_manual(values = c("#E26A89","#0071CC")) +
    labs(x = "How much more/less likely", y = NULL,
         caption = "Plot: @jakepscott2020 | Data: Spotify and Genius") +
    scale_x_continuous(breaks = seq(-6, 6,2),
                       labels = c("32x", "16x","4x", "Same", "4x", "16x","32x")) +
    theme_bw(base_family = "Roboto Condensed",base_size = 12) +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y=element_blank(),
          plot.title = element_markdown(face = "bold", size = rel(1.2)),
          plot.subtitle = element_text(face = "plain", size = rel(1), color = "grey70"),
          plot.caption = element_text(face = "italic", size = rel(0.8), 
                                      color = "grey70"),
          legend.position = "none",
          axis.text.y = element_blank(),
          axis.text.x = element_text(size=rel(1.4)),
          axis.ticks.y=element_blank(),
          plot.title.position = "plot"))

blank <- tibble(x=-6:6,y=-6:6)

(b <- ggplot(blank,aes(x=-6:6),y=-1:1) +
    annotate(geom = "segment", x = .5, xend = 4, y = -1, yend = -1, color = "#E26A89", 
             arrow = arrow(angle = 30, length = unit(0.5, "lines")),
             size=2) +
    annotate(geom = "segment", x = -.5, xend = -4, y = -1, yend = -1, color = "#0071CC", 
             arrow = arrow(angle = 30, length = unit(0.5, "lines")),
             size=2) +
    annotate(geom="text",x=.95,y=-.8,label="HER",color="#E26A89",
             size=5) +
    annotate(geom="text",x=-.85,y=-.8,label="HIS",color="#0071CC",
             size=5) +
    coord_cartesian(ylim=c(-1.1,-.4),
                    xlim=c(-6,6)) +
    theme_void())



b + a + plot_layout(ncol=1,heights = c(1,8)) +
  plot_annotation(title="How much more likely a word is to follow \"his\" <br>versus \"her\" in songs by <span style='color: #E26A89'>**female**</span> artists",
                  theme=theme(plot.title = element_markdown(face = "bold", size = rel(2),
                                                            family = "Roboto Condensed",
                                                            hjust=.5)))

ggsave("Figures/His_Her_Word_Ratio_Female.png",dpi=600)

# Male Singers ----------------------------------------------------------

#Getting ratios
#Have to get words associated with his and her seperately due to a number of ties
his_words <- count %>%
  filter(gender=="Male") %>% 
  # Spread out the word1 column so that there's a column named "he" and one named "her"
  pivot_wider(names_from=word1, values_from= n, values_fill = 0) %>%
  # Add 1 to each number so that logs work (just in case any are zero)
  dplyr::mutate_if(is.numeric, ~(. + 1) / sum(. + 1)) %>%
  # Create a new column that is the logged ratio of the her counts to he counts
  dplyr::mutate(logratio = log2(her / his)) %>%
  # Sort by that ratio
  arrange(desc(logratio)) %>% 
  # This gets the words in the right order---we take the absolute value, select
  # only rows where the absolute value of the log ratio is bigger than 0, and then take the top 15 words
  dplyr::mutate(abslogratio = abs(logratio)) %>% 
  filter(logratio<0) %>% 
  arrange(desc(abslogratio)) %>% 
  head(10)

her_words <- count %>%
  filter(gender=="Male") %>% 
  # Spread out the word1 column so that there's a column named "he" and one named "her"
  pivot_wider(names_from=word1, values_from= n, values_fill = 0) %>%
  # Add 1 to each number so that logs work (just in case any are zero)
  dplyr::mutate_if(is.numeric, ~(. + 1) / sum(. + 1)) %>%
  # Create a new column that is the logged ratio of the her counts to he counts
  dplyr::mutate(logratio = log2(her / his)) %>%
  # Sort by that ratio
  arrange(desc(logratio)) %>% 
  # This gets the words in the right order---we take the absolute value, select
  # only rows where the absolute value of the log ratio is bigger than 0, and then take the top 15 words
  dplyr::mutate(abslogratio = abs(logratio)) %>% 
  filter(logratio>0) %>% 
  arrange(desc(abslogratio)) %>% 
  head(10)

male_word_ratios <- rbind(his_words,her_words) %>% dplyr::mutate(word = reorder(word2, logratio),
                                                                   word=case_when(word=="fuck"~"f*ck",
                                                                                  word=="fucked"~"f*cked",
                                                                                  word=="pussy"~"p*ussy",
                                                                                  TRUE~as.character(word)))
#Graphing
(a <- ggplot(male_word_ratios, aes(y = fct_reorder(word,-logratio), x = logratio)) +
    geom_col(aes(fill=logratio<0)) +
    geom_text(data=filter(male_word_ratios,logratio>0),
              aes(x=logratio,y=word,label=str_to_upper(word)),
              color="white",
              hjust=1.2,
              family="Roboto Condensed",
              size=4) +
    geom_text(data=filter(male_word_ratios,logratio<0),
              aes(x=logratio,y=word,label=str_to_upper(word)),
              color="white",
              hjust=-.1,
              family="Roboto Condensed",
              size=4) +
    geom_vline(xintercept=0, color="grey10") +
    coord_cartesian(xlim=c(-5.5,5.5)) +
    scale_fill_manual(values = c("#E26A89","#0071CC")) +
    labs(x = "How much more/less likely", y = NULL,
         caption = "Plot: @jakepscott2020 | Data: Spotify and Genius") +
    scale_x_continuous(breaks = seq(-6, 6,2),
                       labels = c("32x", "16x","4x", "Same", "4x", "16x","32x")) +
    theme_bw(base_family = "Roboto Condensed",base_size = 12) +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y=element_blank(),
          plot.title = element_markdown(face = "bold", size = rel(1.2)),
          plot.subtitle = element_text(face = "plain", size = rel(1), color = "grey70"),
          plot.caption = element_text(face = "italic", size = rel(0.8), 
                                      color = "grey70"),
          legend.position = "none",
          axis.text.y = element_blank(),
          axis.text.x = element_text(size=rel(1.4)),
          axis.ticks.y=element_blank(),
          plot.title.position = "plot"))

blank <- tibble(x=-6:6,y=-6:6)

(b <- ggplot(blank,aes(x=-6:6),y=-1:1) +
    annotate(geom = "segment", x = .5, xend = 4, y = -1, yend = -1, color = "#E26A89", 
             arrow = arrow(angle = 30, length = unit(0.5, "lines")),
             size=2) +
    annotate(geom = "segment", x = -.5, xend = -4, y = -1, yend = -1, color = "#0071CC", 
             arrow = arrow(angle = 30, length = unit(0.5, "lines")),
             size=2) +
    annotate(geom="text",x=.95,y=-.8,label="HER",color="#E26A89",
             size=5) +
    annotate(geom="text",x=-.85,y=-.8,label="HIS",color="#0071CC",
             size=5) +
    coord_cartesian(ylim=c(-1.1,-.4),
                    xlim=c(-6,6)) +
    theme_void())



b + a + plot_layout(ncol=1,heights = c(1,8)) +
  plot_annotation(title="How much more likely a word is to follow \"his\" <br>versus \"her\" in songs by <span style='color: #0071CC'>**male**</span> artists",
                  theme=theme(plot.title = element_markdown(face = "bold", size = rel(2),
                                                            family = "Roboto Condensed",
                                                            hjust=.5)))

ggsave("Figures/His_Her_Word_Ratio_Male.png",dpi=600)

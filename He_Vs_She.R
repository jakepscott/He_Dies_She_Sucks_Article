
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
Song_Lyrics <- read_rds("Data/Song_Lyrics.rds")
Data <- read_rds("Data/Data.rds")


# Getting All Bigrams ---------------------------------------------------------
Song_Lyrics_Bigram <- Song_Lyrics
for (i in 1:nrow(Song_Lyrics)) {
  print(i)
  tryCatch({
    Song_Lyrics_Bigram$lyrics[[i]] <- Song_Lyrics$lyrics[[i]] %>% as_tibble() %>% 
      dplyr::rename("text"=value) %>% 
      unnest_tokens(bigram, text, token = "ngrams", n = 2) 
  }, error=function(e){print(e)})
}
saveRDS(Song_Lyrics_Bigram,"Data/Song_Lyrics_Bigram.rds")


# Only Including Bigrams That Start With He or She ------------------------
Song_Lyrics_Bigram <- read_rds("Data/Song_Lyrics_Bigram.rds")
He_She_Bigrams <- Song_Lyrics_Bigram
for (i in 1:nrow(Song_Lyrics_Bigram)) {
  print(i)
  tryCatch({
    He_She_Bigrams$lyrics[[i]] <- Song_Lyrics_Bigram$lyrics[[i]] %>% separate(bigram, c("word1", "word2"), sep = " ") %>%
      # Only choose rows where the first word is he or she
      filter(word1 %in% c("he","she")) %>% 
      dplyr::group_by(word1,word2) %>% 
      dplyr::summarise(n=n()) %>% 
      ungroup() %>% filter(word1 %in% c("he","she"))
  }, error=function(e){print(e)})
}
artist_genders <- read_rds("Data/artist_genders.rds")

He_She_Bigrams <- left_join(He_She_Bigrams,artist_genders)

saveRDS(He_She_Bigrams,"Data/He_She_Bigrams.rds")

# Unrolling To Look at All He/She Bigrams Since 2017 -------------------
He_She_Bigrams <- read_rds("Data/He_She_Bigrams.rds")
Unrolled_He_She_Bigrams <- He_She_Bigrams$lyrics[[1]]
for (i in 1:nrow(He_She_Bigrams)) {
  print(i)
  tryCatch({
    Unrolled_He_She_Bigrams <- rbind(Unrolled_He_She_Bigrams,He_She_Bigrams$lyrics[[i]])
  }, error=function(e){print(e)})
}
Unrolled_He_She_Bigrams <- Unrolled_He_She_Bigrams %>% filter(word1=="he" | word1=="she")
saveRDS(Unrolled_He_She_Bigrams,"Data/Unrolled_He_She_Bigrams.rds")


# Prepping the Data -------------------------------------------------------
Unrolled_He_She_Bigrams <- read_rds("Data/Unrolled_He_She_Bigrams.rds")
top_10_raw_words <- Unrolled_He_She_Bigrams %>% 
  dplyr::mutate(n=as.numeric(n)) %>% 
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


# Graphing Raw He Versus She----------------------------------------------------------------
(a <- top_10_raw_words %>% filter(word1=="he") %>% 
   ggplot(aes(x=fct_reorder(word2,percent),y=percent)) +
   geom_col(fill="#0071CC") +
   scale_y_continuous(label=percent) +
   geom_text(aes(x=word2,y=percent,label=str_to_upper(word2)),
             color="white",
             hjust=1.2,
             family="Roboto Condensed",
             size=4) +
   coord_flip(ylim=(c(0,.05))) +
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

(b <- top_10_raw_words %>% filter(word1=="she") %>% 
    ggplot(aes(x=fct_reorder(word2,percent),y=percent)) +
    geom_col(fill="#E26A89") +
    geom_text(aes(x=word2,y=percent,label=str_to_upper(word2)),
              color="white",
              hjust=1.2,
              family="Roboto Condensed",
              size=4) +
    scale_y_continuous(label=percent) +
    coord_flip(ylim=(c(0,.05))) +
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
a + b + 
  plot_layout(ncol = 1) +
  plot_annotation(title="The words associated with <span style='color: #0071CC'>**he**</span> and <span style='color: #E26A89'>**she**</span> are similar and boring <br>in absolute terms",
                  subtitle = "Measured as percent of total words associated with she and he respectively",
                  theme = theme(plot.title = element_markdown(face = "bold", size = rel(1.2)),
                                plot.subtitle = element_text(face = "plain", size = rel(1), color = "grey70")))

#ggsave("Figures/He_She_Words_Absolute.png",dpi=600)

# Removing Stop Words -----------------------------------------------------
Unrolled_He_She_Bigrams <- read_rds("Data/Unrolled_He_She_Bigrams.rds")

stop_words_fixed <- stop_words %>% dplyr::rename("word2"=word)

Unrolled_He_She_Bigrams <- anti_join(Unrolled_He_She_Bigrams,stop_words_fixed,by="word2")

top_10_nonstop_words <- Unrolled_He_She_Bigrams %>%
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

(a <- top_10_nonstop_words %>% filter(word1=="he") %>% 
    ggplot(aes(x=fct_reorder(word2,percent),y=percent)) +
    geom_col(fill="#0071CC") +
    geom_text(aes(x=word2,y=percent,label=str_to_upper(word2)),
              color="white",
              hjust=1.2,
              family="Roboto Condensed",
              size=4) +
    scale_y_continuous(label=percent) +
    coord_flip(ylim=(c(0,.04))) +
    labs(y=NULL,
         x=NULL,
         fill=NULL,
         title=NULL,
         subtitle =NULL) +
    theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
    theme(panel.grid = element_blank(),
          plot.title = element_markdown(face = "bold", size = rel(1.2)),
          plot.subtitle = element_text(face = "plain", size = rel(1), color = "grey70"),
          legend.position = "none",
          axis.text.x = element_text(size=rel(.8)),
          axis.text.y = element_blank(),
          plot.title.position = "plot"))

(b <- top_10_nonstop_words %>% filter(word1=="she") %>% 
    ggplot(aes(x=fct_reorder(word2,percent),y=percent)) +
    geom_col(fill="#E26A89") +
    geom_text(aes(x=word2,y=percent,label=str_to_upper(word2)),
              color="white",
              hjust=1.2,
              family="Roboto Condensed",
              size=4) +
    scale_y_continuous(label=percent) +
    coord_flip(ylim=(c(0,.04))) +
    labs(y=NULL,
         x=NULL,
         fill=NULL,
         title=NULL,
         subtitle =NULL,
         caption = "Plot: @jakepscott2020 | Data: Spotify and Genius") +
    theme_minimal(base_family = "Roboto Condensed",base_size = 16) +
    theme(panel.grid = element_blank(),
          plot.subtitle = element_text(face = "plain", size = rel(1), color = "grey70"),
          legend.position = "none",
          axis.text.x = element_text(size=rel(.8)),
          axis.text.y = element_blank(),
          plot.title.position = "plot",
          plot.caption = element_text(face = "italic", size = rel(0.8), 
                                      color = "grey70")))
a + b + 
  plot_layout(ncol = 1) +
  plot_annotation(title="The words following <span style='color: #0071CC'>**he**</span> and <span style='color: #E26A89'>**she**</span> in songs <br>are quite different",
                  subtitle = "Measured as percent of total words associated with she and he respectively",
                  theme = theme(plot.title = element_markdown(face = "bold", size = rel(2), hjust=.5),
                                plot.subtitle = element_text(face = "plain", size = rel(1), color = "grey70", hjust=.5)))
#ggsave("Figures/He_She_Words_No_Stop_Words.png",dpi=600)

# Looking at Word Ratios -------------------------------------------------
Unrolled_He_She_Bigrams <- read_rds("Data/Unrolled_He_She_Bigrams.rds")
stop_words_fixed <- stop_words %>% dplyr::rename("word2"=word)
Unrolled_He_She_Bigrams <- anti_join(Unrolled_He_She_Bigrams,stop_words_fixed,by="word2")

count <- Unrolled_He_She_Bigrams %>%
  filter(!(word2 %in% c("gon","wanna"))) %>% 
  dplyr::group_by(word1,word2) %>% 
  dplyr::summarise(n=sum(n,na.rm = T)) %>% filter(n>10)

#Getting ratios
word_ratios <- count %>%
  # Spread out the word1 column so that there's a column named "he" and one named "she"
  pivot_wider(names_from=word1, values_from= n, values_fill = 0) %>%
  # Add 1 to each number so that logs work (just in case any are zero)
  mutate_if(is.numeric, ~(. + 1) / sum(. + 1)) %>%
  # Create a new column that is the logged ratio of the she counts to he counts
  mutate(logratio = log2(she / he)) %>%
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
                               TRUE~as.character(word))) 

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
    annotate(geom="text",x=.98,y=-.8,label="SHE",color="#E26A89",
             size=5) +
    annotate(geom="text",x=-.79,y=-.8,label="HE",color="#0071CC",
             size=5) +
    coord_cartesian(ylim=c(-1.1,-.4),
                    xlim=c(-6,6)) +
    theme_void())


b + a + plot_layout(ncol=1,heights = c(1,8)) +
  plot_annotation(title="How much more likely a word is to follow \"he\" \nor \"she\" in songs",
                  theme=theme(plot.title = element_text(face = "bold", size = rel(2),
                                                        family = "Roboto Condensed",
                                                        hjust=.5)))


#ggsave("Figures/He_vs_She_Likelihood.png",dpi=600)


# Word Ratios by Gender ---------------------------------------------------
He_She_Bigrams <- read_rds("Data/He_She_Bigrams.rds")

Unrolled_He_She_Bigrams <- He_She_Bigrams$lyrics[[1]] %>% mutate(gender=He_She_Bigrams$Artist_Gender[[1]])
for (i in 1:nrow(He_She_Bigrams)) {
  print(i)
  tryCatch({
    Unrolled_He_She_Bigrams <- rbind(Unrolled_He_She_Bigrams,He_She_Bigrams$lyrics[[i]] %>% mutate(gender=He_She_Bigrams$Artist_Gender[[i]]))
  }, error=function(e){print(e)})
}

stop_words_fixed <- stop_words %>% dplyr::rename("word2"=word)
Unrolled_He_She_Bigrams <- anti_join(Unrolled_He_She_Bigrams,stop_words_fixed,by="word2")

count <- Unrolled_He_She_Bigrams %>%
  filter(!(word2 %in% c("gon","wanna"))) %>% 
  dplyr::group_by(gender,word1,word2) %>% 
  dplyr::summarise(n=sum(n,na.rm = T)) 


# Female Singers ----------------------------------------------------------

#Getting ratios
female_word_ratios <- count %>%
  filter(gender=="Female") %>% 
  # Spread out the word1 column so that there's a column named "he" and one named "she"
  pivot_wider(names_from=word1, values_from= n, values_fill = 0) %>%
  # Add 1 to each number so that logs work (just in case any are zero)
  mutate_if(is.numeric, ~(. + 1) / sum(. + 1)) %>%
  # Create a new column that is the logged ratio of the she counts to he counts
  mutate(logratio = log2(she / he)) %>%
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
    coord_cartesian(xlim=c(-4.5,4.5)) +
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
    annotate(geom="text",x=.99,y=-.8,label="SHE",color="#E26A89",
             size=5) +
    annotate(geom="text",x=-.85,y=-.8,label="HE",color="#0071CC",
             size=5) +
    coord_cartesian(ylim=c(-1.1,-.4),
                    xlim=c(-6,6)) +
    theme_void())


b + a + plot_layout(ncol=1,heights = c(1,8)) +
  plot_annotation(title="How much more likely a word is to follow \"he\" <br>versus \"she\" in songs by <span style='color: #E26A89'>**female**</span> artists",
                  theme=theme(plot.title = element_markdown(face = "bold", size = rel(2),
                                                            family = "Roboto Condensed",
                                                            hjust=.5)))

ggsave("Figures/Word_Ratio_Female.png",dpi=600)

# Male Artists ------------------------------------------------------------
male_word_ratios <- count %>%
  filter(gender=="Male",n>5) %>% 
  # Spread out the word1 column so that there's a column named "he" and one named "she"
  pivot_wider(names_from=word1, values_from= n, values_fill = 0) %>%
  # Add 1 to each number so that logs work (just in case any are zero)
  mutate_if(is.numeric, ~(. + 1) / sum(. + 1)) %>%
  # Create a new column that is the logged ratio of the she counts to he counts
  mutate(logratio = log2(she / he)) %>%
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
    coord_cartesian(xlim=c(-6,6)) +
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
    annotate(geom="text",x=.99,y=-.8,label="SHE",color="#E26A89",
             size=5) +
    annotate(geom="text",x=-.85,y=-.8,label="HE",color="#0071CC",
             size=5) +
    coord_cartesian(ylim=c(-1.1,-.4),
                    xlim=c(-6,6)) +
    theme_void())


b + a + plot_layout(ncol=1,heights = c(1,8)) +
  plot_annotation(title="How much more likely a word is to follow \"he\" <br>versus \"she\" in songs by <span style='color: #0071CC'>**male**</span> artists",
                  theme=theme(plot.title = element_markdown(face = "bold", size = rel(2),
                                                            family = "Roboto Condensed",
                                                            hjust=.5)))

ggsave("Figures/Word_Ratio_Male.png",dpi=600)


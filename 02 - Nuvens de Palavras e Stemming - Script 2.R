########################################################################
##################                                    ##################
##################                Word                ##################
##################               Clouds               ##################
##################                                    ##################
########################################################################

############### SCRIPT PARA CRIAÇÃO DE NUVENS DE PALAVRAS ###############

#install.packages("gutenbergr")
library("gutenbergr")
library("dplyr")
library("tidytext")
library("wordcloud")
library("stringr")
library("SnowballC")
library("ggplot2")
library("lexiconPT")
library("janeaustenr")
library("stringr")
library("tidyr")

textos <- gutenberg_metadata

############### Download das Obras de Freud que serão analisadas ###############
# A saber, as obras são:

# Dream Psychology: Psychoanalysis for Beginners
# Reflections on War and Death
# Totem and Taboo Resemblances Between the Psychic Lives of Savages and Neurotics
# The Interpretation of Dreams

livros <- gutenberg_download(c(#15489,
  #35875,
  41214,
  66048))

############### Limpeza dos dados ###############

#Remoção de números
nums <- livros %>% filter(str_detect(text, "^[0-9]")) %>% select(text) 

livros <- livros %>%  anti_join(nums, by = "text")

#Remoção de stop words e Tokenização
livros <- livros %>%  unnest_tokens(word, text) %>%  anti_join(stop_words)

#Contagem das palavras mais comuns por obra

## 1 - Dream Psychology: Psychoanalysis for Beginners
#beginners <- livros %>% filter(gutenberg_id == 15489) %>% count(word, sort = TRUE)

## define a paleta de cores
#pal <- brewer.pal(8,"Dark2")

## word cloud
#beginners %>% with(wordcloud(word, n, random.order = FALSE, max.words = 100, colors=pal))

###############

## 2 - Reflections on War and Death
#reflections <- livros %>% filter(gutenberg_id == 35875) %>% count(word, sort = TRUE)

## define a paleta de cores
#pal <- brewer.pal(8,"Dark2")

## word cloud
#reflections %>% with(wordcloud(word, n, random.order = FALSE, max.words = 100, colors=pal))

###############

# 3 - Totem and Taboo Resemblances Between the Psychic Lives of Savages and Neurotics
totem <- livros %>% filter(gutenberg_id == 35875) %>% count(word, sort = TRUE)

# define a paleta de cores
pal <- brewer.pal(8,"Dark2")

# word cloud
totem %>% with(wordcloud(word, n, random.order = FALSE, max.words = 100, colors=pal))

###############

# 4 - The Interpretation of Dreams
dreams <- livros %>% filter(gutenberg_id == 35875) %>% count(word, sort = TRUE)

# define a paleta de cores
pal <- brewer.pal(8,"Dark2")

# word cloud
dreams %>% with(wordcloud(word, n, random.order = FALSE, max.words = 100, colors=pal))

############### Aplicação de Stemming ###############

## 1 - Stemming: Dream Psychology: Psychoanalysis for Beginners
#beginners_stem <- beginners %>%  mutate(stem = wordStem(word)) 

#beginners_stem_count <- livros %>% filter(gutenberg_id == 15489) %>% mutate(stem = wordStem(word)) %>% count(stem, sort = TRUE)

## define a paleta de cores
#pal <- brewer.pal(8,"Dark2")

## word cloud
#beginners_stem_count %>% with(wordcloud(stem, n, random.order = FALSE, max.words = 100, colors=pal))

## 2 - Stemming: Reflections on War and Death
#reflections_stem <- reflections %>%  mutate(stem = wordStem(word)) 

#reflections_stem_count <- livros %>% filter(gutenberg_id == 35875) %>% mutate(stem = wordStem(word)) %>% count(stem, sort = TRUE)

## define a paleta de cores
#pal <- brewer.pal(8,"Dark2")

## word cloud
#reflections_stem_count %>% with(wordcloud(stem, n, random.order = FALSE, max.words = 100, colors=pal))

# 3 - Stemming: Totem and Taboo Resemblances Between the Psychic Lives of Savages and Neurotics
totem_stem <- totem %>%  mutate(stem = wordStem(word)) 

totem_stem_count <- livros %>% filter(gutenberg_id == 41214) %>% mutate(stem = wordStem(word)) %>% count(stem, sort = TRUE)

# define a paleta de cores
pal <- brewer.pal(8,"Dark2")

# word cloud
totem_stem_count %>% with(wordcloud(stem, n, random.order = FALSE, max.words = 100, colors=pal))

# 4 - Stemming: The Interpretation of Dreams
dreams_stem <- dreams %>%  mutate(stem = wordStem(word)) 

dreams_stem_count <- livros %>% filter(gutenberg_id == 66048) %>% mutate(stem = wordStem(word)) %>% count(stem, sort = TRUE)

# define a paleta de cores
pal <- brewer.pal(8,"Dark2")

# word cloud
dreams_stem_count %>% with(wordcloud(stem, n, random.order = FALSE, max.words = 100, colors=pal))

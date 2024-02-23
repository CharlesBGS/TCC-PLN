
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


# LEMATIZAR e fazer algum outro tipo de tratamento
# de dados talvez seja mais interessante

########################################################################
##################                                    ##################
##################                TF-IDF              ##################
##################                                    ##################
##################                                    ##################
########################################################################

# Cria um novo objeto com as obras de Freud
livros_tf_idf <- gutenberg_download(c(#15489,
                                      #35875,
                                      41214,
                                      66048))

#Unnest tokes para análise
book_words <- livros_tf_idf %>%
  unnest_tokens(word, text) %>%
  count(gutenberg_id, word, sort = TRUE)

#Avalia Lei de ZIPF 
#A lei de Zipf afirma que a frequência com que uma palavra aparece é inversamente proporcional a sua classificação.

#TF-IDF
#Não vamos pré-processar, a ideia é mostrar papel do tf-idf
books_tf_idf <- book_words %>% bind_tf_idf(word, gutenberg_id, n)

#Separa para ver palavras mais importantes por texto
#beginners_tf_idf <- books_tf_idf %>% filter(gutenberg_id == 15489)
#reflections_tf_idf <- books_tf_idf %>% filter(gutenberg_id == 35875)
totem_tf_idf <- books_tf_idf %>% filter(gutenberg_id == 41214)
dreams_tf_idf <- books_tf_idf %>% filter(gutenberg_id == 66048)

#Realiza gráfico com palavras mais importantes
books_graph <- books_tf_idf %>% 
  group_by(gutenberg_id) %>% 
  slice_max(tf_idf, n = 15) %>% 
  ungroup() %>%
  mutate(word = reorder(word, tf_idf)) 

books_graph %>% ggplot(aes(tf_idf, word, fill = gutenberg_id)) +
  geom_col(show.legend = FALSE) +
  labs(x = "tf-idf", y = NULL) +
  facet_wrap(~gutenberg_id, ncol = 2, scales = "free")

########################################################################


# Cria um novo objeto com as obras de Freud com pré-processamento
livros_stem_tf_idf <- gutenberg_download(c(#15489,
                                           #35875,
                                           41214,
                                           66048))

#Unnest tokes para análise
livros_stem_tf_idf <- livros_stem_tf_idf %>%
  unnest_tokens(word, text) %>%
  count(gutenberg_id, word, sort = TRUE)

############### Limpeza dos dados ###############

#Remoção de números
nums <- livros_stem_tf_idf %>% filter(str_detect(word, "^[0-9]")) %>% select(word) 

livros_stem_tf_idf <- livros_stem_tf_idf %>%  anti_join(nums, by = "word")

#Remoção de stop words e Tokenização
livros_stem_tf_idf <- livros_stem_tf_idf %>%  unnest_tokens(word, word) %>%  anti_join(stop_words)


# Stemming
livros_stem_tf_idf_2 <- livros_stem_tf_idf %>%  mutate(stem = wordStem(word)) 
livros_stem_count_tf_idf <- livros_stem_tf_idf %>% mutate(stem = wordStem(word)) %>% count(stem, sort = TRUE)

#após fazer o stemming, será necessário remover todas as palavras que sejam iguais
# usando como referência a coluna stem do objeto livros_stem_tf_idf_2

#Contar as palavras mais comuns por obra
#beginners_count_tf_idf <- livros_stem_tf_idf %>% filter(gutenberg_id == 15489) %>% count(word, sort = TRUE)
#reflections_count_tf_idf <- livros_stem_tf_idf %>% filter(gutenberg_id == 35875) %>% count(word, sort = TRUE)
totem_count_tf_idf <- livros_stem_tf_idf %>% filter(gutenberg_id == 41214,) %>% count(word, sort = TRUE)
dreams_count_tf_idf <- livros_stem_tf_idf %>% filter(gutenberg_id == 66048) %>% count(word, sort = TRUE)

#Avalia Lei de ZIPF 
#A lei de Zipf afirma que a frequência com que uma palavra aparece é inversamente proporcional a sua classificação.

#TF-IDF
# Após pré-processamento
livros_stem_tf_idf <- livros_stem_tf_idf %>% bind_tf_idf(word, gutenberg_id, n)

# Stemming
livros_stem_tf_idf_2 <- livros_stem_tf_idf %>%  mutate(stem = wordStem(word)) 
livros_stem_count_tf_idf <- livros_stem_tf_idf %>% mutate(stem = wordStem(word)) %>% count(stem, sort = TRUE)

#Separa para ver palavras mais importantes por texto
#beginners_stem_tf_idf <- livros_stem_tf_idf %>% filter(gutenberg_id == 15489)
#reflections_stem_tf_idf <- livros_stem_tf_idf %>% filter(gutenberg_id == 35875)
totem_stem_tf_idf <- livros_stem_tf_idf %>% filter(gutenberg_id == 41214)
dreams_stem_tf_idf <- livros_stem_tf_idf %>% filter(gutenberg_id == 66048)

#Realiza gráfico com palavras mais importantes
books_graph <- livros_stem_tf_idf_2 %>% 
  group_by(gutenberg_id) %>% 
  slice_max(tf_idf, n = 15) %>% 
  ungroup() %>%
  mutate(word = reorder(word, tf_idf)) 

books_graph %>% ggplot(aes(tf_idf, word, fill = gutenberg_id)) +
  geom_col(show.legend = FALSE) +
  labs(x = "tf-idf", y = NULL) +
  facet_wrap(~gutenberg_id, ncol = 2, scales = "free")

########################################################################
##################                                    ##################
##################            Dendograma              ##################
##################                                    ##################
##################                                    ##################
########################################################################


# Instalando os pacotes 
install.packages("tm")
install.packages("wordcloud")
install.packages("tidyverse")

# Carregando os pacotes
library(tm)
library(wordcloud)
library(tidyverse) 


livros <- gutenberg_download(c(41214))

livros <- livros %>% select(text)

#Remoção de números
#nums <- livros %>% filter(str_detect(text, "^[0-9]")) %>% select(text) 

#livros <- livros %>%  anti_join(nums, by = "text")

#Remoção de stop words e Tokenização
#livros <- livros %>%  unnest_tokens(word, text) %>%  anti_join(stop_words)



# Transformando os textos em um corpus
totem_corpus <- VCorpus(VectorSource(livros))

# Realizando a limpeza do corpus
totem_corpus <- 
  tm_map(
   totem_corpus,
    content_transformer(
      function(x) iconv(x, from = 'UTF-8', to = 'ASCII//TRANSLIT')
    )
  ) %>% 
  tm_map(content_transformer(tolower)) %>% 
  tm_map(removePunctuation) %>% 
  tm_map(removeWords, stopwords("english")) 

# Lista de cores em hexadecimal
paleta <- brewer.pal(8, "Dark2")

# Criando uma nuvem de palavras, com no máximo 100 palavras
# onde tenha se repetido ao menos 2 vezes.
wordcloud(
  totem_corpus,
  min.freq = 2,
  max.words = 100,
  colors = paleta
) 


# Criando uma matriz de termos
obras_document <- DocumentTermMatrix(totem_corpus)

# Removendo os termos menos frequentes
totem_doc <- removeSparseTerms(obras_document, 0.98)

# Gerando uma matrix ordenada, com o termos mais frequentes
obras_freq <- 
  totem_doc %>% 
  as.matrix() %>% 
  colSums() %>% 
  sort(decreasing = T) 


# Criando um dataframe com as palavras mais frequentes
df_obras <- data.frame(
  word = names(obras_freq),
  freq = obras_freq
)

# Gerando um gráfico da frequência
df_obras %>%
  filter(!word %in% c("taboo", "totem")) %>% 
  subset(freq > 450) %>% 
  ggplot(aes(x = reorder(word, freq),
             y = freq)) +
  geom_bar(stat = "identity", fill='#0c6cad', color="#075284") +
  theme(axis.text.x = element_text(angle = 45, hjus = 1)) +
  ggtitle("Termos relacionados a Dreams e Totem") +
  labs(y = "Frequência", x = "Termos") +
  coord_flip() 


# Removendo os termos menos frequentes
obras_doc1 <- removeSparseTerms(obras_document, 0.95)

# Clustering 1 = Dendograma
distancia <- dist(t(obras_doc1), method = "euclidian")
dendograma <- hclust(d = distancia, method = "complete")
plot(dendograma, habg = 1, main = "Dendograma Obras de Freud",
     xlab = "Distância",
     ylab = "Altura") 



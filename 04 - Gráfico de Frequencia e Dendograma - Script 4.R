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
library("tm")
library("wordcloud")
library("tidyverse") 
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
#Instalação do pacote Gutenbergr
install.packages("gutenbergr")

#Carregando o pacote:
library("gutenbergr")

#Baixando o livro "THE INTERPRETATION OF DREAMS" em inglês
dreams <- gutenberg_download(c(66048))

#Remoção de números
dreams <- dreams %>% filter(str_detect(text, "^[0-9]")) %>% select(text) 
dreams <- dreams %>%  anti_join(dreams, by = "text")

#Remoção de stop words e Tokenização
dreams <- dreams %>%  unnest_tokens(word, text) %>%  anti_join(stop_words)
#
# Enters necessarios para que os números das linhas coincidam.
# Enters necessarios para que os números das linhas coincidam.
# Enters necessarios para que os números das linhas coincidam.
# Enters necessarios para que os números das linhas coincidam.
# Enters necessarios para que os números das linhas coincidam.
#.
#Carrega a biblioteca ggplot2
library("ggplot2")
#
#Conte a frequência das palavras
freq_palavras <- dreams %>% 
  count(word, sort = TRUE) %>% 
  head(10) #Pegue as 10 palavras mais frequêntes
#
#Crie o gráfico de barras
ggplot(freq_palavras, aes(x = reorder(word, -n), y = n)) +
  geom_bar(stat = "identity", fill = "dodgerblue") +
  labs(title = "As 10 Palavras Mais Frequentes",
       x = "Palavra",
       y = "Frequência") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#
# Enters necessarios para que os números das linhas coincidam.
# Enters necessarios para que os números das linhas coincidam.
# Enters necessarios para que os números das linhas coincidam.
#
#Remoção da palavra "footnote"
remocao <- dreams %>% filter(str_detect(word, "footnote")) %>% select(word)
#
dreams <- dreams %>% anti_join(remocao, by = "word")
#
#Remoção de stop words e Tokenização
dreams <- dreams %>% unnest_tokens(word, word) %>% anti_join(stop_words)
#
# Enters necessarios para que os números das linhas coincidam.
# Enters necessarios para que os números das linhas coincidam.
# Enters necessarios para que os números das linhas coincidam.
# Enters necessarios para que os números das linhas coincidam.
# Enters necessarios para que os números das linhas coincidam.
# Enters necessarios para que os números das linhas coincidam.
# Enters necessarios para que os números das linhas coincidam.
# Enters necessarios para que os números das linhas coincidam.
# Enters necessarios para que os números das linhas coincidam.
# Enters necessarios para que os números das linhas coincidam.
# Enters necessarios para que os números das linhas coincidam.
# Enters necessarios para que os números das linhas coincidam.
# Enters necessarios para que os números das linhas coincidam.
# Enters necessarios para que os números das linhas coincidam.
# Enters necessarios para que os números das linhas coincidam.
# Enters necessarios para que os números das linhas coincidam.
# Enters necessarios para que os números das linhas coincidam.
# Enters necessarios para que os números das linhas coincidam.
# Enters necessarios para que os números das linhas coincidam.
# Enters necessarios para que os números das linhas coincidam.
#
#Nuvem de Palavras
dreams_wordcloud <- dreams %>% count(word, sort = TRUE)
#
#Define a paleta de cores
pal = brewer.pal(8, "Dark2")
#
#Word Cloud
dreams_wordcloud %>% with(wordcloud(word, n, random.order = FALSE, max.words = 50, colors = pal))
#


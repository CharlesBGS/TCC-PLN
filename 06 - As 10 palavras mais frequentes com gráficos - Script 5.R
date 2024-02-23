
livros <- gutenberg_download(c(41214, 66048))

livro1 <- gutenberg_download(c(41214))
livro2 <- gutenberg_download(c(66048))

############### Limpeza dos dados livros ###############

#Remoção de números
nums <- livros %>% filter(str_detect(text, "^[0-9]")) %>% select(text) 

livros <- livros %>%  anti_join(nums, by = "text")

#Remoção de stop words e Tokenização
livros <- livros %>%  unnest_tokens(word, text) %>%  anti_join(stop_words)


############### Limpeza dos dados livro1 ###############

#Remoção de números
nums <- livro1 %>% filter(str_detect(text, "^[0-9]")) %>% select(text) 

livro1 <- livro1 %>%  anti_join(nums, by = "text")

#Remoção de stop words e Tokenização
livro1 <- livro1 %>%  unnest_tokens(word, text) %>%  anti_join(stop_words)

############### Limpeza dos dados livro2 ###############

#Remoção de números
nums <- livro2 %>% filter(str_detect(text, "^[0-9]")) %>% select(text) 

livro2 <- livro2 %>%  anti_join(nums, by = "text")

#Remoção de stop words e Tokenização
livro2 <- livro2 %>%  unnest_tokens(word, text) %>%  anti_join(stop_words)

############### Gráfico dos dados livros ###############

# Carregue a biblioteca ggplot2
library(ggplot2)

# Conte a frequência das palavras
freq_palavras <- livros %>%
  count(word, sort = TRUE) %>%
  head(10)  # Pegue as 10 palavras mais frequentes

# Crie o gráfico de barras
ggplot(freq_palavras, aes(x = reorder(word, -n), y = n)) +
  geom_bar(stat = "identity", fill = "dodgerblue") +
  labs(title = "As 10 Palavras Mais Frequentes",
       x = "Palavra",
       y = "Frequência") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

############### Gráfico dos dados livro1 ###############

# Conte a frequência das palavras
freq_palavras <- livro1 %>%
  count(word, sort = TRUE) %>%
  head(10)  # Pegue as 10 palavras mais frequentes

# Crie o gráfico de barras
ggplot(freq_palavras, aes(x = reorder(word, -n), y = n)) +
  geom_bar(stat = "identity", fill = "dodgerblue") +
  labs(title = "As 10 Palavras Mais Frequentes",
       x = "Palavra",
       y = "Frequência") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

############### Gráfico dos dados livro2 ###############

# Conte a frequência das palavras
freq_palavras <- livro2 %>%
  count(word, sort = TRUE) %>%
  head(10)  # Pegue as 10 palavras mais frequentes

# Crie o gráfico de barras
ggplot(freq_palavras, aes(x = reorder(word, -n), y = n)) +
  geom_bar(stat = "identity", fill = "dodgerblue") +
  labs(title = "As 10 Palavras Mais Frequentes",
       x = "Palavra",
       y = "Frequência") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

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

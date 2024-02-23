# Instalar e carregar os pacotes necessários
install.packages("gutenbergr")
install.packages("tm")
library(gutenbergr)
library(tm)

# Função para pré-processamento de texto
preprocess_text <- function(text) {
  # Converter para minúsculas
  text <- tolower(text)
  
  # Remover pontuações e números
  text <- removePunctuation(text)
  text <- removeNumbers(text)
  
  # Remover espaços extras
  text <- stripWhitespace(text)
  
  return(text)
}

# Função para calcular as palavras mais frequentes
most_frequent_words <- function(book_text, n = 10) {
  # Pré-processamento do texto
  clean_text <- preprocess_text(book_text)
  
  # Criar corpus
  corpus <- Corpus(VectorSource(clean_text))
  
  # Limpar o corpus
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, removeWords, stopwords("english"))
  corpus <- tm_map(corpus, stripWhitespace)
  
  # Criar TermDocumentMatrix
  tdm <- TermDocumentMatrix(corpus)
  
  # Converta para uma matriz e calcule as frequências
  m <- as.matrix(tdm)
  word_freq <- sort(rowSums(m), decreasing = TRUE)
  
  # Exibir as 'n' palavras mais frequentes e suas frequências
  top_words <- head(word_freq, n)
  return(top_words)
}

# ID do livro no Projeto Gutenberg
book_id <- 66048

# Baixar o texto do livro
book_text <- gutenberg_download(book_id, mirror = "http://mirrors.xmission.com/gutenberg/")

# Calcular as 10 palavras mais frequentes no livro
top_words <- most_frequent_words(book_text$text, 10)
print(top_words)

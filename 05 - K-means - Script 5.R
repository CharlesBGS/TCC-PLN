

livros_tf_idf <- gutenberg_download(c(41214,66048))

#Unnest tokes para análise
book_words <- livros_tf_idf %>%
  unnest_tokens(word, text) %>%
  count(gutenberg_id, word, sort = TRUE)

#Avalia Lei de ZIPF 
#A lei de Zipf afirma que a frequência com que uma palavra aparece é inversamente proporcional a sua classificação.

#TF-IDF
#Não vamos pré-processar, a ideia é mostrar papel do tf-idf
books_tf_idf <- book_words %>% bind_tf_idf(word, gutenberg_id, n)

#transformar os textos em tokens por palavras, depois um livro fica no eixo X
# e outro no eixo Y. Aplico análise de correspondência

base <- books_tf_idf


install.packages ("factoextra")
install.packages("gridExtra")
install.packages ("cluster")

library("cluster")  # utilizacao do pacote no projeto. Pacote de Algoritmos de cluster
library("factoextra") #visualizacao dos dados
library("gridExtra") #visualizacao dos dados

max(base$tf) #maximo valor para taxa de frequencia
min(base$tf) #minimo valor para taxa de frequencia

# valor <- median(base$tf)
# base$tf[base$tf<0] <- valor
# base$tf[base$tf<900] <- valor

# base$perc = base$idf/base$tf_idf
# base$perc[base$perc<0] <- 0


base2 <- base[, c("gutenberg_id","n","tf","idf","tf_idf")]

set.seed(5)

dados.k2 <- kmeans(base2, centers = 2, nstart = 25, iter.max = 100)
dados.k3 <- kmeans(base2, centers = 3, nstart = 25, iter.max = 100)
dados.k4 <- kmeans(base2, centers = 4, nstart = 25, iter.max = 100)
dados.k5 <- kmeans(base2, centers = 5, nstart = 25, iter.max = 100)

G1 <- fviz_cluster(dados.k2, geom = "point", data = base2) + ggtitle("k = 2")
G2 <- fviz_cluster(dados.k3, geom = "point", data = base2) + ggtitle("k = 2")
G3 <- fviz_cluster(dados.k4, geom = "point", data = base2) + ggtitle("k = 2")
G4 <- fviz_cluster(dados.k5, geom = "point", data = base2) + ggtitle("k = 2")

grid.arrange(G1, G2, G3, G4, nrow(2))


grid.arrange(G1)
grid.arrange(G2)
grid.arrange(G3)
grid.arrange(G4)


# Hierarquização dos clusters
hc <- hclust(dist(base2[, c("tf", "idf")]))
# Plotagem do dendrograma
plot(hc, hang = -1)

# Hierarquização dos clusters
hc <- hclust(dist(base2[, c("tf", "idf")]))
# Corte o dendrograma em 5 clusters
cutree_clusters <- cutree(hc, k = 5)
# Plotagem do dendrograma com 5 clusters
plot(hc, hang = -1)
# Adicione linhas para destacar os clusters no dendrograma
rect.hclust(hc, k = 5, border = 2:6)




































































































































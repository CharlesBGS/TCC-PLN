##################################################################################
#                  INSTALAÇÃO E CARREGAMENTO DE PACOTES NECESSÁRIOS             #
##################################################################################
#Pacotes utilizados
pacotes <- c("tidytext","ggplot2","dplyr","tibble","gutenbergr","wordcloud","stringr","SnowballC","widyr","janeaustenr")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}


#Pacotes
library("dplyr")
library("tidytext")
library("ggplot2")
library("tibble")
library("gutenbergr")
library("wordcloud")
library("stringr")
library("SnowballC")



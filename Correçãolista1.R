## Instalando os pacotes
install.packages("devtools")
library(devtools)
devtools::install_github("tbrugz/ribge")
library(ribge)
pop2020 = populacao_municipios(2020)


## 1

#Qual a unidade de análise desse banco de dados?
library(dplyr)
glimpse(pop2020)
#Resposta: Município é a unidade de análise 
#ou Município e unidade federativa é a unidade de análise

## 2

#apenas SP
library(tidyverse)
pop_sp = pop2020 %>%
  filter(uf == "SP")

#(i) Remova as variáveis "codigo uf" e "populacao str"

pop_sp = pop_sp %>%
  select(-codigo_uf) %>%
  select(-populacao_str) 

#ou

pop_sp = pop_sp %>%
  select(-c(codigo_uf,populacao_str))
  
#(ii) Renomeie "nome munic" para "município"

pop_sp = pop_sp %>%
  rename(municipio = nome_munic)

#(iii) Para todos os nomes de municípios contidos na sua nova variável município, coloque todos os caractéres em letra minúscula.
pop_sp = pop_sp %>%
  mutate(municipio = tolower(municipio))

View(pop_sp)

#Quantos municípios há no estado de São Paulo?
pop_sp %>%
  summarise(sum(uf == 'SP'))

#Qual o menor município do estado? Quantos habitantes ele tem?
pop_sp %>%
  filter(populacao == min(populacao))

## 3

#(i) Média
#(ii) Mediana
#(iii) Desvio Padrão
#(iv) Variância


library(knitr)
pop_sp %>%
  summarise(media = mean(populacao),
            mediana = median(populacao),
            desvio = sd(populacao),
            variancia = var(populacao))

#para gerar uma tabela 
pop_sp %>%
  summarise(media = mean(populacao),
            mediana = median(populacao),
            desvio = sd(populacao),
            variancia = var(populacao)) %>%
  knitr::kable(caption = "Resumo da pop em SP")

## 4

install.packages("ggplot2")
library(dplyr)
library(ggplot2)

pop_sp %>%
  ggplot(aes(populacao)) + geom_density() + 
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(labels = scales::comma)
  

#Oq você observa, qual parece ser a medida mais adequada: média ou mediana
#Mediana

## 5

#Remova os municipios com população abaixo de 50.000 hab.
pop_sp %>%
  filter(populacao < 50000) %>%
  ggplot(aes(populacao)) + geom_density() + 
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(labels = scales::comma)

#Quantos municıpios com menos de 50.000 há?
pop_sp %>%
  filter(populacao < 50000) %>%
  summarise(n())

#Em comparação ao grafico anterior, o que voce observa?
#muitos municipios com poucos habitantes

## 6

#Calcule a média da populacao para cada um dos estados brasileiros e informe quais deles possuem maior e menor populacao media por municıpio

pop2020 %>%
  group_by(uf) %>%
  summarise(media_pop = mean(populacao)) %>%
  filter(media_pop == max(media_pop))

pop2020 %>%
  group_by(uf) %>%
  summarise(media_pop = mean(populacao)) %>%
  filter(media_pop == min(media_pop))

## Instalando os pacotes

install.packages("devtools")
devtools::install_github("tbrugz/ribge")
library(ribge)
pop2020 = populacao_municipios(2020)


## 1

#Qual a unidade de análise desse banco de dados?

library(dplyr)
glimpse(pop2020)

#Resposta: Municipios do Brasil e população desses municipios

## 2

#(i) Remova as variáveis "codigo uf" e "populacao str"

dados1 = subset(pop2020, select = -c(codigo_uf,populacao_str))

#(ii) Renomeie "nome munic" para "município"

dados2 = dados1 %>%
  rename(municipio = nome_munic)

#(iii) Para todos os nomes de municípios contidos na sua nova variável município, coloque todos os caractéres em letra minúscula.

dadosfinal = dados2 %>%
  mutate(municipio = tolower(municipio))

#Quantos municípios há no estado de São Paulo?

contagem_sp = sum(dadosfinal$uf == "SP")
print(contagem_sp)

#Qual o menor município do estado? Quantos habitantes ele tem?

install.packages("tidyverse")
library(tidyverse)

df = dadosfinal %>%
  filter(uf == "SP")

df = df %>%
  mutate(menor = min(populacao))

df %>%
  filter(populacao == menor)

#Resposta: Borá (838)

## 3

#(i) Média

mean(dadosfinal$populacao)

#(ii) Mediana

median(dadosfinal$populacao)

#(iii) Desvio Padrão

sd(dadosfinal$populacao)

#(iv) Variância

var(dadosfinal$populacao)

## 4

install.packages("ggplot2")
library(dplyr)
library(ggplot2)

#Gráfico de Densidade da População

dadosfinal %>%
  ggplot(aes(x=populacao)) + geom_density()

#O que você observa? Qual parece ser a medida mais adequada de tendencia central: a média ou a mediana?

#Resposta: Que muitos municipios possuem uma população de valor muito baixo. Sendo assim, a média parece mais próximo da tendência central.

## 5

#Remova os municipios com população abaixo de 50.000 hab.

df2 = subset(dadosfinal, populacao < 50000)

#Quantos municıpios com menos de 50.000 há?

glimpse(df2)

#Resposta: 4.893

#Em comparação ao grafico anterior, o que voce observa?

df2 %>%
  ggplot(aes(x=populacao)) + geom_density()

#Resposta: uma maior distribuição, mas com muitos municipíos com população inferior a 10.000, sendo este o pico do gráfico, e menos concentração de municipios com populações maiores.

## 6

#Calcule a média da populacao para cada um dos estados brasileiros e informe quais deles possuem maior e menor populacao media por municıpio

AC = pop2020 %>%
  filter(uf == "AC")

AL = pop2020%>%
  filter(uf == 'AL')

AP = pop2020 %>%
  filter(uf == 'AP')

AM = pop2020 %>%
  filter(uf == 'AM')

BA = pop2020 %>%
  filter(uf == 'BA')

CE = pop2020 %>%
  filter(uf == 'CE')

DF = pop2020 %>%
  filter(uf == 'DF')

ES = pop2020 %>%
  filter(uf == 'ES')

GO = pop2020 %>%
  filter(uf == 'GO')

MA = pop2020 %>%
  filter(uf == 'MA')

MT = pop2020 %>%
  filter(uf == 'MT')

MS = pop2020 %>%
  filter(uf == 'MS')

MG = pop2020 %>%
  filter(uf == 'MG')

PA = pop2020 %>%
  filter(uf == 'PA')

PB = pop2020 %>%
  filter(uf == 'PB')

PR = pop2020 %>%
  filter(uf == 'PR')

PE = pop2020 %>%
  filter(uf == 'PE')

PI = pop2020 %>%
  filter(uf == 'PI')

RJ = pop2020 %>%
  filter(uf == 'RJ')

RN = pop2020 %>%
  filter(uf == 'RN')

RS = pop2020 %>%
  filter(uf == 'RS')

RO = pop2020 %>%
  filter(uf == 'RO')

RR = pop2020 %>%
  filter(uf == 'RR')

SC = pop2020 %>%
  filter(uf == 'SC')

SP = pop2020 %>%
  filter(uf == 'SP')

SE = pop2020 %>%
  filter(uf == 'SE')

TO = pop2020 %>%
  filter(uf == 'TO')

#médias de todos os estados

mean(AC$populacao)
mean(AL$populacao)
mean(AP$populacao)
mean(AM$populacao)
mean(BA$populacao)
mean(CE$populacao)
mean(DF$populacao)
mean(ES$populacao)
mean(GO$populacao)
mean(MA$populacao)
mean(MT$populacao)
mean(MS$populacao)
mean(MG$populacao)
mean(PA$populacao)
mean(PB$populacao)
mean(PR$populacao)
mean(PE$populacao)
mean(PI$populacao)
mean(RJ$populacao)
mean(RN$populacao)
mean(RS$populacao)
mean(RO$populacao)
mean(RR$populacao)
mean(SC$populacao)
mean(SP$populacao)
mean(SE$populacao)
mean(TO$populacao)

#Resposta: Menor = Tocantins: 11440; Maior = São Paulo: 71766;
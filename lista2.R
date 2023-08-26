## 1

#Instale o pacote
install.packages("PNADcIBGE")
# Carregue o pacote
library(PNADcIBGE)
# Importe os dados desejados
data = get_pnadc(year=2017,
                  quarter=4,
                  selected=FALSE,
                  vars=c("Ano", "Trimestre", "UF", "V2007", "VD4020", "VD4035"),
                  design=FALSE,
                  savedir=tempdir())

# Selecione apenas as variaveis uteis para esta lista:
install.packages("tidyverse")
library(tidyverse)
install.packages("tidylog")
library(tidylog)

data = data %>%
  select(Ano, Trimestre, UF, V2007, VD4020, VD4035)

# Renomeie as variaveis:
data = data %>%
  rename(Sexo = V2007,
         Renda = VD4020,
         Horas_trabalhadas = VD4035)

## 2

# i) a renda media; ii) a variancia da renda;

library(knitr)

data_sem_na = data %>%
  filter(!is.na(Renda)) %>%
  filter(!is.na(Horas_trabalhadas)) 

data_sem_na %>%
  summarise(media = mean(Renda),
            variancia = var(Renda))

#Resposta: Média - 1931; Variancia - 9543677

# iii) a renda media dos homens e das mulheres;

data_sem_na %>%
  group_by(Sexo) %>%
  summarise(medias = mean(Renda))

#Resposta: Média homem - 2078; Média mulheres - 1721

# iv) a renda media em cada estado brasileiro;

medias = data_sem_na %>%
  group_by(UF) %>%
  summarise(medias = mean(Renda))

View(medias)

# v) a covariancia entre a renda e o numero de horas trabalhadas

data_sem_na %>%  
  select(Renda)
  renda_vetor = data_sem_na$Renda

data_sem_na %>%  
  select(Horas_trabalhadas)
  horas_vetor = data_sem_na$Horas_trabalhadas
  
cov(renda_vetor, horas_vetor)

#Resposta: 5776.884

## 3

# Exemplifique a veracidade da equacao, considerando X = Renda, Y = Horas trabalhadas.
# a = 2 e b = 3.

a = 2
b = 3

#mean(a*x + b * Y) = a * mean(x) + b * mean(y)

mean(a * renda_vetor + b * horas_vetor) #3975.141

a * mean(renda_vetor) + b * mean(horas_vetor) #3975.141

## 4 

# Apresente um grafico que permita visualizacao adequada da media da renda por estado brasileiro e sexo.
install.packages("ggplot2")
library(ggplot2)
library(dplyr)
library(stringr) 

dl = data_sem_na %>%
  group_by(UF, Sexo) %>%
  mutate(renda_estados = mean(Renda))

dl %>%
  ggplot(aes(x = UF, y = renda_estados, fill = Sexo)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Média da Renda dos Estados (por gênero)",
       x = "Estados", y = "Média da Renda", fill = "Sexo") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## 5

#Assuma duas variaveis aleatorias, X e Y, tais que X = renda e Y = horas trabalhadas.

# i) mean[X|10 <= Y <= 20]

X = renda_vetor
Y = horas_vetor

filtro = renda_vetor[horas_vetor >= 10 & horas_vetor <= 20]

esperanca = mean(filtro)
print(esperanca)

#Resposta: 939.7574

# ii) mean[X| Y >= 20]

filtro2 = renda_vetor[horas_vetor >= 20]

esperanca2 = mean(filtro2)
print(esperanca2)

#Resposta: 2015.483

## 6

#remova todas as observacoes cuja renda seja superior a 10.000 reais.
#i) apresente um grafico de densidade da variavel renda. Interprete;

data_sem_na %>%
  filter(Renda > 10.000) %>%
  ggplot(aes(Renda)) + geom_density() + 
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(labels = scales::comma)
  

#ii) qual a probabilidade de que, ao retirarmos aleatoriamente uma observacao (um individuo) dessa base de dados, sua renda seja estritamente maior do que 1000 e estritamente menor do que 2000 reais?

data_sem_na %>%
  summarise(probabilidade = mean(Renda > 1000 & Renda < 2000))

#iii) apresente um grafico de densidade da renda dado que as horas trabalhadas (Y) sejam menores ou iguais a 20;
data_sem_na %>%
  filter(Horas_trabalhadas <= 20) %>%
  ggplot(aes(Renda)) + geom_density() + 
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(labels = scales::comma)

#iv) calcule: P(1000 < X < 2000|Y ≤ 20)

menor_que_20 = sum(horas_vetor <= 20)

filtrar_x = sum(renda_vetor > 1000 & renda_vetor < 2000 & horas_vetor <= 20)

prob_condicional = filtrar_x / menor_que_20

print(prob_condicional)

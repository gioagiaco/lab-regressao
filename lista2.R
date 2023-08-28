## 1

# Instale o pacote
options(repos = c(CRAN = "https://cran.r-project.org"))
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

#Resposta: Média - 1931; Variância - 9543677

# iii) a renda media dos homens e das mulheres;

data_sem_na %>%
  group_by(Sexo) %>%
  summarise(medias = mean(Renda))

#Resposta: Média de renda dos Homens - 2078; Média de renda das Mulheres - 1721

# iv) a renda media em cada estado brasileiro;

medias_estados = data_sem_na %>%
  group_by(UF) %>%
  summarise(medias = mean(Renda))

View(medias_estados)

# v) a covariancia entre a renda e o numero de horas trabalhadas

renda_vetor = data_sem_na$Renda

horas_vetor = data_sem_na$Horas_trabalhadas
  
cov(renda_vetor, horas_vetor)

#Resposta: 5776.864

## 3

# Exemplifique a veracidade da equacao, considerando X = Renda, Y = Horas trabalhadas.
# a = 2 e b = 3.

a = 2
b = 3

#mean(a* x + b * Y) == a * mean(x) + b * mean(y) -- Equação

#Resposta:

#mean(a*x + b*Y):
mean(a * renda_vetor + b * horas_vetor) #3975.141

#a * mean(x) + b * mean(Y):
a * mean(renda_vetor) + b * mean(horas_vetor) #3975.141

#Logo, se 3975.141 == 3975.141 a equação é verídica.

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
  labs(title = "Média da Renda dos Estados (por sexo)",
       x = "Estados", y = "Média da Renda", fill = "Sexo") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  aes(reorder(UF, - renda_estados))

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

# remova todas as observacoes cuja renda seja superior a 10.000 reais.
# i) apresente um grafico de densidade da variavel renda. 

data_sem_superior = data_sem_na %>%
  filter(Renda > 10.000)
  
data_sem_superior %>%
  ggplot(aes(Renda)) + geom_density() +
  labs(title = "Densidade da Renda (sem valores > 10.000)",
       x = "Renda", y = "Densidade") +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(labels = scales::comma)
  
#Interprete: esse pico presente no eixo Y logo no início do gráfico aponta que há uma concentração de indivíduos com uma renda relativamente baixa;

# ii) qual a probabilidade de que, ao retirarmos aleatoriamente uma observacao (um individuo) dessa base de dados, sua renda seja estritamente maior do que 1000 e estritamente menor do que 2000 reais?

data_sem_superior %>%
  summarise(probabilidade = mean(Renda > 1000 & Renda < 2000))

#Resposta: 0.276

# iii) apresente um grafico de densidade da renda dado que as horas trabalhadas (Y) sejam menores ou iguais a 20;

data_sem_superior %>%
  filter(Horas_trabalhadas <= 20) %>%
  ggplot(aes(Renda)) + geom_density() + 
  labs(title = "Densidade da renda (com horas trabalhadas ≤ 20)",
       x = "Renda", y = "Densidade") +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(labels = scales::comma)

# iv) calcule: P(1000 < X < 2000|Y ≤ 20)

data_sem_superior %>%
  summarise(soma = sum(Renda > 1000 & Renda < 2000 & Horas_trabalhadas <= 20),
          horas = sum(Horas_trabalhadas <= 20),
          prob = soma / horas) %>%
  pull(prob) 

#Resposta: 0.1330775


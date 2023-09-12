## 1

?rnorm

#Essa função retorna uma quantidade "n" de números aleatórios dentro de uma distribuição normal especifíca criada pelo usuário, através de uma média e um desvio padrão informados na função. 

#Verificando a função:

amostra = rnorm(10, mean = 0, sd = 1)

print(amostra)

## 2

#Rode x = rnorm(100, mean = 2, sd = 1)

x = rnorm(100, mean = 2, sd = 1)

mean(x)

#Por que a media da distribuiçao é diferente da media que voce obtem rodando mean(x)?
#Reposta: devido ao fato de que os números gerados são aleatórios, logo a média real calculada é um valor muito próximo ao específicado na função, mas não o mesmo. É a assim chamada Natureza Aleatória do evento.

## 4

vetor_medias = numeric()
vetor_medias[1] = mean(rnorm(100, mean=2, sd=1))
vetor_medias[2] = mean(rnorm(100, mean=2, sd=1))

print(vetor_medias) 

## 5

vetor_medias = numeric()

  for (i in 1:30) {
  vetor_medias[i] = mean(rnorm(100, mean = 2, sd = 1))
}

print(vetor_medias)

## 6

df = data.frame(medias = vetor_medias, sim_id = 1:30)

install.packages("ggplot2")  
library(ggplot2)
library(dplyr)

ggplot(df, aes(x = medias)) +
  geom_histogram(binwidth = 0.1, fill = "pink", color = "black") +
  labs(title = "Histograma das médias", x = "Valores", y = "Frequência")

#Voce reconhece a distribuiçao apresentada pelo histograma? Se sim, qual é ela?
#Resposta: Distribuição normal;

#Consegue advinhar a media e desvio padrao da distribuiçao ou calcula-la?
#Resposta:
mean(vetor_medias) 
sd(vetor_medias)

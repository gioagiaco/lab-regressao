###aula02

#importando dados
install.packages("data.table")
library(data.table)

#vamos importar uma base de dados de pib municipais de 2013, do IBGE
# o arquivo está em formato RDS, que é um formato do R, e disponível no meu github. Para importá-lo direto no R, vamos ler o arquivo com a função url e depois importa-lo com a função readRDS. 

pib_cid <- readRDS(url("https://github.com/mgaldino/book-regression/raw/main/dados/pib_cid.RDS"))
## PARA SALVAR NO PC --> saveRDS(base_dados, file="pib_cid.RDS")

# para visualizar os dados que foram importados, temos várias funções: glimpse, head e View
library(dplyr) # para glimpse

glimpse(pib_cid)

head(pib_cid)
 
View(pib_cid)

#DBL - NUMERO REAL
#CHR - CHARACTER, CARACTERES

#para manipulação, limpeza e processamento de dados, iremos utilizar o chamado “tidyverse”

library(tidyverse)

# digamos que quero o pib total médio e o pib per capita médio
# basta usar o comando summarise, que resume os dados, e escolher a função mean.

df = pib_cid %>%
  summarise(pib_medio = mean(pib_total),   
            pib_per_capita_medio = mean(pib_per_capita))

df 

df %>% 
  glimpse()

#df= data frame, um banco de dados genérico
# %>% = pipe
#summarise = resumo, mean= em média


#média do pib_total no banco pib_cid
mean(pib_cid$pib_total)

# se eu quiser a soma dos pibs municipais
df = pib_cid %>%
  summarise(soma_pib = sum(pib_total)) %>%
  head()

df

#sum = soma

#limpando N/A do banco
#exemplo
x= c(1,2,3, NA)
mean(x)
mean(x, na.rm=TRUE)

#PIB de SP - filtrar com filter
df = pib_cid %>%
  filter(nome_munic == "São Paulo")

glimpse(df)

#achando tudo com são em SP
df_sao = pib_cid %>%
  filter(sigla_uf == "SP") %>%
  filter(grepl("São", nome_munic))

glimpse(df_sao)

df_sao$nome_munic #imprimindo a lista de nomes das cidades

# maior e menos pibs e pibs per capita entre municípios
df = pib_cid %>%
  summarise(pib_max = max(pib_total),
            pib_min = min(pib_total),
            pib_per_capita_max = max(pib_per_capita),
            pib_per_capita_min = min(pib_per_capita))

df


#criar uma variável no banco de dados com mutate
df = pib_cid %>%
  mutate(pib_max = max(pib_total),
         pib_min = min(pib_total))

View(df)

#me retornando min e max com o municipio
#linha cujo pix é máximo
df %>%
  filter(pib_total == pib_max)

#linha cujo pib é minimo
df %>%
  filter(pib_total == pib_min)

#os dois
df %>%
  filter(pib_total %in% c(pib_max, pib_min))

#filtrando pib per capita max e min

df = pib_cid %>%
  mutate(pib_max = max(pib_total),
         pib_min = min(pib_total),
         pib_max_pc = max(pib_per_capita),
         pib_min_pc = min(pib_per_capita)) %>%
  filter(pib_total %in% c(pib_max, pib_min) | pib_per_capita %in% c(pib_max_pc, pib_min_pc))

view(df) #| significa ou

#quero só algumas variáveis - select

pib_cid %>%
  filter(pib_total == max(pib_total)) %>%
  select(nome_munic, sigla_uf, pib_total, pib_per_capita)

#validando que pib per capita min é a mesma cidade de pib total min
pib_cid %>%
  filter(pib_total == min(pib_total)) %>%
  select(nome_munic, sigla_uf, pib_total, pib_per_capita)

pib_cid %>%
  filter(pib_per_capita == min(pib_per_capita)) %>%
  select(nome_munic, sigla_uf, pib_total, pib_per_capita)

#agrupando por sigla uf - group
df = pib_cid %>%
  group_by(sigla_uf) %>%
  summarise(pib_medio_uf = mean(pib_total))

view(df)

#visualização
#gráficos
library(ggplot2)

pib_cid %>%
  ggplot(aes(y=pib_total, x=impostos)) + geom_point()

#graficos mais bonitos
pib_cid %>%
  ggplot(aes(y=pib_total, x=impostos)) + geom_point() +
  scale_y_continuous(labels = scales::dollar) + theme_light() + theme(text=element_text(size=20)) +
  xlab("impostos municipais") + ggtitle("PIB municipal de 2013 x impostos municipais")

#temas feitos pela comunidade
install.packages("remotes")
remotes::install_github("MatthewBJane/theme_park")
library(ThemePark)

#barbie
pib_cid %>%
  ggplot(aes(y=pib_total, x=impostos)) + geom_point() +
  scale_y_continuous(labels = scales::dollar) + theme_barbie() + theme(text=element_text(size=20)) +
  xlab("impostos municipais") + ggtitle("PIB municipal de 2013 x impostos municipais")

#Histograma
pib_cid %>%
  ggplot(aes(x=pib_per_capita)) + geom_histogram() +
  theme_light() + theme(text=element_text(size=20)) + ggtitle("PIB per capita municipal")

#ESTATISTICA REVISÃO
x <- c(1,2,3,4,5,6,7,8,9,10)
(media_x <- sum(x)/length(x)) #length - numero de observações

x <- c(5,5,5,5,5,5,5)
(media_x <- sum(x)/length(x))

x <- c(1,3,5,7,9,11)
(media_x <- sum(x)/length(x))

x <- c(-5,-4,-3,-2,-1,1,2,3,4,5)
(media_x <- sum(x)/length(x))

x <- c(1,3,5,7,9,11)
mean(x)

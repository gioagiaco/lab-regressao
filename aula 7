##cap 08 - Causalidade

#entender

#controle - variavel dependente; e tratamento - ser nomeada pro STJ(?)
#controle é a causa do tratamento;
#equaçao de vies de seleção
#estimando tem a ver com a quantidade causal que vc quer estimar. Ele é antes da regressão

## Ingressando no cap 9 Estimação por Máxima Verossimilhança
#inferencia - quantificar as incertezas das minhas estimativas
#só com Minimos Quadrados isso não é possivel de quantificar, mas com a Máxima Verossimilhança sim


#Exemplo Brexit 
# https://simonweschle.github.io/psc400.html
#Pesquisar BES, baixar
#passar pra pasta do pc procurando com getwd()

library(data.table)
bes = fread("BES.csv")

#regressao efeito idade por voto do brexit 

reg = lm(leave ~ age, data=bes, na.action = na.omit)
summary(reg)

##

sim_nula = rnorm(10000, mean= 0, sd = 0.0001739 )

#tirar notação cientifica e gerar grafico
options(scipen = 99)
hist(sim_nula)

#intervalo de confiança 
0.0072082 + 2*  0.0001739 
0.0072082 - 2*  0.0001739 

##Aula 9

#Lista
#funcão qnorm
qnorm(p=.025)
#usada pra quando você quer dividir os seus valores em extremos. Nesse exemplo eu pego o que deixa 25% dos meus valores na lateral

#revisar oq é p-valor; intervalo de confiança; t-student
#saber interpretar os coeficientes
#interpretar a magnitude do coeficiente, e não achar que só pq é um intervalo significativo que é estatisticamente relevante; Quem interpreta os dados somos nós

##Objetivo: mostrar que a rigor não podemos usar a normal pra fazer o teste de hipoteses, temos que usar o t-student. Mas se sua amostra tiver poucas observações (20 por exemplo) você pode usar.
#quanto maior o numero de graus de liberdade, mais a cuva t-student se aproxima da curva normal
#exercicio

#Instale o pacote
install.packages("PNADcIBGE")
# Carregue o pacote
library(PNADcIBGE)

data <- get_pnadc(year=2017,
                  quarter=4,
                  selected=FALSE,
                  vars=c("Ano", "Trimestre", "UF", "V2007", "VD4020", "VD4035"),
                  design=FALSE,
                  savedir=tempdir())

# Selecione apenas as variaveis uteis para esta lista:
library(tidyverse)
library(tidylog)

data <- data %>%
  select(Ano, Trimestre, UF, V2007, VD4020, VD4035)
# Renomeie as vari´aveis:
data <- data %>%
  rename(Sexo = V2007,
         Renda = VD4020,
         Horas_trabalhadas = VD4035)
####################

#até 2000 - plotar grafico
#funcão qnorm
qnorm(p=0.025) # somar dois 
#funcao qt
qt(p=0.025)

df_values = seq(10, 2000, by = 50)
quantidades_t = qt(0.025, df = df_values)
quantile_normal = qnorm(0.025)
data = data.frame(degree_of_freedom = df_values, quantile_t = quantidades_t, quantile_normal = rep(quantile_normal, length(df_values)))
ggplot(data, aes(x = degree_of_freedom)) +
  geom_line(aes(y= quantile_t), color = 'blue', size = 1, linetype = 'solid')+
  geom_line(aes(y= quantile_normal), color = 'red', size = 1, linetype = 'dashed') +
  theme_minimal()

#######
#ou poderia ter feito tbm:

vec_n = seq(10, 2000, by = 50)
quantile_z = numeric()
quantile_t = numeric()
for (i in 1:length(vec_n)) {
  quantile_z[i] = qnorm(0.25)
  quantile_t[i] = qt(0.25, vec_n[i])
}

df = data.frame(my_qt = quantile_t,
                my_qz = quantile_z,
                n = vec_n)

library(ggplot2)
p = df %>%
  ggplot(aes(x = n, y = my_qz)) + geom_line()

p + geom_line(aes(y=my_qt), data = df)

##########

#proximo exercicio: dados eleitorais 18 e 22 - baixando
getwd()
library(data.table)
install.packages("janitor")
library(janitor)
presid_18 = fread("votacao_secao_2018_BR.csv", encoding = "Latin-1")
presid_22 = fread("votacao_secao_2022_BR.csv", encoding = "Latin-1")


# 18
df_resultados_18 = presid_18 %>%
  dplyr::filter(!NR_VOTAVEL %in% c(95,96)) %>%
  group_by(NR_ZONA, CD_MUNICIPIO, SG_UF, NR_VOTAVEL, NR_TURNO) %>%
  summarise(total_votos = sum(QT_VOTOS)) %>%
  pivot_wider(names_from = NR_TURNO, values_from = total_votos, values_fill = 0) %>%
  clean_names() %>%
  group_by(nr_zona, cd_municipio, sg_uf) %>%
  mutate(total_validos_1t = sum(x1),
         total_validos_2t = sum(x2)) %>%
  dplyr::filter(nr_votavel == 17) %>%
  mutate(percentual_bolso_1t = x1 /total_validos_1t ,
         percentual_bolso_2t = x2 / total_validos_2t)

#regressao
reg1 = lm(percentual_bolso_2t ~ percentual_bolso_1t, data = df_resultados_18)
summary(reg1)

#22
df_resultados_22 <- presid_22 %>%
  dplyr::filter(!NR_VOTAVEL %in% c(95,96)) %>%
  group_by(NR_ZONA, CD_MUNICIPIO, SG_UF, NR_VOTAVEL, NR_TURNO) %>%
  summarise(total_votos = sum(QT_VOTOS)) %>%
  pivot_wider(names_from = NR_TURNO, values_from = total_votos, values_fill = 0) %>%
  clean_names() %>%
  group_by(nr_zona, cd_municipio, sg_uf) %>%
  mutate(total_validos_1t = sum(x1),
         total_validos_2t = sum(x2)) %>%
  dplyr::filter(nr_votavel == 22) %>%
  dplyr::filter(total_validos_1t >0) %>%
  mutate(percentual_bolso_1t = x1 /total_validos_1t ,
         percentual_bolso_2t = x2 / total_validos_2t)
#
df_resultados_22 = df_resultados_22 %>%
  mutate(y_prev_2t = coef(reg1)[1] + coef(reg1)[2]*percentual_bolso_1t,
         validos_previsto = total_validos_1t*y_prev_2t)

# previsão do resultado eleitoral antes de observar apuração do 2t, supondo comparecimento igual ao 1t
df_resultados_22 %>%
  ungroup() %>%
  summarise(total_bolso = sum(validos_previsto),
            total_valido_previsto = sum(total_validos_1t),
            perc_previsto = total_bolso/total_valido_previsto)

# previsão do resultado eleitoral com o comparecimento do 2o turno real
df_resultados_22 <- df_resultados_22 %>%
  mutate(y_prev_2t_alt = coef(reg1)[1] + coef(reg1)[2]*percentual_bolso_1t,
         validos_previsto_alt = total_validos_2t*y_prev_2t_alt)


df_resultados_22 %>%
  ungroup() %>%
  summarise(total_bolso = sum(validos_previsto_alt),
            total_valido_previsto = sum(total_validos_2t),
            perc_previsto = total_bolso/total_valido_previsto)

#A mesma coisa!

#Mas e a incerteza nas previsões?

previsoes = predict(reg1, newdata = df_resultados_22, interval = "prediction", level = .95) %>%
  as.data.frame()

df_resultados_22 <- df_resultados_22 %>%
  ungroup() %>%
  mutate(prev_perc = previsoes$fit,
         prev_perc_lower = previsoes$lwr,
         prev_perc_upper = previsoes$upr,
         validos_prev = total_validos_2t*prev_perc,
         validos_prev_lower = total_validos_2t*prev_perc_lower,
         validos_prev_upper = total_validos_2t*prev_perc_upper)

df_resultados_22 %>%
  summarise(perc_previsto = sum(validos_prev)/sum(total_validos_2t),
            perc_previsto_lower = sum(validos_prev_lower)/sum(total_validos_2t),
            perc_previsto_upper = sum(validos_prev_upper)/sum(total_validos_2t))


#Como vimos, apenas com base nos dados do primeiro turno, ao nível da zona eleitoral, não era possível prever o resultado final. E à medida que os dados por zona eleitoral foram divulgados, podíamos melhorar nossa previsão de algum modo?
#E será que se incluírmos mais variáveis, podemos melhorar o modelo?
#Vamos deixar isso para a próxima aula. Por enquanto, vamos fazer o seguinte exercício. Como nossas previsões iriam evoluir, com base nesse modelo, à medida que acontecia a apuração?

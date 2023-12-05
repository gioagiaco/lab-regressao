## Aula 10

#### Lista 6 - correção

#tirar coefiente da notação cientifica:
#options(scipen=99)

#4

df = data.frame(residuos = residuals(reg2), preditor = amostra$NU_NOTA_CH)

df %>%
  ggplot(aes(x=preditor, y = residuos)) + geom_point() + geom_smooth()

#5
#podia ter sido o mesmo gráfico
#homocedastico = significa que a variancia precisa ser a mesma ao longo do grafico;

#6
#quanto mais pros extremos mais longe da normal fica meu modelo/ minha distribuição

###

#função check_model

#Regressão Múltipla 

library(data.table)
library(tidyverse)
library(here)
library(janitor)

getwd()
presid_18 <- fread("votacao_secao_2018_BR.csv", encoding = "Latin-1")

# Supondo que seu dataframe seja chamado df
df_resultados <- presid_18 %>%
  dplyr::filter(!NR_VOTAVEL %in% c(95,96)) %>%
  group_by(NR_ZONA, CD_MUNICIPIO, SG_UF, NR_VOTAVEL, NR_TURNO) %>%
  summarise(total_votos = sum(QT_VOTOS)) %>%
  pivot_wider(names_from = NR_TURNO, values_from = total_votos, values_fill = 0) %>%
  clean_names() %>%
  group_by(nr_zona, cd_municipio, sg_uf) %>%
  mutate(total_validos_1t = sum(x1),
         total_validos_2t = sum(x2)) %>%
  dplyr::filter(nr_votavel %in% c(13,17)) %>%
  group_by(nr_votavel) %>%
  mutate(percentual_1t = x1 /total_validos_1t,
         percentual_2t = x2 / total_validos_2t) %>%
  ungroup() %>%
  dplyr::select(-c(x1, x2, total_validos_1t, total_validos_2t)) %>%
  pivot_wider(names_from = nr_votavel, 
              values_from = c(percentual_1t, percentual_2t))
df_resultados %>%
  ggplot(aes(x=percentual_1t_17, y=percentual_2t_17)) + geom_point() + facet_wrap(~sg_uf) + geom_smooth(method="lm", se=F, linewidth = .5)

# modelo de regressão

reg1 <- lm(percentual_2t_17 ~ percentual_1t_17 + percentual_1t_13 + sg_uf, data = df_resultados)
summary(reg1)

#intercepto = todas as outras variaveis são 0

#transformando em classe fatorial manalmente e mudando a categoria de referencia
df_resultados = df_resultados %>%
  mutate(sg_uf = as.factor(sg_uf),
         sg_uf = relevel(sg_uf, ref = "AL"))
 
model_performance(reg1) #indice de performance 
#analise de sensibilidade

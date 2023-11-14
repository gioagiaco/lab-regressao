##Aula 08

#Checagem do Modelo de Regressão 

#Baixar dados
getwd()
library(data.table)
presid_al18 = fread("votacao_secao_2018_BR.csv")

# unzip(here("dados", "votacao_secao_2018_BR.zip"), list = TRUE)

library(data.table)
library(tidyverse)

# filtrando só AL

presid_al18 <- presid_al18 %>%
  filter(SG_UF == "AL")

# modelo voto em Bolsonaro 1t prediz voto no 2t

# descobre o que é voto nulo e branco
presid_al18 %>%
  group_by(NM_VOTAVEL) %>%
  summarise(max(NR_VOTAVEL))

# 95 e 96
presid_al18_valido <- presid_al18 %>%
  filter(!NR_VOTAVEL %in% c(95,96)) %>%
  group_by(NR_SECAO,NR_ZONA, CD_MUNICIPIO, NR_TURNO, NR_VOTAVEL ) %>%
  summarise(validos = sum(QT_VOTOS)) %>%
  mutate(bol_bolsonaro = NR_VOTAVEL == 17,
         validos_bolsonaro = sum(validos*bol_bolsonaro)) %>%
  summarise(total_validos = sum(validos),
            validos_bolsonaro = max(validos_bolsonaro),
            perc_bolsonaro = validos_bolsonaro/total_validos) %>%
  dplyr::select(-total_validos) %>%
  pivot_wider(id_cols = c(NR_SECAO, NR_ZONA, CD_MUNICIPIO), names_from = NR_TURNO, values_from = perc_bolsonaro) %>%
  rename(perc_bolso_turno1 = '1',
         perc_bolso_turno2 = '2')

# modelo de regressão

reg1 <- lm(perc_bolso_turno2 ~ perc_bolso_turno1, data = presid_al18_valido)
summary(reg1)

summary(reg1)


# gráfico

presid_al18_valido %>%
  ggplot(aes(x=perc_bolso_turno1, y=perc_bolso_turno2)) + geom_point() + 
  geom_abline(slope = coef(reg1)[2] , intercept = coef(reg1)[1], colour  = "blue")

###########

#Checagens

#Resíduos contra o preditor
# = 0

df = data.frame(residuos = residuals(reg1), preditor = presid_al18_valido$perc_bolso_turno1)

df %>%
  ggplot(aes(x=preditor, y = residuos)) + geom_point() + geom_smooth(method="lm", se=F)

#Magnitude dos Resíduos Contra o Preditor
#Homocedasticidade
#Não precisa ser = 0, mas tem q ser constante e horizontal

df <- data.frame(residuos_sq = residuals(reg1)^2, preditor = presid_al18_valido$perc_bolso_turno1)

df %>%
  ggplot(aes(x=preditor, y = residuos_sq)) + geom_point() + geom_smooth(method="lm", se=F)

# Resíduos com dados Temporais e/ou Espaciais

presid_al18_valido <- presid_al18_valido %>%
  mutate(id_secao = paste0(NR_SECAO, NR_ZONA , CD_MUNICIPIO))

df <- data.frame(residuos = residuals(reg1), id = as.numeric(presid_al18_valido$id_secao))

df %>%
  ggplot(aes(x=id, y = residuos)) + geom_point() + geom_smooth(method="lm", se=F)

#permutation test
# não pode saber qual q é o diferente 

install.packages("nullabor")
library(nullabor)

set.seed(1234)  # Aleatoriza do mesmo jeito sempre

elec_reg <- data.frame(presid_al18_valido, .resid = residuals(reg1), .fitted = fitted(reg1))

shuffled_residuals <- lineup(null_lm(perc_bolso_turno2 ~ perc_bolso_turno1,
                                     method = "rotate"), true = elec_reg,
                             n = 9)

## decrypt("ve5B DEyE l6 GuClylu6 dT") p/ saber a resposta 

ggplot(shuffled_residuals, aes(x = .fitted, y = .resid)) +
  geom_point() +
  facet_wrap(vars(.sample))

# Normalidade dos resíduos
#verificando se a os residuos tem distribuição normal

df <- data.frame(residuos = residuals(reg1), preditor = presid_al18_valido$perc_bolso_turno1, 
                 density_points = rnorm(length(residuals(reg1)) , 0, sd(residuals(reg1))))

print(sd(residuals(reg1)))

df %>%
  ggplot(aes(residuos)) + geom_histogram(aes(y=..density..)) + geom_density(aes(density_points), colour = "blue")


#Quantil 

x <- rnorm( 1000)

q50 <- quantile(x, .5)
q025 <- quantile(x, .025)
q975 <-quantile(x, .975)

print(c(q50, q025, q975))

# Q-Q plots
#ordeno os dados, e eles devem estar em torno da linha de 45 graus;

y <- rnorm(1000)

# plot de x ordenado contra y ordenado
df %>%
  ggplot(aes(x=sort(x), y=sort(y))) + geom_point() +  geom_abline(intercept = 0, slope = 1, colour ="blue")

# função do proprio R pra isso:
qqnorm(residuals(reg1))
qqline(residuals(reg1))

#############
#Exercicio Haddad em PE

#Baixar
getwd()
library(data.table)
presid_al18 = fread("votacao_secao_2018_BR.csv")

# unzip(here("dados", "votacao_secao_2018_BR.zip"), list = TRUE)

library(data.table)
library(tidyverse)

# filtrando só PE

presid_al18 <- presid_al18 %>%
  filter(SG_UF == "PE")

# 95 e 96

presid_al18_valido <- presid_al18 %>%
  filter(!NR_VOTAVEL %in% c(95,96)) %>%
  group_by(NR_SECAO,NR_ZONA, CD_MUNICIPIO, NR_TURNO, NR_VOTAVEL ) %>%
  summarise(validos = sum(QT_VOTOS)) %>%
  mutate(bol_haddad = NR_VOTAVEL == 13,
         validos_haddad = sum(validos*bol_haddad)) %>%
  summarise(total_validos = sum(validos),
            validos_haddad = max(validos_haddad),
            perc_haddad = validos_haddad/total_validos) %>%
  dplyr::select(-total_validos) %>%
  pivot_wider(id_cols = c(NR_SECAO, NR_ZONA, CD_MUNICIPIO), names_from = NR_TURNO, values_from = perc_haddad) %>%
  rename(perc_h_turno1 = '1',
         perc_h_turno2 = '2')

# modelo de regressão

reg1 <- lm(perc_h_turno2 ~ perc_h_turno1, data = presid_al18_valido)
summary(reg1)

summary(reg1)

# gráfico

presid_al18_valido %>%
  ggplot(aes(x=perc_h_turno1, y=perc_h_turno2)) + geom_point() + 
  geom_abline(slope = coef(reg1)[2] , intercept = coef(reg1)[1], colour  = "blue")

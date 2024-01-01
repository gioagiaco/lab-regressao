library(dplyr)

set.seed(123)
reg_hazel1 = lm(Homi_rate_UNODC ~ ACM_Hazel + IDH + ginet_solt + DesempregoLongo, data = Dados)
summary(reg_hazel1)

reg_grandvalley = lm(Homi_rate_UNODC ~ ACM_GV + IDH + ginet_solt + DesempregoLongo, data = Dados)
summary(reg_grandvalley)

reg_hazel2 = lm(Homi_rate_UNODC ~ ACR_Hazel + IDH + ginet_solt + DesempregoLongo, data = Dados)
summary(reg_hazel2)

reg_cipriani = lm(Homi_rate_UNODC ~ ACR_Cipriani + IDH + ginet_solt + DesempregoLongo, data = Dados)
summary(reg_cipriani)

#check_model 
library(performance)
install.packages("see")
library(see)

check_model(reg_hazel1)
check_model(reg_grandvalley)
check_model(reg_hazel2)
check_model(reg_cipriani)

#padronizando os coeficientes

Dados = na.omit(Dados)
Dados2 = Dados %>% 
  mutate(Homi_rate_UNODC = (Homi_rate_UNODC- mean(Homi_rate_UNODC))/sd(Homi_rate_UNODC),
         ACM_Hazel = (ACM_Hazel - mean(ACM_Hazel))/sd(ACM_Hazel),
         ACR_Hazel = (ACR_Hazel - mean(ACR_Hazel))/sd(ACR_Hazel),
         ACR_Cipriani = (ACR_Cipriani - mean(ACR_Cipriani))/sd(ACR_Cipriani),
         ACM_GV = (ACM_GV - mean(ACM_GV))/sd(ACM_GV),
         IDH = (IDH - mean(IDH))/sd(IDH),
         ginet_solt = (ginet_solt - mean(ginet_solt))/sd(ginet_solt),
         DesempregoLongo = (DesempregoLongo - mean(DesempregoLongo))/sd(DesempregoLongo))

reg_hazel1 = lm(Homi_rate_UNODC ~ ACM_Hazel + IDH + ginet_solt + DesempregoLongo, data = Dados2)
summary(reg_hazel1)

reg_grandvalley = lm(Homi_rate_UNODC ~ ACM_GV + IDH + ginet_solt + DesempregoLongo, data = Dados2)
summary(reg_grandvalley) ##Valores ficaram todos N/A

reg_hazel2 = lm(Homi_rate_UNODC ~ ACR_Hazel + IDH + ginet_solt + DesempregoLongo, data = Dados2)
summary(reg_hazel2)

reg_cipriani = lm(Homi_rate_UNODC ~ ACR_Cipriani + IDH + ginet_solt + DesempregoLongo, data = Dados2)
summary(reg_cipriani)

#Testes de pressupostos - Esperança zero dos resíduos

#Rodando novas regressões com o banco se N/A para análise exclusivamente dos pressupostos
reg_hazel1 = lm(Homi_rate_UNODC ~ ACM_Hazel + IDH + ginet_solt + DesempregoLongo, data = Dados)
reg_grandvalley = lm(Homi_rate_UNODC ~ ACM_GV + IDH + ginet_solt + DesempregoLongo, data = Dados)
reg_hazel2 = lm(Homi_rate_UNODC ~ ACR_Hazel + IDH + ginet_solt + DesempregoLongo, data = Dados)
reg_cipriani = lm(Homi_rate_UNODC ~ ACR_Cipriani + IDH + ginet_solt + DesempregoLongo, data = Dados)

#baixar os dados de novo
info1 = data.frame(residuos = residuals(reg_hazel1), preditor = Dados$ACM_Hazel)
info2 = data.frame(residuos = residuals(reg_hazel2), preditor = Dados$ACR_Hazel)
info3 = data.frame(residuos = residuals(reg_grandvalley), preditor = Dados$ACM_GV)
info4 = data.frame(residuos = residuals(reg_cipriani), preditor = Dados$ACR_Cipriani)
info5 = data.frame(residuos = residuals(reg_hazel1), preditor = Dados$IDH)
info6 = data.frame(residuos = residuals(reg_hazel1), preditor = Dados$ginet_solt)
info7 = data.frame(residuos = residuals(reg_hazel1), preditor = Dados$DesempregoLongo)

install.packages("ggplot2")
library(ggplot2)

info1 %>%
  ggplot(aes(x=preditor, y = residuos)) + geom_point() + geom_smooth(method="lm", se=F)

info2 %>%
  ggplot(aes(x=preditor, y = residuos)) + geom_point() + geom_smooth(method="lm", se=F)

info3 %>%
  ggplot(aes(x=preditor, y = residuos)) + geom_point() + geom_smooth(method="lm", se=F)

info4 %>%
  ggplot(aes(x=preditor, y = residuos)) + geom_point() + geom_smooth(method="lm", se=F)

info5 %>%
  ggplot(aes(x=preditor, y = residuos)) + geom_point() + geom_smooth(method="lm", se=F)

info6 %>%
  ggplot(aes(x=preditor, y = residuos)) + geom_point() + geom_smooth(method="lm", se=F)

info7 %>%
  ggplot(aes(x=preditor, y = residuos)) + geom_point() + geom_smooth(method="lm", se=F)

#Homocedasticidade

info1 = data.frame(residuos_sq = residuals(reg_hazel1)^2, preditor = Dados$ACM_Hazel)
info2 = data.frame(residuos_sq = residuals(reg_hazel2)^2, preditor = Dados$ACR_Hazel)
info3 = data.frame(residuos_sq = residuals(reg_grandvalley)^2, preditor = Dados$ACM_GV)
info4 = data.frame(residuos_sq = residuals(reg_cipriani)^2, preditor = Dados$ACR_Cipriani)
info5 = data.frame(residuos_sq = residuals(reg_hazel1)^2, preditor = Dados$IDH)
info6 = data.frame(residuos_sq = residuals(reg_hazel1)^2, preditor = Dados$ginet_solt)
info7 = data.frame(residuos_sq = residuals(reg_hazel1)^2, preditor = Dados$DesempregoLongo)

info1 %>%
  ggplot(aes(x=preditor, y = residuos_sq)) + geom_point() + geom_smooth(method="lm", se=F)

info2 %>%
  ggplot(aes(x=preditor, y = residuos_sq)) + geom_point() + geom_smooth(method="lm", se=F)

info3 %>%
  ggplot(aes(x=preditor, y = residuos_sq)) + geom_point() + geom_smooth(method="lm", se=F)

info4 %>%
  ggplot(aes(x=preditor, y = residuos_sq)) + geom_point() + geom_smooth(method="lm", se=F)

info5 %>%
  ggplot(aes(x=preditor, y = residuos_sq)) + geom_point() + geom_smooth(method="lm", se=F)

info6 %>%
  ggplot(aes(x=preditor, y = residuos_sq)) + geom_point() + geom_smooth(method="lm", se=F)

info7 %>%
  ggplot(aes(x=preditor, y = residuos_sq)) + geom_point() + geom_smooth(method="lm", se=F)

#Normalidade dos resíduos

info1 = data.frame(residuos = residuals(reg_hazel1), preditor = Dados$ACM_Hazel, 
                   density_points = rnorm(length(residuals(reg_hazel1)) , 0, sd(residuals(reg_hazel1))))

info2 = data.frame(residuos = residuals(reg_hazel2), preditor = Dados$ACR_Hazel, 
                   density_points = rnorm(length(residuals(reg_hazel2)) , 0, sd(residuals(reg_hazel2))))

info3 = data.frame(residuos = residuals(reg_grandvalley), preditor = Dados$ACM_GV, 
                   density_points = rnorm(length(residuals(reg_grandvalley)) , 0, sd(residuals(reg_grandvalley))))

info4 = data.frame(residuos = residuals(reg_cipriani), preditor = Dados$ACR_Cipriani, 
                   density_points = rnorm(length(residuals(reg_cipriani)) , 0, sd(residuals(reg_cipriani))))

info5 = data.frame(residuos = residuals(reg_hazel1), preditor = Dados$IDH, 
                   density_points = rnorm(length(residuals(reg_hazel1)) , 0, sd(residuals(reg_hazel1))))

info6 = data.frame(residuos = residuals(reg_hazel1), preditor = Dados$ginet_solt, 
                   density_points = rnorm(length(residuals(reg_hazel1)) , 0, sd(residuals(reg_hazel1))))

info7 = data.frame(residuos = residuals(reg_hazel1), preditor = Dados$DesempregoLongo, 
                   density_points = rnorm(length(residuals(reg_hazel1)) , 0, sd(residuals(reg_hazel1))))

# plot de x ordenado contra y ordenado

info1 %>%
  ggplot(aes(residuos)) + geom_histogram(aes(y=..density..)) + geom_density(aes(density_points), colour = "blue")

info2 %>%
  ggplot(aes(residuos)) + geom_histogram(aes(y=..density..)) + geom_density(aes(density_points), colour = "blue")

info3 %>%
  ggplot(aes(residuos)) + geom_histogram(aes(y=..density..)) + geom_density(aes(density_points), colour = "blue")

info4 %>%
  ggplot(aes(residuos)) + geom_histogram(aes(y=..density..)) + geom_density(aes(density_points), colour = "blue")

info5 %>%
  ggplot(aes(residuos)) + geom_histogram(aes(y=..density..)) + geom_density(aes(density_points), colour = "blue")

info6 %>%
  ggplot(aes(residuos)) + geom_histogram(aes(y=..density..)) + geom_density(aes(density_points), colour = "blue")

info7 %>%
  ggplot(aes(residuos)) + geom_histogram(aes(y=..density..)) + geom_density(aes(density_points), colour = "blue")


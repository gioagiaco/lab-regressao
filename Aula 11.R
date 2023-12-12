## Aula 11

#formula beta chapeu: y = y.b + e
#equacao de regressao simples = yi = α + β⋅X1+ e

#multicolinearidade
n = 1000
x1 = rnorm(n)
x2 = x1*2
y = x1 + rnorm(n)
reg = lm(y ~x1 + x2)
summary(reg)

#outro exemplo
x3 = rnorm(n)
x4 = 3*x1 + 4*x3
reg = lm(y ~x1 + x3 +x4)

summary(reg)

#relação não linear
x5 = x1^2
reg = lm(y ~x1 + x3 +x5)

summary(reg)

#erro padrão robusto

library(ggplot2)
library(tidyverse)
set.seed(123)

x <- c(1:8, 10, 15)
y <- c(rnorm(8,mean = 5, sd = 1.5), 40, 65)
df <- data.frame(y=y, x=x)

df %>%
  ggplot(aes(x=x, y=y)) + geom_point() + geom_smooth(method = "lm")

fit <- lm(y ~x)
summary(fit)

#Sanduiche na mão
n = length(y)
sigma2 = sigma(fit)^2
mat_I = diag(n)
X = model.matrix(fit)

omega <- sigma2*mat_I
bread <- solve(t(X)%*%X)
meat <- (t(X) %*% omega %*% X)
sanduiche <- bread %*% meat %*% bread
sqrt(diag(sanduiche)) -- #variancias
-- #erros padrões é isso ao quadrado

#No R forma 1
library(lmtest)
install.packages("sandwich")
library(sandwich)
coeftest(fit, vcovHC(fit, "HC3"))

#No R forma 2
install.packages("fixest")
library(fixest)
fit1 = feols( y~x, data = df, vcov = "HC1")
summary(fit1)

#Checagem da homocedasticidade e HC1/HC3 pro trabalho final (pra gente)
#Escolher um dos dois pro trabalho HC1 OU HC3
#NORMALIDADE DOS ERROS, LENIREARIEDADE, TABELA DE REGRESSAO

#ero quadrado tá entre 0 e 1, sendo q 1= explica 100% do modelo e 0 = explica nada

df %>%
  ggplot(aes(x=x, y=y)) + geom_point() + geom_smooth(method = "lm")

fit <- lm(y ~x)
summary(fit)

#Sanduiche na mão
n = length(y)
sigma2 = sigma(fit)^2
mat_I = diag(n)
X = model.matrix(fit)

omega <- sigma2*mat_I
bread <- solve(t(X)%*%X)
meat <- (t(X) %*% omega %*% X)
sanduiche <- bread %*% meat %*% bread
sqrt(diag(sanduiche)) -- variancias
-- erros padrões é isso ao quadrado

#No R forma 1
library(lmtest)
install.packages("sandwich")
library(sandwich)
coeftest(fit, vcovHC(fit, "HC3"))

#No R forma 2
install.packages("fixest")
library(fixest)
fit1 = feols( y~x, data = df)
summary(fit1)

# Checagem da homocedasticidade e HC1/HC3 pro trabalho final (pra gente)
#Escolher um dos dois pro trabalho HC1 OU HC3
#NORMALIDADE DOS ERROS, LINEARIDADE, TABELA DE REGRESSAO

#ero quadrado tá entre 0 e 1, sendo q 1= explica 100% do modelo e 0 = explica nada
#vies relativo tem q ser mais proximo de 0 pra ser bom

install.packages("sensemakr")
library(sensemakr)

# loads dataset
data("darfur")

# runs regression model
model = lm(peacefactor ~ directlyharmed + age + farmer_dar + herder_dar +
              pastvoted + hhsize_darfur + female + village, data = darfur)

summary(model)

## aula 5

#retomando esperança condicional

#usando dados da PNAD e distribuição de salários

data = get_pnadc(year=2017,
                  quarter=4,
                  vars=c("Ano", "Trimestre", "UF", "V2007", 
                         "VD4020", "VD4035", "V1028"),design=FALSE,
                  savedir=tempdir())

library(tidyverse)
library(tidylog)

data = data %>%
  select(Ano, Trimestre, UF, V2007, VD4020, VD4035, V1028)


data = data %>%
  rename(genero = V2007,
         renda = VD4020,
         horas_trabalhadas = VD4035)



#gráfico - #obs: Log usado é na base e 

library(ggplot2)
df = data %>%
  filter(!is.na(renda)) %>%
  filter(!is.na(horas_trabalhadas)) %>%
  filter(renda > 0) %>%
  filter(horas_trabalhadas > 0) %>%
  mutate(salario = renda/(4.5*horas_trabalhadas)) %>%
  mutate(log_salario = log(salario)) %>%
  mutate(genero = as.character(genero))

p2 = df %>%
  ggplot(aes(log_salario)) + geom_density(aes(weight=V1028)) + theme_bw(base_size = 22)

print(p2)

# Distribuição Condicional dos salários

# E[salário | Gênero = Mulher]
# E[salário | Gênero = Homem]

p3 = df %>%
  ggplot(aes(x=log_salario)) +
  geom_density(aes(weight = V1028, group=genero, colour=genero)) + theme_bw(base_size = 22)

print(p3)

#por Renda Total

p5 = df %>%
  mutate(log_renda = log(renda)) %>%
  ggplot(aes(x=log_renda)) +
  geom_density(aes(weight = V1028, group=genero, colour=genero)) + theme_bw(base_size = 22)

print(p5)

#Logaritmo facilita a visualização dos dados
#ademais, logaritmo e porcentagem são aproximados (se a diferença % for pequena)
 
#Sejam dois números positivos a e B com a > b e b > 0. A diferença % é dada por:
#100 *(a-b)/b = p

#o mesmo que

#a/b = 1 + p/100

#passando log dos dois lados, temos:

#log(a/b) = log(1 + p/100)

#Logo:
#log(a) - log(b) ~~~ p/100

#Exercício

df %>%
  group_by(genero) %>%
  summarise(weighted.mean(renda,V1028))

df %>%
  group_by(genero) %>%
  summarise(weighted.mean(log(renda),V1028))

#calculando a diferença %
7.34 - 7.10 #0.24

(2477-1875)/1875 #0.31

#Eu também posso fazer a esperança condicional para mais de uma variável

#Exemplo - E[Salario | G = Homem, Raça = Branco]

#Baixando os dados com raça e gênero

data = get_pnadc(year=2017,
                 quarter=4,
                 vars=c("Ano", "Trimestre", "UF", "V2007", 
                        "VD4020", "VD4035", "V1028", "V2010"),design=FALSE,
                 savedir=tempdir())

data = data %>%
  select(Ano, Trimestre, UF, V2007,  V2010, VD4020, VD4035, V1028)


data = data %>%
  rename(genero = V2007,
         renda = VD4020,
         raca = V2010,
         horas_trabalhadas = VD4035)

df = data %>%
  filter(!is.na(renda)) %>%
  filter(!is.na(horas_trabalhadas)) %>%
  filter(renda > 0) %>%
  filter(horas_trabalhadas > 0) %>%
  mutate(salario = renda/(4.5*horas_trabalhadas)) %>%
  mutate(log_salario = log(salario)) %>%
  mutate(genero = as.character(genero)) %>%
  mutate(raca = as.character(raca))

table1 = df %>%
  mutate(log_renda = log(renda),
         raca1 = ifelse(raca %in% c("Preta", "Parda"), "Negra",as.character(raca))) %>%
  group_by(raca1, genero) %>%
  summarise(salario = round(weighted.mean(salario, w=V1028),2))

table1

#Se eu usar só o gênero pra calcular esperança condicional

df %>%
  ggplot(aes(y=log_salario, x=genero)) + geom_point(shape = 1) +
  scale_y_continuous(labels = scales::dollar) +
  stat_summary(
    geom = "point",
    fun.y = "mean",
    col = "black",
    size = 3,
    shape = 24,
    fill = "red"
  ) + theme_bw(base_size = 22)

#as esperanças condicionais para os dois valores de  X são dadas pelos triângulos vermelhos

#pq usar media e não mediana? 

#Erro Quadratico Médio - por Media
df_erro = df %>%
  group_by(genero) %>%
  mutate(cond_exp = mean(log_salario)) %>%
  ungroup() %>%
  mutate(erro = log_salario - cond_exp)

df_erro %>%
  summarise(eqm =  sum(erro^2)) 

#resposta - 170978.7

#Erro Quadratico - por Mediana
df_erro = df %>%
  group_by(genero) %>%
  mutate(cond_exp = median(log_salario)) %>%
  ungroup() %>%
  mutate(erro = log_salario - cond_exp)

df_erro %>%
  summarise(eqm =  sum(erro^2)) 

#Logo, em previsão dos dados, usar a média é melhor pq seu EQ é menor.

## Função de Esperança Condicional (CEF)

#E[Y| X1 = x1, X2 = x2] 
#As vezes vou chamar essa esperança condicional de m(x1, x2)
m(x1, x2) é a CEF #Como m depende do x isso é uma função
Pode ser tbm m(X), simplificado

#erro da CEF
e = Y - m(X)

#Vamos fazer simulações no R para entender o erro da CEF

#Y = X^2 + U
#quero prever Y, em que:
#U ∼ norm(0,1) e X ∼ norm(0,1)

set.seed(234)
n = 10000
hist(rnorm(n, 0,1))

######
set.seed(234)
n <- 10000
x <- rnorm(n)
u <- rnorm(n)
y <- x^2 + u

df = data.frame(x = x, y=y)

df %>%
  ggplot(aes(x=x, y=y)) + geom_point()

#####

m1 <- mean(y)

m2 <- median(y)

erro1 <- y - m1
erro2 <- y - m2

print(sum(erro1^2))
print(sum(erro2^2))

#Propriedades da CEF

#Brincar com regressão

reg = lm(y ~ x, data = df)
summary(reg)

#ao fazer regressao supõe-se que a esperança condicional é linear

#estimate: se for positivo a reta é positivamente inclinada, se negativo é negativamente inclinada
#std error: estimativa do erro padrão

## aula 4

#lembrete:
#para o RMarkdown rodar, instalar o pacote tinytex

## Retomando - laço (loop)

#exemplos

#imprime i 10x

for (i in 1:10) {
  print(i)
}

#imprime i 10x, mas devagar

for (i in 1:10) {
  print(i)
  Sys.sleep(1)  #Sys.sleep = descansar
}


#loop infinito -> roda sem fim

#obs:
#append anexa no final um novo valor
#length me retorna o tamanho do vetor

###########

#semente 
set.seed(36)

#número de jogadas/simulações
n = 1000

#vetor X, para armazenar o resultado de cada uma das n jogadas
X = numeric()

#simulando n vezes
for( i in 1:n){
  X[i] = sample(1:4, size=1)  #sample = sorteia
}

#visualizando as primeiras 20 jogadas
head(X, 20)

#prob X = 1
sum(X == 1)/n #23%

# prob X = 2
sum(X==2)/ #26%

# prob X = 3
sum(X==3)/n #24%

# prob X = 4
sum(X==4)/n  #27%


## Análise de sensibilidade a medida que o n cresce

#número de jogadas/simulações
n = 1000

#vetor X, para armazenar o resultado de cada uma das n jogadas
X = numeric()

#número de replicações da simulação
k = 100

#vetor para armazenar o erro medio
erro_medio = numeric()

#simulando n vezes
for (j in 1:k) {
  for( i in 1:n){
    X[i] <- sample(1:4, size=1)
  }
  p1 = sum(X==1)/n
  p2 = sum(X==2)/n
  p3 = sum(X==3)/n
  p4 = sum(X==4)/n
  erro_medio[j] = (abs(p1 - .25) + abs(p2 - .25) + abs(p3 - .25) + abs(p3 - .25)) /4
}

summary(erro_medio)

#Ballot theorem

############

## O Modelo de regressão

#regressão e regressão linear não são a mesma coisa;

# Esperança Condicional

#Exemplo:
margem de vitoria = y
pesquisas de intenção de voto = x

#o que todo mundo quer prever:
E[Y|X] -- #esperança condicional

#Y: variável dependente/ resposta/ explicada/regressando
#X: variável independente/ explicativa/ preditor/ regressor


#causalidade está associada a ideia de pensar uma manipulação independente de uma variavel x e ver o efeito dela numa variavel y;
#Mas, em sociais a gente pensa de outra forma:

# A causa dos efeitos:
#você tem várias causas que explicam um fenômeno; esquecer isso;

#O efeito das causas:
#quantitativamente na estatística é um por vez (x causa y?);

#Pq eu quero prever (modelos preditivos), e não explicar;

# Conditional Regression Function (CEF) em PT: Função de Esperança Condicional (FEC). É baseada em esperança condicional;
#é o melhor preditor possivel por possuir menor EQM;
#EQM - Erro Quadrático Médio

#Exemplo:

#Y = margem de vitoria
#m = previsão de margem p/ cada eleição

#p/ 3 eleições
Y1 - m1 = e1
Y2 - m2 = e2
Y3 - m3 = e3

EQM = e1^2 +e2^2 + e3^2

#####

#entendendo na prática com os dados da PNAD 2017
#Pnad - amostra de dados do Censo, que é pesado demais para usar no R

#distribuição dos salários no Brasil

library(ggplot2)

df = data %>%
  filter(!is.na(Renda)) %>%
  filter(!is.na(Horas_trabalhadas)) %>%
  filter(Renda > 0) %>%
  filter(Horas_trabalhadas > 0) %>%
  mutate(salario = Renda/(4.5*Horas_trabalhadas)) %>%
  mutate(log_salario = log(salario)) %>%
  mutate(genero = as.character(genero))

p1 = df %>%
  ggplot(aes(salario)) + geom_density(aes(weight=V1028)) + theme_bw(base_size = 22)

print(p1)

#Nós vemos que a distribuição dos salários por hora é tão concentrada em torno do zero, com uma cauda longa, que é difícil visualizar os dados direito. 
#Nesse e em outros casos, usar o logaritmo natural é uma boa alternativa

#distribuição dos salários com logaritmo dos salários
p2 = df %>%
  ggplot(aes(log_salario)) + geom_density(aes(weight=V1028)) + theme_bw(base_size = 22)

print(p2)

#Teorema do limite central -- explica isso

#Y - variavel dependente salário

#e fato, podemos calcular a esperança  E[Log(salario)] dada por 2.15, ou podemos calcular
#diretamente E[salario] = 14 reais

#E[salario| Sexo = mulher]
#E[salario| Sexo = homem]

p3 = df %>%
  ggplot(aes(x=log_salario)) +
  geom_density(aes(weight = V1028, group=Sexo, colour=Sexo)) + theme_bw(base_size = 22)

print(p3)

## respondendo sobre desigualdade entre os proprios generos
df %>%
  group_by(genero) %>%
  summarise(dp = sd(salario),
            dp_log = sd(log_salario))

#ou seja, mulheres ganham mais proximo do mesmo valor, e homens tem salarios mais variaveis


### Aula 03

#E(x) = Variancia = Media
#Cov(x) = Covariância = quando as variáveis estão desviando em torno a própria média


#linearidade da esperança

x = 1:10
y = 10:1
z = x+y

#mean(x+y) == mean(x) + mean(y)

mean(x+y) #11
mean(x) #5.5
mean(y) #5.5

5.5 + 5.5 = 11

#identidade da variância

#mean(a*x +b*y) == a*mean(x) + b*mean(y)

a = 2
b = 3

mean(a*x + b*y) #27.5
a*mean(x) #11
b*mean(y) #16.5
11+16.5 = 27.5

#somar uma constante não altera a variância
#multiplicar uma constante altera a variância para o número ao quadrado

#distribuição de probabilidade conjunta

#Considere um dado de 4 faces. Seja X a soma dos números jogando ele duas vezes
#e Y o maior valor dos dois dados. O espaço amostral é dado pela tabela abaixo

#Ex:
#X = 4 + 4
#Y = 4

#distribuição de probabilidade dos dois valores juntos

#distribuição de probabilidade condicional
#existe uma condição
#eu tenho uma informação de uma variável, e eu calculo uma probab. sobre outra variável condicionada

#Ex: Se Y = 1, qual a probabilidade de X ser = 2
#Resposta: 100% Ou P(X = 2 | Y = 1) = 1

##Introducao a Simulação

factorial(10)
10*9*8*7*6*5*4*3*2*1

#######

#raízes quadradas no R:
4^(1/2) #ou 
sqrt(4)

#fórmula de Stirling:

sqrt(2*pi)*n^(n+1/2)*exp(-n) 

#Podemos usar simulações para aproximar quantidades, com Stirling

##exemplo

factorial(1000) #erro
sqrt(2*pi*10)*(10/exp(1))^10 #ent eu faço na função 

#crindo uma função
stirling_formula = function(n)  {
  sqrt(2*pi)*(n/exp(1))^n
}

stirling_formula(100)

# A fórmula de Stirling é bem precisa para aproximar fatorial e muito mais fácil de computar.
# funções servem pra abstrair e facilitar o entendimento

# especificando semente, para simulação ser reproduzível
set.seed(234)

# número de amostras
n = 10000

x = sample(1:6, 1000, replace=T)
sum(x == 6)
190/1000

#10000 amostras de um lançamento  de dado de 6 lados 
resultado = sample(1:6, replace = T)

#frequencia relativa 6 é dada pelo número de 6 / total de amostras
prob_6 = sum(resultado ==6)/n

#18,89%
#1/6 = 16.666

#função que calcula a probabilidade de sair um numero de um dado de 6 faces
prob_x_dado6 = function(x, n= 10000) {
  resultado = sample(1:6, n, replace = T)
  sum(resultado == x)/n
}

set.seed(234)
prob_x_dado6(1)


#análise de sensibilidade que garante que uma simulação é boa, para isso varia-se algumas condições, como o n

# número de amostras

vec_amostra = c(100, 1000, 10000, 100000, 1000000)

# lista vazia para armazenar os resultados das simulações
resultado_lista = list()

# vetor vazio para armazenar a frequência relativa de 6
vec_prob6 = numeric()

set.seed(234)

#explicando laço (loop)
#iterando valores 
#ex: sum(1:10)

x = 0
for(i in 1:10) {
  x = x + i
}

x


##########faltou explicar

# loop sobre os tamanhos das amostrar
for ( i in 1:length(vec_amostra)) {
  # n amostras de uma lançamento de dado de 6 lados
  resultado_lista[[i]] <- sample(1:6, vec_amostra[i], TRUE)
  
  # frequência relativa de 6 é dada por número de 6 / total de amostras
  vec_prob6[i] <- sum(resultado_lista[[i]] == 6)/vec_amostra[i]
  
}

print(vec_prob6)


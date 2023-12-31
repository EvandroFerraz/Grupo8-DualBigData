#Bruno Mikael Oliveira de Souza (822161481)
#Lucas Cassidori (821132253)
#Matheus Barbosa Pereira (822161770)
#Victor Ulysses Monteiro de Oliveira (822166074)
#Vitor de Lima Reis (822156460)
#Evandro Ferraz (Professor)

# O topiramato � um f�rmaco anticonvulsivante utilizado no tratamento de epilepsia, enxaqueca e transtornos bipolares.
setwd("C:/Users/822161770/Downloads")
setwd("C:/Users/Matheus/Desktop/Analise")
arq<- read.table(file ="dados_tratados.csv", header = TRUE, sep = ",");arq
View(arq)

# Vari�veis: PRINCIPIO_ATIVO; SEXO; IDADE; UF_VENDA; MES_VENDA
# PRINCIPIO_ATIVO: Refere-se ao princ�pio ativo do medicamento, que � a subst�ncia respons�vel pelo efeito. Qualitativa nominal.
# SEXO: Indica o sexo da pessoa que comprou o medicamento, podendo ser masculino ou feminino. Qualitativa nominal.
# IDADE: Representa a idade da pessoa que comprou o medicamento, expressa em anos. Quantitativa discreta>
# UF_VENDA: Refere-se ao estado em que o medicamento foi vendido, utilizando a sigla do estado). Qualitativa nominal.
# MES_VENDA: Indica o m�s em que o medicamento foi vendido, representado por um n�mero de 1 a 11 (o m�s de dezembro n�o foi contabilizado), correspondendo aos meses do ano. Qualitativa ordinal.

# vari�veis qualitativas
table(arq$PRINCIPIO_ATIVO) # o �nico princ�pio ativo � o TOPIRAMATO
table(arq$SEXO) # o mais frequente � o sexo Feminino
table(arq$UF_VENDA) # o mais frequente � o estado de S�o Paulo
table(arq$MES_VENDA) # o mais frequente � o m�s de Setembro

# vari�veis quantitativas
summary(arq$IDADE) # a m�dia dos consumidores do TOPIRAMATO � de 42.81 anos
var(arq$IDADE) # a vari�ncia da vari�vel IDADE � 205.3716
sd(arq$IDADE) #o desvio padr�o da vari�vel IDADE � 14.33079

install.packages("ggplot2")
library(ggplot2)

data <- subset(arq, IDADE >= 10 & IDADE < 100)
ggplot(data = data, aes(x = IDADE)) +
  geom_density(fill = "blue", alpha = 0.5) +
  labs(x = "IDADE", y = "Densidade") +
  ggtitle("Gr�fico de Densidade do Conjunto de Dados Filtrado")

# filtrar adolescentes (popula��o de 13 a 17 anos)
adolescentes <- subset(arq, IDADE >= 13 & IDADE <= 17)
num_adolescentes <- nrow(adolescentes)
print(paste(num_adolescentes, "adolescentes"))
# filtrar jovens (popula��o de 18 a 39 anos) # 186 adolescentes
jovens <- subset(arq, IDADE >= 18 & IDADE <= 39)
num_jovens <- nrow(jovens)
print(paste(num_jovens, "jovens")) # 7547 jovens
# filtrar adultos (popula��o de 40 a 59 anos)
adultos <- subset(arq, IDADE >= 40 & IDADE <= 59)
num_adultos <- nrow(adultos)
print(paste(num_adultos, "adultos")) # 8705 adultos
# filtrar idosos (popula��o de 60 anos ou mais)
idosos <- subset(arq, IDADE >= 60)
num_idosos <- nrow(idosos)
print(paste(num_idosos, "idosos")) # 1822 idosos

# nova teoria: popula��o de 40 a 59 anos (adultos) consome mais do que a popula��o de 18 a 39 anos (jovens)
# compara��o de m�dias de idade de consumo do Topiramato de duas popula��es diferentes:
# popula��o de 40 a 59 anos (adultos) = popula��o A
# popula��o de 18 a 39 anos (jovens) = popula��o B

# m�dia dos adultos = 48.2533
mean(adultos $IDADE)
# m�dia dos jovens = 31.43845
mean(jovens$IDADE)

# quantidade de adultos = 8705
num_adultos
# quantidade de jovens = 7547
num_jovens

# desvio padr�o dos adultos = 5.594633
sd(adultos$IDADE)
# desvio padr�o dos jovens = 5.540495
sd(jovens$IDADE)












# teste t student para duas amostras:
# Ho: ??A = ??B, hip�tese nula, n�o h� diferen�a significativa entre as m�dias de idade das duas popula��es
# Ha: ??A ??? ??B, hip�tese alternativa, h� diferen�a significativa entre as m�dias de idade das duas popula��es
# Onde ??A e ??B representam a m�dia de idade das popula��es A e B, respectivamente
# t = (xA - xB) / ???((sA^2/nA) + (sB^2/nB))
# xA e xB s�o as m�dias das amostras A e B: 48.2533 e 31.43845, respectivamente
# sA e sB s�o os desvios padr�o das amostras A e B: 5.594633 e 5.540495, respectivamente
# nA e nB s�o o tamanho das amostras A e B: 8705 e 7547, respectivamente

# t = (xA - xB) / ???((sA^2/nA) + (sB^2/nB))
t = (48.2533 - 31.43845) / sqrt((5.594633^2/8705) + (5.540495^2/7547)); t
t.test(adultos$IDADE, jovens$IDADE)
# t = 192.084

# graus de liberdade:
# gl = nA + nB - 2
# gl = 8705 + 7547 - 2
gl = 8705 + 7547 - 2; gl
# gl = 16250

# n�vel de signific�ncia:
# a = 0.05

# tabela de distribui��o t de Student
# https://www.statisticshowto.com/tables/t-distribution-table/#two

# valor cr�tico
#qt(1 - a, gl)
qt(1 - 0.05, 16250)
# xc = 1.644947

# t = 192.084












# considerando que 192.084 � maior do que 1.644947, conclu�mos que t � maior que xc e portanto est� dentro da regi�o cr�tica RC
# como est� dentro da regi�o cr�tica, aceitamos a hip�tese alternativa
# podemos concluir ent�o que existem evid�ncias estat�sticas suficientes para afirmar que as m�dias de idades das duas popula��es s�o diferentes, ao n�vel de signific�ncia de 5%
# an�lise estat�stica com o objetivo de comparar o consumo do medicamento topiramato entre duas faixas et�rias: adultos (40 a 59 anos) e jovens (18 a 39 anos). A hip�tese era de que a popula��o de adultos consumiria mais o medicamento em compara��o com a popula��o de jovens.

# a Solu��o de Rastreabilidade proposta � que a maior parte do consumidores dessas duas popula��es � do sexo feminino
table(adultos$SEXO)
table(jovens$SEXO)
barplot(table(adultos$SEXO), main = "Distribui��o por Sexo para Adultos", xlab = "Sexo", ylab = "Frequ?ncia", names.arg = c("Masculino", "Feminino"))
barplot(table(jovens$SEXO), main = "Distribui��o por Sexo para Jovens", xlab = "Sexo", ylab = "Frequ?ncia", names.arg = c("Masculino", "Feminino"))

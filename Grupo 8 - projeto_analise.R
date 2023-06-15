#Bruno Mikael Oliveira de Souza (822161481)
#Lucas Cassidori (821132253)
#Matheus Barbosa Pereira (822161770)
#Victor Ulysses Monteiro de Oliveira (822166074)
#Vitor de Lima Reis (822156460)
#Evandro Ferraz (Professor)

# O topiramato é um fármaco anticonvulsivante utilizado no tratamento de epilepsia, enxaqueca e transtornos bipolares.
setwd("C:/Users/822161770/Downloads")
setwd("C:/Users/Matheus/Desktop/Analise")
arq<- read.table(file ="dados_tratados.csv", header = TRUE, sep = ",");arq
View(arq)

# Variáveis: PRINCIPIO_ATIVO; SEXO; IDADE; UF_VENDA; MES_VENDA
# PRINCIPIO_ATIVO: Refere-se ao princípio ativo do medicamento, que é a substância responsável pelo efeito. Qualitativa nominal.
# SEXO: Indica o sexo da pessoa que comprou o medicamento, podendo ser masculino ou feminino. Qualitativa nominal.
# IDADE: Representa a idade da pessoa que comprou o medicamento, expressa em anos. Quantitativa discreta>
# UF_VENDA: Refere-se ao estado em que o medicamento foi vendido, utilizando a sigla do estado). Qualitativa nominal.
# MES_VENDA: Indica o mês em que o medicamento foi vendido, representado por um número de 1 a 11 (o mês de dezembro não foi contabilizado), correspondendo aos meses do ano. Qualitativa ordinal.

# variáveis qualitativas
table(arq$PRINCIPIO_ATIVO) # o único princípio ativo é o TOPIRAMATO
table(arq$SEXO) # o mais frequente é o sexo Feminino
table(arq$UF_VENDA) # o mais frequente é o estado de São Paulo
table(arq$MES_VENDA) # o mais frequente é o mês de Setembro

# variáveis quantitativas
summary(arq$IDADE) # a média dos consumidores do TOPIRAMATO é de 42.81 anos
var(arq$IDADE) # a variância da variável IDADE é 205.3716
sd(arq$IDADE) #o desvio padrão da variável IDADE é 14.33079

install.packages("ggplot2")
library(ggplot2)

data <- subset(arq, IDADE >= 10 & IDADE < 100)
ggplot(data = data, aes(x = IDADE)) +
  geom_density(fill = "blue", alpha = 0.5) +
  labs(x = "IDADE", y = "Densidade") +
  ggtitle("Gráfico de Densidade do Conjunto de Dados Filtrado")

# filtrar adolescentes (população de 13 a 17 anos)
adolescentes <- subset(arq, IDADE >= 13 & IDADE <= 17)
num_adolescentes <- nrow(adolescentes)
print(paste(num_adolescentes, "adolescentes"))
# filtrar jovens (população de 18 a 39 anos) # 186 adolescentes
jovens <- subset(arq, IDADE >= 18 & IDADE <= 39)
num_jovens <- nrow(jovens)
print(paste(num_jovens, "jovens")) # 7547 jovens
# filtrar adultos (população de 40 a 59 anos)
adultos <- subset(arq, IDADE >= 40 & IDADE <= 59)
num_adultos <- nrow(adultos)
print(paste(num_adultos, "adultos")) # 8705 adultos
# filtrar idosos (população de 60 anos ou mais)
idosos <- subset(arq, IDADE >= 60)
num_idosos <- nrow(idosos)
print(paste(num_idosos, "idosos")) # 1822 idosos

# nova teoria: população de 40 a 59 anos (adultos) consome mais do que a população de 18 a 39 anos (jovens)
# comparação de médias de idade de consumo do Topiramato de duas populações diferentes:
# população de 40 a 59 anos (adultos) = população A
# população de 18 a 39 anos (jovens) = população B

# média dos adultos = 48.2533
mean(adultos $IDADE)
# média dos jovens = 31.43845
mean(jovens$IDADE)

# quantidade de adultos = 8705
num_adultos
# quantidade de jovens = 7547
num_jovens

# desvio padrão dos adultos = 5.594633
sd(adultos$IDADE)
# desvio padrão dos jovens = 5.540495
sd(jovens$IDADE)












# teste t student para duas amostras:
# Ho: ??A = ??B, hipótese nula, não há diferença significativa entre as médias de idade das duas populações
# Ha: ??A ??? ??B, hipótese alternativa, há diferença significativa entre as médias de idade das duas populações
# Onde ??A e ??B representam a média de idade das populações A e B, respectivamente
# t = (xA - xB) / ???((sA^2/nA) + (sB^2/nB))
# xA e xB são as médias das amostras A e B: 48.2533 e 31.43845, respectivamente
# sA e sB são os desvios padrão das amostras A e B: 5.594633 e 5.540495, respectivamente
# nA e nB são o tamanho das amostras A e B: 8705 e 7547, respectivamente

# t = (xA - xB) / ???((sA^2/nA) + (sB^2/nB))
t = (48.2533 - 31.43845) / sqrt((5.594633^2/8705) + (5.540495^2/7547)); t
t.test(adultos$IDADE, jovens$IDADE)
# t = 192.084

# graus de liberdade:
# gl = nA + nB - 2
# gl = 8705 + 7547 - 2
gl = 8705 + 7547 - 2; gl
# gl = 16250

# nível de significância:
# a = 0.05

# tabela de distribuição t de Student
# https://www.statisticshowto.com/tables/t-distribution-table/#two

# valor crítico
#qt(1 - a, gl)
qt(1 - 0.05, 16250)
# xc = 1.644947

# t = 192.084












# considerando que 192.084 é maior do que 1.644947, concluímos que t é maior que xc e portanto está dentro da região crítica RC
# como está dentro da região crítica, aceitamos a hipótese alternativa
# podemos concluir então que existem evidências estatísticas suficientes para afirmar que as médias de idades das duas populações são diferentes, ao nível de significância de 5%
# análise estatística com o objetivo de comparar o consumo do medicamento topiramato entre duas faixas etárias: adultos (40 a 59 anos) e jovens (18 a 39 anos). A hipótese era de que a população de adultos consumiria mais o medicamento em comparação com a população de jovens.

# a Solução de Rastreabilidade proposta é que a maior parte do consumidores dessas duas populações é do sexo feminino
table(adultos$SEXO)
table(jovens$SEXO)
barplot(table(adultos$SEXO), main = "Distribuição por Sexo para Adultos", xlab = "Sexo", ylab = "Frequ?ncia", names.arg = c("Masculino", "Feminino"))
barplot(table(jovens$SEXO), main = "Distribuição por Sexo para Jovens", xlab = "Sexo", ylab = "Frequ?ncia", names.arg = c("Masculino", "Feminino"))

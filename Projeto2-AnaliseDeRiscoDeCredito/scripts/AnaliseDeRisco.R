###### DEFINIR O PROBLEMA DE NEG�CIO A SER RESOLVIDO ######

# Construir um modelo preditivo generaliz�vel para an�lise de risco de cr�dito - Modelo de Classifica��o

# Configurar o diretorio de trabalho
setwd("D:/FCD/Projetos/DSA---FormacaoCientistaDeDados---R/Projeto2-AnaliseDeRiscoDeCredito/scripts")
getwd()

# Biblioteca de utilit�rios para constru��o de gr�ficos
source("D:/FCD/Projetos/DSA---FormacaoCientistaDeDados---R/Projeto2-AnaliseDeRiscoDeCredito/lib/plot_utils.R")

# Verificar os pacotes instalados
search()

# Instalar os pacotes
install.packages("readr")
install.packages("dplyr")
install.packages("e1071")
install.packages("caret")
install.packages("randomForest")

# Carregar os pacotes
library(readr)
library(dplyr)
library(e1071)
library(ROCR)
library(caret)
library(randomForest)


###### CARGA DOS DADOS ######

# Carregar os dados em um dataframe
credito.df <- read.csv("D:/FCD/Projetos/DSA---FormacaoCientistaDeDados---R/Projeto2-AnaliseDeRiscoDeCredito/data/credit_dataset.csv", header = TRUE, sep = ",")



###### ANALISE EXPLORATORIA ######

# Compreender o conjunto de dados | Descobrir a correla��o entre as vari�veis | Detectar outliers e valores missing | Encontrar vari�veis chave
str(credito.df)
View(credito.df)
class(credito.df)
dim(credito.df)

# An�lise Exploratoria para variaveis numericas - resumo das vari�veis
summary(credito.df$credit.amount)# variavel quantidade de credito nao esta com dist. normal, pois media e mediana s�o diferentes
summary(credito.df$age)# variavel idade nao esta com dist. normal, pois media e mediana s�o diferentes
summary(credito.df$credit.duration.months)


# BoxPlot - visualizar valores outliers
?boxplot
boxplot(credito.df$credit.amount, main = "boxplot para a quantidade de cr�dito", ylab = "Quantidade de Cr�dito")
boxplot(credito.df$age, main = "boxplot para o idade", ylab = "Idade")
boxplot(credito.df$credit.duration.months, main = "boxplot para meses de dura��o do credito", ylab = "Meses de Dura��o do Cr�dito")

# Histograma - distribui��o de 1 variavel
?hist
hist(credito.df$credit.amount, main = "histograma para o quantidade de credito", xlab = "Quantidade de Cr�dito")
hist(credito.df$age, main = "histograma para a idade", xlab = "Idade")
hist(credito.df$credit.duration.months, main = "histograma para meses de dura��o de credito", xlab = "Meses de Dura��o do Cr�dito")

# Scatterplot quantidade de cr�dito x idade - rela��o entre 2 variaveis
?plot
plot(x = credito.df$age, y = credito.df$credit.amount,
     main = "Scaterplot - quantidade de credito x idade",
     ylab = "Quantidade de Credito",
     xlab = "Idade")

# Medidas de Dispers�o (variancia e desvio padrao)
## Se variancia e desvio padr�o estiverem altos, significa que os dados est�o muito espalhados com rela��o a media
var(credito.df$credit.amount)
sd(credito.df$credit.amount)
var(credito.df$age)
sd(credito.df$age)

# Verificar valores missing (NA)
any(is.na(credito.df))



######## PR�-PROCESSAMENTO ############

# Limpeza, transforma��o e manipula��o dos dados (Prepara��o dos dados)

## Fun��o para converter as vari�veis categ�ricas para o tipo fator 
to.factors <- function(df, variables){
  for (variable in variables){
    df[[variable]] <- as.factor(df[[variable]])
  }
  return(df)
}

## Fun��o para Padroniza��o de variaveis tipo numerica
scale.features <- function(df, variables){
  for (variable in variables){
    df[[variable]] <- scale(df[[variable]], center=T, scale=T)
  }
  return(df)
}

## Aplicar a padroniza��o (normaliza��o) nas vari�veis numericas
var.numericas <- c("credit.duration.months", "age", "credit.amount")
credito.df1 <- scale.features(credito.df, var.numericas)
View(credito.df1)

## Aplicar a convers�o nas vari�veis categoricas para o tipo fator
var.categoricas <- c('credit.rating', 'account.balance', 'previous.credit.payment.status',
                      'credit.purpose', 'savings', 'employment.duration', 'installment.rate',
                      'marital.status', 'guarantor', 'residence.duration', 'current.assets',
                      'other.credits', 'apartment.type', 'bank.credits', 'occupation', 
                      'dependents', 'telephone', 'foreign.worker')
class(var.categoricas)
credito.df1 <- to.factors(df = credito.df1, variables = var.categoricas)
str(credito.df1)
table(credito.df1$credit.rating) #variavel target


## Dividindo os dados em treino e teste - 70:30 ratio - dados de teste avalia a performance do modelo
?sample # Coleta uma amostra aleat�ria do tamanho especificado
indexes <- sample(1:nrow(credito.df1), size = 0.7 * nrow(credito.df1))
treino.data <- credito.df1[indexes,] # criando os dados de treino
teste.data <- credito.df1[-indexes,] # criando os dados de teste
class(treino.data)
class(teste.data)

## separando dentro do conjundo de teste as variaveis preditoras e target para fazer as previsoes
teste.feature.vars <- teste.data[,-1]
teste.class.var <- teste.data[,1]
class(teste.feature.vars)
class(teste.class.var)


###### CRIAR O MODELO PREDITIVO - VERS�O 1 ######

# Criar um modelo de regress�o log�stica com a fun��o glm (modelo de classifica��o)
?glm
modelo.v1 <- glm(formula = "credit.rating ~ .", data = treino.data, family = "binomial")

# Visualizar o modelo
summary(modelo.v1)

# Fazer as previs�es com dados de teste
View(teste.data)
previsao <- predict(modelo.v1, teste.data, type="response")
previsao <- round(previsao) #round faz o arredodamento dos resultados
View(previsao)

# Avaliar o modelo
## Confusion Matrix - compara os valores observados com os previstos
confusionMatrix(table(reference = teste.class.var, data = previsao), positive = '1')



###### OTIMIZAR O MODELO ######

# Feature Selection - identificar as variaveis mais importantes para aplicar no modelo
formula <- "credit.rating ~ ."
formula <- as.formula(formula)
class(formula)
controle <- trainControl(method = "repeatedcv", number = 10, repeats = 2)
modelo <- train(formula, data = treino.data, method = "glm", trControl = controle)
?varImp # calcula as variaveis importantes para modelos de regress�o e classifica��o
importancia <- varImp(modelo, scale = FALSE)

# Plot
plot(importancia)




###### CRIAR O MODELO PREDITIVO - VERS�O 2 ######

# Criar um modelo de regress�o log�stica com a fun��o glm (modelo de classifica��o)
?glm
modelo.v2 <- glm(formula = "credit.rating ~ account.balance + credit.purpose + savings + current.assets +
                 installment.rate + previous.credit.payment.status", data = treino.data, family = "binomial")

# Visualizar o resumo do modelo
summary(modelo.v2)

# Fazer as previs�es com dados de teste
View(teste.data)
previsao <- predict(modelo.v2, teste.data, type="response")
previsao <- round(previsao) #round faz o arredodamento dos resultados
View(previsao)

# Avaliar o modelo
## Confusion Matrix
confusionMatrix(table(reference = teste.class.var, data = previsao), positive = '1') # compara os valores observados com os previstos



##### APRESENTAR O RESULTADO FINAL ######

# Apresentando o resultado final do melhor modelo com rela��o a ACURACIA - Criar curvas ROC
resultado <- modelo.v1
previsoes <- predict(resultado, teste.feature.vars, type = "response") # Faz as previs�es
avaliacao <- prediction(previsoes, teste.class.var) # Avalia��o � a diferen�a entre as previs�es e valores observados
par(mfrow = c(1,2)) # Define a �rea de plotagem
plot.roc.curve(avaliacao, title.text = "Curva ROC") # este arquivo "plot_utils.R" cont�m a fun��o para a plotagem
plot.pr.curve(avaliacao, title.text = "Curva Precision/Recall")




###### CONCLUS�O #######

# Acur�cia da vers�o 2 do modelo foi < que a vers�o 1, por�m pode ser que a vers�o 2 seja um modelo mais generaliz�vel.



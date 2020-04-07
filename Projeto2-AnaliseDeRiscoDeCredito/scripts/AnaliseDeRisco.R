###### DEFINIR O PROBLEMA DE NEGÓCIO A SER RESOLVIDO ######

# Construir um modelo preditivo generalizável para análise de risco de crédito - Modelo de Classificação

# Configurar o diretorio de trabalho
setwd("D:/FCD/Projetos/DSA---FormacaoCientistaDeDados---R/Projeto2-AnaliseDeRiscoDeCredito/scripts")
getwd()

# Biblioteca de utilitários para construção de gráficos
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

# Compreender o conjunto de dados | Descobrir a correlação entre as variáveis | Detectar outliers e valores missing | Encontrar variáveis chave
str(credito.df)
View(credito.df)
class(credito.df)
dim(credito.df)

# Análise Exploratoria para variaveis numericas - resumo das variáveis
summary(credito.df$credit.amount)# variavel quantidade de credito nao esta com dist. normal, pois media e mediana são diferentes
summary(credito.df$age)# variavel idade nao esta com dist. normal, pois media e mediana são diferentes
summary(credito.df$credit.duration.months)


# BoxPlot - visualizar valores outliers
?boxplot
boxplot(credito.df$credit.amount, main = "boxplot para a quantidade de crédito", ylab = "Quantidade de Crédito")
boxplot(credito.df$age, main = "boxplot para o idade", ylab = "Idade")
boxplot(credito.df$credit.duration.months, main = "boxplot para meses de duração do credito", ylab = "Meses de Duração do Crédito")

# Histograma - distribuição de 1 variavel
?hist
hist(credito.df$credit.amount, main = "histograma para o quantidade de credito", xlab = "Quantidade de Crédito")
hist(credito.df$age, main = "histograma para a idade", xlab = "Idade")
hist(credito.df$credit.duration.months, main = "histograma para meses de duração de credito", xlab = "Meses de Duração do Crédito")

# Scatterplot quantidade de crédito x idade - relação entre 2 variaveis
?plot
plot(x = credito.df$age, y = credito.df$credit.amount,
     main = "Scaterplot - quantidade de credito x idade",
     ylab = "Quantidade de Credito",
     xlab = "Idade")

# Medidas de Dispersão (variancia e desvio padrao)
## Se variancia e desvio padrão estiverem altos, significa que os dados estão muito espalhados com relação a media
var(credito.df$credit.amount)
sd(credito.df$credit.amount)
var(credito.df$age)
sd(credito.df$age)

# Verificar valores missing (NA)
any(is.na(credito.df))



######## PRÉ-PROCESSAMENTO ############

# Limpeza, transformação e manipulação dos dados (Preparação dos dados)

## Função para converter as variáveis categóricas para o tipo fator 
to.factors <- function(df, variables){
  for (variable in variables){
    df[[variable]] <- as.factor(df[[variable]])
  }
  return(df)
}

## Função para Padronização de variaveis tipo numerica
scale.features <- function(df, variables){
  for (variable in variables){
    df[[variable]] <- scale(df[[variable]], center=T, scale=T)
  }
  return(df)
}

## Aplicar a padronização (normalização) nas variáveis numericas
var.numericas <- c("credit.duration.months", "age", "credit.amount")
credito.df1 <- scale.features(credito.df, var.numericas)
View(credito.df1)

## Aplicar a conversão nas variáveis categoricas para o tipo fator
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
?sample # Coleta uma amostra aleatória do tamanho especificado
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


###### CRIAR O MODELO PREDITIVO - VERSÃO 1 ######

# Criar um modelo de regressão logística com a função glm (modelo de classificação)
?glm
modelo.v1 <- glm(formula = "credit.rating ~ .", data = treino.data, family = "binomial")

# Visualizar o modelo
summary(modelo.v1)

# Fazer as previsões com dados de teste
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
?varImp # calcula as variaveis importantes para modelos de regressão e classificação
importancia <- varImp(modelo, scale = FALSE)

# Plot
plot(importancia)




###### CRIAR O MODELO PREDITIVO - VERSÃO 2 ######

# Criar um modelo de regressão logística com a função glm (modelo de classificação)
?glm
modelo.v2 <- glm(formula = "credit.rating ~ account.balance + credit.purpose + savings + current.assets +
                 installment.rate + previous.credit.payment.status", data = treino.data, family = "binomial")

# Visualizar o resumo do modelo
summary(modelo.v2)

# Fazer as previsões com dados de teste
View(teste.data)
previsao <- predict(modelo.v2, teste.data, type="response")
previsao <- round(previsao) #round faz o arredodamento dos resultados
View(previsao)

# Avaliar o modelo
## Confusion Matrix
confusionMatrix(table(reference = teste.class.var, data = previsao), positive = '1') # compara os valores observados com os previstos



##### APRESENTAR O RESULTADO FINAL ######

# Apresentando o resultado final do melhor modelo com relação a ACURACIA - Criar curvas ROC
resultado <- modelo.v1
previsoes <- predict(resultado, teste.feature.vars, type = "response") # Faz as previsões
avaliacao <- prediction(previsoes, teste.class.var) # Avaliação é a diferença entre as previsões e valores observados
par(mfrow = c(1,2)) # Define a área de plotagem
plot.roc.curve(avaliacao, title.text = "Curva ROC") # este arquivo "plot_utils.R" contém a função para a plotagem
plot.pr.curve(avaliacao, title.text = "Curva Precision/Recall")




###### CONCLUSÃO #######

# Acurácia da versão 2 do modelo foi < que a versão 1, porém pode ser que a versão 2 seja um modelo mais generalizável.



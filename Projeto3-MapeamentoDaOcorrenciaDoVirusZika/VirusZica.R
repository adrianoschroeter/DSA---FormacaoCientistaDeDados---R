##### DEFINIR O PROBLEMA DE NEGOCIO A SER RESOLVIDO #####

# Fazer uma analise de dados para mapear a ocorrencia do Virus Zica no Brasil

# Configurar o diretorio de trabalho
setwd("D:/FCD/Projetos/DSA---FormacaoCientistaDeDados---R/Projeto3-MapeamentoDaOcorrenciaDoVirusZika")
getwd()

# Verificar os pacotes instalados
search()
searchpaths()



# Instalar os pacotes
install.packages("dplyr")
install.packages("ggplot2")
install.packages("ggmap")


# Carregar os pacotes
library(readr)
library(dplyr)
library(ggplot2)



###### CARGA DOS DADOS ######

# Carregar os arquivos e gera uma lista com os respectivos nomes
lst.arquivos <- list.files(pattern = ".csv")
lst.arquivos

?lapply # aplica uma função a uma lista ou vetor
arquivos <- lapply(lst.arquivos, read.csv, stringsAsFactors = FALSE)
class(arquivos)
View(arquivos)

# Sumario dos arquivos
str(arquivos, 2)
lapply(arquivos, head,6)



##### PRÉ-PROCESSAMENTO #####

## Limpeza, transformação e manipulação dos dados (Preparação dos dados)

# Organizando o formato dos dados
?do.call # executa uma chamada a função rbind (combina objetos por linhas ou colunas) - transformando datasets em dataframe
df_brasil <- do.call(rbind, arquivos)
str(df_brasil)

?mutate # transformar a variavel "report_date" em formato data
df_brasil <- df_brasil %>% 
  mutate(report_date = as.Date(report_date))
str(df_brasil)
class(df_brasil)
View(df_brasil)

# Transformar o dataframe um uma tabela dplyr para aplicar funcões deste pacote; e remover as colunas 6 e 7 (observações destas coluna possuem valores missing)
df2_brasil <- df_brasil %>% select(-(6:7)) 

# Visualizando as primeiras 10 linhas
df2_brasil %>% slice (1:10) 

# Para cada reporting_date temos 5 regioes
df2_brasil %>% filter(location_type == "region")

# Visualizar o filtro por regiao de forma grafica em linhas
df2_brasil %>% filter(location_type == "region") %>% 
  ggplot(aes(x = report_date, y = value, group = location, color = location)) + geom_line() +  geom_point() +
  ggtitle("Casos de Zika por Regiao do Brasil")

# Separando as regioes
regiao <- df2_brasil %>% 
  filter(location_type == "region")

# Visualizar o filtro de forma grafica em barras
options(scipen = 999) # Não apresenta numeros científicos
regiao %>% 
  ggplot(aes(x = location, y = value)) + geom_bar(stat = "identity") + ylab("Numero de Casos Reportados") + xlab("Regiao") +
  ggtitle("Casos de Zika Reportados no Brasil")

# Ordenando a classificação do > para o <.
regiao %>% 
  slice(1:length(unique(regiao$location))) %>% 
  arrange(desc(value)) %>%
  mutate(location = factor(location, levels = location,ordered = TRUE)) %>%
  ggplot(aes(x = location, y = value)) + geom_bar(stat = "identity") + ylab("Numero de Casos Reportados") + xlab("Regiao") + 
  ggtitle("Casos de Zika Reportados no Brasil")

# Obter as localidades unicas por numero de casos reportados e ordenar
regiao %>% 
  slice(1:length(unique(regiao$location))) %>% 
  arrange(desc(value))

# Criar as variaveis do tipo fator
regiao %>% 
  slice(1:length(unique(regiao$location))) %>% 
  arrange(desc(value)) %>%
  mutate(location = factor(location,levels=location,ordered=TRUE)) %>% 
  glimpse()

# Agrupando e Sumarizando
brasil_total <- df2_brasil %>% filter(location=="Brazil") 
regiao_total <- df2_brasil %>% filter(location_type=="region") %>%
  group_by(report_date,location) %>%  
  summarize(tot = sum(value)) 

# Padronizar os dados e remover as sumarizacoes
vetreg <- vector()  
length(vetreg) <- nrow(df2_brasil)
for (i in 1:nrow(df_brasil)) {
  if (df2_brasil[i,]$location_type != "region")  {
    vetreg[i] <- newlab
  } else {
    newlab <- df2_brasil[i,]$location
    vetreg[i] <- newlab
  }
}

# Agregando o vetor de regioes ao dataframe brasil
df_estado <- cbind(df2_brasil,vetreg)
View(df_estado)

# Eliminar o sumario de linhas por regiao e paises
df_estado <- df_estado %>% filter(location != "Brazil") 
df_estado <- df_estado %>% filter(location_type != "region") 

# Gerar o total por regioes a partir dos dados transformados
df_estado %>% group_by(report_date,vetreg) %>% 
  summarize(tot=sum(value)) -> totals




##### APRESENTANDO OS RESULTADOS #####

# Gerando os mapas de cada estado do Brasil
library(ggmap)

register_google(key = "????")
longlat <- geocode(unique(df_estado$location)) %>% 
  mutate(loc = unique(df_estado$location)) 

# Salvando os geocodes do dataframe df_estado em um novo dataframe chamado formapping
df_estado %>% filter(as.character(report_date) == "2016-06-11") %>% 
  group_by(location) %>% summarize(cases = sum(value)) %>% 
  inner_join(longlat, by = c("location" = "loc")) %>% 
  mutate(LatLon = paste(lat, lon, sep = ":")) -> formapping

# Visualizando os dados
View(formapping) 

# Formatando a saida e gerando um movo dataframe chamado long_formapping
num_of_times_to_repeat <- formapping$cases
long_formapping <- formapping[rep(seq_len(nrow(formapping)),num_of_times_to_repeat),]

# Visualizando os dados
View(long_formapping)

# Instalando o pacote leaflet
install.packages("leaflet")
library(leaflet)

# Gerando o mapa com o dataframe
# Aplique o zoom
leaflet(long_formapping) %>% 
  addTiles() %>% 
  addMarkers(clusterOptions = markerClusterOptions())

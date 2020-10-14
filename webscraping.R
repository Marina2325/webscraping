
## WEBSCRAPING

## 5.5.1.1

## Instalar pacotes 

if(require(tidyverse) == F) install.packages('tidyverse'); require(tidyverse)
if(require(rvest) == F) install.packages('rvest'); require(rvest)
if(require(httr) == F) install.packages('httr'); require(httr)
if(require(xml2) == F) install.packages('xml2'); require(xml2)

## Definir endereço da web 

link <- "https://pt.wikipedia.org/wiki/Lista_de_munic%C3%ADpios_do_Brasil_por_IDH"

## obter os dados 

conteudo <- readLines(link) # código fonte 

head(conteudo)

## verificar em qual linha está Pernambuco 

grep("Pernambuco", conteudo)

conteudo[784]

conteudo[784 + 4] ### linha com o IDH de Pernambuco 
conteudo[784 + 9] ### Próxima UF
conteudo[784 + 9 + 9] ### Identificando um padrão 

## Selecionar todas as linhas que apresentam os nomes dos municípios 

indice <- 109
nomes_uf <- NULL
i <- 1
while(indice < 1081){
  if(i==1){
    nomes_uf[i] <- conteudo[indice]
  } else{
    nomes_uf[i] <- conteudo[indice+9]
  }
  indice <- indice + 9
  i <- i + 1
}

## Obter um vetor com o nome das UF's

nomes_uf <- gsub("[[:print:]]+\">", "", nomes_uf)
nomes_uf <- gsub("</a>", "", nomes_uf)
nomes_uf <- gsub("</b>", "", nomes_uf)
nomes_uf <- gsub("<b>", "", nomes_uf)
nomes_uf <- gsub("£", "", nomes_uf)

## Lista das UF's

nomes_uf 

## 5.5.2.1

library('rvest')

## Obter o endereço das páginas de todos os deputados federais 

page <- read_html('https://www.camara.leg.br/internet/deputado/DepNovos_Lista.asp?
Legislatura=54&Partido=QQ&SX=QQ&Todos=None&UF=QQ&condic=QQ&forma=lista&nome=&ordem=nome&origem=')

page %>% html_nodes('a') %>% html_attr('href')

## 5.7.0.1 

if(require(httr) == F) install.packages('httr'); require(httr);
if(require(XML) == F) install.packages('XML'); require(XML);
if(require(xml2) == F) install.packages('xml2'); require(xml2);

link <- paste0("http://www.camara.leg.br/SitCamaraWS/Deputados.asmx/ObterDeputados")

## Obter os dados 

response <- GET(link)

## Processar os dados obtidos 

data <- xmlParse(response, encoding = "UTF-8")
ls <- xmlToList(data)

names(ls$deputado)

ideCadastro <- NULL
condicao <- NULL
matricula <- NULL
idParlamentar <- NULL
nome <- NULL
nomeParlamentar <- NULL
urlFoto <- NULL
sexo <- NULL
uf <- NULL
partido <- NULL
email <- NULL

for(i in 1:length(ls)){
  ideCadastro[i] <- ls[[i]]$ideCadastro
  condicao[i] <- ls[[i]]$condicao
  matricula[i] <- ls[[i]]$matricula
  idParlamentar[i] <- ls[[i]]$idParlamentar
  nome[i] <- ls[[i]]$nome
  nomeParlamentar[i] <- ls[[i]]$nomeParlamentar
  urlFoto[i] <- ls[[i]]$urlFoto
  sexo[i] <- ls[[i]]$sexo
  uf[i] <- ls[[i]]$uf
  partido[i] <- ls[[i]]$partido
  email[i] <- ls[[i]]$email
}

bd <- data.frame(ideCadastro, condicao, matricula, idParlamentar, nome,
                 nomeParlamentar, urlFoto, sexo, uf, partido, email)

head(ideCadastro)

## Obter detalhes dos deputados federais que representam Pernambuco 

deputados_pe <- bd %>% filter(uf == 'PE') 

deputados_pe

----------------------------------------------------------------------------


library(rvest)
library(httr)
library(xml2)
library(magrittr)
library(XML)
library(purrr)

legislatura <- (41:55)

## lista com urls das legislaturas 41:55

lista_urls <- sprintf('https://www2.camara.leg.br/deputados/pesquisa/layouts_deputados_resultado_pesquisa?nome=&Partido=QQ&UF=QQ&SX=QQ&Legislatura=%s&condic=QQ&ordem=nome&forma=lista&Pesquisa=',legislatura)

## Criar uma função pra fazer um data frame com os nomes dos deputados 

nomes <- function(x){
  x %>% read_html() %>% html_nodes('.demaisInformacoes span') %>% html_text() %>% data.frame()
}

## Criar uma função pra fazer um data frame com os sites 

sites <- function(x){
  x %>% read_html() %>% html_nodes('.demaisInformacoes a')%>% html_attr('href') %>% data.frame()
}

d <- lapply(lista_urls, nomes)
d2<- lapply(lista_urls, sites)

## Ler legislatura 

for(i in seq_along(d)){
  d[[i]]$legislatura <- rep(legislatura[i], nrow(d[[i]]))
}

d_1 <- data.table::rbindlist(d)

d_2 <- data.table::rbindlist(d2)

## Renomear colunas

colnames(d_1)[1] <- 'Nome'

colnames(d_2)[1]<- 'Site'

## Criar tabela 

zz <- cbind(d_1,d_2)

## Remover separador 

zz$Site <- stringr::str_remove_all(zz$Site, '//')

## Resultado da tabela final 

head(zz)

------------------------------------------------------------------------------

 

require(reshape2)
require(dplyr)
require(stringi)
require(ggplot2)
require(rgdal)
library(rvest)
library(httr)
library(xml2)
library(XML)

## Obter os dados do IDHM na página do Wikipédia

link <- paste0('https://pt.wikipedia.org/wiki/Regi%C3%A3o_Metropolitana_do_Recife')
Metro_recife <- link %>% httr::GET() %>% xml2::read_html()%>% rvest::html_nodes('table')%>% rvest::html_table(header = TRUE, fill = TRUE)
names(Metro_recife)<- 1:7
reg_metro <- Metro_recife$`3`
reg_metro[1]<- NULL
colnames(reg_metro)[4]<- "IDHM"
reg_metro <- transform(reg_metro, IDHM = substr(IDHM, 1,5))              

## carregar shapefile e corrigir nomes

shapefile_pe <-readOGR(dsn = 'C:/Users/marin/Downloads/pe_municipios/26MUE250GC_SIR.shp',stringsAsFactors =  FALSE)
shapefile_pe$NM_MUNICIP <- gsub("ARAÃ‡OIABA", "ARAÇOIABA", shapefile_pe$NM_MUNICIP)
shapefile_pe$NM_MUNICIP <- gsub("ILHA DE ITAMARACÃ\u0081", "ILHA DE ITAMARACÁ", shapefile_pe$NM_MUNICIP)
shapefile_pe$NM_MUNICIP <- gsub("JABOATÃƒO DOS GUARARAPES", "JABOATÃO DOS GUARARAPES", shapefile_pe$NM_MUNICIP)
shapefile_pe$NM_MUNICIP <- gsub("SÃƒO LOURENÃ‡O DA MATA", "SÃO LOURENÇO DA MATA", shapefile_pe$NM_MUNICIP)
reg_metro$IDHM <- gsub(',','.', reg_metro$IDHM)
reg_metro$Município <- toupper(reg_metro$Município)

shapefile_pe$NM_MUNICIP

## Selecionar municípios da Região Metropolitana de PE

shape_metro <- subset(shapefile_pe, NM_MUNICIP %in% reg_metro$Município)
numeric(length = 4)

shape_metro$IDHM <- as.numeric(reg_metro$IDHM[1:15])
as.numeric(as.character(shape_metro$IDHM))

shapefile_df <- fortify(shape_metro)
dim(shapefile_df)
names(shapefile_df)
head(shapefile_df)
shapefile_data <- fortify(shape_metro@data)
shapefile_data$id <- row.names(shapefile_data)
shapefile_df <- full_join(shapefile_df, shapefile_data, by="id")
names(shapefile_df)

## Visualizar shapefile

head(shapefile_df)
View(shapefile_df)

## Gráfico IDHM Região Metropolitana do Recife

install.packages('mapproj')
library(mapproj)
library(ggplot2)

mapa_IDHM <- ggplot() + geom_polygon(data = shapefile_df,
aes(x = long, y = lat, group = group, fill = IDHM),
colour = "gray", size = .2) +
theme_void() + ### Essa é a função que dexa o fundo vazio
# scale_fill_manual(values = c("Black", "Orange", "Brown")) +
scale_fill_gradient2(low = "#ffeda0", mid="#feb24c", high = "#f03b20",
midpoint = median(shapefile_df$IDHM), limits = range(shapefile_df$IDHM)) + coord_map() + 
ggtitle("IDHM da Região Metropolitana do Recife")

## Visualizar mapa 

mapa_IDHM 

-------------------------------------------------------------------------------------------------------------
 
## Mapa    

library(readxl)
library(dplyr)
library(rgdal)
library(ggplot2)
library(mapproj)

## Ler arquivo do PNUD e selecionar o Estado de Rondônia

atlas_df <- read_excel("C:/Users/marin/OneDrive/Área de Trabalho/Nova pasta/atlas2013_dadosbrutos_pt.xlsx")
atlas_IDHM <- atlas_df %>% select(ANO, UF, Codmun7, IDHM) %>% subset(ANO == '2010'& UF == '11')
shape_BR <- readOGR(dsn = 'C:/Users/marin/OneDrive/Área de Trabalho/Nova pasta/BR/BR/BRMUE250GC_SIR.shp')

shape_BR_2<- subset(shape_BR, CD_GEOCMU %in% atlas_IDHM$Codmun7)
shape_BR_2$idhm <- atlas_IDHM$IDHM

shapefile_df <- fortify(shape_BR_2)
shapefile_data <- fortify(shape_BR_2@data)
shapefile_data$id <- row.names(shapefile_data)
shapefile_df <- full_join(shapefile_df, shapefile_data, by="id")
names(shapefile_df)

## Gráfico IDHM  dos municípios de Rondônia

install.packages('mapproj')
library(mapproj)
library(ggplot2)

mapa_IDHM_RO <- ggplot() + geom_polygon(data = shapefile_df,
aes(x = long, y = lat, group = group, fill = idhm),
colour = "gray", size = .2) +
theme_void() + # essa é a função que dexa o fundo vazio
# scale_fill_manual(values = c("Black", "Orange", "Brown")) +
scale_fill_gradient2(low = "#ffeda0", mid="#feb24c", high = "#f03b20",
midpoint = median(shapefile_df$idhm),
limits = range(shapefile_df$idhm)) + coord_map() + ggtitle("IDHM dos municípios de Rondônia")

## Visualizar mapa 

mapa_IDHM_RO



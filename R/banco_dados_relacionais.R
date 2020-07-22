############################
# Manipulação de banco de dados relacionais
# reprdução do script da Sara Mortara
# https://rpubs.com/saramortara/574221
# origem dos dados: https://www.nature.com/articles/s41597-019-0344-7
# aula teórica: https://gitlab.com/liibre/curso/-/wikis/uploads/0bafe70d329a7483ca17b2c12e5bb8c4/aula05.pdf
# Kele Rocha Firmiano
############################

# 1. carregando pacotes, dados, lendo e entendendo os objetos ####
# pacotes
library("tidyr")

# carregando dados (list.files: pacote base)
files.path <- list.files(path = "data/cestes",
                         pattern = ".csv",
                         full.names = TRUE)

files.path # vetor com 5 elementos, que são os 5 datasets

# lendo dados cestes
comm <- read.csv(files.path[1]) # abundancia
coord <- read.csv(files.path[2]) # coordenadas geo
envir <- read.csv(files.path[3]) # variaveis ambientais
splist <- read.csv(files.path[4]) # lista de especies
traits <- read.csv(files.path[5]) # traits

# entendendo os objetos. uses as funções head, dim e summary para cada um deles:
head(comm) # primeiras 5 linhas
dim(comm) # dimensões da matriz
summary(comm) # descriçaõ de cada variaviel

# sumarizando os dados:
nrow(splist) # n espécies
nrow(comm) # n amostras
nrow(envir) # n amostras
length(names(envir)[-1]) # n variaveis ambientais (necessário tirar a 1 coluna pq é soh o nome)

# riqueza da área:
comm.pa <- comm[, -1] > 0 # matriz de abundancia em matriz PA. 1=TRUE, 0=FALSE
row.names(comm.pa) <- envir$Sites # nomeia as linhas das planilhas com o id dos sites.
sum(comm.pa[1, ]) # riqueza da área 1
rich <- apply(X = comm.pa, MARGIN = 1, FUN = sum) # riqueza de todas as áreas fx: apply
summary(rich)

# 2.unindo matrizes através de chaves/ identificadores
coord$Sites # parei aqui



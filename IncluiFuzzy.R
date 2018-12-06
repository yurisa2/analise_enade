PATH <- "C:/Bitnami/wampstack-7.1.20-1/apache2/htdocs/analise_enade"
setwd(PATH)

source(file="include/functions.R")

library(FuzzyR)
# library(lattice)
library(gmodels)

########### AQUISICAO DOS DADOS
r1a <- read.csv("db/1a.csv", header=T, stringsAsFactors=F)
r7a <- read.csv("db/7a.csv", header=T, stringsAsFactors=F)
r7b <- read.csv("db/7b.csv", header=T, stringsAsFactors=F)
r8a <- read.csv("db/8a.csv", header=T, stringsAsFactors=F)
r8b <- read.csv("db/8b.csv", header=T, stringsAsFactors=F)
r8c <- read.csv("db/8c.csv", header=T, stringsAsFactors=F)
r8g <- read.csv("db/8g.csv", header=T, stringsAsFactors=F)


########### SEPARACAO DE SETS
total <- rbind(r1a,r7a,r7b,r8a,r8b,r8c,r8g)
total_masculino <- total[total$questao1=="1",]
total_feminino <- total[total$questao1=="2",]
total_oitavo <- rbind(r8a,r8b,r8c,r8g)
total_setimo <-  rbind(r7a,r7b)
total_primeiro <- r1a
###########SETTINGS
col_numeric <- ncol(total)-1
cols_enade <- 4:10

cols_aji <- c(4,5,6,9)

cols_test_aluno <- c(4,7,8,10)

for(i in 1:col_numeric) {
  total[,i] <- as.numeric(total[,i])
  total_masculino[,i] <- as.numeric(total_masculino[,i])
  total_feminino[,i] <- as.numeric(total_feminino[,i])
  total_oitavo[,i] <- as.numeric(total_oitavo[,i])
  total_setimo[,i] <- as.numeric(total_setimo[,i])
  total_primeiro[,i] <- as.numeric(total_primeiro[,i])
}


entrada <- as.matrix(total[,cols_aji])
entrada <- (entrada - 1) * 0.25
entrada[is.na(entrada)] <- 0.5

source(file="include/functions.R")
totalizante <- cbind(entrada,resultado=fuz_sis(total,cols_aji,entrada))
# hist(totalizante[,5])




entrada_aluno <- as.matrix(total[,cols_test_aluno])
entrada_aluno <- (entrada_aluno - 1) * 0.25
entrada_aluno[is.na(entrada_aluno)] <- 0.5

source(file="include/functions.R")
totalizante_ta <- cbind(entrada_aluno,resultado=fuz_sis(total,cols_test_aluno,entrada_aluno))
hist(totalizante_ta[,5])

hist(create_fuzzy_rules(total,cols_aji))

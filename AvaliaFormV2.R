# PATH <- "C:/Bitnami/wampstack-7.1.20-1/apache2/htdocs/analise_enade"
# setwd(PATH)

# install.packages("FuzzyR")
# install.packages("shiny")

library(FuzzyR)


teste <- read.csv("teste.csv", header=T, stringsAsFactors=F)


for(i in 1:ncol(teste)) teste[,i] <- as.numeric(teste[,i])
for(i in 1:ncol(teste)) teste[,i] <- factor(teste[,i])
#
str(teste)
head(teste)

summary(teste)

for(i in 1:ncol(teste)) {
hist(as.numeric(teste[,i]),
     main={colnames(teste)[i]},
     xlab="Respostas!",
     border="blue",
     col="green",
     xlim=c(1,5),
     prob = TRUE)
}
# Analise_Enade <- addRule(Analise_Enade, regras)

entrada <- matrix(c(teste[,5],teste[,6],teste[,7],teste[,8],teste[,9],teste[,10],teste[,4]), ncol = 7)
entrada <- (entrada - 1) * 0.25

TestarAluno_FIS <- readfis("TestarAluno.fis")
AjudarInstituicao_FIS <- readfis("AjudarInstituicao.fis")

# plotmf(TestarAluno_FIS, "output", 1)
# showGUI(TestarAluno_FIS)



results <- cbind (entrada,evalfis(entrada,TestarAluno_FIS))
results <- cbind (results,evalfis(entrada,AjudarInstituicao_FIS))

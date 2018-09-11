PATH <- "C:/Bitnami/wampstack-7.1.20-1/apache2/htdocs/analise_enade"
setwd(PATH)


library(FuzzyToolkitUoN)

# teste <- read.csv("teste.csv", header=T, stringsAsFactors=F)

#
# for(i in 1:ncol(teste)) teste[,i] <- as.numeric(teste[,i])
# for(i in 1:ncol(teste)) teste[,i] <- factor(teste[,i])
# #
# str(teste)
#
#
# summary(teste)
#
# for(i in 1:ncol(teste)) {
# hist(as.numeric(teste[,i]),
#      main={colnames(teste)[i]},
#      xlab="Passengers",
#      border="blue",
#      col="green",
#      xlim=c(0,5),
#      prob = TRUE)
# }

range <- seq(0,1,0.01)

rm(Analise_Enade)
Analise_Enade <- newFIS("Analise_Enade", FISType = "mamdani", andMethod="min", defuzzMethod="centroid")

Analise_Enade <- addVar(Analise_Enade, "input", "questao4", range)
questao4MF1 <-triMF('CPL',range,c(0.75,1,1,1))
questao4MF2 <-triMF('CPA',range,c(0.5,0.75,1,1))
questao4MF3 <-triMF('NSR',range,c(0.25,0.5,0.75,1))
questao4MF4 <-triMF('DP',range,c(0,0.25,0.5,1))
questao4MF5 <-triMF('DT',range,c(0,0,0.25,1))
Analise_Enade <- addMF(Analise_Enade,"input",1,questao4MF1)
Analise_Enade <- addMF(Analise_Enade,"input",1,questao4MF2)
Analise_Enade <- addMF(Analise_Enade,"input",1,questao4MF3)
Analise_Enade <- addMF(Analise_Enade,"input",1,questao4MF4)
Analise_Enade <- addMF(Analise_Enade,"input",1,questao4MF5)

Analise_Enade <- addVar(Analise_Enade, "input", "questao5", range)
questao5MF1 <-triMF('CPL',range,c(0.75,1,1,1))
questao5MF2 <-triMF('CPA',range,c(0.5,0.75,1,1))
questao5MF3 <-triMF('NSR',range,c(0.25,0.5,0.75,1))
questao5MF4 <-triMF('DP',range,c(0,0.25,0.5,1))
questao5MF5 <-triMF('DT',range,c(0,0,0.25,1))
Analise_Enade <- addMF(Analise_Enade,"input",2,questao5MF1)
Analise_Enade <- addMF(Analise_Enade,"input",2,questao5MF2)
Analise_Enade <- addMF(Analise_Enade,"input",2,questao5MF3)
Analise_Enade <- addMF(Analise_Enade,"input",2,questao5MF4)
Analise_Enade <- addMF(Analise_Enade,"input",2,questao5MF5)


Analise_Enade <- addVar(Analise_Enade, "input", "questao6", range)
questao6MF1 <-triMF('CPL',range,c(0.75,1,1,1))
questao6MF2 <-triMF('CPA',range,c(0.5,0.75,1,1))
questao6MF3 <-triMF('NSR',range,c(0.25,0.5,0.75,1))
questao6MF4 <-triMF('DP',range,c(0,0.25,0.5,1))
questao6MF5 <-triMF('DT',range,c(0,0,0.25,1))
Analise_Enade <- addMF(Analise_Enade,"input",3,questao6MF1)
Analise_Enade <- addMF(Analise_Enade,"input",3,questao6MF2)
Analise_Enade <- addMF(Analise_Enade,"input",3,questao6MF3)
Analise_Enade <- addMF(Analise_Enade,"input",3,questao6MF4)
Analise_Enade <- addMF(Analise_Enade,"input",3,questao6MF5)

Analise_Enade <- addVar(Analise_Enade, "input", "questao7", range)
questao7MF1 <-triMF('CPL',range,c(0.75,1,1,1))
questao7MF2 <-triMF('CPA',range,c(0.5,0.75,1,1))
questao7MF3 <-triMF('NSR',range,c(0.25,0.5,0.75,1))
questao7MF4 <-triMF('DP',range,c(0,0.25,0.5,1))
questao7MF5 <-triMF('DT',range,c(0,0,0.25,1))
Analise_Enade <- addMF(Analise_Enade,"input",4,questao7MF1)
Analise_Enade <- addMF(Analise_Enade,"input",4,questao7MF2)
Analise_Enade <- addMF(Analise_Enade,"input",4,questao7MF3)
Analise_Enade <- addMF(Analise_Enade,"input",4,questao7MF4)
Analise_Enade <- addMF(Analise_Enade,"input",4,questao7MF5)

Analise_Enade <- addVar(Analise_Enade, "input", "questao8", range)
questao8MF1 <-triMF('CPL',range,c(0.75,1,1,1))
questao8MF2 <-triMF('CPA',range,c(0.5,0.75,1,1))
questao8MF3 <-triMF('NSR',range,c(0.25,0.5,0.75,1))
questao8MF4 <-triMF('DP',range,c(0,0.25,0.5,1))
questao8MF5 <-triMF('DT',range,c(0,0,0.25,1))
Analise_Enade <- addMF(Analise_Enade,"input",5,questao8MF1)
Analise_Enade <- addMF(Analise_Enade,"input",5,questao8MF2)
Analise_Enade <- addMF(Analise_Enade,"input",5,questao8MF3)
Analise_Enade <- addMF(Analise_Enade,"input",5,questao8MF4)
Analise_Enade <- addMF(Analise_Enade,"input",5,questao8MF5)


Analise_Enade <- addVar(Analise_Enade, "input", "questao9", range)
questao9MF1 <-triMF('CPL',range,c(0.75,1,1,1))
questao9MF2 <-triMF('CPA',range,c(0.5,0.75,1,1))
questao9MF3 <-triMF('NSR',range,c(0.25,0.5,0.75,1))
questao9MF4 <-triMF('DP',range,c(0,0.25,0.5,1))
questao9MF5 <-triMF('DT',range,c(0,0,0.25,1))
Analise_Enade <- addMF(Analise_Enade,"input",6,questao9MF1)
Analise_Enade <- addMF(Analise_Enade,"input",6,questao9MF2)
Analise_Enade <- addMF(Analise_Enade,"input",6,questao9MF3)
Analise_Enade <- addMF(Analise_Enade,"input",6,questao9MF4)
Analise_Enade <- addMF(Analise_Enade,"input",6,questao9MF5)

Analise_Enade <- addVar(Analise_Enade, "input", "questao10", range)
questao10MF1 <-triMF('CPL',range,c(0.75,1,1,1))
questao10MF2 <-triMF('CPA',range,c(0.5,0.75,1,1))
questao10MF3 <-triMF('NSR',range,c(0.25,0.5,0.75,1))
questao10MF4 <-triMF('DP',range,c(0,0.25,0.5,1))
questao10MF5 <-triMF('DT',range,c(0,0,0.25,1))
Analise_Enade <- addMF(Analise_Enade,"input",7,questao10MF1)
Analise_Enade <- addMF(Analise_Enade,"input",7,questao10MF2)
Analise_Enade <- addMF(Analise_Enade,"input",7,questao10MF3)
Analise_Enade <- addMF(Analise_Enade,"input",7,questao10MF4)
Analise_Enade <- addMF(Analise_Enade,"input",7,questao10MF5)

Analise_Enade <- addVar(Analise_Enade, "output", "AjudarInstituicao", range)
AjudarInstituicaoMF1 <-triMF('Pouco',range,c(0,0,0.25,1))
AjudarInstituicaoMF2 <-triMF('Moderado',range,c(0,0.25,0.5,1))
AjudarInstituicaoMF3 <-triMF('Razoavel',range,c(0.25,0.5,0.75,1))
AjudarInstituicaoMF4 <-triMF('Elevado',range,c(0.5,0.75,1,1))
AjudarInstituicaoMF5 <-triMF('Muito',range,c(0.75,1,1,1))
Analise_Enade <- addMF(Analise_Enade,"output",1,AjudarInstituicaoMF1)
Analise_Enade <- addMF(Analise_Enade,"output",1,AjudarInstituicaoMF2)
Analise_Enade <- addMF(Analise_Enade,"output",1,AjudarInstituicaoMF3)
Analise_Enade <- addMF(Analise_Enade,"output",1,AjudarInstituicaoMF4)
Analise_Enade <- addMF(Analise_Enade,"output",1,AjudarInstituicaoMF5)

Analise_Enade <- addVar(Analise_Enade, "output", "TestaAluno", range)
TestaAlunoMF1 <-triMF('Pouco',range,c(0,0,0.25,1))
TestaAlunoMF2 <-triMF('Moderado',range,c(0,0.25,0.5,1))
TestaAlunoMF3 <-triMF('Razoavel',range,c(0.25,0.5,0.75,1))
TestaAlunoMF4 <-triMF('Elevado',range,c(0.5,0.75,1,1))
TestaAlunoMF5 <-triMF('Muito',range,c(0.75,1,1,1))
Analise_Enade <- addMF(Analise_Enade,"output",2,TestaAlunoMF1)
Analise_Enade <- addMF(Analise_Enade,"output",2,TestaAlunoMF2)
Analise_Enade <- addMF(Analise_Enade,"output",2,TestaAlunoMF3)
Analise_Enade <- addMF(Analise_Enade,"output",2,TestaAlunoMF4)
Analise_Enade <- addMF(Analise_Enade,"output",2,TestaAlunoMF5)
#
Analise_Enade <- addRule(Analise_Enade, c(1,0,0,0,0,0,0,1,0,1,1))
Analise_Enade <- addRule(Analise_Enade, c(2,0,0,0,0,0,0,2,0,1,1))
Analise_Enade <- addRule(Analise_Enade, c(3,0,0,0,0,0,0,3,0,1,1))
Analise_Enade <- addRule(Analise_Enade, c(4,0,0,0,0,0,0,4,0,1,1))
Analise_Enade <- addRule(Analise_Enade, c(5,0,0,0,0,0,0,5,0,1,1))
Analise_Enade <- addRule(Analise_Enade, c(0,1,0,0,0,0,0,2,0,1,1))
Analise_Enade <- addRule(Analise_Enade, c(0,2,0,0,0,0,0,3,0,1,1))
Analise_Enade <- addRule(Analise_Enade, c(0,3,0,0,0,0,0,3,0,1,1))

# Analise_Enade <- addRule(Analise_Enade, c(0,4,0,0,0,0,0,3,0,1,1))

# Analise_Enade <- addRule(Analise_Enade, c(0,5,0,0,0,0,0,4,0,1,1))
#
# Analise_Enade <- addRule(Analise_Enade, c(0,0,1,0,0,0,0,0,1,1,1))
#
#
# Analise_Enade <- addRule(Analise_Enade, c(0,0,2,0,0,0,0,2,2,1,1))
# Analise_Enade <- addRule(Analise_Enade, c(0,0,3,0,0,0,0,3,3,1,1))
# Analise_Enade <- addRule(Analise_Enade, c(0,0,4,0,0,0,0,4,4,1,1))
# Analise_Enade <- addRule(Analise_Enade, c(0,0,5,0,0,0,0,5,5,1,1))
# Analise_Enade <- addRule(Analise_Enade, c(0,0,0,1,0,0,0,0,1,1,1))
# Analise_Enade <- addRule(Analise_Enade, c(0,0,0,2,0,0,0,0,2,1,1))
# Analise_Enade <- addRule(Analise_Enade, c(0,0,0,3,0,0,0,0,3,1,1))
# Analise_Enade <- addRule(Analise_Enade, c(0,0,0,4,0,0,0,0,4,1,1))
# Analise_Enade <- addRule(Analise_Enade, c(0,0,0,5,0,0,0,0,5,1,1))
# Analise_Enade <- addRule(Analise_Enade, c(0,0,0,0,1,0,0,2,1,1,1))
# Analise_Enade <- addRule(Analise_Enade, c(0,0,0,0,2,0,0,3,2,1,1))
# Analise_Enade <- addRule(Analise_Enade, c(0,0,0,0,3,0,0,3,3,1,1))
# Analise_Enade <- addRule(Analise_Enade, c(0,0,0,0,4,0,0,3,4,1,1))
# Analise_Enade <- addRule(Analise_Enade, c(0,0,0,0,5,0,0,4,5,1,1))
# Analise_Enade <- addRule(Analise_Enade, c(0,0,0,0,0,1,0,1,0,1,1))
# Analise_Enade <- addRule(Analise_Enade, c(0,0,0,0,0,2,0,2,0,1,1))
# Analise_Enade <- addRule(Analise_Enade, c(0,0,0,0,0,3,0,3,0,1,1))
# Analise_Enade <- addRule(Analise_Enade, c(0,0,0,0,0,4,0,4,0,1,1))
# Analise_Enade <- addRule(Analise_Enade, c(0,0,0,0,0,5,0,5,0,1,1))
# Analise_Enade <- addRule(Analise_Enade, c(0,0,0,0,0,0,1,2,1,1,1))
# Analise_Enade <- addRule(Analise_Enade, c(0,0,0,0,0,0,2,3,2,1,1))
# Analise_Enade <- addRule(Analise_Enade, c(0,0,0,0,0,0,3,3,3,1,1))
# Analise_Enade <- addRule(Analise_Enade, c(0,0,0,0,0,0,4,3,4,1,1))
# Analise_Enade <- addRule(Analise_Enade, c(0,0,0,0,0,0,5,4,5,1,1))

# Analise_Enade <- addRule(Analise_Enade, regras)
showFIS(Analise_Enade)
entrada <- matrix(c(0,0,0,0,0,0,0), ncol = 7)

analise2 <- readFIS("fis2b.fis")

result <- evalFIS(entrada,analise2)

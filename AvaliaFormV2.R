PATH <- "C:/Bitnami/wampstack-7.1.20-1/apache2/htdocs/analise_enade"
setwd(PATH)


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

########### CONVERTS
for(i in 1:col_numeric) {
  total[,i] <- as.numeric(total[,i])
  total_masculino[,i] <- as.numeric(total_masculino[,i])
  total_feminino[,i] <- as.numeric(total_feminino[,i])
  total_oitavo[,i] <- as.numeric(total_oitavo[,i])
  total_setimo[,i] <- as.numeric(total_setimo[,i])
  total_primeiro[,i] <- as.numeric(total_primeiro[,i])
}

########### LABELS
labels <- read.csv("db/labels.csv",stringsAsFactors=F)
labels_resp <- c("CPL","CPA","NSR","DPA","DTL")


########### VETOR DAS CORES PARA AS BARRAS
col_colors <- c("green","green","green","red","red","purple","blue","purple",
"red","purple","purple","purple")



########### FUNCTIONS



get_fuzzy_res <- function(obj_) {
  entrada_fuzzy <- as.matrix(obj_[,cols_enade])
  entrada_fuzzy <- (entrada_fuzzy - 1) * 0.25

  TestarAluno_FIS <- readfis("fis/TestarAluno.fis")
  AjudarInstituicao_FIS <- readfis("fis/AjudarInstituicao.fis")

  results <- cbind(entrada_fuzzy,evalfis(entrada_fuzzy,TestarAluno_FIS))
  colnames(results)[ncol(results)] <- "TestarAluno"
  results <- cbind(results,evalfis(entrada_fuzzy,AjudarInstituicao_FIS))
  colnames(results)[ncol(results)] <- "AjudarInstituicao"

  plotmf(TestarAluno_FIS,"output",1)

  results <- data.frame(results)
  return(results)
}

questoes_abs <- NULL
questoes_abs <- list()
questoes_pctg <- NULL
questoes_pctg <- list()



create_Categoricas_Plots <- function(obj_,set) {
  filename <- paste("plots/Respostas Categoricas - ",set,".png")
  png(filename, width = 640, height = 896)
  par(mfrow=c(5,2))

  for(i in 1:10)
  {
  cros <- CrossTable(obj_$Semestre,obj_[,i],prop.r=TRUE,prop.t=FALSE,prop.chisq=FALSE,format="SAS")
  questoes_abs[i] <- list(cros$t)
  questoes_pctg[i] <- list(round(cros$prop.row*100, digits=2))

  barplot(questoes_pctg[[i]],
    , main={paste(strtrim(labels[1,i],50),"...")}
    , col=c("lightblue","lightgreen","lightyellow")
    , beside=T
    , legend = paste(rownames(questoes_abs[[i]]), "o. Semestre")
    , ylim = c(0,100)
    # ,names.arg=labels_resp
  )
  }
  dev.off()
}




create_Quantitativas_Plots <- function(obj_,set) {
  filename <- paste("plots/Respostas Quantitativas - ",set,".png")
    png(filename, width = 640, height = 480)
    par(mfrow=c(2,1))

  for(i in 8:9) {
    # filename <- paste("Respostas Quantitativas - ",set,colnames(obj_)[i],".png")
    # jpeg(filename)

    x <- as.numeric(obj_[ ,i])
    h <- hist(x, plot = FALSE, breaks = 10)

    plot(h,
      main={paste(strtrim(labels[1,i+3],30),"...")},
      xlab=paste("Respostas Quantitativas - ",set),
      border="black",
      col=col_colors[i+3],
      xlim=c(0,1),
      ylim=c(0,nrow(obj_))
    )
    xfit<-seq(min(x, na.rm = T),max(x, na.rm = T),length=length(x))
    yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
    yfit <- yfit*diff(h$mids[1:2])*length(x)
    lines(xfit, yfit, col="blue", lwd=2)
  }
  dev.off()
}

########### GET FUZZY RESULTS
res_total <- get_fuzzy_res(total)
res_total_masculino <- get_fuzzy_res(total_masculino)
res_total_feminino <- get_fuzzy_res(total_feminino)
res_total_oitavo <- get_fuzzy_res(total_oitavo)
res_total_setimo <- get_fuzzy_res(total_setimo)
res_total_primeiro <- get_fuzzy_res(total_primeiro)

########### GET create_Quantitativas_Plots
create_Categoricas_Plots(total,"Geral")
create_Categoricas_Plots(total_masculino,"Masculino")
create_Categoricas_Plots(total_feminino,"Feminino")

########### GET create_Quantitativas_Plots
create_Quantitativas_Plots(res_total,"Geral")
create_Quantitativas_Plots(res_total_masculino,"Masculino")
create_Quantitativas_Plots(res_total_feminino,"Feminino")
create_Quantitativas_Plots(res_total_oitavo,"8o. Semestre")
create_Quantitativas_Plots(res_total_setimo,"7o. Semestre")
create_Quantitativas_Plots(res_total_primeiro,"1o. Semestre")

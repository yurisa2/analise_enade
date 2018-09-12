PATH <- "C:/Bitnami/wampstack-7.1.20-1/apache2/htdocs/analise_enade"
setwd(PATH)

library(FuzzyR)


########### AQUISICAO DOS DADOS
r1a <- read.csv("1a.csv", header=T, stringsAsFactors=F)
r7b <- read.csv("7b.csv", header=T, stringsAsFactors=F)
r8a <- read.csv("8a.csv", header=T, stringsAsFactors=F)
r8b <- read.csv("8b.csv", header=T, stringsAsFactors=F)
r8c <- read.csv("8c.csv", header=T, stringsAsFactors=F)
r8g <- read.csv("8g.csv", header=T, stringsAsFactors=F)


########### SEPARACAO DE SETS
total <- rbind(r1a,r7b,r8a,r8b,r8c,r8g)
total_masculino <- total[total$questao1=="1",]
total_feminino <- total[total$questao1=="2",]
total_oitavo <- rbind(r8a,r8b,r8c,r8g)
total_setimo <- r7b
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

########### LABELS DAS PERGUNTAS
labels <- read.csv("labels.csv",stringsAsFactors=F)

########### VETOR DAS CORES PARA AS BARRAS
col_colors <- c("green","green","green","red","red","purple","blue","purple",
"red","purple","purple","purple")



########### FUNCTIONS
get_fuzzy_res <- function(obj_) {
  entrada_fuzzy <- as.matrix(obj_[,cols_enade])
  entrada_fuzzy <- (entrada_fuzzy - 1) * 0.25

  TestarAluno_FIS <- readfis("TestarAluno.fis")
  AjudarInstituicao_FIS <- readfis("AjudarInstituicao.fis")

  results <- cbind(entrada_fuzzy,evalfis(entrada_fuzzy,TestarAluno_FIS))
  colnames(results)[ncol(results)] <- "TestarAluno"
  results <- cbind(results,evalfis(entrada_fuzzy,AjudarInstituicao_FIS))
  colnames(results)[ncol(results)] <- "AjudarInstituicao"

  results <- data.frame(results)
  return(results)
}


create_Categoricas_Plots <- function(obj_,set) {
  for(i in cols_enade) {
    filename <- paste(set,"-Respostas Categoricas",colnames(obj_)[i],".jpg")
    jpeg(filename)
    x <- as.numeric(obj_[ ,i])
    h <- hist(x, plot = FALSE)
    h$counts <- h$counts/sum(h$counts)

    plot(h,
      main={paste(strtrim(labels[1,i],50),"...")},
      xlab="Respostas Categoricas",
      ylab="Porcentagem",
      border="black",
      col=col_colors[i],
      xlim=c(1,5),
      ylim=c(0,1)
    )
    dev.off()
  }
}

create_Quantitativas_Plots <- function(obj_,set) {
  for(i in 1:ncol(obj_)) {
    filename <- paste(set,"-Respostas Quantitativas",colnames(obj_)[i],".jpg")
    jpeg(filename)

    x <- as.numeric(obj_[ ,i])
    h <- hist(x, plot = FALSE)

    plot(h,
      main={paste(strtrim(labels[1,i],50),"...")},
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
    dev.off()
  }
}

########### GET FUZZY RESULTS
res_total <- get_fuzzy_res(total)
res_total_masculino <- get_fuzzy_res(total_masculino)
res_total_feminino <- get_fuzzy_res(total_feminino)
res_total_oitavo <- get_fuzzy_res(total_oitavo)
res_total_setimo <- get_fuzzy_res(total_setimo)
res_total_primeiro <- get_fuzzy_res(total_primeiro)

########### GET create_Quantitativas_Plots
create_Quantitativas_Plots(total,"Geral")
create_Quantitativas_Plots(total_masculino,"Masculino")
create_Quantitativas_Plots(total_feminino,"Feminino")
create_Quantitativas_Plots(total_oitavo,"8o. Semestre")
create_Quantitativas_Plots(total_setimo,"7o. Semestre")
create_Quantitativas_Plots(total_primeiro,"1o. Semestre")

########### GET create_Quantitativas_Plots
create_Categoricas_Plots(total,"Geral")
create_Categoricas_Plots(total_masculino,"Masculino")
create_Categoricas_Plots(total_feminino,"Feminino")
create_Categoricas_Plots(total_oitavo,"8o. Semestre")
create_Categoricas_Plots(total_setimo,"7o. Semestre")
create_Categoricas_Plots(total_primeiro,"1o. Semestre")

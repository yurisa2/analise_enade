PATH <- "C:/Bitnami/wampstack-7.1.20-1/apache2/htdocs/analise_enade"
setwd(PATH)

r7 <- read.csv("resultsV2/7.csv")
r7b <- read.csv("resultsV2/7b.csv")
r8b <- read.csv("resultsV2/8b.csv")
r8g <- read.csv("resultsV2/8g.csv")


total <- rbind(r7,r7b,r8b,r8g)

summary(total)

hist(total$X9..1)

total_f <- NULL

for(i in 1:ncol(total)) total[,i] <- as.numeric(total[,i])



for(i in 2:ncol(total)) {
  g <- total[,i]
  m <- mean(g)

  std<-sqrt(var(g))

  filename <- paste("plots/Segundo Enade ",i-1,".png")
  png(filename, width = 640, height = 896)


  hist(g,     main = paste("Histograma" , colnames(total)[i]), breaks = c(0,1,2,3,4,5), ylim = c(0,120))

  curve(dnorm(x, mean=m, sd=std),
        col="darkblue", lwd=2, add=TRUE, yaxt="n")

  dev.off()
}

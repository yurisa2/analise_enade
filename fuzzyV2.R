PATH <- "C:/Bitnami/wampstack-7.1.20-1/apache2/htdocs/analise_enade"
setwd(PATH)

r7 <- read.csv("resultsV2/7.csv")
r7b <- read.csv("resultsV2/7b.csv")
r8b <- read.csv("resultsV2/8b.csv")
r8g <- read.csv("resultsV2/8g.csv")



labels <- read.csv("db/labels_v2.csv")

total <- rbind(r7,r7b,r8b,r8g)

total[,1] <- NULL

colnames(total) <- labels$label


for(i in 1:ncol(total)) total[,i] <- as.numeric(total[,i])

total_factor <- total
for(i in 1:ncol(total_factor)) total_factor[,i] <- factor(total_factor[,i], exclude = NA)

teste <- summary(total_factor)

write.csv(teste,file="teste.csv",row.names=TRUE,col.names=TRUE)

for(i in 1:ncol(total)) {

  filename <- paste("plots/Histogram Segundo Enade ",i,".png")
  png(filename, width = 640, height = 896)
  hist(total[,i],     main = paste("Histograma: ",colnames(total)[i]), breaks = 0:max(total[,i], na.rm=T), ylim = c(0,120))
  dev.off()
}

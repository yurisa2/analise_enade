PATH <- "C:/Bitnami/wampstack-7.1.20-1/apache2/htdocs/analise_enade"
setwd(PATH)

r7 <- read.csv("resultsV2/7.csv")
r7b <- read.csv("resultsV2/7b.csv")
r8b <- read.csv("resultsV2/8b.csv")
r8g <- read.csv("resultsV2/8g.csv")


total <- rbind(r7,r7b,r8b,r8g)

summary(total)

hist(total$X9..1)

for(i in 1:ncol(total)) total[,i] <- as.numeric(total[,i])


for(i in 2:ncol(total)) hist(total[,i],     main = paste("Histogram of" , colnames(total)[i]), breaks = c(1,2,3,4,5,6))

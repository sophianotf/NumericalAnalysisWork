setwd("D:\\UNI\\3 курс\\Чисельні методи\\РР1\\Yulia")
getwd()

pharma <- read.csv("farm_sales_f.csv", header = TRUE)

to_clust <- pharma[,c(9,10)]
m <- dist(scale(to_clust))
hc <- hclust(m)
plot(hc, crex=0.7, hang=-1)

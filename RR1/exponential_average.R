#rm(list=ls())

# бібліотека для прогнозування
library(png)
library(grid)
library(zoo)
library(expsmooth)
require(graphics)

pharma <- read.csv("C:/Users/Sophia/Documents/MO_Rozraha1/Rozrahunkova1/farm_sales_f.csv", header = TRUE)

pdf(file="C:/Users/Sophia/Documents/MO_Rozraha1/Rozrahunkova1/plots/Exponential/exponentialsmoothing.pdf",
    width = 4,
    height = 4)
#M01AB
M01AB_desk <- ts(pharma[,2], start = c(2016,1), frequency = 300)
M01AB_desk.smooth<- HoltWinters(M01AB_desk, beta=0.1, gamma=0.1)
plot(M01AB_desk.smooth, xlab = "Years", ylab = "Sales volumes M01AB")
#lines(M01AB_desk.smooth, lty=2, col="blue")

#M01AE
M01AE_desk <- ts(pharma[,3], start = c(2016,1), frequency = 300)
M01AE_desk.smooth<- HoltWinters(M01AE_desk, beta=0.1, gamma=0.1)
plot(M01AE_desk.smooth, xlab = "Years", ylab = "Sales volumes M01AE")

#N02BA
N02BA_desk <- ts(pharma[,4], start = c(2016,1), frequency = 300)
N02BA_desk.smooth<- HoltWinters(N02BA_desk, beta=0.1, gamma=0.1)
plot(N02BA_desk.smooth, xlab = "Years", ylab = "Sales volumes N02BA")

#N02BE
N02BE_desk <- ts(pharma[,5], start = c(2016,1), frequency = 300)
N02BE_desk.smooth<- HoltWinters(N02BE_desk, beta=0.1, gamma=0.1)
plot(N02BE_desk.smooth, xlab = "Years", ylab = "Sales volumes N02BE")

#N05B
N05B_desk <- ts(pharma[,6], start = c(2016,1), frequency = 300)
N05B_desk.smooth<- HoltWinters(N05B_desk, beta=0.1, gamma=0.1)
plot(N05B_desk.smooth, xlab = "Years", ylab = "Sales volumes N05B")

#N05C
N05C_desk <- ts(pharma[,7], start = c(2016,1), frequency = 300)
N05C_desk.smooth<- HoltWinters(N05C_desk, beta=0.1, gamma=0.1)
plot(N05C_desk.smooth, xlab = "Years", ylab = "Sales volumes N05C")

#R03
R03_desk <- ts(pharma[,8], start = c(2016,1), frequency = 300)
R03_desk.smooth<- HoltWinters(R03_desk, beta=0.1, gamma=0.1)
plot(R03_desk.smooth, xlab = "Years", ylab = "Sales volumes R03")

#R06
R06_desk <- ts(pharma[,9], start = c(2016,1), frequency = 300)
R06_desk.smooth<- HoltWinters(R06_desk, beta=0.1, gamma=0.1)
plot(R06_desk.smooth, xlab = "Years", ylab = "Sales volumes R06")

dev.off()

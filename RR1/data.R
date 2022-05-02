#rm(list=ls())

library(psych)
library(eurostat)
library(nasapower)
library(soildb)
library(ggplot2)

# читання csv файла
pharma <- read.csv("C:/Users/Sophia/Documents/MO_Rozraha1/Rozrahunkova1/farm_sales_f.csv", header = TRUE)

# відображення даних в консолі, для валідації даних
head(pharma)

# графічне зображення даних
pdf(file="C:/Users/Sophia/Documents/MO_Rozraha1/Rozrahunkova1/plots/graphs.pdf")
M01AB_plot = ggplot(pharma, aes(x=pharma[,1], y=pharma[,2])) +
  geom_line(stat="identity") + theme_light()
M01AB_plot
M01AE_plot = ggplot(pharma, aes(x=pharma[,1], y=pharma[,3])) +
  geom_line(stat="identity") + theme_light()
M01AE_plot
N02BA_plot = ggplot(pharma, aes(x=pharma[,1], y=pharma[,4])) +
  geom_line(stat="identity") + theme_light()
N02BA_plot
N02BE_plot = ggplot(pharma, aes(x=pharma[,1], y=pharma[,5])) +
  geom_line(stat="identity") + theme_light()
N02BE_plot
N05B_plot = ggplot(pharma, aes(x=pharma[,1], y=pharma[,6])) +
  geom_line(stat="identity") + theme_light()
N05B_plot
N05C_plot = ggplot(pharma, aes(x=pharma[,1], y=pharma[,7])) +
  geom_line(stat="identity") + theme_light()
N05C_plot
R03_plot = ggplot(pharma, aes(x=pharma[,1], y=pharma[,8])) +
  geom_line(stat="identity") + theme_light()
R03_plot
R06_plot = ggplot(pharma, aes(x=pharma[,1], y=pharma[,9])) +
  geom_line(stat="identity") + theme_light()
R06_plot

# графічне зображення даних в полярних координатах
M01AB_plot + coord_polar()
M01AE_plot + coord_polar()
N02BA_plot + coord_polar()
N02BE_plot + coord_polar()
N05B_plot + coord_polar()
N05C_plot + coord_polar()
R03_plot + coord_polar()
R06_plot + coord_polar()
dev.off()

# описова характеристика (окрім моди)
describe(pharma)

# мода
getmode <- function(v)
{
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
modaM01AB <- getmode(pharma$M01AB)
print(modaM01AB)
modaM01AE <- getmode(pharma$M01AE)
print(modaM01AE)
modaN02BA <- getmode(pharma$N02BA)
print(modaN02BA)
modaN02BE <- getmode(pharma$N02BE)
print(modaN02BE)
modaN05B <- getmode(pharma$N05B)
print(modaN05B)
modaR03 <- getmode(pharma$R03)
print(modaR03)
modaR06 <- getmode(pharma$R06)
print(modaR06)

# гістограми
pdf(file="C:/Users/Sophia/Documents/MO_Rozraha1/Rozrahunkova1/plots/histohrama.pdf")
hist(M01AB_desk)
hist(M01AE_desk)
hist(N02BA_desk)
hist(N02BE_desk)
hist(N05B_desk)
hist(N05C_desk)
hist(R03_desk)
hist(R06_desk)
dev.off()

# кумуляти
pdf(file="C:/Users/Sophia/Documents/MO_Rozraha1/Rozrahunkova1/plots/cumulative.pdf")
# кумулята для M01AB
plot(ecdf(pharma[,2]))
ggplot(NULL, aes(x=pharma[,2]))+
  geom_step(stat="ecdf")+
  labs(x= "Intervals",y = "Probability")+
  ggtitle("Cumulative by integral percentage, M01AB")
# кумулята для M01AE
plot(ecdf(pharma[,3]))
ggplot(NULL, aes(x=pharma[,3]))+
  geom_step(stat="ecdf")+
  labs(x= "Intervals",y = "Probability")+
  ggtitle("Cumulative by integral percentage, M01AE")
# кумулята для N02BA
plot(ecdf(pharma[,4]))
ggplot(NULL, aes(x=pharma[,4]))+
  geom_step(stat="ecdf")+
  labs(x= "Intervals",y = "Probability")+
  ggtitle("Cumulative by integral percentage, N02BA")
# кумулята для N02BE
plot(ecdf(pharma[,5]))
ggplot(NULL, aes(x=pharma[,5]))+
  geom_step(stat="ecdf")+
  labs(x= "Intervals",y = "Probability")+
  ggtitle("Cumulative by integral percentage, N02BE")
# кумулята для N05B
plot(ecdf(pharma[,6]))
ggplot(NULL, aes(x=pharma[,6]))+
  geom_step(stat="ecdf")+
  labs(x= "Intervals",y = "Probability")+
  ggtitle("Cumulative by integral percentage, N05B")
# кумулята для N05C
plot(ecdf(pharma[,7]))
ggplot(NULL, aes(x=pharma[,7]))+
  geom_step(stat="ecdf")+
  labs(x= "Intervals",y = "Probability")+
  ggtitle("Cumulative by integral percentage, N05C")
# кумулята для R03
plot(ecdf(pharma[,8]))
ggplot(NULL, aes(x=pharma[,8]))+
  geom_step(stat="ecdf")+
  labs(x= "Intervals",y = "Probability")+
  ggtitle("Cumulative by integral percentage, R03")
# кумулята для R06
plot(ecdf(pharma[,9]))
ggplot(NULL, aes(x=pharma[,9]))+
  geom_step(stat="ecdf")+
  labs(x= "Intervals",y = "Probability")+
  ggtitle("Cumulative by integral percentage, R06")
dev.off()


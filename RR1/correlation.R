#1) тут треба вказати шл€х до папочки, де Ї файл txt
setwd("D:\\UNI\\3 курс\\„исельн≥ методи\\––1")
getwd()

#2) тут просто дивимось чи запускаЇтьс€
file.show("farm_sales.txt")

#3) робимо датафрейм
sales <- read.table("farm_sales.txt", head=TRUE, as.is=TRUE, sep=",")
str(sales)

#5) часовий р€д з антиг≥стам≥нних
r06_desk <- ts(sales$R06, frequency=1)
plot_r06 <- plot(r06_desk,xlab = "Datum index", ylab = "Sales volumes R06", type="p", col="blue")
str(r06)

#6) ≥нтервали
r06_desk_16_1 <- ts(sales$R06[1:182], frequency=1)
r06_desk_16_2 <- ts(sales$R06[183:365], frequency=1)
r06_desk_17_1 <- ts(sales$R06[366:547], frequency=1)
r06_desk_17_2 <- ts(sales$R06[548:731], frequency=1)
r06_desk_18_1 <- ts(sales$R06[732:914], frequency=1)
r06_desk_18_2 <- ts(sales$R06[915:1096], frequency=1)

plot_r06_16_1 <- plot(r06_desk_16_1,xlab = "Datum index", ylab = "Sales volumes R06", type="p", col="blue")
plot_r06_16_2 <- plot(r06_desk_16_2,xlab = "Datum index", ylab = "Sales volumes R06", type="p", col="blue")
plot_r06_17_1 <- plot(r06_desk_17_1,xlab = "Datum index", ylab = "Sales volumes R06", type="p", col="blue")
plot_r06_17_2 <- plot(r06_desk_17_2,xlab = "Datum index", ylab = "Sales volumes R06", type="p", col="blue")
plot_r06_18_1 <- plot(r06_desk_18_1,xlab = "Datum index", ylab = "Sales volumes R06", type="p", col="blue")
plot_r06_18_2 <- plot(r06_desk_18_2,xlab = "Datum index", ylab = "Sales volumes R06", type="p", col="blue")

cor(sales$datum.index[1:182] ,sales$R06[1:182])
cor(sales$datum.index[183:365] ,sales$R06[183:365])
cor(sales$datum.index[366:547] ,sales$R06[366:547])
cor(sales$datum.index[548:731] ,sales$R06[548:731])
cor(sales$datum.index[732:914] ,sales$R06[732:914])
cor(sales$datum.index[915:1096] ,sales$R06[915:1096])

#тут детальн≥ше можна подивитись дан≥ про корел€ц≥ю
cor.test(sales$datum.index[1:182] ,sales$R06[1:182])
cor.test(sales$datum.index[183:365] ,sales$R06[183:365])
cor.test(sales$datum.index[366:547] ,sales$R06[366:547])
cor.test(sales$datum.index[548:731] ,sales$R06[548:731])
cor.test(sales$datum.index[732:914] ,sales$R06[732:914])
cor.test(sales$datum.index[915:1096] ,sales$R06[915:1096])

#6)корел€ц≥йна матриц€
cor(sales) 

#7) коеф≥ц≥Їнти ≥ граф≥к автокорел€ц≥њ
(acf(r06_desk, main="", lag.max=8))



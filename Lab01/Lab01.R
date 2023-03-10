a <- 20/log(2.78)
a
help(abs)
a <- 8:75
a
b <- length(a)
b
c <- a^2
c
d <- sum(c)
e <- d/b
e
apropos("plot", mode = "function")
getwd()
setwd("E:/Users/denn/Desktop/apu")
getwd()
a <- "tablet"
save(a, file = "workdir")
load("workdir")
remove(a)
a
load("workdir")
a
install.packages("gridExtra")
help(package="gridExtra")
library(gridExtra)
library(grid)
a <- head(volcano[,1:10])
grid.table(a)
seq(1000,200,-8)
a<-seq(50,30)
b<-seq(4,50)
c<-c(b,a)
c
nazwa <- c('iPad A', 'iPad B', 'iPad C', 'iPad D', 'iPad E', 'iPad F', 'iPad G', 'iPad H', 'iPad I', 'iPad J')
modem <-c('LTE', 'LTE', 'WiFi', 'LTE', 'WiFi', 'LTE', 'LTE', 'LTE', 'WiFi', 'LTE')
wyswietlacz <- c('Liquid Retina', 'LCD', 'LCD', 'LCD', 'Retina', 'LCD', 'LCD', 'Retina', 'LCD','Liquid Retina')
pamiec_RAM <- c(4, 2, 2, 4, 2, 2, 8, 4, 2, 4)
pamiec_wbudowana <- c(64, 32, 32, 64, 32, 32, 48, 64, 32, 64)
cena <- c(5000,3000,3300,5000,3400,3400,4000,5100,3000, 4999)
liczba_opinii <- c(111,11,51,43,150,113,10,300,140,21)
ramka <- data.frame(modem, wyswietlacz, pamiec_RAM, pamiec_wbudowana,cena, liczba_opinii)
ramka
srednia <- mean(ramka$cena)
srednia
nowy_ipad <- data.frame(modem = "LTE", wyswietlacz = "Retina", pamiec_RAM = 8, pamiec_wbudowana = 64, cena = 5500, liczba_opinii = 83)
ramka <- rbind(ramka, nowy_ipad)
srednia <- mean(ramka$cena)
srednia
ocena_klientow <- c (4.5, 3.5, 3, 3, 2.5, 3, 4, 4, 4.5, 4.5, 5)
ramka <- cbind(ramka, ocena_klientow)
ramka
mean(ramka[ocena_klientow==2.5, 'cena'])
mean(ramka[ocena_klientow==3.0, 'cena'])
mean(ramka[ocena_klientow==3.5, 'cena'])
mean(ramka[ocena_klientow==4.0, 'cena'])
mean(ramka[ocena_klientow==4.5, 'cena'])
nowy_ipad <- data.frame(modem = "LTE", wyswietlacz = "Retina", pamiec_RAM = 4, pamiec_wbudowana = 64, cena = 5700, liczba_opinii = 13, ocena_klientow = 3.0)
ramka <- rbind(ramka, nowy_ipad)
nowy_ipad <- data.frame(modem = "LTE", wyswietlacz = "Retina", pamiec_RAM = 4, pamiec_wbudowana = 128, cena = 5000, liczba_opinii = 63, ocena_klientow = 4.0)
ramka <- rbind(ramka, nowy_ipad)
nowy_ipad <- data.frame(modem = "WiFi", wyswietlacz = "Retina", pamiec_RAM = 2, pamiec_wbudowana = 18, cena = 3000, liczba_opinii = 123, ocena_klientow = 3.5)
ramka <- rbind(ramka, nowy_ipad)
nowy_ipad <- data.frame(modem = "WiFi", wyswietlacz = "LCD", pamiec_RAM = 4, pamiec_wbudowana = 32, cena = 2500, liczba_opinii = 223, ocena_klientow = 2.5)
ramka <- rbind(ramka, nowy_ipad)
count <- table(ramka$ocena_klientow)
barplot(count, main = "Liczebność reprezentantów każdej z ocen", ylim = c(0,4), xlab="Ocena", ylab = "Liczba ocen")
percentage <- table(ramka$ocena_klientow)/length(ramka$ocena_klientow)
pie(percentage)
View(percentage)
install.packages("plotrix")
library(plotrix)
fan.plot(percentage, labels = names(percentage), main = "Rozkład wachlarzowy")
paste(nazwa, "ma ocenę klientów", ocena_klientow, "bo ma liczbę opinii", liczba_opinii, sep = " ")
write.csv(ramka, "E:/Users/denn/Desktop//ramka.csv", row.names=TRUE)
read.csv("E:/Users/denn/Desktop//ramka.csv")

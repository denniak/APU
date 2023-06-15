# Instalacja pakietu "C50"
install.packages("C50") 
# £adowanie pakietu "C50"
library("C50")

# Wczytanie pliku CSV do zmiennej "data"
data <- read.csv("E:/Users/denn/Desktop/iPady.csv")  
# Wyœwietlenie zawartoœci zmiennej "data"
data 

# Konwersja kolumny "ocena_klienta" na zmienn¹ kategoryczn¹
data$ocena_klienta <- factor(data$ocena_klienta) 
# Tworzenie modelu C5.0 na podstawie danych
model <- C5.0(x=data[,2:7], y=data$ocena_klienta) 

# Wyœwietlenie informacji o modelu
model 
# Wyœwietlenie podsumowania modelu
summary(model)  
# Wygenerowanie wykresu modelu
plot(model)  
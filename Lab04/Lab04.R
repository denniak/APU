# Instalacja pakietu "C50"
install.packages("C50") 
# Ładowanie pakietu "C50"
library("C50")

# Wczytanie pliku CSV do zmiennej "data"
data <- read.csv("E:/Users/denn/Desktop/iPady.csv")  
# Wyświetlenie zawartości zmiennej "data"
data 

# Konwersja kolumny "ocena_klienta" na zmienną kategoryczną
data$ocena_klienta <- factor(data$ocena_klienta) 
# Tworzenie modelu C5.0 na podstawie danych
model <- C5.0(x=data[,2:7], y=data$ocena_klienta) 

# Wyświetlenie informacji o modelu
model 
# Wyświetlenie podsumowania modelu
summary(model)  
# Wygenerowanie wykresu modelu
plot(model)  
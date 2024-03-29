# Wczytanie danych z pliku CSV
data <- read.csv("E:/Users/denn/Desktop/iPady.csv") 
data

# Funkcja do normalizacji danych
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

# Wyb�r odpowiednich kolumn dla analizy
iPady_simplified <- data[, c('ram', 'pamiec', 'aparat', 'cena', 'procesor')]
iPady_normalized <- as.data.frame(lapply(iPady_simplified, normalize))  # Normalizacja danych

# Podzia� danych na zbi�r treningowy i testowy
traindata <- iPady_normalized
testdata <- iPady_normalized[1:3, ]

# Tworzenie modelu sieci neuronowej
iPady <- neuralnet(cena ~ ram + pamiec + aparat + procesor, data = traindata,
                   hidden = c(3, 2), threshold = 0.01)

# Wy�wietlenie informacji o modelu
print(iPady)
plot(iPady)

# Obliczenie wynik�w dla danych testowych
iPady.results <- compute(iPady, testdata)
print(iPady.results$net.result)

# Por�wnanie przewidywanych warto�ci z warto�ciami rzeczywistymi
comparison <- data.frame(actual = testdata[, 'cena'], prediction = iPady.results$net.result)
comparison
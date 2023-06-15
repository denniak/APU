# Wczytanie danych z pliku CSV
data <- read.csv("E:/Users/denn/Desktop/iPady.csv") 
data

# Funkcja do normalizacji danych
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

# Wybór odpowiednich kolumn dla analizy
iPady_simplified <- data[, c('ram', 'pamiec', 'aparat', 'cena', 'procesor')]
iPady_normalized <- as.data.frame(lapply(iPady_simplified, normalize))  # Normalizacja danych

# Podzia³ danych na zbiór treningowy i testowy
traindata <- iPady_normalized
testdata <- iPady_normalized[1:3, ]

# Tworzenie modelu sieci neuronowej
iPady <- neuralnet(cena ~ ram + pamiec + aparat + procesor, data = traindata,
                   hidden = c(3, 2), threshold = 0.01)

# Wyœwietlenie informacji o modelu
print(iPady)
plot(iPady)

# Obliczenie wyników dla danych testowych
iPady.results <- compute(iPady, testdata)
print(iPady.results$net.result)

# Porównanie przewidywanych wartoœci z wartoœciami rzeczywistymi
comparison <- data.frame(actual = testdata[, 'cena'], prediction = iPady.results$net.result)
comparison
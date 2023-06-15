# Instalacja i za³adowanie pakietu neuralnet
install.packages("neuralnet")
library(neuralnet)

# Tworzenie danych treningowych
traininginput <- as.data.frame(runif(100, min = 1, max = 100))
trainingoutput <- traininginput^3 - (2 * traininginput)

# Definicja funkcji normalizuj¹cej
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

# £¹czenie danych treningowych w jedn¹ ramkê danych
data <- cbind(traininginput, trainingoutput)
colnames(data) <- c("input", "output")

# Normalizacja danych
normalized <- as.data.frame(lapply(data, normalize))

# Podzia³ na zbiór treningowy i testowy
traindata <- normalized[1:80, ]
testdata <- normalized[81:100, ]

# Tworzenie sieci neuronowej
net.sqrt <- neuralnet(output ~ input, traindata, hidden = 10, threshold = 0.01, 
                      linear.output = TRUE)

# Wyœwietlanie informacji o sieci neuronowej
print(net.sqrt)

# Wykres sieci neuronowej
plot(net.sqrt, rep = "best")

# Obliczanie wyników dla zbioru testowego
net.results <- compute(net.sqrt, testdata)

# Wyœwietlanie wyników sieci neuronowej
print(net.results$net.result)

# Porównanie wyników rzeczywistych z przewidywanymi
comparison <- data.frame(actual = testdata[, 2], prediction = net.results$net.result)
comparison

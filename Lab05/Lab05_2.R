# Instalacja pakietów
install.packages("mlr")
install.packages("e1071")
install.packages("C50")
install.packages("rFerns")
install.packages("randomForest")
install.packages("rpart")

# £adowanie bibliotek
library(mlr)
library(e1071)
library(C50)
library(rFerns)
library(randomForest)
library(rpart)

# Wczytanie pliku CSV do zmiennej "data"
data <- read.csv("E:/Users/denn/Desktop/iPady.csv")  
# Wyœwietlenie zawartoœci zmiennej "data"
data 

# Ograniczenie kolumn danych
data <- data[, 1:8]

# Tworzenie zmiennych kategorycznych
data$model = factor(data$model)
data$ocena_klienta = factor(data$ocena_klienta)


# Tworzenie opisu procedury resamplingu 
rdesc = makeResampleDesc(method = "CV", iters = 10)


# Tworzenie zadania klasyfikacji na podstawie wczytanych danych
task = makeClassifTask(data = data, target = "ocena_klienta")


# Tworzenie zbioru modeli (learners) do przetestowania w benchmarkingu
lrns <- makeLearners(c("naiveBayes", "rpart", "rFerns", "C50", "randomForest"), type = "classif")


# Przeprowadzenie procedury benchmarkingu dla ró¿nych modeli uczenia maszynowego
bmr <- benchmark(learners = lrns, tasks = task, rdesc, models = TRUE, measures = list(acc, ber))

# Wyœwietlanie wyników
p = getBMRPredictions(bmr)
p
plotBMRSummary(bmr)

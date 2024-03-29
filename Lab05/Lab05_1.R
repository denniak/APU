# Instalacja pakietu mlr
install.packages("mlr")
library(mlr)

# Załadowanie pakietu MASS
install.packages("MASS")
library(MASS)

# Załadowanie pakietu rpart.plot (wyświetlanie modelu)
install.packages("rpart.plot")
library(rpart.plot)


# Załadowanie zbioru danych "cats"
data(cats)

# Utworzenie zadania klasyfikacji na podstawie zmiennej "Sex"
cats_task <- makeClassifTask(data = cats, target = "Sex")

# Podział danych na zbiór treningowy i testowy 
train_indices <- sample(1:nrow(cats), size = round(0.7 * nrow(cats)), replace = FALSE)
traindata <- cats[train_indices, ]
testdata <- cats[-train_indices, ]

# Utworzenie modelu uczącego i trening modelu
learner <- makeLearner("classif.rpart")
model <- train(learner, cats_task)

# Ocena jakości modelu
predictions <- predict(model, newdata = testdata)

# Podsumowanie modelu
summary(model)

# Wyświetlenie modelu
rpart.plot(getLearnerModel(model))

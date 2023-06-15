# Instalacja pakietu mlr
install.packages("mlr")
library(mlr)

# Za³adowanie pakietu MASS
install.packages("MASS")
library(MASS)

# Za³adowanie pakietu rpart.plot (wyœwietlanie modelu)
install.packages("rpart.plot")
library(rpart.plot)


# Za³adowanie zbioru danych "cats"
data(cats)

# Utworzenie zadania klasyfikacji na podstawie zmiennej "Sex"
cats_task <- makeClassifTask(data = cats, target = "Sex")

# Podzia³ danych na zbiór treningowy i testowy 
train_indices <- sample(1:nrow(cats), size = round(0.7 * nrow(cats)), replace = FALSE)
traindata <- cats[train_indices, ]
testdata <- cats[-train_indices, ]

# Utworzenie modelu ucz¹cego i trening modelu
learner <- makeLearner("classif.rpart")
model <- train(learner, cats_task)

# Ocena jakoœci modelu
predictions <- predict(model, newdata = testdata)

# Podsumowanie modelu
summary(model)

# Wyœwietlenie modelu
rpart.plot(getLearnerModel(model))

# Instalacja pakietu mlr
install.packages("mlr")
library(mlr)

# Za�adowanie pakietu MASS
install.packages("MASS")
library(MASS)

# Za�adowanie pakietu rpart.plot (wy�wietlanie modelu)
install.packages("rpart.plot")
library(rpart.plot)


# Za�adowanie zbioru danych "cats"
data(cats)

# Utworzenie zadania klasyfikacji na podstawie zmiennej "Sex"
cats_task <- makeClassifTask(data = cats, target = "Sex")

# Podzia� danych na zbi�r treningowy i testowy 
train_indices <- sample(1:nrow(cats), size = round(0.7 * nrow(cats)), replace = FALSE)
traindata <- cats[train_indices, ]
testdata <- cats[-train_indices, ]

# Utworzenie modelu ucz�cego i trening modelu
learner <- makeLearner("classif.rpart")
model <- train(learner, cats_task)

# Ocena jako�ci modelu
predictions <- predict(model, newdata = testdata)

# Podsumowanie modelu
summary(model)

# Wy�wietlenie modelu
rpart.plot(getLearnerModel(model))
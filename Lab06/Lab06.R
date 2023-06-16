# Instalacja TensorFlow
install.packages("tensorflow")
library(tensorflow)
install_tensorflow()

# Instalacja Keras
install.packages("keras")
library(keras)
install_keras()

# Import biblioteki Keras i rozpakowanie danych
mnist <- dataset_mnist()
x_train <- mnist$train$x
x_test <- mnist$test$x
y_train <- mnist$train$y
y_test <- mnist$test$y


# Ustalenie zakresu wartoœci matrycy obrazu do [0,1]
x_train <- x_train / 255
x_test <- x_test / 255

# Konwersja liczb ca³kowitych na kategorie
y_train <- to_categorical(y_train, num_classes = 10)
y_test <- to_categorical(y_test, num_classes = 10)

# Trening modelu (ukryta warstwa, na koñcu warstwa wyjœciowa)
model <- keras_model_sequential() %>%
  layer_flatten(input_shape = c(28, 28)) %>%
  layer_dense(units = 128, activation = "relu") %>%
  layer_dense(units = 10, activation = "softmax")

# Wyœwietlenie architektury modelu
summary(model)

# Kompilacja
model %>% compile(
  loss = "categorical_crossentropy",
  optimizer = optimizer_adam(),
  metrics = c("accuracy")
)

# Trenowanie modelu funkcj¹ fit()
history <- model %>%
  fit( x_train, y_train, epochs = 50, batch_size = 128, validation_split = 0.15)

# Ocena wydajnoœci na zbiorze
model %>% evaluate(x_test, y_test)


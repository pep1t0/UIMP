# -----------------------------------------
# Librerías necesarias
# -----------------------------------------
library(caret)        # Para particiones de datos y entrenamiento
library(ggplot2)      # Para visualización
library(dplyr)        # Para manipulación de datos
library(tidyr)        # Para manipulación de datos
library(nnet)         # Para el Perceptrón Multicapa (MLP)

# -----------------------------------------
# Función para calcular F1-Score
# -----------------------------------------
calculate_f1 <- function(confusion_matrix) {
  precision <- diag(confusion_matrix) / rowSums(confusion_matrix)
  recall <- diag(confusion_matrix) / colSums(confusion_matrix)
  f1 <- 2 * ((precision * recall) / (precision + recall))
  mean(f1, na.rm = TRUE)  # Promedio del F1-score para todas las clases
}

# -----------------------------------------
# Cargar datasets discretizados
# -----------------------------------------
datasets <- list(
  EqualWidth = glass_discretized_equal_width,
  EqualFrequency = glass_discretized_equal_frequency,
  ChiMerge = disc$Disc.data
)

# -----------------------------------------
# Configuración inicial
# -----------------------------------------
set.seed(123)  # Para reproducibilidad
results <- data.frame(Method = character(), Accuracy = numeric(), F1 = numeric())

# -----------------------------------------
# Entrenamiento y evaluación
# -----------------------------------------
for (method in names(datasets)) {
  dataset <- datasets[[method]]
  
  # Asegurarse de que la columna 'Type' sea un factor
  dataset$Type <- as.factor(dataset$Type)
  
  # Asegurarse de que todas las variables predictoras sean factores
  dataset <- dataset %>% mutate(across(-Type, as.factor))
  
  # Dividir en entrenamiento y prueba (70/30)
  trainIndex <- createDataPartition(dataset$Type, p = 0.7, list = FALSE)
  trainData <- dataset[trainIndex, ]
  testData <- dataset[-trainIndex, ]
  
  # Entrenamiento del modelo Perceptrón Multicapa (MLP) con 20 neuronas y decay = 0.01
  mlp_model <- train(Type ~ ., data = trainData, method = "nnet", 
                     tuneGrid = data.frame(size = 20, decay = 0.01), 
                     linout = FALSE, trace = FALSE,  # No necesitamos salida lineal ni imprimir el progreso
                     trControl = trainControl(method = "cv", number = 5))
  
  # Predicción sobre el conjunto de prueba
  predictions <- predict(mlp_model, testData)
  
  # Calcular matriz de confusión
  confusion <- confusionMatrix(predictions, testData$Type)
  
  # Calcular métricas
  accuracy <- confusion$overall["Accuracy"]
  kappa <- confusion$overall["Kappa"]
  f1_score <- calculate_f1(confusion$table)
  
  # Guardar resultados
  results <- rbind(results, data.frame(Method = method, 
                                       Accuracy = accuracy, 
                                       Kappa = kappa,
                                       F1 = f1_score))
}

# -----------------------------------------
# Visualización de resultados
# -----------------------------------------
# Gráfico de barras para Accuracy y F1-score
results_long <- results %>%
  pivot_longer(cols = c(Accuracy, F1), names_to = "Metric", values_to = "Value")

ggplot(results_long, aes(x = Method, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  labs(title = "Comparación de Métricas entre Métodos de Discretización para MLP",
       x = "Método de Discretización y Configuración de MLP",
       y = "Valor Métrico") +
  scale_fill_brewer(palette = "Set3") +
  theme_minimal()

# Gráfico de líneas para comparar tendencias
ggplot(results_long, aes(x = Method, y = Value, group = Metric, color = Metric)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  labs(title = "Tendencias de Métricas por Método de Discretización para MLP",
       x = "Método de Discretización y Configuración de MLP",
       y = "Valor Métrico") +
  theme_minimal() +
  scale_color_brewer(palette = "Set1")

# -----------------------------------------
# Tabla de resultados
# -----------------------------------------
# Instalar knitr si no está disponible
if (!require(knitr)) install.packages("knitr")

# Crear una tabla formateada
library(knitr)

# Reiniciar los nombres de fila
rownames(results) <- NULL

# Mostrar los resultados en una tabla
kable(results, 
      col.names = c("Método de Discretización", "Accuracy", "Kappa", "F1-Score"),
      caption = "Resultados del Modelo Perceptrón Multicapa (MLP) con 20 Neuronas y decay = 0.01",
      align = c('l', 'c', 'c', 'c'))


Table: Resultados del Modelo Perceptrón Multicapa (MLP) con 20 Neuronas y decay = 0.01

|Método de Discretización | Accuracy  |   Kappa   | F1-Score  |
|:------------------------|:---------:|:---------:|:---------:|
|EqualWidth               | 0.5901639 | 0.4234405 | 0.5638528 |
|EqualFrequency           | 0.7213115 | 0.6111736 | 0.7035714 |
|ChiMerge                 | 0.8196721 | 0.7494399 | 0.7899041 |
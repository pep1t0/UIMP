# ------------------------------------------------------------------------------------------------------
# Entrenamiento con el algoritmo Knn 
# ------------------------------------------------------------------------------------------------------

# -----------------------------------------
# Librerías necesarias
# -----------------------------------------
library(caret)        # Para entrenar modelos
library(ggplot2)      # Para visualización
library(dplyr)        # Para manipulación de datos
library(e1071)        # Para kNN
library(tidyr)

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

# Inicializar una lista vacía para almacenar las matrices de confusión
confusion_matrices <- list()

# -----------------------------------------
# Entrenamiento y evaluación
# -----------------------------------------
for (method in names(datasets)) {
  dataset <- datasets[[method]]
  
  # Dividir en entrenamiento y prueba (70/30)
  trainIndex <- createDataPartition(dataset$Type, p = 0.7, list = FALSE)
  trainData <- dataset[trainIndex, ]
  testData <- dataset[-trainIndex, ]
  
  # Validación cruzada con 5 particiones
  train_control <- trainControl(method = "cv", number = 5)
  
  # Modelo kNN
  knn_model <- train(Type ~ ., data = trainData, method = "knn", 
                     tuneGrid = expand.grid(k = c(3, 5, 7)), 
                     trControl = train_control)
  
  # Predicción sobre el conjunto de prueba
  predictions <- predict(knn_model, testData)

  # Asegurarse de que testData$Type sea un factor
  testData$Type <- factor(testData$Type)

  # Convertir las predicciones en factor con los mismos niveles que testData$Type
  predictions <- factor(predictions, levels = levels(testData$Type))

  # Calcular matriz de confusión
  confusion <- confusionMatrix(predictions, testData$Type)

  # Guardar la matriz de confusión en la lista
  confusion_matrices[[method]] <- confusion$table

  # Calcular métricas
  accuracy <- confusion$overall["Accuracy"]
  kappa <- confusion$overall["Kappa"]
  # sensitivity <- confusion$byClass["Sensitivity"]
  # specificity <- confusion$byClass["Specificity"]
  # f1_score <- confusion$byClass["F1"]
  f1_score <- calculate_f1(confusion$table)

  # Guardar resultados
  # results <- rbind(results, data.frame(Method = method, Accuracy = accuracy, F1 = f1_score))
  results <- rbind(results, data.frame(Method = method, 
                                       Accuracy = accuracy, 
                                       Kappa = kappa,
                                       F1 = f1_score)) 
   #                                    Sensitivity = sensitivity, 
   #                                    Specificity = specificity,                               
}

# -----------------------------------------
# Visualización de resultados
# -----------------------------------------
# Gráfico de barras para Accuracy y F1-score
results_long <- results %>%
  pivot_longer(cols = c(Accuracy, Kappa, F1), names_to = "Metric", values_to = "Value")

ggplot(results_long, aes(x = Method, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  labs(title = "Comparación de Métricas entre Métodos de Discretización para Knn",
       x = "Método de Discretización",
       y = "Valor Métrico") +
  scale_fill_brewer(palette = "Set3") +
  theme_minimal()

# Gráfico de líneas para comparar tendencias
ggplot(results_long, aes(x = Method, y = Value, group = Metric, color = Metric)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  labs(title = "Tendencias de Métricas por Método de Discretización para Knn",
       x = "Método de Discretización",
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
      caption = "Resultados del Modelo kNN según Método de Discretización",
      align = c('l', 'c', 'c', 'c'))
  




Table: Resultados del Modelo kNN según Método de Discretización

|Método de Discretización | Accuracy  |   Kappa   | F1-Score  |
|:------------------------|:---------:|:---------:|:---------:|
|EqualWidth               | 0.7142857 | 0.4166667 | 1.0000000 |
|EqualFrequency           | 0.7500000 | 0.6653595 | 0.8068910 |
|ChiMerge                 | 0.7540984 | 0.6560150 | 0.7180135 |


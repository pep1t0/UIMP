# ------------------------------------------------------------------------------------------------------
# Entrenamiento con el algoritmo Naive Bayes
# ------------------------------------------------------------------------------------------------------


# -----------------------------------------
# Librerías necesarias
# -----------------------------------------
library(caret)        # Para particiones de datos
library(ggplot2)      # Para visualización
library(dplyr)        # Para manipulación de datos
library(e1071)        # Para el algoritmo Naive Bayes
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
  
  # Entrenar el modelo Naive Bayes
  nb_model <- naiveBayes(Type ~ ., data = trainData)
  
  # Predicción sobre el conjunto de prueba
  predictions <- predict(nb_model, testData)

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
  pivot_longer(cols = c(Accuracy, Kappa, F1), names_to = "Metric", values_to = "Value")

ggplot(results_long, aes(x = Method, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  labs(title = "Comparación de Métricas entre Métodos de Discretización para Naive Bayes",
       x = "Método de Discretización",
       y = "Valor Métrico") +
  scale_fill_brewer(palette = "Set3") +
  theme_minimal()

# Gráfico de líneas para comparar tendencias
ggplot(results_long, aes(x = Method, y = Value, group = Metric, color = Metric)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  labs(title = "Tendencias de Métricas por Método de Discretización para Naive Bayes",
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
      caption = "Resultados del Modelo Naive Bayes según Método de Discretización",
      align = c('l', 'c', 'c', 'c'))



Table: Resultados del Modelo Naive Bayes según Método de Discretización

|Método de Discretización | Accuracy  |   Kappa   | F1-Score  |
|:------------------------|:---------:|:---------:|:---------:|
|EqualWidth               | 0.5573770 | 0.3859060 | 0.5780045 |
|EqualFrequency           | 0.6393443 | 0.5062546 | 0.6574074 |
|ChiMerge                 | 0.8032787 | 0.7323583 | 0.7689153 |
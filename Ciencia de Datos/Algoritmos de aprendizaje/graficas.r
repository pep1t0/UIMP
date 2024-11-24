# Ver las primeras filas del dataset
head(dataset)

# Resumen estadístico del dataset
summary(dataset)

# Correlación entre atributos y clase
cor(dataset[, -c(1)], dataset$Type)

# Instalar y cargar la librería pheatmap para el heatmap
if (!require(pheatmap)) install.packages("pheatmap")
library(pheatmap)

# Calcular la matriz de correlación entre todas las variables
cor_matrix <- cor(dataset[, sapply(dataset, is.numeric)])

# Crear un heatmap de la matriz de correlación
pheatmap(cor_matrix, main = "Matriz de Correlación de Atributos", display_numbers = TRUE)
# ----------------------------------------------------------------------------------------
# Código EXTRA 
# ----------------------------------------------------------------------------------------


# -----------------------------------------
# Cálculo de F1-score por clase
# -----------------------------------------
f1_scores <- numeric(length = nrow(confusion$table))  # Vector para almacenar F1 por clase

# Iterar sobre cada clase
for (i in 1:nrow(confusion$table)) {
  # Obtener la matriz de confusión para la clase
  TP <- confusion$table[i, i]  # Verdaderos positivos
  FP <- sum(confusion$table[, i]) - TP  # Falsos positivos
  FN <- sum(confusion$table[i, ]) - TP  # Falsos negativos
  
  # Calcular precision y recall
  precision <- TP / (TP + FP)
  recall <- TP / (TP + FN)
  
  # Calcular F1-score para la clase
  f1_scores[i] <- 2 * ((precision * recall) / (precision + recall))
}

# -----------------------------------------
# Crear un DataFrame para los F1-scores
# -----------------------------------------
f1_results <- data.frame(Class = rownames(confusion$table), F1_Score = f1_scores)

# -----------------------------------------
# Gráfico de barras para F1-score por clase
# -----------------------------------------
ggplot(f1_results, aes(x = Class, y = F1_Score, fill = Class)) +
  geom_bar(stat = "identity", color = "black", show.legend = FALSE) +
  labs(title = "F1-Score por Clase", x = "Clase", y = "F1-Score") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set3")
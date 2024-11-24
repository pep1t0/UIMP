# ------------------------------------------------------------------------------------------------------
# Discretización de cada atributo usando Equal-Frequency Binning con 4 intervalos ----------------------
# ------------------------------------------------------------------------------------------------------

library(caret)       
library(cluster)     
library(dplyr)
library(tidyr)
library(discretization)
library(ggplot2)


glass <- read.csv("~/Documents/UIMP/Ciencia/glass/glass.data", header = FALSE)

# Asignar nombres a las columnas
colnames(glass) <- c("ID", "RI", "Na", "Mg", "Al", "Si", "K", "Ca", "Ba", "Fe", "Type")

# Mostrar las primeras filas del dataset
head(glass)

# Eliminar la columna 'ID'
glass <- glass[, -1]

# Mostrar las primeras filas del dataset sin la columna 'ID'
head(glass)


glass_discretized_equal_frequency <- glass
glass_discretized_equal_frequency[, -ncol(glass)] <- apply(glass[, -ncol(glass)], 2, function(x) {
  # Calcular los percentiles únicos
  breaks <- unique(quantile(x, probs = seq(0, 1, by = 0.25), na.rm = TRUE))
  
  # Ajustar los intervalos si los límites no son únicos
  if (length(breaks) <= 2) {
    breaks <- seq(min(x, na.rm = TRUE), max(x, na.rm = TRUE), length.out = 5)
  }
  
  # Discretizar con los intervalos calculados
  cut(x, breaks = breaks, labels = FALSE, include.lowest = TRUE)
})

# Mostrar las primeras filas del dataset discretizado
head(glass_discretized_equal_frequency)

# Generar lista de variables numéricas a graficar
numeric_vars <- colnames(glass_discretized_equal_frequency)[-ncol(glass)]  # Excluir la columna 'Type'

# Convertir el dataframe a formato largo para graficar todas las variables en una sola gráfica
glass_long_equal_frequency <- glass_discretized_equal_frequency %>%
  pivot_longer(cols = all_of(numeric_vars), names_to = "variable", values_to = "intervalo")

ggplot(glass_long_equal_frequency, aes(x = intervalo, fill = as.factor(intervalo))) +  # Llenar por intervalo, no por variable
  geom_bar(position = "dodge", color = "black", show.legend = TRUE) +
  labs(title = "Distribución de Variables Discretizadas (Equal-Frequency Binning)",
       x = "Intervalos", y = "Número de Observaciones") +
  facet_wrap(~ variable, scales = "free_x") +  # Crear un panel para cada variable
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  # Rota las etiquetas del eje X
        legend.position = "none") +  # Elimina la leyenda
  scale_fill_brewer(palette = "Set3")  # Usar una paleta de colores para los intervalos


# ----------------------------------------------------------------------
# Analisis -------------------------------------------------------------
# ----------------------------------------------------------------------

# Mostrar los puntos de corte para cada variable en Equal-Frequency Binning
breaks_equal_frequency <- lapply(glass[, -ncol(glass)], function(x) {
  # Calcular los percentiles únicos
  quantile(x, probs = seq(0, 1, by = 0.25), na.rm = TRUE)
})

# Mostrar los puntos de corte para cada atributo
breaks_equal_frequency

# Calcular la frecuencia de observaciones en cada intervalo para Equal-Frequency Binning
freq_equal_frequency <- lapply(glass[, -ncol(glass)], function(x) {
  # Usamos table para contar las frecuencias de los valores en los intervalos
  table(cut(x, breaks = unique(quantile(x, probs = seq(0, 1, by = 0.25), na.rm = TRUE)), 
            labels = FALSE, include.lowest = TRUE))
})

# Mostrar las frecuencias para cada atributo
freq_equal_frequency



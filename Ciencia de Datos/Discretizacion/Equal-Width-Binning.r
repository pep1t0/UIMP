# ------------------------------------------------------------------------------------------------------
# Discretización de cada atributo usando Equal-Width Binning con 4 intervalos --------------------------
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

glass_discretized_equal_width <- glass
glass_discretized_equal_width[, -ncol(glass)] <- apply(glass[, -ncol(glass)], 2, function(x) {
  # Discretizamos con intervalos de igual tamaño
  cut(x, breaks = 4, labels = FALSE)
})

# Mostrar las primeras filas del dataset discretizado
head(glass_discretized_equal_width)

# Generamos una lista de las variables a graficar (todas las variables numéricas)
numeric_vars <- colnames(glass_discretized_equal_width)[-ncol(glass)]  # Excluir la columna 'Type'

# Convertir el dataframe a formato largo (long format) para poder graficar todas las variables en una sola gráfica
glass_long <- glass_discretized_equal_width %>%
  pivot_longer(cols = all_of(numeric_vars), names_to = "variable", values_to = "intervalo")

# Convertir el intervalo a factor para asignar colores diferentes
glass_long$intervalo <- as.factor(glass_long$intervalo)

# Graficar todas las distribuciones en una única gráfica con barras de colores diferentes
ggplot(glass_long, aes(x = intervalo, fill = intervalo)) +  # Usar 'intervalo' para los colores
  geom_bar(position = "dodge", color = "black", show.legend = FALSE) +  # Eliminar la leyenda
  labs(title = "Distribución de Variables Discretizadas (Equal-Width Binning)",
       x = "Intervalos", y = "Número de Observaciones") +
  facet_wrap(~ variable, scales = "free_x") +  # Crear un panel para cada variable
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Ajustar el texto de los ejes
  scale_fill_brewer(palette = "Set3")  # Asignar colores diferentes a las barras dentro de cada gráfico


# ----------------------------------------------------------------------
# Analisis -------------------------------------------------------------
# ----------------------------------------------------------------------

# Calcular los puntos de corte para cada atributo
breaks_list <- lapply(glass[, -ncol(glass)], function(x) {
  # Calcular los puntos de corte con Equal-Width Binning
  cut(x, breaks = 4, labels = FALSE, include.lowest = TRUE, right = FALSE)
})

# Mostrar los puntos de corte para cada atributo
breaks_list

# Calcular la frecuencia de observaciones en cada intervalo para cada variable
freq_list <- lapply(glass[, -ncol(glass)], function(x) {
  # Usamos table para contar las frecuencias de los valores en los intervalos
  table(cut(x, breaks = 4, labels = FALSE, include.lowest = TRUE, right = FALSE))
})

# Mostrar las frecuencias de cada atributo
freq_list
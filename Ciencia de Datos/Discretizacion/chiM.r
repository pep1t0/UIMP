# ------------------------------------------------------------------------------------------------------
# Discretización supervisada de cada atributo usando ChiMerge ------------------------------------------
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

# Instalar y cargar el paquete discretization
install.packages("discretization")
library(discretization)

# Convertir la columna 'Type' a factor (esto es importante para la discretización supervisada)
glass$Type <- as.factor(glass$Type)

# Aplicar ChiMerge para discretizar las variables predictoras
# Usamos la función `chiMerge` de la librería 'discretization'
glass_discretized_chimerge <- glass

# Discretizamos cada variable numérica utilizando ChiMerge
numeric_vars <- colnames(glass)[-ncol(glass)]  # Excluir la columna 'Type'

# Discretizar la variable 'RI' en función de la clase 'Type' usando chiM
disc <- chiM(glass, alpha = 0.05)

# Ver los resultados de la discretización
print(disc)

# Mostrar el resultado de la discretización
head(disc)


# ---------------------------------------------------------------------------------------------------

# Cargar la librería ggplot2 para los gráficos
library(ggplot2)

# Crear una lista con los nombres de las variables a graficar (sin incluir la variable de clase 'Type')
variables <- setdiff(names(glass), "Type")

# Crear un data frame vacío para almacenar los datos de todas las variables
df_total <- data.frame()

# Recorremos cada variable y obtenemos sus intervalos discretizados
for (var in variables) {
  
  # Extraer los intervalos de la variable correspondiente
  intervalos <- disc$Disc.data[[var]]
  
  # Crear un data frame con la variable y sus intervalos discretizados
  df <- data.frame(Variable = glass[[var]], Intervalo = factor(intervalos), VarName = var)
  
  # Añadir los datos al data frame total
  df_total <- rbind(df_total, df)
}

# Crear la gráfica combinada con facet_wrap para todas las variables
ggplot(df_total, aes(x = Intervalo, fill = Intervalo)) +  # Añadir 'fill' para cambiar el color de las columnas
  geom_bar(aes(y = ..count..), color = "black") +  # Agregar contornos a las barras
  labs(title = "Distribución de Variables después de Discretización con chiM",
       x = "Intervalo",
       y = "Frecuencia") +
  facet_wrap(~VarName, scales = "free", ncol = 3) + # Muestra cada gráfico en una "subgráfica"
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  # Rota las etiquetas del eje X
        legend.position = "none") +  # Elimina la leyenda
  scale_fill_brewer(palette = "Set3")  # Utiliza una paleta de colores (puedes elegir otras paletas)




# ----------------------------------------------------------------------
# Analisis -------------------------------------------------------------
# ----------------------------------------------------------------------
# Mostrar los puntos de corte generados por ChiMerge para cada variable
disc$Disc.data

# Calcular la frecuencia de observaciones en cada intervalo para ChiMerge
freq_chimerge <- lapply(variables, function(var) {
  table(disc$Disc.data[[var]])  # Tabla de frecuencias para cada variable
})

# Mostrar las frecuencias de cada variable discretizada
freq_chimerge

# Contar el número de intervalos generados por ChiMerge para cada variable
num_intervals_chimerge <- sapply(variables, function(var) {
  length(unique(disc$Disc.data[[var]]))  # Número de intervalos únicos para cada variable
})

# Mostrar los resultados
num_intervals_chimerge
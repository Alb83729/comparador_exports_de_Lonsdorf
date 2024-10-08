# Cargar las librerías necesarias
library(ggplot2)
library(dplyr)
# Leer los archivos CSV
data_LowRes <- read.csv('Data/Lonsdorf_results_2024-07-05_14-23-02_125.csv', header = TRUE)
data_HighRes <- read.csv('Data/Lonsdorf_results_2024-07-09_17-15-47_62_con_5.csv', header = TRUE)

# Convertir las columnas V5 y V6 a numérico
# data_LowRes$V5 <- as.numeric(as.character(data_LowRes$V5))
# data_LowRes$V6 <- as.numeric(as.character(data_LowRes$V6))
# data_HighRes$V5 <- as.numeric(as.character(data_HighRes$V5))
# data_HighRes$V6 <- as.numeric(as.character(data_HighRes$V6))

# Crear las columnas Lonsdorf_Medio
data_LowRes$Lonsdorf_Medio <- 0.5 * data_LowRes$Lonsdorf_big + 0.5 * data_LowRes$Lonsdorf_small
data_HighRes$Lonsdorf_Medio <- 0.5 * data_HighRes$Lonsdorf_big + 0.5 * data_HighRes$Lonsdorf_small

# Crear un dataframe para la nube de puntos
df_comparison <- data.frame(
  Lonsdorf_Medio_LowRes = data_LowRes$Lonsdorf_Medio,
  Lonsdorf_Medio_HighRes = data_HighRes$Lonsdorf_Medio
)

# Generar la nube de puntos con la recta de regresión
ggplot(df_comparison, aes(x = Lonsdorf_Medio_LowRes, y = Lonsdorf_Medio_HighRes)) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE, color = 'blue') +
  labs(title = 'Comparación de Lonsdorf Medio',
       x = 'Lonsdorf_Medio (Baja resolución: 125 x 125 pixel)',
       y = 'Lonsdorf_Medio (Alta resolución: 62.5 x 62.5 pixel)') +
  theme_minimal()

# Calcular la correlación de Pearson y su p-valor
cor_test_result <- cor.test(df_comparison$Lonsdorf_Medio_LowRes, df_comparison$Lonsdorf_Medio_HighRes,method = "pearson",alternative = "greater")

# Imprimir el resultado de la prueba de correlación en consola
print(cor_test_result)

# Crear un gráfico adicional para mostrar la correlación de Pearson
# df_correlation <- data.frame(
#   Correlation = cor_test_result$estimate,
#   PValue = cor_test_result$p.value
# )
# 
# ggplot(df_correlation, aes(x = factor(1), y = Correlation)) +
#   geom_bar(stat = 'identity') +
#   geom_text(aes(label = sprintf("cor. de Pearson = %.15f\np-valor = %.18e", Correlation, PValue)), vjust = -0.5) +
#   labs(title = '                                   Correlación de Pearson entre Lonsdorf_Medio de ambos archivos',
#        x = '',
#        y = 'Correlación de Pearson') +
#   theme_minimal() +
#   ylim(-1, 2)

# Ajustar el modelo de regresión lineal
linear_model <- lm(Lonsdorf_Medio_HighRes ~ Lonsdorf_Medio_LowRes, data = df_comparison)

# Obtener la pendiente de la regresión
slope <- coef(linear_model)[2]

# Imprimir la pendiente en consola
print(paste("Pendiente de la recta de regresión:", slope))

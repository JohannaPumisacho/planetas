############
# Johanna Pumisacho

# Cargar las librerías
library(dplyr)
library(tidyr)
library(car)
library(ggplot2)

getwd()
setwd("C:/Users/ASUS/Desktop/ProyectoJohanna/planetas")

# Cargar los datos desde el archivo CSV 
datos = read.csv2("H7_10511140.csv")

#Corregir los valores a numeros
datos$Temperatura <- as.numeric(datos$Temperatura)

#Crear una columna nueva que tenga la media por cada día
temperatura_suma_por_dia = datos %>%
  group_by(Fecha) %>%
  summarise(Suma_Temperatura = sum(Temperatura, na.rm = TRUE))

#Realizar un anova
anova_result <- aov(Temperatura ~ Hora, data = datos)

# Mostrar los resultados del ANOVA
summary(anova_result)

# Calcular los residuos del modelo ANOVA
residuos <- residuals(anova_result)

# Prueba de normalidad de los residuos (Shapiro-Wilk)
shapiro_test <- shapiro.test(residuos)
print(shapiro_test)

# Gráfico Q-Q plot para evaluar la normalidad de los residuos
qqnorm(residuos)
qqline(residuos)

# Filtrar las filas con valores no finitos
datos_filtrados <- datos[complete.cases(datos), ]

# Gráfico de temperatura por hora con valores no finitos excluidos
ggplot(datos_filtrados, aes(x = Temperatura, y = Hora)) +
  geom_boxplot(na.rm = TRUE) +
  labs(title = "Cambios de la temperatura por horas",
       x = "Temperatura", y = "Hora") +
  theme_minimal()



# Gráfico de línea para visualizar los cambios de temperatura por hora
ggplot(datos, aes(x = Hora, y = Temperatura, group = 1)) +
  geom_line() +
  labs(title = "Cambios de la temperatura por hora",
       x = "Hora", y = "Temperatura") +
  theme_minimal()

# Gráfico de línea para visualizar los cambios de temperatura por hora
ggplot(datos, aes(x = Temperatura, y = Hora, group = 1)) +
  geom_line() +
  labs(title = "Cambios de la temperatura por hora",
       x = "Temperatura", y = "Hora") +
  theme_minimal()


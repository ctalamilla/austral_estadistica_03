#1 Ejercicio 1
# Cargar la librería ggplot2
library(ggplot2)
#a)
# Crear los vectores x y prop
x <- c(0, 1, 2, 3, 4, 5)
prop <- c(2/15, 1/3, 0, 1/5, 1/15, 4/15)
prob_acumulada <- cumsum(prop)
supervivencia <- (1 - prob_acumulada)
# Crear un dataframe
data <- data.frame(x, prop, prob_acumulada,supervivencia)

# Imprimir el dataframe
print(data)

# Crear un gráfico de barras para la función de probabilidad
grafico_probabilidad <- ggplot(data, aes(x = as.factor(x), y = prop)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(title = "Función de Probabilidad", x = "Variable X", y = "Probabilidad") +
  theme_minimal()

# Crear un gráfico de línea para la distribución acumulativa
distribucion_acumulativa <- ggplot(data, aes(x = x, y = prob_acumulada)) +
  geom_line(color = "red") +
  labs(title = "Distribución Acumulativa", x = "Variable X", y = "Distribución Acumulativa") +
  theme_minimal()
# Mostrar ambos gráficos

library(gridExtra)
grid.arrange(grafico_probabilidad, distribucion_acumulativa, ncol = 2)

data
#b)
#1 P[X>2]
#P(X>2)=P(X=3)+P(X=4)+P(X=5)
resultado = data$prop[data$x==3] + data$prop[data$x==5] + data$prop[data$x==5]
resultado

#2 P[X>=2]
resultado = data$prop[data$x==2] + data$prop[data$x==3] + data$prop[data$x==4] + data$prop[data$x==5] 
resultado

#3 P[X<=1]
resultado = data$prop[data$x==0] + data$prop[data$x==1] 
resultado

#4 P[X<1]
resultado = data$prop[data$x==0]
resultado

#5 F[X=1] # Acumulada
resultado = data$prob_acumulada[data$x==1]
resultado

#6 P[ 3 <= X <= 4]
#P[ 3 <= X <= 4] = P(X=3)+P(X=4)
resultado = data$prop[data$x==3] + data$prop[data$x==4] 
resultado



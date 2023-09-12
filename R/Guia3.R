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

# Ejercicio 2

# Cargar la librería dplyr (si aún no está cargada)
library(dplyr)

# Crear los vectores x y prob
x <- c(0, 2, 5, 6, 1)
prob <- c(1/30, 3/30, 12/30, 8/30, 6/30)

# Crear un dataframe
data <- data.frame(x, prob)

# Calcular la probabilidad acumulada
data <- data %>%
  arrange(x) %>%
  mutate(prob_acumulada = cumsum(prob))

# Agregar una nueva variable aleatoria Z = 2X - 20
data <- data %>%
  mutate(Z = 2 * x - 20)

# Imprimir el dataframe completo con la columna Z
print(data)

# a) Esperanza Matematica
# E(Z) = SUM ( Zi * P(Zi))
# Calcular la esperanza matemática (media) de Z

esperanza_Z <- sum(data$Z * data$prob)

# b) Calcular el desvió estándar de Z
# SD = sqrt(SUM( Zi - E(Z) **2 * P(Zi)  ))
desvio_estandar_Z <- sqrt(sum((data$Z - esperanza_Z)^2 * data$prob))

# Imprimir los resultados
cat("Esperanza Matemática (Media) de Z:", esperanza_Z, "\n")
cat("Desvió Estándar de Z:", desvio_estandar_Z, "\n")

# Ejercicio 3

# Datos dados
EX <- 2
EY <- 4
EW <- 1
VarX <- 2
VarY <- 2
VarW <- 1

# a) E(X + Y + W)
a <- EX + EY + EW
cat("a) E(X + Y + W) =", a, "\n")

# b) E(2X)
b <- 2 * EX
cat("b) E(2X) =", b, "\n")

# c) E(3X + Y)
c <- 3 * EX + EY
cat("c) E(3X + Y) =", c, "\n")

# d) E[W * (X + Y)]
d <- EW * (EX + EY)
cat("d) E[W * (X + Y)] =", d, "\n")

# e) Var(X + Y + W)
e <- VarX + VarY + VarW
cat("e) Var(X + Y + W) =", e, "\n")

# f) Var(2X)
f <- (2^2) * VarX
cat("f) Var(2X) =", f, "\n")

# g) Var(3X - Y)
g <- (3^2) * VarX + VarY
cat("g) Var(3X - Y) =", g, "\n")

# Ejercicio 4

# El 10% de las piezas que produce una máquina son defectuosas. Si se toma al
# azar una muestra de 20 piezas.
# Datos
n <- 20  # Tamaño de la muestra
p <- 0.10  # Probabilidad de que una pieza sea defectuosa

# a) ¿Cuál es la probabilidad de que contenga 3 piezas defectuosas como máximo?

dbinom(0, n, p)
dbinom(1, n, p)
dbinom(2, n, p)
dbinom(3, n, p)

piezas = 0:3 #vector 0, 1, 2, 3

dbinom(piezas, n, p)

prob_max_3_defectuosas <- sum(dbinom(0:3, n, p))
prob_max_3_defectuosas

#   b) ¿Cuál es la probabilidad de que contenga 3 piezas defectuosas como mínimo?
piezas = 3:20
dbinom(piezas, n, p)# la probabilidad de cada elemento del vector

dbinom(piezas, n, p) %>% sum() # Resultado  /// suma del vector

prob_min_3_defectuosas = dbinom(piezas, n, p) %>% sum() 

#   c) ¿Cuál es la probabilidad de que se den los dos sucesos descriptos
# anteriormente?

prob_ambos = prob_max_3_defectuosas * prob_min_3_defectuosas
prob_ambos


# #Ejercicio 5
# El 15% de los operarios de una fábrica no tienen hijos a cargo, el 28% tiene
# solamente uno a cargo, el 41% posee 2 hijos, el 13% tiene 3 hijos y el resto tiene
# 4 hijos. Se sabe que, a fin de año, se abona un adicional cuyo monto se fija de la
# siguiente forma,

hijos = c(0, 1, 2, 3, 4)

prop = c(0.15, 0.28, 0.41, 0.13, 0.03)

prop_acc = cumsum(prop)

data = data.frame(hijos, prop, prop_acc)

# i. $200 para los operarios que no tienen hijos a cargo

# ii. $250 para los operarios con hijos a cargo que poseen menos de 3 hijos;

# iii. $400 para el resto de los operarios.

# a) Calcular el valor esperado y el desvío estándar de la variable aleatoria definida
# como “cantidad de hijos a cargo de un operario”.

esperanza<- sum(data$hijos * data$prop)
esperanza

desvio_estandar <- sqrt(sum((data$hijos - esperanza)**2 * data$prop))
desvio_estandar

#b) Si en la fábrica trabajan 1.000 operarios, ¿Cuánto se espera tener que abonar
#en total este fin de año en concepto de adicionales?

data

prop_noHijos = data$prop[data$hijos==0]
prop_Menos3_Hijos = data$prop[data$hijos<3 & data$hijos > 0] %>%  sum()
prop_3oMas_Hijos = data$prop[data$hijos>=3] %>%  sum()

monto_adicional = (prop_noHijos * 200 + prop_Menos3_Hijos * 250 + prop_3oMas_Hijos *400) * 1000
monto_adicional

# Ejercicio 6
# El 34% de los vendedores de un comercio están casados, se toma una muestra al
# azar de 15 de ellos y se desea conocer las siguientes probabilidades de encontrar:

p = 0.34
n = 15

#a) Exactamente 7 vendedores casados

dbinom(0:7, n, p) %>% sum()

#b) A lo sumo 3 vendedores casados

dbinom(0:3, n, p) %>% sum()

#c) Menos de 5 vendedores casados

dbinom(0:4, n, p) %>% sum()

#d) Como mínimo 8 vendedores casados
dbinom(8:15, n, p) %>% sum()

#e) Más de 10 vendedores casados

dbinom(11:15, n, p) %>% sum()

#f) Entre 4 y 9 vendedores casados

dbinom(4:9, n, p) %>% sum()

# g) Por lo menos un vendedor casado

#alternativa 1
1 - dbinom(0, n, p)

#alternativa 2
1 - (1-p)**15

# h) Como máximo 6 vendedores no casados

dbinom(0:6, n, 1-p) %>% sum()

prop = dbinom(0:15, n, p)
cant_cansados = 0:15

esperanza = (prop * cant_cansados) %>%  sum()
esperanza

sd = sqrt(sum((cant_cansados - esperanza)**2 * prop))
varianza = sum((cant_cansados - esperanza)**2 * prop)

# Ejercicio 7 
# Un curso de Estadística de la Universidad de Buenos Aires posee 10 alumnos, de
# los cuales 4 son mujeres y el resto hombres. Se eligieron al azar 7 estudiantes del
# curso para tomarles un examen. Hallar la probabilidad de que, entre los alumnos
# seleccionados, haya tres mujeres.

p = 4/10
n = 7

dbinom(3, n, p)

# Ejercicio n° 8
# El retorno diario de cierto activo financiero sigue una distribución normal, con
# una media de 0,60% y un desvío típico de 0,15%.

Mu = 0.60
sd_poblacion = 0.15


# a) ¿Cuál es la probabilidad de que, en un día cualquiera, este activo haya
# brindado un retorno entre el 0,42% y el 0,90%, si se sabe que fue superior al
# 0,51%?

# P[0.42 <  X  < 0.9 | P[x]>0.51]

linf = 0.51

zinf = (linf - Mu) / sd_poblacion 
# -1.2
pnorm(zinf)

lsup = 0.90
zsup = (lsup - Mu) / sd_poblacion 
# 2
pnorm(zsup)

#P[0.42 <  X  < 0.9]
pnorm(zsup) - pnorm(zinf)

# P[X>0.51]
p051 = 0.51
z051 = (p051 - Mu) / sd_poblacion 

# denominador
1 - pnorm(z051)

# P[0.42 <  X  < 0.9 | P[x]>0.51]
(pnorm(zsup) - pnorm(zinf)) / (1 - pnorm(z051))



# b) ¿Cuál es el retorno diario que no es superado el 1% de las veces?
#P(X>a)=1%
qnorm(1/100,mean = Mu,sd=sd)


# c) Un inversor piensa comprar acciones de este activo y venderlas 15 ruedas más
# tarde (es decir, 15 días hábiles posteriores) y desea saber la probabilidad de
# que en más de dos ruedas, la rentabilidad diaria sea por lo menos del 0,75%
#Y~Bi(n=15,p=?)

p = 1-pnorm(.75, mean= Mu, sd=sd)
#P(Y>2)BONIMIAL
#1 - P(Y<=2)
pbinom(2,15,p=p)


#Ejercicio 9
# El peso de los paquetes de harina marca A, que se envasan automáticamente,
# tienen distribución normal con un promedio de 500 gramos. Se sabe que el 32%
# de los paquetes pesa como mínimo 513 gramos, contestar:

mu=500

#a) ¿Cuánto vale el desvío estándar?

#P(X>513)=0.32
#P[X<513] = 1 - 0.32
p = 1 -0.32 
z = qnorm(p)

# 0.4676 = 513 - 500 / desvio_muestra
desvio_muestra = (513 - mu) / z
desvio_muestra
#27.7956

#b) Se elige al azar un paquete y se comprueba que su peso es inferior al peso
#promedio, ¿Cuál es la probabilidad de que pese por lo menos 475 gramos?

#P(X>475 | P(x<500))

p1 = 1 - pnorm(475, mean=500, sd=desvio_muestra )
p1

#P[X<Mu]
p2 = 0.5 #prob menor o igual a la media

#respuesta = P[X>475] - P[X<Mu]
respuesta = p1 - p2
respuesta

pnorm(475,mean=500,sd=sd)# resolucion guia


# c) Se vende un lote de 15 paquetes de harina, ¿Cuál es la probabilidad de que a
# lo sumo las dos quintas partes de los paquetes del lote pesen menos de 493
# gramos?

#Y~Bi(n=15,p=(P(X<493)))
p=pnorm(493,mean = 500,sd=sd)
(2/5)*15
#P(Y<=6)
pbinom(6,size = 15,p=p)

# Ejercicio 10
# Una fábrica de cartuchos de tinta para impresora comercializa su producción en
# cajas que contienen 12 unidades. En base a información histórica de la empresa,
# se sabe que el 7% resultan defectuosos y que la ganancia obtenida por cada caja
# se encuentra en función de la cantidad de unidades defectuosas según el
# siguiente esquema,

# - Si la caja no contiene ningún cartucho defectuoso, la ganancia es de $2,50.
# - Si la caja contiene uno o dos cartuchos defectuosos, la ganancia es de $1,80.
# - Si la caja contiene tres o cuatro cartuchos defectuosos, la ganancia es de
# $1,20.
#- Si la caja contiene más de 4 cartuchos defectuosos es devuelta y origina una
# pérdida de $2,40.

# a) Calcular la ganancia que se espera obtener por la venta de 200 cajas.

# b) ¿Cuál es la probabilidad de que una caja produzca como mínimo $1,50 de
# ganancia?

#   c) Se vendieron 4 cajas, ¿Cuál es la probabilidad de obtener una ganancia total
# de $10 por dicha venta?





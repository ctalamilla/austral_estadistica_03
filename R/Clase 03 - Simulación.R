##########################################################
#                   ESTADISTICA                          #
#               Prof. DEL ROSSO - LEVINIS                #
#	            MAESTRIA EN CIENCIA DE DATOS               #
#                FACULTAD DE INGENIERIA                  #
#                 UNIVERSIDAD AUSTRAL                    #
##########################################################

## Asignatura: ESTADÍSTICA
## Docentes: Rodrigo Del Rosso - Gustavo Levinis

rm(list = ls())

###########################################
## Simulaciones con Números "Aleatorios" ##
###########################################

## Función Sample ##

1:6

sample(1:6, 1)

sample(1:6, 10, replace = T)

# Roll One Dice = Rodar un Dado
Roll1Dice <- function(n){
  return(sample(1:6, n, rep = T))
}

Roll1Dice(10)

# Flip One Coin = Lanzar una Moneda
# Head = Cara
# Tail = Cruz

n = 10
sample(c("Heads", "Tails"), n, rep = T)

Flip1Coin <- function(n){
  return(sample(c("Heads", "Tails"), n, rep = T))
}

Flip1Coin(n)

C = Flip1Coin(1000)

sum(C == "Heads")
sum(C == "Tails")
sum(C != "Heads")

table(C)

prop.table(table(C))

# Ejemplo de Ciclo For

for(i in 1:100){
  print(i)
}

# Experimento de Tirar 2 Dados
roll1 = NULL
roll2 = NULL
for(i in 1:100){
  roll1[i] = Roll1Dice(1)
  roll2[i] = Roll1Dice(1)
}

sum(roll1 == roll2)  ## Suma la cantidad de veces que los valores son iguales

## Histograma de la Suma de la tirada de dos Dados
hist((roll1 + roll2), density = 100, breaks = 1:12, prob = T)

## Barplot de la Suma de la tirada de dos Dados
barplot(table(roll1 + roll2), main = "2 Dice Sum, 100 Rolls")

## Igual experimento pero se repite 10000 veces 
roll1 = NULL
roll2 = NULL
for (i in 1:10000) {
  roll1[i] = Roll1Dice(1)
  roll2[i] = Roll1Dice(1)
}

## Barplot para las 10000 tiradas
barplot(table(roll1 + roll2), density = 100, main = "2 Dice Sum, 10000 Rolls")

## Otro ejemplo
random.numbers = sample(x = 1:10, size = 5, replace = TRUE)
sum(random.numbers)

## Tirada de un dado con reposición
sample(1:6, size = 1, replace = TRUE)

## Tirada de dos dados en forma individual con reposición
sample(1:6, size = 1, replace = TRUE) + sample(1:6, size = 1, replace = TRUE)

## Tirada de dos dados en forma conjunta
Dice.roll <- sample(1:6, size = 2, replace = TRUE)
Dice.roll

sum(Dice.roll)

# Otra forma
sum(sample(1:6, size = 2, replace = TRUE))

# Crear otra función = Tirada de dos dados
two.Dice <- function(){
  Dice <- sample(1:6, size = 2, replace = TRUE)
  return(sum(Dice))
}

two.Dice()

# Lo replico 20 veces
replicate(n = 20, expr = two.Dice())

## Función simil para tamaño "n.Dice"
Dice.sum <- function(n.Dice){
  Dice <- sample(1:6, size = n.Dice, replace = TRUE)
  return(sum(Dice))
}

# lo replicamos 50 veces con 3 tiradas de un dado
replicate(50, Dice.sum(3))

# ahora generalizamos la cantidad de lados del dado a "n.sides"
# y la cantidad de tiradas del lado "n.Dice"
my.Dice.sum <- function(n.Dice, n.sides){
  Dice <- sample(1:n.sides, size = n.Dice, replace = TRUE)
  return(sum(Dice))
}

# lo replicamos 100 veces
replicate(100, my.Dice.sum(5,4))


## Aproximación al concepto de Probabilidad

sims <- replicate(100, two.Dice())
table(sims)
table(sims)/length(sims)

plot(table(sims), 
     xlab = 'Sum', 
     ylab = 'Frequency', 
     main = '100 Rolls of 2 Fair Dice')


plot(table(sims)/length(sims), 
     xlab = 'Sum', 
     ylab = 'Relative Frequency', 
     main = '100 Rolls of 2 Fair Dice')

# 1000 Simulaciones
more.sims <- replicate(1000, two.Dice())
table(more.sims)/length(more.sims)

plot(table(more.sims)/length(more.sims), 
     xlab = 'Sum', ylab = 'Relative Frequency', main = '1000 Rolls of 2 Fair Dice')

# 100000 Simulaciones
even.more.sims <- replicate(100000, two.Dice())
table(even.more.sims)/length(even.more.sims)

plot(table(even.more.sims)/length(even.more.sims), 
     xlab = 'Sum', 
     ylab = 'Relative Frequency', 
     main = '100000 Rolls of 2 Fair Dice')

# Este problema fue inicialmente planteado por el famoso jugador del siglo XVII 
# Antoine Gombaud, más conocido como el Caballero de Meré.
# Fermat y Pascal discutieron su solución en su correspondencia

# Que es más probable: 
# (A) obtener al menos un 6 al lanzar un solo dado 
#  de seis lados justo 4 veces o 

# (B) obtener al menos un par de seis cuando se lanzan dos dados 
# de seis lados justos 24 veces.

experimentA <- function(){
  rolls <- sample(1:6, size = 4, replace = TRUE)
  condition <- sum(rolls == 6) > 0
  return(condition)
}

experimentB <- function(){
  first.Dice <- sample(1:6, size = 24, replace = TRUE)
  second.Dice <- sample(1:6, size = 24, replace = TRUE)
  condition <- sum((first.Dice == second.Dice) & (first.Dice == 6)) > 0
  return(condition)
}

simsA <- replicate(100000, experimentA())
sum(simsA)/length(simsA)

simsB <- replicate(100000, experimentB())
sum(simsB)/length(simsB) 

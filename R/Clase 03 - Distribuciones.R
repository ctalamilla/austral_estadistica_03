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

##############################################
####### DISTRIBUCIONES DE PROBABILIDAD #######
##############################################

# Distribución Binomial
# Ejemplo:  Sea X ~ Bi(5,0.1) 

dbinom(3,5,0.1) # probabilidad puntual (Función de Probabilidad Puntual)

pbinom(3,5,0.1)               # probabilidad acumulada a izquierda (Función de Distribución)

pbinom(3,5,0.1,lower.tail=F)  # probabilidad acumulada a derecha (Función de Supervivencia)

pbinom(3,5,0.1,F)


#.k/P(x<=k)=0,9999

qbinom(0.9999,5,0.1)

#Comprobamos P(x<=4)

pbinom(4,5,0.1)

#Si queremos generar "números aleatorios" con distribución  binomial 

rbinom(10,5,0.1)        #genera valores "al azar", pseudoaleatorios 

rbinom(10,5,0.2)

rbinom(10,5,0.5)

set.seed(34)
rbinom(10,5,0.9)

x <- rbinom(10,5,0.9)
x
table(x)  ## tabla de frecuencias absolutas simples
prop.table(table(x))  ## frecuencias relativas

#Analogamente para las distribuciones discretas help(distributions)

help(distributions)

###############
## Discretas ##
###############

dbinom    ## Binomial (Bernoulli)
dgeom     ## Geométrica
dpois     ## Poisson
dnbinom   ## Binomial Negativa
dmultinom ## Multinomial
dhyper    ## Hipergeométrica

###############
## Continuas ##
###############

dbeta     ## Beta
dchisq    ## Chi Cuadrado
dexp      ## Exponencial
df        ## F de Snedecor
dgamma    ## Gamma
dlnorm    ## Lognormal
dnorm     ## Normal
dt        ## T de Student
dunif     ## Uniforme
dweibull  ## Weibull

## más distribuciones en el paquete "distr"

########################
####### GRÁFICOS #######
########################

# Grafiquemos la función de probabilidad puntual

n <- 5
x <- seq(0,n)

par(mex=0.7)
par(mgp=c(2,0.3,0))
par(cex=0.65)  

p<-0.1
y<-dbinom(x,n,p)

plot(x,y,ylab="p(x)",type="h",lwd = 2,col="green")
points(x,y,pch = 16,col="green")
mtext("p=0.10",3,-2)

# Sea X~Bi(20,p) , grafiquemos la función de probabilidad puntual para distintos valores de p

x11()
n <- 20
x <- seq(0,n)

par(mfrow=c(3,2))
par(mex=0.7)
par(mgp=c(2,0.3,0))
par(cex=0.65)

p<-0.1
y<-dbinom(x,n,p)
plot(x,y,ylab="p(x)",type="h",lwd = 2,col="green")
points(x,y,pch = 16,col="green")
mtext("p=0.10",3,-2)

p<-0.25
y<-dbinom(x,n,p)
plot(x,y,ylab="p(x)",type="h",lwd = 2,col="green")
points(x,y,pch = 16,col="green")
mtext("p=0.25",3,-2)

p<-0.50
y<-dbinom(x,n,p)
plot(x,y,ylab="p(x)",type="h",lwd = 2,col="green")
points(x,y,pch = 16,col="green")
mtext("p=0.50",3,-2)


p<-0.60
y<-dbinom(x,n,p)
plot(x,y,ylab="p(x)",type="h",lwd = 2,col="green")
points(x,y,pch = 16,col="green")
mtext("p=0.60",3,-2)

p<-0.75
y<-dbinom(x,n,p)
plot(x,y,ylab="p(x)",type="h",lwd = 2,col="green")
points(x,y,pch = 16,col="green")
mtext("p=0.75",3,-2)

p<-0.90
y<-dbinom(x,n,p)
plot(x,y,ylab="p(x)",type="h",lwd = 2,col="green")
points(x,y,pch = 16,col="green")
mtext("p=0.90",3,-2)

# Distribución Normal

# Ejemplo:  Sea X~N(mu, sigma) 

# dnorm(x,mean,sd) evalua la función de densidad en x
# pnorm(q,mean,sd) P(x<=q)
# qnorm(p,mean,sd) q/P(x<=q)=p
# rnorm(n,mean,sd) genera n valores pseudoaleatorios normales con la media y el desv???o dado.

# X~N(0,1)

#.f(0)
dnorm(0,0,1)

1/sqrt(2*pi)

#.P(X<=0)
pnorm(0,0,1)

#.P(X<=2)
pnorm(2,0,1)

#.P(X 2)

pnorm(2,0,1,F)

qnorm(0.99,0,1)

qnorm(0.99,0,1,F)

set.seed(34)
rnorm(10,0,1)

# Gráfico de densidades

x11()
x <- seq(-8,8,0.01)
plot(x,dnorm(x),type="l",ylim=c(0,1),ylab="")    #  normal estandar
lines(x,dnorm(x,0,2),type="l",ylim=c(0,1),col="red") # media 0 y desvío 2
lines(x,dnorm(x,0,0.5),type="l",ylim=c(0,1),col="blue") # media 0 y desvío 0.5
lines(x,dnorm(x,2,1),type="l",ylim=c(0,1),col="green") # media 2 y desvío 1
lines(x,dnorm(x,-2,0.5),type="l",ylim=c(0,1),col="brown") # media -2 y desvío 0.5
legend(1.7,1, legend = c("Media=0 Desvío=1","Media=0 Desvío=2","Media=0 Desvío=0.5","Media=2 Desv???o=1","Media=-2 Desv???o=0.5"),
       col = c(1,"red","blue","green","brown"),lty = 1, cex = .8, y.intersp = 1)
title("Funciones de densidad  de la distribución Normal")

## Limpieza de todos los objetos ##

rm(list = ls())
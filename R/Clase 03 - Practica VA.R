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

#Ejercicio 1

# Definimos una función de probabilidad puntual
p = c(2/15,1/3,0,1/5,1/15,4/15)
names(p)<-0:5
sum(p)
p

# Gráfico de Bastones
plot(0:5,p,col = "steelblue",pch = 19,frame.plot = F)
segments(x0=0:5,x1=0:5,y0=0,y1=p,col="steelblue",lwd=2)

Fx = function(x){
  cumsum(p)[x+1]
}
Fx(0)

# P(X>2)
1-Fx(2)
sum(p[4:6])

#Px(X<=1)
Fx(1)
sum(p[c("0","1")])

#P(X<1)
Fx(1)-p["1"]
p["0"]

#F(x)
Fx(1)
#P(3 <= X <= 4)
Fx(4)-Fx(2)
sum(p[c("3","4")])

# Ejercicio 2
p = c(1/30,3/30,12/30,8/30,6/30)
names(p)<-c("0","2","5","6","10")
p

Z = 2*c(0,2,5,6,10)-20
sum(Z*p)

# Ejercicio 4

# P(X<=3)
pbinom(3,prob = 0.1,size = 20)

# P(X>=3) = 1 - P(X<=2)
1-pbinom(2,prob = 0.1,size = 20)

# P(X=3)
dbinom(3,size = 20,prob = 0.1)

# Ejercicio 5
p = c(15,28,41,13)/100
p = c(p,1-sum(p))

# Valor esperado
sum(p*c(200,250,250,250,50))

# en 1000 empleados
sum(p*c(200,250,250,250,50))*1000

# Ejercicio 6
p=.34

#P(X=7)
dbinom(7,size = 15,prob = p)
pbinom(3,size = 15,prob = p)
pbinom(4,size = 15,prob = p)

#P(X>=8) = 1 - P(X<=7)
1- pbinom(7,15,p)
pbinom(7,15,p,lower.tail = F)

#P(X>10)
pbinom(10,15,p,lower.tail = F)

#P(4<=X<=9)
pbinom(9,15,p)-pbinom(3,15,p)
sum(dbinom(4:9,15,p))

#P(X>0) = 1 - P(X=0)
1 - dbinom(0,15,p)

#6 No Casados => 15-6 = 9 Casados
#P(X>=9) = 1 - P(X<=8)
1 - pbinom(8,15,p)
#Esperanza, Var, SD
15*p
15*p*(1-p)
sqrt(15*p*(1-p))

# Ejercicio  7
n = 10
p = 4/10
dbinom(3,size = 7,prob = p)

#Ejercicio 8
mu = 0.6
sd = 0.15
#P(.42<X<.90|X>.51)
#P(.51<X<.90)/P(X>.51)
num = pnorm(.90,mean = mu,sd = sd) - pnorm(.51,mean = mu,sd = sd)
num / (1 - pnorm(.51,mean=mu,sd=sd))
#P(X>a)=1%
qnorm(1/100,mean = mu,sd=sd)
#Y~Bi(n=15,p=?)
p = 1-pnorm(.75)
#P(Y>2)
#1 - P(Y<=2)
pbinom(2,15,p=p)

#Ejercicio 9
#mu=500g
#P(X>512)=.32
qnorm(1-.32)
512-500 / sd = 0.47
sd = (512-500)/qnorm(1-.32)
qnorm(1-.32,mean = 500,sd=sd)
#P(X<475)
pnorm(475,mean=500,sd=sd)
#Y~Bi(n=15,p=(P(X<493)))
p=pnorm(493,mean = 500,sd=sd)
(2/5)*15
#P(Y<=6)
pbinom(6,size = 15,p=p)

#Ejercicio 10
#p=7%
#n=12
#P(X=0) * 2.5
p0 = dbinom(0,size = 12,prob = 7/100)
#P(X=1 U X=2) * 1.8
p12 = sum(dbinom(1:2,size = 12,prob = 7/100))
#P(X=3 U X=4) * 1.2
p34 = sum(dbinom(3:4,size = 12,prob = 7/100))
#P(X>4) * -2.4
p4c = 1 - pbinom(4,size = 12,prob = 7/100)
p=c(p0,p12,p34,p4c)
sum(p)
sum(p*c(2.5,1.8,1.2,-2.4))*200
#P(Y>1.5) = P(X=1 U X=2) + P(X=0)
p0+p12
#P(Z=$10) => P(X=0)*P(X=0)*P(X=0)*P(X=0)
p0^4

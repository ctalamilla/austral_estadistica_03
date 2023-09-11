
## SETEAR RUTA DE TRABAJO
setwd("...") ## colocar entre comillas la ruta de trabajo

## CARGAR PAQUETES
suppressPackageStartupMessages({
  library(dplyr)
  library(rriskDistributions)
  library(fitdistrplus)
  library(lessR)
})

## CARGAR LOS DATOS
datos <- read.csv("results.csv",header = T)
head(datos)
names(datos)

## TOMAR DATOS DE ARGENTINA y BRASIL DE LOCAL
Argentina <- filter(datos,home_team == "Argentina")
Brasil <- filter(datos, home_team == "Brazil")

## HISTOGRAMA
Histogram(x = home_score,
          data = Argentina,
          xlab = "# Goles Local", 
          ylab ="Cantidad", 
          main = "Argentina")

hist(Argentina$home_score)

Histogram(x = away_score,
          data = Argentina,
          xlab = "# Goles Visitante", 
          ylab ="Cantidad", 
          main = "Argentina")

Histogram(x = home_score,
          data = Brasil,
          xlab = "# Goles Local", 
          ylab ="Cantidad", 
          main = "Brasil")

Histogram(x = away_score,
          data = Brasil,
          xlab = "# Goles Visitante", 
          ylab ="Cantidad", 
          main = "Brasil")

Histogram(x = home_score,data = datos)
Histogram(x = away_score,data = datos)

Argentina <- na.omit(Argentina)

## ESTIMACIÓN DEL PARÁMETRO LAMBDA EN POISSON
rriskMMEdist(na.omit(Argentina$home_score), "pois")
rriskMMEdist(Argentina$away_score, "pois")

## OTRA ALTERNATIVA DE AJUSTE PARAMÉTRICO

fit <- fitdist(Argentina$home_score,
               distr =  "pois",
               method = "mle")
summary(fit)

x11()
plot(fit)

fit2 <- fitdist(datos$home_score,
               distr =  "pois",
               method = "mle")
summary(fit2)
x11()
plot(fit2)

fit3 <- fitdist(Brasil$home_score,
               distr =  "pois",
               method = "mle")
summary(fit3)

x11()
plot(fit3)

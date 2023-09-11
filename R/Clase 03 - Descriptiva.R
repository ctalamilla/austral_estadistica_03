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

################################
####### SETEO DE CARPETA #######
################################

getwd()

path = "...."

setwd(path)

dir()

# Basado en https://www.princeton.edu/~otorres/sessions/
# http://www.princeton.edu/~otorres/sessions/s2r.pdf

# Leemos el archivo

download.file(url = "http://dss.princeton.edu/training/students.xls",
              destfile = paste0(path,"/students.xls"),
              method = "auto",
              quiet = FALSE,
              mode = "wb",
              cacheOK = TRUE)
dir()

## Importar el dataset como csv
datos <- read.csv2("students.csv",
                   header = T)

## Explorando los datos

summary(datos) # Suministra estad???stica descriptiva b???sica

edit(datos) # Abre el editor de datos

str(datos) # Suministra la estructura de los datos

names(datos) # Lista las variables

head(datos) # Primeras 6 filas

head(datos, n = 10) # Primeras 10 filas

head(datos, n= -10) # ???ltimas 10 filas

tail(datos) # ???ltimas 6 filas

tail(datos, n = 10) # ???ltimas 10 filas

tail(datos, n = -10) # ???ltimas 10 filas

datos[1:10, ] # Primeras 10 filas

datos[1:10,1:3] # Primeras 10 filas de las 3 primeras variables

# Informaci???n Faltante (Missing)

rowSums(is.na(datos)) # Cantidad de Missing por Filas

colSums(is.na(datos)) # Cantidad de Missing por Columnas

# Ejemplo de Conversi???n a Datos Faltantes

datos[datos$Age == 30,"Age"] <- NA

# Listar filas que tienen valores faltantes
datos[!complete.cases(datos),]

# Crear un nuevo dataset sin datos faltantes
datos1 <- na.omit(datos)

# Reemplazar un valor
datos1[datos1$SAT == 1787,"SAT"] <- 1800

datos1[datos1$Country == "Bulgaria","Country"] <- "US"

# Renombrar Variables

# Interactivamente
fix(datos) 

# Comando b???sico "names"
names(datos)[3] <- "First"

# Mediante paquete reshape
library(reshape)

datos <- rename(datos, c(Last.Name = "Last"))

datos <- rename(datos, c(First.Name = "First"))

datos <- rename(datos, c(Student.Status = "Status"))

datos <- rename(datos, c(Average.score..grade. = "Score"))

datos <- rename(datos, c(Height..in. = "Height"))

datos <- rename(datos, c(Newspaper.readership..times.wk. = "Read"))

names(datos)

# Crear secuencia de n???meros id
datos$id <- seq(dim(datos)[1])

# Crear una variable con el n???mero total de observaciones
datos$total <- dim(datos)[1]

# Recoding variables

library(car)
datos$Age.rec <- recode(datos$Age, "18:19='18to19';20:29='20to29';30:39='30to39'")
datos$Age.rec <- as.factor(datos$Age.rec)

# Estad???stica Descriptiva mediante paquete "pastecs"-
library(pastecs)

stat.desc(datos)

stat.desc(datos[,c("Age","SAT","Score","Height", "Read")])

stat.desc(datos[,c("Age","SAT","Score")], 
          basic=TRUE, 
          desc=TRUE, 
          norm=TRUE, 
          p=0.95)

stat.desc(datos[10:14], 
          basic = TRUE, 
          desc = TRUE, 
          norm = TRUE, 
          p = 0.95)

# Estad???stica Descriptiva

mean(datos)

mean(datos$SAT)

with(datos, mean(SAT))

median(datos$SAT)

var(datos$SAT)

sd(datos$SAT)

max(datos$SAT)

min(datos$SAT)

range(datos$SAT)

quantile(datos$SAT)

quantile(datos$SAT, c(.3,.6,.9))

fivenum(datos$SAT) 

length(datos$SAT)

length(datos)

which.max(datos$SAT)

which.min(datos$SAT)

# Modo por Frecuencias

table(datos$Country)

max(table(datos$Country))

names(sort(-table(datos$Country)))[1]

# Estad???stica Descriptiva mediante grupos usando tapply

mean <- tapply(datos$SAT,datos$Gender, mean)

sd <- tapply(datos$SAT,datos$Gender, sd)

median <- tapply(datos$SAT,datos$Gender, median)

max <- tapply(datos$SAT,datos$Gender, max)

cbind(mean, median, sd, max)

round(cbind(mean, median, sd, max),digits=1)

t1 <- round(cbind(mean, median, sd, max),digits=1)

t1

# Estad???stica Descriptiva por grupos mediante aggregate

aggregate(datos[c("Age","SAT")],by=list(sex=datos$Gender), mean, na.rm=TRUE)

aggregate(datos[c("Age","SAT")],datos["Gender"], mean, na.rm=TRUE)

aggregate(datos,by=list(sex=datos$Gender), mean, na.rm=TRUE)

aggregate(datos,
          by=list(sex = datos$Gender, 
                  major = datos$Major, 
                  status = datos$Status), 
          mean,
          na.rm=TRUE)

aggregate(datos$SAT,
          by=list(sex=datos$Gender, 
                  major=datos$Major, 
                  status=datos$Status), 
          mean,
          na.rm=TRUE)

aggregate(datos[c("SAT")],
          by=list(sex=datos$Gender, 
                  major=datos$Major, 
                  status=datos$Status),
          mean, 
          na.rm=TRUE)

# Histogramas

library(car)

head(Prestige)

hist(Prestige$income)

hist(Prestige$income, col="green")

with(Prestige, hist(income))

with(Prestige, hist(income, breaks="FD", col="green"))
box()

hist(Prestige$income, breaks="FD")

# Histogramas Condicionales

par(mfrow=c(1, 2))
hist(datos$SAT[datos$Gender=="Female"], breaks="FD", main="Female", xlab="SAT",col="green")
hist(datos$SAT[datos$Gender=="Male"], breaks="FD", main="Male", xlab="SAT", col="green")

# {} indican un comando compuesto que 
# permite varios comandos con el comando 'with'

par(mfrow=c(1, 1))

with(Prestige, {
  hist(income, breaks="FD", freq=FALSE, col="green")
  lines(density(income), lwd = 2)
  lines(density(income, adjust = 0.5),lwd = 1)
  rug(income)
})

# Histogramas Superpuesto

hist(datos$SAT, 
     breaks="FD", 
     col="green")

hist(datos$SAT[datos$Gender=="Male"], 
     breaks="FD", 
     col="gray", 
     add=TRUE)

legend("topright", c("Female","Male"), fill=c("green","gray"))

# Chequear
satgender <- table(datos$SAT,datos$Gender)
satgender

###############################
####### OBJETOS CREADOS #######
###############################

ls()   # los objetos que ha creado.     

objects() ## otra forma de listar todos los objetos creados

rm("nombre objeto") ## para eliminar un objeto creado

rm("nombre objeto 1","nombre objeto 2")

rm(list = ls(all = TRUE)) ## elimina todos los objetos creados
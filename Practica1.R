library(dplyr)
library(lubridate)
datos<-read.csv("CdP_estadistica.csv")
datos<- mutate(datos, year=year(date), month=month(date), yday=yday(date))
datos_verano14<-datos[(datos$month==7 | datos$month==8 | datos$month==9) & datos$year==2014,]
datos_verano15<-datos[(datos$month==7 | datos$month==8 | datos$month==9) & datos$year==2015,]
#Media
med14<-mean(datos_verano14$AVG.Temp.)
med15<-mean(datos_verano15$AVG.Temp.)
med14
med15
#Desviaciones Estándar
desv_est14<-sd(datos_verano14$AVG.Temp.)
desv_est15<-sd(datos_verano15$AVG.Temp.)
desv_est14
desv_est15
#Varianza
varianza14<-var(datos_verano14$AVG.Temp.)
varianza15<-var(datos_verano15$AVG.Temp.)
varianza14
varianza15
#Tests
mitest<-t.test(datos_verano14$AVG.Temp.,datos_verano15$AVG.Temp.)
#Como el p-value es muy cercano a cero, luego rechazo la hipótesis de que las dos
#distribuciones sean la misma
mivartest<-var.test(datos_verano14$AVG.Temp.,datos_verano15$AVG.Temp.)
#El p-value asociado a la varianza es cercano a 1, luego no puedo rechazar la hipotesis de que las
#dos distribuciones sean la misma por varianza.

#Histogramas
hist(datos_verano14$AVG.Temp.)
hist(datos_verano15$AVG.Temp.)

#Las dos distribuciones son distintas, sin embargo el comportamiento es muy similar

datos_verano<-datos[(datos$month==7 | datos$month==8 | datos$month==9),]
#Media
med<-mean(datos_verano$AVG.Temp.)
med
#Desviaciones Estándar
desv_est<-sd(datos_verano$AVG.Temp.)
desv_est
#Varianza
varianza<-var(datos_verano$AVG.Temp.)
varianza


dat1<-read.table("dat1.DAT")
dat2<-read.table("dat2.DAT")
dat11<-dat1[!is.na(dat1)]
dat22<-dat2[!is.na(dat2)]


#Media
mean(dat11)
mean(dat22)

#Desviacion estandar
sd(dat11)
sd(dat22)

#Varianza
var(dat11)
var(dat22)

testdat<-t.test(dat11,dat22)
#p.value=1 Por lo que no podemos descartar que sean diferentes aunque tampoco
#podemos garantizar que sean iguales.
vartestdat<-var.test(dat11,dat22)
#p. value nos sale cercano a uno por lo que no podemos garantizar que se trate de distribuciones
#diferentes , pero tampoco garantizar que sean iguales.

#Histogramas
hist(dat11)
hist(dat22)

#Aunque los test no descarten que las distribuciones sean distintas en el histograma
#podemos ver como en realidad las funciones son muy distintas.


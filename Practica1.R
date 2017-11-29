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
#dos distribuciones sean la misma por varianza
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

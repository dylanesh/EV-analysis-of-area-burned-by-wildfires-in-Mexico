library(dplyr)
library(tidyverse)
library(fs)
library(forecast)
library(tseries)
library(quantmod)
library(foreign)
library(lubridate)
library(astsa)
library(dynlm)
library(POT)
library(extRemes)

mx_inc<-read.csv("C:\\Users\\Dylan\\Downloads\\BD_Historico_IncendiosForestales_1970-2021.csv",header=F)

colnames(mx_inc)<-c("Año","Estado","No_de_incendios","Superficie_quemada","Región")

#BlockMaxima
#En esta parte tomamos el anual máximo de área quemada de todas las entidades
bmmx_inc<-blockmaxxer.data.frame(mx_inc,blen=32,span=52,which=4)
plot(mx_inc$Año, mx_inc$Superficie_quemada, xlab="Año", ylab="Área total quemada (Ha)",col="darkblue",bg="lightblue",pch=21)
points(bmmx_inc$Año,bmmx_inc$Superficie_quemada,col="darkred",bg="darkred",cex=2)

#Estimación de parámetros por MLE y comparar con distribución GEV
bm_evd<-fevd(as.vector(bmmx_inc$Superficie_quemada),method="MLE",type="GEV")
plot(bm_evd,type="density",main="probability density distribution",xlab="Área quemada (Ha)")

#Niveles de retorno para 2, 5, 10, 20, 50 y 100 años
return.level(bm_evd, conf = 0.05, return.period= c(2,5,10,20,50,100))

#POT method
#Mean residual life plot
POT::mrlplot(mx_inc$Superficie_quemada)
#Del gráfico anterior se toma el rango (35000,45000) para seleccionar el umbral
POT::mrlplot(mx_inc$Superficie_quemada,u.range=c(15000,25000))
#Estabilidad de parámetros
threshrange.plot(mx_inc$Superficie_quemada,r=c(15000,25000),type="GP",nint=20)
#Se selecciona u=40000
plot(mx_inc$Año, mx_inc$Superficie_quemada, xlab="Año", ylab="Área total quemada (Ha)",col="darkblue",bg="lightblue",pch=21)
abline(h=21000,col="red")

#Estimación de parámetros por MLE y comparar con distribución GPD
pot_evd<-fevd(mx_inc$Superficie_quemada,threshold=21000,method="MLE",type="GP")
plot(pot_evd,type="density",main="probability density distribution",xlab="Área quemada (Ha)")

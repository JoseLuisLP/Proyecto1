#             BASE DE DATOS DE LA INVERSIÓN MINERA EN PERÚ
#                    LIMPIEZA DE DATOS

#Librerias que vamos a usar
getwd()
library(readxl)
library(readr)
library(tidyverse)
library(dplyr)
library(tidyr)
library(knitr)
library(forcats)
library(ggplot2)
library(kableExtra)
library(rmarkdown)
library(markdown)
library(parcats) #
library(xtable) # exporta tablas en latex o html
library(lubridate) # manejador de fechas
rm(list = ls())

#Fijamos nuestro directorio de trabajo

setwd("D:/CURSOS_VIRTUALES/BEST/Sesion3/Proyecto")

INV2000_2019<-read_excel("INV2000-2019.xls", skip = 2, sheet = "Inversiones")
names(INV2000_2019)


INV2000_2019<-INV2000_2019[c(1:151),]

names(INV2000_2019)[2:13]="2000"
names(INV2000_2019)[15:26]="2001"
names(INV2000_2019)[28:39]="2002"
names(INV2000_2019)[41:52]="2003"
names(INV2000_2019)[54:65]="2004"
names(INV2000_2019)[67:78]="2005"
names(INV2000_2019)[80:91]="2006"
names(INV2000_2019)[93:104]="2007"
names(INV2000_2019)[106:117]="2008"
names(INV2000_2019)[119:130]="2009"
names(INV2000_2019)[132:143]="2010"
names(INV2000_2019)[145:156]="2011"
names(INV2000_2019)[158:169]="2012"
names(INV2000_2019)[171:182]="2013"
names(INV2000_2019)[184:195]="2014"
names(INV2000_2019)[197:208]="2015"
names(INV2000_2019)[210:221]="2016"
names(INV2000_2019)[223:234]="2017"
names(INV2000_2019)[236:247]="2018"
names(INV2000_2019)[249:256]="2019"



# Filtramos datos según rubros mineros

# 1) DESARROLLO Y PREPARACIÓN (1, 3:26), 2) EQUIPAMIENTO MINERO(28:51), 3)EXPLORACIÓN(53:76),
# 4)INFRAESTRUCTUR(78:101)A, 5) PLANTA DE BENEFICIO(103:125), 6) OTROS (127:150)

Rubro1<-INV2000_2019[c(3:26),] # seleccionamos el rubro I
names(Rubro1)[1]="Departamento" #Le asignamos un nombre a la primera columna
Rubro1[,-1]<-sapply(Rubro1[,-1],function(y) round(as.numeric(as.character(y)),2)) # convertimos los datos a numérico

# Y hacemos lo mismo para los 5 rubros mineros restantes

Rubro2<-INV2000_2019[c(28:51),]
names(Rubro2)[1]="Departamento"
Rubro2[,-1]<-sapply(Rubro2[,-1],function(y) round(as.numeric(as.character(y)),2))

Rubro3<-INV2000_2019[c(53:76),]
names(Rubro3)[1]="Departamento"
Rubro3[,-1]<-sapply(Rubro3[,-1],function(y) round(as.numeric(as.character(y)),2))

Rubro4<-INV2000_2019[c(78:101),]
names(Rubro4)[1]="Departamento"
Rubro4[,-1]<-sapply(Rubro4[,-1],function(y) round(as.numeric(as.character(y)),2))

Rubro5<-INV2000_2019[c(103:125),]
names(Rubro5)[1]="Departamento"
Rubro5[,-1]<-sapply(Rubro5[,-1],function(y) round(as.numeric(as.character(y)),2))

Rubro6<-INV2000_2019[c(127:150),]
names(Rubro6)[1]="Departamento" 
Rubro6[,-1]<-sapply(Rubro6[,-1],function(y) round(as.numeric(as.character(y)),2))


sapply(Rubro1,class)

#Estructuramos la data del rubro 1, convirtiendo en variables únicas

INVMINA1<-Rubro1%>%
  pivot_longer(-Departamento, names_to = "Año", values_to = "Inversión") 
INVMINA1$Rubro="Desarrollo y preparación" #Agregamos una variable que identifique el rubro

INVMINA2<-Rubro2%>%
  pivot_longer(-Departamento,names_to = "Año", values_to = "Inversión")
INVMINA2$Rubro="Equipamiento Minero"

INVMINA3<-Rubro3%>%
  pivot_longer(-Departamento,names_to = "Año", values_to = "Inversión")
INVMINA3$Rubro="Exploración"

INVMINA4<-Rubro4%>%
  pivot_longer(-Departamento,names_to = "Año", values_to = "Inversión")
INVMINA4$Rubro="Infraestructura"

INVMINA5<-Rubro5%>%
  pivot_longer(-Departamento,names_to = "Año", values_to = "Inversión")
INVMINA5$Rubro="Planta de beneficio"

INVMINA6<-Rubro6%>%
  pivot_longer(-Departamento,names_to = "Año", values_to = "Inversión")
INVMINA6$Rubro="Otros"

#Juntamos la base de datos con la funcion rbind

DATAFINAL<-rbind(INVMINA1,INVMINA2,INVMINA3,INVMINA4,INVMINA5,INVMINA6)
(5*6120)+5865
sapply(DATAFINAL,class)

#Cambiamos el nombre de la columna 3

names(DATAFINAL)[3]="mes"

#Generamos una lista que no deseamos tener en nuestra base de datos

Filtro<-c("Total 2000", "Total 2001","Total 2002", "Total 2003","Total 2004", "Total 2005", "Total 2006",
          "Total 2007","Total 2008","Total 2009","Total 2010", "Total 2011",
          "Total 2012", "Total 2013", "Total 2014", "Total 2015","Total 2016","Total 2017","Total 2018")

#Filtrar todas excepto Año=="Total*"

DATAFINAL<-DATAFINAL%>%
  filter(!Año %in% Filtro)

sapply(DATAFINAL,class) # verificamos la estuctura de los datos

unique(DATAFINAL$Año)

#Convertimos las columnas 2 y 3 en tipo numeric

DATAFINAL$Año<-sapply(DATAFINAL$Año, function(x) as.numeric(as.character(x)))
DATAFINAL$Mes<-sapply(DATAFINAL$Mes, function(y) as.numeric(as.integer(y)))

#Para una mejor visualización le ponemos los nombres de los meses
#Creamos una variable llamado Mes
DATAFINAL$Mes<-"mes"
DATAFINAL$Mes<-DATAFINAL$Mes%>%
  
Meses<-c("Enero","Febrero","Marzo","Abril","Mayo","Junio","Julio",
         "Agosto","septiembre","Octubre","Noviembre","Diciembre")

DATAFINAL<-DATAFINAL%>%
  mutate(Mes=replace(Mes, Mes %in% c(1:12), Meses))

sapply(DATAFINAL,class)
sum(is.na(DATAFINAL$Inversión))

#CREAMOS TABLAS POR GRUPO

INV_DEPA<-DATAFINAL%>%
  group_by(Departamento,Año)%>%
  summarise(Total=sum(Inversión))%>%
  arrange(Año)

INV_DEPA<-INV_DEPA%>%
  filter(!Total=="NA")

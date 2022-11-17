---
title: "Report final"
output: html_document
author: "Diego Reguera"
date: "2022-11-15"
---

##Instalación de paquetes



install.packages("tidyverse")
install.packages("ggplot2")
install.packages("readxl")



library(ggplot2)
library("readxl")
library(dplyr)
library(scales)
library(tidyverse)



#Importación Xlsx usando readlxl


Datos_Sectores <- read_excel("Sexo y sectores de actividad, selecionadas.xlsx")


#Filtrado de datos


df_w_comunidades <- Datos_Sectores
df_w_comunidades1 <- filter(df_w_comunidades,`Comunidades autónomas` %in% c("Madrid, Comunidad de", "Cataluña", "Comunitat Valenciana"))
df_w_comunidades2 <- filter(df_w_comunidades1,`Sectores de actividad` %in% c("Todos los sectores"))
df_w_comunidades3 <- filter(df_w_comunidades2,Sexo %in% c("Ambos sexos"))
Datos_SectoresNac <- Datos_Sectores %>% filter(`Comunidades autónomas` == "Cataluña")

Datos_Sectores_Nac_Ambos <- Datos_SectoresNac %>%
  slice(1:13)
Datos_Sectores_Nac_Mujer <- Datos_SectoresNac %>%
  slice(14:26)
Datos_Sectores_Nac_Hombre <- Datos_SectoresNac %>%
  slice(27:39)
Datos_SectoresNac4 <- Datos_SectoresNac %>%
  slice(1:39)
Datos_SectoresMad <- Datos_Sectores %>% filter(`Comunidades autónomas` == "Madrid, Comunidad de")
Datos_Sectores_Mad_Ambos <- Datos_SectoresNac %>%
  slice(1:13)
Datos_SectoresMad <- Datos_Sectores %>% filter(`Comunidades autónomas` == "Madrid, Comunidad de", "Cataluña", "Comunitat Valenciana")



#Visualización del gráfico de Cataluña


Datos_Sectores_Nac_Ambos %>%
  ggplot(aes( x= Periodo, y = Total, label = Total)) +
  geom_line(color = "blue", linewidth = 3)+
  theme(axis.text.x = element_text (size = 12, angle =95)) + labs (x= "Años", y = "Euros") +
          geom_point(color = "blue", size = 4)+
  labs(subtitle="Evolución salarial en Cataluña", x= "Año", y = "Euros", caption = "Fuente: Elaboración propia con datos del INE" )


#Visualización del gráfico de todas las comunidades


ggplot(df_w_comunidades3, aes(x= Periodo, y = Total, group=`Comunidades autónomas`))+ 
  geom_line(aes(color=`Comunidades autónomas`)) +
    theme_classic() +
  labs(subtitle ="Evolución salario bruto por comunidades autónomas", x= "Año", y = "Euros", caption = "Fuente: Elaboración propia con datos del INE") 
  theme(legend.position = "top")



## Sectores por género

#Cargando los paquetes de archivos

#Se procede a filtrar datos esta vez por fecha 2020


Datos_SectoresNacional <- Datos_Sectores %>% filter(`Comunidades autónomas` == "Total Nacional")
Datos_SectoresNacional_2020 <- Datos_SectoresNacional %>% filter(`Periodo` == "2020")



#Se junta o "concatenan" dos columnas que contienen valores string



Datos_SectoresNacional_2020$Sector_Sexo <- paste(Datos_SectoresNacional_2020$`Sectores de actividad`, Datos_SectoresNacional_2020$Sexo)


#Se procede a la visualización añadiendo la función "identity" para evitar errores



Datos_SectoresNacional_2020 %>%
ggplot(aes(x = Sector_Sexo, y = Total, label = Total)) +
  coord_flip()+
  geom_bar( fill ="#97B3C6", stat='identity', width=0.4) +
  theme(aspect.ratio = 2/1)+
    theme_bw() +
  labs(title="Salarios por sector económico y género en 2020", x= "Sector económico por género", y = "Euros", caption = "Fuente: Elaboración propia con datos del INE" )



##Variación salarial Nacional y por comunidades

#Plot para filtrar datos


df_var <- Sexo_y_sectores_de_actividad_var

df_var_Nac <- filter(df_var,`Comunidades autónomas` %in% c("Total Nacional"))
df_var_Nac1 <- filter(df_var_Nac,`Sectores de actividad` %in% c("Todos los sectores"))
df_var_Nac2 <- filter(df_var_Nac1,Sexo %in% c("Mujeres", "Hombres"))

df_var_Cat <- filter(df_var,`Comunidades autónomas` %in% c("Cataluña"))
df_var_cat1 <- filter(df_var_Cat,`Sectores de actividad` %in% c("Todos los sectores"))
df_var_cat2 <- df_var_cat1 %>%
  slice(13:36)

df_var_Val <- filter(df_var,`Comunidades autónomas` %in% c("Comunitat Valenciana"))
df_var_Val1 <- filter(df_var_Val,`Sectores de actividad` %in% c("Todos los sectores"))
df_var_Val2 <- filter(df_var_Val1,Sexo %in% c("Mujeres", "Hombres"))

df_var_Mad <- filter(df_var,`Comunidades autónomas` %in% c("Madrid, Comunidad de"))
df_var_Mad1 <- filter(df_var_Mad,`Sectores de actividad` %in% c("Todos los sectores"))
df_var_Mad2 <- filter(df_var_Mad1,Sexo %in% c("Mujeres", "Hombres"))


#Se junta o "concatenan" dos columnas que contienen valores string



df_var$Sector_Sexo <- paste(df_var$`Sectores de actividad`, df_var$Sexo)


Datos_Sectores_Nac_Ambos <- Datos_SectoresNac %>%
  slice(1:13)
Datos_Sectores_Nac_Mujer <- Datos_SectoresNac %>%
  slice(14:26)
Datos_Sectores_Nac_Hombre <- Datos_SectoresNac %>%
  slice(27:39)
Datos_SectoresNac4 <- Datos_SectoresNac %>%
  slice(1:39)
Datos_SectoresMad <- Datos_Sectores %>% filter(`Comunidades autónomas` == "Madrid, Comunidad de")
Datos_Sectores_Mad_Ambos <- Datos_SectoresNac %>%
  slice(1:13)
Datos_SectoresMad <- Datos_Sectores %>% filter(`Comunidades autónomas` == "Madrid, Comunidad de", "Cataluña", "Comunitat Valenciana")




#Visualización del gráfico de todas las comunidades


ggplot(df_var_Nac2, aes(x= Periodo, y = `Tasa_variación&Media2008`, group= Sexo))+ 
  geom_line(aes(color=`Sexo`)) +
  theme_classic()+
  geom_point()+
  geom_point(color = "#97B3C6", size = 4)+
  labs(subtitle="Evolución Nacional salarios por género, 2008-2020", x= "Salarios por género", y = "Porcentaje", caption = "Fuente: Elaboración propia con datos del INE" )

ggplot(df_var_cat2, aes(x= Periodo, y = `Tasa_variación&Media2008`, group= Sexo))+ 
  geom_line(aes(color=`Sexo`)) +
  theme_classic()+
  geom_point()+
  geom_point(color = "#d46157", size = 4)+
  labs(subtitle="Evolución salarial por género en Cataluña, 2008-2020", x= "Salarios por género", y = "Porcentaje", caption = "Fuente: Elaboración propia con datos del INE" )

ggplot(df_var_Val2, aes(x= Periodo, y = `Tasa_variación&Media2008`, group= Sexo))+ 
  geom_line(aes(color=`Sexo`)) +
  theme_classic()+
  geom_point()+
  geom_point(color = "#65bf74", size = 4)+
  labs(subtitle="Evolución salarial por género en C. Valenciana, 2008-2020", x= "Salarios por género", y = "Porcentaje", caption = "Fuente: Elaboración propia con datos del INE" )

ggplot(df_var_Mad2, aes(x= Periodo, y = `Tasa_variación&Media2008`, group= Sexo))+ 
  geom_line(aes(color=`Sexo`)) +
  theme_classic()+
  geom_point()+
  geom_point(color = "#5574f2", size = 4)+
  labs(subtitle="Evolución Nacional salarial por género en C. de Madrid, 2008-2020", x= "Salarios por género", y = "Euros", caption = "Fuente: Elaboración propia con datos del INE" )



##Crecimiento medio Nacional y por comunidades autónomas

#Se junta o "concatenan" dos columnas que contienen valores string


df_medias <- Sexo_y_sectores_de_actividad_medias1
df_medias$Sector_Sexo <- paste(df_medias$`Sectores de actividad`, df_medias$Sexo)



df_medias_Nac <- df_medias %>%
  slice(1:6)
df_medias_Cat <- df_medias %>%
  slice(7:12)
df_medias_Val <- df_medias %>%
  slice(13:18)
df_medias_Mad <- df_medias %>%
  slice(19:25)


df_medias_Nac %>%
  ggplot(aes(x = Sector_Sexo, y = Crecimiento_medio, label = Crecimiento_medio)) +
  coord_flip()+
  geom_bar(fill = "#97B3C6", stat='identity') +
  theme_bw() +
  labs(subtitle = "Crecimiento salarial medio por sector y género Nacional, 2008-2020", x= "Sectores y género en Nacional", y = "Porcentaje", caption = "Fuente: Elaboración propia con datos del INE" )

df_medias_Cat %>%
  ggplot(aes(x = Sector_Sexo, y = Crecimiento_medio, label = Crecimiento_medio)) +
  coord_flip()+
  geom_bar(fill = "#d46157", stat='identity') +
  theme_bw() +
  labs(subtitle = "Crecimiento salarial medio por sector y género Cataluña, 2008-2020", x= "Sectores y género en Cataluña", y = "Porcentaje", caption = "Fuente: Elaboración propia con datos del INE" )

df_medias_Val %>%
  ggplot(aes(x = Sector_Sexo, y = Crecimiento_medio, label = Crecimiento_medio)) +
  coord_flip()+
  geom_bar(fill = "#65bf74", stat='identity') +
  theme_bw() +
  labs(subtitle = "Crecimiento salarial medio por sector y género C. de Valencia, 2008-2020", x= "Sectores y género en C. de Valencia", y = "Porcentaje", caption = "Fuente: Elaboración propia con datos del INE" )

df_medias_Mad %>%
  ggplot(aes(x = Sector_Sexo, y = Crecimiento_medio, label = Crecimiento_medio)) +
  coord_flip()+
  geom_bar(fill = "#5574f2", stat='identity') +
  theme_bw() +
  labs(subtitle = "Crecimiento salarial medio por sector y género en C. de Madrid, 2008-2020", x= "Sectores y género en C. de Madrid", y = "Porcentaje", caption = "Fuente: Elaboración propia con datos del INE" )




## Relación de divorcios y salarios


#Creación del primer plot y su viualización 

plot(df_w_d$Periodo,
     df_w_d$Total_var_Divor,
         col = "blue",
     xlab = "Año",
     ylab = "Porcentaje")

#Se añaden los datos de la segunda variable y su visalización

lines(df_w_d$Periodo,
     df_w_d$Total_var_w,
     col = 3,
     xlab = "Año",
     ylab = "Porcentaje")

#Se añade una leyenda del gráfico en la ubicación deseada que indiquen los nombres de los valores

legend("bottomleft", inset=.02, title="Relación entre variables salarios y divorcios",
       c("Tasa de divorcio","Variación salarial"), fill=topo.colors(3), horiz=TRUE, cex=0.8)


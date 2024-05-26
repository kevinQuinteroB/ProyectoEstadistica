library(lsr)
library(moments)
library(readr)
library(dplyr)

#Leemos el archivo ubicado en el github del proyecto
data <- read.csv("https://raw.githubusercontent.com/kevinQuinteroB/ProyectoEstadistica/main/Delitos_ocurridos_en_el_Municipio_de_Bucaramanga_20240519.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)

#Buscamos los campos que se encuentran vacios
latitud_vacia <- data %>% filter(LATITUD == "xx.xxxx")
longitud_vacia <- data %>% filter(LONGITUD == "-yy.yyyy")
filas_vacias <- data %>% filter(CURSO_DE_VIDA == "ERROR: #N/A")

data_clean <- data %>% filter(LATITUD != "xx.xxxx" & CURSO_DE_VIDA != "ERROR: #N/A")



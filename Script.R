library(lsr)
library(moments)
library(readr)
library(dplyr)
library(ggplot2)

#Leemos el archivo ubicado en el github del proyecto
data <- read.csv("https://raw.githubusercontent.com/kevinQuinteroB/ProyectoEstadistica/main/Delitos_ocurridos_en_el_Municipio_de_Bucaramanga_20240519.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)

#Buscamos los campos que se encuentran vacios 
latitud_vacia <- data %>% filter(LATITUD == "xx.xxxx")  # Para la location
longitud_vacia <- data %>% filter(LONGITUD == "-yy.yyyy") # Para la location
filas_vacias <- data %>% filter(CURSO_DE_VIDA == "ERROR: #N/A")

data_clean <- data %>% filter(CURSO_DE_VIDA != "ERROR: #N/A")

#1. Cuales son los mecanismos mas utilizados por los delincutes para realizar sus actos delictivos?
data_delincuentes <- data_clean %>% filter(CONDUCTA != "LESIONES CULPOSAS ( EN ACCIDENTE DE TRANSITO )" & CONDUCTA != "HOMICIDIO CULPOSO ( EN ACCIDENTE DE TRÁNSITO)")

  # 1.1 Crear el grafico de torta para el tipo de Arma del agresor
Datos_Armas <- data_delincuentes$ARMAS_MEDIOS 
tabla_frecuencias_armas <- table(Datos_Armas)
df_frecuencias_armas <- as.data.frame(tabla_frecuencias_armas)
df_frecuencias_armas_pequeño <- df_frecuencias_armas %>% filter(Freq <= 1500)
otros_frecuencia_armas_agresor <- sum(df_frecuencias_armas_pequeño$Freq)
df_otros_armas_agresor <- data.frame(Datos_Armas = "Otros", Freq = otros_frecuencia_armas_agresor)
df_frecuencias_armas_grande <- df_frecuencias_armas %>% filter(Freq >= 1500)
df_frecuencias_armas_grande <- bind_rows(df_frecuencias_armas_grande, df_otros_armas_agresor)

ggplot(df_frecuencias_armas_grande, aes(x = "", y = Freq, fill = Datos_Armas)) +
  geom_bar(stat = "identity", width = 1, color = "gray") +
  coord_polar("y", start = 0) +
  scale_fill_brewer(palette = "Set3") +
  labs(title = "Distribución de Armas Utilizados por Delincuentes",
       fill = "Armas") +
  theme_void() +
  theme(legend.position = "right") +
  geom_text(aes(label = paste0(round(Freq / sum(Freq) * 100, 1), "%")),
            position = position_stack(vjust = 0.5), color = "black", size = 3.5)


  # 1.2 Crear el grafico de torta para el tipo de vehiculos del agresor
Datos_Vehiculos_Delincuentes <- data_delincuentes$MOVIL_AGRESOR
tabla_frecuencias_vehiculos <- table(Datos_Vehiculos_Delincuentes)
df_frecuencias_vehiculos_agresor <- as.data.frame(tabla_frecuencias_vehiculos)
df_frecuencias_vehiculos_agresor_pequeño <- df_frecuencias_vehiculos_agresor %>% filter(Freq <= 2000)
otros_frecuencia_moviles_agresor <- sum(df_frecuencias_vehiculos_agresor_pequeño$Freq)
df_otros_moviles_agresor <- data.frame(Datos_Vehiculos_Delincuentes = "Otros", Freq = otros_frecuencia_moviles_agresor)
df_frecuencias_vehiculos_agresor_grande <- df_frecuencias_vehiculos_agresor %>% filter(Freq >= 2000)
df_frecuencias_vehiculos_agresor_grande <- bind_rows(df_frecuencias_vehiculos_agresor_grande, df_otros_moviles_agresor)

ggplot(df_frecuencias_vehiculos_agresor_grande, aes(x = "", y = Freq, fill = Datos_Vehiculos_Delincuentes)) +
  geom_bar(stat = "identity", width = 1, color = "gray") +
  coord_polar("y", start = 0) +
  scale_fill_brewer(palette = "Set3") +
  labs(title = "Distribución de Vehículos Utilizados por Delincuentes",
       fill = "Vehículo") +
  theme_void() +
  theme(legend.position = "right") +
  geom_text(aes(label = paste0(round(Freq / sum(Freq) * 100, 1), "%")),
            position = position_stack(vjust = 0.5), color = "black", size = 3.5)



  # 1.3 Crear el gráfico de torta para la combinación de ARMAS_MEDIOS y MOVIL_AGRESOR con mejor formato
tabla_frecuencias_cruzadas_agresores <- table(data_delincuentes$ARMAS_MEDIOS, data_delincuentes$MOVIL_AGRESOR)
df_frecuencias_cruzadas_agresores <- as.data.frame(tabla_frecuencias_cruzadas_agresores)
colnames(df_frecuencias_cruzadas_agresores) <- c("ARMAS_MEDIOS", "MOVIL_AGRESOR", "FRECUENCIA")
df_frecuencias_cruzadas_agresores <- df_frecuencias_cruzadas_agresores %>% mutate(ARMAS_MOVIL = paste(ARMAS_MEDIOS, MOVIL_AGRESOR, sep = " - "))
df_armas_moviles_agresor <- df_frecuencias_cruzadas_agresores %>% group_by(ARMAS_MOVIL) %>% summarise(FRECUENCIA = sum(FRECUENCIA))
df_armas_moviles_agresor_pequeños <- df_armas_moviles_agresor %>% filter(FRECUENCIA <= 1220)
otros_frecuencia_armas_moviles_agresor <- sum(df_armas_moviles_agresor_pequeños$FRECUENCIA)
df_otros_armas_moviles_agresor <- data.frame(ARMAS_MOVIL = "Otros", FRECUENCIA = otros_frecuencia_armas_moviles_agresor)
df_armas_moviles_agresor_clean <- df_armas_moviles_agresor %>% filter(FRECUENCIA >= 1220)
df_armas_moviles_agresor_clean <- bind_rows(df_armas_moviles_agresor_clean, df_otros_armas_moviles_agresor)

ggplot(df_armas_moviles_agresor_clean, aes(x = "", y = FRECUENCIA, fill = ARMAS_MOVIL)) +
  geom_bar(stat = "identity", width = 1, color = "gray") +
  coord_polar("y", start = 0) +
  scale_fill_brewer(palette = "Set3") +
  labs(title = "Distribución de Combinaciones de Armas y Vehículos Utilizados por Delincuentes",
       fill = "Arma - Vehículo") +
  theme_void() +
  theme(legend.position = "right") +
  geom_text(aes(label = paste0(round(FRECUENCIA / sum(FRECUENCIA) * 100, 1), "%")),
            position = position_stack(vjust = 0.5), color = "black", size = 3.5)

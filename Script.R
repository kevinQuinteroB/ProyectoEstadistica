library(lsr)
library(moments)
library(readr)
library(dplyr)
library(ggplot2)
library(sf)

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

#2. Cuales son los sectores en los cuales se concentra una mayor cantidad de criminalidad
tabla_frecuencias_barrios <- table(data_delincuentes$BARRIOS_HECHO)
df_frecuencias_barrios <- as.data.frame(tabla_frecuencias_barrios)
colnames(df_frecuencias_barrios) <- c("BARRIO", "FRECUENCIA")

df_frecuencias_barrios_pequeño <- df_frecuencias_barrios %>% filter(FRECUENCIA <= 1746)
otros_frecuencias_barrios_pequeño <- sum(df_frecuencias_barrios_pequeño$FRECUENCIA)
df_frecuencias_barrios_pequeño <- data.frame(BARRIO = "Otros", FRECUENCIA = otros_frecuencias_barrios_pequeño)
df_frecuencias_barrios_grande <- df_frecuencias_barrios %>% filter(FRECUENCIA >= 1746)
df_frecuencias_barrios_final  <- bind_rows(df_frecuencias_barrios_grande, df_frecuencias_barrios_pequeño)

ggplot(df_frecuencias_barrios_grande, aes(x = reorder(BARRIO, -FRECUENCIA), y = FRECUENCIA, fill = BARRIO)) +
  geom_bar(stat = "identity", color = "black") +
  scale_fill_brewer(palette = "Set3") +
  labs(title = "Frecuencia de Delitos por Barrio",
       x = "Barrio",
       y = "Frecuencia",
       fill = "Barrio") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_text(aes(label = FRECUENCIA), vjust = -0.5, color = "black", size = 3.5)

#3. Como van los indices de delitos segun cada año?
tabla_frecuencias_delitos_año <- table(data_delincuentes$ANO, data_delincuentes$CONDUCTA)
df_frecuencias_delitos_año <- as.data.frame(tabla_frecuencias_delitos_año)
  # 3.1 Casos de violencia Sexual
df_frecuencias_delitos_año_ACCESO_CARNAL_ABUSIVO_CON_MENOR_DE_14_AÑOS <- df_frecuencias_delitos_año %>% filter(Freq > 0 & Var2 == "ACCESO CARNAL ABUSIVO CON MENOR DE 14 AÑOS")
df_frecuencias_delitos_año_ACCESO_CARNAL_VIOLENTO <- df_frecuencias_delitos_año %>% filter(Freq > 0 & Var2 == "ACCESO CARNAL VIOLENTO")
df_frecuencias_delitos_año_ACOSO_SEXUAL <- df_frecuencias_delitos_año %>% filter(Freq > 0 & Var2 == "ACOSO SEXUAL")
df_frecuencias_delitos_año_ACTOS_SEXUALES_CON_MENOR_DE_14_AÑOSL <- df_frecuencias_delitos_año %>% filter(Freq > 0 & Var2 == "ACTOS SEXUALES CON MENOR DE 14 AÑOS")
df_frecuencias_delitos_año_sexuales <- bind_rows(df_frecuencias_delitos_año_ACTOS_SEXUALES_CON_MENOR_DE_14_AÑOSL, df_frecuencias_delitos_año_ACOSO_SEXUAL, df_frecuencias_delitos_año_ACCESO_CARNAL_VIOLENTO, df_frecuencias_delitos_año_ACCESO_CARNAL_ABUSIVO_CON_MENOR_DE_14_AÑOS)

ggplot(df_frecuencias_delitos_año_sexuales, aes(x = Var1, y = Freq, color = Var2, group = Var2)) +
  geom_line() +        
  geom_point() +        
  labs(title = "Tendencia de Delitos sexuales a lo Largo del Tiempo",
       x = "Fecha",
       y = "Número de Delitos",
       color = "Tipo de Delito") +
  theme_minimal() +
  theme(
    panel.grid.major = element_line(color = "grey80"),
    panel.grid.minor = element_line(color = "grey90"),
    panel.background = element_rect(fill = "white", color = "grey50")
  )

  #3.2 Casos de violencia
df_frecuencias_delitos_año_EXTORSIÓN <- df_frecuencias_delitos_año %>% filter(Freq > 0 & Var2 == "EXTORSIÓN")
df_frecuencias_delitos_año_FEMINICIDIO <- df_frecuencias_delitos_año %>% filter(Freq > 0 & Var2 == "FEMINICIDIO")
df_frecuencias_delitos_año_HOMICIDIO <- df_frecuencias_delitos_año %>% filter(Freq > 0 & Var2 == "HOMICIDIO")
df_frecuencias_delitos_año_LESIONES_PERSONALES <- df_frecuencias_delitos_año %>% filter(Freq > 0 & Var2 == "LESIONES PERSONALES")
df_frecuencias_delitos_año_VIOLENCIA_INTRAFAMILIAR <- df_frecuencias_delitos_año %>% filter(Freq > 0 & Var2 == "VIOLENCIA INTRAFAMILIAR")
df_frecuencias_delitos_año_violencia <- bind_rows(df_frecuencias_delitos_año_VIOLENCIA_INTRAFAMILIAR, df_frecuencias_delitos_año_LESIONES_PERSONALES, df_frecuencias_delitos_año_HOMICIDIO, df_frecuencias_delitos_año_FEMINICIDIO, df_frecuencias_delitos_año_EXTORSIÓN)

ggplot(df_frecuencias_delitos_año_violencia, aes(x = Var1, y = Freq, color = Var2, group = Var2)) +
  geom_line() +        
  geom_point() +        
  labs(title = "Tendencia de Delitos de violencia a lo Largo del Tiempo",
       x = "Fecha",
       y = "Número de Delitos",
       color = "Tipo de Delito") +
  theme_minimal() +
  theme(
    panel.grid.major = element_line(color = "grey80"),
    panel.grid.minor = element_line(color = "grey90"),
    panel.background = element_rect(fill = "white", color = "grey50")
  )


  #3.3 Casos de robos
df_frecuencias_delitos_año_HURTO_A_AUTOMOTORES <- df_frecuencias_delitos_año %>% filter(Freq > 0 & Var2 == "HURTO A AUTOMOTORES")
df_frecuencias_delitos_año_HURTO_A_ENTIDADES_COMERCIALES <- df_frecuencias_delitos_año %>% filter(Freq > 0 & Var2 == "HURTO A ENTIDADES COMERCIALES")
df_frecuencias_delitos_año_HURTO_A_MOTOCICLETAS <- df_frecuencias_delitos_año %>% filter(Freq > 0 & Var2 == "HURTO A MOTOCICLETAS")
df_frecuencias_delitos_año_HURTO_A_PERSONAS <- df_frecuencias_delitos_año %>% filter(Freq > 0 & Var2 == "HURTO A PERSONAS")
df_frecuencias_delitos_año_HURTO_A_RESIDENCIAS <- df_frecuencias_delitos_año %>% filter(Freq > 0 & Var2 == "HURTO A RESIDENCIAS")
df_frecuencias_delitos_año_hurto <- bind_rows(df_frecuencias_delitos_año_HURTO_A_RESIDENCIAS, df_frecuencias_delitos_año_HURTO_A_PERSONAS, df_frecuencias_delitos_año_HURTO_A_MOTOCICLETAS, df_frecuencias_delitos_año_HURTO_A_ENTIDADES_COMERCIALES, df_frecuencias_delitos_año_HURTO_A_AUTOMOTORES)

ggplot(df_frecuencias_delitos_año_hurto, aes(x = Var1, y = Freq, color = Var2, group = Var2)) +
  geom_line() +        
  geom_point() +        
  labs(title = "Tendencia de Delitos de Hurto a lo Largo del Tiempo",
       x = "Fecha",
       y = "Número de Delitos",
       color = "Tipo de Delito") +
  theme_minimal() +
  theme(
    panel.grid.major = element_line(color = "grey80"),
    panel.grid.minor = element_line(color = "grey90"),
    panel.background = element_rect(fill = "white", color = "grey50")
  )

# 4 Cuales son los indices de las personas mas afectadas en los delitos?
  #4.1 Curso de vida de los afectados 
tabla_frecuencias_victimas_cursoVida <- table(data_delincuentes$CURSO_DE_VIDA)
df_frecuencias_victimas_cursoVida <- as.data.frame(tabla_frecuencias_victimas_cursoVida)

df_frecuencias_victimas_cursoVida <- df_frecuencias_victimas_cursoVida %>%
  mutate(Percentage = paste0(round(Freq / sum(Freq) * 100, 1), "%"),
         Label = paste(Var1, "(", Percentage, ")"))

ggplot(df_frecuencias_victimas_cursoVida, aes(x = "", y = Freq, fill = Label)) +
  geom_bar(stat = "identity", width = 1, color = "gray") +
  coord_polar("y", start = 0) +
  scale_fill_brewer(palette = "Set3", labels = df_frecuencias_victimas_cursoVida$Label) +
  labs(title = "Curso de vida de las victimas",
       fill = "Curso de vida") +
  theme_void() +
  theme(legend.position = "right")

  # 4.2 Comparacion de generos
tabla_frecuencias_victimas_genero <- table(data_delincuentes$GENERO)
df_frecuencias_victimas_genero <- as.data.frame(tabla_frecuencias_victimas_genero)

df_frecuencias_victimas_genero <- df_frecuencias_victimas_genero %>%
  mutate(Percentage = paste0(round(Freq / sum(Freq) * 100, 1), "%"),
         Label = paste(Var1, "(", Percentage, ")"))

ggplot(df_frecuencias_victimas_genero, aes(x = "", y = Freq, fill = Label)) +
  geom_bar(stat = "identity", width = 1, color = "gray") +
  coord_polar("y", start = 0) +
  scale_fill_brewer(palette = "Set3", labels = df_frecuencias_victimas_genero$Label) +
  labs(title = "Genero de las victimas",
       fill = "Genero") +
  theme_void() +
  theme(legend.position = "right")

  # Muertes y lesiones segun genero
tabla_frecuencias_victimas <- table(data_delincuentes$CLASIFICACIONES.DELITO, data_delincuentes$GENERO)
df_frecuencias_victimas <- as.data.frame(tabla_frecuencias_victimas)
df_frecuencias_victimas_mujeres <- df_frecuencias_victimas %>% filter(Var2 == "FEMENINO")
df_frecuencias_victimas_mujeres <- df_frecuencias_victimas_mujeres %>%
  mutate(Percentage = paste0(round(Freq / sum(Freq) * 100, 1), "%"),
         Label = paste(Var1, "(", Percentage, ")"))

ggplot(df_frecuencias_victimas_mujeres, aes(x = "", y = Freq, fill = Label)) +
  geom_bar(stat = "identity", width = 1, color = "gray") +
  coord_polar("y", start = 0) +
  scale_fill_brewer(palette = "Set3", labels = df_frecuencias_victimas_mujeres$Label) +
  labs(title = "Clasificacion del estado de las victimas mujeres",
       fill = "Clasificacion") +
  theme_void() +
  theme(legend.position = "right")


df_frecuencias_victimas_hombres <- df_frecuencias_victimas %>% filter(Var2 == "MASCULINO")
df_frecuencias_victimas_hombres <- df_frecuencias_victimas_hombres %>%
  mutate(Percentage = paste0(round(Freq / sum(Freq) * 100, 1), "%"),
         Label = paste(Var1, "(", Percentage, ")"))

ggplot(df_frecuencias_victimas_hombres, aes(x = "", y = Freq, fill = Label)) +
  geom_bar(stat = "identity", width = 1, color = "gray") +
  coord_polar("y", start = 0) +
  scale_fill_brewer(palette = "Set3", labels = df_frecuencias_victimas_hombres$Label) +
  labs(title = "Clasificacion del estado de las victimas hombres",
       fill = "Clasificacion") +
  theme_void() +
  theme(legend.position = "right")

# 5.1 Cuales son los dias en los cuales se presenta el mayor indice de criminalidad
tabla_frecuencias_dia <- table(data_delincuentes$DIA_SEMANA)
df_frecuencias_dia <- as.data.frame(tabla_frecuencias_dia)

ggplot(df_frecuencias_dia, aes(x = Var1, y = Freq, fill = Var1)) +
  geom_bar(stat = "identity", color = "black") +
  geom_text(aes(label = Freq), vjust = -0.5, size = 3.5, color = "black") +
  labs(title = "Frecuencia de delitos segun el dia de la semana",
       x = "Dia de la semana",
       y = "Frecuencia",
       fill = "Dia de la semana") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set3") +
  theme(
    panel.grid.major = element_line(color = "grey80"),
    panel.grid.minor = element_line(color = "grey90"),
    panel.background = element_rect(fill = "white", color = "grey50"),
    legend.position = "none"
  )

  # Cual es el promedio de asesinatos por dia en los ultimos 5 años registrados?
tabla_frecuencias_promedio_asesinatos <- table(data_delincuentes$DIA_SEMANA, data_delincuentes$ANO, data_delincuentes$CLASIFICACIONES.DELITO)
df_frecuencias_promedio_asesinatos <- as.data.frame(tabla_frecuencias_promedio_asesinatos)
df_frecuencias_promedio_asesinatos <- df_frecuencias_promedio_asesinatos %>% filter (Var3 == "LESIONES FATALES")
df_frecuencias_promedio_asesinatos_2017 <- df_frecuencias_promedio_asesinatos %>% filter(Var2 %in% c("2017", "2018", "2019","2020","2021"))
df_frecuencias_promedio_asesinatos_2017 <- df_frecuencias_promedio_asesinatos_2017 %>% select(-Var3)
media_asesinatos_ultimos_anos <- df_frecuencias_promedio_asesinatos_2017 %>% summarise(media_asesinatos = mean(Freq))
print(media_asesinatos_ultimos_anos)








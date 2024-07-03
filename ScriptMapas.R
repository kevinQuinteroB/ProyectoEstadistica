library(sf)

map <- st_read("UIS/Estadistica/Proyecto/MGN2022_MPIO_POLITICO/MGN_MPIO_POLITICO.shp")
plot(map)
map2 <- map[,1] # Solo el mapa en terminos de su codigo
plot(map2)

map2 <- st_read("UIS/Estadistica/Proyecto/Mapas_Manzana/MGN_URB_MANZANA.shp")

load("UIS/Estadistica/Proyecto/DatosMapasDelincuentes.RData")


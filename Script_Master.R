################################################
# Taller de FototrampeoR
#
# Paquete RAI_eR_Beta.R 
#
# Objetivo: Calcular la abundancia relativa (RAI) y la tasa de encuentro (eR) con datos obtenidos con cámaras trampa
#
# Elaborado por: SMandujanoR
#
# Última modificación: Octubre 30, 2023
################################################

# MATERIAL COMPLEMENTARIO PARA CONSULTAR Y FACILITAR EL EMPLEO DEL PAQUETE:

# 1. Tutorial del empleo de R para datos de fototrampeo en: https://rpubs.com/SMR8810/FTR_reproducible

# 2. Página del libro "FototrampeoR: Organización y análisis de datos, Volumen 1" en: https://smandujanor.github.io/Foto-trampeo-R-Vol_I

# 3. PDF del libro en: https://www.researchgate.net/publication/348922971_Fototrampeo_en_R_Organizacion_y_Analisis_de_Datos_Volumen_I

# 4. Material (códigos R y datos) del libro: https://smandujanor.github.io/Foto-trampeo-R-Vol_I

# 5. Si se empleo camtrapR para etiquetar, organizar y generar la data.frame, se sugiere consultar la siguiente viñeta para preparar la data.frame que requiere el paquete RAI_eR en: https://rpubs.com/SMR8810/IAR_datos

# 6. Para más detalles de cómo usar la función para crear una grid de camáras consultar la viñeta en: https://rpubs.com/SMR8810/Grid_camaras

# 7. IMPORTANTE: El paquete automáticamente guardar las tablas de resultados, gráficos y mapas. Si prefiere primero visualizar se debe desactivar "guardar tablas y figuras" empleando el simbolo "#" en el script RAI_eR_Beta.R

################################################
# Previamente instalar los paquetes:

paquetes <- c("agricolae", "MASS", "maptools", "raster", "RColorBrewer", "stringr", "xtable", "akima", "prettymapr", "GISTools", "leaflet")

pkgs_miss <- paquetes[!(paquetes %in% 
                          installed.packages())]

if(length(pkgs_miss)>0L)
  install.packages(pkgs_miss, 
  repos = "https://cloud.r-project.org/", 
  dependencies = TRUE)

################################################
# INICIA PROCESO CON EL PAQUETE

# PASO 1. Crear un subdirectorio para guardar los resultados

dir.create("Results")

# PASO 2. Leer datos de las especies, XY de las CTs y covariables hábitat:

wildlife.data <- read.csv("data/mamiferos.csv", header = T)

View(wildlife.data)
###head(wildlife.data, 10); tail(wildlife.data); names(wildlife.data); class(wildlife.data); length(wildlife.data$Species); unique(wildlife.data$Species)

habitat.data <- read.csv("data/habitat.csv", header = T)
View(habitat.data)

datos <- merge(x = wildlife.data, y = habitat.data, by = "Camera", all = TRUE)
View(datos)

# PASO 3. Cargar el paquete RAI: 

source("RAI_eR_Beta.R") 

##################################################
# Función para visualizar el área de estudio y las CTs

library(raster)

# cargar shape y reproyectar si es necesario
map <- shapefile("shapes/veg.shp")

###summary(map); class(map); bbox(map); map$tipo_Suelo; bbox(map); xmin(map)

proje <- "+proj=utm +zone=14 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"

proje2 <- CRS("+proj=utm +zone=14 +datum=WGS84 +towgs84=0,0,0")

map <- spTransform(map, proje)

# OJO: Craer paleta de colores para cada tipo de vegetación(o cualquier otras clasificación. O emplear los colores de R (eg., topo.colors, heat.colors, u otro):
my_colors <- c("gold", "deepskyblue2", "darkolivegreen4", "darkolivegreen1", "azure3", "darkorange2")

# Función:
Map_CTs(map, my_colors)

##################################################
# Función para proyectar los Eventos de las CTs en Esri.WorldImagery

Esri_CTs(habitat.data, proje2)

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#@ ANÁLISIS DE VARIAS ESPECIES     @
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

##################################################
# Función para cálculo del RAIeR general, en porcentaje relativo, y ocupación naive

RAIeR_gral(datos) 

##################################################
# Función para visualizar los registros de eventos por cámara para la(s) especie(s) seleccionada(s)

Event_CT(datos, c("Bas_ast", "Nas_nar"), parRow = 1, parCol = 2, pointSize = 2, labels = T) 

Event_CT(datos, c("Syl_flo", "Odo_vir", "Can_lat", "Uro_cin"), parRow = 2, parCol = 2, pointSize = 0.3, labels = T)

Event_CT(datos, c("Syl_flo", "Odo_vir", "Can_lat", "Uro_cin", "Mep_mac", "Pro_lot", "Pec_taj", "Bas_ast", "Nas_nar"), parRow = 3, parCol = 3, pointSize = 0.35, labels = F)

##################################################
# Función para calcular el RAIeR para cada especie por cámara

RAIeR_ct(datos, Ymax = 30) # OJO: Ymax para ajustar el eje Y

##################################################
# Función para pruebas estadísticas de los RAIeR entre especies 

RAIeR_test(Ymax = 15)

##################################################
# Función para generar matriz del RAIeR y Eventos por especies y cámara, útil para análisis de diversidad en otros paquetes 

RAIeR_matx()

##################################################
# Función para calcular el RAIeR para todas las especies como  modelo lineal generalizado (GLM) tipo regresión Poisson

RAIeR_glm(datos, family = "poisson")

##################################################
# Función para tabla final con todos los resultados

RAIeR_final(datos)


#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#@ ANÁLISIS DE UNA SOLA ESPECIE    @
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

##################################################
# Función para seleccionar una especie (Pendiente) 

###Select_sp(species = "Odo_vir", Spatial = c("Location", "Habitat"), Temporal = c("Season", "Year"))

##################################################
# Función para generar mapa del RAIeR de la especie seleccionada 

RAIeR_mapSp(species = "Odo_vir", pointSize = 0.1, mycolors = my_colors)

RAIeR_mapSp("Can_lat", pointSize = 0.85, my_colors)

RAIeR_mapSp("Syl_flo", pointSize = 0.15, my_colors)

RAIeR_mapSp("Pro_lot", pointSize = 1, my_colors)

RAIeR_mapSp("Lin_ruf", pointSize = 1, my_colors)

##################################################
# Función para proyectar los RAIeR en Esri.WorldImagery

Esri_RAIeR(species = "Syl_flo", proje2, pointsize = 0.5)

##################################################
# Función para calcular el RAI por especie en relación con covariables empleando un GLM Poisson 

RAIeR_glmSp(Species = "Pro_lot", covs = "Goats", family = "poisson")

RAIeR_glmSp(Species = "Syl_flo", covs = c("Kernel_cam", "Kernel_gps"), family = "poisson")

RAIeR_glmSp(Species = "Uro_cin", covs = c("Kernel_cam", "Kernel_gps", "Kernel_todos"), family = "poisson")

RAIeR_glmSp(Species = "Odo_vir", covs = c("Kernel_cam", "Kernel_gps", "Kernel_todos", "Goats"), family = "poisson")

##################################################
# Función para generar raster y jpg basado en la densidad kernel para la especie seleccionada 

RAIeR_kernelSp("Syl_flo", S = 0.9, HR = 95, pointSize =  20, proje = proje, mycolors = hcl.colors(99, palette = "viridis", alpha = NULL, rev = F))  

#@@@@@@@@@@@@@@@@@@@@@@@@
#@ OTRAS FUNCIONES      @
#@@@@@@@@@@@@@@@@@@@@@@@@

##################################################
# Función para diseño de muestreo en campo de CTs 

miGrid(n1 = 7, n2 = 5, CTx = 690500, CTy = 2007000, CTdist = 1000, proje = proje)

##################################################
# Función para formater el .csv generado en camtrapR (Pendiente)

###dataFormat(data_Spp, data_CT)


#################################################
# FIN SCRIPT

rm(list = ls()) # para borrar la memoria
dev.off() # para borrar los gráficos

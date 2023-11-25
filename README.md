# Salvador Mandujano Rodríguez

 <https://www.researchgate.net/profile/Salvador_Mandujano>

- Semana del 28 de Septiembre al 2 de Octubre del 2020.

- Horario 10:00 a 14:00 h.

---

# Taller de FototrampeoR
# Versión Beta del Paquete RAI_eR_Beta.R 
# Objetivo: Calcular la abundancia relativa (RAI) y la tasa de encuentro (eR) con datos obtenidos con cámaras trampa
# Elaborado por: SMandujanoR
# Última modificación: Octubre 30, 2023

---


# MATERIAL COMPLEMENTARIO PARA CONSULTAR Y FACILITAR EL EMPLEO DEL PAQUETE:

1. Tutorial del empleo de R para datos de fototrampeo en: https://rpubs.com/SMR8810/FTR_reproducible

<img align="rigth" src="extras/Figura 3.JPG" alt="drawing" width="500"/>

---

2. Página del libro "FototrampeoR: Organización y análisis de datos, Volumen 1" en: https://smandujanor.github.io/Foto-trampeo-R-Vol_I

---

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

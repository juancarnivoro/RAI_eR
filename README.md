# Taller de FototrampeoR

- Versión Beta: **Paquete RAI_eR_Beta.R**
- Objetivo: **Calcular la abundancia relativa (RAI) y la tasa de encuentro (eR) con datos obtenidos con cámaras trampa**
- Elaborado por: **SMandujanoR**  <https://www.researchgate.net/profile/Salvador_Mandujano>
- Última modificación: **Octubre 30, 2023**

---

# Previamente instalar los paquetes:

paquetes <- c("agricolae", "MASS", "maptools", "raster", "RColorBrewer", "stringr", "xtable", "akima", "prettymapr", "GISTools", "leaflet")

pkgs_miss <- paquetes[!(paquetes %in% 
                          installed.packages())]

if(length(pkgs_miss)>0L)
  install.packages(pkgs_miss, 
  repos = "https://cloud.r-project.org/", 
  dependencies = TRUE)

---

# Para clonar el paquete desde GitHUb:

- Paso 1: entrar a la liga

<https://github.com/SMandujanoR/RAI_eR_Beta>

- Paso 2:
  
<img align="rigth" src="extras/f_1.jpg" alt="drawing" width="1500"/>

- Paso 3:
  
<img align="rigth" src="extras/f_2.jpg" alt="drawing" width="500"/>

---

# IMPORTANTE: 

El paquete automáticamente guardar las tablas de resultados, gráficos y mapas. Si prefiere primero visualizar se debe desactivar "guardar tablas y figuras" empleando el simbolo "#" en el script RAI_eR_Beta.R

---


# MATERIAL COMPLEMENTARIO PARA CONSULTAR Y FACILITAR EL EMPLEO DEL PAQUETE:

---

**1. Tutorial del empleo de R para datos de fototrampeo en: https://rpubs.com/SMR8810/FTR_reproducible**

<img align="rigth" src="extras/f_4.jpg" alt="drawing" width="1500"/>

---

**2. Página del libro "FototrampeoR: Organización y análisis de datos, Volumen 1" en: https://smandujanor.github.io/Foto-trampeo-R-Vol_I**

<img align="rigth" src="extras/f_6.jpg" alt="drawing" width="1500"/>

---

**3. PDF del libro en: https://www.researchgate.net/publication/348922971_Fototrampeo_en_R_Organizacion_y_Analisis_de_Datos_Volumen_I**

<img align="rigth" src="extras/f_7.jpg" alt="drawing" width="1500"/>

---

**4. Material (códigos R y datos) del libro: https://smandujanor.github.io/Foto-trampeo-R-Vol_I** 

<img align="rigth" src="extras/f_8.jpg" alt="drawing" width="1500"/>

---

**5. Si se empleo camtrapR para etiquetar, organizar y generar la data.frame, se sugiere consultar la siguiente viñeta para preparar la data.frame que requiere el paquete RAI_eR en: https://rpubs.com/SMR8810/IAR_datos** 

<img align="rigth" src="extras/f_5.jpg" alt="drawing" width="1500"/>

---

**6. Para más detalles de cómo usar la función para crear una grid de camáras consultar la viñeta en: https://rpubs.com/SMR8810/Grid_camaras**

<img align="rigth" src="extras/f_3.jpg" alt="drawing" width="1500"/>

---







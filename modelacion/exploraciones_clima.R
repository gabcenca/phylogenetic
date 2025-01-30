
### Trabajar con raster 

library(terra)
library(ggplot2)
library(dplyr)

describe("modelacion/wc2.1_2.5m_bio/wc2.1_2.5m_bio_1.tif")

bioclim_info <- capture.output(
  describe("modelacion/wc2.1_2.5m_bio/wc2.1_2.5m_bio_1.tif")
)

rast_annualtemp <-
  rast("modelacion/wc2.1_2.5m_bio/wc2.1_2.5m_bio_1.tif")

rast_annualtemp


annualtemp_df <- as.data.frame(rast_annualtemp, xy = TRUE)
str(annualtemp_df)

#Para visualizarlo, esta muy pesado: 
# ggplot() +
#   geom_raster(data = annualtemp_df , aes(x = x, y = y, fill = wc2.1_2.5m_bio_1)) +
#   scale_fill_viridis_c(na.value = 'deeppink') +
#   coord_quickmap()

crs(rast_annualtemp, proj = TRUE)

#maximos de temperaturas:
minmax(rast_annualtemp)

#Para ver las bandas del raster:
nlyr(rast_annualtemp)

#Subir vector estados Mex
library(sf)
mex <- st_read("modelacion/dest_2010gw/dest_2010gw.shp")
mex

ggplot()+
  geom_sf(data = mex, color = "blue", fill="red") +
  coord_sf()

#Cortar raster con el vector de mex

#Para visualizar todo el raster con el corte:
# ggplot() +
#   geom_raster(data = annualtemp_df, aes(x = x, y = y, fill = wc2.1_2.5m_bio_1)) +
#   scale_fill_viridis_c() +
#   geom_sf(data = mex, color = "blue", fill = NA) +
#   coord_sf()

#Para cortar 
rast_annualtemp_mex <- crop(x = rast_annualtemp, y = mex)

rast_annualtemp_mex_df <- as.data.frame(rast_annualtemp_mex, xy = TRUE)

library(ggspatial) #para el norte 

mex_annualtemp <- ggplot() +
  geom_raster(data = rast_annualtemp_mex_df, aes(x = x, y = y, fill = wc2.1_2.5m_bio_1)) +
  scale_fill_viridis_c() +
  geom_sf(data = mex, color = "blue", fill = NA) +
  coord_sf()+  
  annotation_north_arrow(location = "tr", which_north = "true",  # Ubicación del norte (tr = top-right)
                         pad_x = unit(0.1, "in"), pad_y = unit(0.1, "in"),  # Ajuste de distancia
                         style = north_arrow_fancy_orienteering) +  # Estilo de flecha
  theme_minimal()

ggsave("D:/Base_datos/graficas finales/mex_annualtemp_rast2.jpg", plot = mex_annualtemp, 
       type = "cairo", width = 8, height = 6, dpi = 300)


#Extraer valores de un raster a partir de un vector 
annualtemp <- extract(x = rast_annualtemp_mex, y = mex, raw = FALSE)

str(annualtemp)

#Histograma para ver los valores 
ggplot() +
  geom_histogram(data = annualtemp, aes(x = wc2.1_2.5m_bio_1)) +
  ggtitle("Histogram of CHM Height Values (m)") +
  xlab("Temperature") +
  ylab("Frequency of Pixels")

summary(annualtemp$wc2.1_2.5m_bio_1)


#Extraer la especie de mayor y menor distribucion 
conteo_especies <- read.csv("modelacion/tabla_conteo_especies.csv", header=T)
#Quercus rekonis es la de menor y Quercus Rugosa la de mayor 

#Base de datos completa
library(readr)
hgbif_filt <- read_csv("data/out/hgbif_filt_10s_terres.csv")

#De todos los encinos:
#Pasar a formato sf
library(tidyr)

hgbif_filt <- hgbif_filt %>%
  mutate(
    geometry = gsub("[c()]", "", geometry),  # Eliminar "c(" y ")"
    geometry = gsub(",", "", geometry)      # Eliminar la coma
  )

puntos_quercus <- st_as_sf(hgbif_filt, coords = c("decimalLongitude", "decimalLatitude"), crs = 4326)

rekonis_quercus_meantemp <- ggplot() +
  geom_raster(data = rast_annualtemp_mex_df, aes(x = x, y = y, fill = wc2.1_2.5m_bio_1)) +
  scale_fill_viridis_c(name = "Temperatura Promedio Anual") +
  geom_sf(data = mex, color = "black", fill = NA) +
  geom_sf(data = puntos_rekonis, aes(geometry = geometry), 
          color = "#8B2323", size = 3,shape = 1, fill = NA) +
  geom_sf(data = puntos_rugosa, aes(geometry = geometry), 
          color = "#27408B", size = 3,shape = 1,fill = NA) +
  coord_sf()+  
  annotation_north_arrow(location = "tr", which_north = "true",  # Ubicación del norte (tr = top-right)
                         pad_x = unit(0.1, "in"), pad_y = unit(0.1, "in"),  # Ajuste de distancia
                         style = north_arrow_fancy_orienteering) +  # Estilo de flecha
  theme_minimal()

ggsave("D:/Base_datos/graficas finales/mex_annualtemp_puntos2.jpg", plot = rekonis_quercus_meantemp, 
       type = "cairo", width = 8, height = 6, dpi = 300)

quercus_meantemp <- ggplot() +
  geom_raster(data = rast_annualtemp_mex_df, aes(x = x, y = y, fill = wc2.1_2.5m_bio_1)) +
  scale_fill_viridis_c(name = "Temperatura Promedio Anual") +
  geom_sf(data = mex, color = "black", fill = NA) +
  geom_sf(data = puntos_quercus, aes(geometry = geometry), 
          color = "#8B2323", size = 3) +
  coord_sf()+  
  annotation_north_arrow(location = "tr", which_north = "true",  # Ubicación del norte (tr = top-right)
                         pad_x = unit(0.1, "in"), pad_y = unit(0.1, "in"),  # Ajuste de distancia
                         style = north_arrow_fancy_orienteering) +  # Estilo de flecha
  theme_minimal()

ggsave("D:/Base_datos/graficas finales/mex_annualtemp_quercuspuntos2.jpg", plot = quercus_meantemp, 
       type = "cairo", width = 8, height = 6, dpi = 300)


#Extraer los valores de la temperatura para las especies
#Q rekonis
annualtemp_rekonis <- extract(x = rast_annualtemp_mex, y = puntos_rekonis, raw = FALSE)

str(annualtemp_rekonis)

summary(annualtemp_rekonis$wc2.1_2.5m_bio_1)

#Q rugosa
annualtemp_rugosa <- extract(x = rast_annualtemp_mex, y = puntos_rugosa, raw = FALSE)

str(annualtemp_rugosa)

summary(annualtemp_rugosa$wc2.1_2.5m_bio_1)

#Todos los quercus
annualtemp_quercus <- extract(x = rast_annualtemp_mex, y = puntos_quercus, 
                              raw = FALSE)

str(annualtemp_quercus)

summary(annualtemp_quercus$wc2.1_2.5m_bio_1)

temp_quercus <- ggplot() +
  geom_histogram(data = annualtemp_quercus, aes(x = wc2.1_2.5m_bio_1)) +
  ggtitle("Histograma de temperaturas anuales promedio para todos los registros de Encinos") +
  xlab("Temperatura (C)") +
  ylab("Frecuencia pixeles") +
  theme_minimal()

ggsave("D:/Base_datos/graficas finales/temp_quercus.jpg", plot = temp_quercus, 
       type = "cairo", width = 8, height = 6, dpi = 300)


temp_rugosa <- ggplot() +
  geom_histogram(data = annualtemp_rugosa, aes(x = wc2.1_2.5m_bio_1)) +
  ggtitle("Histograma de temperaturas anuales promedio para Q. rugosa") +
  xlab("Temperatura (C)") +
  ylab("Frecuencia pixeles") +
  theme_minimal()

ggsave("D:/Base_datos/graficas finales/temp_rugosa.jpg", plot = temp_rugosa, 
       type = "cairo", width = 8, height = 6, dpi = 300)

temp_rekonis <- ggplot() +
  geom_histogram(data = annualtemp_rekonis, aes(x = wc2.1_2.5m_bio_1)) +
  ggtitle("Histograma de temperaturas anuales promedio para Q. rekonis") +
  xlab("Temperatura (C)") +
  ylab("Frecuencia pixeles") +
  theme_minimal()

ggsave("D:/Base_datos/graficas finales/temp_rekonis.jpg", plot = temp_rekonis, 
       type = "cairo", width = 8, height = 6, dpi = 300)


#Para cortar para precipitacion 
rast_annualpreci <-
  rast("modelacion/wc2.1_2.5m_bio/wc2.1_2.5m_bio_12.tif")


rast_annualpreci


annualpreci_df <- as.data.frame(rast_annualpreci, xy = TRUE)

rast_annualpreci_mex <- crop(x = rast_annualpreci, y = mex)

rast_rast_annualpreci_mex_df <- as.data.frame(rast_annualpreci_mex, xy = TRUE)

#Mapa de raster con mexico 
mex_annualpreci <- ggplot() +
  geom_raster(data = rast_rast_annualpreci_mex_df, aes(x = x, y = y, fill = wc2.1_2.5m_bio_12)) +
  scale_fill_viridis_c() +
  geom_sf(data = mex, color = "blue", fill = NA) +
  coord_sf()+  
  annotation_north_arrow(location = "tr", which_north = "true",  # Ubicación del norte (tr = top-right)
                         pad_x = unit(0.1, "in"), pad_y = unit(0.1, "in"),  # Ajuste de distancia
                         style = north_arrow_fancy_orienteering) +  # Estilo de flecha
  theme_minimal()

ggsave("D:/Base_datos/graficas finales/mex_annualpreci_rast.jpg", plot = mex_annualpreci, 
       type = "cairo", width = 8, height = 6, dpi = 300)



mex_annualpreci <- ggplot() +
  geom_raster(data = rast_rast_annualpreci_mex_df, aes(x = x, y = y, fill = wc2.1_2.5m_bio_12)) +
  scale_fill_viridis_c() +
  geom_sf(data = puntos_quercus, color = "blue", fill = NA) +
  coord_sf()+  
  annotation_north_arrow(location = "tr", which_north = "true",  # Ubicación del norte (tr = top-right)
                         pad_x = unit(0.1, "in"), pad_y = unit(0.1, "in"),  # Ajuste de distancia
                         style = north_arrow_fancy_orienteering) +  # Estilo de flecha
  theme_minimal()

ggsave("D:/Base_datos/graficas finales/mex_annualpreci_rast.jpg", plot = mex_annualpreci, 
       type = "cairo", width = 8, height = 6, dpi = 300)

#Extraer los valores de la precipitacion para las especies
#Q rekonis
annualpreci_rekonis <- extract(x = rast_annualpreci_mex, y = puntos_rekonis, raw = FALSE)

str(annualpreci_rekonis)

summary(annualpreci_rekonis$wc2.1_2.5m_bio_12)

#Q rugosa
annualpreci_rugosa <- extract(x = rast_annualpreci_mex, y = puntos_rugosa, raw = FALSE)

str(annualpreci_rugosa)

summary(annualpreci_rugosa$wc2.1_2.5m_bio_12)

#Todos los quercus 
annualpreci_quercus <- extract(x = rast_annualpreci_mex, y = puntos_quercus,
                               raw = FALSE)

str(annualpreci_quercus)

summary(annualpreci_quercus$wc2.1_2.5m_bio_12)

#Histogramas de la informacion obtenida 

preci_quercus <- ggplot() +
  geom_histogram(data = annualpreci_quercus, aes(x = wc2.1_2.5m_bio_12)) +
  ggtitle("Histograma de precipitación anual para todos los registros de Encinos") +
  xlab("Precipitación (mm)") +
  ylab("Frecuencia pixeles") +
  theme_minimal()

ggsave("D:/Base_datos/graficas finales/preci_quercus.jpg", plot = preci_quercus, 
       type = "cairo", width = 8, height = 6, dpi = 300)

preci_rugosa <- ggplot() +
  geom_histogram(data = annualpreci_rugosa, aes(x = wc2.1_2.5m_bio_12)) +
  ggtitle("Histograma de precipitación anual para Q. rugosa") +
  xlab("Precipitación (mm)") +
  ylab("Frecuencia pixeles") +
  theme_minimal()

ggsave("D:/Base_datos/graficas finales/preci_rugosa.jpg", plot = preci_rugosa, 
       type = "cairo", width = 8, height = 6, dpi = 300)

preci_rekonis <- ggplot() +
  geom_histogram(data = annualpreci_rekonis, aes(x = wc2.1_2.5m_bio_12)) +
  ggtitle("Histograma de precipitación anual para Q. rekonis") +
  xlab("Precipitación (mm)") +
  ylab("Frecuencia pixeles") +
  theme_minimal()

ggsave("D:/Base_datos/graficas finales/preci_rekonis.jpg", plot = preci_rekonis, 
       type = "cairo", width = 8, height = 6, dpi = 300)


#Mapas de dispersion precipitacion vs temperatura

qrugosa_info <- merge(annualpreci_rugosa, annualtemp_rugosa, by = "ID")
qrekonis_info <- merge(annualpreci_rekonis, annualtemp_rekonis, by = "ID")
quercus_info <- merge(annualpreci_quercus,annualtemp_quercus, by ="ID")

qrugosa_info$elevacion <- qrugosa$elevacion
qrekonis_info$elevacion <- qrekonis$elevacion
quercus_info$elevacion <- hgbif_filt$elevacion

write.csv(quercus_info,"modelacion/quercus_info_temp_preci.csv")

str(qrugosa_info)

dispersion_rugosa<- ggplot(qrugosa_info, aes(x = wc2.1_2.5m_bio_12, y = wc2.1_2.5m_bio_1)) +
  geom_point(color = "#7AC5CD", alpha = 0.7) +  # Puntos en el gráfico
  labs(x = "Precipitación (mm)", y = "Temperatura (°C)", 
       title = "Dispersión: Precipitación vs Temperatura para Q. rugosa") +
  theme_minimal() +
  geom_smooth(method = "lm", color = "#CD5C5C", se = T)

ggsave("D:/Base_datos/graficas finales/dispersion_rugosa_tempvsprec.jpg", plot = dispersion_rugosa, 
       type = "cairo", width = 8, height = 6, dpi = 300)


dispersion_rekonis<- ggplot(qrekonis_info, aes(x = wc2.1_2.5m_bio_12, y = wc2.1_2.5m_bio_1)) +
  geom_point(color = "#7AC5CD", alpha = 0.7) +  # Puntos en el gráfico
  labs(x = "Precipitación (mm)", y = "Temperatura (°C)", 
       title = "Dispersión: Precipitación vs Temperatura para Q. rekonis") +
  theme_minimal() +
  geom_smooth(method = "lm", color = "#CD5C5C", se = T)

ggsave("D:/Base_datos/graficas finales/dispersion_rekonis_tempvsprec.jpg", plot = dispersion_rekonis, 
       type = "cairo", width = 8, height = 6, dpi = 300)


dispersion_quercus<- ggplot(quercus_info, aes(x = wc2.1_2.5m_bio_12, y = wc2.1_2.5m_bio_1)) +
  geom_point(color = "#7AC5CD", alpha = 0.7) +  # Puntos en el gráfico
  labs(x = "Precipitación (mm)", y = "Temperatura (°C)", 
       title = "Dispersión: Precipitación vs Temperatura para todos los registros de Encinos") +
  theme_minimal() +
  geom_smooth(method = "lm", color = "#CD5C5C", se = T)

ggsave("D:/Base_datos/graficas finales/dispersion_quercus_tempvsprec2.jpg", plot = dispersion_quercus, 
       type = "cairo", width = 8, height = 6, dpi = 300)


#Testar correlacion 
quercus_info <- read.csv("modelacion/quercus_info_temp_preci.csv")

hist(quercus_info$wc2.1_2.5m_bio_1)
shapiro.test(quercus_info$wc2.1_2.5m_bio_1)

library(nortest)
ad.test(quercus_info$wc2.1_2.5m_bio_1)

cor.test(quercus_info$wc2.1_2.5m_bio_1, quercus_info$wc2.1_2.5m_bio_12, method = "kendall")


#mostrar una relación lineal entre las dos variables que estás analizando
#rango sombreado alrededor de la línea de tendencia. Este intervalo de confianza te muestra la incertidumbre de la estimación de la línea de regresión. Es decir, te indica que la verdadera línea de relación podría estar en algún 
#lugar dentro de ese rango y no necesariamente sobre la línea exacta.


#Máxima Entropía (MaxEnt)

# Cargar los datos de presencia con elevación incluida
# Asegúrate de que la tabla tenga columnas: decimalLongitude, decimalLatitude, elevacion



#Indice de riqueza 
# Crear una grilla sobre el área de interés
mexico_bbox <- st_bbox(puntos_quercus) # Extensión de los puntos
grilla <- st_make_grid(
  st_as_sfc(mexico_bbox),
  cellsize = 1,  # Tamaño de la celda en grados (ajusta según la resolución)
  crs = st_crs(puntos_quercus) # Usar el mismo CRS
)

# Convertir la grilla a un objeto sf
grilla_sf <- st_sf(geometry = grilla)

# Asignar cada punto a una celda de la grilla
puntos_en_grilla <- st_join(puntos_quercus, grilla_sf)

# Calcular la riqueza por celda
riqueza <- puntos_en_grilla %>%
  group_by(geometry) %>%         # Agrupar por celda
  summarise(riqueza = n_distinct(correctname)) # Contar especies únicas

# Unir la riqueza a la grilla
grilla_sf <- grilla_sf %>%
st_join(riqueza, join = st_intersects) %>% # Unión espacial
mutate(riqueza = replace_na(riqueza, 0))   # Llenar celdas vacías

ggplot(grilla_sf) +
  geom_sf(aes(fill = riqueza)) +
  scale_fill_viridis_c() +
  labs(title = "Índice de riqueza de encinos en México",
       fill = "Riqueza") +
  theme_minimal()

grilla_sf_con_estados <- st_join(grilla_sf, mex, join = st_intersects)

riqueza_mex_quercus <- ggplot() +
  geom_sf(data = grilla_sf_con_estados, aes(fill = riqueza)) +
  geom_sf(data = mex, color = "black", alpha = 0.2) +
  scale_fill_viridis_c() +
  labs(title = "Índice de riqueza de encinos en México",
       fill = "Riqueza") +
  theme_minimal()

ggsave("D:/Base_datos/graficas finales/riqueza_mex_quercus.jpg", plot = riqueza_mex_quercus, 
       type = "cairo", width = 8, height = 6, dpi = 300)


#Ver correlacion de riqueza con precipitacion/temperatura
library(exactextractr)

# Calcular el promedio de precipitación para cada polígono
grilla_sf$precip <- exact_extract(rast_annualpreci_mex, grilla_sf, 'mean')
grilla_sf$temp <- exact_extract(rast_annualtemp_mex, grilla_sf, 'mean')

# Modelo lineal múltiple
modelo <- lm(riqueza ~ precip + temp, data = grilla_sf)

# Resumen del modelo
summary(modelo)
plot(modelo)

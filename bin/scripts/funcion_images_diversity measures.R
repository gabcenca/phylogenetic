library(ggplot2)
library(sf)
library(rnaturalearthdata)
library(rnaturalearth)
library(terra)


# Obtener el mapa de México con rnaturalearth
mexico_map <- ne_countries(scale = "large", returnclass = "sf", country = "Mexico")

# df de archivos
# Enlistar carpetas _tif

dirs <- list.dirs(here::here('data/out')) %>% .[str_detect(.,'tif')]

dt <- data.table(folders = dirs)
dt[, folder_row := .I]

# Columna para diferenciar los subconjuntos de especies TRUE = conjunto de la filogenia, FALSE = todas las especies
dt[,spSet := str_detect(folders, 'tree|PD')]

# Enlistar los archivos de cada carpeta
dt[, files_df := lapply(folders, function(f) {
  data.table(files = list.files(f, full.names = TRUE))
})]

unnested_dt <- rbindlist(dt$files_df, idcol = "folder_row")

unnested_dt <- merge(unnested_dt, dt[, !"files_df"], by = "folder_row", all.x = TRUE)


#Hacer columna que indique el valor de la medida de biodiversidad
unnested_dt[, c('measure','scale') := tstrsplit(basename(files), '_', keep = c(1,3))]

unnested_dt [, scale := str_remove(scale, '.tif')]

#Leer el raster para cada medida
unnested_dt[, rast := lapply(files,rast)]

unnested_dt[, files := basename(files)][,folders := NULL]


# Pasar el raster a data frame 

unnested_dt[, rast_df := lapply(rast, function(r){
  r_df <- as.data.table(r, xy = TRUE, na.rm = TRUE)
  colnames(r_df) <- c('x', 'y', 'value')
  return(r_df)
  })]


unnested_dt[, file_row := .I]
unnested_raster <- rbindlist(unnested_dt$rast_df, idcol = "file_row", fill = T)

#guardar todos los resultados en un solo df
long_rast_df <- merge(unnested_raster, unnested_dt[,!c('rast', 'rast_df')],
                      by = "file_row", all.x = T)


# Plot 1 medidas todas las escalas

ggplot() +
  geom_sf(data = mexico_map, fill = NA, color = "black") +
  geom_raster(data = long_rast_df[measure == 'richness',], aes(x = x, y = y, fill = value)) +
  coord_sf(xlim = c(-120, -85), ylim = c(14, 33)) +  # adjust to fit your raster
  theme_bw() +
  theme(axis.text.x = element_blank(), axis.ticks = element_blank()) +
  scale_fill_viridis_c() +
  labs(title = "Richness by scale", fill = "Richness", x = '', y = '') +
  facet_wrap(~scale, ncol = 2)


#Plot 1 escala todas las medidas 
ggplot() +
  geom_sf(data = mexico_map, fill = NA, color = "black") +
  geom_raster(data = long_rast_df[scale == '0.45',], aes(x = x, y = y, fill = value)) +
  coord_sf(xlim = c(-120, -85), ylim = c(14, 33)) +  # adjust to fit your raster
  theme_bw() +
  theme(axis.text.x = element_blank(), axis.ticks = element_blank()) +
  scale_fill_viridis_c() +
  labs(title = "Diversity measures (scale 0.45)", fill = "Value", x = '', y = '') +
  facet_wrap(~measure, ncol = 2)



library(ggplot2)
library(sf)
library(rnaturalearthdata)
library(rnaturalearth)
library(terra)
library(data.table)

# Obtener el mapa de México con rnaturalearth
mexico_map <- ne_countries(scale = "large", returnclass = "sf", country = "Mexico")

# df de archivos
# Enlistar carpetas _tif

dirs <- list.dirs(here::here('data/out')) %>% .[stringr::str_detect(.,'tif')]

dt <- data.table(folders = dirs)
dt[, folder_row := .I]

# Columna para diferenciar los subconjuntos de especies TRUE = conjunto de la filogenia, FALSE = todas las especies
dt[,spSet := stringr::str_detect(folders, 'tree|PD')]

# Enlistar los archivos de cada carpeta
dt[, files_df := lapply(folders, function(f) {
  data.table(files = list.files(f, full.names = TRUE))
})]

unnested_dt <- rbindlist(dt$files_df, idcol = "folder_row")

unnested_dt <- merge(unnested_dt, dt[, !"files_df"], by = "folder_row", all.x = TRUE)


#Hacer columna que indique el valor de la medida de biodiversidad
unnested_dt[, c('measure','scale') := tstrsplit(basename(files), '_', keep = c(1,3))]

unnested_dt [, scale := stringr::str_remove(scale, '.tif')]

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

#Plot 1 medida todas las escalas 
plot1 <- ggplot() +
  geom_sf(data = mexico_map, fill = NA, color = "black") +
  geom_raster(data = long_rast_df[measure == 'richness',], 
              aes(x = x, y = y, fill = value)) +
  coord_sf(xlim = c(-120, -85), ylim = c(14, 33)) +  # adjust to fit your raster
  theme_bw() +
  theme(axis.text = element_blank(), axis.ticks = element_blank())+
  scale_fill_viridis_c() +
  facet_wrap(~scale, ncol = 2)+
  labs(title = "Riqueza por escala", fill = 'Riqueza', x = '', y = '')

ggsave(here::here("data/out/all_maps/rich_scales.png"), plot1, width =35 , height = 30, units = 'cm')


plot2 <- ggplot() +
  geom_sf(data = mexico_map, fill = NA, color = "black") +
  geom_raster(data = long_rast_df[measure == 'wendemism',], aes(x = x, y = y, fill = value)) +
  coord_sf(xlim = c(-120, -85), ylim = c(14, 33)) +  # adjust to fit your raster
  theme_bw() +
  theme(axis.text = element_blank(), axis.ticks = element_blank())+
  scale_fill_viridis_c() +
  facet_wrap(~scale, ncol = 2)+
  labs(title = "Endemismo ponderado por escala", fill = 'Endemismo ponderado', x = '', y = '')

ggsave(here::here("data/out/all_maps/wendemism_scales.png"), plot2, width =35 , height = 30, units = 'cm')


plot3 <- ggplot() +
  geom_sf(data = mexico_map, fill = NA, color = "black") +
  geom_raster(data = long_rast_df[measure == 'PD',], aes(x = x, y = y, fill = value)) +
  coord_sf(xlim = c(-120, -85), ylim = c(14, 33)) +  # adjust to fit your raster
  theme_bw() +
  theme(axis.text = element_blank(), axis.ticks = element_blank())+
  scale_fill_viridis_c() +
  facet_wrap(~scale, ncol = 2)+
  labs(title = "Diversidad filogenética por escala", fill = 'Diversidad filogenética', x = '', y = '')

ggsave(here::here("data/out/all_maps/pd_scales.png"), plot3, width =35 , height = 30, units = 'cm')

plot4 <- ggplot() +
  geom_sf(data = mexico_map, fill = NA, color = "black") +
  geom_raster(data = long_rast_df[measure == 'richness' & spSet == 'TRUE',], aes(x = x, y = y, fill = value)) +
  coord_sf(xlim = c(-120, -85), ylim = c(14, 33)) +  # adjust to fit your raster
  theme_bw() +
  theme(axis.text = element_blank(), axis.ticks = element_blank())+
  scale_fill_viridis_c() +
  facet_wrap(~scale, ncol = 2)+
  labs(title = "Riqueza por escala de especies presentes en el árbol filogénetic", 
       fill = 'Riqueza', x = '', y = '')

ggsave(here::here("data/out/all_maps/rich_scales_tree.png"), plot4, width =35 , height = 30, units = 'cm')


plot5 <- ggplot() +
  geom_sf(data = mexico_map, fill = NA, color = "black") +
  geom_raster(data = long_rast_df[measure == 'wendemism' & spSet == 'TRUE',], 
              aes(x = x, y = y, fill = value)) +
  coord_sf(xlim = c(-120, -85), ylim = c(14, 33)) +  # adjust to fit your raster
  theme_bw() +
  theme(axis.text = element_blank(), axis.ticks = element_blank())+
  scale_fill_viridis_c() +
  facet_wrap(~scale, ncol = 2)+
  labs(title = "Endemismo ponderado por escala de especies presentes en el árbol filogenético", 
       fill = 'Endemismo ponderado', x = '', y = '')

ggsave(here::here("data/out/all_maps/wendemism_scales_tree.png"), plot5, width =35 , height = 30, units = 'cm')

#Plot 1 escala todas las medidas 
library(ggplot2)
library(patchwork)

# Filtrar por cada medida
plot_rich_315 <- ggplot() +
  geom_sf(data = mexico_map, fill = NA, color = "black") +
  geom_raster(data = long_rast_df[scale == "0.315" & measure == "richness",],
              aes(x = x, y = y, fill = value)) +
  coord_sf(xlim = c(-120, -85), ylim = c(14, 33)) +
  scale_fill_viridis_c(name = "Riqueza") +
  theme_bw() +
  theme(axis.text = element_blank(), axis.ticks = element_blank()) +
  labs(title = "Riqueza de especies, escala 0.315°", x = "", y = "")

plot_endemismo315 <- ggplot() +
  geom_sf(data = mexico_map, fill = NA, color = "black") +
  geom_raster(data = long_rast_df[scale == "0.315" & measure == "wendemism",],
              aes(x = x, y = y, fill = value)) +
  coord_sf(xlim = c(-120, -85), ylim = c(14, 33)) +
  scale_fill_viridis_c(name = "Endemismo ponderado") +
  theme_bw() +
  theme(axis.text = element_blank(), axis.ticks = element_blank()) +
  labs(title = "Endemismo ponderado, escala 0.315°", x = "", y = "")

plot_rich_0315tree <- ggplot() +
  geom_sf(data = mexico_map, fill = NA, color = "black") +
  geom_raster(data = long_rast_df[scale == "0.315" & spSet == 'TRUE' & measure == "richness",],
              aes(x = x, y = y, fill = value)) +
  coord_sf(xlim = c(-120, -85), ylim = c(14, 33)) +
  scale_fill_viridis_c(name = "Riqueza") +
  theme_bw() +
  theme(axis.text = element_blank(), axis.ticks = element_blank()) +
  labs(title = "Riqueza de especies presentes en el árbol, escala 0.315°", x = "", y = "")

plot_endemismo315tree <- ggplot() +
  geom_sf(data = mexico_map, fill = NA, color = "black") +
  geom_raster(data = long_rast_df[scale == "0.315" & spSet == 'TRUE' & measure == "wendemism",],
              aes(x = x, y = y, fill = value)) +
  coord_sf(xlim = c(-120, -85), ylim = c(14, 33)) +
  scale_fill_viridis_c(name = "Endemismo ponderado") +
  theme_bw() +
  theme(axis.text = element_blank(), axis.ticks = element_blank()) +
  labs(title = "Endemismo ponderado de especies presentes en el árbol, escala 0.315°", x = "", y = "")

plot_pd <- ggplot() +
  geom_sf(data = mexico_map, fill = NA, color = "black") +
  geom_raster(data = long_rast_df[scale == "0.315" & spSet == 'TRUE' & measure == "PD",],
              aes(x = x, y = y, fill = value)) +
  coord_sf(xlim = c(-120, -85), ylim = c(14, 33)) +
  scale_fill_viridis_c(name = "Diversidad filogenética") +
  theme_bw() +
  theme(axis.text = element_blank(), axis.ticks = element_blank()) +
  labs(title = "Diversidad filogenética, escala 0.315°", x = "", y = "")


# Calcular los límites conjuntos de ambos datasets
max_val <- max(long_rast_df[scale == "0.315" & measure == "richness", ]$value, na.rm = TRUE)
min_val <- min(long_rast_df[scale == "0.315" & measure == "richness", ]$value, na.rm = TRUE)

# Combinarlos


#Mapas para la presentacion del morton 

# calcular el rango de valores de riqueza para ambas capas
rango_rich <- range(long_rast_df[scale == "0.315" & measure == "richness", value],
                    long_rast_df[scale == "0.315" & spSet == TRUE & measure == "richness", value],
                    na.rm = TRUE)

plot_rich_315 <- ggplot() +
  geom_sf(data = mexico_map, fill = NA, color = "black") +
  geom_raster(data = long_rast_df[scale == "0.315" & measure == "richness",],
              aes(x = x, y = y, fill = value)) +
  coord_sf(xlim = c(-120, -85), ylim = c(14, 33)) +
  scale_fill_viridis_c(name = "Riqueza", limits = rango_rich) +
  theme_bw() +
  theme(axis.text = element_blank(), axis.ticks = element_blank()) +
  labs(title = "Riqueza de especies", x = "", y = "")

plot_rich_0315tree <- ggplot() +
  geom_sf(data = mexico_map, fill = NA, color = "black") +
  geom_raster(data = long_rast_df[scale == "0.315" & spSet == 'TRUE' & measure == "richness",],
              aes(x = x, y = y, fill = value)) +
  coord_sf(xlim = c(-120, -85), ylim = c(14, 33)) +
  scale_fill_viridis_c(name = "Riqueza", limits = rango_rich) +
  theme_bw() +
  theme(axis.text = element_blank(), axis.ticks = element_blank()) +
  labs(title = "Riqueza para las especies compartidas entre el árbol filogenético y la base de datos", x = "", y = "")



library(patchwork)
g <- plot_rich_315 + plot_rich_0315tree

ggsave("data/out/all_maps/scale_315/richness_315.png", g, 
       width =35 , height = 15, units = 'cm')

#Guardalas por separado para la tesis
ggsave("data/out/all_maps/scale_315/richness_315_not_tree.png", plot_rich_315, 
       width =35 , height = 15, units = 'cm')
ggsave("data/out/all_maps/scale_315/richness_315_tree.png", plot_rich_0315tree, 
       width =35 , height = 15, units = 'cm')

##ENDEMISMO
# Calcular los límites conjuntos de ambos datasets
max_val <- max(long_rast_df[scale == "0.315" & measure == "wendemism", ]$value, na.rm = TRUE)
min_val <- min(long_rast_df[scale == "0.315" & measure == "wendemism", ]$value, na.rm = TRUE)


plot_endemismo315 <- ggplot() +
  geom_sf(data = mexico_map, fill = NA, color = "black") +
  geom_raster(data = long_rast_df[scale == "0.315" & measure == "wendemism",],
              aes(x = x, y = y, fill = value)) +
  coord_sf(xlim = c(-120, -85), ylim = c(14, 33)) +
  scale_fill_viridis_c(option = "plasma", name = "Endemismo ponderado", limits = c(min_val, max_val),trans = "sqrt") +
  theme_bw() +
  theme(axis.text = element_blank(), axis.ticks = element_blank()) +
  labs(title = "Endemismo ponderado", x = "", y = "")

plot_endemismo315tree <- ggplot() +
  geom_sf(data = mexico_map, fill = NA, color = "black") +
  geom_raster(data = long_rast_df[scale == "0.315" & spSet == 'TRUE' & measure == "wendemism",],
              aes(x = x, y = y, fill = value)) +
  coord_sf(xlim = c(-120, -85), ylim = c(14, 33)) +
  scale_fill_viridis_c(option = "plasma", name = "Endemismo ponderado", limits = c(min_val, max_val),trans = "sqrt") +
  theme_bw() +
  theme(axis.text = element_blank(), axis.ticks = element_blank()) +
  labs(title = "Endemismo ponderado para las especies compartidas entre el árbol filogenético y la base de datos", x = "", y = "")

library(patchwork)
g <- plot_endemismo315 | plot_endemismo315tree

#Guardalas por separado para la tesis
ggsave("data/out/all_maps/scale_315/we_315_not_tree_sqrt.png", plot_endemismo315, 
       width =35 , height = 15, units = 'cm')
ggsave("data/out/all_maps/scale_315/we_315_tree_sqrt.png", plot_endemismo315tree, 
       width =35 , height = 15, units = 'cm')


ggsave("data/out/all_maps/scale_315/WE_315.png", g, 
       width =35 , height = 15, units = 'cm')

plot_pd <- ggplot() +
  geom_sf(data = mexico_map, fill = NA, color = "black") +
  geom_raster(data = long_rast_df[scale == "0.315" & spSet == 'TRUE' & measure == "PD",],
              aes(x = x, y = y, fill = value)) +
  coord_sf(xlim = c(-120, -85), ylim = c(14, 33)) +
  scale_fill_viridis_c(name = "PD") +
  theme_bw() +
  theme(axis.text = element_blank(), axis.ticks = element_blank()) +
  labs(title = "Diversidad filogenética", x = "", y = "")

ggsave("data/out/all_maps/scale_315/PD_315_espanol.png", plot_pd, 
       width =25 , height = 15, units = 'cm')

library(terra)
library(dplyr)
library(stringr)

########### Richness ##################


# Shapefile con provincias (debe tener atributos "JJM2017" y "Regiones")
provincias <- vect("data/in/biogeo_provinces/all_bio_regions/pbiogmx17gw.shp")

# Cargar un raster de riqueza
rich <- rast("data/out/richness_tif/richness_tif_0.315.tif")

# Extraer valor por celda + provincia
extra <- terra::extract(rich, provincias)

# Esto devuelve algo así:
#   ID  richness_tif_0.225
#  1   1        34
#  2   1        28
#  3   2        12
# donde ID enlaza con provincias

# Agregar un identificador de celda (row number)
tabla <- extra %>%
  left_join(as.data.frame(provincias)[,c("ID_JJM2017","JJM2017","Regiones")],
            by = c("ID" = "ID_JJM2017"))
head(tabla)

write.csv(tabla, "data/out/measures_per_cell/rich_0.315_per_cell.csv")
  

############## Endemism ###################

# Cargar un raster de riqueza
endemism <- rast("data/out/wendemism_tif/wendemism_tif_0.315.tif")

# Extraer valor por celda + provincia
extra <- terra::extract(endemism, provincias)

# Agregar un identificador de celda (row number)
tabla <- extra %>%
  left_join(as.data.frame(provincias)[,c("ID_JJM2017","JJM2017","Regiones")],
            by = c("ID" = "ID_JJM2017"))
head(tabla)

write.csv(tabla, "data/out/measures_per_cell/endemism_0.315_per_cell.csv")



############## Richness species in the tree ###################

# Cargar un raster de riqueza
rich_tree <- rast("data/out/richness_tif_tree_species/richness_tif_0.315.tif")

# Extraer valor por celda + provincia
extra <- terra::extract(rich_tree, provincias)

# Agregar un identificador de celda (row number)
tabla <- extra %>%
  left_join(as.data.frame(provincias)[,c("ID_JJM2017","JJM2017","Regiones")],
            by = c("ID" = "ID_JJM2017"))
head(tabla)

write.csv(tabla, "data/out/measures_per_cell/richTree_0.315_per_cell.csv")



############## Endemism species in the tree ###################

# Cargar un raster de riqueza
endemism_tree <- rast("data/out/wendemism_tif_tree/wendemism_tif_0.315.tif")

# Extraer valor por celda + provincia
extra <- terra::extract(endemism_tree, provincias)

# Agregar un identificador de celda (row number)
tabla <- extra %>%
  left_join(as.data.frame(provincias)[,c("ID_JJM2017","JJM2017","Regiones")],
            by = c("ID" = "ID_JJM2017"))
head(tabla)

write.csv(tabla, "data/out/measures_per_cell/endemismTree_0.315_per_cell.csv")


############## PD ###################

# Cargar un raster de riqueza
PD <- rast("data/out/PD_tif/PD_tif_0.315.tif")

# Extraer valor por celda + provincia
extra <- terra::extract(PD, provincias)

# Agregar un identificador de celda (row number)
tabla <- extra %>%
  left_join(as.data.frame(provincias)[,c("ID_JJM2017","JJM2017","Regiones")],
            by = c("ID" = "ID_JJM2017"))
head(tabla)

write.csv(tabla, "data/out/measures_per_cell/PD_0.315_per_cell.csv")



####### Hacer histogramas por provincia y para cada medida #########
library(ggplot2)
library(dplyr)
library(readr)
library(patchwork)

####RIQUEZA
# Leer tabla
rich_df <- read_csv("data/out/measures_per_cell/rich_0.315_per_cell.csv")

# Remover NA y SR = 0
rich_df <- rich_df %>% 
  filter(!is.na(SR), SR > 0)

# Revisar rango
summary(rich_df$SR)

# Definir breaks (intervalos)
rich_df <- rich_df %>%
  mutate(
    clase_rich = cut(
      SR,
      breaks = c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45),
      include.lowest = FALSE,  # excluir 0 del primer intervalo
      right = TRUE
    ),
    clase_rich = factor(clase_rich, levels = levels(clase_rich), ordered = TRUE)
  )

levels(rich_df$clase_rich)

# Traducir nombres de provincias
province_en <- c(
  "Californian province",
  "Baja Californian province",
  "Sonoran province",
  "Chihuahuan Desert province",
  "Tamaulipas province",
  "Sierra Madre Occidental province",
  "Sierra Madre Oriental province",
  "Transmexican Volcanic Belt province",
  "Sierra Madre del Sur province",
  "Chiapas Highlands province",
  "Pacific Lowlands province",
  "Balsas Basin province",
  "Veracruzan province",
  "Yucatan Peninsula Province"
)

province_es <- c(
  "California",
  "Baja California",
  "Sonorense",
  "Desierto Chihuahuense",
  "Tamaulipas",
  "Sierra Madre Occidental",
  "Sierra Madre Oriental",
  "Eje Volcánico Transmexicano",
  "Sierra Madre del Sur",
  "Altiplano de Chiapas",
  "Llanuras del Pacífico",
  "Cuenca Balsas",
  "Provincia Veracruzana",
  "Península de Yucatán"
)
# Reemplazar nombres en el data frame
rich_df$JJM2017 <- factor(rich_df$JJM2017, 
                          levels = province_en, 
                          labels = province_es)

# Función para histogramas por provincia
plot_hist_province <- function(province_name, data){
  ggplot(data %>% filter(JJM2017 == province_name), aes(x = clase_rich, fill = JJM2017)) +
    geom_bar(color = "black") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
    labs(
      title = province_name,
      x = "Clase de riqueza de especies",
      y = "Número de celdas"
    ) +
    theme_minimal(base_size = 16) +
    theme(
      legend.position = "none",
      plot.title = element_text(hjust = 0.5)
    ) +
    scale_fill_manual(values = c("#1b9e77"))
}

# Crear histogramas para todas las provincias
provinces <- levels(rich_df$JJM2017)
plots <- lapply(provinces, plot_hist_province, data = rich_df)
names(plots) <- provinces

# Combinar todos los plots
combined_plot <- wrap_plots(plots, ncol = 3) + 
  plot_annotation(title = "Distribución de la riqueza de especies por provincia") &
  theme(
    text = element_text(size = 22),       # tamaño de texto global para todos los subplots
    plot.title = element_text(size = 32, hjust = 0.5)  # tamaño del título principal
  )

# Guardar o mostrar
ggsave("data/out/histograms_per_prov/sr_per_province_same_scale2.png", combined_plot, width = 30, height = 45, dpi = 300)



### ENDEMISMO
# Leer tabla
we_df <- read_csv("data/out/measures_per_cell/endemism_0.315_per_cell.csv")

#we_df <- we_df %>% filter(!is.na(WE))

we_df <- we_df %>% 
filter(!is.na(WE), WE > 0)

# Revisar rango
summary(we_df$WE)

# Crear breaks (intervalos)
# Bin 1: 0.00 – 0.5
# Bin 2: 0.5 – 1
# Bin 3: 1 – 1.5
# Bin 4: 1.5 – 2

we_df <- we_df %>%
  mutate(
    clase_we = cut(
      WE,
      breaks = c(0, 0.25,0.5,0.75,1,1.25,1.50,1.75,2),
      include.lowest = TRUE,
      right = TRUE
    ),
    clase_we = factor(clase_we, levels = levels(clase_we), ordered = TRUE)
  )

levels(we_df$clase_we)

# Reemplazar los nombres en el data frame
we_df$JJM2017 <- factor(we_df$JJM2017, 
                        levels = province_en, 
                        labels = province_es)

# Función para hacer los histogramas por provincia
plot_hist_province <- function(province_name, data){
  ggplot(data %>% filter(JJM2017 == province_name), aes(x = clase_we, fill = JJM2017)) +
    geom_bar(color = "black") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
    labs(
      title = province_name,
      x = "Clase de endemismo ponderado",
      y = "Número de celdas"
    ) +
    theme_minimal(base_size = 22) +
    theme(
      legend.position = "none",
      plot.title = element_text(hjust = 0.5)
    ) +
    scale_fill_manual(values = c("#d95f02"))
}

# Crear histogramas para todas las provincias
provinces <- levels(we_df$JJM2017)

plots <- lapply(provinces, plot_hist_province, data = we_df)
names(plots) <- provinces

# Combinar todos los plots
combined_plot <- wrap_plots(plots, ncol = 3) + 
  plot_annotation(title = "Distribución del endemismo ponderado por provincia")  &
  theme(
    text = element_text(size = 22),       # tamaño de texto global para todos los subplots
    plot.title = element_text(size = 32, hjust = 0.5)  # tamaño del título principal
  )

# Guardar o mostrar
ggsave("data/out/histograms_per_prov/we_per_province_same_scale3.png", combined_plot, width = 30, height = 45, dpi = 300)

### DIVERSIDAD FILOGENETICA
# Leer tabla
pd_df <- read_csv("data/out/measures_per_cell/PD_0.315_per_cell.csv")

pd_df <- pd_df %>% filter(!is.na(PD), PD > 0)

# Revisar rango
summary(pd_df$PD)

# Crear breaks (intervalos)
pd_df <- pd_df %>%
  mutate(
    clase_pd = cut(
      PD,
      breaks = c(54,138,220,302,384),
      include.lowest = TRUE,
      right = TRUE
    ),
    clase_pd = factor(clase_pd, levels = levels(clase_pd), ordered = TRUE)
  )
levels(pd_df$clase_pd)



# Reemplazar nombres en el data frame
pd_df$JJM2017 <- factor(pd_df$JJM2017, 
                          levels = province_en, 
                          labels = province_es)

# Función para hacer los histogramas por provincia
plot_hist_province <- function(province_name, data){
  ggplot(data %>% filter(JJM2017 == province_name), aes(x = clase_pd, fill = JJM2017)) +
    geom_bar(color = "black") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
    labs(
      title = province_name,
      x = "Clase de diversidad filogenética ponderada",
      y = "Número de celdas"
    ) +
    theme_minimal(base_size = 22) +
    theme(
      legend.position = "none",
      plot.title = element_text(hjust = 0.5)
    ) +
    scale_fill_manual(values = c("skyblue"))
}


#funcion para hacer los histogramas
provinces <- unique(pd_df$JJM2017)
plots <- lapply(provinces, plot_hist_province, data = pd_df)
names(plots) <- provinces

combined_plot <- wrap_plots(plots, ncol = 3) + 
  plot_annotation(title = "Distribución de la diversidad filogenética ponderado por provinciaPhylogenetic diversity distribution per province")  &
  theme(
    text = element_text(size = 22),       # global text size for all subplots
    plot.title = element_text(size = 32, hjust = 0.5)  # annotation title size
  )


# Save or display
ggsave("data/out/histograms_per_prov/pd_per_province_same_scale2.png", combined_plot, width = 30, height = 45, dpi = 300)


####### RIQUEZA DE ESPECIES EN EL ARBOL
# Leer tabla
rich_df <- read_csv("data/out/measures_per_cell/richTree_0.315_per_cell.csv")

rich_df <- rich_df %>% filter(!is.na(SR), SR > 0)

# Revisar rango
summary(rich_df$SR)

# Crear breaks (intervalos)
# --- 1. Ensure clase_rich is a proper ordered factor
rich_df <- rich_df %>%
  mutate(
    clase_rich = cut(
      SR,
      breaks = c(0, 5, 10, 15, 20, 25, 30, 34),
      include.lowest = TRUE,
      right = TRUE
    ),
    clase_rich = factor(clase_rich, levels = levels(clase_rich), ordered = TRUE)
  )

levels(rich_df$clase_rich)

# Reemplazar nombres en el data frame
rich_df$JJM2017 <- factor(rich_df$JJM2017, 
                        levels = province_en, 
                        labels = province_es)

#funcion para hacer los histogramas

plot_hist_province <- function(province_name, data){
  ggplot(data %>% filter(JJM2017 == province_name), aes(x = clase_rich, fill = JJM2017)) +
    geom_bar(color = "black") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
    labs(title = province_name,
         x = "Clase de riqueza de especies",
         y = "Número de celdas") +
    theme_minimal(base_size = 16) +
    theme(legend.position = "none",
          plot.title = element_text(hjust = 0.5)) +
    scale_fill_manual(values = c("#436EEE"))
}

provinces <- unique(rich_df$JJM2017)
plots <- lapply(provinces, plot_hist_province, data = rich_df)
names(plots) <- provinces

combined_plot <- wrap_plots(plots, ncol = 3) + 
  plot_annotation(title = "Distribución de la riqueza de especies presentes en el árbol filogenético por provincia") &
  theme(
    text = element_text(size = 22),       # global text size for all subplots
    plot.title = element_text(size = 32, hjust = 0.5)  # annotation title size
  )


# Save or display
ggsave("data/out/histograms_per_prov/sr_tree_per_province_same_scale2.png", combined_plot, width = 30, height = 45, dpi = 300)


### ENDEMISMO DE ESPECIES PRESENTES EN EL ARBOL
# Leer tabla
we_df <- read_csv("data/out/measures_per_cell/endemismTree_0.315_per_cell.csv")

we_df <- we_df %>% filter(!is.na(WE), WE > 0)

# Revisar rango
summary(we_df$WE)

# Crear breaks (intervalos)

we_df <- we_df %>%
  mutate(
    clase_we = cut(
      WE,
      breaks = c(0, 0.25,0.5,0.75,1,1.25,1.5),
      include.lowest = TRUE,
      right = TRUE
    ),
    clase_we = factor(clase_we, levels = levels(clase_we), ordered = TRUE)
  )

levels(we_df$clase_we)

# Reemplazar nombres en el data frame
we_df$JJM2017 <- factor(we_df$JJM2017, 
                          levels = province_en, 
                          labels = province_es)


#funcion para hacer los histogramas

plot_hist_province <- function(province_name, data){
  ggplot(data %>% filter(JJM2017 == province_name), aes(x = clase_we, fill = JJM2017)) +
    geom_bar(color = "black") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
    labs(title = province_name,
         x = "Clase de endemismo ponderado",
         y = "Número de celdas") +
    theme_minimal(base_size = 22) +
    theme(legend.position = "none",
          plot.title = element_text(hjust = 0.5)) +
    scale_fill_manual(values = c("purple"))
}

provinces <- unique(we_df$JJM2017)
plots <- lapply(provinces, plot_hist_province, data = we_df)
names(plots) <- provinces

combined_plot <- wrap_plots(plots, ncol = 3) + 
  plot_annotation(title = "Distribución del endemismo ponderado de especies presentes en el árbol filogenético por provincia")  &
  theme(
    text = element_text(size = 22),       # global text size for all subplots
    plot.title = element_text(size = 32, hjust = 0.5)  # annotation title size
  )

# Save or display
ggsave("data/out/histograms_per_prov/we_tree_per_province_same_scale3.png", combined_plot, width = 30, height = 45, dpi = 300)

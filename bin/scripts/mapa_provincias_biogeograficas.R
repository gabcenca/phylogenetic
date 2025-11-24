library(sf)
library(tidyverse)
library(here)
library(data.table)
library(ggplot2)

# Read the biogeographic provinces of Mexico
provinces <- st_read("data/in/biogeo_provinces/all_bio_regions/pbiogmx17gw.shp")

provinces$JJM2017_es <- dplyr::recode(provinces$JJM2017,
                                      "Baja Californian province" = "Baja California",
                                      "Balsas Basin province" = "Cuenca del Balsas",
                                      "Californian province" = "Californiana",
                                      "Chiapas Highlands province" = "Altos de Chiapas",
                                      "Chihuahuan Desert province" = "Desierto Chihuahuense",
                                      "Pacific Lowlands province" = "Llanuras del Pacífico",
                                      "Sierra Madre Occidental province" = "Sierra Madre Occidental",
                                      "Sierra Madre Oriental province" = "Sierra Madre Oriental",
                                      "Sierra Madre del Sur province" = "Sierra Madre del Sur",
                                      "Sonoran province" = "Sonorense",
                                      "Tamaulipas province" = "Tamaulipas",
                                      "Transmexican Volcanic Belt province" = "Eje Volcánico Transmexicano",
                                      "Veracruzan province" = "Veracruzana",
                                      "Yucatan Peninsula Province" = "Península de Yucatán"
)

library(RColorBrewer)

colores <- c(brewer.pal(12, "Set3"), brewer.pal(2, "Set1"))

provinces$numero <- 1:nrow(provinces)  # Numera del 1 al total de polígonos

library(ggspatial)
library(MetBrewer)

paleta <- MetBrewer::met.brewer("Signac", 14)

g<-ggplot(provinces) +
  geom_sf(aes(fill = JJM2017_es)) +
  theme_minimal() +
  labs(fill = "Provincia biogeográfica") +
  scale_fill_manual(values = colores) +
  theme(
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 12)
  ) +
  annotation_north_arrow(
    location = "tr",          # "tl", "tr", "bl", "br"
    which_north = "true",     # true north
    style = north_arrow_fancy_orienteering) #  +
  # annotation_scale(
  #   location = "bl",          # "tl", "tr", "bl", "br"
  #   width_hint = 0.3          # ancho relativo de la barra de escala
  # )

ggsave("data/out/biogeographic_provinces_map/biogeoprov.jpg", g, width =45 , height = 22, units = 'cm')


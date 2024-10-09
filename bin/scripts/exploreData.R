######
# Script : Explorar y limpiar los datos de registros de encinos
# Author: Gabriela Centeno y Sofía Zorrilla
# Date: 01/07/2024
# Description: 
# Usage: 
# Arguments:
#   - Input: 
#   - Output: 
#######

# --- Load libraries ---

library(dplyr)
library(ggplot2)
library(maps)
library(ggthemes)
library(sf)
library(BIEN)


# --- Load data ---

quercus_BienData <- read.csv("data/in/BIENdata_csv/quercusBienData.csv")


# --- Class, dimension and head of data ---

class(quercus_BienData)

dim(quercus_BienData)

names(quercus_BienData)


# --- Filter registres from Mexico ---
records_BIEN_mex <- quercus_BienData %>%
  filter(country=="Mexico")

head(reg_mex_quercus)
dim(reg_mex_quercus)

#save the filter
write.csv(records_BIEN_mex, "data/in/dataBIENmex_csv/dataBIENmex.csv", row.names = TRUE)



# --- Number of species ---
num_species <- records_BIEN_mex %>% 
  summarise(total_species = n_distinct(scrubbed_species_binomial))

print(num_species)


# --- Number of registers per specie ---
register_per_especie <- records_BIEN_mex %>% 
  group_by(scrubbed_species_binomial) %>% 
  summarise(num_registros = n()) %>%
  arrange(num_registros)

print(register_per_especie)


### --- Species that have less than 5 records ---

less_records <- register_per_especie %>%
  filter(num_registros < 5)

print(less_records)



# --- Base map of Mexico ---
map_mexico <- map_data("world") %>%
  filter(region == "Mexico")

# --- Cleaning of missing or out-of-range values ---  
reg_mex_quercus_clean <- reg_mex_quercus %>%
  filter(!is.na(latitude) & !is.na(longitude)) %>%
  filter(longitude >= -180 & longitude <= 180 & latitude >= -90 & latitude <= 90)

# --- Missing values that were eliminated (NA) ---
reg_deleted <- reg_mex_quercus %>%
  filter(is.na(latitude) | is.na(longitude) |
           longitude < -180 | longitude > 180 |
           latitude < -90 | latitude > 90)


# --- Plot the map ---
mapa_reg_mexico <- ggplot() +
  geom_polygon(data = map_mexico, aes(x = long, y = lat, group = group), fill = "lightgray", color = "white") +
  geom_point(data = reg_mex_quercus_clean, aes(x = longitude, y = latitude), alpha = 0.6, size = 2, color = "darkred") +
  coord_fixed(1.3) +
  labs(title = "Registros de Quercus en México",
       x = "Longitud",
       y = "Latitud") +
  theme_minimal() +
  theme(legend.position = "none")

print(mapa_reg_mexico)

# --- Range of each specie ---


genus_vector <- c("Quercus")

BIEN_ranges_genus(genus = "Quercus", 
                  directory = "out/range_test", matched = TRUE, 
                  match_names_only = FALSE, include.gid = FALSE)

# --- Plot of the range of Quercus Castanea --- 

Quercus_castanea_range <- read_sf(dsn = "out/range_test/Quercus_castanea.shp")

# -- world --

plot(Quercus_castanea_range[1])

map('world', fill = TRUE, col = "grey")

plot(Quercus_castanea_range[1], col="forest green", add = TRUE)

# -- country --  no queda ajustado

map("world", regions = "Mexico", fill = TRUE, col = "grey")
plot(Quercus_castanea_range[1], col="forest green", add = TRUE)

# -- ggplot/sf -- buscar como hacerlo, geomap


# BIEN_ranges_sf(sf, directory = NULL, species.names.only = FALSE, 
#return.species.list = TRUE, crop.ranges = FALSE, include.gid = FALSE, ... )
# Funcion: Download range maps that intersect a user-supplied sf object.



### Information on when the observation was made/collected is recorded 
### in the field date_collected

reg_mex_quercus$date_collected
reg_mex_quercus$datasource


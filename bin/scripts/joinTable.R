######
# Script : Join tables of gbif, herbario mexicano and BIEN package
# Author: Gabriela Centeno y Sofía Zorrilla
# Date: 23/09/2024
# Description: 
# Usage: 
# Arguments:
#   - Input: 
#   - Output: 
#######

# --- Load libraries ---
library(dplyr)
library(readr)


# --- Load tables ---
recordsGbif <- read_delim("data/in/gbif_csv/gbif.csv", 
                          delim = "\t", escape_double = FALSE, 
                          trim_ws = TRUE)

recordsHerbario <- read_csv("data/in/herbariomex_dw/occurrences.csv")

recordsBIEN <- read_csv("data/in/dataBIENmex_csv/dataBIENmex.csv")


# --- Add a unique id to each value --- 
recordsGbif <- recordsGbif %>%
  mutate(valueID = paste0("GBIF_", row_number()))

recordsHerbario <- recordsHerbario %>%
  mutate(valueID = paste0("Herb_", row_number()))

recordsBIEN <- recordsBIEN %>%
  mutate(valueID = paste0("BIEN_", row_number()))


# Convertir todas las columnas de recordsGbif a character
recordsGbif <- recordsGbif %>%
  mutate_all(as.character)

# Convertir todas las columnas de recordsHerbario a character
recordsHerbario <- recordsHerbario %>%
  mutate_all(as.character)

# Convertir todas las columnas de recordsBIEN a character
recordsBIEN <- recordsBIEN %>%
  mutate_all(as.character)

#Juntar df de gbif y herbario 
herbGbif <- bind_rows(recordsHerbario,recordsGbif)
names(herbGbif)

#Seleccionar las columnas de interes 
herbGbif <- herbGbif %>%
  select("scientificName", "eventDate","year", 
         "month","day","occurrenceRemarks",
         "habitat", "stateProvince","county", "municipality","locality",
         "decimalLatitude", "decimalLongitude","minimumElevationInMeters",
         "maximumElevationInMeters","valueID","basisOfRecord",
         "individualCount","verbatimScientificNameAuthorship") %>%
  mutate("elevation_m"="NA","datasource"="NA")  #Agregar esta columna para agregar las elevaciones de BIEN


#Seleccionar los registros de BIEN de interes y reenombrarlos para que coincidan con gbif/herbario
recordsBien <- recordsBIEN %>%
  select("scrubbed_species_binomial","state_province", "county","locality", 
         "elevation_m","date_collected","datasource",
         "latitude","longitude","valueID") %>%
  rename("scientificName"="scrubbed_species_binomial",
         "stateProvince"="state_province", 
         "eventDate"="date_collected",
         "decimalLatitude"="latitude",
         "decimalLongitude"="longitude")


names(recordsBIEN)

#Juntar df de gbif/herbario con Bien
herbGbifBien <- bind_rows(herbGbif,recordsBien)
names(herbGbif)

#Guardar df
write.csv(herbGbifBien, 
          "data/temp/herbaGbifBien_df/herbGbifBien.csv", row.names = TRUE)


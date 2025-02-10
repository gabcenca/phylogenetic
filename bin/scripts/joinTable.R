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
library(data.table)

##Dividir dwc en 3 para guardarlo en github

# --- Load tables ---
recordsGbif_raw <- read_delim("data/in/gbif_dwc/occurrence.txt", 
                          delim = "\t", escape_double = FALSE, 
                          trim_ws = TRUE)

tercio <- round(nrow(recordsGbif_raw)/3)

list_recordsGbif_raw <- list(recordsGbif_raw[1:tercio,],
                             recordsGbif_raw[(tercio+1):(2*tercio),],
                             recordsGbif_raw[(2*tercio+1):nrow(recordsGbif_raw),])

lapply(list_recordsGbif_raw, dim)

lapply(seq_along(list_recordsGbif_raw), function(x){write.csv(list_recordsGbif_raw[[x]],
      file = paste0("data/in/gbif_dwc/division_occurrence/occurrence_",x,".csv"), row.names=F)})

## Upload data frames 

recordsHerbario_raw <- fread("data/in/herbariomex_dw/occurrences.csv")

recordsBIEN_raw <- fread("data/in/dataBIENmex_csv/dataBIENmex.csv")


# Load tables from gbif 
files_list<- list.files("data/in/gbif_dwc/division_occurrence/", pattern=".csv", full.names=T)

files <- lapply(files_list,fread)

recordsGbif_raw <- do.call(rbind,files)


# --- Add a unique id to each value --- 
recordsGbif <- recordsGbif_raw %>%
  mutate(id_interno = paste0("GBIF_", row_number()), taxonID=)

recordsHerbario <- recordsHerbario_raw %>%
  mutate(id_interno = paste0("Herb_", row_number()))

recordsBIEN <- recordsBIEN_raw %>%
  mutate(id_interno = paste0("BIEN_", row_number()))


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
herbGbif_raw <- bind_rows(recordsHerbario,recordsGbif)
names(herbGbif_raw)


#For columns in herbario not in gbif columns
antijoin_herbgbif <- recordsHerbario %>%
  anti_join(recordsGbif, 
            # Define equivalence in column names in both df
            by = "scientificName")
unique(antijoin2$scientificName)

#Seleccionar las columnas de interes 
herbGbif <- herbGbif_raw %>%
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


library(readr)
library(dplyr)
library(skimr)
library(rgbif)


herbGbifBien <- read_csv("data/temp/herbaGbifBIEN_df/herbGbifBien.csv")

#Ver un resumen de mis datos
glimpse(herbGbifBien)
head(herbGbifBien)
skim(herbGbifBien)

# Extraer los nombres científicos únicos
species_names <- unique(herbGbifBien$scientificName)

# Validar los nombres científicos en GBIF
validated_species <- lapply(species_names, function(x) name_backbone(name = x))

# Mostrar los resultados
validated_species
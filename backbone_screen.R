library(dplyr)
library(readr)
library(data.table)
library(ggplot2)
library(here)
library(sf)
library(here)
library(rgbif)

files_list <- list.files(here::here("data/in/gbif_dwc/division_occurrence"), pattern=".csv", full.names=TRUE)


files <- lapply(files_list,fread)

recordsGbif_raw <- do.call(rbind,files)

# --- Load the Red de Herbarios Mexicanos data frame ---
recordsHerbario_raw <- fread(here::here("data/in/herbariomex_dw/occurrences.csv"))


match_species <- function(name, index = NULL, total = NULL) {
  if (!is.null(index) && !is.null(total)) {
    cat(sprintf("Procesando %d de %d: %s\n", index, total, name))
  } else {
    cat(sprintf("Procesando: %s\n", name))
  }
  
  result <- name_backbone(name = name)
  
  if (is.null(result)) {
    return(data.frame(verbatim_name = name, usageKey = NA, canonicalName = NA, status = NA, confidence = NA))
  } else {
    result_df <- as.data.frame(result)
    result_df$verbatim_name <- name
    return(result_df)
  }
}

scientific_names_gbif <- recordsGbif_raw %>% pull(scientificName)
total_gbif_names <- length(scientific_names_gbif)

backbone_gbif <- lapply(seq_along(scientific_names_gbif), function(i) {
  match_species(scientific_names_gbif[i], index = i, total = total_gbif_names)
}) %>% bind_rows()


scientific_names_herb <- recordsHerbario_raw %>% pull(scientificName)
total_herb_names <- length(scientific_names_herb)

backbone_herb <- lapply(seq_along(scientific_names_herb), function(i) {
  match_species(scientific_names_herb[i], index = i, total = total_herb_names)
}) %>% bind_rows()


backbone_gbif <- recordsGbif_raw %>%
  pull(scientificName) %>%                      # Extrae solo la columna species
  lapply(match_species) %>%              # Aplica la función a cada nombre
  bind_rows()                            # Combina los resultados


backbone_herb <- recordsHerbario_raw %>%
  pull(scientificName) %>%                      
  lapply(match_species) %>%              
  bind_rows()  


# Save it
write_csv(backbone_gbif, 
          here::here("data/in/backbone/gbif_nombres_backbone.csv"))

write_csv(backbone_herb, 
          here::here("data/in/backbone/herb_final_backbone.csv"))


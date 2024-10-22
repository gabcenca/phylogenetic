######
# Script : Descargar y guardar los datos de registros de encinos
# Author: Gabriela Centeno y Sofía Zorrilla
# Date: 20/06/2024
# Description: 
# Usage: 
# Arguments:
#   - Input: 
#   - Output: 
#######

# --- Load libraries ---

library(BIEN)
library(magrittr)

library(rgbif)
library("usethis")
#usethis::edit_r_environ()  #setting gbif account
library(readr)

# --- Load functions ---

write_data_genus <- function(name){
  BIEN_occurrence_genus(cultivated = F, new.world = T, observation.type = T,political.boundaries = T, genus = name, native.status = T) %>% 
  BIEN_occurrence_genus(cultivated = F, new.world = T, observation.type = T,political.boundaries = T, genus = name) %>% 
    write.csv(file = "quercusBienData.csv", row.names = F)
}

## Script
write_data_genus(name = "Quercus")

### download gbif taxonomic catalog ###

download_request <- occ_download(
  pred("datasetKey", "7f707904-f762-11e1-a439-00145eb45e9a"),
  format = "SIMPLE_CSV")

print(download_request)  #status of the gbif download

species_quercus <- read_delim("data/in/TaxonomiaQuercus/0010521-240906103802322.csv", 
                              delim = "\t", escape_double = FALSE, 
                              trim_ws = TRUE)


### download quercus records in gbif for Mexico ###

name_backbone("Quercus") # get best match in the GBIF backbone

#download all records of quercus
gbif_quercus <- occ_download(pred("taxonKey", 2877951),format = "DWCA")

#download all records from mexico 
gbif_quercus_mex <- occ_download(
  pred("hasGeospatialIssue", FALSE),
  pred("hasCoordinate", TRUE),
  pred("taxonKey", 2877951),
  pred("country", "MX"),
  format = "DWCA"
)

occ_download_wait(gbif_quercus_mex)

records_gbif_dwc <- occ_download_get(gbif_quercus_mex) %>%
  occ_download_import()
names(records_gbif)


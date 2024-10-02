library(rgbif)
library("usethis")
usethis::edit_r_environ()
library(readr)


### download gbif taxonomic catalog ###

download_request <- occ_download(
  pred("datasetKey", "7f707904-f762-11e1-a439-00145eb45e9a"),
  format = "SIMPLE_CSV"
)

occ_download_wait(download_request)

autoridad_taxonomica <- occ_download_get(download_request) %>%
  occ_download_import()

names(records_gbif)

tax <- read_delim("data/in/TaxonomiaQuercus/0010521-240906103802322.csv", 
                              delim = "\t", escape_double = FALSE, 
                              trim_ws = TRUE)


### download quercus records in gbif ###

name_backbone("Quercus") # get best match in the GBIF backbone

#download records
gbif_quercus <- occ_download(
  pred("hasGeospatialIssue", FALSE),
  pred("hasCoordinate", TRUE),
  pred("taxonKey", 2877951),
  pred("country", "MX"),
  format = "SIMPLE_CSV"
)

occ_download_wait(gbif_quercus)

records_gbif <- occ_download_get(gbif_quercus) %>%
  occ_download_import()
names(records_gbif)

names(records_gbif) #especie, genero, localidad, estado, toda la informacion geografica, elevacion, precision elevacion, desglosada fecha,basisOfRecord, institutionCode 

presence_status <- records_gbif


  
institutes_gbif <- records_gbif %>%
  select(basisOfRecord, institutionCode) %>%
  distinct()
  
### records of Red de Herbarios Mexicanos ###

records_labs <- read_csv("data/in/herbanwmex/occurrences.csv")

names(records_labs)

### --- Find synonyms in BIEN data frame ---

synonyms <- registers_BIEN_mex %>%
    select(scrubbed_species_binomial) %>%
    anti_join(species_quercus, by = c("scrubbed_species_binomial" = "species")) %>%
    distinct()

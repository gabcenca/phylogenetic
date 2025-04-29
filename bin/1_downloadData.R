######
# Script : Download and save the registers of oaks 
# Author: Gabriela Centeno y Sofía Zorrilla
# Date: 20/06/2024
# Description: 
# Usage: 
# Arguments:
#   - Input: 
#   - Output: 
#######

# --- Load libraries ---

library(rgbif)
library(here)

#With this line you need to set the gbif account so the download can proceed.
usethis::edit_r_environ()  

# --- Download all quercus records from mexico  ---

#You need to establish your search with the next parameters:
gbif_quercus_mex <- occ_download(
  pred("hasGeospatialIssue", FALSE),
  pred("hasCoordinate", TRUE),
  pred("taxonKey", 2877951),
  pred("country", "MX"),
  format = "DWCA"
)

occ_download_meta(gbif_quercus_mex)

# occ_download_meta(gbif_quercus_mex)
# <<gbif download metadata>>
# Status: SUCCEEDED
# DOI: 10.15468/dl.bzhg67
# Format: DWCA
# Download key: 0005520-250426092105405
# Created: 2025-04-29T16:34:53.239+00:00
# Modified: 2025-04-29T16:48:44.398+00:00
# Download link: https://api.gbif.org/v1/occurrence/download/request/0005520-250426092105405.zip
# Total records: 70558

# gbif_citation("0005520-250426092105405")
# $download
# [1] "GBIF Occurrence Download https://doi.org/10.15468/dl.bzhg67 Accessed from R via rgbif (https://github.com/ropensci/rgbif) on 2025-04-29"


#Then you will import the data
records_gbif_dwc <- occ_download_get(gbif_quercus_mex, 
                                     path = here::here("data/in/gbif_dwc/"),
                                     overwrite = TRUE) %>%
  occ_download_import()

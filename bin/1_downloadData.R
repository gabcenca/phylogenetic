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

#Then you will import the data
records_gbif_dwc <- occ_download_get(gbif_quercus_mex, 
                                     path = here::here("data/in/gbif_dwc/")) %>%
  occ_download_import()

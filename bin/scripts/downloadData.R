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

# --- Load functions ---

write_data_genus <- function(name){
  BIEN_occurrence_genus(cultivated = F, new.world = T, observation.type = T,political.boundaries = T, genus = name) %>% 
    write.csv(file = "quercusBienData.csv", row.names = F)
}

## Script
write_data_genus(name = "Quercus")

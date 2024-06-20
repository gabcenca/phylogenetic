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

write_data_sp <- function(name){
  BIEN_occurrence_species(cultivated = F, observation.type = T, species = name) %>% 
    write.csv(file = "quercusBienData.csv", row.names = F)
}

## Script

write_data_sp(name = "Quercus brandegeei")

######
# Script : Relación entre riqueza y número de registros
# Author: Sofía Zorrilla
# Date: 2025-03-31
# Description: 
# Arguments:
#   - Input: 
#   - Output: 
#######

# Load libraries -------------------------------------------------
library(tidyverse)
library(ggplot2)
library(data.table)
library(here)


# Cargar los datos
hgbif_clean <- fread(here::here("data/in/hgbif_clean.csv"))

# Ordenar los datos aleatoriamente
set.seed(13235)

hgbif_azar <- hgbif_clean[sample(seq_len(nrow(hgbif_clean)), nrow(hgbif_clean), replace = FALSE)]



# Seleccionar progresivamente más registros


countSp_in_N_records <- function(df, n, groupping_column, sample = FALSE) {  
    
    if (sample) {
        records_by_sp <- df[sample(seq_len(nrow(df)), n, replace = FALSE), .N, by = c(groupping_column)]
    } else {
        records_by_sp <- df[seq_len(n), .N, by = c(groupping_column)]
    }

  return(nrow(records_by_sp))
}

richness_accum <- sapply(seq_len(1000), function(i) {
    print(i)
    countSp_in_N_records(df = hgbif_azar, n = i, groupping_column = "correctname")})


richness_accum_random <- sapply(seq_len(1000), function(i) {
    print(i)
    countSp_in_N_records(df = hgbif_azar, n = i, groupping_column = "correctname", sample = TRUE)})


plot(richness_accum_random, type = 'l')
lines(richness_accum, col = "blue", lwd = 3)

# Graficar la riqueza acumulada en función del número de registros

# Curvas de acumulación por región biogeográfica  (riqueza ~# cuadros)----
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
hgbif_clean <- fread("hgbif_completo_iucn.csv")

# Ordenar los datos aleatoriamente
set.seed(13235)

hgbif_azar <- hgbif_clean[sample(1:nrow(hgbif_clean), nrow(hgbif_clean), replace = FALSE)]



# Seleccionar progresivamente más registros

countSp_in_N_records <- function(df, n, groupping_column) {
  records_by_sp <- df[1:n, .N , by = c(groupping_column)]
  return(nrow(records_by_sp))
}

countSp_in_N_records(hgbif_azar, 2, "correctname")
count <- sapply(seq(1:1000),function(i){countSp_in_N_records(df = hgbif_azar, n = i, 
                                                             groupping_column = "correctname")})

plot(count, type = "l")


# Seleccionamos con reemplazamiento progresivamente más registros
countSp_in_N_records_replace <- function(df, n, groupping_column) {
  records_by_sp <- df[sample(1:nrow(df), n, replace = FALSE), .N , by = c(groupping_column)]
  return(nrow(records_by_sp))
}

count2 <- sapply(seq(1:1000),function(i){countSp_in_N_records_replace(df = hgbif_azar, n = i, 
                                                                      groupping_column = "correctname")})

plot(count2, type = "l")

#Unir las dos funciones que acabamos de hacer
# Seleccionar progresivamente más registros
countSp_in_N_records <- function(df, n, groupping_column, sample = FALSE) {  
  
  if (sample) {
    records_by_sp <- df[sample(1:nrow(df),n,replace = FALSE), .N , by = c(groupping_column)]
  } else {
    records_by_sp <- df[1:n, .N , by = c(groupping_column)]
  }
  
  return(nrow(records_by_sp))
}

richness_accum <- sapply(seq(1:nrow(hgbif_azar)),function(i){
  print(i)
  countSp_in_N_records(df = hgbif_azar, n = i, groupping_column = "correctname")})


richness_accum_random <- sapply(seq(1:nrow(hgbif_azar)),function(i){
  print(i)
  countSp_in_N_records(df = hgbif_azar, n = i, groupping_column = "correctname", sample = TRUE)})


plot(richness_accum_random, type = 'l')
lines(richness_accum, col = "blue",lwd = 3)

png("richness_accum_plot.png", width = 800, height = 600)  # Abrir dispositivo gráfico PNG

plot(richness_accum_random, type = 'l', xlab = "Número de registros", ylab = "Riqueza de especies")
lines(richness_accum, col = "blue", lwd = 3)

dev.off()  # Cerrar el dispositivo y guardar la imagen

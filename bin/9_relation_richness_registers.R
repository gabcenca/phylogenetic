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
library(dplyr)

# Cargar los datos
hgbif_clean <- fread(here::here("data/in/hgbif_completo_iucn.csv"))

orden <- hgbif_clean[,.(correctname)]

#Hacer 100 columnas con ordenes distintos de los registros, se indica el numero de fila de cada orden
#Para hacer una curva de acumulación por cada orden
for (i in 2:101) {
  orden[,paste('orden_',i)] <- sample(1:nrow(orden), nrow(hgbif_clean), replace = FALSE)
}


#' Count Unique Groups in the First N Records
#'
#' This function counts how many unique values there are in a specified grouping column
#' within the first \code{n} rows of a data table.
#'
#' @param df A \code{data.table}. The input data from which the first \code{n} rows will be considered.
#' @param n An \code{integer}. The number of top rows from the data table to consider.
#' @param groupping_column A \code{character} string. The name of the column by which to group the records.
#'
#' @return An \code{integer} indicating the number of unique groups (e.g., species) found in the top \code{n} records.
#'
#' @examples
#' library(data.table)
#' dt <- data.table(species = rep(c("sp1", "sp2", "sp3"), times = c(5, 3, 2)))
#' countSp_in_N_records(dt, 5, "species")
#'
#' @export
countSp_in_N_records <- function(df, n, groupping_column) {
  records_by_sp <- df[1:n, .N , by = c(groupping_column)]
  return(nrow(records_by_sp))
}


#Generate a loop that iterate in the columns of the orders the function countSp_in_N_records. This is to 
#finish with a data table where the columns represent the accumulation curves of each order.

repetitions_count <- list()

for(i in 2:101){
  
  #seleccionar columna i y convertirla a vector, la cual tiene el orden de las filas
  orden_filas <- c(select(orden, all_of(i))) 
  
  #ordenar la columna de correctname por el orden indicado por i
  df <- orden[order(orden_filas),.(correctname)] 
  
  #Ejecutando la funcion para contar el numero de especies respecto a n registros dados por las interacciones de 1:1000
  count <- sapply(seq(1:30000), function(i){countSp_in_N_records(df = df, n = i,
                                                                groupping_column = "correctname")})
  
  repetitions_count[[paste0('rep',i-1)]] <- count #guardar el conteo en un elemento de la lista
  
}

long_repetitions_count <- as.data.frame(repetitions_count) 
long_repetitions_count$rep_mean <- rowMeans(long_repetitions_count)

long_repetitions_count <- long_repetitions_count %>% 
  rownames_to_column('row_id') %>%
  pivot_longer(cols = starts_with('rep'),names_to = 'rep', values_to = 'richness') 

ggplot(data = long_repetitions_count,
       aes(x = as.numeric(row_id), y=richness, group = rep))+
       geom_line(alpha = 0.1, color = 'darkgray') +
       geom_line(data = filter(long_repetitions_count, rep == 'rep_mean'),
                 aes(x = as.numeric(row_id), y=richness, group = rep), linewidth = 1, color = 'red') +
  theme_bw()
  
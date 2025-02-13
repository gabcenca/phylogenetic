######
# Script : Extract IUCN category for each oak species 
# Author: Sofía Zorrilla
# Date: 2025-02-12
# Description: 
# Arguments:
#   - Input: 
#   - Output: 
#######

# --- Load packages ---

library(data.table)
library(rredlist)
library(stringr)

# To generate an API key to use IUCN information
# use rredlist::rl_use_iucn() to get instructions, 
# set up and verify an account and request an 
# API in your account


# --- Load Data ---

data <- fread("data/in/hgbif_no_duplicates_coor_completo.csv")


get_iucn <- function(genus = "Quercus", epithet){

#' Extract IUCN conservation status category for a single species
#' 
#' @genus character vector for genus
#' @species character vector for species epithet
#' 
#' @description
#' Extracts latest species assessment and from that the conservation category code 
#' 
#' @return character with conservaiton categroy 
  
  assessment <- list()
  try({
    assessment <- rl_species(genus = "Quercus", species = epithet)$assessments
  })
  
  if(length(assessment)==0){
    print(paste("Warning: no assessment for: Quercus",epithet))
    return(NA)
  }
  id <- assessment[which(assessment$latest),]$assessment_id
  
  if(length(id) > 1){
    print("Warning: assessment id > 1. Choosing global assessment.")
    scopes <- rbindlist(assessment[which(assessment$latest),]$scopes)
    id <- assessment[which(scopes$code == 1),]$assessment_id
  }
  
  
  
  conservation_status_code <- rl_assessment(id)$red_list_category$code
  print(conservation_status_code)
  return(conservation_status_code)
}

# Split correct name to extract species epithet
data[, epithet := str_split_i(correctname, " ",2 )]

# Get unique species names (215)
species <- unique(data[,.(correctname,epithet)])
species <- species[!is.na(epithet)]
species <- species[epithet != "candicans"]

# Apply get_iucn to each species name.
status_list <- lapply(as.list(species$epithet), function(x){
  print(paste("Extracting: Quercus ",x))
  return(get_iucn(epithet = x))
  Sys.sleep(0.9)
})

# Convertir los valores nulos en NA
# TODO: Revisar que el arreglo de la función esté bien y los valores nulos los regrese como NA
st_ls <- lapply(status_list, function(x){
  if(is.null(x)){
    return(NA)
  }else{
    return(x)
  }
  })

# Add IUCN conservation status to species list
species$IUCN <- unlist(st_ls)

# Save result
fwrite(species, "data/in/conservation_status/IUCN_per_species.csv")


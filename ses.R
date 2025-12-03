library(phyloraster)
library(terra)
library(ape)
library(phylobase)
library(here)
library(data.table)
library(here)
library(stringr)
library(ggplot2)
library(tidyverse)
library(SESraster)

file_list <- list.files(here::here("data/out/raster_species/"), full.names = TRUE, pattern = ".tif$")

files <- lapply(file_list, terra::rast)

data_morton <- fread(here::here("data/in/filogenia/Hipp2019_sample_metadata.csv"))
geo_data <- fread(here::here("data/in/hgbif_completo_iucn.csv"))

#TODO: Revisar si los nombres en el correctname que no coinciden con los del morton realmnete no están en la filogenia 
#TODO: Decidir si esta es la filogenia que vamos a utilizar

common_morton <- data_morton[`Cleaned_NAMES-USE-THIS` %in% geo_data$correctname,]
common_geo <- geo_data[correctname %in% common_morton$`Cleaned_NAMES-USE-THIS`]

diff<- setdiff(unique(data_morton$`Cleaned_NAMES-USE-THIS`),unique(geo_data$correctname))

common_geo <- merge(common_geo,unique(common_morton[,.(`Cleaned_NAMES-USE-THIS`,section,clade,subgenus)]), by.x = "correctname", by.y="Cleaned_NAMES-USE-THIS",all.x = T)

fwrite(common_geo, here::here('data/out/common_geo.csv'), row.names = F)


library(ape)
library(treeio)
library(rotl)

## --- Functions --- ##

#' Modify Tip Labels of a Phylogenetic Tree
#'
#' This function modifies the tip labels of a phylogenetic tree based on a user-defined string transformation function.
#' By default, it removes substrings matching '_ott.*' and replaces underscores with spaces.
#'
#' @param tree A phylogenetic tree (likely of class `phylo`) containing tip labels.
#' @param modify_label_fn A function that takes a character vector of labels and returns a modified character vector.
#' The default function removes '_ott.*' and replaces underscores with spaces.
#'
#' @return A modified phylogenetic tree with updated tip labels.
#' @importFrom tibble as_tibble
#' @importFrom stringr str_replace str_remove str_detect
modify_tips <- function(tree, modify_label_fn = function(x) str_remove(x, '_ott.*')) {
  
  metadata <- as_tibble(tree) %>% mutate(label = modify_label_fn(label))
  tree$tip.label <- metadata[which(str_detect(metadata$label, 'Quercus')),]$label
  
  return(tree)
}

complete_tree <- read.tree(here::here('data/in/filogenia/OpenTreeOfLife_hipp2019.tre'))


complete_tree <- modify_tips(complete_tree, modify_label_fn = function(x) str_replace_all(str_remove_all(str_remove(x, '\\|.*'),"\\'"),' |-','_'))

sub_dist <- list()

for(i in seq_along(files)) {
  
  names(files[[i]]) <- str_replace_all(names(files[[i]]),' |-','_')
  rasters_to_use <- which(names(files[[i]]) %in% str_replace_all(unique(common_geo$correctname),' |-','_'))
  
  sub_dist[[i]] <- subset(files[[i]], rasters_to_use)
  
}

#Toma las puntas del arbol y busca las que no se encuentran en la base de datos, es decir, y con el sub_tree se eliminan
tips_to_drop <-  which(!complete_tree$tip.label %in% str_replace_all(unique(common_geo$correctname),' |-','_'))
sub_tree <- drop.tip(complete_tree,tips_to_drop)


# Con el paquete phyloraster, se reacomodan los datos de los rasters para que esten en el orden de la filogenia. 

#Esto arroja un Warning, el cual indica que las especies Quercus_depressipes, Quercus_gambelii no se encuentran en el arbol. Por lo que solo se estaria calculando la PD de 85 especies.

dataprep <- list()
pdr <- list()

for(i in seq_along(sub_dist)) {
  
  #tabla con especies ordenadas
  dataprep[[i]] <- phylo.pres(x = sub_dist[[i]], tree = sub_tree)
  
  #el raster del calculo
  pdr[[i]] <- rast.pd(x = dataprep[[i]]$x, dataprep[[i]]$tree)
  
}

x <- sub_dist[[3]]
tree <- sub_tree

# Genera la tabla ordenada
data <- phylo.pres(x, tree)

# Calcula SES
t <- rast.pd.ses(data$x, 
                 edge.path = data$edge.path,
                 branch.length = data$branch.length, 
                 aleats = 999)

# GUARDA inmediatamente para no perderlo
writeRaster(t, here::here("data/out/SES/PD_SES.tif"), overwrite = TRUE)

# Guarda también un respaldo seguro
saveRDS(t, here::here("data/out/SES/PD_SES.rds"))

png(here::here("data/out/SES/ses_pd.png"), width = 1200, height = 800)
plot(t)
dev.off()



t <- rast.we.ses(x, aleats = 999)

# GUARDA inmediatamente para no perderlo
writeRaster(t, here::here("data/out/SES/WE_SES.tif"), overwrite = TRUE)


png(here::here("data/out/SES/we_ses.tif"), width = 1200, height = 800)
plot(t)
dev.off()


# ---------------------------------------------------
# SES Calculation Script
# Author: Gabriela Centeno & Sofía Zorrilla
# ---------------------------------------------------

library(phyloraster)
library(terra)
library(ape)
library(phylobase)
library(here)
library(data.table)
library(stringr)
library(ggplot2)
library(tidyverse)
library(SESraster)
library(treeio)
library(rotl)

# ---------------------------------------------------
# Load species raster stack
# ---------------------------------------------------

file_list <- list.files(here::here("data/out/raster_species/"),
                        full.names = TRUE, pattern = ".tif$")

files <- lapply(file_list, terra::rast)

# ---------------------------------------------------
# Load metadata
# ---------------------------------------------------

data_morton <- fread(here::here("data/in/filogenia/Hipp2019_sample_metadata.csv"))
geo_data <- fread(here::here("data/in/hgbif_completo_iucn.csv"))

common_morton <- data_morton[`Cleaned_NAMES-USE-THIS` %in% geo_data$correctname,]
common_geo <- geo_data[correctname %in% common_morton$`Cleaned_NAMES-USE-THIS`]

diff <- setdiff(unique(data_morton$`Cleaned_NAMES-USE-THIS`),
                unique(geo_data$correctname))

common_geo <- merge(common_geo,
                    unique(common_morton[,.(Cleaned_NAMES-USE-THIS,section,clade,subgenus)]),
                    by.x = "correctname", by.y = "Cleaned_NAMES-USE-THIS", all.x = TRUE)

fwrite(common_geo, here::here("data/out/common_geo.csv"), row.names = FALSE)

# ---------------------------------------------------
# Clean tree labels
# ---------------------------------------------------

modify_tips <- function(tree, modify_label_fn = function(x) str_remove(x, '_ott.*')) {
  metadata <- as_tibble(tree) %>% mutate(label = modify_label_fn(label))
  tree$tip.label <- metadata[which(str_detect(metadata$label, "Quercus")),]$label
  return(tree)
}

complete_tree <- read.tree(here::here("data/in/filogenia/OpenTreeOfLife_hipp2019.tre"))

complete_tree <- modify_tips(
  complete_tree,
  modify_label_fn = function(x)
    str_replace_all(str_remove_all(str_remove(x, "\\|.*"), "\\'"), " |-","_")
)

# ---------------------------------------------------
# Match species between rasters and phylogeny
# ---------------------------------------------------

sub_dist <- list()

for(i in seq_along(files)) {
  names(files[[i]]) <- str_replace_all(names(files[[i]]), " |-","_")
  rasters_to_use <- which(names(files[[i]]) %in%
                            str_replace_all(unique(common_geo$correctname), " |-","_"))
  sub_dist[[i]] <- subset(files[[i]], rasters_to_use)
}

tips_to_drop <- which(!complete_tree$tip.label %in%
                        str_replace_all(unique(common_geo$correctname), " |-","_"))

sub_tree <- drop.tip(complete_tree, tips_to_drop)

# ---------------------------------------------------
# Prepare phylo-pres data
# ---------------------------------------------------

dataprep <- list()
pdr <- list()

for(i in seq_along(sub_dist)) {
  dataprep[[i]] <- phylo.pres(x = sub_dist[[i]], tree = sub_tree)
  pdr[[i]] <- rast.pd(x = dataprep[[i]]$x, tree = dataprep[[i]]$tree)
}

# ---------------------------------------------------
# Calculate SES (PD and WE)
# ---------------------------------------------------

x <- sub_dist[[3]]
tree <- sub_tree

data <- phylo.pres(x, tree)

t <- rast.pd.ses(data$x, edge.path = data$edge.path,
                 branch.length = data$branch.length, aleats = 2)

writeRaster(t, "PD_SES.tif", overwrite = TRUE)

png("ses_pd.png", width = 1200, height = 800)
plot(t)
dev.off()

t <- rast.we.ses(x, aleats = 2)

png("ses_we.png", width = 1200, height = 800)
plot(t)
dev.off()

# ---------------------------------------------------
cat("SES calculation completed successfully.\n")
# ---------------------------------------------------

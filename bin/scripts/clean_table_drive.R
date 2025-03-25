library(googlesheets4)
library(data.table)
library(dplyr)

iucn_table <- read_sheet(ss= 'https://docs.google.com/spreadsheets/d/1yDi-Csyd6tJL-v18njcAhH6i0-By2XYA24P5ZTP2NTU/edit?usp=sharing', 
           sheet = 1)

ios_names <- fread("data/in/IOS/quercus_species_distribution.csv")

ios_names_unique <- ios_names %>%
  group_by(`Species Name`) %>%
  filter(n() == 1) %>%
  ungroup()

iucn_table_ios <- iucn_table %>%
  mutate(IOS = correctname %in% ios_names$`Species Name`)

write_sheet(iucn_table,ss = 'https://docs.google.com/spreadsheets/d/1yDi-Csyd6tJL-v18njcAhH6i0-By2XYA24P5ZTP2NTU/edit?usp=sharing', 
            sheet = 2)

common_geo_names <- unique(common_geo$correctname)

# Create a logical column (TRUE/FALSE) showing which names are in common_geo_names
iucn_table$tree_presence <- iucn_table$correctname %in% common_geo_names

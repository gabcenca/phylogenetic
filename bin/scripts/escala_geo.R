library(sf)
library(tidyverse)
library(terra)
library(here)
library(data.table)
# Read the biogeographic provinces of Mexico
provinces <- st_read("data/in/biogeo_provinces/all_bio_regions/pbiogmx17gw.shp")

# Read the raster stack of oak species
oak_rasters <- rast(here::here("data/out/raster_species/ras_species_res_0.5.tif"))

# Grid resolutions to create
grid_resolutions <- c(0.135, 0.225, 0.315, 0.405, 0.450)


#' Create a Spatial Grid Over a Shapefile
#'
#' This function generates a spatial grid of square polygons with a specified resolution and clips it to the provided shapefile.
#'
#' @param resolution A numeric value specifying the grid cell size in the same units as the shapefile's coordinate reference system.
#' @param shapefile An `sf` object representing the geographic region over which the grid will be created.
#'
#' @return An `sf` object containing the grid polygons that intersect with the input shapefile.
#'
#' @examples
#' library(sf)
#' nc <- st_read(system.file("shape/nc.shp", package = "sf"))
#' grid <- create_grid(0.5, nc)
#'
#' @import sf
#' @importFrom dplyr %>%
#' @export
create_grid <- function(resolution, shapefile) {
  grid_province <- shapefile %>% 
    st_make_grid(cellsize = resolution, what = "polygons", square = TRUE) %>%
    st_sf() %>%
    st_intersection(shapefile) 
  
  return(grid_province)
  
  } 
  
#' Count Grids by a Grouping Feature
#'
#' This function counts the number of grid cells in `grid_province` that belong to each unique value of the specified `grouping_feature`.
#'
#' @param grid_province A data frame or tibble containing a column with the grouping feature (e.g., biogeographic regions).
#' @param grouping_feature A string specifying the column name in `grid_province` used for grouping (default: `"Provincias"`).
#'
#' @return A data frame with two columns: 
#'   - The grouping feature column (e.g., `"Provincias"`) with unique values.
#'   - `n`: The count of grid cells for each unique value of the grouping feature.
#'
#' @examples
#' grid_data <- data.frame(Provincias = c("Region1", "Region2", "Region1", NA, "Region2"))
#' count_grids(grid_data, "Provincias")
#'
#' @import dplyr
#' @importFrom tidyr drop_na
#' @export
count_grids <- function(grid_province, grouping_feature = "JJM2017") {
  
  summary <- as.data.frame(grid_province) %>% 
    drop_na(.data[[grouping_feature]]) %>% 
    group_by(.data[[grouping_feature]]) %>% 
    summarise(n = n(), .groups = "drop")
  
  return(summary)
  
  }


# Create grid for each resolution in grid_resolutions
grids <- lapply(grid_resolutions, function(res) {
 
 grid <- create_grid(res, provinces) 
 
 return(grid)
 }
)

#Make the summary of the presence of the provinces in each grid size
summary_grids <- lapply(seq_along(grids), function(i) {
  
  summary <- count_grids(grids[[i]]) 
  
  summary$resolution <- grid_resolutions[i]
  
  return(summary)
}
)


# Rename list elements based on the resolution
names(grids) <- paste0("grid_", grid_resolutions)
names(summary_grids) <- paste0("grid_", grid_resolutions)

summary_resolution <- do.call(rbind,summary_grids)

#Graphic the results
plot_res <- ggplot(summary_resolution, aes(x = JJM2017, y = n)) +
  geom_col() +
  facet_wrap(~resolution) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
#Save it
ggsave("data/out/scale_analysis/scale_analysis.png",plot_res, 
       width = 10, height = 8, dpi = 300)


# Save each grid as a shapefile so we can work it later
for (grid_name in names(grids)) {
  st_write(grids[[grid_name]], paste0("data/out/shapefiles_provinces_grid/", 
                                      grid_name, ".shp"), delete_layer = TRUE)
}


#Intersect the records with the grids

#Add an id for each square of the grid
grid_045 <- st_read("data/out/sf_prov_grid/grid_0.45.shp") %>%
  mutate(grid_id = row_number())

hgbif <- fread(here::here("data/in/hgbif_completo_iucn.csv"))
hgbif_sf <- st_as_sf(hgbif, coords = c("X","Y"), crs = "WGS84")


points_in_grid <- st_join(hgbif_sf, grid_045)

# Calculate species richness per grid cell
richness_counts <- points_in_grid %>%
  st_drop_geometry() %>%
  group_by(grid_id) %>%
  summarize(
    species_richness = n_distinct(correctname),
    province = first(JJM2017)  # Assuming each grid cell has only one province
  )

# Join richness counts back to the grid spatial data to generate a sf with the grid_id, richness and province
grid_with_richness <- grid_045 %>%
  left_join(richness_counts, by = "grid_id") %>%
  # Replace NA values with 0 for grid cells with no species
  mutate(species_richness = ifelse(is.na(species_richness), 0, species_richness))


# Ordenar los datos aleatoriamente
set.seed(13235)

grid_with_richness_df <- grid_with_richness_df %>%
                              as.data.frame() %>%
                              st_drop_geometry()


grid_azar <- grid_with_richness_df[sample(1:nrow(grid_with_richness_df)), ] %>%
  filter(species_richness > 0)

provincia1 <- grid_azar %>%
  filter(province == "Sierra Madre Occidental province")

# Seleccionar progresivamente más registros

countSp_in_N_records <- function(df, n, richness) {
  richness_sum <- sum(df[1:n, get(richness)])
  return(richness_sum)  
}

countSp_in_N_records(provincia1, 4, "species_richness")

count <- sapply(seq(1:nrow(provincia1)),function(i){countSp_in_N_records(df = provincia1, n = i, 
                                                   richness = "species_richness")})

plot(count, type = "l")


library(sf)
library(dplyr)
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
count_grids <- function(grid_province, grouping_feature = "Provincias") {
  
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

plot_res <- ggplot(summary_resolution, aes(x = Provincias, y = n)) +
  geom_col() +
  facet_wrap(~resolution) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
  

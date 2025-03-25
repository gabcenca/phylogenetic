library(sf)
library(dplyr)
library(terra)
library(here)

# Read the biogeographic provinces of Mexico
provinces <- st_read("data/in/mexico_provinces_biogeo/mex_prov_biogeo.shp")

# Read the raster stack of oak species
oak_rasters <- rast(here::here("data/out/raster_species/ras_species_res_0.5.tif"))

# Grid resolutions to create
grid_resolutions <- c(0.135, 0.225, 0.315, 0.405, 0.450)\

grid <- provinces %>% 
  st_make_grid(cellsize = c(0.450,0.450), what = "polygons", square = TRUE) %>% # grid of points
  st_intersection(provinces)

g <- ggplot() + 
geom_sf(data = provinces) + 
geom_sf(data = grid)

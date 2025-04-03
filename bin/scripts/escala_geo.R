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


# Curvas de acumulación de riqueza por provincia --------------------------

#' Calculate richness per province for n number of randomly selected cells
#'
#' This function calculates species richness at the provincial level (JJM2017) by randomly
#' sampling a specified number of grid cells.
#'
#' @param n Integer. Number of grid cells to randomly sample within each province (JJM2017).
#'          Provinces where n exceeds the total number of available grid cells are excluded from the results.
#' @param df data.table. Must contain columns 'JJM2017', 'grid_id', and 'correctname',
#'           where 'correctname' typically represents species names.
#'
#' @return A data.table with three columns:
#'   \item{JJM2017}{Province identifier}
#'   \item{riqueza}{Species richness (count of unique species) in the sampled grid cells}
#'   \item{n}{Number of grid cells requested for sampling}
#'
#' @details The function first randomly selects n grid cells (identified by 'grid_id') within each
#'          province (JJM2017). It then includes all records (rows) belonging to those selected
#'          grid cells. Finally, it calculates species richness as the count of unique species
#'          (correctname) per province. Provinces where n exceeds the total number of available
#'          grid cells are excluded from the final output.
#'
#' @examples
#' \dontrun{
#' # Calculate richness using 5 random grid cells per province
#' richness_df <- calc_richness(5, species_data)
#' 
#' # Compare richness at different sampling intensities
#' richness_comparison <- rbind(
#'   calc_richness(2, species_data),
#'   calc_richness(5, species_data),
#'   calc_richness(10, species_data)
#' )
#' }
#'
#' @importFrom data.table .N .SD :=
#' @export
calc_richness <- function(n, df) {
  print(paste('Calculating richness for',n,'randomly sampled cells per province'))
  # Get max available grid cells per province
  max_cells_per_province <- df[, .(max_cells = uniqueN(grid_id)), by = "JJM2017"]
  
  # Identify provinces where n exceeds the number of available grid cells
  valid_provinces <- max_cells_per_province[max_cells_per_province$max_cells >= n, JJM2017]
  
  # Filter df to only include valid provinces
  df <- df[JJM2017 %in% valid_provinces]
  
  # Sample n grid_id values per province and subset data
  randomRowSample <- df[, {
    sampled_grid_ids <- sample(unique(grid_id), min(n, uniqueN(grid_id)))
    .SD[grid_id %in% sampled_grid_ids]
  }, by = "JJM2017"]
  
  # Compute species richness while ignoring NAs in correctname
  result <- randomRowSample[, .(riqueza = uniqueN(correctname, na.rm = TRUE)), by = "JJM2017"]
  
  # Add n column
  result[, n := n]
  
  return(result)
}




## Calcular riqueza acumulada por provincia ------------------------------

run_richness_reps <- function(n_reps, df, max_n = NULL) {
  # Get the maximum number of grid cells in any province
  max_cells <- df[, .(max = uniqueN(grid_id)), by = "JJM2017"]
  
  if(is.null(max_n)){
  max_n <- max(max_cells$max)
  }
  
  # Run first repetition to define structure
  summary_list <- lapply(seq_len(max_n), FUN = calc_richness, df = df)
  richnessDT <- do.call(rbind, summary_list)[, .(JJM2017, n, riqueza1 = riqueza)]
  
  # Preallocate a list to store results
  all_reps <- vector("list", n_reps)
  all_reps[[1]] <- richnessDT
  
  # Repeat calculation for the remaining repetitions
  for (i in 2:n_reps) {
    print(paste("Repetition", i))
    summary_list <- lapply(seq_len(max_n), FUN = calc_richness, df = df)
    richnessDT <- do.call(rbind, summary_list)[, .(riqueza = riqueza)]
    all_reps[[i]] <- richnessDT
  }
  
  # Combine results into a single data.table
  rarefaction <- do.call(cbind, all_reps)
  
  # Rename columns
  setnames(rarefaction, c("JJM2017", "n", paste0("riqueza", 1:n_reps)))
  
  # Calculate mean richness across repetitions
  rarefaction[, mean := rowMeans(.SD, na.rm = TRUE), .SDcols = paste0("riqueza", 1:n_reps)]
  
  return(rarefaction)
}

# Run function with 50 repetitions
df_rarefaction <- run_richness_reps(10, points_in_grid_dt, 10)


## Tabla de registros por celda --------------------------------------------

### En chatGPT generar una funcion que tenga como input la ruta del shp y los puntos y regrese la cuadricula con la info de los puntos añadida (points_in_grid)

#Intersect the records with the grids

#Add an id for each square of the grid
grid_045 <- st_read("data/out/sf_prov_grid/grid_0.45.shp") %>%
  mutate(grid_id = row_number())

# TODO: Hay que corregir la tabla para limpiar las coordenadas raras

hgbif <- fread(here::here("data/in/hgbif_completo_iucn.csv")) %>% 
  filter(!is.na(Y) & !is.na(X)) %>%
  filter(X >= -118 & X <= -87,  
         Y >= 14 & Y <= 33) 

hgbif_sf <- st_as_sf(hgbif, coords = c("X","Y"), crs = "WGS84")


points_in_grid <- st_join( grid_045, hgbif_sf,join = st_contains)



# Main (coordinadora) -----------------------------------------------------

# Funcion que coordine la creacion del df (points_in_grid) y el calculo de la riqueza muchas veces.
# Puede ser que el resultado sea el plot

# Graficar resultados de rarefacción (qué tanto está representada la riqueza dependiendo del numero de celdas que muestreamos)
ggplot(data = rarefaction[,c("JJM2017","n","mean")], aes(x = n, y = mean, colour = JJM2017, group = JJM2017))+
  geom_line()+
  theme_bw()



#inputs
# La ruta al shape 
# ruta puntos
































# Calculate species richness per grid cell
# richness_counts <- points_in_grid %>%
#   st_drop_geometry() %>%
#   group_by(grid_id) %>%
#   summarize(
#     species_richness = n_distinct(correctname),
#     province = first(JJM2017)  # Assuming each grid cell has only one province
#   )
# 
# # Join richness counts back to the grid spatial data to generate a sf with the grid_id, richness and province
# grid_with_richness <- grid_045 %>%
#   left_join(richness_counts, by = "grid_id") %>%
#   # Replace NA values with 0 for grid cells with no species
#   mutate(species_richness = ifelse(is.na(species_richness), 0, species_richness))
# 
# 
# # Ordenar los datos aleatoriamente
# set.seed(13235)
# 
# grid_with_richness_df <- grid_with_richness_df %>%
#                               as.data.frame() %>%
#                               st_drop_geometry()
# 
# 
# grid_azar <- grid_with_richness_df[sample(1:nrow(grid_with_richness_df)), ] %>%
#   filter(species_richness > 0)
# 
# provincia1 <- grid_azar %>%
#   filter(province == "Sierra Madre Occidental province")
# 
# # Seleccionar progresivamente más registros
# 
# countSp_in_N_records <- function(df, n, richness) {
#   richness_sum <- sum(df[1:n, get(richness)])
#   return(richness_sum)  
# }
# 
# countSp_in_N_records(provincia1, 4, "species_richness")
# 
# count <- sapply(seq(1:nrow(provincia1)),function(i){countSp_in_N_records(df = provincia1, n = i, 
#                                                    richness = "species_richness")})
# 
# plot(count, type = "l")
# 

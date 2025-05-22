library(sf)
library(tidyverse)
library(here)
library(data.table)
library(ggplot2)

# Curvas de acumulación de riqueza por provincia --------------------------

# 1. Cargar funciones auxiliares (intersect_points_with_grid, calc_richness, run_richness_reps y plot_richness_accumulation)


#' Intersects Points with Grid
#'
#' This function reads a grid shapefile and a CSV file containing point records, 
#' and returns a new grid shapefile with the point records that fall inside each grid cell.
#' 
#' @param grid_shp_path Character. Path to the grid shapefile.
#' @param points_csv_path Character. Path to the CSV file containing point records with coordinates X and Y.
#' 
#' @return A spatial data frame (sf) containing the grid with the points added to each grid cell.
#' 
#' @importFrom sf st_read st_as_sf st_join st_make_valid
#' @importFrom data.table fread
#' @importFrom dplyr mutate filter
#' @export
intersect_points_with_grid <- function(grid_shp_path, points_csv_path) {
  library(sf)
  library(data.table)
  library(dplyr)
  
  grid <- st_read(grid_shp_path, quiet = TRUE) %>%
    st_make_valid() %>% 
    mutate(grid_id = row_number())
  
  points <- fread(points_csv_path) %>%
    filter(!is.na(X) & !is.na(Y)) %>%
    filter(X >= -118 & X <= -87, Y >= 14 & Y <= 33)
  
  points_sf <- st_as_sf(points, coords = c("X", "Y"), crs = 4326)
  
  points_in_grid <- st_join(grid, points_sf, join = st_contains)
  
  return(points_in_grid)
}

#' Calculates Species Richness
#'
#' This function calculates the species richness by randomly selecting a specified 
#' number of grid cells within each province and counting the unique species in those cells.
#'
#' @param n Integer. The number of grid cells to randomly sample per province.
#' @param df data.table. A data.table containing the columns 'JJM2017' (province identifier), 
#'            'grid_id', and 'correctname' (species names).
#'
#' @return A data.table with three columns: JJM2017 (province identifier), riqueza (species richness), and n (number of grid cells).
#' 
#' @importFrom data.table .N .SD :=
#' @export
calc_richness <- function(n, df) {
  print(paste('Calculating richness for', n, 'randomly sampled cells per province'))
  
  if(!is.data.table(df)){
    df <- as.data.table(df)
  }
  
  max_cells_per_province <- df[, .(max_cells = uniqueN(grid_id)), by = "JJM2017"]
  valid_provinces <- max_cells_per_province[max_cells_per_province$max_cells >= n, JJM2017]
  df <- df[JJM2017 %in% valid_provinces]
  
  randomRowSample <- df[, {
    sampled_grid_ids <- sample(unique(grid_id), min(n, uniqueN(grid_id)))
    .SD[grid_id %in% sampled_grid_ids]
  }, by = "JJM2017"]
  
  result <- randomRowSample[, .(riqueza = uniqueN(correctname, na.rm = TRUE)), by = "JJM2017"]
  result[, n := n]
  
  return(result)
}

#' Runs Species Richness Repetitions
#'
#' This function calculates species richness multiple times (repetitions) by randomly selecting grid cells.
#' It then calculates the mean richness over the specified number of repetitions.
#'
#' @param n_reps Integer. The number of repetitions to run.
#' @param df data.table. A data.table containing species records in the format of the output from 'intersect_points_with_grid'.
#' @param max_n Integer. The maximum number of grid cells available to sample. If NULL, it will automatically use the maximum number of grid cells per province.
#'
#' @return A data.table with the species richness calculated for each repetition and the mean richness.
#' 
#' @importFrom data.table .SD
#' @export
run_richness_reps <- function(n_reps, df, max_n = NULL, output_file_path = NULL) {
  max_cells <- df[, .(max = uniqueN(grid_id)), by = "JJM2017"]
  
  if (is.null(max_n)) {
    max_n <- max(max_cells$max)
  }
  
  summary_list <- lapply(seq_len(max_n), FUN = calc_richness, df = df)
  richnessDT <- do.call(rbind, summary_list)[, .(JJM2017, n, riqueza1 = riqueza)]
  
  all_reps <- vector("list", n_reps)
  all_reps[[1]] <- richnessDT
  
  if (n_reps < 2) {
    stop("Se necesitan al menos dos repeticiones para calcular el promedio.")
  }
  
  for (i in 2:n_reps) {
    print(paste("Repetition", i))
    summary_list <- lapply(seq_len(max_n), FUN = calc_richness, df = df)
    richnessDT <- do.call(rbind, summary_list)[, .(riqueza = riqueza)]
    all_reps[[i]] <- richnessDT
  }
  
  rarefaction <- do.call(cbind, all_reps)
  setnames(rarefaction, c("JJM2017", "n", paste0("riqueza", 1:n_reps)))
  rarefaction[, mean := rowMeans(.SD, na.rm = TRUE), .SDcols = paste0("riqueza", 1:n_reps)]
  
  
  
  if(!is.null(output_file_path)){
    fwrite(file = output_file_path, rarefaction, row.names = F)
  }

  
  return(rarefaction)
}

#' Plots Species Richness Accumulation
#'
#' This function generates and saves a richness accumulation curve based on the species richness calculated by the 'run_richness_reps' function.
#' The curve shows how the species richness increases as more grid cells are sampled.
#'
#' @param grid_shp_path Character. Path to the grid shapefile.
#' @param points_csv_path Character. Path to the CSV file containing point records.
#' @param n_reps Integer. The number of repetitions for the richness calculation.
#' @param max_n Integer. The maximum number of grid cells available to sample. If NULL, it will automatically use the maximum number of grid cells per province.
#' @param output_dir Character. Path to the directory where the plot will be saved.
#' @param plot_filename Character. The name of the output plot file. If NULL, the function generates a default name based on the grid file.
#'
#' @return A ggplot object representing the richness accumulation curve.
#' 
#' @importFrom ggplot2 ggplot geom_line labs ggsave theme_bw
#' @importFrom tools file_path_sans_ext
#' @export
plot_richness_accumulation <- function(grid_shp_path, 
                                       points_csv_path, 
                                       n_reps = 10, 
                                       max_n = NULL,
                                       output_dir,
                                       plot_filename = NULL) {
  
  if (!dir.exists(output_dir)) {
    stop("El directorio de salida no existe. Por favor créalo antes de correr la función.")
  }
  
  if (is.null(plot_filename)) {
    grid_name <- tools::file_path_sans_ext(basename(grid_shp_path))
    plot_filename <- paste0("richness_accumulation_", grid_name, ".png")
    csv_filename <- paste0("richness_accumulation_", grid_name, ".csv")
  }
  
  plot_path <- file.path(output_dir, plot_filename)
  
  message("Intersectando puntos con el grid...")
  points_in_grid_sf <- intersect_points_with_grid(grid_shp_path, points_csv_path)
  points_in_grid_dt <- as.data.table(points_in_grid_sf)
  
  if (!"JJM2017" %in% names(points_in_grid_dt)) {
    stop("La columna 'JJM2017' no está presente en los datos.")
  }
  
  message("Calculando riqueza acumulada...")
  rarefaction <- run_richness_reps(n_reps = n_reps, df = points_in_grid_dt, max_n = max_n, 
                                   output_file_path = file.path(output_dir, csv_filename))
  
  message("Generando y guardando gráfica...")
  g <- ggplot(data = rarefaction[, .(JJM2017, n, mean)], aes(x = n, y = mean, colour = JJM2017, group = JJM2017)) +
    geom_line() +
    theme_bw() +
    labs(
      x = "Número de celdas muestreadas",
      y = "Riqueza media acumulada",
      colour = "Provincia",
      title = "Curva de rarefacción por provincia"
    )
  
  ggsave(filename = plot_path, plot = g, width = 8, height = 6, dpi = 300)
  
  message("Gráfica guardada en: ", plot_path)
  
}


# 2. Cargar función para correr batch con varios shapefiles

#' Run Batch Richness Plots for Multiple Shapefiles
#'
#' This function processes multiple shapefiles in a specified directory, performing species richness 
#' accumulation analysis and generating plots for each grid. It iterates over all the shapefiles in the 
#' provided directory, calling the `plot_richness_accumulation()` function for each one.
#'
#' @param grid_dir Character. Path to the directory containing grid shapefiles (.shp).
#' @param points_csv_path Character. Path to the CSV file containing point records with coordinates (X, Y).
#' @param n_reps Integer. The number of repetitions for the richness calculation. Default is 10.
#' @param max_n Integer. The maximum number of grid cells available to sample. If NULL, it will automatically use the maximum number of grid cells per province.
#' @param output_dir Character. Path to the directory where the plots will be saved.
#'
#' @return NULL. The function generates and saves a plot for each shapefile in the grid directory. 
#'         The plots are saved in the specified output directory.
#' 
#' @details This function checks whether the input directories exist, then processes each shapefile 
#'          by calling `plot_richness_accumulation()`. The results for each shapefile are saved in 
#'          the output directory specified by the user. If any errors occur during processing, they 
#'          are caught and logged, but the function continues with the next shapefile.
#'
#' @importFrom tools file_path_sans_ext
#' @export
run_batch_richness_plots <- function(grid_dir, 
                                     points_csv_path, 
                                     n_reps = 10, 
                                     max_n = NULL, 
                                     output_dir) {
  
  # Verifica que el directorio existe
  if (!dir.exists(grid_dir)) stop("El directorio de grids no existe.")
  if (!dir.exists(output_dir)) stop("El directorio de salida no existe.")
  
  # Lista todos los archivos .shp en el directorio
  grid_files <- list.files(grid_dir, pattern = "\\.shp$", full.names = TRUE)
  
  if (length(grid_files) == 0) stop("No se encontraron archivos .shp en el directorio.")
  
  message("Ejecutando análisis para ", length(grid_files), " grids...")
  
  # Corre la función para cada grid
  for (grid_shp in grid_files) {
    message("\n➡ Procesando: ", basename(grid_shp))
    
    tryCatch({
      plot_richness_accumulation(
        grid_shp_path = grid_shp,
        points_csv_path = points_csv_path,
        n_reps = n_reps,
        max_n = max_n,
        output_dir = output_dir
      )
    }, error = function(e) {
      message("Error procesando ", basename(grid_shp), ": ", e$message)
    })
  }
  
  message("\n Proceso completado.")
}

# 3. Ejecutar el análisis en batch
run_batch_richness_plots(
  grid_dir = "data/out/sf_prov_grid",                # Carpeta con los .shp
  points_csv_path = "data/in/hgbif_completo_iucn.csv",  # Archivo CSV de puntos
  n_reps = 50,                                        # Número de repeticiones
  output_dir = "data/out/accum_provinces/" ,                               # Carpeta donde guardar .png
  max_n = NULL 
  )
# Hacer las graficas de todas las escalas


grid_045 <- fread(input = "data/out/accum_provinces/richness_accumulation_grid_0.45.csv")
grid_135 <- fread(input = "data/out/accum_provinces/richness_accumulation_grid_0.135.csv")
grid_225 <- fread(input = "data/out/accum_provinces/richness_accumulation_grid_0.225.csv")
grid_405 <- fread(input = "data/out/accum_provinces/richness_accumulation_grid_0.405.csv")
grid_315 <- fread(input = "data/out/accum_provinces/richness_accumulation_grid_0.225.csv")

grid_045$escala <- "0.45"
grid_135$escala  <- "0.135"
grid_225$escala   <- "0.225"
grid_405$escala   <- "0.405"
grid_315$escala   <- "0.315"

# Si ya están en data.table, rbindlist es ideal
todo <- rbindlist(list(grid_045, grid_135, grid_225, grid_405, grid_315))



g <- ggplot(data = todo, aes(x = n, y = mean, colour = JJM2017, group = JJM2017)) +
  geom_line() +
  theme_bw() +
  labs(
    x = "Número de celdas muestreadas",
    y = "Riqueza media acumulada",
    colour = "Provincia",
    title = "Curva de rarefacción por provincia") +
  scale_colour_manual(
    values = scales::hue_pal()(length(unique(grid_045$JJM2017))), # o define tus propios colores
    labels = c(
      "Baja Californian province" = "Baja California",
      "Balsas Basin province" = "Cuenca del Balsas",
      "Californian province" = "Californiana",
      "Chiapas Highlands province" = "Altos de Chiapas",
      "Chihuahuan Desert province" = "Desierto Chihuahuense",
      "Pacific Lowlands province" = "Tierras Bajas del Pacífico",
      "Sierra Madre Occidental province" = "Sierra Madre Occidental",
      "Sierra Madre Oriental province" = "Sierra Madre Oriental",
      "Sierra Madre del Sur province" = "Sierra Madre del Sur",
      "Sonoran province" = "Sonorense",
      "Tamaulipas province" = "Tamaulipas",
      "Transmexican Volcanic Belt province" = "Eje Volcánico Transmexicano",
      "Veracruzan province" = "Veracruzana",
      "Yucatan Peninsula Province" = "Península de Yucatán"
    )
  ) +
  facet_wrap(~ escala, scales = "free_x")

ggsave(filename = "data/out/accum_provinces/all_graphs.png", plot = g, width = 15, height = 8, dpi = 300)


#Tomar menos celdas
g2 <- g + coord_cartesian(xlim = c(0, 30))
g2

ggsave(filename = "data/out/accum_provinces/all_graphs_30celdas.png", plot = g2, width = 15, height = 8, dpi = 300)

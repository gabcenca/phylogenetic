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
#' @param parallel Logical. Whether to process grids in parallel. Default is FALSE.
#'
#' @return NULL. The function generates and saves a plot for each shapefile in the grid directory. 
#'         The plots are saved in the specified output directory.
#'
#' @importFrom tools file_path_sans_ext
#' @export
run_batch_richness_plots <- function(grid_dir, 
                                     points_csv_path, 
                                     n_reps = 10, 
                                     max_n = NULL, 
                                     output_dir,
                                     parallel = FALSE) {
  
  # Check for required packages
  required_packages <- c("future", "future.apply")
  missing_packages <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]
  
  if (length(missing_packages) > 0 && parallel) {
    warning("Parallel processing requested but the following packages are missing: ", 
            paste(missing_packages, collapse = ", "), 
            ". Will fall back to sequential processing.")
    parallel <- FALSE
  }
  
  # Verifica que el directorio existe
  if (!dir.exists(grid_dir)) stop("El directorio de grids no existe.")
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
    message("Created output directory: ", output_dir)
  }
  
  # Lista todos los archivos .shp en el directorio
  grid_files <- list.files(grid_dir, pattern = "\\.shp$", full.names = TRUE)
  
  if (length(grid_files) == 0) stop("No se encontraron archivos .shp en el directorio.")
  
  message("Ejecutando análisis para ", length(grid_files), " grids...")
  
  # Process grids
  if (parallel && length(grid_files) > 1) {
    # Load required packages for parallel processing
    requireNamespace("future", quietly = TRUE)
    requireNamespace("future.apply", quietly = TRUE)
    
    # Set up parallel processing
    future::plan(future::multisession, workers = min(parallel::detectCores() - 1, 4))
    
    # Process grids in parallel
    future.apply::future_lapply(grid_files, function(grid_shp) {
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
    })
    
    # Restore sequential processing
    future::plan(future::sequential)
  } else {
    # Sequential processing
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
  }
  
  message("\n Proceso completado.")
}

run_batch_richness_plots(
  grid_dir = "data/out/sf_prov_grid",               # Grid shapefiles folder
  points_csv_path = "data/in/hgbif_completo_iucn.csv", # Point records CSV
  n_reps = 50,                                      # Number of repetitions
  max_n = 500,                                      # Maximum 500 cells
  output_dir = "data/out/accum_provinces/test",     # Output folder
  parallel = TRUE                                   # Enable parallel processing
)

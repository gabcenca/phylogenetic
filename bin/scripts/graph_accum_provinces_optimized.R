library(sf)
library(tidyverse)
library(here)
library(data.table)
library(ggplot2)
library(tools)

# Install required packages if not available
required_packages <- c("sf", "tidyverse", "data.table", "ggplot2", "future", "future.apply")
new_packages <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]
if (length(new_packages) > 0) {
  message("Installing required packages: ", paste(new_packages, collapse = ", "))
  install.packages(new_packages)
}

# Load packages for parallel processing if available
has_parallel <- all(sapply(c("future", "future.apply"), requireNamespace, quietly = TRUE))
if (has_parallel) {
  library(future)
  library(future.apply)
}

#' Intersects Points with Grid
#'
#' This function reads a grid shapefile and a CSV file containing point records, 
#' and returns a new grid shapefile with the point records that fall inside each grid cell.
#' 
#' @param grid_shp_path Character. Path to the grid shapefile.
#' @param points_csv_path Character. Path to the CSV file containing point records with coordinates X and Y.
#' 
#' @return A data.table containing the grid with the points added to each grid cell.
#' 
#' @export
intersect_points_with_grid <- function(grid_shp_path, points_csv_path) {
  # Cache the results to avoid reprocessing the same grid and points
  cache_file <- paste0(
    file_path_sans_ext(grid_shp_path), 
    "_", 
    file_path_sans_ext(basename(points_csv_path)), 
    "_cache.rds"
  )
  
  if (file.exists(cache_file)) {
    message("Using cached intersection data")
    return(readRDS(cache_file))
  }
  
  message("Reading grid shapefile...")
  grid <- st_read(grid_shp_path, quiet = TRUE) %>%
    st_make_valid() %>% 
    mutate(grid_id = row_number())
  
  message("Reading points data...")
  # Use fread with select to only read necessary columns if they exist
  columns_to_select <- c("X", "Y", "correctname", "JJM2017")
  all_columns <- names(fread(points_csv_path, nrows = 1))
  available_columns <- columns_to_select[columns_to_select %in% all_columns]
  
  points <- fread(
    points_csv_path, 
    select = available_columns,
    na.strings = c("NA", "", "NULL")
  )
  
  # Check if required columns exist
  if (!all(c("X", "Y") %in% names(points))) {
    stop("Required columns 'X' and 'Y' not found in the points data.")
  }
  
  if (!"correctname" %in% names(points)) {
    warning("Column 'correctname' not found in points data. Using a placeholder.")
    points[, correctname := "species"]
  }
  
  if (!"JJM2017" %in% names(points)) {
    # Try to get JJM2017 from grid if it's not in the points
    if ("JJM2017" %in% names(grid)) {
      message("JJM2017 not found in points data. Will use JJM2017 from grid.")
    } else {
      warning("Column 'JJM2017' not found in points or grid data. Using a placeholder.")
      grid$JJM2017 <- "unknown"
    }
  }
  
  # Filter points to valid coordinate range
  points <- points[!is.na(X) & !is.na(Y) & X >= -118 & X <= -87 & Y >= 14 & Y <= 33]
  
  message("Converting points to spatial objects...")
  points_sf <- st_as_sf(points, coords = c("X", "Y"), crs = 4326)
  
  message("Joining points with grid...")
  # Use st_intersects for better performance than st_contains
  intersection <- st_intersects(grid, points_sf)
  
  # Convert to data.table format more efficiently
  result_list <- vector("list", length(intersection))
  
  for (i in seq_along(intersection)) {
    if (length(intersection[[i]]) > 0) {
      # Get the points that fall within this grid cell
      points_in_cell <- points_sf[intersection[[i]], ]
      
      # Get the JJM2017 value from either the grid or points
      if ("JJM2017" %in% names(grid)) {
        jjm_value <- grid$JJM2017[i]
      } else if ("JJM2017" %in% names(points_in_cell)) {
        # If multiple JJM values exist, take the most common one
        jjm_values <- table(points_in_cell$JJM2017)
        jjm_value <- names(jjm_values)[which.max(jjm_values)]
      } else {
        jjm_value <- "unknown"
      }
      
      # Create a data.table for this grid cell
      cell_dt <- data.table(
        grid_id = i,
        JJM2017 = jjm_value,
        correctname = points_in_cell$correctname
      )
      
      result_list[[i]] <- cell_dt
    }
  }
  
  # Combine all data.tables
  if (length(result_list) > 0) {
    result <- rbindlist(result_list, fill = TRUE)
    
    # Save to cache
    saveRDS(result, cache_file)
    return(result)
  } else {
    stop("No points intersect with the grid.")
  }
}

#' Pre-process data for richness calculation
#'
#' @param df data.table. The input data table.
#' @param max_n Integer. Maximum number of cells to sample.
#' @return A list containing pre-processed data.
pre_process_data <- function(df, max_n = NULL) {
  # Get the maximum cells per province
  max_cells_per_province <- df[, .(max_cells = uniqueN(grid_id)), by = "JJM2017"]
  
  # Filter provinces with too few cells
  if (!is.null(max_n)) {
    valid_provinces <- max_cells_per_province[max_cells >= 1, JJM2017]
  } else {
    valid_provinces <- max_cells_per_province$JJM2017
  }
  
  df_filtered <- df[JJM2017 %in% valid_provinces]
  
  # Pre-compute the grid IDs by province to avoid repeating this step
  grid_ids_by_province <- df_filtered[, .(grid_ids = list(unique(grid_id))), by = "JJM2017"]
  
  # Pre-compute the species by grid_id and province
  species_by_grid <- df_filtered[, .(species = list(unique(correctname))), by = .(JJM2017, grid_id)]
  
  # Determine the actual max_n to use
  if (is.null(max_n)) {
    max_n <- max(max_cells_per_province$max_cells)
  } else {
    max_n <- min(max_n, max(max_cells_per_province$max_cells))
  }
  
  return(list(
    grid_ids_by_province = grid_ids_by_province,
    species_by_grid = species_by_grid,
    max_n = max_n,
    valid_provinces = valid_provinces
  ))
}

#' Calculates Species Richness
#'
#' This function calculates the species richness by randomly selecting a specified 
#' number of grid cells within each province and counting the unique species in those cells.
#'
#' @param n Integer. The number of grid cells to randomly sample per province.
#' @param pre_data List. Pre-processed data from pre_process_data.
#'
#' @return A data.table with three columns: JJM2017 (province identifier), riqueza (species richness), and n (number of grid cells).
#' 
#' @export
calc_richness <- function(n, pre_data) {
  grid_ids_by_province <- pre_data$grid_ids_by_province
  species_by_grid <- pre_data$species_by_grid
  valid_provinces <- pre_data$valid_provinces
  
  # Initialize results list
  result_list <- vector("list", length(valid_provinces))
  
  # Process each province
  for (i in seq_along(valid_provinces)) {
    province <- valid_provinces[i]
    
    # Get grid IDs for this province
    province_grid_ids <- grid_ids_by_province[JJM2017 == province, grid_ids][[1]]
    
    # Sample grid IDs if there are enough
    if (length(province_grid_ids) >= n) {
      sampled_grid_ids <- sample(province_grid_ids, n)
      
      # Get all species from the sampled grids
      all_species <- species_by_grid[
        JJM2017 == province & grid_id %in% sampled_grid_ids, 
        unlist(species, use.names = FALSE)
      ]
      
      # Count unique species
      unique_species <- unique(all_species[!is.na(all_species)])
      richness <- length(unique_species)
      
      # Store result
      result_list[[i]] <- data.table(
        JJM2017 = province,
        riqueza = richness,
        n = n
      )
    }
  }
  
  # Combine results
  if (length(result_list) > 0) {
    return(rbindlist(result_list))
  } else {
    return(data.table(JJM2017 = character(0), riqueza = integer(0), n = integer(0)))
  }
}

#' Runs Species Richness Repetitions
#'
#' This function calculates species richness multiple times (repetitions) by randomly selecting grid cells.
#' It then calculates the mean richness over the specified number of repetitions.
#'
#' @param n_reps Integer. The number of repetitions to run.
#' @param df data.table. A data.table containing species records.
#' @param max_n Integer. The maximum number of grid cells available to sample. If NULL, it will automatically use the maximum number of grid cells per province.
#'
#' @return A data.table with the species richness calculated for each repetition and the mean richness.
#' 
#' @export
run_richness_reps <- function(n_reps, df, max_n = NULL) {
  # Pre-process data once
  pre_data <- pre_process_data(df, max_n)
  max_n <- pre_data$max_n
  
  # Set up progress reporting
  message("Starting richness calculation with ", n_reps, " repetitions")
  message("Maximum cells to sample: ", max_n)
  
  # Set up parallel processing if available
  use_parallel <- has_parallel && n_reps > 1
  
  if (use_parallel) {
    message("Using parallel processing")
    future::plan(future::multisession, workers = min(parallel::detectCores() - 1, 4))
  }
  
  # Create sampling plan - all combinations of n_values and repetitions
  n_values <- seq_len(max_n)
  
  # For each n value, run all repetitions
  all_results <- vector("list", max_n)
  
  for (n_idx in seq_along(n_values)) {
    n <- n_values[n_idx]
    message("Processing n = ", n, " (", n_idx, "/", max_n, ")")
    
    # Run all repetitions for this n
    if (use_parallel) {
      rep_results <- future_lapply(seq_len(n_reps), function(rep) {
        if (rep %% 10 == 0) message("  Repetition ", rep, "/", n_reps)
        calc_richness(n, pre_data)
      })
    } else {
      rep_results <- lapply(seq_len(n_reps), function(rep) {
        if (rep %% 10 == 0) message("  Repetition ", rep, "/", n_reps)
        calc_richness(n, pre_data)
      })
    }
    
    # Process the repetition results
    if (n_idx == 1) {
      # For first n, keep all columns
      base_result <- rep_results[[1]]
      richness_cols <- paste0("riqueza", 1)
      setnames(base_result, "riqueza", richness_cols)
      
      # Add other repetitions
      for (i in 2:n_reps) {
        rep_col <- paste0("riqueza", i)
        base_result[, (rep_col) := rep_results[[i]]$riqueza]
      }
      
      all_results[[n_idx]] <- base_result
    } else {
      # For subsequent n values, create result with same structure
      result_n <- data.table(
        JJM2017 = rep_results[[1]]$JJM2017,
        n = n
      )
      
      # Add all repetition columns
      for (i in 1:n_reps) {
        rep_col <- paste0("riqueza", i)
        result_n[, (rep_col) := rep_results[[i]]$riqueza]
      }
      
      all_results[[n_idx]] <- result_n
    }
  }
  
  # Restore sequential processing
  if (use_parallel) {
    future::plan(future::sequential)
  }
  
  # Combine all results
  rarefaction <- rbindlist(all_results)
  
  # Calculate mean richness across repetitions
  richness_cols <- paste0("riqueza", 1:n_reps)
  rarefaction[, mean := rowMeans(.SD, na.rm = TRUE), .SDcols = richness_cols]
  
  return(rarefaction)
}

#' Plots Species Richness Accumulation
#'
#' This function generates and saves a richness accumulation curve based on the species richness calculated.
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
#' @export
plot_richness_accumulation <- function(grid_shp_path, 
                                       points_csv_path, 
                                       n_reps = 10, 
                                       max_n = NULL,
                                       output_dir,
                                       plot_filename = NULL) {
  
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
    message("Created output directory: ", output_dir)
  }
  
  if (is.null(plot_filename)) {
    grid_name <- file_path_sans_ext(basename(grid_shp_path))
    plot_filename <- paste0("richness_accumulation_", grid_name, ".png")
  }
  
  plot_path <- file.path(output_dir, plot_filename)
  
  # Check if results already exist to avoid reprocessing
  results_path <- file.path(
    output_dir, 
    paste0("rarefaction_data_", file_path_sans_ext(basename(grid_shp_path)), ".rds")
  )
  
  if (file.exists(results_path)) {
    message("Loading cached rarefaction results...")
    rarefaction <- readRDS(results_path)
  } else {
    message("Intersecting points with grid...")
    points_in_grid_dt <- intersect_points_with_grid(grid_shp_path, points_csv_path)
    
    if (!"JJM2017" %in% names(points_in_grid_dt)) {
      stop("Column 'JJM2017' not found in the data.")
    }
    
    message("Calculating accumulated richness...")
    rarefaction <- run_richness_reps(n_reps = n_reps, df = points_in_grid_dt, max_n = max_n)
    
    
    # Also save as CSV for easy access
    csv_path <- file.path(output_dir, paste0("rarefaction_data_", file_path_sans_ext(basename(grid_shp_path)), ".csv"))
    fwrite(rarefaction, csv_path)
  }
  
  message("Generating and saving plot...")
  # Use simplified data for plotting
  plot_data <- rarefaction[, .(JJM2017, n, mean)]
  
  # Use a better color palette for distinguishing provinces
  num_provinces <- uniqueN(plot_data$JJM2017)
  
  if (num_provinces <= 8) {
    color_palette <- "Dark2"
  } else {
    color_palette <- "Set1"
  }
  
  g <- ggplot(data = plot_data, aes(x = n, y = mean, colour = JJM2017, group = JJM2017)) +
    geom_line(linewidth = 1) +
    theme_bw() +
    theme(
      legend.position = "right",
      panel.grid.minor = element_blank(),
      text = element_text(size = 12),
      axis.title = element_text(size = 14)
    ) +
    scale_color_brewer(palette = color_palette, guide = guide_legend(ncol = 1)) +
    labs(
      x = "Número de celdas muestreadas",
      y = "Riqueza media acumulada",
      colour = "Provincia",
      title = "Curva de rarefacción por provincia",
      subtitle = paste("Basado en", n_reps, "repeticiones")
    )
  
  # Save plot as PNG
  ggsave(filename = plot_path, plot = g, width = 8, height = 6, dpi = 300)
  
  # Save rarefaction data frame
  data_path <- file.path(output_dir, paste0(file_path_sans_ext(plot_filename), "_data.csv"))
  fwrite(rarefaction, data_path)
  
  message("Plot saved at: ", plot_path)
  message("Rarefaction data saved at: ", data_path)
  
  return(g)
}

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
#' @export
run_batch_richness_plots <- function(grid_dir, 
                                     points_csv_path, 
                                     n_reps = 10, 
                                     max_n = NULL, 
                                     output_dir,
                                     parallel = FALSE) {
  
  # Check for required packages for parallel processing
  if (parallel && !has_parallel) {
    warning("Parallel processing requested but required packages are missing. Will use sequential processing instead.")
    parallel <- FALSE
  }
  
  # Verify directories exist
  if (!dir.exists(grid_dir)) stop("Grid directory does not exist.")
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
    message("Created output directory: ", output_dir)
  }
  
  # List all .shp files in the directory
  grid_files <- list.files(grid_dir, pattern = "\\.shp$", full.names = TRUE)
  
  if (length(grid_files) == 0) stop("No .shp files found in the directory.")
  
  message("Running analysis for ", length(grid_files), " grids...")
  
  # Process grids
  if (parallel && length(grid_files) > 1) {
    # Set up parallel processing
    future::plan(future::multisession, workers = min(parallel::detectCores() - 1, 4))
    
    future.apply::future_lapply(grid_files, function(grid_shp) {
      message("\n➡ Processing: ", basename(grid_shp))
      
      tryCatch({
        plot_richness_accumulation(
          grid_shp_path = grid_shp,
          points_csv_path = points_csv_path,
          n_reps = n_reps,
          max_n = max_n,
          output_dir = output_dir
        )
      }, error = function(e) {
        message("Error processing ", basename(grid_shp), ": ", e$message)
      })
    })
    
    # Restore sequential processing
    future::plan(future::sequential)
  } else {
    # Sequential processing
    for (grid_shp in grid_files) {
      message("\n➡ Processing: ", basename(grid_shp))
      
      tryCatch({
        plot_richness_accumulation(
          grid_shp_path = grid_shp,
          points_csv_path = points_csv_path,
          n_reps = n_reps,
          max_n = max_n,
          output_dir = output_dir
        )
      }, error = function(e) {
        message("Error processing ", basename(grid_shp), ": ", e$message)
      })
    }
  }
  
  message("\nProcess completed successfully.")
}

# Example usage with 500 cells maximum
run_batch_richness_plots(
  grid_dir = "data/out/sf_prov_grid",               # Grid shapefiles folder
  points_csv_path = "data/in/hgbif_completo_iucn.csv", # Point records CSV
  n_reps = 5,                                      # Number of repetitions
  max_n = 20,                                      # Maximum 500 cells
  output_dir = "data/out/accum_provinces/test",         # Output folder
  parallel = FALSE                                   # Enable parallel processing
)
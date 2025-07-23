# SIGABISA Dashboard - app.R (COMPLETE VERSION)

# Load required libraries
library(shiny)
library(shinydashboard)
library(shinyjs)
library(DT)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(leaflet)
library(sf)
library(corrplot)
library(broom)
library(car)
library(nortest)
library(lmtest)
library(RColorBrewer)
library(cluster)
library(factoextra)
library(openxlsx)
library(htmlwidgets)
library(webshot)
library(mapview)
library(gridExtra)
library(forecast)

# ===================================================================
# DATA LOADING & PREPARATION
# ===================================================================

# Load SOVI data
load_sovi_data <- function() {
  if(!file.exists("data/sovi_data.csv")) {
    stop("File data/sovi_data.csv tidak ditemukan. Pastikan file ada di folder data/")
  }
  
  sovi_data <- read.csv("data/sovi_data.csv", stringsAsFactors = FALSE)
  
  # Add region grouping
  create_region_groups <- function(districtcode) {
    first_two <- substr(as.character(districtcode), 1, 2)
    case_when(
      first_two %in% c("11", "12", "13", "14", "15", "16", "17", "18", "19", "21") ~ "Sumatera",
      first_two %in% c("31", "32", "33", "34", "35", "36", "51", "52", "53") ~ "Jawa, Bali, and Nusa Tenggara",  
      first_two %in% c("61", "62", "63", "64", "65") ~ "Kalimantan",
      first_two %in% c("71", "72", "73", "74", "75", "76") ~ "Sulawesi",
      first_two %in% c("81", "82", "91", "92", "93", "94") ~ "Maluku and Papua",
      TRUE ~ "Lainnya"
    )
  }
  
  sovi_data$REGION <- create_region_groups(sovi_data$DISTRICTCODE)
  return(sovi_data)
}

# Load GeoJSON data
load_geojson_data <- function() {
  if(!file.exists("data/PetaKab_Variabel.geojson")) {
    stop("File data/PetaKab_Variabel.geojson tidak ditemukan. Pastikan file ada di folder data/")
  }
  
  tryCatch({
    geojson_data <- sf::st_read("data/PetaKab_Variabel.geojson", quiet = TRUE)
    
    # Fix invalid geometries
    if(any(!sf::st_is_valid(geojson_data))) {
      geojson_data <- sf::st_make_valid(geojson_data)
    }
    
    # Sort GeoJSON berdasarkan DISTRICTCODE ascending
    district_columns <- c("idkab", "DISTRICTCODE", "kode_kab", "kabkot_id")
    district_col <- NULL
    
    for(col in district_columns) {
      if(col %in% names(geojson_data)) {
        district_col <- col
        break
      }
    }
    
    if(!is.null(district_col)) {
      # Convert ke numeric jika perlu
      geojson_data[[district_col]] <- as.numeric(geojson_data[[district_col]])
      
      # Sort berdasarkan district code secara ascending
      geojson_data <- geojson_data[order(geojson_data[[district_col]]), ]
      
      cat("GeoJSON berhasil diurutkan berdasarkan", district_col, "secara ascending\n")
      cat("Range:", min(geojson_data[[district_col]], na.rm = TRUE), 
          "sampai", max(geojson_data[[district_col]], na.rm = TRUE), "\n")
      
    } else {
      cat("Warning: Tidak menemukan kolom district code untuk sorting\n")
      cat("Available columns:", paste(names(geojson_data), collapse = ", "), "\n")
    }
    
    cat("Kolom tersedia di GeoJSON:", names(geojson_data), "\n")
    cat("Jumlah features:", nrow(geojson_data), "\n")
    
    return(geojson_data)
    
  }, error = function(e) {
    warning(paste("Error loading GeoJSON:", e$message))
    return(NULL)
  })
}

# Load distance matrix
load_distance_matrix <- function() {
  if(!file.exists("data/distance.csv")) {
    warning("File data/distance.csv tidak ditemukan. Clustering spasial tidak akan tersedia.")
    return(NULL)
  }
  
  distance_raw <- read.csv("data/distance.csv", row.names = 1)
  distance_matrix <- as.matrix(distance_raw)
  
  # Validasi distance matrix
  if(nrow(distance_matrix) != ncol(distance_matrix)) {
    warning("Distance matrix tidak simetris")
    return(NULL)
  }
  
  # Pastikan diagonal = 0
  if(any(diag(distance_matrix) != 0)) {
    cat("Warning: Diagonal distance matrix bukan 0, akan diperbaiki\n")
    diag(distance_matrix) <- 0
  }
  
  # Pastikan simetris
  if(!isSymmetric(distance_matrix)) {
    cat("Warning: Matrix tidak simetris, akan diperbaiki\n")
    distance_matrix <- (distance_matrix + t(distance_matrix)) / 2
  }
  
  # Set rownames dan colnames berdasarkan GeoJSON yang sudah terurut
  if(exists("geojson_data") && !is.null(geojson_data) && nrow(distance_matrix) == nrow(geojson_data)) {
    
    # Cari kolom district code
    district_columns <- c("idkab", "DISTRICTCODE", "kode_kab", "kabkot_id")
    district_col <- NULL
    
    for(col in district_columns) {
      if(col %in% names(geojson_data)) {
        district_col <- col
        break
      }
    }
    
    if(!is.null(district_col)) {
      # Ambil DISTRICTCODE dari geojson yang sudah terurut
      districtcodes_sorted <- geojson_data[[district_col]]
      
      # Set rownames dan colnames
      rownames(distance_matrix) <- districtcodes_sorted
      colnames(distance_matrix) <- districtcodes_sorted
      
      cat("Distance matrix berhasil di-mapping dengan DISTRICTCODE dari GeoJSON yang sudah terurut\n")
      cat("Range DISTRICTCODE:", min(districtcodes_sorted, na.rm = TRUE), 
          "-", max(districtcodes_sorted, na.rm = TRUE), "\n")
      cat("Mapping: v1 =", districtcodes_sorted[1], ", v2 =", districtcodes_sorted[2], 
          ", v511 =", districtcodes_sorted[length(districtcodes_sorted)], "\n")
      
    } else {
      cat("Tidak menemukan kolom district code di GeoJSON\n")
    }
    
  } else {
    cat("GeoJSON belum ter-load atau ukuran tidak cocok dengan distance matrix\n")
  }
  
  cat("Distance matrix loaded successfully:", nrow(distance_matrix), "x", ncol(distance_matrix), "\n")
  cat("Distance range:", round(min(distance_matrix[distance_matrix > 0]), 2), 
      "to", round(max(distance_matrix), 2), "km\n")
  
  return(distance_matrix)
}

# Function untuk konversi distance ke weight
create_spatial_weights <- function(distance_matrix, method = "gaussian", bandwidth = 100, alpha = 2) {
  if(is.null(distance_matrix)) return(NULL)
  
  n <- nrow(distance_matrix)
  W <- matrix(0, nrow = n, ncol = n)
  
  if(method == "gaussian") {
    # Gaussian kernel: W = exp(-d²/(2*h²))
    W <- exp(-distance_matrix^2 / (2 * bandwidth^2))
    
  } else if(method == "inverse_distance") {
    # Inverse distance: W = 1/d^alpha
    W <- 1 / (distance_matrix^alpha)
    W[!is.finite(W)] <- 0  # Handle division by zero
    
  } else if(method == "exponential") {
    # Exponential decay: W = exp(-d/h)
    W <- exp(-distance_matrix / bandwidth)
  }
  
  # Set diagonal ke 1 (self-similarity)
  diag(W) <- 1
  
  # Normalisasi row-wise (optional, tapi recommended)
  row_sums <- rowSums(W)
  row_sums[row_sums == 0] <- 1  # Avoid division by zero
  W <- W / row_sums
  
  cat("Spatial weights created using", method, "method\n")
  cat("Weight range:", round(min(W[W > 0]), 4), "to", round(max(W), 4), "\n")
  
  return(W)
}

# Load kabupaten data
load_kabupaten_data <- function() {
  if(!file.exists("data/kabupatenkota.csv")) {
    warning("File data/kabupatenkota.csv tidak ditemukan. Fitur provinsi tidak akan tersedia.")
    return(NULL)
  }
  
  kabupaten_data <- read.csv("data/kabupatenkota.csv", stringsAsFactors = FALSE)
  
  # Hanya ambil kolom yang diperlukan untuk provinsi
  required_cols <- c("districtcode", "nmprov")
  if(!all(required_cols %in% tolower(names(kabupaten_data)))) {
    warning("Kolom districtcode dan nmprov tidak ditemukan di kabupatenkota.csv")
    return(NULL)
  }
  
  # Standardize column names dan ambil kolom yang diperlukan
  names(kabupaten_data) <- toupper(names(kabupaten_data))
  kabupaten_data <- kabupaten_data %>%
    select(DISTRICTCODE, NMPROV) %>%
    distinct()
  
  cat("Kabupaten data loaded for provinces:", nrow(kabupaten_data), "records\n")
  return(kabupaten_data)
}

# Implementasi FGWC
fgwc_clustering <- function(data, distance_matrix, k = 3, m = 2, max_iter = 100, tolerance = 1e-4, 
                            spatial_method = "gaussian", bandwidth = 100, lambda = 0.5) {
  n <- nrow(data)
  p <- ncol(data)
  
  # Initialize membership matrix U
  set.seed(123)
  U <- matrix(runif(n * k), nrow = n, ncol = k)
  U <- U / rowSums(U)  # Normalize
  
  # Initialize cluster centers V
  V <- matrix(0, nrow = k, ncol = p)
  
  # Create spatial weights
  W <- create_spatial_weights(distance_matrix, method = spatial_method, bandwidth = bandwidth)
  
  # If no spatial weights, use identity
  if(is.null(W)) {
    W <- diag(n)
    lambda <- 0  # No spatial penalty
    cat("No spatial weights available, using standard fuzzy c-means\n")
  }
  
  # FGWC iterations
  for(iter in 1:max_iter) {
    U_old <- U
    
    # Update cluster centers V
    for(i in 1:k) {
      numerator <- colSums((U[, i]^m) * data)
      denominator <- sum(U[, i]^m)
      if(denominator > 0) {
        V[i, ] <- numerator / denominator
      } else {
        V[i, ] <- colMeans(data)  # Fallback
      }
    }
    
    # Update membership matrix U with spatial constraint
    for(i in 1:n) {
      distances <- numeric(k)
      
      for(j in 1:k) {
        # Feature space distance
        feature_dist <- sqrt(sum((data[i, ] - V[j, ])^2))
        
        # Spatial constraint: weighted average of neighbors' membership to cluster j
        if(lambda > 0 && !is.null(W)) {
          spatial_constraint <- sum(W[i, ] * U[, j])
          # Combine feature distance with spatial constraint
          distances[j] <- feature_dist * (1 + lambda * (1 - spatial_constraint))
        } else {
          distances[j] <- feature_dist
        }
      }
      
      # Update membership values
      for(j in 1:k) {
        if(distances[j] == 0) {
          U[i, ] <- 0
          U[i, j] <- 1
          break
        } else {
          sum_term <- sum((distances[j] / distances)^(2/(m-1)))
          if(is.finite(sum_term) && sum_term > 0) {
            U[i, j] <- 1 / sum_term
          } else {
            U[i, j] <- 1 / k  # Fallback
          }
        }
      }
      
      # Normalize membership (ensure sum = 1)
      row_sum <- sum(U[i, ])
      if(row_sum > 0) {
        U[i, ] <- U[i, ] / row_sum
      } else {
        U[i, ] <- rep(1/k, k)  # Equal membership if all zero
      }
    }
    
    # Check convergence
    if(sum((U - U_old)^2) < tolerance) {
      cat("FGWC converged after", iter, "iterations\n")
      break
    }
  }
  
  # Get hard cluster assignments
  clusters <- apply(U, 1, which.max)
  
  # Calculate objective function value (optional)
  obj_value <- 0
  for(i in 1:n) {
    for(j in 1:k) {
      feature_dist <- sum((data[i, ] - V[j, ])^2)
      if(lambda > 0 && !is.null(W)) {
        spatial_penalty <- lambda * sum(W[i, ] * (U[, j] - U[i, j])^2)
        obj_value <- obj_value + (U[i, j]^m) * (feature_dist + spatial_penalty)
      } else {
        obj_value <- obj_value + (U[i, j]^m) * feature_dist
      }
    }
  }
  
  return(list(
    clusters = clusters,
    centers = V,
    membership = U,
    iterations = iter,
    objective = obj_value,
    spatial_lambda = lambda,
    spatial_method = spatial_method,
    bandwidth = bandwidth
  ))
}

# Load global data
sovi_data <- load_sovi_data()
geojson_data <- load_geojson_data()
distance_matrix <- load_distance_matrix()
kabupaten_data <- load_kabupaten_data()

# Variable definitions
numeric_variables <- c("CHILDREN", "FEMALE", "ELDERLY", "FHEAD", "FAMILYSIZE", 
                       "NOELECTRIC", "LOWEDU", "GROWTH", "POVERTY", "ILLITERATE", 
                       "NOTRAINING", "DPRONE", "RENTED", "NOSEWER", "TAPWATER", "POPULATION")

# Get actual provinces from GeoJSON
if(!is.null(geojson_data) && "nmprov" %in% names(geojson_data)) {
  provinces_list <- unique(geojson_data$nmprov)
  provinces_list <- provinces_list[!is.na(provinces_list)]
} else {
  provinces_list <- c("SUMATERA UTARA", "JAWA BARAT", "JAWA TENGAH")
}

regions_list <- c("Sumatera", "Jawa, Bali, and Nusa Tenggara", "Kalimantan", "Sulawesi", "Maluku and Papua")

# ===================================================================
# CATEGORIZATION HELPER FUNCTIONS
# ===================================================================

# Natural Breaks (Jenks) implementation
jenks_breaks <- function(data, n_classes) {
  if(length(data) < n_classes) return(quantile(data, probs = seq(0, 1, length.out = n_classes + 1)))
  
  data <- sort(data)
  n <- length(data)
  
  # Initialize matrices
  lower_class_limits <- matrix(0, nrow = n + 1, ncol = n_classes + 1)
  variance_combinations <- matrix(Inf, nrow = n + 1, ncol = n_classes + 1)
  
  # Base case
  variance_combinations[1:n_classes, 1] <- 0
  for(i in 2:(n + 1)) {
    variance_combinations[i, 1] <- variance_within_class(data, 1, i - 1)
  }
  
  # Fill the matrices
  for(l in 2:n_classes) {
    for(i in (l + 1):(n + 1)) {
      for(j in l:i) {
        val <- variance_combinations[j, l - 1] + variance_within_class(data, j, i - 1)
        if(val < variance_combinations[i, l]) {
          variance_combinations[i, l] <- val
          lower_class_limits[i, l] <- j
        }
      }
    }
  }
  
  # Extract breaks
  breaks <- numeric(n_classes + 1)
  breaks[n_classes + 1] <- max(data)
  breaks[1] <- min(data)
  
  k <- n + 1
  for(j in n_classes:2) {
    breaks[j] <- data[lower_class_limits[k, j] - 1]
    k <- lower_class_limits[k, j]
  }
  
  return(breaks)
}

# Variance within class helper
variance_within_class <- function(data, start, end) {
  if(start > end) return(0)
  subset_data <- data[start:end]
  if(length(subset_data) <= 1) return(0)
  return(var(subset_data) * (length(subset_data) - 1))
}

# Optimize number of categories using variance ratio
optimize_categories <- function(data, max_categories = 7) {
  data_clean <- data[!is.na(data) & is.finite(data)]
  if(length(data_clean) < 4) return(3)
  
  variance_ratios <- numeric(max_categories - 1)
  
  for(k in 2:max_categories) {
    # Try different methods and use the best variance ratio
    methods_variance <- c()
    
    # Quantiles method
    breaks_q <- quantile(data_clean, probs = seq(0, 1, length.out = k + 1))
    categories_q <- cut(data_clean, breaks = breaks_q, include.lowest = TRUE, labels = FALSE)
    between_var_q <- var(aggregate(data_clean, by = list(categories_q), FUN = mean, na.rm = TRUE)$x)
    within_var_q <- mean(aggregate(data_clean, by = list(categories_q), FUN = var, na.rm = TRUE)$x, na.rm = TRUE)
    methods_variance <- c(methods_variance, between_var_q / (within_var_q + 1e-10))
    
    # K-means method
    tryCatch({
      kmeans_result <- kmeans(data_clean, centers = k, nstart = 10)
      methods_variance <- c(methods_variance, kmeans_result$betweenss / kmeans_result$tot.withinss)
    }, error = function(e) {})
    
    variance_ratios[k - 1] <- max(methods_variance, na.rm = TRUE)
  }
  
  # Find elbow point or maximum variance ratio
  if(all(is.finite(variance_ratios))) {
    # Simple elbow detection
    diffs <- diff(variance_ratios)
    second_diffs <- diff(diffs)
    optimal_k <- which.max(second_diffs) + 2
    optimal_k <- min(max(optimal_k, 2), max_categories)
  } else {
    optimal_k <- 3
  }
  
  return(optimal_k)
}

# Main categorization function
categorize_variable_auto <- function(data, method = "quantiles", n_categories = 3, auto_optimize = FALSE, binary_threshold = NULL) {
  data_clean <- data[!is.na(data) & is.finite(data)]
  
  if(length(data_clean) < 3) {
    return(list(breaks = range(data, na.rm = TRUE), method = "insufficient_data", categories = rep(NA, length(data))))
  }
  
  # Auto optimize number of categories
  if(auto_optimize) {
    n_categories <- optimize_categories(data_clean)
  }
  
  # Handle binary categorization separately
  if(!is.null(binary_threshold)) {
    breaks <- c(min(data_clean), binary_threshold, max(data_clean))
    categories <- cut(data, breaks = breaks, include.lowest = TRUE, 
                      labels = c("0", "1"))  # Keep as character labels
    
    return(list(
      breaks = breaks,
      method = "manual_binary",
      categories = categories,
      n_categories = 2,
      auto_optimized = FALSE
    ))
  }
  
  # Generate breaks based on method (existing code for other methods)
  breaks <- switch(method,
                   "quantiles" = quantile(data_clean, probs = seq(0, 1, length.out = n_categories + 1)),
                   "equal" = seq(min(data_clean), max(data_clean), length.out = n_categories + 1),
                   "kmeans" = {
                     tryCatch({
                       km_result <- kmeans(data_clean, centers = n_categories, nstart = 25)
                       centers <- sort(km_result$centers[,1])
                       c(min(data_clean), 
                         (centers[-length(centers)] + centers[-1]) / 2, 
                         max(data_clean))
                     }, error = function(e) {
                       quantile(data_clean, probs = seq(0, 1, length.out = n_categories + 1))
                     })
                   },
                   "jenks" = {
                     tryCatch({
                       jenks_breaks(data_clean, n_categories)
                     }, error = function(e) {
                       quantile(data_clean, probs = seq(0, 1, length.out = n_categories + 1))
                     })
                   }
  )
  
  # Ensure breaks are unique and properly ordered
  breaks <- sort(unique(breaks))
  if(length(breaks) < 2) {
    breaks <- c(min(data_clean), max(data_clean))
  }
  
  # Create categories
  categories <- cut(data, breaks = breaks, include.lowest = TRUE, 
                    labels = paste0("Cat_", 1:(length(breaks)-1)))
  
  return(list(
    breaks = breaks,
    method = method,
    categories = categories,
    n_categories = length(breaks) - 1,
    auto_optimized = auto_optimize
  ))
}

# ===================================================================
# INTERPRETATION FUNCTIONS
# ===================================================================

# Descriptive interpretation
interpret_descriptive_real <- function(desc_stats, data_subset, variables, region = "all") {
  if(nrow(desc_stats) == 0) return("Tidak ada data untuk diinterpretasi.")
  
  interpretations <- paste0("INTERPRETASI STATISTIK DESKRIPTIF\n",
                            "Wilayah: ", region, "\n",
                            "Jumlah Observasi: ", nrow(data_subset), "\n",
                            "Variabel Dianalisis: ", length(variables), "\n\n")
  
  for(i in 1:nrow(desc_stats)) {
    var <- desc_stats$Variable[i]
    mean_val <- desc_stats$Mean[i]
    median_val <- desc_stats$Median[i]
    sd_val <- desc_stats$SD[i]
    min_val <- desc_stats$Min[i]
    max_val <- desc_stats$Max[i]
    n_val <- if("N" %in% names(desc_stats)) desc_stats$N[i] else nrow(data_subset)
    
    if(is.na(mean_val) || is.na(sd_val)) {
      interpretations <- paste0(interpretations, "- ", var, ": Data tidak tersedia atau tidak valid\n")
      next
    }
    
    cv <- (sd_val / mean_val) * 100
    variability <- ifelse(cv > 30, "sangat bervariasi", ifelse(cv > 15, "bervariasi", "relatif homogen"))
    range_val <- max_val - min_val
    
    if(var %in% c("POVERTY", "ILLITERATE", "NOELECTRIC")) {
      level <- ifelse(mean_val > 20, "tinggi", ifelse(mean_val > 10, "sedang", "rendah"))
      interpretations <- paste0(interpretations, 
                                "- ", var, " (n=", n_val, "): Rata-rata ", round(mean_val, 2), "% (tingkat ", level, "). ",
                                "Rentang ", round(min_val, 2), "% - ", round(max_val, 2), "%. ",
                                "Variabilitas ", variability, " (CV=", round(cv, 1), "%).\n")
    } else if(var == "POPULATION") {
      interpretations <- paste0(interpretations,
                                "- ", var, " (n=", n_val, "): Rata-rata ", format(round(mean_val), big.mark = ","), " jiwa. ",
                                "Rentang ", format(round(min_val), big.mark = ","), " - ", 
                                format(round(max_val), big.mark = ","), " jiwa. ",
                                "Variabilitas ", variability, ".\n")
    } else {
      interpretations <- paste0(interpretations,
                                "- ", var, " (n=", n_val, "): Rata-rata ", round(mean_val, 2), "%. ",
                                "Variabilitas ", variability, " dengan rentang ", round(range_val, 2), "%.\n")
    }
  }
  
  return(interpretations)
}

# Visualization interpretation
interpret_visualization_real <- function(viz_type, variables, plot_data = NULL, cor_data = NULL) {
  base_interpretation <- switch(viz_type,
                                "Histogram" = "Distribusi frekuensi menunjukkan pola sebaran data untuk setiap variabel.",
                                "Boxplot" = "Perbandingan antar provinsi menunjukkan variasi dan outlier dalam data.",
                                "Scatter Plot" = "Hubungan antara dua variabel dan pola korelasinya.",
                                "Correlation Heatmap" = "Matriks korelasi menunjukkan kekuatan hubungan antar variabel.",
                                "Density Plot" = "Distribusi kepadatan data menunjukkan konsentrasi nilai.",
                                "Violin Plot" = "Distribusi lengkap data dengan informasi kepadatan.",
                                "Bar Chart" = "Perbandingan nilai rata-rata antar kategori."
  )
  
  real_interpretation <- ""
  if(viz_type == "Correlation Heatmap" && !is.null(cor_data)) {
    cor_matrix <- as.matrix(cor_data)
    cor_matrix[upper.tri(cor_matrix, diag = TRUE)] <- NA
    cor_long <- reshape2::melt(cor_matrix, na.rm = TRUE)
    cor_long <- cor_long[order(abs(cor_long$value), decreasing = TRUE), ]
    
    if(nrow(cor_long) > 0) {
      strongest_cor <- cor_long[1, ]
      real_interpretation <- paste0("Korelasi terkuat ditemukan antara ", strongest_cor$Var1, 
                                    " dan ", strongest_cor$Var2, " (r = ", round(strongest_cor$value, 3), "). ")
    }
  }
  
  if(!is.null(plot_data) && nrow(plot_data) > 0 && "Value" %in% names(plot_data)) {
    mean_val <- mean(plot_data$Value, na.rm = TRUE)
    sd_val <- sd(plot_data$Value, na.rm = TRUE)
    real_interpretation <- paste0(real_interpretation, " Data menunjukkan rata-rata ", 
                                  round(mean_val, 2), " dengan standar deviasi ", round(sd_val, 2), ".")
  }
  
  return(paste0("INTERPRETASI VISUALISASI\n\n",
                "Jenis: ", viz_type, "\n",
                "Variabel: ", paste(variables, collapse = ", "), "\n\n",
                "Interpretasi Umum:\n", base_interpretation, "\n\n",
                "Temuan dari Data:\n", real_interpretation))
}

# Map interpretation
interpret_map_real <- function(map_variable, map_data, level = "kabupaten") {
  if(is.null(map_data)) return("Data tidak tersedia untuk interpretasi.")
  
  # Extract values
  if(level == "provinsi") {
    if(!"Value" %in% names(map_data)) return("Data provinsi tidak tersedia untuk interpretasi.")
    values <- map_data$Value
    area_names <- map_data$nmprov
  } else {
    if(!map_variable %in% names(map_data)) return("Variabel tidak ditemukan dalam data.")
    values <- as.numeric(map_data[[map_variable]])
    area_names <- if("nmkab" %in% names(map_data)) map_data$nmkab else map_data$DISTRICTCODE
  }
  
  valid_indices <- !is.na(values) & is.finite(values)
  values <- values[valid_indices]
  area_names <- area_names[valid_indices]
  
  if(length(values) == 0) return("Tidak ada data valid untuk interpretasi.")
  
  # Calculate statistics
  mean_val <- mean(values)
  median_val <- median(values)
  sd_val <- sd(values)
  min_val <- min(values)
  max_val <- max(values)
  q25 <- quantile(values, 0.25)
  q75 <- quantile(values, 0.75)
  
  # Identify extremes
  worst_indices <- which(values == max_val)
  best_indices <- which(values == min_val)
  
  if(map_variable %in% c("POVERTY", "ILLITERATE", "NOELECTRIC", "NOTRAINING", "DPRONE", "RENTED", "NOSEWER")) {
    worst_areas <- area_names[worst_indices]
    best_areas <- area_names[best_indices]
    urgency_direction <- "menurunkan"
  } else {
    worst_areas <- area_names[best_indices]
    best_areas <- area_names[worst_indices]
    urgency_direction <- "meningkatkan"
  }
  
  cv <- (sd_val / mean_val) * 100
  spatial_pattern <- ifelse(cv > 50, "sangat heterogen", 
                            ifelse(cv > 30, "heterogen", 
                                   ifelse(cv > 15, "cukup bervariasi", "homogen")))
  
  level_text <- ifelse(level == "provinsi", "tingkat provinsi", "tingkat kabupaten/kota")
  
  interpretation <- paste0(
    "INTERPRETASI PETA SPASIAL\n\n",
    "Variabel: ", map_variable, "\n",
    "Level Agregasi: ", level_text, "\n",
    "Jumlah wilayah: ", length(values), "\n\n",
    "STATISTIK DESKRIPTIF:\n",
    "- Rata-rata: ", round(mean_val, 2), "\n",
    "- Median: ", round(median_val, 2), "\n",
    "- Rentang: ", round(min_val, 2), " - ", round(max_val, 2), "\n",
    "- Pola distribusi: ", spatial_pattern, "\n\n"
  )
  
  # Performance analysis
  performance_analysis <- paste0(
    "ANALISIS PERFORMA WILAYAH:\n",
    "🔴 TERBESAR: ", paste(head(worst_areas, 2), collapse = ", "), " (", round(max_val, 2), ")\n",
    "🟢 TERKECIL: ", paste(head(best_areas, 2), collapse = ", "), " (", round(min_val, 2), ")\n\n"
  )
  
  # Recommendations
  if(level == "provinsi") {
    recommendations <- paste0(
      "🚨 ALOKASI DANA PRIORITAS NASIONAL:\n",
      "1. URGENT - ", paste(head(worst_areas, 2), collapse = " dan "), " memerlukan intervensi segera\n",
      "2. BENCHMARK - Pelajari strategi sukses dari ", paste(head(best_areas, 2), collapse = " dan "), "\n",
      "3. TARGET: ", urgency_direction, " ", map_variable, " melalui program nasional terintegrasi\n"
    )
  } else {
    recommendations <- paste0(
      "🚨 ALOKASI DANA PRIORITAS DAERAH:\n",
      "1. URGENT - ", paste(head(worst_areas, 3), collapse = ", "), " butuh intervensi segera\n",
      "2. BENCHMARK - Contoh sukses: ", paste(head(best_areas, 2), collapse = " dan "), "\n",
      "3. PROGRAM: ", urgency_direction, " ", map_variable, " berbasis komunitas lokal\n"
    )
  }
  
  return(paste0(interpretation, performance_analysis, recommendations))
}

# Normality interpretation
interpret_normality <- function(test_results, variables) {
  interpretation <- "INTERPRETASI UJI NORMALITAS\n\n"
  
  for(var in variables) {
    var_results <- test_results[test_results$Variable == var, ]
    
    if(nrow(var_results) == 0) next
    
    interpretation <- paste0(interpretation, "Variabel: ", var, "\n")
    
    for(i in 1:nrow(var_results)) {
      test_name <- var_results$Test[i]
      p_value <- var_results$P_Value[i]
      is_normal <- var_results$Normal[i]
      
      interpretation <- paste0(interpretation, 
                               "- ", test_name, ": p-value = ", round(p_value, 4), 
                               " (", ifelse(is_normal, "Normal", "Tidak Normal"), ")\n")
    }
    
    # Overall conclusion
    normal_count <- sum(var_results$Normal)
    total_tests <- nrow(var_results)
    
    if(normal_count == total_tests) {
      conclusion <- "Data berdistribusi normal menurut semua uji."
    } else if(normal_count > total_tests/2) {
      conclusion <- "Data cenderung berdistribusi normal."
    } else {
      conclusion <- "Data tidak berdistribusi normal."
    }
    
    interpretation <- paste0(interpretation, "Kesimpulan: ", conclusion, "\n\n")
  }
  
  interpretation <- paste0(interpretation, 
                           "REKOMENDASI ANALISIS:\n",
                           "- Jika data normal: gunakan uji parametrik (t-test, ANOVA)\n",
                           "- Jika data tidak normal: gunakan uji non-parametrik atau transformasi data\n",
                           "- Untuk regresi: periksa normalitas residual, bukan variabel asli")
  
  return(interpretation)
}

# Homogeneity interpretation
interpret_homogeneity <- function(levene_result, bartlett_result, variable, group_var) {
  interpretation <- paste0("INTERPRETASI UJI HOMOGENITAS\n\n",
                           "Variabel: ", variable, "\n",
                           "Pengelompokan: ", group_var, "\n\n")
  
  # Levene's test
  interpretation <- paste0(interpretation, 
                           "Levene's Test:\n",
                           "- F-statistic: ", round(levene_result$`F value`[1], 4), "\n",
                           "- p-value: ", round(levene_result$`Pr(>F)`[1], 4), "\n",
                           "- Kesimpulan: ", ifelse(levene_result$`Pr(>F)`[1] > 0.05, 
                                                    "Varians homogen", "Varians tidak homogen"), "\n\n")
  
  # Bartlett's test
  interpretation <- paste0(interpretation,
                           "Bartlett's Test:\n",
                           "- Statistic: ", round(bartlett_result$statistic, 4), "\n",
                           "- p-value: ", round(bartlett_result$p.value, 4), "\n",
                           "- Kesimpulan: ", ifelse(bartlett_result$p.value > 0.05, 
                                                    "Varians homogen", "Varians tidak homogen"), "\n\n")
  
  # Overall conclusion
  levene_homogen <- levene_result$`Pr(>F)`[1] > 0.05
  bartlett_homogen <- bartlett_result$p.value > 0.05
  
  if(levene_homogen && bartlett_homogen) {
    overall_conclusion <- "Asumsi homogenitas varians TERPENUHI."
    recommendation <- "Gunakan ANOVA klasik atau uji t dengan equal variances."
  } else if(levene_homogen || bartlett_homogen) {
    overall_conclusion <- "Asumsi homogenitas varians tidak terpenuhi."
    recommendation <- "Gunakan Welch's ANOVA atau uji t dengan unequal variances."
  } else {
    overall_conclusion <- "Asumsi homogenitas varians TIDAK TERPENUHI."
    recommendation <- "Gunakan uji non-parametrik (Kruskal-Wallis) atau transformasi data."
  }
  
  interpretation <- paste0(interpretation,
                           "KESIMPULAN KESELURUHAN:\n",
                           overall_conclusion, "\n\n",
                           "REKOMENDASI:\n",
                           recommendation)
  
  return(interpretation)
}

# T-test interpretation
interpret_ttest <- function(ttest_result, variable, mu_value) {
  p_value <- ttest_result$p.value
  t_stat <- ttest_result$statistic
  df <- ttest_result$parameter
  conf_int <- ttest_result$conf.int
  estimate <- ttest_result$estimate
  
  interpretation <- paste0("INTERPRETASI UJI T (ONE SAMPLE)\n\n",
                           "Variabel: ", variable, "\n",
                           "Nilai hipotesis (μ₀): ", mu_value, "\n",
                           "Sampel mean: ", round(estimate, 3), "\n",
                           "t-statistic: ", round(t_stat, 3), "\n",
                           "df: ", df, "\n",
                           "p-value: ", round(p_value, 4), "\n",
                           "95% CI: [", round(conf_int[1], 3), ", ", round(conf_int[2], 3), "]\n\n")
  
  # Hypothesis test conclusion
  if(p_value < 0.05) {
    conclusion <- paste0("TOLAK H₀: Rata-rata populasi ", variable, " BERBEDA SIGNIFIKAN dari ", mu_value)
    if(estimate > mu_value) {
      direction <- paste0("Rata-rata populasi LEBIH TINGGI dari ", mu_value)
    } else {
      direction <- paste0("Rata-rata populasi LEBIH RENDAH dari ", mu_value)
    }
  } else {
    conclusion <- paste0("TERIMA H₀: Rata-rata populasi ", variable, " TIDAK BERBEDA SIGNIFIKAN dari ", mu_value)
    direction <- "Tidak ada perbedaan yang bermakna secara statistik"
  }
  
  interpretation <- paste0(interpretation,
                           "KESIMPULAN STATISTIK:\n",
                           conclusion, "\n",
                           direction, "\n\n")
  
  # Practical interpretation
  effect_size <- abs(estimate - mu_value) / sqrt(ttest_result$estimate^2 / df)
  
  if(p_value < 0.05) {
    if(abs(estimate - mu_value) < 1) {
      practical <- "Perbedaan secara statistik signifikan namun kecil secara praktis."
    } else if(abs(estimate - mu_value) < 5) {
      practical <- "Perbedaan signifikan dan bermakna secara praktis."
    } else {
      practical <- "Perbedaan sangat signifikan dan sangat bermakna secara praktis."
    }
  } else {
    practical <- "Tidak ada bukti perbedaan yang bermakna."
  }
  
  interpretation <- paste0(interpretation,
                           "INTERPRETASI PRAKTIS:\n",
                           practical, "\n",
                           "Selisih rata-rata: ", round(estimate - mu_value, 3))
  
  return(interpretation)
}

# ANOVA interpretation
interpret_anova <- function(anova_result, posthoc_result, dependent_var, factor_var) {
  f_stat <- anova_result$`F value`[1]
  p_value <- anova_result$`Pr(>F)`[1]
  df1 <- anova_result$Df[1]
  df2 <- anova_result$Df[2]
  
  interpretation <- paste0("INTERPRETASI ANOVA SATU ARAH\n\n",
                           "Variabel Terikat: ", dependent_var, "\n",
                           "Faktor: ", factor_var, "\n",
                           "F-statistic: ", round(f_stat, 3), "\n",
                           "df: ", df1, ", ", df2, "\n",
                           "p-value: ", round(p_value, 4), "\n\n")
  
  # Main ANOVA conclusion
  if(p_value < 0.05) {
    main_conclusion <- paste0("TOLAK H₀: Terdapat perbedaan rata-rata ", dependent_var, 
                              " yang SIGNIFIKAN antar kelompok ", factor_var)
    follow_up <- "Perlu dilakukan uji post-hoc untuk menentukan kelompok mana yang berbeda."
  } else {
    main_conclusion <- paste0("TERIMA H₀: Tidak ada perbedaan rata-rata ", dependent_var, 
                              " yang signifikan antar kelompok ", factor_var)
    follow_up <- "Uji post-hoc tidak diperlukan karena tidak ada perbedaan signifikan."
  }
  
  interpretation <- paste0(interpretation,
                           "KESIMPULAN ANOVA:\n",
                           main_conclusion, "\n",
                           follow_up, "\n\n")
  
  # Post-hoc interpretation
  if(p_value < 0.05 && !is.null(posthoc_result)) {
    interpretation <- paste0(interpretation, "HASIL UJI POST-HOC (Tukey HSD):\n")
    
    significant_pairs <- posthoc_result[posthoc_result$`p adj` < 0.05, ]
    
    if(nrow(significant_pairs) > 0) {
      interpretation <- paste0(interpretation, "Pasangan yang berbeda signifikan:\n")
      for(i in 1:min(5, nrow(significant_pairs))) {
        pair <- significant_pairs[i, ]
        interpretation <- paste0(interpretation, 
                                 "- ", pair$comparison, ": perbedaan = ", round(pair$diff, 3), 
                                 " (p = ", round(pair$`p adj`, 4), ")\n")
      }
    } else {
      interpretation <- paste0(interpretation, "Tidak ada pasangan yang berbeda signifikan dalam uji post-hoc.\n")
    }
  }
  
  return(interpretation)
}

# Regression interpretation
interpret_regression <- function(model, diagnostic_tests) {
  model_summary <- summary(model)
  
  interpretation <- paste0("INTERPRETASI REGRESI LINEAR BERGANDA\n\n",
                           "Model: ", deparse(formula(model)), "\n",
                           "N observasi: ", nobs(model), "\n",
                           "R-squared: ", round(model_summary$r.squared, 4), 
                           " (", round(model_summary$r.squared * 100, 1), "%)\n",
                           "Adjusted R-squared: ", round(model_summary$adj.r.squared, 4), "\n",
                           "F-statistic: ", round(model_summary$fstatistic[1], 3), 
                           " (p < 0.001)\n\n")
  
  # Coefficients interpretation
  interpretation <- paste0(interpretation, "INTERPRETASI KOEFISIEN:\n")
  
  coeffs <- model_summary$coefficients
  for(i in 1:nrow(coeffs)) {
    var_name <- rownames(coeffs)[i]
    estimate <- coeffs[i, "Estimate"]
    p_value <- coeffs[i, "Pr(>|t|)"]
    
    if(var_name == "(Intercept)") {
      interpretation <- paste0(interpretation, 
                               "- Intercept: ", round(estimate, 3), 
                               " (nilai Y ketika semua X = 0)\n")
    } else {
      significance <- ifelse(p_value < 0.001, "***", 
                             ifelse(p_value < 0.01, "**", 
                                    ifelse(p_value < 0.05, "*", "ns")))
      
      if(p_value < 0.05) {
        direction <- ifelse(estimate > 0, "MENINGKAT", "MENURUN")
        interpretation <- paste0(interpretation, 
                                 "- ", var_name, ": ", round(estimate, 3), " ", significance, 
                                 " (setiap kenaikan 1 unit ", var_name, 
                                 " menyebabkan Y ", direction, " sebesar ", abs(round(estimate, 3)), ")\n")
      } else {
        interpretation <- paste0(interpretation, 
                                 "- ", var_name, ": ", round(estimate, 3), " ", significance, 
                                 " (tidak signifikan, p = ", round(p_value, 3), ")\n")
      }
    }
  }
  
  # Model quality assessment
  interpretation <- paste0(interpretation, "\nKUALITAS MODEL:\n")
  
  if(model_summary$r.squared > 0.7) {
    quality <- "SANGAT BAIK (R² > 70%)"
  } else if(model_summary$r.squared > 0.5) {
    quality <- "BAIK (R² > 50%)"
  } else if(model_summary$r.squared > 0.3) {
    quality <- "SEDANG (R² > 30%)"
  } else {
    quality <- "LEMAH (R² < 30%)"
  }
  
  interpretation <- paste0(interpretation, "- Daya prediksi: ", quality, "\n")
  
  # Diagnostic interpretation
  if(!is.null(diagnostic_tests)) {
    interpretation <- paste0(interpretation, "\nUJI ASUMSI REGRESI:\n",
                             "- Normalitas residual: ", diagnostic_tests$normality, "\n",
                             "- Homoskedastisitas: ", diagnostic_tests$homoscedasticity, "\n",
                             "- Autokorelasi: ", diagnostic_tests$autocorrelation, "\n",
                             "- Multikolinearitas: ", diagnostic_tests$multicollinearity)
  }
  
  return(interpretation)
}

# Clustering interpretation
interpret_clustering <- function(cluster_result, data, method = "kmeans") {
  if(is.null(cluster_result)) return("Hasil clustering tidak tersedia.")
  
  n_clusters <- length(unique(cluster_result$cluster))
  n_obs <- length(cluster_result$cluster)
  
  interpretation <- paste0("INTERPRETASI ", toupper(method), " CLUSTERING\n\n",
                           "Jumlah cluster: ", n_clusters, "\n",
                           "Jumlah observasi: ", n_obs, "\n")
  
  # Cluster size distribution
  cluster_sizes <- table(cluster_result$cluster)
  interpretation <- paste0(interpretation, "\nUKURAN CLUSTER:\n")
  
  for(i in 1:n_clusters) {
    size <- cluster_sizes[i]
    percentage <- round(size/n_obs * 100, 1)
    interpretation <- paste0(interpretation, 
                             "- Cluster ", i, ": ", size, " observasi (", percentage, "%)\n")
  }
  
  # Cluster characteristics (if centers available)
  if("centers" %in% names(cluster_result)) {
    interpretation <- paste0(interpretation, "\nKARAKTERISTIK CLUSTER:\n")
    centers <- cluster_result$centers
    
    for(i in 1:n_clusters) {
      interpretation <- paste0(interpretation, "Cluster ", i, ":\n")
      
      # Find dominant characteristics
      center_values <- centers[i, ]
      sorted_vars <- names(sort(center_values, decreasing = TRUE))
      top_vars <- sorted_vars[1:min(3, length(sorted_vars))]
      
      interpretation <- paste0(interpretation, 
                               "  Karakteristik dominan: ", paste(top_vars, collapse = ", "), "\n")
      
      # Risk level assessment
      high_risk_vars <- c("POVERTY", "ILLITERATE", "NOELECTRIC", "DPRONE")
      high_risk_scores <- center_values[intersect(names(center_values), high_risk_vars)]
      avg_risk <- mean(high_risk_scores, na.rm = TRUE)
      
      risk_level <- ifelse(avg_risk > 20, "TINGGI", 
                           ifelse(avg_risk > 10, "SEDANG", "RENDAH"))
      
      interpretation <- paste0(interpretation, 
                               "  Tingkat risiko: ", risk_level, "\n\n")
    }
  }
  
  # Recommendations
  interpretation <- paste0(interpretation, 
                           "REKOMENDASI KEBIJAKAN:\n",
                           "1. Prioritaskan intervensi pada cluster dengan risiko TINGGI\n",
                           "2. Kembangkan strategi khusus untuk setiap cluster\n",
                           "3. Alokasikan sumber daya sesuai karakteristik dominan cluster\n",
                           "4. Monitor perubahan komposisi cluster dari waktu ke waktu")
  
  return(interpretation)
}

interpret_clustering_enhanced <- function(cluster_result, data, method = "fgwc", 
                                          spatial_method = NULL, bandwidth = NULL, lambda = NULL) {
  if(is.null(cluster_result)) return("Hasil clustering tidak tersedia.")
  
  n_clusters <- if(method == "fgwc") max(cluster_result$clusters) else length(unique(cluster_result$cluster))
  n_obs <- if(method == "fgwc") length(cluster_result$clusters) else length(cluster_result$cluster)
  
  interpretation <- paste0("INTERPRETASI ", toupper(method), " CLUSTERING\n\n",
                           "Jumlah cluster: ", n_clusters, "\n",
                           "Jumlah observasi: ", n_obs, "\n")
  
  if(method == "fgwc") {
    interpretation <- paste0(interpretation,
                             "Iterasi konvergen: ", cluster_result$iterations, "\n",
                             "Objective function: ", round(cluster_result$objective, 2), "\n")
    
    if(!is.null(spatial_method)) {
      interpretation <- paste0(interpretation,
                               "\nPARAMETER SPASIAL:\n",
                               "- Metode spatial weight: ", spatial_method, "\n")
      
      if(!is.null(bandwidth)) {
        interpretation <- paste0(interpretation,
                                 "- Bandwidth: ", bandwidth, " km\n")
      }
      
      if(!is.null(lambda)) {
        interpretation <- paste0(interpretation,
                                 "- Spatial penalty (λ): ", lambda, "\n")
        
        if(lambda == 0) {
          interpretation <- paste0(interpretation,
                                   "  → Tidak ada constraint spasial (setara fuzzy c-means)\n")
        } else if(lambda < 0.3) {
          interpretation <- paste0(interpretation,
                                   "  → Constraint spasial lemah\n")
        } else if(lambda < 0.7) {
          interpretation <- paste0(interpretation,
                                   "  → Constraint spasial sedang\n")
        } else {
          interpretation <- paste0(interpretation,
                                   "  → Constraint spasial kuat\n")
        }
      }
    }
    
    # Analyze membership uncertainty
    if(!is.null(cluster_result$membership)) {
      max_membership <- apply(cluster_result$membership, 1, max)
      avg_uncertainty <- mean(1 - max_membership)
      
      interpretation <- paste0(interpretation,
                               "\nANALISIS FUZZY:\n",
                               "- Rata-rata ketidakpastian: ", round(avg_uncertainty, 3), "\n")
      
      if(avg_uncertainty < 0.2) {
        interpretation <- paste0(interpretation,
                                 "  → Cluster sangat jelas (hard clustering)\n")
      } else if(avg_uncertainty < 0.4) {
        interpretation <- paste0(interpretation,
                                 "  → Cluster cukup jelas\n")
      } else {
        interpretation <- paste0(interpretation,
                                 "  → Cluster fuzzy (banyak overlap)\n")
      }
    }
  }
  
  # Cluster size distribution (both methods)
  if(method == "fgwc") {
    cluster_sizes <- table(cluster_result$clusters)
  } else {
    cluster_sizes <- table(cluster_result$cluster)
  }
  
  interpretation <- paste0(interpretation, "\nUKURAN CLUSTER:\n")
  
  for(i in 1:n_clusters) {
    size <- cluster_sizes[i]
    percentage <- round(size/n_obs * 100, 1)
    interpretation <- paste0(interpretation, 
                             "- Cluster ", i, ": ", size, " observasi (", percentage, "%)\n")
  }
  
  # Recommendations
  interpretation <- paste0(interpretation, 
                           "\nREKOMENDASI KEBIJAKAN:\n",
                           "1. Prioritaskan intervensi pada cluster dengan karakteristik risiko tinggi\n",
                           "2. Manfaatkan kedekatan geografis untuk koordinasi antar wilayah\n",
                           "3. Kembangkan strategi mitigasi bencana yang spesifik per cluster\n",
                           "4. Alokasikan sumber daya berdasarkan karakteristik dan lokasi cluster\n")
  
  if(method == "fgwc" && !is.null(lambda) && lambda > 0) {
    interpretation <- paste0(interpretation,
                             "5. Pertimbangkan efek spillover antar wilayah dalam implementasi kebijakan\n")
  }
  
  return(interpretation)
}

# ===================================================================
# USER INTERFACE (Extended with complete tabs)
# ===================================================================

ui <- fluidPage(
  useShinyjs(),
  
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")),
  
  # Navigation Bar
  tags$div(class = "navbar",
           tags$div(class = "navbar-content",
                    tags$div(class = "logo", "SIGABISA"),
                    tags$div(class = "nav-links",
                             tags$button("Beranda", class = "nav-link active", id = "nav-beranda",
                                         onclick = "Shiny.setInputValue('current_page', 'beranda', {priority: 'event'})"),
                             tags$button("Manajemen Data", class = "nav-link", id = "nav-manajemen",
                                         onclick = "Shiny.setInputValue('current_page', 'manajemen', {priority: 'event'})"),
                             tags$button("Eksplorasi Data", class = "nav-link", id = "nav-eksplorasi",
                                         onclick = "Shiny.setInputValue('current_page', 'eksplorasi', {priority: 'event'})"),
                             tags$button("Uji Asumsi", class = "nav-link", id = "nav-asumsi",
                                         onclick = "Shiny.setInputValue('current_page', 'asumsi', {priority: 'event'})"),
                             tags$button("Statistik Inferensia", class = "nav-link", id = "nav-inferensia",
                                         onclick = "Shiny.setInputValue('current_page', 'inferensia', {priority: 'event'})"),
                             tags$button("Regresi", class = "nav-link", id = "nav-regresi",
                                         onclick = "Shiny.setInputValue('current_page', 'regresi', {priority: 'event'})"),
                             tags$button("Clustering", class = "nav-link", id = "nav-clustering",
                                         onclick = "Shiny.setInputValue('current_page', 'clustering', {priority: 'event'})")
                    )
           )
  ),
  
  # Main Content
  tags$div(class = "main-content",
           
           # Beranda Page
           conditionalPanel(
             condition = "input.current_page == 'beranda' || input.current_page == null || input.current_page == ''",
             tags$div(class = "hero-section",
                      tags$h1("SIGABISA", class = "hero-title"),
                      tags$h3("Sistem Informasi Geospasial Ancaman Bencana berbasis Indikator Sosial", class = "hero-subtitle"),
                      tags$p("Analisis kerentanan sosial di 511 kabupaten/kota Indonesia berdasarkan data SUSENAS 2017", 
                             class = "hero-description"),
                      tags$button("Mulai Eksplorasi", class = "btn-hero",
                                  onclick = "Shiny.setInputValue('current_page', 'eksplorasi', {priority: 'event'})")
             ),
             tags$div(class = "features-section",
                      fluidRow(
                        column(4, div(class = "feature-card",
                                      h4("Analisis Komprehensif", class = "feature-title"),
                                      p("17 indikator kerentanan dari 511 kabupaten/kota Indonesia menggunakan data SUSENAS 2017")
                        )),
                        column(4, div(class = "feature-card",
                                      h4("Visualisasi Interaktif", class = "feature-title"),
                                      p("Peta dinamis, grafik, dan analisis statistik dengan interpretasi real-time")
                        )),
                        column(4, div(class = "feature-card",
                                      h4("FGWC Clustering", class = "feature-title"),
                                      p("Fuzzy Geographically Weighted Clustering dengan analisis spasial menggunakan distance matrix")
                        ))
                      )
             )
           ),
           
           # Manajemen Data Page (keeping original implementation)
           conditionalPanel(
             condition = "input.current_page == 'manajemen'",
             tags$div(class = "page-header",
                      tags$h1("Manajemen Data", class = "page-title"),
                      tags$p("Pengelolaan, Cleaning, dan Transformasi Data SOVI", class = "page-subtitle")
             ),
             # [Previous manajemen data implementation remains the same]
             tags$div(class = "content-section",
                      tabsetPanel(
                        type = "tabs",
                        
                        tabPanel("Overview Data",
                                 tags$div(class = "controls-card",
                                          fluidRow(
                                            column(6, h4("Informasi Dataset"), tableOutput("dataset_info")),
                                            column(6, h4("Distribusi Regional"), tableOutput("regional_distribution"))
                                          )
                                 ),
                                 tags$div(class = "results-card",
                                          h4("Preview Data"),
                                          DT::dataTableOutput("data_preview")
                                 )
                        ),
                        
                        tabPanel("Metadata",
                                 tags$div(class = "results-card",
                                          h4("Metadata Variabel SOVI"),
                                          p("Informasi lengkap tentang setiap variabel dalam dataset"),
                                          DT::dataTableOutput("metadata_table"),
                                          br(),
                                          downloadButton("download_metadata", "Download Metadata", class = "btn-primary")
                                 )
                        ),
                        
                        tabPanel("Missing Values",
                                 tags$div(class = "controls-card",
                                          h4("Deteksi dan Penanganan Missing Values"),
                                          fluidRow(
                                            column(4, actionButton("check_missing", "Cek Missing Values", class = "btn-primary")),
                                            column(4, selectInput("missing_method", "Metode Penanganan:",
                                                                  choices = c("Hapus Baris" = "delete_rows", "Imputasi Mean" = "mean_impute", "Imputasi Median" = "median_impute"))),
                                            column(4, actionButton("handle_missing", "Tangani Missing", class = "btn-primary"))
                                          )
                                 ),
                                 tags$div(class = "results-card",
                                          h4("Hasil Analisis Missing Values"),
                                          DT::dataTableOutput("missing_analysis"),
                                          br(),
                                          h4("Data Setelah Penanganan"),
                                          DT::dataTableOutput("data_after_missing")
                                 )
                        ),
                        
                        tabPanel("Outlier Detection",
                                 tags$div(class = "controls-card",
                                          h4("Deteksi dan Penanganan Outlier"),
                                          fluidRow(
                                            column(3, selectInput("outlier_variable", "Pilih Variabel:", choices = numeric_variables, selected = "POVERTY")),
                                            column(3, selectInput("outlier_method", "Metode Deteksi:", choices = c("IQR Method" = "iqr", "Z-Score (±3)" = "zscore"))),
                                            column(3, selectInput("outlier_treatment", "Penanganan:", choices = c("Capping/Flooring (IQR)" = "capping_flooring", "Winsorizing (5%-95%)" = "winsorize", "Remove Outliers" = "remove"))),
                                            column(3, br(), actionButton("detect_outliers", "Deteksi", class = "btn-primary"), actionButton("treat_outliers", "Tangani", class = "btn-primary"))
                                          )
                                 ),
                                 tags$div(class = "results-card",
                                          h4("Visualisasi Outlier"),
                                          plotlyOutput("outlier_plot", height = "400px"),
                                          br(),
                                          h4("Hasil Deteksi Outlier"),
                                          DT::dataTableOutput("outlier_results")
                                 )
                        ),
                        
                        tabPanel("Kategorisasi Data",
                                 tags$div(class = "controls-card",
                                          h4("Kategorisasi Variabel Kontinyu"),
                                          fluidRow(
                                            column(4, 
                                                   selectInput("var_to_categorize", "Pilih Variabel:", 
                                                               choices = numeric_variables, selected = "POVERTY"),
                                                   radioButtons("categorization_method", "Metode Kategorisasi:",
                                                                choices = c("Manual (Custom Thresholds)" = "manual",
                                                                            "Otomatis (Quantiles)" = "quantiles", 
                                                                            "Otomatis (K-Means)" = "kmeans",
                                                                            "Otomatis (Natural Breaks/Jenks)" = "jenks",
                                                                            "Otomatis (Equal Intervals)" = "equal"),
                                                                selected = "manual", inline = FALSE)
                                            ),
                                            column(4,
                                                   conditionalPanel(
                                                     condition = "input.categorization_method == 'manual'",
                                                     radioButtons("manual_categories", "Jumlah Kategori Manual:",
                                                                  choices = c("2 Kategori" = "2", "3 Kategori" = "3"),
                                                                  selected = "3", inline = TRUE),
                                                     
                                                     conditionalPanel(
                                                       condition = "input.manual_categories == '2'",
                                                       numericInput("binary_threshold", "Batas Pemisah:", value = 20),
                                                       helpText("Nilai di bawah batas = 0 (Rendah), di atas batas = 1 (Tinggi)")
                                                     ),
                                                     
                                                     conditionalPanel(
                                                       condition = "input.manual_categories == '3'",
                                                       numericInput("low_threshold", "Batas Rendah:", value = 15),
                                                       numericInput("high_threshold", "Batas Tinggi:", value = 25)
                                                     )
                                                   ),
                                                   conditionalPanel(
                                                     condition = "input.categorization_method == 'quantiles' || input.categorization_method == 'kmeans' || input.categorization_method == 'jenks' || input.categorization_method == 'equal'",
                                                     numericInput("n_categories", "Jumlah Kategori:", 
                                                                  value = 3, min = 2, max = 7),
                                                     checkboxInput("auto_optimize", "Optimasi Otomatis Jumlah Kategori", FALSE)
                                                   ),
                                                   conditionalPanel(
                                                     condition = "input.auto_optimize == true",
                                                     p("Sistem akan menentukan jumlah kategori optimal berdasarkan variance ratio")
                                                   )
                                            ),
                                            column(4,
                                                   h5("Preview Kategorisasi:"),
                                                   verbatimTextOutput("categorization_preview"),
                                                   br(),
                                                   actionButton("categorize_variable", "Terapkan Kategorisasi", class = "btn-primary")
                                            )
                                          )
                                 ),
                                 tags$div(class = "results-card",
                                          h4("Hasil Kategorisasi"),
                                          fluidRow(
                                            column(6, 
                                                   h5("Distribusi Kategori"),
                                                   plotlyOutput("categorization_plot", height = "400px")
                                            ),
                                            column(6,
                                                   h5("Statistik per Kategori"),
                                                   DT::dataTableOutput("category_stats")
                                            )
                                          ),
                                          br(),
                                          h4("Data Hasil Kategorisasi"),
                                          DT::dataTableOutput("categorized_data")
                                 )
                        ),
                        tabPanel("Transformasi Data",
                                 tags$div(class = "controls-card",
                                          h4("Transformasi Matematika"),
                                          fluidRow(
                                            column(4, selectInput("transform_variable", "Pilih Variabel:", choices = numeric_variables, selected = "POVERTY")),
                                            column(4, selectInput("transform_method", "Metode Transformasi:",
                                                                  choices = c("Log Natural (ln)" = "log", "Log10" = "log10", "Akar Kuadrat (√)" = "sqrt", "Invers (1/x)" = "inverse", "Box-Cox" = "boxcox"))),
                                            column(4, br(), actionButton("apply_transform", "Terapkan Transformasi", class = "btn-primary"))
                                          )
                                 ),
                                 tags$div(class = "results-card",
                                          h4("Perbandingan Sebelum vs Sesudah"),
                                          plotlyOutput("transformation_plot", height = "400px"),
                                          br(),
                                          h4("Data Hasil Transformasi"),
                                          DT::dataTableOutput("transformed_data_new")
                                 )
                        ),
                        
                        tabPanel("Export Data",
                                 tags$div(class = "controls-card",
                                          h4("Unduh Data"),
                                          fluidRow(
                                            column(6, 
                                                   h5("Data Original"),
                                                   downloadButton("download_csv", "Download CSV", class = "btn-primary"), 
                                                   br(), br(),
                                                   downloadButton("download_excel", "Download Excel", class = "btn-primary")
                                            ),
                                            column(6, 
                                                   h5("Data Hasil Olahan"),
                                                   h6("Pilih Kolom yang Akan Diexport:", style = "margin-top: 10px;"),
                                                   p("Kolom DISTRICTCODE dan REGION selalu disertakan", 
                                                     style = "font-size: 12px; color: #666; margin-bottom: 10px;"),
                                                   div(style = "max-height: 200px; overflow-y: auto; border: 1px solid #ddd; padding: 10px; background-color: #f9f9f9;",
                                                       checkboxGroupInput("export_columns", NULL,
                                                                          choices = NULL,
                                                                          selected = NULL)),
                                                   fluidRow(
                                                     column(6,
                                                            actionButton("select_all_cols", "Pilih Semua", 
                                                                         class = "btn-secondary btn-sm", style = "margin-right: 5px;"),
                                                            actionButton("select_none_cols", "Kosongkan", 
                                                                         class = "btn-secondary btn-sm")
                                                     ),
                                                     column(6,
                                                            actionButton("select_original_only", "Hanya Variabel Asli", 
                                                                         class = "btn-secondary btn-sm")
                                                     )
                                                   ),
                                                   br(),
                                                   div(style = "border-top: 1px solid #ddd; padding-top: 10px;",
                                                       downloadButton("download_processed_csv", "Download Processed CSV", 
                                                                      class = "btn-primary"), 
                                                       br(), br(),
                                                       downloadButton("download_processed_excel", "Download Processed Excel", 
                                                                      class = "btn-primary")
                                                   )
                                            )
                                          ),
                                          br(),
                                          fluidRow(
                                            column(12,
                                                   div(style = "border-top: 2px solid grey; padding-top: 15px;",
                                                       h5("📦 Data Keseluruhan"),
                                                       p("Download semua data (original + processed) dengan metadata lengkap", 
                                                         style = "color: #666; margin-bottom: 10px;"),
                                                       downloadButton("download_all_management", "Download Data Keseluruhan (ZIP)", 
                                                                      class = "btn-primary btn-lg")
                                                   )
                                            )
                                          )
                                 )
                        )
                      )
             )
           ),
           
           # Eksplorasi Data Page (keeping original implementation)
           conditionalPanel(
             condition = "input.current_page == 'eksplorasi'",
             tags$div(class = "page-header",
                      tags$h1("Eksplorasi Data", class = "page-title"),
                      tags$p("Analisis Deskriptif dan Visualisasi Data SOVI", class = "page-subtitle")
             ),
             # [Previous eksplorasi data implementation remains the same]
             tags$div(class = "content-section",
                      tabsetPanel(
                        type = "tabs",
                        
                        tabPanel("Statistik Deskriptif",
                                 tags$div(class = "controls-card",
                                          fluidRow(
                                            column(6, selectizeInput("desc_variables", "Pilih Variabel:", choices = numeric_variables,
                                                                     selected = c("POVERTY", "ILLITERATE", "CHILDREN"), multiple = TRUE,
                                                                     options = list(placeholder = "Ketik atau pilih variabel..."))),
                                            column(6, 
                                                   radioButtons("desc_level", "Level Analisis:",
                                                                choices = c("Provinsi" = "provinsi", "Region" = "region"),
                                                                selected = "provinsi", inline = TRUE),
                                                   
                                                   conditionalPanel(
                                                     condition = "input.desc_level == 'provinsi'",
                                                     selectInput("desc_province", "Pilih Provinsi:", 
                                                                 choices = c("Semua Provinsi" = "all"), 
                                                                 selected = "all")
                                                   ),
                                                   
                                                   conditionalPanel(
                                                     condition = "input.desc_level == 'region'",
                                                     selectInput("desc_region", "Pilih Region:", 
                                                                 choices = c("Semua Region" = "all",
                                                                             "Sumatera" = "Sumatera",
                                                                             "Jawa, Bali, and Nusa Tenggara" = "Jawa, Bali, and Nusa Tenggara",
                                                                             "Kalimantan" = "Kalimantan", 
                                                                             "Sulawesi" = "Sulawesi",
                                                                             "Maluku and Papua" = "Maluku and Papua"), 
                                                                 selected = "all")
                                                   ),
                                                   
                                                   br(), actionButton("run_descriptive", "Analisis", class = "btn-primary")
                                            )
                                          )
                                 ),
                                 tags$div(class = "results-card",
                                          h4("Hasil Statistik Deskriptif"),
                                          DT::dataTableOutput("descriptive_stats"),
                                          br(),
                                          h4("Interpretasi"),
                                          verbatimTextOutput("descriptive_interpretation"),
                                          br(),
                                          downloadButton("download_descriptive", "Download Hasil", class = "btn-primary")
                                 )
                        ),
                        
                        tabPanel("Visualisasi",
                                 tags$div(class = "controls-card",
                                          fluidRow(
                                            column(4, selectInput("viz_type", "Jenis Visualisasi:",
                                                                  choices = c("Histogram", "Boxplot", "Scatter Plot", "Correlation Heatmap", "Density Plot", "Violin Plot", "Bar Chart"),
                                                                  selected = "Boxplot")),
                                            column(4, selectizeInput("viz_variables", "Pilih Variabel:", choices = numeric_variables,
                                                                     selected = c("POVERTY", "ILLITERATE"), multiple = TRUE,
                                                                     options = list(placeholder = "Ketik atau pilih variabel..."))),
                                            column(4, 
                                                   conditionalPanel(condition = "input.viz_type == 'Scatter Plot'",
                                                                    selectInput("scatter_x", "Variabel X:", choices = numeric_variables, selected = "POVERTY"),
                                                                    selectInput("scatter_y", "Variabel Y:", choices = numeric_variables, selected = "ILLITERATE")
                                                   ),
                                                   conditionalPanel(condition = "input.viz_type == 'Boxplot'",
                                                                    radioButtons("boxplot_level", "Level Perbandingan:",
                                                                                 choices = c("Provinsi" = "provinsi", "Region" = "region"),
                                                                                 selected = "provinsi", inline = TRUE)
                                                   ),
                                                   actionButton("create_viz", "Buat Visualisasi", class = "btn-primary")
                                            )
                                          )
                                 ),
                                 tags$div(class = "results-card",
                                          h4("Visualisasi"),
                                          plotlyOutput("visualization_plot", height = "500px"),
                                          br(),
                                          h4("Interpretasi"),
                                          verbatimTextOutput("visualization_interpretation"),
                                          br(),
                                          fluidRow(
                                            column(6, downloadButton("download_visualization", "Download Plot", class = "btn-primary")),
                                            column(6, downloadButton("download_viz_report", "Download Plot + Interpretasi (PDF)", class = "btn-primary"))
                                          )
                                 )
                        ),
                        
                        tabPanel("Peta Spasial",
                                 tags$div(class = "controls-card",
                                          fluidRow(
                                            column(4, selectInput("map_variable", "Variabel untuk Peta:", choices = numeric_variables, selected = "POVERTY")),
                                            column(4, radioButtons("map_level", "Level Aggregasi:",
                                                                   choices = c("Kabupaten/Kota" = "kabupaten", "Provinsi" = "provinsi"), selected = "kabupaten")),
                                            column(4, actionButton("update_map", "Update Peta", class = "btn-primary"),
                                                   br(), br(), downloadButton("download_map", "Download Peta (HTML)", class = "btn-primary"))
                                          )
                                 ),
                                 tags$div(class = "results-card",
                                          h4("Peta Spasial"),
                                          leafletOutput("spatial_map", height = "500px"),
                                          br(),
                                          h4("Interpretasi Peta"),
                                          verbatimTextOutput("map_interpretation"),
                                          br(),
                                          fluidRow(
                                            column(6, downloadButton("download_map_interpretation", "Download Interpretasi (PDF)", class = "btn-primary")),
                                            column(6, downloadButton("download_map_complete", "Download Peta + Interpretasi (ZIP)", class = "btn-primary"))
                                          )
                                 )
                        )
                      )
             )
           ),
           
           conditionalPanel(
             condition = "input.current_page == 'asumsi'",
             tags$div(class = "page-header",
                      tags$h1("Uji Asumsi", class = "page-title"),
                      tags$p("Pengujian Normalitas dan Homogenitas Data", class = "page-subtitle")
             ),
             tags$div(class = "content-section",
                      tabsetPanel(
                        type = "tabs",
                        
                        tabPanel("Input Data",
                                 tags$div(class = "controls-card",
                                          h4("Upload Data untuk Analisis"),
                                          fluidRow(
                                            column(6,
                                                   radioButtons("asumsi_data_source", "Sumber Data:",
                                                                choices = c("Data dari Manajemen Data" = "processed",
                                                                            "Upload File Baru" = "upload"),
                                                                selected = "processed", inline = TRUE),
                                                   conditionalPanel(
                                                     condition = "input.asumsi_data_source == 'upload'",
                                                     fileInput("asumsi_data_file", "Upload File Data:",
                                                               accept = c(".csv", ".xlsx", ".xls"),
                                                               placeholder = "Pilih file CSV atau Excel")
                                                   )
                                            ),
                                            column(6,
                                                   h5("Informasi Data:"),
                                                   verbatimTextOutput("asumsi_data_info"),
                                                   br(),
                                                   actionButton("load_asumsi_data", "Load Data", class = "btn-primary")
                                            )
                                          )
                                 ),
                                 tags$div(class = "results-card",
                                          h4("Preview Data yang Akan Dianalisis"),
                                          DT::dataTableOutput("asumsi_data_preview")
                                 )
                        ),
                        
                        tabPanel("Uji Normalitas",
                                 tags$div(class = "controls-card",
                                          fluidRow(
                                            column(6, selectizeInput("norm_variables", "Pilih Variabel:", 
                                                                     choices = NULL,
                                                                     selected = NULL, multiple = TRUE,
                                                                     options = list(placeholder = "Load data terlebih dahulu..."))),
                                            column(6, selectizeInput("norm_tests", "Pilih Uji:",
                                                                     choices = c("Shapiro-Wilk", "Kolmogorov-Smirnov", "Anderson-Darling"),
                                                                     selected = c("Shapiro-Wilk", "Anderson-Darling"), multiple = TRUE,
                                                                     options = list(placeholder = "Pilih jenis uji...")),
                                                   actionButton("run_normality", "Jalankan Uji", class = "btn-primary"))
                                          )
                                 ),
                                 tags$div(class = "results-card",
                                          h4("Hasil Uji Normalitas"),
                                          DT::dataTableOutput("normality_results"),
                                          br(),
                                          h4("Q-Q Plots"),
                                          plotlyOutput("qq_plots", height = "500px"),
                                          br(),
                                          h4("Interpretasi"),
                                          verbatimTextOutput("normality_interpretation"),
                                          br(),
                                          fluidRow(
                                            column(6, downloadButton("download_normality", "Download Hasil", class = "btn-primary")),
                                            column(6, downloadButton("download_normality_report", "Download Plot + Interpretasi (PDF)", class = "btn-primary"))
                                          )
                                 )
                        ),
                        
                        tabPanel("Uji Homogenitas",
                                 tags$div(class = "controls-card",
                                          h4("Konfigurasi Uji Homogenitas"),
                                          fluidRow(
                                            column(4, 
                                                   selectInput("homog_variable", "Variabel:", choices = NULL),
                                                   br(),
                                                   actionButton("run_homogeneity", "Jalankan Uji", class = "btn-primary")
                                            ),
                                            column(4, 
                                                   radioButtons("homog_group_type", "Tipe Pengelompokan:",
                                                                choices = c("Berdasarkan Region" = "region", 
                                                                            "Berdasarkan Provinsi" = "provinsi",
                                                                            "Custom Grouping" = "custom"),
                                                                selected = "region")
                                            ),
                                            column(4,
                                                   conditionalPanel(
                                                     condition = "input.homog_group_type == 'provinsi'",
                                                     h5("Pilih Provinsi untuk Dibandingkan:"),
                                                     selectizeInput("homog_provinces", NULL,
                                                                    choices = NULL, multiple = TRUE,
                                                                    options = list(placeholder = "Pilih minimal 2 provinsi..."))
                                                   ),
                                                   conditionalPanel(
                                                     condition = "input.homog_group_type == 'custom'",
                                                     h5("Variabel Pengelompokan:"),
                                                     selectInput("homog_custom_var", NULL,
                                                                 choices = NULL)
                                                   )
                                            )
                                          )
                                 ),
                                 tags$div(class = "results-card",
                                          h4("Hasil Uji Homogenitas"),
                                          verbatimTextOutput("homogeneity_results"),
                                          br(),
                                          h4("Box Plot untuk Visualisasi"),
                                          plotlyOutput("homogeneity_plot", height = "400px"),
                                          br(),
                                          h4("Interpretasi"),
                                          verbatimTextOutput("homogeneity_interpretation"),
                                          br(),
                                          fluidRow(
                                            column(6, downloadButton("download_homogeneity", "Download Hasil", class = "btn-primary")),
                                            column(6, downloadButton("download_homogeneity_report", "Download Plot + Interpretasi (PDF)", class = "btn-primary"))
                                          )
                                 )
                        )
                      )
             )
           ),
           
           conditionalPanel(
             condition = "input.current_page == 'inferensia'",
             tags$div(class = "page-header",
                      tags$h1("Statistik Inferensia", class = "page-title"),
                      tags$p("Uji Hipotesis dan Analisis Perbandingan", class = "page-subtitle")
             ),
             tags$div(class = "content-section",
                      tabsetPanel(
                        type = "tabs",
                        
                        tabPanel("Input Data",
                                 tags$div(class = "controls-card",
                                          h4("Upload Data untuk Analisis Inferensia"),
                                          fluidRow(
                                            column(6,
                                                   radioButtons("inferensia_data_source", "Sumber Data:",
                                                                choices = c("Data dari Manajemen Data" = "processed",
                                                                            "Upload File Baru" = "upload"),
                                                                selected = "processed", inline = TRUE),
                                                   conditionalPanel(
                                                     condition = "input.inferensia_data_source == 'upload'",
                                                     fileInput("inferensia_data_file", "Upload File Data:",
                                                               accept = c(".csv", ".xlsx", ".xls"),
                                                               placeholder = "Pilih file CSV atau Excel")
                                                   )
                                            ),
                                            column(6,
                                                   h5("Informasi Data:"),
                                                   verbatimTextOutput("inferensia_data_info"),
                                                   br(),
                                                   actionButton("load_inferensia_data", "Load Data", class = "btn-primary")
                                            )
                                          )
                                 ),
                                 tags$div(class = "results-card",
                                          h4("Preview Data yang Akan Dianalisis"),
                                          DT::dataTableOutput("inferensia_data_preview")
                                 )
                        ),
                        tabPanel("Uji Rata-rata",
                                 tags$div(class = "controls-card",
                                          h4("Pilih Jenis Uji Rata-rata"),
                                          radioButtons("mean_test_type", "Jenis Uji:",
                                                       choices = c("One Sample t-test" = "one_sample",
                                                                   "Two Sample t-test (Independent)" = "two_sample_indep", 
                                                                   "Paired t-test (Dependent)" = "paired"),
                                                       selected = "one_sample"),
                                          
                                          # One Sample (sudah ada, tinggal pindahkan)
                                          conditionalPanel(
                                            condition = "input.mean_test_type == 'one_sample'",
                                            fluidRow(
                                              column(4, selectInput("t_variable", "Variabel:", choices = NULL)),
                                              column(4, numericInput("mu_value", "Nilai μ₀ (Hipotesis):", value = 15)),
                                              column(4, radioButtons("t_alternative", "Hipotesis Alternatif:",
                                                                     choices = c("Dua arah (≠)" = "two.sided",
                                                                                 "Lebih besar (>)" = "greater", 
                                                                                 "Lebih kecil (<)" = "less"),
                                                                     selected = "two.sided"))
                                            )
                                          ),
                                          
                                          # Two Sample Independent (BARU)
                                          conditionalPanel(
                                            condition = "input.mean_test_type == 'two_sample_indep'",
                                            fluidRow(
                                              column(4, selectInput("t2_variable", "Variabel Numerik:", choices = NULL)),
                                              column(4, selectInput("t2_grouping", "Variabel Pengelompokan:", choices = NULL)),
                                              column(4, 
                                                     checkboxInput("t2_equal_var", "Equal Variances", TRUE),
                                                     radioButtons("t2_alternative", "Hipotesis Alternatif:",
                                                                  choices = c("Dua arah (≠)" = "two.sided",
                                                                              "Group1 > Group2" = "greater", 
                                                                              "Group1 < Group2" = "less"),
                                                                  selected = "two.sided"))
                                            )
                                          ),
                                          
                                          # Paired t-test (BARU)
                                          conditionalPanel(
                                            condition = "input.mean_test_type == 'paired'",
                                            fluidRow(
                                              column(4, selectInput("paired_var1", "Variabel 1 (Before):", choices = NULL)),
                                              column(4, selectInput("paired_var2", "Variabel 2 (After):", choices = NULL)),
                                              column(4, radioButtons("paired_alternative", "Hipotesis Alternatif:",
                                                                     choices = c("Dua arah (≠)" = "two.sided",
                                                                                 "Var1 > Var2" = "greater", 
                                                                                 "Var1 < Var2" = "less"),
                                                                     selected = "two.sided"))
                                            )
                                          ),
                                          
                                          actionButton("run_mean_test", "Jalankan Uji", class = "btn-primary")
                                 ),
                                 
                                 tags$div(class = "results-card",
                                          h4("Hasil Uji Rata-rata"),
                                          verbatimTextOutput("mean_test_results"),
                                          br(),
                                          h4("Visualisasi"),
                                          plotlyOutput("mean_test_plot", height = "400px"),
                                          br(),
                                          h4("Interpretasi Lengkap"),
                                          verbatimTextOutput("mean_test_interpretation"),
                                          br(),
                                          fluidRow(
                                            column(6, downloadButton("download_mean_test", "Download Hasil", class = "btn-primary")),
                                            column(6, downloadButton("download_mean_test_report", "Download Report (PDF)", class = "btn-primary"))
                                          )
                                 )
                        ),
                        
                        tabPanel("Uji Proporsi",
                                 tags$div(class = "controls-card",
                                          h4("Pilih Jenis Uji Proporsi"),
                                          # NOTES sebelum uji proporsi
                                          tags$div(class = "alert alert-info", style = "background-color: #e3f2fd; padding: 10px; margin-bottom: 15px;",
                                                   tags$strong("CATATAN PENTING:"), br(),
                                                   "• Untuk uji proporsi, data harus dalam format 0/1 atau TRUE/FALSE", br(),
                                                   "• Jika belum ada, gunakan ", tags$strong("Kategorisasi 2 Kategori"), " di tab Manajemen Data", br(),
                                                   "• Pilih variabel dengan akhiran '_Numeric' untuk data 0/1", br(),
                                                   "• Pilih variabel dengan akhiran '_category' untuk pengelompokkan atau variabel lain yag memiliki 2 kelompok"
                                          ),
                                          radioButtons("prop_test_type", "Jenis Uji:",
                                                       choices = c("One Proportion z-test" = "one_prop",
                                                                   "Two Proportion z-test" = "two_prop"),
                                                       selected = "one_prop"),
                                          
                                          # One Proportion
                                          conditionalPanel(
                                            condition = "input.prop_test_type == 'one_prop'",
                                            fluidRow(
                                              column(4, 
                                                     radioButtons("prop1_input_type", "Jenis Input:",
                                                                  choices = c("Raw Data" = "raw", "Manual Input" = "manual"),
                                                                  selected = "raw")),
                                              column(4,
                                                     conditionalPanel(condition = "input.prop1_input_type == 'raw'",
                                                                      selectInput("prop1_variable", "Variabel (0/1 atau TRUE/FALSE):", choices = NULL)),
                                                     conditionalPanel(condition = "input.prop1_input_type == 'manual'",
                                                                      numericInput("prop1_success", "Jumlah Success:", value = 50),
                                                                      numericInput("prop1_total", "Total Sample:", value = 100))
                                              ),
                                              column(4,
                                                     numericInput("prop1_p0", "Proporsi Hipotesis (p₀):", value = 0.5, min = 0, max = 1, step = 0.01),
                                                     radioButtons("prop1_alternative", "Hipotesis Alternatif:",
                                                                  choices = c("Dua arah (≠)" = "two.sided",
                                                                              "Lebih besar (>)" = "greater", 
                                                                              "Lebih kecil (<)" = "less"),
                                                                  selected = "two.sided"))
                                            )
                                          ),
                                          
                                          # Two Proportion
                                          conditionalPanel(
                                            condition = "input.prop_test_type == 'two_prop'",
                                            fluidRow(
                                              column(4,
                                                     radioButtons("prop2_input_type", "Jenis Input:",
                                                                  choices = c("Raw Data" = "raw", "Manual Input" = "manual"),
                                                                  selected = "raw")),
                                              column(4,
                                                     conditionalPanel(condition = "input.prop2_input_type == 'raw'",
                                                                      selectInput("prop2_variable", "Variabel Proporsi:", choices = NULL),
                                                                      selectInput("prop2_grouping", "Variabel Pengelompokan:", choices = NULL)),
                                                     conditionalPanel(condition = "input.prop2_input_type == 'manual'",
                                                                      numericInput("prop2_success1", "Success Group 1:", value = 30),
                                                                      numericInput("prop2_total1", "Total Group 1:", value = 100),
                                                                      numericInput("prop2_success2", "Success Group 2:", value = 40),
                                                                      numericInput("prop2_total2", "Total Group 2:", value = 100))
                                              ),
                                              column(4,
                                                     radioButtons("prop2_alternative", "Hipotesis Alternatif:",
                                                                  choices = c("Dua arah (≠)" = "two.sided",
                                                                              "p1 > p2" = "greater", 
                                                                              "p1 < p2" = "less"),
                                                                  selected = "two.sided"))
                                            )
                                          ),
                                          
                                          actionButton("run_prop_test", "Jalankan Uji", class = "btn-primary")
                                 ),
                                 
                                 tags$div(class = "results-card",
                                          h4("Hasil Uji Proporsi"),
                                          verbatimTextOutput("prop_test_results"),
                                          br(),
                                          h4("Visualisasi"),
                                          plotlyOutput("prop_test_plot", height = "400px"),
                                          br(),
                                          h4("Interpretasi Lengkap"),
                                          verbatimTextOutput("prop_test_interpretation"),
                                          br(),
                                          fluidRow(
                                            column(6, downloadButton("download_prop_test", "Download Hasil", class = "btn-primary")),
                                            column(6, downloadButton("download_prop_test_report", "Download Report (PDF)", class = "btn-primary"))
                                          )
                                 )
                        ),
                        
                        tabPanel("Uji Ragam",
                                 tags$div(class = "controls-card",
                                          h4("Pilih Jenis Uji Ragam/Variance"),
                                          tags$div(class = "alert alert-info", style = "background-color: #e3f2fd; padding: 10px; margin-bottom: 15px;",
                                                   tags$strong("CATATAN PENTING:"), br(),
                                                   
                                                   "• Untuk uji 2 sampel, pilih variabel dengan akhiran '_category' untuk pengelompokkan dari hasil kategorisasi di manajemen data", br(),
                                                   "• Atau bisa menggunakan variabel lain yag memiliki 2 kelompok"
                                          ),
                                          radioButtons("var_test_type", "Jenis Uji:",
                                                       choices = c("One Sample Variance (Chi-square)" = "one_var",
                                                                   "Two Sample Variance (F-test)" = "two_var"),
                                                       selected = "one_var"),
                                          
                                          # One Sample Variance
                                          conditionalPanel(
                                            condition = "input.var_test_type == 'one_var'",
                                            fluidRow(
                                              column(4, selectInput("var1_variable", "Variabel:", choices = NULL)),
                                              column(4, numericInput("var1_sigma0", "Variance Hipotesis (σ²₀):", value = 1, min = 0)),
                                              column(4, radioButtons("var1_alternative", "Hipotesis Alternatif:",
                                                                     choices = c("Dua arah (≠)" = "two.sided",
                                                                                 "Lebih besar (>)" = "greater", 
                                                                                 "Lebih kecil (<)" = "less"),
                                                                     selected = "two.sided"))
                                            )
                                          ),
                                          
                                          # Two Sample Variance
                                          conditionalPanel(
                                            condition = "input.var_test_type == 'two_var'",
                                            fluidRow(
                                              column(4, selectInput("var2_variable", "Variabel Numerik:", choices = NULL)),
                                              column(4, selectInput("var2_grouping", "Variabel Pengelompokan:", choices = NULL)),
                                              column(4, radioButtons("var2_alternative", "Hipotesis Alternatif:",
                                                                     choices = c("Dua arah (≠)" = "two.sided",
                                                                                 "σ₁² > σ₂²" = "greater", 
                                                                                 "σ₁² < σ₂²" = "less"),
                                                                     selected = "two.sided"))
                                            )
                                          ),
                                          
                                          actionButton("run_var_test", "Jalankan Uji", class = "btn-primary")
                                 ),
                                 
                                 tags$div(class = "results-card",
                                          h4("Hasil Uji Ragam"),
                                          verbatimTextOutput("var_test_results"),
                                          br(),
                                          h4("Visualisasi"),
                                          plotlyOutput("var_test_plot", height = "400px"),
                                          br(),
                                          h4("Interpretasi Lengkap"),
                                          verbatimTextOutput("var_test_interpretation"),
                                          br(),
                                          fluidRow(
                                            column(6, downloadButton("download_var_test", "Download Hasil", class = "btn-primary")),
                                            column(6, downloadButton("download_var_test_report", "Download Report (PDF)", class = "btn-primary"))
                                          )
                                 )
                        ),
                        
                        tabPanel("ANOVA",
                                 tags$div(class = "controls-card",
                                          h4("Pilih Jenis ANOVA"),
                                          radioButtons("anova_type", "Jenis ANOVA:",
                                                       choices = c("One-way ANOVA" = "one_way",
                                                                   "Two-way ANOVA" = "two_way"),
                                                       selected = "one_way"),
                                          
                                          # One-way ANOVA 
                                          conditionalPanel(
                                            condition = "input.anova_type == 'one_way'",
                                            fluidRow(
                                              column(6, selectInput("anova1_dependent", "Variabel Terikat:", choices = NULL)),
                                              column(6, selectInput("anova1_factor", "Faktor:", choices = NULL))
                                            )
                                          ),
                                          
                                          # Two-way ANOVA 
                                          conditionalPanel(
                                            condition = "input.anova_type == 'two_way'",
                                            fluidRow(
                                              column(4, selectInput("anova2_dependent", "Variabel Terikat:", choices = NULL)),
                                              column(4, selectInput("anova2_factor1", "Faktor 1:", choices = NULL)),
                                              column(4, selectInput("anova2_factor2", "Faktor 2:", choices = NULL)),
                                              column(12, checkboxInput("anova2_interaction", "Include Interaction", TRUE))
                                            )
                                          ),
                                          
                                          actionButton("run_anova_test", "Jalankan ANOVA", class = "btn-primary")
                                 ),
                                 
                                 tags$div(class = "results-card",
                                          h4("Hasil ANOVA"),
                                          verbatimTextOutput("anova_test_results"),
                                          br(),
                                          h4("Post-hoc Test"),
                                          DT::dataTableOutput("anova_posthoc_results"),
                                          br(),
                                          h4("Visualisasi ANOVA"),
                                          plotlyOutput("anova_test_plot", height = "400px"),
                                          br(),
                                          h4("Interpretasi Lengkap"),
                                          verbatimTextOutput("anova_test_interpretation"),
                                          br(),
                                          fluidRow(
                                            column(6, downloadButton("download_anova_test", "Download Hasil", class = "btn-primary")),
                                            column(6, downloadButton("download_anova_test_report", "Download Report (PDF)", class = "btn-primary"))
                                          )
                                 )
                        )
                      )
             )
           ),
           
           # COMPLETE Regresi Page with Assumption Tests
           conditionalPanel(
             condition = "input.current_page == 'regresi'",
             tags$div(class = "page-header",
                      tags$h1("Regresi Linear Berganda", class = "page-title"),
                      tags$p("Analisis Hubungan dan Prediksi dengan Uji Asumsi", class = "page-subtitle")
             ),
             tags$div(class = "content-section",
                      tabsetPanel(
                        type = "tabs",
                        
                        tabPanel("Input Data",
                                 tags$div(class = "controls-card",
                                          h4("Upload Data untuk Analisis Regresi"),
                                          fluidRow(
                                            column(6,
                                                   radioButtons("regresi_data_source", "Sumber Data:",
                                                                choices = c("Data dari Manajemen Data" = "processed",
                                                                            "Upload File Baru" = "upload"),
                                                                selected = "processed", inline = TRUE),
                                                   conditionalPanel(
                                                     condition = "input.regresi_data_source == 'upload'",
                                                     fileInput("regresi_data_file", "Upload File Data:",
                                                               accept = c(".csv", ".xlsx", ".xls"),
                                                               placeholder = "Pilih file CSV atau Excel")
                                                   )
                                            ),
                                            column(6,
                                                   h5("Informasi Data:"),
                                                   verbatimTextOutput("regresi_data_info"),
                                                   br(),
                                                   actionButton("load_regresi_data", "Load Data", class = "btn-primary")
                                            )
                                          )
                                 ),
                                 tags$div(class = "results-card",
                                          h4("Preview Data yang Akan Dianalisis"),
                                          DT::dataTableOutput("regresi_data_preview")
                                 )
                        ),
                        
                        tabPanel("Seleksi Model Terbaik",
                                 tags$div(class = "controls-card",
                                          h4("Seleksi Model Terbaik"),
                                          p("Pilih variabel dan metode untuk menemukan model terbaik sebelum building"),
                                          fluidRow(
                                            column(4,
                                                   selectInput("sel_dependent", "Variabel Terikat:", choices = NULL),
                                                   selectizeInput("sel_predictors", "Variabel Prediktor (Kandidat):", 
                                                                  choices = NULL, multiple = TRUE,
                                                                  options = list(placeholder = "Load data terlebih dahulu..."))
                                            ),
                                            column(4,
                                                   selectInput("sel_method", "Metode Seleksi:",
                                                               choices = c("Backward Elimination" = "backward",
                                                                           "Forward Selection" = "forward", 
                                                                           "Stepwise (Both)" = "both"),
                                                               selected = "both"),
                                                   numericInput("sel_alpha_in", "Alpha to Enter:", value = 0.05, min = 0.01, max = 0.1, step = 0.01),
                                                   numericInput("sel_alpha_out", "Alpha to Remove:", value = 0.10, min = 0.01, max = 0.15, step = 0.01)
                                            ),
                                            column(4,
                                                   h5("Info Kandidat:"),
                                                   verbatimTextOutput("candidate_info"),
                                                   br(),
                                                   actionButton("run_model_selection", "Jalankan Seleksi Model", class = "btn-primary")
                                            )
                                          )
                                 ),
                                 tags$div(class = "results-card",
                                          h4("Hasil Seleksi Model"),
                                          fluidRow(
                                            column(6,
                                                   h5("Model Terbaik Berdasarkan Kriteria:"),
                                                   DT::dataTableOutput("best_models_table"),
                                                   br(),
                                                   h5("Model yang Direkomendasikan:"),
                                                   verbatimTextOutput("recommended_model")
                                            ),
                                            column(6,
                                                   h5("Perbandingan Semua Kriteria:"),
                                                   DT::dataTableOutput("all_criteria_table"),
                                                   br(),
                                                   h5("Visualisasi Kriteria:"),
                                                   plotlyOutput("criteria_comparison_plot", height = "300px")
                                            )
                                          ),
                                          br(),
                                          h4("Interpretasi Seleksi"),
                                          verbatimTextOutput("selection_complete_interpretation"),
                                          br(),
                                          fluidRow(
                                            column(6, 
                                                   actionButton("apply_selected_model", "Gunakan Model Terpilih untuk Building", 
                                                                class = "btn-success", style = "font-weight: bold;")),
                                            column(6, downloadButton("download_selection_results", "Download Hasil Seleksi", class = "btn-primary"))
                                          )
                                 )
                        ),
                        
                        tabPanel("Model Building",
                                 tags$div(class = "controls-card",
                                          fluidRow(
                                            column(6, selectInput("reg_dependent", "Variabel Terikat:", choices = NULL)),
                                            column(6, selectizeInput("reg_predictors", "Prediktor:", choices = NULL,
                                                                     selected = NULL, multiple = TRUE,
                                                                     options = list(placeholder = "Load data terlebih dahulu...")))
                                          ),
                                          actionButton("build_model", "Buat Model", class = "btn-primary")
                                 ),
                                 tags$div(class = "results-card",
                                          h4("Ringkasan Model"),
                                          verbatimTextOutput("model_summary"),
                                          br(),
                                          h4("Interpretasi Model"),
                                          verbatimTextOutput("model_interpretation"),
                                          br(),
                                          downloadButton("download_model", "Download Model", class = "btn-primary")
                                 )
                        ),
                        
                        tabPanel("Uji Asumsi Regresi",
                                 tags$div(class = "controls-card",
                                          h4("Uji Asumsi Regresi Linear"),
                                          p("Pastikan model telah dibuat di tab 'Model Building' terlebih dahulu"),
                                          actionButton("run_regression_assumptions", "Jalankan Uji Asumsi", class = "btn-primary")
                                 ),
                                 tags$div(class = "results-card",
                                          h4("Hasil Uji Asumsi"),
                                          verbatimTextOutput("regression_assumptions_results"),
                                          br(),
                                          h4("Plot Asumsi Regresi"),
                                          plotlyOutput("regression_assumptions_plot", height = "600px"),
                                          br(),
                                          h4("Interpretasi Uji Asumsi"),
                                          verbatimTextOutput("regression_assumptions_interpretation"),
                                          br(),
                                          fluidRow(
                                            column(6, downloadButton("download_regression_assumptions", "Download Hasil", class = "btn-primary")),
                                            column(6, downloadButton("download_regression_assumptions_report", "Download Plot + Interpretasi (PDF)", class = "btn-primary"))
                                          )
                                 )
                        ),
                        
                        tabPanel("Diagnostik Model",
                                 tags$div(class = "results-card",
                                          h4("Plot Diagnostik Regresi"),
                                          plotlyOutput("diagnostic_plots", height = "600px"),
                                          br(),
                                          h4("Interpretasi Diagnostik"),
                                          verbatimTextOutput("diagnostic_interpretation"),
                                          br(),
                                          fluidRow(
                                            column(6, downloadButton("download_diagnostic", "Download Plot", class = "btn-primary")),
                                            column(6, downloadButton("download_diagnostic_report", "Download Plot + Interpretasi (PDF)", class = "btn-primary"))
                                          )
                                 )
                        ),
                        
                        tabPanel("Prediksi",
                                 tags$div(class = "controls-card",
                                          h4("Prediksi Menggunakan Model"),
                                          p("Masukkan nilai prediktor untuk prediksi"),
                                          uiOutput("prediction_inputs"),
                                          actionButton("make_prediction", "Buat Prediksi", class = "btn-primary")
                                 ),
                                 tags$div(class = "results-card",
                                          h4("Hasil Prediksi"),
                                          verbatimTextOutput("prediction_results"),
                                          br(),
                                          h4("Visualisasi Prediksi vs Aktual"),
                                          plotlyOutput("prediction_plot", height = "400px"),
                                          br(),
                                          downloadButton("download_prediction", "Download Hasil Prediksi", class = "btn-primary")
                                 )
                        )
                      )
             )
           ),
           
           conditionalPanel(
             condition = "input.current_page == 'clustering'",
             tags$div(class = "page-header",
                      tags$h1("Analisis Clustering", class = "page-title"),
                      tags$p("K-Means dan FGWC untuk Pengelompokan Spasial", class = "page-subtitle")
             ),
             tags$div(class = "content-section",
                      tabsetPanel(
                        type = "tabs",
                        
                        tabPanel("Input Data",
                                 tags$div(class = "controls-card",
                                          h4("Upload Data untuk Analisis Clustering"),
                                          fluidRow(
                                            column(6,
                                                   radioButtons("clustering_data_source", "Sumber Data:",
                                                                choices = c("Data dari Manajemen Data" = "processed",
                                                                            "Upload File Baru" = "upload"),
                                                                selected = "processed", inline = TRUE),
                                                   conditionalPanel(
                                                     condition = "input.clustering_data_source == 'upload'",
                                                     fileInput("clustering_data_file", "Upload File Data:",
                                                               accept = c(".csv", ".xlsx", ".xls"),
                                                               placeholder = "Pilih file CSV atau Excel")
                                                   )
                                            ),
                                            column(6,
                                                   h5("Informasi Data:"),
                                                   verbatimTextOutput("clustering_data_info"),
                                                   br(),
                                                   actionButton("load_clustering_data", "Load Data", class = "btn-primary")
                                            )
                                          )
                                 ),
                                 tags$div(class = "results-card",
                                          h4("Preview Data yang Akan Dianalisis"),
                                          DT::dataTableOutput("clustering_data_preview")
                                 )
                        ),
                        
                        tabPanel("K-Means Clustering",
                                 tags$div(class = "controls-card",
                                          fluidRow(
                                            column(6, selectizeInput("cluster_variables", "Pilih Variabel untuk Clustering:", 
                                                                     choices = NULL,
                                                                     selected = NULL, multiple = TRUE,
                                                                     options = list(placeholder = "Load data terlebih dahulu..."))),
                                            column(6, radioButtons("k_method", "Metode Penentuan K:",
                                                                   choices = c("Elbow Method" = "elbow", "Manual" = "manual"), selected = "elbow"),
                                                   conditionalPanel(condition = "input.k_method == 'manual'",
                                                                    numericInput("manual_k", "Jumlah Cluster:", value = 3, min = 2, max = 8)),
                                                   actionButton("run_clustering", "Jalankan K-Means", class = "btn-primary"))
                                          )
                                 ),
                                 tags$div(class = "results-card",
                                          h4("Elbow Method Plot"),
                                          plotlyOutput("elbow_plot", height = "400px"),
                                          br(),
                                          h4("Hasil K-Means Clustering"),
                                          DT::dataTableOutput("cluster_results"),
                                          br(),
                                          h4("Interpretasi Clustering"),
                                          verbatimTextOutput("clustering_interpretation"),
                                          br(),
                                          fluidRow(
                                            column(6, downloadButton("download_clustering", "Download Hasil", class = "btn-primary")),
                                            column(6, downloadButton("download_clustering_report", "Download Plot + Interpretasi (PDF)", class = "btn-primary"))
                                          )
                                 )
                        ),
                        
                        tabPanel("FGWC Clustering",
                                 tags$div(class = "controls-card",
                                          h4("Fuzzy Geographically Weighted Clustering"),
                                          fluidRow(
                                            column(4, 
                                                   selectizeInput("fgwc_variables", "Pilih Variabel untuk FGWC:", 
                                                                  choices = NULL,
                                                                  selected = NULL, multiple = TRUE,
                                                                  options = list(placeholder = "Load data terlebih dahulu...")),
                                                   numericInput("fgwc_k", "Jumlah Cluster:", value = 3, min = 2, max = 6),
                                                   numericInput("fgwc_m", "Fuzziness Parameter (m):", value = 2, min = 1.1, max = 5, step = 0.1)
                                            ),
                                            column(4,
                                                   h5("Parameter Spasial:"),
                                                   selectInput("fgwc_spatial_method", "Spatial Weight Method:",
                                                               choices = c("Gaussian" = "gaussian", 
                                                                           "Inverse Distance" = "inverse_distance",
                                                                           "Exponential" = "exponential"),
                                                               selected = "gaussian"),
                                                   numericInput("fgwc_bandwidth", "Bandwidth (km):", 
                                                                value = 100, min = 10, max = 1000, step = 10),
                                                   numericInput("fgwc_lambda", "Spatial Penalty (λ):", 
                                                                value = 0.5, min = 0, max = 1, step = 0.1)
                                            ),
                                            column(4,
                                                   h5("Keterangan Parameter:"),
                                                   tags$div(style = "font-size: 12px; color: #666;",
                                                            tags$p("• ", tags$strong("Spatial Method:"), " Metode perhitungan weight spasial"),
                                                            tags$p("• ", tags$strong("Bandwidth:"), " Jarak pengaruh spasial (km)"),  
                                                            tags$p("• ", tags$strong("Lambda (λ):"), " Bobot constraint spasial (0-1)"),
                                                            tags$p("  - λ = 0: Tidak ada constraint spasial"),
                                                            tags$p("  - λ = 1: Constraint spasial maksimal")
                                                   ),
                                                   br(),
                                                   actionButton("run_fgwc", "Jalankan FGWC", class = "btn-primary")
                                            )
                                          )
                                 ),
                                 tags$div(class = "results-card",
                                          h4("Hasil FGWC"),
                                          DT::dataTableOutput("fgwc_results"),
                                          br(),
                                          h4("Membership Matrix (Top 10)"),
                                          DT::dataTableOutput("fgwc_membership"),
                                          br(),
                                          h4("Interpretasi FGWC"),
                                          verbatimTextOutput("fgwc_interpretation"),
                                          br(),
                                          fluidRow(
                                            column(6, downloadButton("download_fgwc", "Download Hasil", class = "btn-primary")),
                                            column(6, downloadButton("download_fgwc_report", "Download Plot + Interpretasi (PDF)", class = "btn-primary"))
                                          )
                                 )
                        ),
                        
                        tabPanel("Visualisasi Cluster",
                                 tags$div(class = "controls-card",
                                          h4("Kontrol Visualisasi Cluster"),
                                          fluidRow(
                                            column(6,
                                                   radioButtons("cluster_map_method", "Pilih Metode untuk Dipetakan:",
                                                                choices = c("K-Means Clustering" = "kmeans",
                                                                            "FGWC Clustering" = "fgwc"),
                                                                selected = "kmeans", inline = TRUE)
                                            ),
                                            column(6,
                                                   textOutput("cluster_map_status"),
                                                   br(),
                                                   actionButton("update_cluster_map", "Update Peta", class = "btn-primary")
                                            )
                                          )
                                 ),
                                 tags$div(class = "results-card",
                                          h4("Peta Cluster"),
                                          leafletOutput("cluster_map", height = "500px"),
                                          br(),
                                          h4("Silhouette Analysis"),
                                          plotlyOutput("silhouette_plot", height = "400px"),
                                          br(),
                                          h4("Interpretasi Visualisasi Cluster"),
                                          verbatimTextOutput("cluster_visual_interpretation"),
                                          br(),
                                          fluidRow(
                                            column(6, downloadButton("download_cluster_viz", "Download Visualisasi", class = "btn-primary")),
                                            column(6, downloadButton("download_cluster_complete", "Download Semua Hasil Clustering (ZIP)", class = "btn-primary"))
                                          )
                                 )
                        )
                      )
             )
           )
  )
)

# ===================================================================
# SERVER LOGIC (Complete implementation)
# ===================================================================

server <- function(input, output, session) {
  
  # Reactive values for storing results
  values <- reactiveValues(
    kmeans_result = NULL,
    fgwc_result = NULL,
    cluster_data = NULL,
    cluster_method_available = c(), 
    current_cluster_map_data = NULL,
    current_descriptive = NULL,
    current_descriptive_data = NULL,
    current_viz_data = NULL,
    current_viz_plot = NULL,
    current_viz_interpretation = NULL,
    current_map_data = NULL,
    current_map_widget = NULL,
    processed_data = NULL,
    missing_handled_data = NULL,
    outlier_treated_data = NULL,
    transformed_data = NULL,
    categorized_data = NULL,
    kabupaten_choices = NULL,
    normality_results = NULL,
    normality_plots = NULL,
    normality_interpretation = NULL,
    homogeneity_results = NULL,
    homogeneity_plots = NULL,
    homogeneity_interpretation = NULL,
    regression_model = NULL,
    regression_assumptions = NULL,
    ttest_results = NULL,
    anova_results = NULL,
    posthoc_results = NULL,
    # Data input
    asumsi_data = NULL,
    inferensia_data = NULL,
    regresi_data = NULL,
    clustering_data = NULL,
    model_selection_results = NULL,
    selected_best_model = NULL,
    recommended_formula = NULL
  )
  
  load_data_from_file <- function(file_path) {
    if(is.null(file_path)) return(NULL)
    
    tryCatch({
      file_ext <- tools::file_ext(file_path)
      
      if(file_ext %in% c("csv")) {
        data <- read.csv(file_path, stringsAsFactors = FALSE)
      } else if(file_ext %in% c("xlsx", "xls")) {
        data <- openxlsx::read.xlsx(file_path)
      } else {
        stop("Format file tidak didukung. Gunakan CSV atau Excel.")
      }
      
      # Validasi data
      if(nrow(data) < 5) {
        stop("Data terlalu sedikit (minimal 5 baris)")
      }
      
      return(data)
    }, error = function(e) {
      showNotification(paste("Error loading file:", e$message), type = "error")
      return(NULL)
    })
  }
  
  # Initialize processed_data with original data
  observe({
    if(is.null(values$processed_data)) {
      values$processed_data <- sovi_data
    }
  })
  
  # Navigation handling
  observe({
    req(input$current_page)
    shinyjs::removeClass(selector = ".nav-link", class = "active")
    shinyjs::addClass(selector = paste0("#nav-", input$current_page), class = "active")
  })
  
  # Province-kabupaten dependency update
  observe({
    if(!is.null(geojson_data) && "nmprov" %in% names(geojson_data)) {
      values$kabupaten_choices <- geojson_data %>%
        st_drop_geometry() %>%
        select(nmprov, nmkab) %>%
        distinct() %>%
        arrange(nmprov, nmkab)
    } else if(!is.null(values$processed_data)) {
      values$kabupaten_choices <- values$processed_data %>%
        select(REGION, DISTRICTCODE) %>%
        distinct() %>%
        arrange(REGION, DISTRICTCODE)
    }
  })
  
  # Observer untuk update choices di semua tab ketika processed_data berubah
  observe({
    if(!is.null(values$processed_data)) {
      # Get updated columns
      numeric_cols <- names(values$processed_data)[sapply(values$processed_data, is.numeric)]
      char_factor_cols <- names(values$processed_data)[sapply(values$processed_data, function(x) is.character(x) || is.factor(x) || (is.numeric(x) && length(unique(x)) <= 10))]
      all_cols <- names(values$processed_data)
      
      # Binary numeric variables (for proportion tests)
      binary_numeric_cols <- numeric_cols[sapply(values$processed_data[numeric_cols], function(x) {
        unique_vals <- unique(x[!is.na(x)])
        length(unique_vals) == 2 && all(unique_vals %in% c(0, 1))
      })]
      
      # ===================================================================
      # UPDATE EKSPLORASI DATA CHOICES
      # ===================================================================
      updateSelectizeInput(session, "desc_variables", 
                           choices = numeric_cols,
                           selected = input$desc_variables)
      
      updateSelectizeInput(session, "viz_variables", 
                           choices = numeric_cols,
                           selected = input$viz_variables)
      
      updateSelectInput(session, "scatter_x", 
                        choices = numeric_cols, 
                        selected = input$scatter_x)
      
      updateSelectInput(session, "scatter_y", 
                        choices = numeric_cols, 
                        selected = input$scatter_y)
      
      updateSelectInput(session, "map_variable", 
                        choices = numeric_cols, 
                        selected = input$map_variable)
      
      # ===================================================================
      # UPDATE UJI ASUMSI CHOICES
      # ===================================================================
      updateSelectizeInput(session, "norm_variables", 
                           choices = numeric_cols,
                           selected = input$norm_variables)
      
      updateSelectInput(session, "homog_variable", 
                        choices = numeric_cols, 
                        selected = input$homog_variable)
      
      updateSelectInput(session, "homog_custom_var", 
                        choices = char_factor_cols,
                        selected = input$homog_custom_var)
      
      # ===================================================================
      # UPDATE STATISTIK INFERENSIA CHOICES
      # ===================================================================
      
      # Mean tests
      updateSelectInput(session, "t_variable", 
                        choices = numeric_cols, 
                        selected = input$t_variable)
      
      updateSelectInput(session, "t2_variable", 
                        choices = numeric_cols, 
                        selected = input$t2_variable)
      
      updateSelectInput(session, "t2_grouping", 
                        choices = char_factor_cols, 
                        selected = input$t2_grouping)
      
      updateSelectInput(session, "paired_var1", 
                        choices = numeric_cols, 
                        selected = input$paired_var1)
      
      updateSelectInput(session, "paired_var2", 
                        choices = numeric_cols, 
                        selected = input$paired_var2)
      
      # Proportion tests - prioritize binary variables
      prop_choices <- c(binary_numeric_cols, all_cols)
      prop_choices <- prop_choices[!duplicated(prop_choices)]
      
      updateSelectInput(session, "prop1_variable", 
                        choices = prop_choices, 
                        selected = input$prop1_variable)
      
      updateSelectInput(session, "prop2_variable", 
                        choices = prop_choices, 
                        selected = input$prop2_variable)
      
      updateSelectInput(session, "prop2_grouping", 
                        choices = char_factor_cols, 
                        selected = input$prop2_grouping)
      
      # Variance tests
      updateSelectInput(session, "var1_variable", 
                        choices = numeric_cols, 
                        selected = input$var1_variable)
      
      updateSelectInput(session, "var2_variable", 
                        choices = numeric_cols, 
                        selected = input$var2_variable)
      
      updateSelectInput(session, "var2_grouping", 
                        choices = char_factor_cols, 
                        selected = input$var2_grouping)
      
      # ANOVA
      updateSelectInput(session, "anova1_dependent", 
                        choices = numeric_cols, 
                        selected = input$anova1_dependent)
      
      updateSelectInput(session, "anova1_factor", 
                        choices = char_factor_cols, 
                        selected = input$anova1_factor)
      
      updateSelectInput(session, "anova2_dependent", 
                        choices = numeric_cols, 
                        selected = input$anova2_dependent)
      
      updateSelectInput(session, "anova2_factor1", 
                        choices = char_factor_cols, 
                        selected = input$anova2_factor1)
      
      updateSelectInput(session, "anova2_factor2", 
                        choices = char_factor_cols, 
                        selected = input$anova2_factor2)
      
      # ===================================================================
      # UPDATE REGRESI CHOICES
      # ===================================================================
      updateSelectInput(session, "reg_dependent", 
                        choices = numeric_cols, 
                        selected = input$reg_dependent)
      
      updateSelectizeInput(session, "reg_predictors", 
                           choices = numeric_cols,
                           selected = input$reg_predictors)
      
      updateSelectInput(session, "sel_dependent", 
                        choices = numeric_cols, 
                        selected = input$sel_dependent)
      
      updateSelectizeInput(session, "sel_predictors", 
                           choices = numeric_cols,
                           selected = input$sel_predictors)
      
      # ===================================================================
      # UPDATE CLUSTERING CHOICES
      # ===================================================================
      updateSelectizeInput(session, "cluster_variables", 
                           choices = numeric_cols,
                           selected = input$cluster_variables)
      
      updateSelectizeInput(session, "fgwc_variables", 
                           choices = numeric_cols,
                           selected = input$fgwc_variables)
      
      cat("✓ All choices updated with new variables:", length(numeric_cols), "numeric,", length(char_factor_cols), "factor\n")
      cat("✓ Binary variables for proportion tests:", length(binary_numeric_cols), "\n")
      
      # Detect new variables added
      original_vars <- intersect(numeric_variables, names(values$processed_data))
      all_current_vars <- names(values$processed_data)
      new_vars <- setdiff(all_current_vars, c(original_vars, "DISTRICTCODE", "REGION"))
      
      if(length(new_vars) > 0) {
        cat("✓ New variables detected:", paste(new_vars, collapse = ", "), "\n")
        
        # Categorize new variables
        categorical_vars <- new_vars[grepl("_Category$", new_vars)]
        transformed_vars <- new_vars[grepl("_(ln|log10|sqrt|inv|boxcox)$", new_vars)]
        other_vars <- setdiff(new_vars, c(categorical_vars, transformed_vars))
        
        if(length(categorical_vars) > 0) {
          cat("✓ Categorical variables:", paste(categorical_vars, collapse = ", "), "\n")
        }
        if(length(transformed_vars) > 0) {
          cat("✓ Transformed variables:", paste(transformed_vars, collapse = ", "), "\n")
        }
      }
    }
  })
  
  observe({
    if(!is.null(geojson_data) && "nmprov" %in% names(geojson_data)) {
      province_list <- unique(geojson_data$nmprov)
      province_list <- province_list[!is.na(province_list)]
      province_list <- sort(province_list)
      
      updateSelectInput(session, "desc_province", 
                        choices = c("Semua Provinsi" = "all", setNames(province_list, province_list)))
    }
  })
  
  # ===================================================================
  # MANAJEMEN DATA SECTION 
  # ===================================================================
  
  # Update available columns for export
  observe({
    if(!is.null(values$processed_data)) {
      all_cols <- names(values$processed_data)
      # Exclude mandatory columns from choices
      optional_cols <- setdiff(all_cols, c("DISTRICTCODE", "REGION"))
      
      # Categorize columns dengan nama variabel yang benar
      original_vars <- intersect(numeric_variables, optional_cols)
      categorical_vars <- optional_cols[grepl("_Category$", optional_cols)]
      transformed_vars <- optional_cols[grepl("_(ln|log10|sqrt|inv|boxcox)$", optional_cols)]
      other_vars <- setdiff(optional_cols, c(original_vars, categorical_vars, transformed_vars))
      
      # PERBAIKAN: Buat choices yang menampilkan setiap variabel individual
      choices_list <- c()  # Ubah dari list() menjadi vector kosong
      
      if(length(original_vars) > 0) {
        # Tambahkan setiap variabel original secara individual
        for(var in original_vars) {
          choices_list[var] <- var
        }
      }
      if(length(categorical_vars) > 0) {
        # Tambahkan setiap variabel kategori secara individual  
        for(var in categorical_vars) {
          choices_list[var] <- var
        }
      }
      if(length(transformed_vars) > 0) {
        # Tambahkan setiap variabel transformasi secara individual
        for(var in transformed_vars) {
          choices_list[var] <- var
        }
      }
      if(length(other_vars) > 0) {
        # Tambahkan variabel lain secara individual
        for(var in other_vars) {
          choices_list[var] <- var
        }
      }
      
      updateCheckboxGroupInput(session, "export_columns", 
                               choices = choices_list,
                               selected = original_vars)
    } else {
      # Jika belum ada processed data, gunakan original data
      updateCheckboxGroupInput(session, "export_columns", 
                               choices = setNames(numeric_variables, numeric_variables),
                               selected = head(numeric_variables, 5))
    }
  })
  
  # Select all columns button
  observeEvent(input$select_all_cols, {
    if(!is.null(values$processed_data)) {
      all_optional_cols <- setdiff(names(values$processed_data), c("DISTRICTCODE", "REGION"))
      updateCheckboxGroupInput(session, "export_columns", selected = all_optional_cols)
    }
  })
  
  # Select none button
  observeEvent(input$select_none_cols, {
    updateCheckboxGroupInput(session, "export_columns", selected = character(0))
  })
  
  # Select original variables only
  observeEvent(input$select_original_only, {
    if(!is.null(values$processed_data)) {
      original_vars <- intersect(numeric_variables, names(values$processed_data))
      updateCheckboxGroupInput(session, "export_columns", selected = original_vars)
    }
  })
  
  # Dataset info
  output$dataset_info <- renderTable({
    current_data <- if(is.null(values$processed_data)) sovi_data else values$processed_data
    data.frame(
      Metric = c("Total Districts", "Variables", "Regions", "Data Source", "Year", "Processing Status"),
      Value = c(nrow(current_data), ncol(current_data)-2, 
                length(unique(current_data$REGION)), "SUSENAS BPS", "2017",
                ifelse(is.null(values$processed_data), "Original", "Processed"))
    )
  })
  
  # Regional distribution
  output$regional_distribution <- renderTable({
    current_data <- if(is.null(values$processed_data)) sovi_data else values$processed_data
    current_data %>%
      group_by(REGION) %>%
      summarise(Districts = n(), .groups = 'drop') %>%
      arrange(REGION)
  })
  
  # Data preview
  output$data_preview <- DT::renderDataTable({
    DT::datatable(sovi_data, options = list(scrollX = TRUE, pageLength = 10))
  })
  
  # Metadata table
  output$metadata_table <- DT::renderDataTable({
    metadata_df <- data.frame(
      Variabel = c("CHILDREN", "FEMALE", "ELDERLY", "FHEAD", "FAMILYSIZE", 
                   "NOELECTRIC", "LOWEDU", "GROWTH", "POVERTY", "ILLITERATE", 
                   "NOTRAINING", "DPRONE", "RENTED", "NOSEWER", "TAPWATER", "POPULATION"),
      Alias = c("Anak-anak", "Perempuan", "Lansia", "KRT Perempuan", "Ukuran Keluarga",
                "Tanpa Listrik", "Pendidikan Rendah", "Pertumbuhan", "Kemiskinan", "Buta Huruf",
                "Tanpa Pelatihan", "Rawan Bencana", "Rumah Sewa", "Tanpa Sanitasi", "Air Bersih", "Populasi"),
      Konsep = c("Proporsi penduduk usia 0-14 tahun", "Proporsi penduduk perempuan", 
                 "Proporsi penduduk usia 65+ tahun", "Proporsi KRT perempuan", "Rata-rata anggota per rumah tangga",
                 "Proporsi rumah tangga tanpa listrik", "Proporsi penduduk tanpa pendidikan formal", 
                 "Tingkat pertumbuhan penduduk", "Proporsi penduduk miskin", "Proporsi penduduk buta huruf",
                 "Proporsi penduduk tanpa pelatihan kerja", "Proporsi rumah tangga di daerah rawan bencana", 
                 "Proporsi rumah tangga penyewa", "Proporsi rumah tangga tanpa sanitasi", 
                 "Proporsi akses air bersih", "Jumlah total penduduk"),
      Definisi = c("Persentase penduduk berusia di bawah 15 tahun terhadap total populasi",
                   "Persentase penduduk berjenis kelamin perempuan terhadap total populasi",
                   "Persentase penduduk berusia 65 tahun ke atas terhadap total populasi",
                   "Persentase rumah tangga dengan kepala rumah tangga perempuan",
                   "Rata-rata jumlah anggota keluarga per rumah tangga",
                   "Persentase rumah tangga yang tidak memiliki akses listrik",
                   "Persentase penduduk dengan tingkat pendidikan di bawah SMP",
                   "Tingkat pertumbuhan penduduk tahunan dalam persen",
                   "Persentase penduduk yang hidup di bawah garis kemiskinan",
                   "Persentase penduduk usia 15+ yang tidak dapat membaca dan menulis",
                   "Persentase rumah tangga yang tidak pernah mengikuti pelatihan bencana",
                   "Persentase rumah tangga yang tinggal di daerah rawan bencana alam",
                   "Persentase rumah tangga yang tinggal di rumah sewa/kontrak",
                   "Persentase rumah tangga tanpa akses sanitasi yang layak",
                   "Persentase rumah tangga dengan akses air bersih",
                   "Jumlah total penduduk dalam wilayah tersebut"),
      Satuan = c("Persen (%)", "Persen (%)", "Persen (%)", "Persen (%)", "Orang",
                 "Persen (%)", "Persen (%)", "Persen (%)", "Persen (%)", "Persen (%)",
                 "Persen (%)", "Persen (%)", "Persen (%)", "Persen (%)", "Persen (%)", "Jiwa"),
      Tipe_Data = rep("Numerik", 16),
      Referensi = rep("SUSENAS BPS 2017", 16)
    )
    
    DT::datatable(metadata_df, options = list(scrollX = TRUE, pageLength = 16),
                  caption = "Metadata Lengkap Variabel SOVI Dataset")
  })
  
  # Missing values analysis
  observeEvent(input$check_missing, {
    req(values$processed_data)
    
    tryCatch({
      available_vars <- intersect(numeric_variables, names(values$processed_data))
      
      if(length(available_vars) == 0) {
        showNotification("Tidak ada variabel numerik yang tersedia", type = "warning")
        return()
      }
      
      missing_summary <- values$processed_data %>%
        select(all_of(available_vars)) %>%
        summarise_all(~sum(is.na(.))) %>%
        pivot_longer(everything(), names_to = "Variable", values_to = "Missing_Count") %>%
        mutate(
          Missing_Percentage = round((Missing_Count / nrow(values$processed_data)) * 100, 2),
          Status = case_when(
            Missing_Count == 0 ~ "✅ Complete",
            Missing_Percentage < 5 ~ "⚠️ Minor",
            TRUE ~ "🚨 Major"
          )
        ) %>%
        arrange(desc(Missing_Count))
      
      output$missing_analysis <- DT::renderDataTable({
        DT::datatable(missing_summary, options = list(scrollX = TRUE),
                      caption = "Analisis Missing Values per Variabel") %>%
          DT::formatStyle("Status", backgroundColor = DT::styleEqual(
            c("✅ Complete", "⚠️ Minor", "🚨 Major"),
            c("lightgreen", "lightyellow", "lightcoral")))
      })
      
      cat("Missing values analysis completed for", length(available_vars), "variables\n")
      
    }, error = function(e) {
      showNotification(paste("Error dalam analisis missing values:", e$message), type = "error")
      cat("Error in missing analysis:", e$message, "\n")
    })
  })
  
  # Handle missing values
  observeEvent(input$handle_missing, {
    req(input$missing_method, values$processed_data)
    
    tryCatch({
      available_vars <- intersect(numeric_variables, names(values$processed_data))
      
      if(length(available_vars) == 0) {
        showNotification("Tidak ada variabel numerik untuk diproses", type = "warning")
        return()
      }
      
      missing_before <- sum(is.na(values$processed_data[available_vars]))
      
      if(input$missing_method == "delete_rows") {
        complete_rows <- complete.cases(values$processed_data[, available_vars, drop = FALSE])
        values$processed_data <- values$processed_data[complete_rows, ]
        message_text <- "Baris dengan missing values dihapus."
        
      } else if(input$missing_method == "mean_impute") {
        for(col in available_vars) {
          col_data <- values$processed_data[[col]]
          missing_idx <- is.na(col_data)
          if(sum(missing_idx) > 0) {
            mean_val <- mean(col_data, na.rm = TRUE)
            if(!is.na(mean_val)) {
              values$processed_data[[col]][missing_idx] <- mean_val
            }
          }
        }
        message_text <- "Missing values diisi dengan rata-rata."
        
      } else if(input$missing_method == "median_impute") {
        for(col in available_vars) {
          col_data <- values$processed_data[[col]]
          missing_idx <- is.na(col_data)
          if(sum(missing_idx) > 0) {
            median_val <- median(col_data, na.rm = TRUE)
            if(!is.na(median_val)) {
              values$processed_data[[col]][missing_idx] <- median_val
            }
          }
        }
        message_text <- "Missing values diisi dengan median."
      }
      
      missing_after <- sum(is.na(values$processed_data[available_vars]))
      
      output$data_after_missing <- DT::renderDataTable({
        DT::datatable(values$processed_data, options = list(scrollX = TRUE, pageLength = 10),
                      caption = paste("Data Setelah Penanganan Missing Values - Metode:", input$missing_method, 
                                      "| Baris:", nrow(values$processed_data), 
                                      "| Missing sebelum:", missing_before, "| Missing sesudah:", missing_after))
      })
      
      showNotification(paste(message_text, "Missing values berkurang dari", missing_before, "ke", missing_after), type = "message")
      cat("Missing values handled successfully. Before:", missing_before, "After:", missing_after, "\n")
      
    }, error = function(e) {
      showNotification(paste("Error dalam penanganan missing values:", e$message), type = "error")
      cat("Error in handling missing values:", e$message, "\n")
    })
  })
  
  # Outlier detection
  observeEvent(input$detect_outliers, {
    req(input$outlier_variable, input$outlier_method, values$processed_data)
    
    tryCatch({
      if(!input$outlier_variable %in% names(values$processed_data)) {
        showNotification(paste("Variabel", input$outlier_variable, "tidak ditemukan"), type = "error")
        return()
      }
      
      var_data <- values$processed_data[[input$outlier_variable]]
      var_data_clean <- var_data[!is.na(var_data) & is.finite(var_data)]
      
      if(length(var_data_clean) < 5) {
        showNotification("Data tidak cukup untuk deteksi outlier (minimal 5 observasi)", type = "warning")
        return()
      }
      
      if(input$outlier_method == "iqr") {
        Q1 <- quantile(var_data_clean, 0.25, na.rm = TRUE)
        Q3 <- quantile(var_data_clean, 0.75, na.rm = TRUE)
        IQR_val <- Q3 - Q1
        lower_bound <- Q1 - 1.5 * IQR_val
        upper_bound <- Q3 + 1.5 * IQR_val
        method_desc <- "IQR Method (Q1-1.5xIQR, Q3+1.5xIQR)"
        
      } else if(input$outlier_method == "zscore") {
        mean_val <- mean(var_data_clean, na.rm = TRUE)
        sd_val <- sd(var_data_clean, na.rm = TRUE)
        lower_bound <- mean_val - 3 * sd_val
        upper_bound <- mean_val + 3 * sd_val
        method_desc <- "Z-Score Method (mean ± 3 SD)"
      }
      
      outlier_analysis <- data.frame(
        Index = 1:nrow(values$processed_data),
        DISTRICTCODE = values$processed_data$DISTRICTCODE,
        REGION = values$processed_data$REGION,
        Value = values$processed_data[[input$outlier_variable]],
        stringsAsFactors = FALSE
      ) %>%
        filter(!is.na(Value) & is.finite(Value)) %>%
        mutate(
          Outlier_Type = ifelse(Value > upper_bound, "Upper Outlier",
                                ifelse(Value < lower_bound, "Lower Outlier", "Normal")),
          Distance_From_Bound = ifelse(Value > upper_bound, round(Value - upper_bound, 3),
                                       ifelse(Value < lower_bound, round(lower_bound - Value, 3), 0)),
          Is_Outlier = Outlier_Type != "Normal"
        )
      
      upper_outliers <- sum(outlier_analysis$Outlier_Type == "Upper Outlier")
      lower_outliers <- sum(outlier_analysis$Outlier_Type == "Lower Outlier")
      total_outliers <- upper_outliers + lower_outliers
      
      p <- ggplot(outlier_analysis, aes(x = Index, y = Value, color = Outlier_Type)) +
        geom_point(alpha = 0.7, size = 1.5) +
        scale_color_manual(
          values = c("Normal" = "#22c55e", "Upper Outlier" = "#ef4444", "Lower Outlier" = "#f59e0b"),
          name = "Status"
        ) +
        geom_hline(yintercept = upper_bound, linetype = "dashed", color = "#ef4444", alpha = 0.8) +
        geom_hline(yintercept = lower_bound, linetype = "dashed", color = "#f59e0b", alpha = 0.8) +
        annotate("text", x = max(outlier_analysis$Index) * 0.7, y = upper_bound + (max(outlier_analysis$Value) - upper_bound) * 0.1, 
                 label = paste("Upper Bound:", round(upper_bound, 2)), color = "#ef4444", size = 3) +
        annotate("text", x = max(outlier_analysis$Index) * 0.7, y = lower_bound - (lower_bound - min(outlier_analysis$Value)) * 0.1, 
                 label = paste("Lower Bound:", round(lower_bound, 2)), color = "#f59e0b", size = 3) +
        theme_minimal() +
        labs(
          title = paste("Deteksi Outlier -", input$outlier_variable),
          subtitle = paste(method_desc, "| Total Outliers:", total_outliers, "(Upper:", upper_outliers, ", Lower:", lower_outliers, ")"),
          x = "Index Observasi", 
          y = input$outlier_variable
        ) +
        theme(legend.position = "bottom")
      
      output$outlier_plot <- renderPlotly({
        ggplotly(p, tooltip = c("x", "y", "colour"))
      })
      
      if(total_outliers > 0) {
        outlier_results <- outlier_analysis %>%
          filter(Is_Outlier) %>%
          select(DISTRICTCODE, REGION, Value, Outlier_Type, Distance_From_Bound) %>%
          arrange(desc(abs(Distance_From_Bound)))
        
        output$outlier_results <- DT::renderDataTable({
          DT::datatable(outlier_results, options = list(scrollX = TRUE, pageLength = 10),
                        caption = paste("Outliers Terdeteksi:", total_outliers, "dari", nrow(outlier_analysis), 
                                        "| Upper Outliers:", upper_outliers, "| Lower Outliers:", lower_outliers)) %>%
            DT::formatStyle("Outlier_Type", backgroundColor = DT::styleEqual(
              c("Upper Outlier", "Lower Outlier"),
              c("#fecaca", "#fed7aa")
            ))
        })
      } else {
        output$outlier_results <- DT::renderDataTable({
          DT::datatable(data.frame(Message = "Tidak ada outlier terdeteksi dengan metode ini"),
                        options = list(dom = 't'), caption = "Hasil Deteksi Outlier - Data Bersih")
        })
      }
      
      values$current_outlier_info <- list(
        variable = input$outlier_variable,
        method = input$outlier_method,
        method_desc = method_desc,
        upper_indices = which(values$processed_data[[input$outlier_variable]] > upper_bound),
        lower_indices = which(values$processed_data[[input$outlier_variable]] < lower_bound),
        all_indices = which(values$processed_data[[input$outlier_variable]] > upper_bound | 
                              values$processed_data[[input$outlier_variable]] < lower_bound),
        bounds = c(lower_bound, upper_bound),
        counts = c(lower_outliers, upper_outliers, total_outliers),
        winsorize_bounds = c(quantile(var_data_clean, 0.05), quantile(var_data_clean, 0.95))
      )
      
    }, error = function(e) {
      showNotification(paste("Error dalam deteksi outlier:", e$message), type = "error")
      output$outlier_plot <- renderPlotly({
        plotly_empty() %>% layout(title = paste("Error:", e$message))
      })
    })
  })
  
  # Treat outliers
  observeEvent(input$treat_outliers, {
    req(input$outlier_variable, input$outlier_treatment, values$processed_data)
    
    tryCatch({
      if(is.null(values$current_outlier_info)) {
        showNotification("Silakan deteksi outlier terlebih dahulu", type = "warning")
        return()
      }
      
      outlier_info <- values$current_outlier_info
      var_name <- outlier_info$variable
      bounds <- outlier_info$bounds
      lower_bound <- bounds[1]
      upper_bound <- bounds[2]
      
      counts <- outlier_info$counts
      lower_count <- counts[1]
      upper_count <- counts[2] 
      total_count <- counts[3]
      
      if(total_count == 0) {
        showNotification("Tidak ada outlier untuk ditangani", type = "message")
        return()
      }
      
      original_var <- values$processed_data[[var_name]]
      original_rows <- nrow(values$processed_data)
      
      if(input$outlier_treatment == "capping_flooring") {
        treated_var <- original_var
        
        lower_outlier_idx <- outlier_info$lower_indices
        if(length(lower_outlier_idx) > 0) {
          treated_var[lower_outlier_idx] <- lower_bound
        }
        
        upper_outlier_idx <- outlier_info$upper_indices
        if(length(upper_outlier_idx) > 0) {
          treated_var[upper_outlier_idx] <- upper_bound
        }
        
        values$processed_data[[var_name]] <- treated_var
        
        treatment_msg <- paste("Capping/Flooring berhasil diterapkan.",
                               "Upper outliers (", upper_count, ") di-cap ke", round(upper_bound, 3), 
                               ". Lower outliers (", lower_count, ") di-floor ke", round(lower_bound, 3))
        
      } else if(input$outlier_treatment == "winsorize") {
        winsorize_bounds <- outlier_info$winsorize_bounds
        P5 <- winsorize_bounds[1]
        P95 <- winsorize_bounds[2]
        
        treated_var <- original_var
        treated_var[!is.na(treated_var) & treated_var < P5] <- P5
        treated_var[!is.na(treated_var) & treated_var > P95] <- P95
        
        values$processed_data[[var_name]] <- treated_var
        
        winsorized_lower <- sum(!is.na(original_var) & original_var < P5)
        winsorized_upper <- sum(!is.na(original_var) & original_var > P95)
        
        treatment_msg <- paste("Winsorizing berhasil diterapkan.",
                               "Data di-winsorize ke P5 (", round(P5, 3), "):", winsorized_lower, "nilai",
                               "dan P95 (", round(P95, 3), "):", winsorized_upper, "nilai")
        
      } else if(input$outlier_treatment == "remove") {
        outlier_indices <- outlier_info$all_indices
        values$processed_data <- values$processed_data[-outlier_indices, ]
        
        treatment_msg <- paste("Remove outliers berhasil.",
                               total_count, "outliers dihapus (", upper_count, "upper,", lower_count, "lower).",
                               "Data tersisa:", nrow(values$processed_data), "observasi")
      }
      
      showNotification(paste(treatment_msg, "- Data ter-update di semua menu"), type = "message", duration = 6)
      cat("✓ Outlier treatment completed successfully\n")
      cat("✓ Data rows: ", original_rows, "→", nrow(values$processed_data), "\n")
      
      # Force re-detection with updated data
      shinyjs::delay(1000, shinyjs::click("detect_outliers"))
      
      # TRIGGER UPDATE: Force reactive update
      shinyjs::delay(200, {
        cat("✓ Choices will be updated in all tabs after outlier treatment\n")
      })
      
    }, error = function(e) {
      showNotification(paste("Error dalam penanganan outlier:", e$message), type = "error")
    })
  })
  
  # Preview kategorisasi
  output$categorization_preview <- renderText({
    req(input$var_to_categorize, values$processed_data)
    
    if(!input$var_to_categorize %in% names(values$processed_data)) {
      return("Variabel tidak ditemukan")
    }
    
    var_data <- values$processed_data[[input$var_to_categorize]]
    
    if(input$categorization_method == "manual") {
      if(input$manual_categories == "2") {
        preview_text <- paste0(
          "Metode: Manual (2 Kategori)\n",
          "Kategori 1 (0/Rendah): < ", input$binary_threshold, "\n",
          "Kategori 2 (1/Tinggi): ≥ ", input$binary_threshold, "\n"
        )
      } else {
        preview_text <- paste0(
          "Metode: Manual (3 Kategori)\n",
          "Kategori 1 (Rendah): < ", input$low_threshold, "\n",
          "Kategori 2 (Sedang): ", input$low_threshold, " - ", input$high_threshold, "\n",
          "Kategori 3 (Tinggi): > ", input$high_threshold, "\n"
        )
      }
    } else {
      n_cat <- if(input$auto_optimize) {
        optimize_categories(var_data)
      } else {
        input$n_categories
      }
      
      result <- categorize_variable_auto(var_data, input$categorization_method, 
                                         input$n_categories, input$auto_optimize)
      
      preview_text <- paste0(
        "Metode: ", toupper(input$categorization_method), "\n",
        "Jumlah kategori: ", result$n_categories, 
        if(input$auto_optimize) " (optimized)" else "", "\n",
        "Breaks: ", paste(round(result$breaks, 2), collapse = " | "), "\n"
      )
    }
    
    return(preview_text)
  })
  
  # Plot kategorisasi
  output$categorization_plot <- renderPlotly({
    req(input$var_to_categorize, values$processed_data)
    
    if(!input$var_to_categorize %in% names(values$processed_data)) {
      return(plotly_empty() %>% layout(title = "Variabel tidak ditemukan"))
    }
    
    var_data <- values$processed_data[[input$var_to_categorize]]
    
    if(input$categorization_method == "manual") {
      if(input$manual_categories == "2") {
        # 2 kategori manual - FIX untuk binary
        breaks <- c(min(var_data, na.rm = TRUE), input$binary_threshold, max(var_data, na.rm = TRUE))
        
      } else {
        # 3 kategori manual (existing code)
        breaks <- c(min(var_data, na.rm = TRUE), input$low_threshold, input$high_threshold, max(var_data, na.rm = TRUE))
      }
    } else {
      result <- categorize_variable_auto(var_data, input$categorization_method, 
                                         input$n_categories, input$auto_optimize)
      breaks <- result$breaks
    }
    
    # Create histogram with category boundaries
    p <- ggplot(data.frame(value = var_data), aes(x = value)) +
      geom_histogram(bins = 30, fill = "#dc2626", alpha = 0.7, color = "white") +
      geom_vline(xintercept = breaks[-c(1, length(breaks))], 
                 linetype = "dashed", color = "#991b1b", size = 1) +
      theme_minimal() +
      labs(title = paste("Distribusi", input$var_to_categorize, "dengan Batas Kategori"),
           x = input$var_to_categorize, y = "Frekuensi")
    
    # Add break labels
    for(i in 2:(length(breaks)-1)) {
      p <- p + annotate("text", x = breaks[i], y = Inf, 
                        label = round(breaks[i], 2), vjust = 2, color = "#991b1b")
    }
    
    ggplotly(p)
  })
  
  # Statistics per category
  output$category_stats <- DT::renderDataTable({
    req(input$var_to_categorize, values$processed_data)
    
    if(!input$var_to_categorize %in% names(values$processed_data)) {
      return(DT::datatable(data.frame(Message = "Variabel tidak ditemukan")))
    }
    
    var_data <- values$processed_data[[input$var_to_categorize]]
    
    if(input$categorization_method == "manual") {
      if(input$manual_categories == "2") {
        # 2 kategori manual - FIX
        category_values <- rep(NA_character_, length(var_data))
        category_values[!is.na(var_data) & var_data < input$binary_threshold] <- "0"
        category_values[!is.na(var_data) & var_data >= input$binary_threshold] <- "1"
        
      } else {
        # 3 kategori manual
        category_values <- rep(NA_character_, length(var_data))
        category_values[!is.na(var_data) & var_data < input$low_threshold] <- "Rendah"
        category_values[!is.na(var_data) & var_data >= input$low_threshold & var_data <= input$high_threshold] <- "Sedang"
        category_values[!is.na(var_data) & var_data > input$high_threshold] <- "Tinggi"
      }
    } else {
      result <- categorize_variable_auto(var_data, input$categorization_method, 
                                         input$n_categories, input$auto_optimize)
      category_values <- as.character(result$categories)
    }
    
    # Calculate statistics per category
    stats_data <- data.frame(
      Variable = var_data,
      Category = category_values
    ) %>%
      filter(!is.na(Category)) %>%
      group_by(Category) %>%
      summarise(
        Count = n(),
        Percentage = round(n() / nrow(.) * 100, 1),
        Mean = round(mean(Variable, na.rm = TRUE), 2),
        Median = round(median(Variable, na.rm = TRUE), 2),
        Min = round(min(Variable, na.rm = TRUE), 2),
        Max = round(max(Variable, na.rm = TRUE), 2),
        .groups = 'drop'
      )
    
    # Create color palette that matches number of categories
    unique_categories <- unique(stats_data$Category)
    colors <- RColorBrewer::brewer.pal(max(3, length(unique_categories)), "Set3")[1:length(unique_categories)]
    
    DT::datatable(stats_data, options = list(scrollX = TRUE, pageLength = 10),
                  caption = "Statistik per Kategori") %>%
      DT::formatStyle("Category", backgroundColor = DT::styleEqual(
        unique_categories, colors
      ))
  })
  
  # Kategorisasi
  observeEvent(input$categorize_variable, {
    req(input$var_to_categorize, values$processed_data)
    
    tryCatch({
      if(!input$var_to_categorize %in% names(values$processed_data)) {
        showNotification(paste("Variabel", input$var_to_categorize, "tidak ditemukan"), type = "error")
        return()
      }
      
      var_data <- values$processed_data[[input$var_to_categorize]]
      category_var_name <- paste0(input$var_to_categorize, "_Category")
      
      if(input$categorization_method == "manual") {
        if(input$manual_categories == "2") {
          # 2 kategori manual - FIX untuk menghasilkan data numerik 0/1 untuk uji proporsi
          result <- categorize_variable_auto(var_data, "manual", 2, FALSE, input$binary_threshold)
          category_values <- result$categories
          
          # Convert to numeric 0/1 for proportion tests
          category_numeric <- rep(NA_real_, length(var_data))
          category_numeric[!is.na(var_data) & var_data < input$binary_threshold] <- 0
          category_numeric[!is.na(var_data) & var_data >= input$binary_threshold] <- 1
          
          # Store both versions
          values$processed_data[[category_var_name]] <- category_values  # For display
          values$processed_data[[paste0(category_var_name, "_Numeric")]] <- category_numeric  # For tests
          
          method_desc <- paste0("Manual 2 Kategori (threshold: ", input$binary_threshold, ") - Siap untuk uji proporsi")
          
        } else {
          # 3 kategori manual (existing logic)
          if(input$low_threshold >= input$high_threshold) {
            showNotification("Batas rendah harus lebih kecil dari batas tinggi", type = "error")
            return()
          }
          
          category_values <- rep(NA_character_, length(var_data))
          category_values[!is.na(var_data) & var_data < input$low_threshold] <- "Rendah"
          category_values[!is.na(var_data) & var_data >= input$low_threshold & var_data <= input$high_threshold] <- "Sedang"
          category_values[!is.na(var_data) & var_data > input$high_threshold] <- "Tinggi"
          
          values$processed_data[[category_var_name]] <- category_values
          
          method_desc <- paste0("Manual 3 Kategori (", input$low_threshold, ", ", input$high_threshold, ")")
        }
        
      } else {
        result <- categorize_variable_auto(var_data, input$categorization_method, 
                                           input$n_categories, input$auto_optimize)
        category_values <- result$categories
        
        values$processed_data[[category_var_name]] <- category_values
        
        method_desc <- paste0(toupper(result$method), 
                              " (", result$n_categories, " categories",
                              if(result$auto_optimized) ", auto-optimized" else "", ")")
      }
      
      # Update categorized data display
      output$categorized_data <- DT::renderDataTable({
        display_data <- values$processed_data %>%
          select(DISTRICTCODE, REGION, !!input$var_to_categorize, !!category_var_name) %>%
          arrange(!!sym(input$var_to_categorize))
        
        # Add numeric column to display if it's binary categorization
        if(input$categorization_method == "manual" && input$manual_categories == "2") {
          numeric_var_name <- paste0(category_var_name, "_Numeric")
          if(numeric_var_name %in% names(values$processed_data)) {
            display_data[[paste0(category_var_name, "_Numeric")]] <- values$processed_data[[numeric_var_name]]
          }
        }
        
        cat_summary <- table(values$processed_data[[category_var_name]], useNA = "ifany")
        
        # Create color palette that matches number of categories
        unique_categories <- names(cat_summary)[names(cat_summary) != "NA"]
        colors <- RColorBrewer::brewer.pal(max(3, length(unique_categories)), "Set3")[1:length(unique_categories)]
        
        DT::datatable(display_data, options = list(scrollX = TRUE, pageLength = 10),
                      caption = paste("Kategorisasi:", method_desc, "| Distribusi:", 
                                      paste(names(cat_summary), cat_summary, sep = ":", collapse = " | "))) %>%
          DT::formatStyle(category_var_name, backgroundColor = DT::styleEqual(
            unique_categories, colors
          ))
      })
      
      showNotification(paste("Kategorisasi berhasil!", method_desc, "| Variabel baru:", category_var_name), 
                       type = "message", duration = 5)
      
      # Special notification for binary categorization
      if(input$categorization_method == "manual" && input$manual_categories == "2") {
        showNotification("Variabel numerik 0/1 tersedia untuk uji proporsi!", type = "message", duration = 3)
      }
      
      cat("Categorization completed for", input$var_to_categorize, "using", method_desc, "\n")
      
    }, error = function(e) {
      showNotification(paste("Error dalam kategorisasi:", e$message), type = "error")
      cat("Error in categorization:", e$message, "\n")
    })
  })
  
  # Data transformation
  observeEvent(input$apply_transform, {
    req(input$transform_variable, input$transform_method, values$processed_data)
    
    tryCatch({
      if(!input$transform_variable %in% names(values$processed_data)) {
        showNotification(paste("Variabel", input$transform_variable, "tidak ditemukan"), type = "error")
        return()
      }
      
      original_data <- values$processed_data[[input$transform_variable]]
      valid_indices <- !is.na(original_data) & is.finite(original_data)
      original_clean <- original_data[valid_indices]
      
      if(length(original_clean) == 0) {
        showNotification("Tidak ada data valid untuk transformasi", type = "error")
        return()
      }
      
      # Perform transformation
      if(input$transform_method == "log") {
        if(any(original_clean <= 0)) {
          showNotification("Log transformation: menambahkan konstanta 1 untuk nilai ≤ 0", type = "warning")
          transformed_data <- log(abs(original_clean) + 1)
        } else {
          transformed_data <- log(original_clean)
        }
        new_var_name <- paste0(input$transform_variable, "_ln")
        
      } else if(input$transform_method == "log10") {
        if(any(original_clean <= 0)) {
          showNotification("Log10 transformation: menambahkan konstanta 1 untuk nilai ≤ 0", type = "warning")
          transformed_data <- log10(abs(original_clean) + 1)
        } else {
          transformed_data <- log10(original_clean)
        }
        new_var_name <- paste0(input$transform_variable, "_log10")
        
      } else if(input$transform_method == "sqrt") {
        if(any(original_clean < 0)) {
          showNotification("Square root: menggunakan nilai absolut untuk nilai negatif", type = "warning")
          transformed_data <- sqrt(abs(original_clean))
        } else {
          transformed_data <- sqrt(original_clean)
        }
        new_var_name <- paste0(input$transform_variable, "_sqrt")
        
      } else if(input$transform_method == "inverse") {
        if(any(original_clean == 0)) {
          showNotification("Inverse transformation: mengganti nilai 0 dengan 0.001", type = "warning")
          original_clean[original_clean == 0] <- 0.001
        }
        transformed_data <- 1 / original_clean
        new_var_name <- paste0(input$transform_variable, "_inv")
        
      } else if(input$transform_method == "boxcox") {
        if(any(original_clean <= 0)) {
          shift_val <- abs(min(original_clean)) + 1
          original_clean <- original_clean + shift_val
          showNotification(paste("Box-Cox: data di-shift sebesar", round(shift_val, 3)), type = "warning")
        }
        
        if(requireNamespace("forecast", quietly = TRUE)) {
          lambda <- forecast::BoxCox.lambda(original_clean)
          transformed_data <- forecast::BoxCox(original_clean, lambda)
        } else {
          transformed_data <- log(original_clean)
          showNotification("Box-Cox tidak tersedia, menggunakan log transformation", type = "warning")
        }
        new_var_name <- paste0(input$transform_variable, "_boxcox")
      }
      
      # PENTING: Update processed_data dengan variabel baru
      values$processed_data[[new_var_name]] <- rep(NA_real_, nrow(values$processed_data))
      values$processed_data[[new_var_name]][valid_indices] <- transformed_data
      
      # Create comparison data for plotting
      comparison_data <- data.frame(
        Index = which(valid_indices),
        Original = original_clean,
        Transformed = transformed_data
      )
      
      # Create comparison plots
      p1 <- ggplot(comparison_data, aes(x = Original)) +
        geom_histogram(bins = 25, fill = "#3b82f6", alpha = 0.7, color = "white") +
        theme_minimal() +
        labs(title = "Data Original", x = input$transform_variable, y = "Frekuensi") +
        theme(plot.title = element_text(size = 12))
      
      p2 <- ggplot(comparison_data, aes(x = Transformed)) +
        geom_histogram(bins = 25, fill = "#ef4444", alpha = 0.7, color = "white") +
        theme_minimal() +
        labs(title = paste("Data Transformasi (", input$transform_method, ")", sep = ""), 
             x = new_var_name, y = "Frekuensi") +
        theme(plot.title = element_text(size = 12))
      
      output$transformation_plot <- renderPlotly({
        subplot(ggplotly(p1), ggplotly(p2), nrows = 1, shareY = FALSE) %>%
          layout(title = list(text = paste("Perbandingan Transformasi:", input$transform_method), 
                              font = list(size = 14)))
      })
      
      # Update transformed data display
      output$transformed_data_new <- DT::renderDataTable({
        display_data <- values$processed_data %>%
          select(DISTRICTCODE, REGION, !!input$transform_variable, !!new_var_name) %>%
          filter(!is.na(!!sym(new_var_name)))
        
        DT::datatable(display_data, options = list(scrollX = TRUE, pageLength = 10),
                      caption = paste("Data Hasil Transformasi:", input$transform_method, 
                                      "| Valid observations:", nrow(display_data), 
                                      "| New variable:", new_var_name))
      })
      
      showNotification(paste("Transformasi", input$transform_method, "berhasil! Variabel baru:", new_var_name, "- Tersedia di semua menu"), 
                       type = "message", duration = 5)
      cat("✓ Transformation", input$transform_method, "completed successfully. New variable:", new_var_name, "\n")
      cat("✓ Variable added to processed_data. Total columns:", ncol(values$processed_data), "\n")
      
      # TRIGGER UPDATE: Force reactive update dengan slight delay
      shinyjs::delay(100, {
        cat("✓ Choices will be updated in all tabs\n")
      })
      
    }, error = function(e) {
      showNotification(paste("Error dalam transformasi:", e$message), type = "error")
      cat("Error in transformation:", e$message, "\n")
    })
  })
  
  # ===================================================================
  # EKSPLORASI DATA SECTION (keeping original implementation)
  # ===================================================================
  
  # Descriptive statistics using processed_data
  observeEvent(input$run_descriptive, {
    req(input$desc_variables)
    
    tryCatch({
      if(input$desc_level == "provinsi") {
        if(!is.null(geojson_data) && "nmprov" %in% names(geojson_data)) {
          data_subset <- geojson_data %>% st_drop_geometry()
          
          if(input$desc_province != "all") {
            data_subset <- data_subset %>% filter(nmprov == input$desc_province)
            region_label <- input$desc_province
          } else {
            region_label <- "Semua Provinsi"
          }
          
          if(region_label == "Semua Provinsi") {
            data_subset <- data_subset %>%
              group_by(nmprov) %>%
              summarise(across(all_of(input$desc_variables), ~mean(.x, na.rm = TRUE)), .groups = 'drop')
          }
          
        } else {
          data_subset <- values$processed_data
          if(input$desc_province != "all") {
            data_subset <- data_subset %>% filter(REGION == input$desc_province)
            region_label <- input$desc_province
          } else {
            region_label <- "Semua Region"
          }
        }
        
      } else if(input$desc_level == "region") {
        data_subset <- values$processed_data
        
        if(input$desc_region != "all") {
          data_subset <- data_subset %>% filter(REGION == input$desc_region)
          region_label <- input$desc_region
        } else {
          region_label <- "Semua Region"
        }
        
        if(region_label == "Semua Region") {
          data_subset <- data_subset %>%
            group_by(REGION) %>%
            summarise(across(all_of(input$desc_variables), ~mean(.x, na.rm = TRUE)), .groups = 'drop')
        }
      }
      
      if(nrow(data_subset) == 0) {
        showNotification("Tidak ada data untuk kriteria yang dipilih", type = "warning")
        return()
      }
      
      if(length(input$desc_variables) == 1) {
        var_name <- input$desc_variables[1]
        var_data <- data_subset[[var_name]]
        var_data <- var_data[!is.na(var_data) & is.finite(var_data)]
        
        if(length(var_data) > 0) {
          desc_stats <- data.frame(
            Variable = var_name,
            N = length(var_data),
            Mean = round(mean(var_data), 3),
            Median = round(median(var_data), 3),
            SD = round(sd(var_data), 3),
            Min = round(min(var_data), 3),
            Max = round(max(var_data), 3),
            Q25 = round(quantile(var_data, 0.25), 3),
            Q75 = round(quantile(var_data, 0.75), 3)
          )
        } else {
          desc_stats <- data.frame(
            Variable = var_name,
            N = 0, Mean = NA, Median = NA, SD = NA, Min = NA, Max = NA, Q25 = NA, Q75 = NA
          )
        }
      } else {
        desc_stats <- data_subset %>%
          select(all_of(input$desc_variables)) %>%
          summarise(across(everything(), list(
            N = ~sum(!is.na(.x) & is.finite(.x)),
            Mean = ~round(mean(.x, na.rm = TRUE), 3),
            Median = ~round(median(.x, na.rm = TRUE), 3),
            SD = ~round(sd(.x, na.rm = TRUE), 3),
            Min = ~round(min(.x, na.rm = TRUE), 3),
            Max = ~round(max(.x, na.rm = TRUE), 3),
            Q25 = ~round(quantile(.x, 0.25, na.rm = TRUE), 3),
            Q75 = ~round(quantile(.x, 0.75, na.rm = TRUE), 3)
          ), .names = "{.col}_{.fn}")) %>%
          pivot_longer(everything(), names_to = "Variable_Stat", values_to = "Value") %>%
          separate(Variable_Stat, into = c("Variable", "Statistic"), sep = "_") %>%
          pivot_wider(names_from = Statistic, values_from = Value) %>%
          select(Variable, N, Mean, Median, SD, Min, Max, Q25, Q75)
      }
      
      values$current_descriptive <- desc_stats
      values$current_descriptive_data <- data_subset
      
      output$descriptive_stats <- DT::renderDataTable({
        DT::datatable(desc_stats, 
                      options = list(scrollX = TRUE, pageLength = 15),
                      caption = paste0("Statistik Deskriptif - ", region_label, 
                                       " (", input$desc_level, ", ", nrow(data_subset), " observasi)")) %>%
          DT::formatRound(columns = c("Mean", "Median", "SD", "Min", "Max", "Q25", "Q75"), digits = 3)
      })
      
      output$descriptive_interpretation <- renderText({
        interpret_descriptive_real(desc_stats, data_subset, input$desc_variables, region_label)
      })
      
    }, error = function(e) {
      showNotification(paste("Error dalam analisis deskriptif:", e$message), type = "error")
      cat("Error in descriptive analysis:", e$message, "\n")
    })
  })
  
  # Enhanced visualization using processed_data
  observeEvent(input$create_viz, {
    req(input$viz_type, input$viz_variables)
    
    tryCatch({
      if(!is.null(geojson_data) && all(input$viz_variables %in% names(geojson_data))) {
        plot_data_source <- st_drop_geometry(geojson_data)
      } else {
        plot_data_source <- values$processed_data
      }
      
      cor_data_for_interpretation <- NULL
      plot_data_for_interpretation <- NULL
      
      if(input$viz_type == "Histogram") {
        plot_data <- plot_data_source %>%
          select(all_of(input$viz_variables)) %>%
          pivot_longer(everything(), names_to = "Variable", values_to = "Value") %>%
          filter(!is.na(Value))
        
        plot_data_for_interpretation <- plot_data
        
        p <- ggplot(plot_data, aes(x = Value)) +
          geom_histogram(bins = 20, fill = "#dc2626", alpha = 0.7, color = "white") +
          facet_wrap(~Variable, scales = "free") +
          theme_minimal() +
          labs(title = "Distribusi Frekuensi Variabel Terpilih", x = "Nilai", y = "Frekuensi")
        
      } else if(input$viz_type == "Boxplot") {
        # Tentukan level berdasarkan input user (default: provinsi jika tidak ada input)
        boxplot_level <- ifelse(is.null(input$boxplot_level), "provinsi", input$boxplot_level)
        
        if(boxplot_level == "provinsi" && !is.null(geojson_data) && "nmprov" %in% names(geojson_data)) {
          # Boxplot per provinsi menggunakan data geojson
          plot_data <- geojson_data %>% 
            st_drop_geometry() %>%
            select(nmprov, all_of(input$viz_variables)) %>%
            pivot_longer(-nmprov, names_to = "Variable", values_to = "Value") %>%
            filter(!is.na(Value))
          
          p <- ggplot(plot_data, aes(x = nmprov, y = Value)) +
            geom_boxplot(alpha = 0.7, fill = "#dc2626") +
            facet_wrap(~Variable, scales = "free") +
            theme_minimal() +
            theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
            labs(title = "Perbandingan Antar Provinsi", x = "Provinsi", y = "Nilai")
          
        } else {
          # Boxplot per region menggunakan processed data
          current_data <- if(is.null(values$processed_data)) sovi_data else values$processed_data
          
          plot_data <- current_data %>%
            select(REGION, all_of(input$viz_variables)) %>%
            pivot_longer(-REGION, names_to = "Variable", values_to = "Value") %>%
            filter(!is.na(Value))
          
          p <- ggplot(plot_data, aes(x = REGION, y = Value)) +
            geom_boxplot(alpha = 0.7, fill = "#dc2626") +
            facet_wrap(~Variable, scales = "free") +
            theme_minimal() +
            theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
            labs(title = "Perbandingan Antar Region", x = "Region", y = "Nilai")
        }
        
        plot_data_for_interpretation <- plot_data
        
      } else if(input$viz_type == "Scatter Plot") {
        req(input$scatter_x, input$scatter_y)
        
        plot_data <- plot_data_source %>%
          select(!!input$scatter_x, !!input$scatter_y) %>%
          filter(!is.na(!!sym(input$scatter_x)), !is.na(!!sym(input$scatter_y)))
        
        p <- ggplot(plot_data, aes_string(x = input$scatter_x, y = input$scatter_y)) +
          geom_point(alpha = 0.7, size = 2, color = "#dc2626") +
          geom_smooth(method = "lm", se = TRUE, color = "#991b1b") +
          theme_minimal() +
          labs(title = paste("Hubungan:", input$scatter_x, "vs", input$scatter_y))
        
        plot_data_for_interpretation <- plot_data
        
      } else if(input$viz_type == "Correlation Heatmap") {
        cor_data <- plot_data_source %>%
          select(all_of(input$viz_variables)) %>%
          cor(use = "complete.obs")
        
        cor_data_for_interpretation <- cor_data
        
        cor_long <- expand.grid(Var1 = rownames(cor_data), Var2 = colnames(cor_data))
        cor_long$value <- as.vector(cor_data)
        
        p <- ggplot(cor_long, aes(x = Var1, y = Var2, fill = value)) +
          geom_tile() +
          scale_fill_gradient2(low = "#fca5a5", mid = "white", high = "#dc2626", 
                               midpoint = 0, limit = c(-1, 1)) +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
          labs(title = "Matriks Korelasi", fill = "Korelasi")
        
      } else if(input$viz_type == "Density Plot") {
        plot_data <- plot_data_source %>%
          select(all_of(input$viz_variables)) %>%
          pivot_longer(everything(), names_to = "Variable", values_to = "Value") %>%
          filter(!is.na(Value))
        
        plot_data_for_interpretation <- plot_data
        
        p <- ggplot(plot_data, aes(x = Value, fill = Variable)) +
          geom_density(alpha = 0.7, color = "white") +
          scale_fill_manual(values = rep("#dc2626", length(input$viz_variables))) +
          facet_wrap(~Variable, scales = "free") +
          theme_minimal() +
          labs(title = "Distribusi Kepadatan Data")
        
      } else if(input$viz_type == "Violin Plot") {
        if("nmprov" %in% names(plot_data_source)) {
          plot_data <- plot_data_source %>%
            select(nmprov, all_of(input$viz_variables)) %>%
            pivot_longer(-nmprov, names_to = "Variable", values_to = "Value") %>%
            filter(!is.na(Value)) %>%
            slice_sample(n = min(1000, nrow(.)))
        } else {
          plot_data <- plot_data_source %>%
            select(REGION, all_of(input$viz_variables)) %>%
            pivot_longer(-REGION, names_to = "Variable", values_to = "Value") %>%
            filter(!is.na(Value))
        }
        
        plot_data_for_interpretation <- plot_data
        
        p <- ggplot(plot_data, aes(x = Variable, y = Value)) +
          geom_violin(fill = "#dc2626", alpha = 0.7) +
          geom_boxplot(width = 0.1, fill = "white", alpha = 0.8) +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
          labs(title = "Violin Plot - Distribusi dan Kuartil")
        
      } else if(input$viz_type == "Bar Chart") {
        # Untuk Bar Chart, gunakan level yang sama dengan boxplot jika ada
        bar_level <- ifelse(is.null(input$boxplot_level), "region", input$boxplot_level)
        
        if(bar_level == "provinsi" && !is.null(geojson_data) && "nmprov" %in% names(geojson_data)) {
          plot_data <- geojson_data %>% 
            st_drop_geometry() %>%
            select(nmprov, all_of(input$viz_variables)) %>%
            pivot_longer(-nmprov, names_to = "Variable", values_to = "Value") %>%
            filter(!is.na(Value)) %>%
            group_by(nmprov, Variable) %>%
            summarise(Mean_Value = mean(Value, na.rm = TRUE), .groups = 'drop') %>%
            group_by(Variable) %>%
            slice_max(order_by = Mean_Value, n = 10) %>%
            ungroup()
          
          p <- ggplot(plot_data, aes(x = reorder(nmprov, Mean_Value), y = Mean_Value)) +
            geom_col(fill = "#dc2626", alpha = 0.8) +
            facet_wrap(~Variable, scales = "free") +
            coord_flip() +
            theme_minimal() +
            labs(title = "Rata-rata Nilai per Provinsi", x = "Provinsi", y = "Rata-rata")
          
        } else {
          # Bar chart per region
          current_data <- if(is.null(values$processed_data)) sovi_data else values$processed_data
          
          plot_data <- current_data %>%
            select(REGION, all_of(input$viz_variables)) %>%
            pivot_longer(-REGION, names_to = "Variable", values_to = "Value") %>%
            filter(!is.na(Value)) %>%
            group_by(REGION, Variable) %>%
            summarise(Mean_Value = mean(Value, na.rm = TRUE), .groups = 'drop')
          
          p <- ggplot(plot_data, aes(x = reorder(REGION, Mean_Value), y = Mean_Value)) +
            geom_col(fill = "#dc2626", alpha = 0.8) +
            facet_wrap(~Variable, scales = "free") +
            coord_flip() +
            theme_minimal() +
            labs(title = "Rata-rata Nilai per Region", x = "Region", y = "Rata-rata")
        }
        
        plot_data_for_interpretation <- plot_data
      }
      
      values$current_viz_plot <- p
      values$current_viz_data <- plot_data_for_interpretation
      
      output$visualization_plot <- renderPlotly({
        ggplotly(p)
      })
      
      viz_interpretation <- interpret_visualization_real(input$viz_type, input$viz_variables, 
                                                         plot_data_for_interpretation, cor_data_for_interpretation)
      values$current_viz_interpretation <- viz_interpretation
      
      output$visualization_interpretation <- renderText({
        viz_interpretation
      })
      
    }, error = function(e) {
      showNotification(paste("Error in visualization:", e$message), type = "error")
    })
  })
  
  # Spatial map with real interpretation and geometry fix (continuation)
  observeEvent(input$update_map, {
    req(input$map_variable, input$map_level)
    
    output$spatial_map <- renderLeaflet({
      tryCatch({
        if(is.null(geojson_data)) {
          return(leaflet() %>%
                   addTiles() %>%
                   setView(lng = 118, lat = -2, zoom = 5) %>%
                   addMarkers(lng = 118, lat = -2, popup = "GeoJSON data tidak dapat dimuat"))
        }
        
        if(!input$map_variable %in% names(geojson_data)) {
          return(leaflet() %>%
                   addTiles() %>%
                   setView(lng = 118, lat = -2, zoom = 5) %>%
                   addMarkers(lng = 118, lat = -2, 
                              popup = paste("Variabel", input$map_variable, "tidak ditemukan")))
        }
        
        # Handle geometry issues
        map_data <- geojson_data
        
        # Ensure geometries are valid
        if(any(!st_is_valid(map_data))) {
          map_data <- st_make_valid(map_data)
        }
        
        if(input$map_level == "provinsi") {
          # Aggregate by province
          map_data <- map_data %>%
            group_by(nmprov) %>%
            summarise(
              Value = mean(!!sym(input$map_variable), na.rm = TRUE),
              .groups = 'drop'
            ) %>%
            filter(!is.na(Value), is.finite(Value))
          
          values$current_map_data <- map_data
          
          if(nrow(map_data) > 0) {
            map_values <- map_data$Value[is.finite(map_data$Value)]
            
            if(length(map_values) > 0) {
              pal <- colorNumeric(palette = "Reds", domain = map_values, na.color = "lightgrey")
              
              map_widget <- leaflet(map_data) %>%
                addTiles() %>%
                setView(lng = 118, lat = -2, zoom = 5) %>%
                addPolygons(
                  fillColor = ~pal(Value),
                  weight = 2, opacity = 1, color = "white", dashArray = "3", fillOpacity = 0.7,
                  popup = ~paste0("<strong>Provinsi:</strong> ", nmprov, "<br/>",
                                  "<strong>", input$map_variable, " (Rata-rata):</strong> ", round(Value, 2)),
                  label = ~nmprov
                ) %>%
                addLegend(pal = pal, values = ~Value, opacity = 0.7, 
                          title = paste("Rata-rata", input$map_variable), position = "bottomright")
              
              values$current_map_widget <- map_widget
              return(map_widget)
            }
          }
        } else {
          # Kabupaten level
          map_values <- as.numeric(map_data[[input$map_variable]])
          map_values_clean <- map_values[!is.na(map_values) & is.finite(map_values)]
          
          values$current_map_data <- map_data
          
          if(length(map_values_clean) > 0) {
            pal <- colorNumeric(palette = "Reds", domain = map_values_clean, na.color = "lightgrey")
            
            map_widget <- leaflet(map_data) %>%
              addTiles() %>%
              setView(lng = 118, lat = -2, zoom = 5) %>%
              addPolygons(
                fillColor = ~pal(as.numeric(get(input$map_variable))),
                weight = 1, opacity = 1, color = "white", dashArray = "3", fillOpacity = 0.7,
                popup = ~paste0("<strong>Kabupaten/Kota:</strong> ", nmkab, "<br/>",
                                "<strong>Provinsi:</strong> ", nmprov, "<br/>",
                                "<strong>", input$map_variable, ":</strong> ", 
                                round(as.numeric(get(input$map_variable)), 2)),
                label = ~nmkab
              ) %>%
              addLegend(pal = pal, values = map_values_clean, opacity = 0.7, 
                        title = input$map_variable, position = "bottomright")
            
            values$current_map_widget <- map_widget
            return(map_widget)
          }
        }
      }, error = function(e) {
        return(leaflet() %>%
                 addTiles() %>%
                 setView(lng = 118, lat = -2, zoom = 5) %>%
                 addMarkers(lng = 118, lat = -2, popup = paste("Error:", e$message)))
      })
    })
    
    # Real map interpretation
    output$map_interpretation <- renderText({
      if(!is.null(values$current_map_data)) {
        interpret_map_real(input$map_variable, values$current_map_data, input$map_level)
      } else {
        "Peta belum dibuat atau terjadi kesalahan dalam pembuatan peta."
      }
    })
  })
  
  # ===================================================================
  # UJI ASUMSI SECTION
  # ===================================================================
  
  # Load data untuk uji asumsi
  observeEvent(input$load_asumsi_data, {
    tryCatch({
      if(input$asumsi_data_source == "processed" && !is.null(values$processed_data)) {
        values$asumsi_data <- values$processed_data
        source_text <- "Data dari Manajemen Data"
      } else if(input$asumsi_data_source == "upload" && !is.null(input$asumsi_data_file)) {
        values$asumsi_data <- load_data_from_file(input$asumsi_data_file$datapath)
        source_text <- paste("File upload:", input$asumsi_data_file$name)
      } else {
        showNotification("Pilih sumber data terlebih dahulu", type = "warning")
        return()
      }
      
      if(!is.null(values$asumsi_data)) {
        # Update variable choices
        numeric_cols <- names(values$asumsi_data)[sapply(values$asumsi_data, is.numeric)]
        updateSelectizeInput(session, "norm_variables", choices = numeric_cols, 
                             selected = head(numeric_cols, 2))
        updateSelectInput(session, "homog_variable", choices = numeric_cols, 
                          selected = numeric_cols[1])
        
        # Update grouping variables for homogeneity
        all_cols <- names(values$asumsi_data)
        char_factor_cols <- all_cols[sapply(values$asumsi_data, function(x) is.character(x) || is.factor(x))]
        updateSelectInput(session, "homog_custom_var", choices = char_factor_cols,
                          selected = if(length(char_factor_cols) > 0) char_factor_cols[1] else NULL)
        
        # PENTING: Trigger update provinsi dan kabupaten setelah data dimuat
        shinyjs::delay(200, {
          # Trigger update dengan nilai dummy
          updateSelectInput(session, "homog_filter_province", selected = "all")
        })
        
        showNotification(paste("Data berhasil dimuat:", source_text), type = "message")
      }
    }, error = function(e) {
      showNotification(paste("Error loading data:", e$message), type = "error")
    })
  })
  
  observe({
    if(!is.null(kabupaten_data)) {
      province_list <- unique(kabupaten_data$NMPROV)
      province_list <- province_list[!is.na(province_list)]
      province_list <- sort(province_list)
      
      updateSelectInput(session, "homog_filter_province", 
                        choices = c("Semua Provinsi" = "all", setNames(province_list, province_list)))
      
      updateSelectizeInput(session, "homog_provinces", 
                           choices = setNames(province_list, province_list),
                           selected = head(province_list, 3))
    }
  })
  
  # Data info untuk uji asumsi
  output$asumsi_data_info <- renderText({
    if(input$asumsi_data_source == "processed" && !is.null(values$processed_data)) {
      paste0("Data Manajemen: ", nrow(values$processed_data), " obs, ", 
             ncol(values$processed_data), " vars")
    } else if(input$asumsi_data_source == "upload" && !is.null(input$asumsi_data_file)) {
      paste0("File: ", input$asumsi_data_file$name, "\n",
             "Size: ", round(input$asumsi_data_file$size/1024, 2), " KB")
    } else {
      "Belum ada data yang dipilih"
    }
  })
  
  # Preview data untuk uji asumsi
  output$asumsi_data_preview <- DT::renderDataTable({
    if(!is.null(values$asumsi_data)) {
      DT::datatable(values$asumsi_data, options = list(scrollX = TRUE, pageLength = 10),
                    caption = paste("Preview Data - Total:", nrow(values$asumsi_data), "observasi"))
    } else {
      DT::datatable(data.frame(Message = "Load data terlebih dahulu"))
    }
  })
  
  # Normality tests
  observeEvent(input$run_normality, {
    req(input$norm_variables, input$norm_tests, values$asumsi_data)
    
    tryCatch({
      current_data <- values$asumsi_data
      
      # Check if variables exist
      available_vars <- intersect(input$norm_variables, names(current_data))
      if(length(available_vars) == 0) {
        showNotification("Variabel yang dipilih tidak tersedia dalam data", type = "warning")
        return()
      }
      
      # Perform normality tests
      normality_results <- data.frame()
      
      for(var in available_vars) {
        var_data <- current_data[[var]]
        
        # Clean data - remove NA, infinite, and non-numeric values
        var_data_clean <- var_data[!is.na(var_data) & is.finite(var_data)]
        
        # Ensure data is numeric
        if(!is.numeric(var_data_clean)) {
          next
        }
        
        # Check if we have enough data
        if(length(var_data_clean) < 3) {
          next  # Skip if insufficient data
        }
        
        # Check for constant values (no variation)
        if(length(unique(var_data_clean)) <= 1) {
          # Add result for constant data
          for(test in input$norm_tests) {
            normality_results <- rbind(normality_results, data.frame(
              Variable = var,
              Test = paste(test, "(constant data)"),
              Statistic = NA,
              P_Value = NA,
              Normal = NA,
              N = length(var_data_clean),
              Note = "Data konstant - tidak dapat diuji",
              stringsAsFactors = FALSE
            ))
          }
          next
        }
        
        for(test in input$norm_tests) {
          tryCatch({
            if(test == "Shapiro-Wilk") {
              # Shapiro-Wilk test limitation
              if(length(var_data_clean) > 5000) {
                # Use random sample for large datasets
                set.seed(123)
                sample_data <- sample(var_data_clean, 5000)
                test_result <- shapiro.test(sample_data)
                test_name <- "Shapiro-Wilk (sample n=5000)"
              } else if(length(var_data_clean) < 3) {
                next  # Skip if too few observations
              } else {
                test_result <- shapiro.test(var_data_clean)
                test_name <- "Shapiro-Wilk"
              }
              statistic <- as.numeric(test_result$statistic)
              p_value <- as.numeric(test_result$p.value)
              
            } else if(test == "Kolmogorov-Smirnov") {
              # K-S test requires at least 1 observation
              if(length(var_data_clean) < 1) {
                next
              }
              
              # Calculate parameters for normal distribution
              mean_val <- mean(var_data_clean)
              sd_val <- sd(var_data_clean)
              
              # Check if standard deviation is valid
              if(is.na(sd_val) || sd_val == 0) {
                next
              }
              
              test_result <- ks.test(var_data_clean, "pnorm", mean = mean_val, sd = sd_val)
              test_name <- "Kolmogorov-Smirnov"
              statistic <- as.numeric(test_result$statistic)
              p_value <- as.numeric(test_result$p.value)
              
            } else if(test == "Anderson-Darling") {
              # Anderson-Darling test
              if(length(var_data_clean) < 8) {
                next  # AD test needs at least 8 observations
              }
              
              # Check if nortest package is available
              if(!requireNamespace("nortest", quietly = TRUE)) {
                next
              }
              
              test_result <- nortest::ad.test(var_data_clean)
              test_name <- "Anderson-Darling"
              statistic <- as.numeric(test_result$statistic)
              p_value <- as.numeric(test_result$p.value)
            }
            
            # Add result to dataframe
            normality_results <- rbind(normality_results, data.frame(
              Variable = var,
              Test = test_name,
              Statistic = round(statistic, 4),
              P_Value = round(p_value, 4),
              Normal = p_value > 0.05,
              N = length(var_data_clean),
              Note = "OK",
              stringsAsFactors = FALSE
            ))
            
          }, error = function(e) {
            cat("Error in", test, "for", var, ":", e$message, "\n")
            
            # Add error result
            normality_results <<- rbind(normality_results, data.frame(
              Variable = var,
              Test = paste(test, "(error)"),
              Statistic = NA,
              P_Value = NA,
              Normal = NA,
              N = length(var_data_clean),
              Note = paste("Error:", substr(e$message, 1, 50)),
              stringsAsFactors = FALSE
            ))
          })
        }
      }
      
      values$normality_results <- normality_results
      
      # Display results table
      output$normality_results <- DT::renderDataTable({
        if(nrow(normality_results) > 0) {
          DT::datatable(normality_results, options = list(scrollX = TRUE, pageLength = 15),
                        caption = "Hasil Uji Normalitas") %>%
            DT::formatStyle("Normal", backgroundColor = DT::styleEqual(
              c(TRUE, FALSE), c("lightgreen", "lightcoral")
            ))
        } else {
          DT::datatable(data.frame(Message = "Tidak ada hasil uji yang valid"))
        }
      })
      
      # Create Q-Q plots only for variables with valid data
      valid_vars <- normality_results$Variable[normality_results$Note == "OK"]
      valid_vars <- unique(valid_vars)
      
      if(length(valid_vars) > 0) {
        qq_plots <- list()
        
        for(var in valid_vars) {
          var_data <- current_data[[var]]
          var_data_clean <- var_data[!is.na(var_data) & is.finite(var_data)]
          
          if(length(var_data_clean) >= 3 && length(unique(var_data_clean)) > 1) {
            # Sample data if too large for visualization
            if(length(var_data_clean) > 1000) {
              set.seed(123)
              var_data_clean <- sample(var_data_clean, 1000)
            }
            
            # Create Q-Q plot data manually for more control
            n <- length(var_data_clean)
            sorted_data <- sort(var_data_clean)
            theoretical_quantiles <- qnorm(ppoints(n))
            
            qq_data <- data.frame(
              theoretical = theoretical_quantiles,
              sample = sorted_data
            )
            
            p <- ggplot(qq_data, aes(x = theoretical, y = sample)) +
              geom_point(alpha = 0.6, color = "#dc2626") +
              geom_smooth(method = "lm", se = FALSE, color = "#991b1b", size = 1) +
              theme_minimal() +
              labs(title = paste("Q-Q Plot:", var), x = "Theoretical Quantiles", y = "Sample Quantiles")
            
            qq_plots[[var]] <- p
          }
        }
        
        if(length(qq_plots) > 0) {
          values$normality_plots <- qq_plots
          
          output$qq_plots <- renderPlotly({
            if(length(qq_plots) == 1) {
              ggplotly(qq_plots[[1]])
            } else {
              # For multiple plots, create subplot - limit to 4 plots
              plotly_plots <- lapply(qq_plots[1:min(4, length(qq_plots))], ggplotly)
              subplot(plotly_plots, nrows = ceiling(length(plotly_plots)/2))
            }
          })
        }
      }
      
      # Generate interpretation
      if(nrow(normality_results) > 0) {
        interpretation <- interpret_normality(normality_results, available_vars)
        values$normality_interpretation <- interpretation
        
        output$normality_interpretation <- renderText({
          interpretation
        })
      }
      
      showNotification("Uji normalitas selesai", type = "message")
      
    }, error = function(e) {
      showNotification(paste("Error dalam uji normalitas:", e$message), type = "error")
      cat("Error in normality tests:", e$message, "\n")
    })
  })
  
  # Homogeneity tests
  observeEvent(input$run_homogeneity, {
    req(input$homog_variable, input$homog_group_type, values$asumsi_data)
    
    tryCatch({
      current_data <- values$asumsi_data
      
      if(!input$homog_variable %in% names(current_data)) {
        showNotification("Variabel tidak ditemukan dalam data", type = "error")
        return()
      }
      
      # Merge dengan data kabupaten jika diperlukan
      if(input$homog_group_type == "provinsi" && !is.null(kabupaten_data)) {
        current_data <- current_data %>%
          left_join(kabupaten_data, by = "DISTRICTCODE")
        
        if(!"NMPROV" %in% names(current_data)) {
          showNotification("Gagal menggabungkan data kabupaten", type = "error")
          return()
        }
      }
      
      # Prepare grouping variable based on selection
      if(input$homog_group_type == "region") {
        if("REGION" %in% names(current_data)) {
          group_var <- "REGION"
          test_data <- current_data %>%
            select(!!input$homog_variable, REGION) %>%
            filter(!is.na(!!sym(input$homog_variable)), !is.na(REGION))
          group_label <- "Region"
        } else {
          showNotification("Variabel REGION tidak ditemukan dalam data", type = "error")
          return()
        }
        
      } else if(input$homog_group_type == "provinsi") {
        if(is.null(input$homog_provinces) || length(input$homog_provinces) < 2) {
          showNotification("Pilih minimal 2 provinsi untuk perbandingan", type = "warning")
          return()
        }
        
        if("NMPROV" %in% names(current_data)) {
          group_var <- "NMPROV"
          test_data <- current_data %>%
            filter(NMPROV %in% input$homog_provinces) %>%
            select(!!input$homog_variable, NMPROV) %>%
            filter(!is.na(!!sym(input$homog_variable)), !is.na(NMPROV))
          group_label <- paste("Provinsi (", paste(input$homog_provinces, collapse = ", "), ")")
        } else {
          showNotification("Data provinsi tidak tersedia. Pastikan file kabupatenkota.csv tersedia", type = "error")
          return()
        }
      } else if(input$homog_group_type == "custom") {
        if(is.null(input$homog_custom_var)) {
          showNotification("Pilih variabel pengelompokan", type = "warning")
          return()
        }
        
        group_var <- input$homog_custom_var
        test_data <- current_data %>%
          select(!!input$homog_variable, !!group_var) %>%
          filter(!is.na(!!sym(input$homog_variable)), !is.na(!!sym(group_var)))
        group_label <- paste("Custom (", input$homog_custom_var, ")")
      }
      
      if(nrow(test_data) < 10) {
        showNotification("Data tidak cukup untuk uji homogenitas (minimal 10 observasi)", type = "warning")
        return()
      }
      
      # Check number of groups
      n_groups <- length(unique(test_data[[group_var]]))
      if(n_groups < 2) {
        showNotification("Diperlukan minimal 2 kelompok untuk uji homogenitas", type = "warning")
        return()
      }
      
      # Levene's test
      formula_levene <- as.formula(paste(input$homog_variable, "~", "factor(", group_var, ")"))
      levene_result <- car::leveneTest(formula_levene, data = test_data)
      
      # Bartlett's test  
      formula_bartlett <- as.formula(paste(input$homog_variable, "~", "factor(", group_var, ")"))
      bartlett_result <- bartlett.test(formula_bartlett, data = test_data)
      
      values$homogeneity_results <- list(levene = levene_result, bartlett = bartlett_result)
      
      # Display results
      output$homogeneity_results <- renderText({
        paste0(
          "UJI HOMOGENITAS VARIANS\n",
          "=======================\n",
          "Variabel: ", input$homog_variable, "\n",
          "Pengelompokan: ", group_label, "\n",
          "Jumlah kelompok: ", n_groups, "\n",
          "Total observasi: ", nrow(test_data), "\n\n",
          
          "LEVENE'S TEST\n",
          "=============\n",
          "F-statistic: ", round(levene_result$`F value`[1], 4), "\n",
          "df1: ", levene_result$Df[1], ", df2: ", levene_result$Df[2], "\n",
          "p-value: ", round(levene_result$`Pr(>F)`[1], 4), "\n",
          "Kesimpulan: ", ifelse(levene_result$`Pr(>F)`[1] > 0.05, "Varians homogen ✓", "Varians tidak homogen ✗"), "\n\n",
          
          "BARTLETT'S TEST\n",
          "===============\n",
          "Bartlett's K-squared: ", round(bartlett_result$statistic, 4), "\n",
          "df: ", bartlett_result$parameter, "\n",
          "p-value: ", round(bartlett_result$p.value, 4), "\n",
          "Kesimpulan: ", ifelse(bartlett_result$p.value > 0.05, "Varians homogen ✓", "Varians tidak homogen ✗"), "\n\n",
          
          "REKOMENDASI ANALISIS:\n",
          ifelse(levene_result$`Pr(>F)`[1] > 0.05 && bartlett_result$p.value > 0.05,
                 "✓ Asumsi homogenitas terpenuhi - gunakan ANOVA klasik",
                 "⚠ Asumsi homogenitas tidak terpenuhi - gunakan Welch ANOVA atau uji non-parametrik")
        )
      })
      
      # Create box plot
      test_data[[group_var]] <- as.factor(test_data[[group_var]])
      
      p <- ggplot(test_data, aes_string(x = paste0("factor(", group_var, ")"), y = input$homog_variable)) +
        geom_boxplot(fill = "#dc2626", alpha = 0.7, outlier.color = "#991b1b") +
        geom_point(position = position_jitter(width = 0.2), alpha = 0.3, color = "#059669") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        labs(title = paste("Homogeneity Test:", input$homog_variable, "by", group_label),
             x = group_label, y = input$homog_variable)
      
      values$homogeneity_plots <- p
      
      output$homogeneity_plot <- renderPlotly({
        ggplotly(p)
      })
      
      # Generate interpretation
      interpretation <- interpret_homogeneity(levene_result, bartlett_result, input$homog_variable, group_label)
      values$homogeneity_interpretation <- interpretation
      
      output$homogeneity_interpretation <- renderText({
        interpretation
      })
      
      showNotification(paste("Uji homogenitas selesai -", group_label), type = "message")
      
    }, error = function(e) {
      showNotification(paste("Error dalam uji homogenitas:", e$message), type = "error")
      cat("Error in homogeneity tests:", e$message, "\n")
    })
  })
  
  # ===================================================================
  # STATISTIK INFERENSIA SECTION
  # ===================================================================
  
  # Load data untuk inferensia
  observeEvent(input$load_inferensia_data, {
    tryCatch({
      if(input$inferensia_data_source == "processed" && !is.null(values$processed_data)) {
        values$inferensia_data <- values$processed_data
        source_text <- "Data dari Manajemen Data"
      } else if(input$inferensia_data_source == "upload" && !is.null(input$inferensia_data_file)) {
        values$inferensia_data <- load_data_from_file(input$inferensia_data_file$datapath)
        source_text <- paste("File upload:", input$inferensia_data_file$name)
      } else {
        showNotification("Pilih sumber data terlebih dahulu", type = "warning")
        return()
      }
      
      if(!is.null(values$inferensia_data)) {
        # Update variable choices
        numeric_cols <- names(values$inferensia_data)[sapply(values$inferensia_data, is.numeric)]
        char_factor_cols <- names(values$inferensia_data)[sapply(values$inferensia_data, function(x) is.character(x) || is.factor(x) || (is.numeric(x) && length(unique(x)) <= 10))]
        
        # PENTING: Include all columns for proportion tests (including 0/1 numeric variables)
        all_cols <- names(values$inferensia_data)
        
        # Identify binary numeric variables (for proportion tests)
        binary_numeric_cols <- numeric_cols[sapply(values$inferensia_data[numeric_cols], function(x) {
          unique_vals <- unique(x[!is.na(x)])
          length(unique_vals) == 2 && all(unique_vals %in% c(0, 1))
        })]
        
        # ORIGINAL choices 
        updateSelectInput(session, "t_variable", choices = numeric_cols, selected = numeric_cols[1])
        updateSelectInput(session, "anova_dependent", choices = numeric_cols, selected = numeric_cols[1])
        updateSelectInput(session, "t_grouping_var", choices = char_factor_cols, selected = if(length(char_factor_cols) > 0) char_factor_cols[1] else NULL)
        updateSelectInput(session, "anova_factor", choices = char_factor_cols, selected = if(length(char_factor_cols) > 0) char_factor_cols[1] else NULL)
        
        # Mean tests
        updateSelectInput(session, "t2_variable", choices = numeric_cols, 
                          selected = if(length(numeric_cols) > 0) numeric_cols[1] else NULL)
        updateSelectInput(session, "t2_grouping", choices = char_factor_cols, 
                          selected = if(length(char_factor_cols) > 0) char_factor_cols[1] else NULL)
        updateSelectInput(session, "paired_var1", choices = numeric_cols, 
                          selected = if(length(numeric_cols) > 0) numeric_cols[1] else NULL)
        updateSelectInput(session, "paired_var2", choices = numeric_cols, 
                          selected = if(length(numeric_cols) > 1) numeric_cols[2] else NULL)
        
        # Proportion tests - PRIORITIZE binary variables
        prop_choices <- c(binary_numeric_cols, all_cols)  # Binary first, then all
        prop_choices <- prop_choices[!duplicated(prop_choices)]  # Remove duplicates
        
        updateSelectInput(session, "prop1_variable", choices = prop_choices, 
                          selected = if(length(binary_numeric_cols) > 0) binary_numeric_cols[1] else prop_choices[1])
        updateSelectInput(session, "prop2_variable", choices = prop_choices, 
                          selected = if(length(binary_numeric_cols) > 0) binary_numeric_cols[1] else prop_choices[1])
        updateSelectInput(session, "prop2_grouping", choices = char_factor_cols, 
                          selected = if(length(char_factor_cols) > 0) char_factor_cols[1] else NULL)
        
        # Variance tests
        updateSelectInput(session, "var1_variable", choices = numeric_cols, 
                          selected = if(length(numeric_cols) > 0) numeric_cols[1] else NULL)
        updateSelectInput(session, "var2_variable", choices = numeric_cols, 
                          selected = if(length(numeric_cols) > 0) numeric_cols[1] else NULL)
        updateSelectInput(session, "var2_grouping", choices = char_factor_cols, 
                          selected = if(length(char_factor_cols) > 0) char_factor_cols[1] else NULL)
        
        # ANOVA new inputs
        updateSelectInput(session, "anova1_dependent", choices = numeric_cols, 
                          selected = if(length(numeric_cols) > 0) numeric_cols[1] else NULL)
        updateSelectInput(session, "anova1_factor", choices = char_factor_cols, 
                          selected = if(length(char_factor_cols) > 0) char_factor_cols[1] else NULL)
        updateSelectInput(session, "anova2_dependent", choices = numeric_cols, 
                          selected = if(length(numeric_cols) > 0) numeric_cols[1] else NULL)
        updateSelectInput(session, "anova2_factor1", choices = char_factor_cols, 
                          selected = if(length(char_factor_cols) > 0) char_factor_cols[1] else NULL)
        updateSelectInput(session, "anova2_factor2", choices = char_factor_cols, 
                          selected = if(length(char_factor_cols) > 1) char_factor_cols[2] else NULL)
        
        
        showNotification(paste("Data berhasil dimuat:", source_text, 
                               if(length(binary_numeric_cols) > 0) paste("| Binary variables for proportion tests:", length(binary_numeric_cols)) else ""), 
                         type = "message")
      }
    }, error = function(e) {
      showNotification(paste("Error loading data:", e$message), type = "error")
    })
  })
  
  # Data info untuk inferensia
  output$inferensia_data_info <- renderText({
    if(input$inferensia_data_source == "processed" && !is.null(values$processed_data)) {
      paste0("Data Manajemen: ", nrow(values$processed_data), " obs, ", 
             ncol(values$processed_data), " vars")
    } else if(input$inferensia_data_source == "upload" && !is.null(input$inferensia_data_file)) {
      paste0("File: ", input$inferensia_data_file$name, "\n",
             "Size: ", round(input$inferensia_data_file$size/1024, 2), " KB")
    } else {
      "Belum ada data yang dipilih"
    }
  })
  
  # Preview data untuk inferensia
  output$inferensia_data_preview <- DT::renderDataTable({
    if(!is.null(values$inferensia_data)) {
      DT::datatable(values$inferensia_data, options = list(scrollX = TRUE, pageLength = 10),
                    caption = paste("Preview Data - Total:", nrow(values$inferensia_data), "observasi"))
    } else {
      DT::datatable(data.frame(Message = "Load data terlebih dahulu"))
    }
  })
  
  
  # ===================================================================
  # UJI RATA-RATA
  # ===================================================================
  
  # Run mean tests
  observeEvent(input$run_mean_test, {
    req(input$mean_test_type, values$inferensia_data)
    
    tryCatch({
      current_data <- values$inferensia_data
      
      if(input$mean_test_type == "one_sample") {
        req(input$t_variable, input$mu_value)
        
        if(!input$t_variable %in% names(current_data)) {
          showNotification("Variabel tidak ditemukan dalam data", type = "error")
          return()
        }
        
        var_data <- current_data[[input$t_variable]]
        var_data_clean <- var_data[!is.na(var_data) & is.finite(var_data)]
        
        if(length(var_data_clean) < 3) {
          showNotification("Data tidak cukup untuk uji t (minimal 3 observasi)", type = "warning")
          return()
        }
        
        # Perform one-sample t-test
        ttest_result <- t.test(var_data_clean, mu = input$mu_value, alternative = input$t_alternative)
        values$current_mean_test <- list(
          type = "one_sample",
          result = ttest_result,
          variable = input$t_variable,
          mu0 = input$mu_value,
          alternative = input$t_alternative,
          data = var_data_clean
        )
        
      } else if(input$mean_test_type == "two_sample_indep") {
        req(input$t2_variable, input$t2_grouping)
        
        if(!all(c(input$t2_variable, input$t2_grouping) %in% names(current_data))) {
          showNotification("Variabel tidak ditemukan dalam data", type = "error")
          return()
        }
        
        test_data <- current_data %>%
          select(!!input$t2_variable, !!input$t2_grouping) %>%
          filter(!is.na(!!sym(input$t2_variable)), !is.na(!!sym(input$t2_grouping)))
        
        groups <- unique(test_data[[input$t2_grouping]])
        if(length(groups) != 2) {
          showNotification("Variabel pengelompokan harus memiliki tepat 2 kelompok", type = "error")
          return()
        }
        
        group1_data <- test_data[test_data[[input$t2_grouping]] == groups[1], input$t2_variable]
        group2_data <- test_data[test_data[[input$t2_grouping]] == groups[2], input$t2_variable]
        
        # Perform two-sample t-test
        ttest_result <- t.test(group1_data, group2_data, 
                               var.equal = input$t2_equal_var,
                               alternative = input$t2_alternative)
        
        values$current_mean_test <- list(
          type = "two_sample_indep",
          result = ttest_result,
          variable = input$t2_variable,
          grouping = input$t2_grouping,
          groups = groups,
          alternative = input$t2_alternative,
          equal_var = input$t2_equal_var,
          data = test_data
        )
        
      } else if(input$mean_test_type == "paired") {
        req(input$paired_var1, input$paired_var2)
        
        if(!all(c(input$paired_var1, input$paired_var2) %in% names(current_data))) {
          showNotification("Variabel tidak ditemukan dalam data", type = "error")
          return()
        }
        
        paired_data <- current_data %>%
          select(!!input$paired_var1, !!input$paired_var2) %>%
          filter(!is.na(!!sym(input$paired_var1)), !is.na(!!sym(input$paired_var2)))
        
        if(nrow(paired_data) < 3) {
          showNotification("Data tidak cukup untuk paired t-test", type = "warning")
          return()
        }
        
        # Perform paired t-test
        ttest_result <- t.test(paired_data[[input$paired_var1]], 
                               paired_data[[input$paired_var2]],
                               paired = TRUE,
                               alternative = input$paired_alternative)
        
        values$current_mean_test <- list(
          type = "paired",
          result = ttest_result,
          var1 = input$paired_var1,
          var2 = input$paired_var2,
          alternative = input$paired_alternative,
          data = paired_data
        )
      }
      
      # Generate output
      generate_mean_test_output()
      showNotification("Uji rata-rata selesai", type = "message")
      
    }, error = function(e) {
      showNotification(paste("Error dalam uji rata-rata:", e$message), type = "error")
    })
  })
  
  # ===================================================================
  # UJI PROPORSI (BARU)
  # ===================================================================
  
  observeEvent(input$run_prop_test, {
    req(input$prop_test_type, values$inferensia_data)
    
    tryCatch({
      current_data <- values$inferensia_data
      
      if(input$prop_test_type == "one_prop") {
        if(input$prop1_input_type == "raw") {
          req(input$prop1_variable, input$prop1_p0)
          
          if(!input$prop1_variable %in% names(current_data)) {
            showNotification("Variabel tidak ditemukan", type = "error")
            return()
          }
          
          prop_data <- current_data[[input$prop1_variable]]
          # Convert to 0/1 if logical
          if(is.logical(prop_data)) {
            prop_data <- as.numeric(prop_data)
          }
          
          # Check if binary (0/1)
          unique_vals <- unique(prop_data[!is.na(prop_data)])
          if(!all(unique_vals %in% c(0, 1))) {
            showNotification("Variabel harus berisi nilai 0/1 atau TRUE/FALSE", type = "error")
            return()
          }
          
          successes <- sum(prop_data == 1, na.rm = TRUE)
          total <- sum(!is.na(prop_data))
          
        } else {
          req(input$prop1_success, input$prop1_total, input$prop1_p0)
          successes <- input$prop1_success
          total <- input$prop1_total
        }
        
        if(total < 5 || successes < 5 || (total - successes) < 5) {
          showNotification("Sample size terlalu kecil untuk uji proporsi (minimal 5 untuk setiap kategori)", type = "warning")
        }
        
        # Perform one-proportion test
        prop_result <- prop.test(successes, total, p = input$prop1_p0, 
                                 alternative = input$prop1_alternative)
        
        values$current_prop_test <- list(
          type = "one_prop",
          result = prop_result,
          successes = successes,
          total = total,
          p0 = input$prop1_p0,
          alternative = input$prop1_alternative,
          input_type = input$prop1_input_type
        )
        
      } else if(input$prop_test_type == "two_prop") {
        if(input$prop2_input_type == "raw") {
          req(input$prop2_variable, input$prop2_grouping)
          
          if(!all(c(input$prop2_variable, input$prop2_grouping) %in% names(current_data))) {
            showNotification("Variabel tidak ditemukan", type = "error")
            return()
          }
          
          test_data <- current_data %>%
            select(!!input$prop2_variable, !!input$prop2_grouping) %>%
            filter(!is.na(!!sym(input$prop2_variable)), !is.na(!!sym(input$prop2_grouping)))
          
          prop_var <- test_data[[input$prop2_variable]]
          if(is.logical(prop_var)) {
            prop_var <- as.numeric(prop_var)
          }
          
          groups <- unique(test_data[[input$prop2_grouping]])
          if(length(groups) != 2) {
            showNotification("Variabel pengelompokan harus memiliki tepat 2 kelompok", type = "error")
            return()
          }
          
          group1_data <- prop_var[test_data[[input$prop2_grouping]] == groups[1]]
          group2_data <- prop_var[test_data[[input$prop2_grouping]] == groups[2]]
          
          successes1 <- sum(group1_data == 1, na.rm = TRUE)
          total1 <- sum(!is.na(group1_data))
          successes2 <- sum(group2_data == 1, na.rm = TRUE)
          total2 <- sum(!is.na(group2_data))
          
        } else {
          req(input$prop2_success1, input$prop2_total1, input$prop2_success2, input$prop2_total2)
          successes1 <- input$prop2_success1
          total1 <- input$prop2_total1
          successes2 <- input$prop2_success2
          total2 <- input$prop2_total2
          groups <- c("Group 1", "Group 2")
        }
        
        # Perform two-proportion test
        prop_result <- prop.test(c(successes1, successes2), c(total1, total2),
                                 alternative = input$prop2_alternative)
        
        values$current_prop_test <- list(
          type = "two_prop",
          result = prop_result,
          successes = c(successes1, successes2),
          totals = c(total1, total2),
          groups = groups,
          alternative = input$prop2_alternative,
          input_type = input$prop2_input_type
        )
      }
      
      # Generate output
      generate_prop_test_output()
      showNotification("Uji proporsi selesai", type = "message")
      
    }, error = function(e) {
      showNotification(paste("Error dalam uji proporsi:", e$message), type = "error")
    })
  })
  
  # ===================================================================
  # UJI RAGAM (BARU)
  # ===================================================================
  
  observeEvent(input$run_var_test, {
    req(input$var_test_type, values$inferensia_data)
    
    tryCatch({
      current_data <- values$inferensia_data
      
      if(input$var_test_type == "one_var") {
        req(input$var1_variable, input$var1_sigma0)
        
        if(!input$var1_variable %in% names(current_data)) {
          showNotification("Variabel tidak ditemukan", type = "error")
          return()
        }
        
        var_data <- current_data[[input$var1_variable]]
        var_data_clean <- var_data[!is.na(var_data) & is.finite(var_data)]
        
        if(length(var_data_clean) < 3) {
          showNotification("Data tidak cukup untuk uji variance", type = "warning")
          return()
        }
        
        # Manual chi-square test for variance
        n <- length(var_data_clean)
        sample_var <- var(var_data_clean)
        chi_stat <- (n - 1) * sample_var / input$var1_sigma0
        df <- n - 1
        
        if(input$var1_alternative == "two.sided") {
          p_value <- 2 * min(pchisq(chi_stat, df), 1 - pchisq(chi_stat, df))
          critical_lower <- qchisq(0.025, df)
          critical_upper <- qchisq(0.975, df)
        } else if(input$var1_alternative == "greater") {
          p_value <- 1 - pchisq(chi_stat, df)
          critical_lower <- qchisq(0.05, df)
          critical_upper <- Inf
        } else {
          p_value <- pchisq(chi_stat, df)
          critical_lower <- 0
          critical_upper <- qchisq(0.95, df)
        }
        
        var_result <- list(
          statistic = chi_stat,
          parameter = df,
          p.value = p_value,
          estimate = sample_var,
          null.value = input$var1_sigma0,
          alternative = input$var1_alternative,
          method = "Chi-squared test for variance",
          critical_values = c(critical_lower, critical_upper)
        )
        
        values$current_var_test <- list(
          type = "one_var",
          result = var_result,
          variable = input$var1_variable,
          sigma0 = input$var1_sigma0,
          alternative = input$var1_alternative,
          data = var_data_clean
        )
        
      } else if(input$var_test_type == "two_var") {
        req(input$var2_variable, input$var2_grouping)
        
        if(!all(c(input$var2_variable, input$var2_grouping) %in% names(current_data))) {
          showNotification("Variabel tidak ditemukan", type = "error")
          return()
        }
        
        test_data <- current_data %>%
          select(!!input$var2_variable, !!input$var2_grouping) %>%
          filter(!is.na(!!sym(input$var2_variable)), !is.na(!!sym(input$var2_grouping)))
        
        groups <- unique(test_data[[input$var2_grouping]])
        if(length(groups) != 2) {
          showNotification("Variabel pengelompokan harus memiliki tepat 2 kelompok", type = "error")
          return()
        }
        
        group1_data <- test_data[test_data[[input$var2_grouping]] == groups[1], input$var2_variable]
        group2_data <- test_data[test_data[[input$var2_grouping]] == groups[2], input$var2_variable]
        
        # Perform F-test for variance
        var_result <- var.test(group1_data, group2_data, alternative = input$var2_alternative)
        
        values$current_var_test <- list(
          type = "two_var",
          result = var_result,
          variable = input$var2_variable,
          grouping = input$var2_grouping,
          groups = groups,
          alternative = input$var2_alternative,
          data = test_data
        )
      }
      
      # Generate output
      generate_var_test_output()
      showNotification("Uji ragam selesai", type = "message")
      
    }, error = function(e) {
      showNotification(paste("Error dalam uji ragam:", e$message), type = "error")
    })
  })
  
  # ===================================================================
  # ANOVA ENHANCED (TAMBAH TWO-WAY)
  # ===================================================================
  
  observeEvent(input$run_anova_test, {
    req(input$anova_type, values$inferensia_data)
    
    tryCatch({
      current_data <- values$inferensia_data
      
      if(input$anova_type == "one_way") {
        req(input$anova1_dependent, input$anova1_factor)
        
        if(!all(c(input$anova1_dependent, input$anova1_factor) %in% names(current_data))) {
          showNotification("Variabel tidak ditemukan", type = "error")
          return()
        }
        
        anova_data <- current_data %>%
          select(!!input$anova1_dependent, !!input$anova1_factor) %>%
          filter(!is.na(!!sym(input$anova1_dependent)), !is.na(!!sym(input$anova1_factor)))
        
        if(nrow(anova_data) < 10) {
          showNotification("Data tidak cukup untuk ANOVA", type = "warning")
          return()
        }
        
        n_groups <- length(unique(anova_data[[input$anova1_factor]]))
        if(n_groups < 2) {
          showNotification("Diperlukan minimal 2 kelompok untuk ANOVA", type = "warning")
          return()
        }
        
        # Perform one-way ANOVA
        anova_formula <- as.formula(paste(input$anova1_dependent, "~", input$anova1_factor))
        anova_model <- aov(anova_formula, data = anova_data)
        anova_result <- summary(anova_model)
        
        # Post-hoc test if significant
        posthoc_result <- NULL
        if(anova_result[[1]]$`Pr(>F)`[1] < 0.05) {
          tukey_result <- TukeyHSD(anova_model)
          posthoc_result <- data.frame(
            comparison = rownames(tukey_result[[1]]),
            diff = tukey_result[[1]][, "diff"],
            lwr = tukey_result[[1]][, "lwr"],
            upr = tukey_result[[1]][, "upr"],
            p_adj = tukey_result[[1]][, "p adj"],
            significant = tukey_result[[1]][, "p adj"] < 0.05
          )
        }
        
        values$current_anova_test <- list(
          type = "one_way",
          result = anova_result,
          model = anova_model,
          posthoc = posthoc_result,
          dependent = input$anova1_dependent,
          factor1 = input$anova1_factor,
          data = anova_data
        )
        
      } else if(input$anova_type == "two_way") {
        req(input$anova2_dependent, input$anova2_factor1, input$anova2_factor2)
        
        if(!all(c(input$anova2_dependent, input$anova2_factor1, input$anova2_factor2) %in% names(current_data))) {
          showNotification("Variabel tidak ditemukan", type = "error")
          return()
        }
        
        anova_data <- current_data %>%
          select(!!input$anova2_dependent, !!input$anova2_factor1, !!input$anova2_factor2) %>%
          filter(!is.na(!!sym(input$anova2_dependent)), 
                 !is.na(!!sym(input$anova2_factor1)), 
                 !is.na(!!sym(input$anova2_factor2)))
        
        if(nrow(anova_data) < 20) {
          showNotification("Data tidak cukup untuk two-way ANOVA (minimal 20 observasi)", type = "warning")
          return()
        }
        
        # Create formula with or without interaction
        if(input$anova2_interaction) {
          formula_str <- paste(input$anova2_dependent, "~", input$anova2_factor1, "*", input$anova2_factor2)
        } else {
          formula_str <- paste(input$anova2_dependent, "~", input$anova2_factor1, "+", input$anova2_factor2)
        }
        
        anova_formula <- as.formula(formula_str)
        anova_model <- aov(anova_formula, data = anova_data)
        anova_result <- summary(anova_model)
        
        # Post-hoc tests for significant main effects
        posthoc_result <- list()
        if(length(anova_result[[1]]$`Pr(>F)`) >= 2) {
          if(!is.na(anova_result[[1]]$`Pr(>F)`[1]) && anova_result[[1]]$`Pr(>F)`[1] < 0.05) {
            tukey1 <- TukeyHSD(anova_model, which = input$anova2_factor1)
            posthoc_result[[input$anova2_factor1]] <- data.frame(tukey1[[1]])
          }
          if(!is.na(anova_result[[1]]$`Pr(>F)`[2]) && anova_result[[1]]$`Pr(>F)`[2] < 0.05) {
            tukey2 <- TukeyHSD(anova_model, which = input$anova2_factor2)
            posthoc_result[[input$anova2_factor2]] <- data.frame(tukey2[[1]])
          }
        }
        
        values$current_anova_test <- list(
          type = "two_way",
          result = anova_result,
          model = anova_model,
          posthoc = posthoc_result,
          dependent = input$anova2_dependent,
          factor1 = input$anova2_factor1,
          factor2 = input$anova2_factor2,
          interaction = input$anova2_interaction,
          data = anova_data
        )
      }
      
      # Generate output
      generate_anova_test_output()
      showNotification("ANOVA selesai", type = "message")
      
    }, error = function(e) {
      showNotification(paste("Error dalam ANOVA:", e$message), type = "error")
    })
  })
  
  # ===================================================================
  # GENERATE OUTPUT FUNCTIONS
  # ===================================================================
  
  # Generate mean test output
  generate_mean_test_output <- function() {
    if(is.null(values$current_mean_test)) return()
    
    test_info <- values$current_mean_test
    result <- test_info$result
    
    # Results output
    output$mean_test_results <- renderText({
      if(test_info$type == "one_sample") {
        format_one_sample_results(result, test_info$variable, test_info$mu0, test_info$alternative)
      } else if(test_info$type == "two_sample_indep") {
        format_two_sample_results(result, test_info$variable, test_info$groups, test_info$alternative, test_info$equal_var)
      } else if(test_info$type == "paired") {
        format_paired_results(result, test_info$var1, test_info$var2, test_info$alternative)
      }
    })
    
    # Visualization
    output$mean_test_plot <- renderPlotly({
      create_mean_test_plot(test_info)
    })
    
    # Interpretation
    output$mean_test_interpretation <- renderText({
      create_mean_test_interpretation(test_info)
    })
  }
  
  # Generate proportion test output
  generate_prop_test_output <- function() {
    if(is.null(values$current_prop_test)) return()
    
    test_info <- values$current_prop_test
    result <- test_info$result
    
    # Results output
    output$prop_test_results <- renderText({
      if(test_info$type == "one_prop") {
        format_one_prop_results(result, test_info$successes, test_info$total, test_info$p0, test_info$alternative)
      } else if(test_info$type == "two_prop") {
        format_two_prop_results(result, test_info$successes, test_info$totals, test_info$groups, test_info$alternative)
      }
    })
    
    # Visualization
    output$prop_test_plot <- renderPlotly({
      create_prop_test_plot(test_info)
    })
    
    # Interpretation
    output$prop_test_interpretation <- renderText({
      create_prop_test_interpretation(test_info)
    })
  }
  
  # Generate variance test output
  generate_var_test_output <- function() {
    if(is.null(values$current_var_test)) return()
    
    test_info <- values$current_var_test
    result <- test_info$result
    
    # Results output
    output$var_test_results <- renderText({
      if(test_info$type == "one_var") {
        format_one_var_results(result, test_info$variable, test_info$sigma0, test_info$alternative)
      } else if(test_info$type == "two_var") {
        format_two_var_results(result, test_info$variable, test_info$groups, test_info$alternative)
      }
    })
    
    # Visualization
    output$var_test_plot <- renderPlotly({
      create_var_test_plot(test_info)
    })
    
    # Interpretation
    output$var_test_interpretation <- renderText({
      create_var_test_interpretation(test_info)
    })
  }
  
  # Generate ANOVA test output
  generate_anova_test_output <- function() {
    if(is.null(values$current_anova_test)) return()
    
    test_info <- values$current_anova_test
    result <- test_info$result
    
    # Results output
    output$anova_test_results <- renderText({
      if(test_info$type == "one_way") {
        format_one_way_anova_results(result, test_info$dependent, test_info$factor1)
      } else if(test_info$type == "two_way") {
        format_two_way_anova_results(result, test_info$dependent, test_info$factor1, test_info$factor2, test_info$interaction)
      }
    })
    
    # Post-hoc results
    output$anova_posthoc_results <- DT::renderDataTable({
      if(!is.null(test_info$posthoc)) {
        if(test_info$type == "one_way") {
          DT::datatable(test_info$posthoc, options = list(scrollX = TRUE, pageLength = 10),
                        caption = "Tukey HSD Post-hoc Test") %>%
            DT::formatStyle("significant", backgroundColor = DT::styleEqual(
              c(TRUE, FALSE), c("lightcoral", "lightgreen")
            ))
        } else {
          # For two-way, combine all post-hoc results
          combined_posthoc <- data.frame()
          for(factor_name in names(test_info$posthoc)) {
            factor_results <- test_info$posthoc[[factor_name]]
            factor_results$Factor <- factor_name
            factor_results$Comparison <- rownames(factor_results)
            combined_posthoc <- rbind(combined_posthoc, factor_results)
          }
          if(nrow(combined_posthoc) > 0) {
            DT::datatable(combined_posthoc, options = list(scrollX = TRUE, pageLength = 10),
                          caption = "Tukey HSD Post-hoc Tests for Significant Factors")
          } else {
            DT::datatable(data.frame(Message = "No significant main effects for post-hoc testing"))
          }
        }
      } else {
        DT::datatable(data.frame(Message = "Post-hoc test not performed (no significant effects)"))
      }
    })
    
    # Visualization
    output$anova_test_plot <- renderPlotly({
      create_anova_test_plot(test_info)
    })
    
    # Interpretation
    output$anova_test_interpretation <- renderText({
      create_anova_test_interpretation(test_info)
    })
  }
  
  # ===================================================================
  # FORMAT RESULTS FUNCTIONS
  # ===================================================================
  
  # Format one-sample t-test results
  format_one_sample_results <- function(result, variable, mu0, alternative) {
    paste0(
      "UJI T SATU SAMPEL\n",
      "==================\n",
      "Variabel: ", variable, "\n",
      "Hipotesis μ₀: ", mu0, "\n",
      "Sample mean: ", round(result$estimate, 4), "\n",
      "t-statistic: ", round(result$statistic, 4), "\n",
      "df: ", result$parameter, "\n",
      "p-value: ", round(result$p.value, 4), "\n",
      "95% Confidence Interval: [", round(result$conf.int[1], 4), ", ", round(result$conf.int[2], 4), "]\n",
      "Alternative hypothesis: ", result$alternative, "\n\n",
      "Keputusan: ", ifelse(result$p.value < 0.05, "TOLAK H₀", "TERIMA H₀"), "\n",
      "Kesimpulan: ", ifelse(result$p.value < 0.05, 
                             paste("Rata-rata populasi berbeda signifikan dari", mu0),
                             paste("Tidak ada perbedaan signifikan dari", mu0))
    )
  }
  
  # Format two-sample t-test results
  format_two_sample_results <- function(result, variable, groups, alternative, equal_var) {
    paste0(
      "UJI T DUA SAMPEL INDEPENDEN\n",
      "============================\n",
      "Variabel: ", variable, "\n",
      "Kelompok: ", paste(groups, collapse = " vs "), "\n",
      "Equal variances: ", ifelse(equal_var, "Ya", "Tidak"), "\n",
      "Mean Group 1: ", round(result$estimate[1], 4), "\n",
      "Mean Group 2: ", round(result$estimate[2], 4), "\n",
      "Mean difference: ", round(result$estimate[1] - result$estimate[2], 4), "\n",
      "t-statistic: ", round(result$statistic, 4), "\n",
      "df: ", round(result$parameter, 2), "\n",
      "p-value: ", round(result$p.value, 4), "\n",
      "95% CI for difference: [", round(result$conf.int[1], 4), ", ", round(result$conf.int[2], 4), "]\n",
      "Alternative hypothesis: ", result$alternative, "\n\n",
      "Keputusan: ", ifelse(result$p.value < 0.05, "TOLAK H₀", "TERIMA H₀"), "\n",
      "Kesimpulan: ", ifelse(result$p.value < 0.05, 
                             "Ada perbedaan rata-rata yang signifikan antar kelompok",
                             "Tidak ada perbedaan rata-rata yang signifikan antar kelompok")
    )
  }
  
  # Format paired t-test results
  format_paired_results <- function(result, var1, var2, alternative) {
    paste0(
      "UJI T BERPASANGAN\n",
      "==================\n",
      "Variabel 1: ", var1, "\n",
      "Variabel 2: ", var2, "\n",
      "Mean difference (", var1, " - ", var2, "): ", round(result$estimate, 4), "\n",
      "t-statistic: ", round(result$statistic, 4), "\n",
      "df: ", result$parameter, "\n",
      "p-value: ", round(result$p.value, 4), "\n",
      "95% CI for difference: [", round(result$conf.int[1], 4), ", ", round(result$conf.int[2], 4), "]\n",
      "Alternative hypothesis: ", result$alternative, "\n\n",
      "Keputusan: ", ifelse(result$p.value < 0.05, "TOLAK H₀", "TERIMA H₀"), "\n",
      "Kesimpulan: ", ifelse(result$p.value < 0.05, 
                             paste("Ada perbedaan signifikan antara", var1, "dan", var2),
                             paste("Tidak ada perbedaan signifikan antara", var1, "dan", var2))
    )
  }
  
  # Format one proportion test results
  format_one_prop_results <- function(result, successes, total, p0, alternative) {
    sample_prop <- successes / total
    paste0(
      "UJI PROPORSI SATU SAMPEL\n",
      "=========================\n",
      "Successes: ", successes, "\n",
      "Total: ", total, "\n",
      "Sample proportion: ", round(sample_prop, 4), "\n",
      "Hypothesized proportion (p₀): ", p0, "\n",
      "X-squared: ", round(result$statistic, 4), "\n",
      "df: ", result$parameter, "\n",
      "p-value: ", round(result$p.value, 4), "\n",
      "95% CI: [", round(result$conf.int[1], 4), ", ", round(result$conf.int[2], 4), "]\n",
      "Alternative hypothesis: ", result$alternative, "\n\n",
      "Keputusan: ", ifelse(result$p.value < 0.05, "TOLAK H₀", "TERIMA H₀"), "\n",
      "Kesimpulan: ", ifelse(result$p.value < 0.05, 
                             paste("Proporsi populasi berbeda signifikan dari", p0),
                             paste("Tidak ada perbedaan signifikan dari", p0))
    )
  }
  
  # Format two proportion test results
  format_two_prop_results <- function(result, successes, totals, groups, alternative) {
    prop1 <- successes[1] / totals[1]
    prop2 <- successes[2] / totals[2]
    paste0(
      "UJI PROPORSI DUA SAMPEL\n",
      "========================\n",
      "Group 1 (", groups[1], "): ", successes[1], "/", totals[1], " = ", round(prop1, 4), "\n",
      "Group 2 (", groups[2], "): ", successes[2], "/", totals[2], " = ", round(prop2, 4), "\n",
      "Proportion difference: ", round(prop1 - prop2, 4), "\n",
      "X-squared: ", round(result$statistic, 4), "\n",
      "df: ", result$parameter, "\n",
      "p-value: ", round(result$p.value, 4), "\n",
      "95% CI for difference: [", round(result$conf.int[1], 4), ", ", round(result$conf.int[2], 4), "]\n",
      "Alternative hypothesis: ", result$alternative, "\n\n",
      "Keputusan: ", ifelse(result$p.value < 0.05, "TOLAK H₀", "TERIMA H₀"), "\n",
      "Kesimpulan: ", ifelse(result$p.value < 0.05, 
                             "Ada perbedaan proporsi yang signifikan antar kelompok",
                             "Tidak ada perbedaan proporsi yang signifikan antar kelompok")
    )
  }
  
  # Format one variance test results
  format_one_var_results <- function(result, variable, sigma0, alternative) {
    paste0(
      "UJI RAGAM SATU SAMPEL (CHI-SQUARE)\n",
      "===================================\n",
      "Variabel: ", variable, "\n",
      "Hypothesized variance (σ²₀): ", sigma0, "\n",
      "Sample variance: ", round(result$estimate, 4), "\n",
      "Chi-square statistic: ", round(result$statistic, 4), "\n",
      "df: ", result$parameter, "\n",
      "p-value: ", round(result$p.value, 4), "\n",
      "Critical values: [", round(result$critical_values[1], 4), ", ", 
      ifelse(is.finite(result$critical_values[2]), round(result$critical_values[2], 4), "∞"), "]\n",
      "Alternative hypothesis: ", result$alternative, "\n\n",
      "Keputusan: ", ifelse(result$p.value < 0.05, "TOLAK H₀", "TERIMA H₀"), "\n",
      "Kesimpulan: ", ifelse(result$p.value < 0.05, 
                             paste("Ragam populasi berbeda signifikan dari", sigma0),
                             paste("Tidak ada perbedaan signifikan dari", sigma0))
    )
  }
  
  # Format two variance test results (F-test)
  format_two_var_results <- function(result, variable, groups, alternative) {
    paste0(
      "UJI RAGAM DUA SAMPEL (F-TEST)\n",
      "==============================\n",
      "Variabel: ", variable, "\n",
      "Kelompok: ", paste(groups, collapse = " vs "), "\n",
      "Variance Group 1: ", round(result$estimate[1], 4), "\n",
      "Variance Group 2: ", round(result$estimate[2], 4), "\n",
      "Variance ratio (F): ", round(result$statistic, 4), "\n",
      "df1: ", result$parameter[1], ", df2: ", result$parameter[2], "\n",
      "p-value: ", round(result$p.value, 4), "\n",
      "95% CI for ratio: [", round(result$conf.int[1], 4), ", ", round(result$conf.int[2], 4), "]\n",
      "Alternative hypothesis: ", result$alternative, "\n\n",
      "Keputusan: ", ifelse(result$p.value < 0.05, "TOLAK H₀", "TERIMA H₀"), "\n",
      "Kesimpulan: ", ifelse(result$p.value < 0.05, 
                             "Ada perbedaan ragam yang signifikan antar kelompok",
                             "Tidak ada perbedaan ragam yang signifikan antar kelompok")
    )
  }
  
  # Format one-way ANOVA results
  format_one_way_anova_results <- function(result, dependent, factor1) {
    anova_table <- result[[1]]
    paste0(
      "ANOVA SATU ARAH\n",
      "================\n",
      "Variabel Terikat: ", dependent, "\n",
      "Faktor: ", factor1, "\n\n",
      "ANOVA Table:\n",
      "Source\t\tDF\tSum Sq\t\tMean Sq\t\tF value\t\tPr(>F)\n",
      "--------------------------------------------------------------------------\n",
      factor1, "\t", anova_table$Df[1], "\t", round(anova_table$`Sum Sq`[1], 2), "\t\t",
      round(anova_table$`Mean Sq`[1], 2), "\t\t", round(anova_table$`F value`[1], 4), "\t\t",
      round(anova_table$`Pr(>F)`[1], 4), "\n",
      "Residuals\t", anova_table$Df[2], "\t", round(anova_table$`Sum Sq`[2], 2), "\t\t",
      round(anova_table$`Mean Sq`[2], 2), "\n\n",
      "Keputusan: ", ifelse(anova_table$`Pr(>F)`[1] < 0.05, "TOLAK H₀", "TERIMA H₀"), "\n",
      "Kesimpulan: ", ifelse(anova_table$`Pr(>F)`[1] < 0.05, 
                             "Ada perbedaan rata-rata yang signifikan antar kelompok",
                             "Tidak ada perbedaan rata-rata yang signifikan antar kelompok")
    )
  }
  
  # Format two-way ANOVA results
  format_two_way_anova_results <- function(result, dependent, factor1, factor2, interaction) {
    anova_table <- result[[1]]
    
    output_text <- paste0(
      "ANOVA DUA ARAH\n",
      "===============\n",
      "Variabel Terikat: ", dependent, "\n",
      "Faktor 1: ", factor1, "\n",
      "Faktor 2: ", factor2, "\n",
      "Interaksi: ", ifelse(interaction, "Ya", "Tidak"), "\n\n",
      "ANOVA Table:\n",
      "Source\t\t\tDF\tSum Sq\t\tMean Sq\t\tF value\t\tPr(>F)\n",
      "--------------------------------------------------------------------------\n"
    )
    
    # Add each row of ANOVA table
    for(i in 1:nrow(anova_table)) {
      source_name <- rownames(anova_table)[i]
      if(source_name == "Residuals") {
        output_text <- paste0(output_text,
                              source_name, "\t", anova_table$Df[i], "\t", round(anova_table$`Sum Sq`[i], 2), "\t\t",
                              round(anova_table$`Mean Sq`[i], 2), "\n")
      } else {
        output_text <- paste0(output_text,
                              source_name, "\t", anova_table$Df[i], "\t", round(anova_table$`Sum Sq`[i], 2), "\t\t",
                              round(anova_table$`Mean Sq`[i], 2), "\t\t", round(anova_table$`F value`[i], 4), "\t\t",
                              round(anova_table$`Pr(>F)`[i], 4), "\n")
      }
    }
    
    # Overall conclusion
    significant_effects <- sum(anova_table$`Pr(>F)` < 0.05, na.rm = TRUE)
    output_text <- paste0(output_text, "\n",
                          "Efek signifikan: ", significant_effects, " dari ", sum(!is.na(anova_table$`Pr(>F)`)), " efek\n"
    )
    
    return(output_text)
  }
  
  # ===================================================================
  # INTERPRETASI LENGKAP FUNCTIONS (6 LANGKAH)
  # ===================================================================
  
  # Create mean test interpretation
  create_mean_test_interpretation <- function(test_info) {
    result <- test_info$result
    
    if(test_info$type == "one_sample") {
      return(interpret_one_sample_ttest(result, test_info$variable, test_info$mu0, test_info$alternative))
    } else if(test_info$type == "two_sample_indep") {
      return(interpret_two_sample_ttest(result, test_info$variable, test_info$groups, test_info$alternative, test_info$equal_var))
    } else if(test_info$type == "paired") {
      return(interpret_paired_ttest(result, test_info$var1, test_info$var2, test_info$alternative))
    }
  }
  
  # Interpretasi One-Sample t-test
  interpret_one_sample_ttest <- function(result, variable, mu0, alternative) {
    alpha <- 0.05
    t_stat <- as.numeric(result$statistic)
    df <- as.numeric(result$parameter)
    p_value <- as.numeric(result$p.value)
    sample_mean <- as.numeric(result$estimate)
    
    # Critical values
    if(alternative == "two.sided") {
      t_critical <- qt(0.975, df)
      critical_region <- paste("t < -", round(t_critical, 4), " atau t > ", round(t_critical, 4))
      h1_text <- paste("μ ≠", mu0)
    } else if(alternative == "greater") {
      t_critical <- qt(0.95, df)
      critical_region <- paste("t >", round(t_critical, 4))
      h1_text <- paste("μ >", mu0)
    } else {
      t_critical <- qt(0.05, df)
      critical_region <- paste("t <", round(t_critical, 4))
      h1_text <- paste("μ <", mu0)
    }
    
    # Decision
    decision <- ifelse(p_value < alpha, "TOLAK H₀", "TERIMA H₀")
    
    # Effect size (Cohen's d)
    pooled_sd <- sqrt(sum((test_info$data - mean(test_info$data))^2) / (length(test_info$data) - 1))
    cohens_d <- abs(sample_mean - mu0) / pooled_sd
    effect_size_interpretation <- ifelse(cohens_d < 0.2, "sangat kecil",
                                         ifelse(cohens_d < 0.5, "kecil",
                                                ifelse(cohens_d < 0.8, "sedang", "besar")))
    
    paste0(
      "INTERPRETASI LENGKAP UJI T SATU SAMPEL\n",
      "=======================================\n",
      "Variabel: ", variable, "\n",
      "Sample size: ", length(test_info$data), "\n",
      "Sample mean: ", round(sample_mean, 4), "\n\n",
      
      "1) HIPOTESIS:\n",
      "   H₀: μ = ", mu0, " (rata-rata populasi sama dengan ", mu0, ")\n",
      "   H₁: ", h1_text, " (rata-rata populasi ", 
      ifelse(alternative == "two.sided", "tidak sama dengan", 
             ifelse(alternative == "greater", "lebih besar dari", "lebih kecil dari")), " ", mu0, ")\n\n",
      
      "2) TINGKAT SIGNIFIKANSI:\n",
      "   α = ", alpha, " (5%)\n",
      "   Tingkat kepercayaan = 95%\n\n",
      
      "3) WILAYAH KRITIS:\n",
      "   Distribusi: t dengan df = ", df, "\n",
      "   Wilayah kritis: ", critical_region, "\n",
      "   Statistik uji jatuh di wilayah: ", 
      ifelse(p_value < alpha, "KRITIS (tolak H₀)", "TIDAK KRITIS (terima H₀)"), "\n\n",
      
      "4) STATISTIK UJI:\n",
      "   t = (x̄ - μ₀) / (s/√n)\n",
      "   t = (", round(sample_mean, 4), " - ", mu0, ") / (s/√", length(test_info$data), ")\n",
      "   t = ", round(t_stat, 4), "\n",
      "   p-value = ", round(p_value, 4), "\n\n",
      
      "5) KEPUTUSAN STATISTIK:\n",
      "   Karena p-value (", round(p_value, 4), ") ", 
      ifelse(p_value < alpha, "<", "≥"), " α (", alpha, "), maka ", decision, "\n",
      "   Berdasarkan wilayah kritis: statistik uji ", 
      ifelse(p_value < alpha, "berada di", "tidak berada di"), " wilayah kritis\n\n",
      
      "6) KESIMPULAN:\n",
      "   Pada tingkat signifikansi 5%, ", 
      ifelse(p_value < alpha, 
             paste("terdapat cukup bukti untuk menyatakan bahwa rata-rata", variable, 
                   ifelse(alternative == "two.sided", "berbeda signifikan dari", 
                          ifelse(alternative == "greater", "lebih besar dari", "lebih kecil dari")), mu0),
             paste("tidak terdapat cukup bukti untuk menyatakan bahwa rata-rata", variable, 
                   ifelse(alternative == "two.sided", "berbeda dari", 
                          ifelse(alternative == "greater", "lebih besar dari", "lebih kecil dari")), mu0)), ".\n",
      "   Effect size (Cohen's d): ", round(cohens_d, 3), " (", effect_size_interpretation, ")\n",
      "   95% CI untuk μ: [", round(result$conf.int[1], 4), ", ", round(result$conf.int[2], 4), "]\n\n",
      
      "IMPLIKASI PRAKTIS:\n",
      ifelse(p_value < alpha,
             paste("Perbedaan rata-rata sebesar", round(abs(sample_mean - mu0), 4), 
                   "secara statistik signifikan dan memiliki effect size yang", effect_size_interpretation, "."),
             paste("Perbedaan rata-rata sebesar", round(abs(sample_mean - mu0), 4), 
                   "tidak signifikan secara statistik, mungkin disebabkan oleh variasi sampling.")),
      ifelse(p_value < alpha && cohens_d < 0.5, 
             " Meskipun signifikan secara statistik, perbedaan ini mungkin tidak bermakna secara praktis.", "")
    )
  }
  
  # Interpretasi Two-Sample t-test
  interpret_two_sample_ttest <- function(result, variable, groups, alternative, equal_var) {
    alpha <- 0.05
    t_stat <- as.numeric(result$statistic)
    df <- as.numeric(result$parameter)
    p_value <- as.numeric(result$p.value)
    mean1 <- as.numeric(result$estimate[1])
    mean2 <- as.numeric(result$estimate[2])
    mean_diff <- mean1 - mean2
    
    # Critical values
    if(alternative == "two.sided") {
      t_critical <- qt(0.975, df)
      critical_region <- paste("t < -", round(t_critical, 4), " atau t > ", round(t_critical, 4))
      h1_text <- "μ₁ ≠ μ₂"
    } else if(alternative == "greater") {
      t_critical <- qt(0.95, df)
      critical_region <- paste("t >", round(t_critical, 4))
      h1_text <- "μ₁ > μ₂"
    } else {
      t_critical <- qt(0.05, df)
      critical_region <- paste("t <", round(t_critical, 4))
      h1_text <- "μ₁ < μ₂"
    }
    
    decision <- ifelse(p_value < alpha, "TOLAK H₀", "TERIMA H₀")
    
    # Effect size (Cohen's d for independent samples)
    pooled_sd <- sqrt(((mean1 - mean2)^2) / 2)  # Simplified estimate
    cohens_d <- abs(mean_diff) / pooled_sd
    effect_size_interpretation <- ifelse(cohens_d < 0.2, "sangat kecil",
                                         ifelse(cohens_d < 0.5, "kecil",
                                                ifelse(cohens_d < 0.8, "sedang", "besar")))
    
    paste0(
      "INTERPRETASI LENGKAP UJI T DUA SAMPEL INDEPENDEN\n",
      "================================================\n",
      "Variabel: ", variable, "\n",
      "Kelompok 1: ", groups[1], " (rata-rata: ", round(mean1, 4), ")\n",
      "Kelompok 2: ", groups[2], " (rata-rata: ", round(mean2, 4), ")\n",
      "Asumsi equal variance: ", ifelse(equal_var, "Ya", "Tidak"), "\n\n",
      
      "1) HIPOTESIS:\n",
      "   H₀: μ₁ = μ₂ (rata-rata kedua kelompok sama)\n",
      "   H₁: ", h1_text, " (rata-rata kedua kelompok ", 
      ifelse(alternative == "two.sided", "tidak sama", 
             ifelse(alternative == "greater", "kelompok 1 lebih besar", "kelompok 1 lebih kecil")), ")\n\n",
      
      "2) TINGKAT SIGNIFIKANSI:\n",
      "   α = ", alpha, " (5%)\n",
      "   Tingkat kepercayaan = 95%\n\n",
      
      "3) WILAYAH KRITIS:\n",
      "   Distribusi: t dengan df = ", round(df, 2), "\n",
      "   Jenis uji: ", ifelse(equal_var, "Equal variance", "Welch's t-test"), "\n",
      "   Wilayah kritis: ", critical_region, "\n\n",
      
      "4) STATISTIK UJI:\n",
      "   t = (x̄₁ - x̄₂) / SE\n",
      "   t = (", round(mean1, 4), " - ", round(mean2, 4), ") / SE\n",
      "   t = ", round(t_stat, 4), "\n",
      "   p-value = ", round(p_value, 4), "\n\n",
      
      "5) KEPUTUSAN STATISTIK:\n",
      "   Karena p-value (", round(p_value, 4), ") ", 
      ifelse(p_value < alpha, "<", "≥"), " α (", alpha, "), maka ", decision, "\n\n",
      
      "6) KESIMPULAN:\n",
      "   Pada tingkat signifikansi 5%, ", 
      ifelse(p_value < alpha, 
             paste("terdapat perbedaan rata-rata yang signifikan antara", groups[1], "dan", groups[2]),
             paste("tidak terdapat perbedaan rata-rata yang signifikan antara", groups[1], "dan", groups[2])), ".\n",
      "   Selisih rata-rata: ", round(mean_diff, 4), "\n",
      "   Effect size (Cohen's d): ", round(cohens_d, 3), " (", effect_size_interpretation, ")\n",
      "   95% CI untuk selisih: [", round(result$conf.int[1], 4), ", ", round(result$conf.int[2], 4), "]\n\n",
      
      "IMPLIKASI PRAKTIS:\n",
      ifelse(p_value < alpha,
             paste("Kelompok", groups[1], 
                   ifelse(mean_diff > 0, "memiliki rata-rata lebih tinggi", "memiliki rata-rata lebih rendah"),
                   "sebesar", round(abs(mean_diff), 4), "dibanding", groups[2], "."),
             "Tidak ada perbedaan yang bermakna antara kedua kelompok.")
    )
  }
  
  # Interpretasi Paired t-test
  interpret_paired_ttest <- function(result, var1, var2, alternative) {
    alpha <- 0.05
    t_stat <- as.numeric(result$statistic)
    df <- as.numeric(result$parameter)
    p_value <- as.numeric(result$p.value)
    mean_diff <- as.numeric(result$estimate)
    
    # Critical values
    if(alternative == "two.sided") {
      t_critical <- qt(0.975, df)
      critical_region <- paste("t < -", round(t_critical, 4), " atau t > ", round(t_critical, 4))
      h1_text <- "μd ≠ 0"
    } else if(alternative == "greater") {
      t_critical <- qt(0.95, df)
      critical_region <- paste("t >", round(t_critical, 4))
      h1_text <- "μd > 0"
    } else {
      t_critical <- qt(0.05, df)
      critical_region <- paste("t <", round(t_critical, 4))
      h1_text <- "μd < 0"
    }
    
    decision <- ifelse(p_value < alpha, "TOLAK H₀", "TERIMA H₀")
    
    paste0(
      "INTERPRETASI LENGKAP UJI T BERPASANGAN\n",
      "======================================\n",
      "Variabel 1 (Before): ", var1, "\n",
      "Variabel 2 (After): ", var2, "\n",
      "Jumlah pasangan: ", df + 1, "\n",
      "Rata-rata selisih (d̄): ", round(mean_diff, 4), "\n\n",
      
      "1) HIPOTESIS:\n",
      "   H₀: μd = 0 (tidak ada perbedaan rata-rata antara pengukuran pertama dan kedua)\n",
      "   H₁: ", h1_text, " (ada perbedaan rata-rata)\n",
      "   dimana μd = rata-rata selisih populasi (", var1, " - ", var2, ")\n\n",
      
      "2) TINGKAT SIGNIFIKANSI:\n",
      "   α = ", alpha, " (5%)\n",
      "   Tingkat kepercayaan = 95%\n\n",
      
      "3) WILAYAH KRITIS:\n",
      "   Distribusi: t dengan df = ", df, "\n",
      "   Wilayah kritis: ", critical_region, "\n\n",
      
      "4) STATISTIK UJI:\n",
      "   t = d̄ / (sd/√n)\n",
      "   t = ", round(mean_diff, 4), " / (sd/√", df + 1, ")\n",
      "   t = ", round(t_stat, 4), "\n",
      "   p-value = ", round(p_value, 4), "\n\n",
      
      "5) KEPUTUSAN STATISTIK:\n",
      "   Karena p-value (", round(p_value, 4), ") ", 
      ifelse(p_value < alpha, "<", "≥"), " α (", alpha, "), maka ", decision, "\n\n",
      
      "6) KESIMPULAN:\n",
      "   Pada tingkat signifikansi 5%, ", 
      ifelse(p_value < alpha, 
             paste("terdapat perbedaan yang signifikan antara", var1, "dan", var2),
             paste("tidak terdapat perbedaan yang signifikan antara", var1, "dan", var2)), ".\n",
      "   Rata-rata selisih: ", round(mean_diff, 4), "\n",
      "   95% CI untuk μd: [", round(result$conf.int[1], 4), ", ", round(result$conf.int[2], 4), "]\n\n",
      
      "IMPLIKASI PRAKTIS:\n",
      ifelse(p_value < alpha,
             paste("Ada perubahan rata-rata sebesar", round(abs(mean_diff), 4), 
                   ifelse(mean_diff > 0, 
                          paste("(peningkatan dari", var2, "ke", var1, ")"),
                          paste("(penurunan dari", var1, "ke", var2, ")"))),
             "Tidak ada perubahan yang bermakna antara kedua pengukuran.")
    )
  }
  
  # Create proportion test interpretation
  create_prop_test_interpretation <- function(test_info) {
    result <- test_info$result
    
    if(test_info$type == "one_prop") {
      return(interpret_one_prop_test(result, test_info$successes, test_info$total, test_info$p0, test_info$alternative))
    } else if(test_info$type == "two_prop") {
      return(interpret_two_prop_test(result, test_info$successes, test_info$totals, test_info$groups, test_info$alternative))
    }
  }
  
  # Interpretasi One Proportion Test
  interpret_one_prop_test <- function(result, successes, total, p0, alternative) {
    alpha <- 0.05
    chi_stat <- as.numeric(result$statistic)
    df <- as.numeric(result$parameter)
    p_value <- as.numeric(result$p.value)
    sample_prop <- successes / total
    
    # Critical values for chi-square
    chi_critical <- qchisq(0.95, df)
    
    # Z-statistic for proportion (alternative calculation)
    se <- sqrt(p0 * (1 - p0) / total)
    z_stat <- (sample_prop - p0) / se
    
    if(alternative == "two.sided") {
      z_critical <- qnorm(0.975)
      critical_region <- paste("z < -", round(z_critical, 4), " atau z > ", round(z_critical, 4))
      h1_text <- paste("p ≠", p0)
    } else if(alternative == "greater") {
      z_critical <- qnorm(0.95)
      critical_region <- paste("z >", round(z_critical, 4))
      h1_text <- paste("p >", p0)
    } else {
      z_critical <- qnorm(0.05)
      critical_region <- paste("z <", round(z_critical, 4))
      h1_text <- paste("p <", p0)
    }
    
    decision <- ifelse(p_value < alpha, "TOLAK H₀", "TERIMA H₀")
    
    # Effect size (Cohen's h)
    cohens_h <- 2 * (asin(sqrt(sample_prop)) - asin(sqrt(p0)))
    effect_size_interpretation <- ifelse(abs(cohens_h) < 0.2, "sangat kecil",
                                         ifelse(abs(cohens_h) < 0.5, "kecil",
                                                ifelse(abs(cohens_h) < 0.8, "sedang", "besar")))
    
    paste0(
      "INTERPRETASI LENGKAP UJI PROPORSI SATU SAMPEL\n",
      "=============================================\n",
      "Successes: ", successes, " dari ", total, " observasi\n",
      "Sample proportion: ", round(sample_prop, 4), "\n",
      "Hypothesized proportion: ", p0, "\n\n",
      
      "1) HIPOTESIS:\n",
      "   H₀: p = ", p0, " (proporsi populasi sama dengan ", p0, ")\n",
      "   H₁: ", h1_text, " (proporsi populasi ", 
      ifelse(alternative == "two.sided", "tidak sama dengan", 
             ifelse(alternative == "greater", "lebih besar dari", "lebih kecil dari")), " ", p0, ")\n\n",
      
      "2) TINGKAT SIGNIFIKANSI:\n",
      "   α = ", alpha, " (5%)\n",
      "   Tingkat kepercayaan = 95%\n\n",
      
      "3) WILAYAH KRITIS:\n",
      "   Distribusi: Chi-square dengan df = ", df, " (atau Z untuk proporsi)\n",
      "   Chi-square kritis: ", round(chi_critical, 4), "\n",
      "   Z kritis: ", critical_region, "\n",
      "   Asumsi: np₀ ≥ 5 dan n(1-p₀) ≥ 5\n",
      "   Check: n×", p0, " = ", total * p0, " dan n×", 1-p0, " = ", total * (1-p0), "\n\n",
      
      "4) STATISTIK UJI:\n",
      "   Chi-square = (observed - expected)² / expected\n",
      "   χ² = ", round(chi_stat, 4), "\n",
      "   Atau menggunakan Z-test:\n",
      "   z = (p̂ - p₀) / √(p₀(1-p₀)/n)\n",
      "   z = (", round(sample_prop, 4), " - ", p0, ") / √(", p0, "×", 1-p0, "/", total, ")\n",
      "   z = ", round(z_stat, 4), "\n",
      "   p-value = ", round(p_value, 4), "\n\n",
      
      "5) KEPUTUSAN STATISTIK:\n",
      "   Karena p-value (", round(p_value, 4), ") ", 
      ifelse(p_value < alpha, "<", "≥"), " α (", alpha, "), maka ", decision, "\n",
      "   Chi-square statistic (", round(chi_stat, 4), ") ", 
      ifelse(chi_stat > chi_critical, "berada di", "tidak berada di"), " wilayah kritis\n\n",
      
      "6) KESIMPULAN:\n",
      "   Pada tingkat signifikansi 5%, ", 
      ifelse(p_value < alpha, 
             paste("terdapat cukup bukti untuk menyatakan bahwa proporsi populasi", 
                   ifelse(alternative == "two.sided", "berbeda signifikan dari", 
                          ifelse(alternative == "greater", "lebih besar dari", "lebih kecil dari")), p0),
             paste("tidak terdapat cukup bukti untuk menyatakan bahwa proporsi populasi", 
                   ifelse(alternative == "two.sided", "berbeda dari", 
                          ifelse(alternative == "greater", "lebih besar dari", "lebih kecil dari")), p0)), ".\n",
      "   Selisih proporsi: ", round(sample_prop - p0, 4), "\n",
      "   Effect size (Cohen's h): ", round(cohens_h, 3), " (", effect_size_interpretation, ")\n",
      "   95% CI untuk p: [", round(result$conf.int[1], 4), ", ", round(result$conf.int[2], 4), "]\n\n",
      
      "IMPLIKASI PRAKTIS:\n",
      ifelse(p_value < alpha,
             paste("Proporsi sampel (", round(sample_prop, 4), ") berbeda secara signifikan dari yang dihipotesiskan (",
                   p0, "). Perbedaan sebesar", round(abs(sample_prop - p0), 4), "memiliki effect size yang", effect_size_interpretation, "."),
             paste("Proporsi sampel (", round(sample_prop, 4), ") tidak berbeda secara signifikan dari yang dihipotesiskan (",
                   p0, "). Perbedaan yang diamati mungkin disebabkan oleh variasi sampling."))
    )
  }
  
  # Interpretasi Two Proportion Test
  interpret_two_prop_test <- function(result, successes, totals, groups, alternative) {
    alpha <- 0.05
    chi_stat <- as.numeric(result$statistic)
    df <- as.numeric(result$parameter)
    p_value <- as.numeric(result$p.value)
    prop1 <- successes[1] / totals[1]
    prop2 <- successes[2] / totals[2]
    prop_diff <- prop1 - prop2
    
    # Pooled proportion
    pooled_prop <- sum(successes) / sum(totals)
    
    # Standard error for difference
    se_diff <- sqrt(pooled_prop * (1 - pooled_prop) * (1/totals[1] + 1/totals[2]))
    z_stat <- prop_diff / se_diff
    
    # Critical values
    if(alternative == "two.sided") {
      z_critical <- qnorm(0.975)
      critical_region <- paste("z < -", round(z_critical, 4), " atau z > ", round(z_critical, 4))
      h1_text <- "p₁ ≠ p₂"
    } else if(alternative == "greater") {
      z_critical <- qnorm(0.95)
      critical_region <- paste("z >", round(z_critical, 4))
      h1_text <- "p₁ > p₂"
    } else {
      z_critical <- qnorm(0.05)
      critical_region <- paste("z <", round(z_critical, 4))
      h1_text <- "p₁ < p₂"
    }
    
    decision <- ifelse(p_value < alpha, "TOLAK H₀", "TERIMA H₀")
    
    # Effect size (Cohen's h)
    cohens_h <- 2 * (asin(sqrt(prop1)) - asin(sqrt(prop2)))
    effect_size_interpretation <- ifelse(abs(cohens_h) < 0.2, "sangat kecil",
                                         ifelse(abs(cohens_h) < 0.5, "kecil",
                                                ifelse(abs(cohens_h) < 0.8, "sedang", "besar")))
    
    paste0(
      "INTERPRETASI LENGKAP UJI PROPORSI DUA SAMPEL\n",
      "============================================\n",
      "Kelompok 1 (", groups[1], "): ", successes[1], "/", totals[1], " = ", round(prop1, 4), "\n",
      "Kelompok 2 (", groups[2], "): ", successes[2], "/", totals[2], " = ", round(prop2, 4), "\n",
      "Pooled proportion: ", round(pooled_prop, 4), "\n\n",
      
      "1) HIPOTESIS:\n",
      "   H₀: p₁ = p₂ (proporsi kedua kelompok sama)\n",
      "   H₁: ", h1_text, " (proporsi kedua kelompok ", 
      ifelse(alternative == "two.sided", "tidak sama", 
             ifelse(alternative == "greater", "kelompok 1 lebih besar", "kelompok 1 lebih kecil")), ")\n\n",
      
      "2) TINGKAT SIGNIFIKANSI:\n",
      "   α = ", alpha, " (5%)\n",
      "   Tingkat kepercayaan = 95%\n\n",
      
      "3) WILAYAH KRITIS:\n",
      "   Distribusi: Chi-square dengan df = ", df, " (atau Z untuk proporsi)\n",
      "   Z kritis: ", critical_region, "\n",
      "   Asumsi: Semua expected frequencies ≥ 5\n\n",
      
      "4) STATISTIK UJI:\n",
      "   Chi-square = ", round(chi_stat, 4), "\n",
      "   Atau menggunakan Z-test:\n",
      "   z = (p̂₁ - p̂₂) / SE\n",
      "   z = (", round(prop1, 4), " - ", round(prop2, 4), ") / ", round(se_diff, 4), "\n",
      "   z = ", round(z_stat, 4), "\n",
      "   p-value = ", round(p_value, 4), "\n\n",
      
      "5) KEPUTUSAN STATISTIK:\n",
      "   Karena p-value (", round(p_value, 4), ") ", 
      ifelse(p_value < alpha, "<", "≥"), " α (", alpha, "), maka ", decision, "\n\n",
      
      "6) KESIMPULAN:\n",
      "   Pada tingkat signifikansi 5%, ", 
      ifelse(p_value < alpha, 
             paste("terdapat perbedaan proporsi yang signifikan antara", groups[1], "dan", groups[2]),
             paste("tidak terdapat perbedaan proporsi yang signifikan antara", groups[1], "dan", groups[2])), ".\n",
      "   Selisih proporsi: ", round(prop_diff, 4), "\n",
      "   Effect size (Cohen's h): ", round(cohens_h, 3), " (", effect_size_interpretation, ")\n",
      "   95% CI untuk selisih: [", round(result$conf.int[1], 4), ", ", round(result$conf.int[2], 4), "]\n\n",
      
      "IMPLIKASI PRAKTIS:\n",
      ifelse(p_value < alpha,
             paste("Kelompok", groups[1], "memiliki proporsi", 
                   ifelse(prop_diff > 0, "lebih tinggi", "lebih rendah"),
                   "sebesar", round(abs(prop_diff), 4), "dibanding", groups[2], "."),
             "Tidak ada perbedaan proporsi yang bermakna antara kedua kelompok.")
    )
  }
  
  # Create variance test interpretation
  create_var_test_interpretation <- function(test_info) {
    result <- test_info$result
    
    if(test_info$type == "one_var") {
      return(interpret_one_var_test(result, test_info$variable, test_info$sigma0, test_info$alternative))
    } else if(test_info$type == "two_var") {
      return(interpret_two_var_test(result, test_info$variable, test_info$groups, test_info$alternative))
    }
  }
  
  # Interpretasi One Variance Test (Chi-square)
  interpret_one_var_test <- function(result, variable, sigma0, alternative) {
    alpha <- 0.05
    chi_stat <- as.numeric(result$statistic)
    df <- as.numeric(result$parameter)
    p_value <- as.numeric(result$p.value)
    sample_var <- as.numeric(result$estimate)
    
    # Critical values
    if(alternative == "two.sided") {
      chi_lower <- qchisq(0.025, df)
      chi_upper <- qchisq(0.975, df)
      critical_region <- paste("χ² < ", round(chi_lower, 4), " atau χ² > ", round(chi_upper, 4))
      h1_text <- paste("σ² ≠", sigma0)
    } else if(alternative == "greater") {
      chi_critical <- qchisq(0.95, df)
      critical_region <- paste("χ² >", round(chi_critical, 4))
      h1_text <- paste("σ² >", sigma0)
    } else {
      chi_critical <- qchisq(0.05, df)
      critical_region <- paste("χ² <", round(chi_critical, 4))
      h1_text <- paste("σ² <", sigma0)
    }
    
    decision <- ifelse(p_value < alpha, "TOLAK H₀", "TERIMA H₀")
    
    paste0(
      "INTERPRETASI LENGKAP UJI RAGAM SATU SAMPEL\n",
      "==========================================\n",
      "Variabel: ", variable, "\n",
      "Sample size: ", df + 1, "\n",
      "Sample variance: ", round(sample_var, 4), "\n",
      "Hypothesized variance: ", sigma0, "\n\n",
      
      "1) HIPOTESIS:\n",
      "   H₀: σ² = ", sigma0, " (ragam populasi sama dengan ", sigma0, ")\n",
      "   H₁: ", h1_text, " (ragam populasi ", 
      ifelse(alternative == "two.sided", "tidak sama dengan", 
             ifelse(alternative == "greater", "lebih besar dari", "lebih kecil dari")), " ", sigma0, ")\n\n",
      
      "2) TINGKAT SIGNIFIKANSI:\n",
      "   α = ", alpha, " (5%)\n",
      "   Tingkat kepercayaan = 95%\n\n",
      
      "3) WILAYAH KRITIS:\n",
      "   Distribusi: Chi-square dengan df = ", df, "\n",
      "   Wilayah kritis: ", critical_region, "\n",
      "   Asumsi: Data berdistribusi normal\n\n",
      
      "4) STATISTIK UJI:\n",
      "   χ² = (n-1)s² / σ₀²\n",
      "   χ² = (", df + 1, "-1) × ", round(sample_var, 4), " / ", sigma0, "\n",
      "   χ² = ", df, " × ", round(sample_var, 4), " / ", sigma0, "\n",
      "   χ² = ", round(chi_stat, 4), "\n",
      "   p-value = ", round(p_value, 4), "\n\n",
      
      "5) KEPUTUSAN STATISTIK:\n",
      "   Karena p-value (", round(p_value, 4), ") ", 
      ifelse(p_value < alpha, "<", "≥"), " α (", alpha, "), maka ", decision, "\n",
      "   Chi-square statistic ", 
      ifelse(p_value < alpha, "berada di", "tidak berada di"), " wilayah kritis\n\n",
      
      "6) KESIMPULAN:\n",
      "   Pada tingkat signifikansi 5%, ", 
      ifelse(p_value < alpha, 
             paste("terdapat cukup bukti untuk menyatakan bahwa ragam populasi", variable, 
                   ifelse(alternative == "two.sided", "berbeda signifikan dari", 
                          ifelse(alternative == "greater", "lebih besar dari", "lebih kecil dari")), sigma0),
             paste("tidak terdapat cukup bukti untuk menyatakan bahwa ragam populasi", variable, 
                   ifelse(alternative == "two.sided", "berbeda dari", 
                          ifelse(alternative == "greater", "lebih besar dari", "lebih kecil dari")), sigma0)), ".\n",
      "   Rasio variance: s²/σ₀² = ", round(sample_var/sigma0, 4), "\n\n",
      
      "IMPLIKASI PRAKTIS:\n",
      ifelse(p_value < alpha,
             paste("Variabilitas data (", round(sample_var, 4), ") berbeda secara signifikan dari yang dihipotesiskan (",
                   sigma0, "). Ini menunjukkan bahwa asumsi tentang konsistensi data perlu ditinjau ulang."),
             paste("Variabilitas data konsisten dengan yang dihipotesiskan. Ragam sampel tidak berbeda signifikan dari",
                   sigma0, "."))
    )
  }
  
  # Interpretasi Two Variance Test (F-test)
  interpret_two_var_test <- function(result, variable, groups, alternative) {
    alpha <- 0.05
    f_stat <- as.numeric(result$statistic)
    df1 <- as.numeric(result$parameter[1])
    df2 <- as.numeric(result$parameter[2])
    p_value <- as.numeric(result$p.value)
    var1 <- as.numeric(result$estimate[1])
    var2 <- as.numeric(result$estimate[2])
    var_ratio <- var1 / var2
    
    # Critical values
    if(alternative == "two.sided") {
      f_lower <- qf(0.025, df1, df2)
      f_upper <- qf(0.975, df1, df2)
      critical_region <- paste("F < ", round(f_lower, 4), " atau F > ", round(f_upper, 4))
      h1_text <- "σ₁² ≠ σ₂²"
    } else if(alternative == "greater") {
      f_critical <- qf(0.95, df1, df2)
      critical_region <- paste("F >", round(f_critical, 4))
      h1_text <- "σ₁² > σ₂²"
    } else {
      f_critical <- qf(0.05, df1, df2)
      critical_region <- paste("F <", round(f_critical, 4))
      h1_text <- "σ₁² < σ₂²"
    }
    
    decision <- ifelse(p_value < alpha, "TOLAK H₀", "TERIMA H₀")
    
    paste0(
      "INTERPRETASI LENGKAP UJI RAGAM DUA SAMPEL\n",
      "=========================================\n",
      "Variabel: ", variable, "\n",
      "Kelompok 1 (", groups[1], "): variance = ", round(var1, 4), "\n",
      "Kelompok 2 (", groups[2], "): variance = ", round(var2, 4), "\n",
      "Rasio variance: ", round(var_ratio, 4), "\n\n",
      
      "1) HIPOTESIS:\n",
      "   H₀: σ₁² = σ₂² (ragam kedua kelompok sama)\n",
      "   H₁: ", h1_text, " (ragam kedua kelompok ", 
      ifelse(alternative == "two.sided", "tidak sama", 
             ifelse(alternative == "greater", "kelompok 1 lebih besar", "kelompok 1 lebih kecil")), ")\n\n",
      
      "2) TINGKAT SIGNIFIKANSI:\n",
      "   α = ", alpha, " (5%)\n",
      "   Tingkat kepercayaan = 95%\n\n",
      
      "3) WILAYAH KRITIS:\n",
      "   Distribusi: F dengan df₁ = ", df1, " dan df₂ = ", df2, "\n",
      "   Wilayah kritis: ", critical_region, "\n",
      "   Asumsi: Kedua populasi berdistribusi normal dan independen\n\n",
      
      "4) STATISTIK UJI:\n",
      "   F = s₁² / s₂²\n",
      "   F = ", round(var1, 4), " / ", round(var2, 4), "\n",
      "   F = ", round(f_stat, 4), "\n",
      "   p-value = ", round(p_value, 4), "\n\n",
      
      "5) KEPUTUSAN STATISTIK:\n",
      "   Karena p-value (", round(p_value, 4), ") ", 
      ifelse(p_value < alpha, "<", "≥"), " α (", alpha, "), maka ", decision, "\n\n",
      
      "6) KESIMPULAN:\n",
      "   Pada tingkat signifikansi 5%, ", 
      ifelse(p_value < alpha, 
             paste("terdapat perbedaan ragam yang signifikan antara", groups[1], "dan", groups[2]),
             paste("tidak terdapat perbedaan ragam yang signifikan antara", groups[1], "dan", groups[2])), ".\n",
      "   95% CI untuk rasio variance: [", round(result$conf.int[1], 4), ", ", round(result$conf.int[2], 4), "]\n\n",
      
      "IMPLIKASI PRAKTIS:\n",
      ifelse(p_value < alpha,
             paste("Kelompok", groups[1], 
                   ifelse(var_ratio > 1, "memiliki variabilitas lebih tinggi", "memiliki variabilitas lebih rendah"),
                   "dibanding", groups[2], ". Rasio variance:", round(var_ratio, 4)),
             "Kedua kelompok memiliki variabilitas yang sebanding. Asumsi equal variance terpenuhi.")
    )
  }
  
  # Create ANOVA test interpretation
  create_anova_test_interpretation <- function(test_info) {
    result <- test_info$result
    
    if(test_info$type == "one_way") {
      return(interpret_one_way_anova(result, test_info$dependent, test_info$factor1, test_info$posthoc))
    } else if(test_info$type == "two_way") {
      return(interpret_two_way_anova(result, test_info$dependent, test_info$factor1, test_info$factor2, test_info$interaction, test_info$posthoc))
    }
  }
  
  # Interpretasi One-Way ANOVA
  interpret_one_way_anova <- function(result, dependent, factor1, posthoc) {
    alpha <- 0.05
    anova_table <- result[[1]]
    f_stat <- as.numeric(anova_table$`F value`[1])
    df1 <- as.numeric(anova_table$Df[1])
    df2 <- as.numeric(anova_table$Df[2])
    p_value <- as.numeric(anova_table$`Pr(>F)`[1])
    
    # Critical F value
    f_critical <- qf(0.95, df1, df2)
    
    # Effect size (Eta-squared)
    ss_between <- anova_table$`Sum Sq`[1]
    ss_total <- sum(anova_table$`Sum Sq`)
    eta_squared <- ss_between / ss_total
    
    effect_size_interpretation <- ifelse(eta_squared < 0.01, "sangat kecil",
                                         ifelse(eta_squared < 0.06, "kecil",
                                                ifelse(eta_squared < 0.14, "sedang", "besar")))
    
    decision <- ifelse(p_value < alpha, "TOLAK H₀", "TERIMA H₀")
    
    # Post-hoc summary
    posthoc_summary <- ""
    if(!is.null(posthoc) && p_value < alpha) {
      significant_pairs <- sum(posthoc$significant)
      total_pairs <- nrow(posthoc)
      posthoc_summary <- paste0(
        "\n   Post-hoc analysis: ", significant_pairs, " dari ", total_pairs, 
        " perbandingan berpasangan signifikan"
      )
    }
    
    paste0(
      "INTERPRETASI LENGKAP ANOVA SATU ARAH\n",
      "====================================\n",
      "Variabel Terikat: ", dependent, "\n",
      "Faktor: ", factor1, "\n",
      "Jumlah kelompok: ", df1 + 1, "\n",
      "Total observasi: ", df1 + df2 + 1, "\n\n",
      
      "1) HIPOTESIS:\n",
      "   H₀: μ₁ = μ₂ = μ₃ = ... = μₖ (semua rata-rata kelompok sama)\n",
      "   H₁: Minimal ada satu rata-rata kelompok yang berbeda\n",
      "   dimana k = ", df1 + 1, " kelompok\n\n",
      
      "2) TINGKAT SIGNIFIKANSI:\n",
      "   α = ", alpha, " (5%)\n",
      "   Tingkat kepercayaan = 95%\n\n",
      
      "3) WILAYAH KRITIS:\n",
      "   Distribusi: F dengan df₁ = ", df1, " dan df₂ = ", df2, "\n",
      "   F kritis = ", round(f_critical, 4), "\n",
      "   Wilayah kritis: F > ", round(f_critical, 4), "\n",
      "   Asumsi: Normalitas, homogenitas varians, independensi\n\n",
      
      "4) STATISTIK UJI:\n",
      "   F = MSB / MSW (Mean Square Between / Mean Square Within)\n",
      "   F = ", round(anova_table$`Mean Sq`[1], 4), " / ", round(anova_table$`Mean Sq`[2], 4), "\n",
      "   F = ", round(f_stat, 4), "\n",
      "   p-value = ", round(p_value, 4), "\n\n",
      
      "5) KEPUTUSAN STATISTIK:\n",
      "   Karena p-value (", round(p_value, 4), ") ", 
      ifelse(p_value < alpha, "<", "≥"), " α (", alpha, "), maka ", decision, "\n",
      "   F statistic (", round(f_stat, 4), ") ", 
      ifelse(f_stat > f_critical, ">", "≤"), " F kritis (", round(f_critical, 4), ")\n\n",
      
      "6) KESIMPULAN:\n",
      "   Pada tingkat signifikansi 5%, ", 
      ifelse(p_value < alpha, 
             paste("terdapat perbedaan rata-rata yang signifikan antar kelompok", factor1),
             paste("tidak terdapat perbedaan rata-rata yang signifikan antar kelompok", factor1)), ".\n",
      "   Effect size (η²): ", round(eta_squared, 4), " (", effect_size_interpretation, ")\n",
      "   Proporsi varians dijelaskan oleh ", factor1, ": ", round(eta_squared * 100, 2), "%", 
      posthoc_summary, "\n\n",
      
      "IMPLIKASI PRAKTIS:\n",
      ifelse(p_value < alpha,
             paste("Faktor", factor1, "berpengaruh signifikan terhadap", dependent, ". Effect size", 
                   effect_size_interpretation, "menunjukkan bahwa", round(eta_squared * 100, 2), 
                   "% variasi dalam", dependent, "dapat dijelaskan oleh perbedaan", factor1, "."),
             paste("Faktor", factor1, "tidak berpengaruh signifikan terhadap", dependent, 
                   ". Perbedaan yang diamati kemungkinan disebabkan oleh variasi acak.")),
      ifelse(p_value < alpha && !is.null(posthoc), 
             " Uji post-hoc menunjukkan kelompok mana saja yang berbeda secara signifikan.", "")
    )
  }
  
  # Interpretasi Two-Way ANOVA
  interpret_two_way_anova <- function(result, dependent, factor1, factor2, interaction, posthoc) {
    alpha <- 0.05
    anova_table <- result[[1]]
    
    # Extract effects (order may vary)
    effect_names <- rownames(anova_table)
    effect_names <- effect_names[effect_names != "Residuals"]
    
    decisions <- c()
    interpretations <- c()
    
    for(i in 1:length(effect_names)) {
      if(i <= nrow(anova_table) && !is.na(anova_table$`Pr(>F)`[i])) {
        p_val <- anova_table$`Pr(>F)`[i]
        f_val <- anova_table$`F value`[i]
        df1 <- anova_table$Df[i]
        
        # Find residuals row
        residual_row <- which(rownames(anova_table) == "Residuals")
        if(length(residual_row) > 0) {
          df2 <- anova_table$Df[residual_row]
          f_critical <- qf(0.95, df1, df2)
        } else {
          df2 <- NA
          f_critical <- NA
        }
        
        decision <- ifelse(p_val < alpha, "SIGNIFIKAN", "TIDAK SIGNIFIKAN")
        decisions <- c(decisions, decision)
        
        # Effect size
        if(!is.na(anova_table$`Sum Sq`[i])) {
          ss_effect <- anova_table$`Sum Sq`[i]
          ss_total <- sum(anova_table$`Sum Sq`, na.rm = TRUE)
          eta_squared <- ss_effect / ss_total
          effect_size_interpretation <- ifelse(eta_squared < 0.01, "sangat kecil",
                                               ifelse(eta_squared < 0.06, "kecil",
                                                      ifelse(eta_squared < 0.14, "sedang", "besar")))
        } else {
          eta_squared <- NA
          effect_size_interpretation <- "tidak dapat dihitung"
        }
        
        # Find residuals row for df2
        residual_row <- which(rownames(anova_table) == "Residuals")
        if(length(residual_row) > 0) {
          df2 <- anova_table$Df[residual_row]
        } else {
          # Alternative: calculate total df2 as last row
          df2 <- anova_table$Df[nrow(anova_table)]
        }
        
        # Make sure df2 is valid
        if(is.na(df2) || df2 <= 0) {
          df2 <- sum(anova_table$Df) - sum(anova_table$Df[1:(nrow(anova_table)-1)])
        }
        
        interpretation <- paste0(
          "   ", effect_names[i], ": F(", df1, ",", df2, ") = ", 
          round(f_val, 4), ", p = ", round(p_val, 4), " → ", decision,
          ifelse(!is.na(eta_squared), paste0(" (η² = ", round(eta_squared, 4), ", ", effect_size_interpretation, ")"), "")
        )
        
        interpretations <- c(interpretations, interpretation)
      }
    }
    
    # Overall summary
    significant_effects <- sum(decisions == "SIGNIFIKAN", na.rm = TRUE)
    total_effects <- length(decisions)
    
    paste0(
      "INTERPRETASI LENGKAP ANOVA DUA ARAH\n",
      "===================================\n",
      "Variabel Terikat: ", dependent, "\n",
      "Faktor 1: ", factor1, "\n",
      "Faktor 2: ", factor2, "\n",
      "Interaksi: ", ifelse(interaction, "Dianalisis", "Tidak dianalisis"), "\n\n",
      
      "1) HIPOTESIS:\n",
      "   Main Effect ", factor1, ":\n",
      "     H₀: Tidak ada perbedaan rata-rata antar level ", factor1, "\n",
      "     H₁: Ada perbedaan rata-rata antar level ", factor1, "\n",
      "   Main Effect ", factor2, ":\n",
      "     H₀: Tidak ada perbedaan rata-rata antar level ", factor2, "\n",
      "     H₁: Ada perbedaan rata-rata antar level ", factor2, "\n",
      ifelse(interaction, paste0(
        "   Interaction Effect:\n",
        "     H₀: Tidak ada interaksi antara ", factor1, " dan ", factor2, "\n",
        "     H₁: Ada interaksi antara ", factor1, " dan ", factor2, "\n"), ""), "\n",
      
      "2) TINGKAT SIGNIFIKANSI:\n",
      "   α = ", alpha, " untuk setiap uji\n",
      "   Tingkat kepercayaan = 95%\n\n",
      
      "3) WILAYAH KRITIS:\n",
      "   Distribusi: F dengan df yang berbeda untuk setiap efek\n",
      "   Asumsi: Normalitas, homogenitas varians, independensi\n\n",
      
      "4) STATISTIK UJI:\n",
      paste(interpretations, collapse = "\n"), "\n\n",
      
      "5) KEPUTUSAN STATISTIK:\n",
      "   Efek signifikan: ", significant_effects, " dari ", total_effects, " efek yang diuji\n",
      paste(paste0("   ", effect_names, ": ", decisions), collapse = "\n"), "\n\n",
      
      "6) KESIMPULAN:\n",
      "   Pada tingkat signifikansi 5%:\n",
      paste(paste0("   - ", effect_names, " ", 
                   ifelse(decisions == "SIGNIFIKAN", "berpengaruh signifikan", "tidak berpengaruh signifikan"),
                   " terhadap ", dependent), collapse = "\n"), "\n\n",
      
      "IMPLIKASI PRAKTIS:\n",
      ifelse(significant_effects > 0,
             paste("Terdapat", significant_effects, "efek signifikan yang perlu dipertimbangkan dalam interpretasi.",
                   ifelse(any(grepl(":", effect_names)) && any(decisions[grepl(":", effect_names)] == "SIGNIFIKAN"),
                          " Adanya interaksi signifikan menunjukkan bahwa efek satu faktor bergantung pada level faktor lainnya.",
                          "")),
             paste("Tidak ada efek yang signifikan. Faktor-faktor yang diuji tidak berpengaruh terhadap", dependent, "."))
    )
  }
  
  # ===================================================================
  # FUNGSI VISUALISASI
  # ===================================================================
  
  # Create mean test plot
  create_mean_test_plot <- function(test_info) {
    if(test_info$type == "one_sample") {
      hist_data <- data.frame(value = test_info$data)
      
      p <- ggplot(hist_data, aes(x = value)) +
        geom_histogram(bins = 20, fill = "#dc2626", alpha = 0.7, color = "white") +
        geom_vline(xintercept = mean(test_info$data), color = "#991b1b", linetype = "solid", size = 1) +
        geom_vline(xintercept = test_info$mu0, color = "#059669", linetype = "dashed", size = 1) +
        annotate("text", x = mean(test_info$data), y = Inf, 
                 label = paste("Sample mean:", round(mean(test_info$data), 2)), 
                 vjust = 2, color = "#991b1b", size = 3) +
        annotate("text", x = test_info$mu0, y = Inf, 
                 label = paste("H₀ value:", test_info$mu0), 
                 vjust = 4, color = "#059669", size = 3) +
        theme_minimal() +
        labs(title = paste("One-Sample t-test:", test_info$variable), 
             x = test_info$variable, y = "Frequency")
      
    } else if(test_info$type == "two_sample_indep") {
      plot_data <- test_info$data %>%
        rename(Variable = !!test_info$variable, Group = !!test_info$grouping)
      
      p <- ggplot(plot_data, aes(x = Group, y = Variable, fill = Group)) +
        geom_boxplot(alpha = 0.7) +
        geom_point(position = position_jitter(width = 0.2), alpha = 0.5) +
        stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "white") +
        scale_fill_brewer(type = "qual", palette = "Set2") +
        theme_minimal() +
        theme(legend.position = "none") +
        labs(title = paste("Two-Sample t-test:", test_info$variable, "by", test_info$grouping),
             x = test_info$grouping, y = test_info$variable)
      
    } else if(test_info$type == "paired") {
      paired_long <- test_info$data %>%
        mutate(ID = row_number()) %>%
        pivot_longer(cols = c(!!test_info$var1, !!test_info$var2), 
                     names_to = "Variable", values_to = "Value")
      
      p <- ggplot(paired_long, aes(x = Variable, y = Value)) +
        geom_boxplot(aes(fill = Variable), alpha = 0.7) +
        geom_line(aes(group = ID), alpha = 0.3, color = "gray") +
        geom_point(aes(fill = Variable), shape = 21, size = 2, alpha = 0.7) +
        scale_fill_brewer(type = "qual", palette = "Set2") +
        theme_minimal() +
        theme(legend.position = "none") +
        labs(title = paste("Paired t-test:", test_info$var1, "vs", test_info$var2),
             x = "Variable", y = "Value")
    }
    
    return(p)
  }
  
  # Create proportion test plot
  create_prop_test_plot <- function(test_info) {
    if(test_info$type == "one_prop") {
      # Create bar plot for one proportion
      plot_data <- data.frame(
        Category = c("Success", "Failure"),
        Count = c(test_info$successes, test_info$total - test_info$successes),
        Proportion = c(test_info$successes/test_info$total, 1 - test_info$successes/test_info$total)
      )
      
      p <- ggplot(plot_data, aes(x = Category, y = Count, fill = Category)) +
        geom_col(alpha = 0.8) +
        geom_text(aes(label = paste0(Count, "\n(", round(Proportion*100, 1), "%)")), 
                  vjust = 0.5, color = "white", fontface = "bold") +
        scale_fill_manual(values = c("Success" = "#059669", "Failure" = "#dc2626")) +
        theme_minimal() +
        theme(legend.position = "none") +
        labs(title = paste("One-Proportion Test (p₀ =", test_info$p0, ")"),
             x = "Category", y = "Count") +
        geom_hline(yintercept = test_info$total * test_info$p0, 
                   linetype = "dashed", color = "blue", size = 1) +
        annotate("text", x = 1.5, y = test_info$total * test_info$p0, 
                 label = paste("Expected under H₀:", round(test_info$total * test_info$p0, 1)),
                 vjust = -0.5, color = "blue")
      
    } else if(test_info$type == "two_prop") {
      # Create comparison plot for two proportions
      plot_data <- data.frame(
        Group = rep(test_info$groups, each = 2),
        Category = rep(c("Success", "Failure"), 2),
        Count = c(test_info$successes[1], test_info$totals[1] - test_info$successes[1],
                  test_info$successes[2], test_info$totals[2] - test_info$successes[2])
      )
      
      plot_data$Proportion <- c(
        test_info$successes[1]/test_info$totals[1], 1 - test_info$successes[1]/test_info$totals[1],
        test_info$successes[2]/test_info$totals[2], 1 - test_info$successes[2]/test_info$totals[2]
      )
      
      p <- ggplot(plot_data, aes(x = Group, y = Count, fill = Category)) +
        geom_col(position = "stack", alpha = 0.8) +
        geom_text(aes(label = Count), position = position_stack(vjust = 0.5), 
                  color = "white", fontface = "bold") +
        scale_fill_manual(values = c("Success" = "#059669", "Failure" = "#dc2626")) +
        theme_minimal() +
        labs(title = "Two-Proportion Test Comparison",
             x = "Group", y = "Count", fill = "Category")
    }
    
    return(p)
  }
  
  # Create variance test plot
  create_var_test_plot <- function(test_info) {
    if(test_info$type == "one_var") {
      # Create histogram with variance annotation
      hist_data <- data.frame(value = test_info$data)
      
      p <- ggplot(hist_data, aes(x = value)) +
        geom_histogram(bins = 20, fill = "#dc2626", alpha = 0.7, color = "white") +
        theme_minimal() +
        labs(title = paste("One-Sample Variance Test:", test_info$variable),
             subtitle = paste("Sample var:", round(var(test_info$data), 4), 
                              "| H₀ var:", test_info$sigma0),
             x = test_info$variable, y = "Frequency")
      
    } else if(test_info$type == "two_var") {
      # Create side-by-side boxplot with variance comparison
      plot_data <- test_info$data %>%
        rename(Variable = !!test_info$variable, Group = !!test_info$grouping)
      
      # Calculate variances for annotation
      var_summary <- plot_data %>%
        group_by(Group) %>%
        summarise(Variance = var(Variable, na.rm = TRUE), .groups = 'drop')
      
      p <- ggplot(plot_data, aes(x = Group, y = Variable, fill = Group)) +
        geom_boxplot(alpha = 0.7) +
        geom_point(position = position_jitter(width = 0.2), alpha = 0.5) +
        scale_fill_brewer(type = "qual", palette = "Set2") +
        theme_minimal() +
        theme(legend.position = "none") +
        labs(title = paste("Two-Sample Variance Test:", test_info$variable),
             subtitle = paste("Variance ratio:", round(test_info$result$statistic, 4)),
             x = test_info$grouping, y = test_info$variable) +
        annotate("text", x = 1:nrow(var_summary), y = Inf, 
                 label = paste("Var:", round(var_summary$Variance, 4)),
                 vjust = 2, size = 3)
    }
    
    return(p)
  }
  
  # Create ANOVA test plot
  create_anova_test_plot <- function(test_info) {
    if(test_info$type == "one_way") {
      plot_data <- test_info$data %>%
        rename(Dependent = !!test_info$dependent, Factor = !!test_info$factor1)
      
      p <- ggplot(plot_data, aes(x = Factor, y = Dependent, fill = Factor)) +
        geom_boxplot(alpha = 0.7) +
        geom_point(position = position_jitter(width = 0.2), alpha = 0.5) +
        stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "white") +
        scale_fill_brewer(type = "qual", palette = "Set3") +
        theme_minimal() +
        theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1)) +
        labs(title = paste("One-Way ANOVA:", test_info$dependent, "by", test_info$factor1),
             x = test_info$factor1, y = test_info$dependent)
      
    } else if(test_info$type == "two_way") {
      plot_data <- test_info$data %>%
        rename(Dependent = !!test_info$dependent, 
               Factor1 = !!test_info$factor1,
               Factor2 = !!test_info$factor2)
      
      if(test_info$interaction) {
        # Interaction plot
        p <- ggplot(plot_data, aes(x = Factor1, y = Dependent, color = Factor2, group = Factor2)) +
          stat_summary(fun = mean, geom = "point", size = 3) +
          stat_summary(fun = mean, geom = "line", size = 1) +
          stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.1) +
          scale_color_brewer(type = "qual", palette = "Set2") +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
          labs(title = paste("Two-Way ANOVA with Interaction:", test_info$dependent),
               subtitle = paste(test_info$factor1, "×", test_info$factor2),
               x = test_info$factor1, y = test_info$dependent, color = test_info$factor2)
      } else {
        # Main effects plot
        p <- ggplot(plot_data, aes(x = Factor1, y = Dependent, fill = Factor2)) +
          geom_boxplot(alpha = 0.7) +
          scale_fill_brewer(type = "qual", palette = "Set2") +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
          labs(title = paste("Two-Way ANOVA:", test_info$dependent),
               subtitle = paste(test_info$factor1, "+", test_info$factor2),
               x = test_info$factor1, y = test_info$dependent, fill = test_info$factor2)
      }
    }
    
    return(p)
  }
  
  # T-test
  observeEvent(input$run_ttest, {
    req(input$t_variable, input$mu_value)
    
    tryCatch({
      current_data <- values$processed_data
      
      if(!input$t_variable %in% names(current_data)) {
        showNotification("Variabel tidak ditemukan dalam data", type = "error")
        return()
      }
      
      # Get clean data
      var_data <- current_data[[input$t_variable]]
      var_data_clean <- var_data[!is.na(var_data) & is.finite(var_data)]
      
      if(length(var_data_clean) < 3) {
        showNotification("Data tidak cukup untuk uji t (minimal 3 observasi)", type = "warning")
        return()
      }
      
      # Perform one-sample t-test
      ttest_result <- t.test(var_data_clean, mu = input$mu_value)
      values$ttest_results <- ttest_result
      
      # Display results
      output$ttest_results <- renderText({
        paste0(
          "ONE-SAMPLE T-TEST\n",
          "=================\n",
          "Hypothesis: μ = ", input$mu_value, "\n",
          "Sample mean: ", round(ttest_result$estimate, 4), "\n",
          "t-statistic: ", round(ttest_result$statistic, 4), "\n",
          "df: ", ttest_result$parameter, "\n",
          "p-value: ", round(ttest_result$p.value, 4), "\n",
          "95% Confidence Interval: [", round(ttest_result$conf.int[1], 4), ", ", round(ttest_result$conf.int[2], 4), "]\n",
          "Alternative hypothesis: ", ttest_result$alternative, "\n\n",
          "Decision: ", ifelse(ttest_result$p.value < 0.05, "REJECT H0", "FAIL TO REJECT H0"), "\n",
          "Conclusion: ", ifelse(ttest_result$p.value < 0.05, 
                                 paste("Population mean is significantly different from", input$mu_value),
                                 paste("No significant difference from", input$mu_value))
        )
      })
      
      # Create visualization
      hist_data <- data.frame(value = var_data_clean)
      
      p <- ggplot(hist_data, aes(x = value)) +
        geom_histogram(bins = 30, fill = "#dc2626", alpha = 0.7, color = "white") +
        geom_vline(xintercept = mean(var_data_clean), color = "#991b1b", linetype = "solid", size = 1) +
        geom_vline(xintercept = input$mu_value, color = "#059669", linetype = "dashed", size = 1) +
        annotate("text", x = mean(var_data_clean), y = Inf, 
                 label = paste("Sample mean:", round(mean(var_data_clean), 2)), 
                 vjust = 2, color = "#991b1b") +
        annotate("text", x = input$mu_value, y = Inf, 
                 label = paste("H0 value:", input$mu_value), 
                 vjust = 4, color = "#059669") +
        theme_minimal() +
        labs(title = paste("Distribution of", input$t_variable), 
             x = input$t_variable, y = "Frequency")
      
      output$ttest_plot <- renderPlotly({
        ggplotly(p)
      })
      
      # Generate interpretation
      interpretation <- interpret_ttest(ttest_result, input$t_variable, input$mu_value)
      
      output$ttest_interpretation <- renderText({
        interpretation
      })
      
      showNotification("Uji t selesai", type = "message")
      
    }, error = function(e) {
      showNotification(paste("Error dalam uji t:", e$message), type = "error")
      cat("Error in t-test:", e$message, "\n")
    })
  })
  
  # ANOVA
  observeEvent(input$run_anova, {
    req(input$anova_dependent, input$anova_factor)
    
    tryCatch({
      current_data <- values$processed_data
      
      if(!input$anova_dependent %in% names(current_data)) {
        showNotification("Variabel terikat tidak ditemukan", type = "error")
        return()
      }
      
      if(!input$anova_factor %in% names(current_data)) {
        showNotification("Faktor tidak ditemukan", type = "error")
        return()
      }
      
      # Prepare data
      anova_data <- current_data %>%
        select(!!input$anova_dependent, !!input$anova_factor) %>%
        filter(!is.na(!!sym(input$anova_dependent)), !is.na(!!sym(input$anova_factor)))
      
      if(nrow(anova_data) < 10) {
        showNotification("Data tidak cukup untuk ANOVA", type = "warning")
        return()
      }
      
      # Check if we have at least 2 groups
      n_groups <- length(unique(anova_data[[input$anova_factor]]))
      if(n_groups < 2) {
        showNotification("Diperlukan minimal 2 kelompok untuk ANOVA", type = "warning")
        return()
      }
      
      # Perform ANOVA
      anova_formula <- as.formula(paste(input$anova_dependent, "~", input$anova_factor))
      anova_model <- aov(anova_formula, data = anova_data)
      anova_result <- summary(anova_model)
      
      values$anova_results <- anova_result
      
      # Display ANOVA results
      output$anova_results <- renderText({
        anova_table <- anova_result[[1]]
        
        paste0(
          "ONE-WAY ANOVA\n",
          "=============\n",
          "Dependent Variable: ", input$anova_dependent, "\n",
          "Factor: ", input$anova_factor, "\n",
          "Number of groups: ", n_groups, "\n",
          "Total observations: ", nrow(anova_data), "\n\n",
          "ANOVA Table:\n",
          "Source\t\tDF\tSum Sq\t\tMean Sq\t\tF value\t\tPr(>F)\n",
          input$anova_factor, "\t", anova_table$Df[1], "\t", round(anova_table$`Sum Sq`[1], 2), "\t\t",
          round(anova_table$`Mean Sq`[1], 2), "\t\t", round(anova_table$`F value`[1], 4), "\t\t",
          round(anova_table$`Pr(>F)`[1], 4), "\n",
          "Residuals\t", anova_table$Df[2], "\t", round(anova_table$`Sum Sq`[2], 2), "\t\t",
          round(anova_table$`Mean Sq`[2], 2), "\n\n",
          "Decision: ", ifelse(anova_table$`Pr(>F)`[1] < 0.05, "REJECT H0", "FAIL TO REJECT H0"), "\n",
          "Conclusion: ", ifelse(anova_table$`Pr(>F)`[1] < 0.05, 
                                 "There are significant differences between groups",
                                 "No significant differences between groups")
        )
      })
      
      # Post-hoc test if significant
      if(anova_result[[1]]$`Pr(>F)`[1] < 0.05) {
        tukey_result <- TukeyHSD(anova_model)
        
        # Convert to dataframe for display
        tukey_df <- data.frame(
          comparison = rownames(tukey_result[[1]]),
          diff = round(tukey_result[[1]][, "diff"], 4),
          lwr = round(tukey_result[[1]][, "lwr"], 4),
          upr = round(tukey_result[[1]][, "upr"], 4),
          p_adj = round(tukey_result[[1]][, "p adj"], 4),
          significant = tukey_result[[1]][, "p adj"] < 0.05
        )
        
        values$posthoc_results <- tukey_df
        
        output$posthoc_results <- DT::renderDataTable({
          DT::datatable(tukey_df, options = list(scrollX = TRUE, pageLength = 10),
                        caption = "Tukey HSD Post-hoc Test") %>%
            DT::formatStyle("significant", backgroundColor = DT::styleEqual(
              c(TRUE, FALSE), c("lightcoral", "lightgreen")
            ))
        })
      } else {
        output$posthoc_results <- DT::renderDataTable({
          DT::datatable(data.frame(Message = "Post-hoc test not needed (ANOVA not significant)"),
                        options = list(dom = 't'))
        })
        values$posthoc_results <- NULL
      }
      
      # Create visualization
      p <- ggplot(anova_data, aes_string(x = input$anova_factor, y = input$anova_dependent)) +
        geom_boxplot(fill = "#dc2626", alpha = 0.7, outlier.color = "#991b1b") +
        geom_point(position = position_jitter(width = 0.2), alpha = 0.5, color = "#059669") +
        stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "white", color = "black") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        labs(title = paste("ANOVA:", input$anova_dependent, "by", input$anova_factor),
             x = input$anova_factor, y = input$anova_dependent)
      
      output$anova_plot <- renderPlotly({
        ggplotly(p)
      })
      
      # Generate interpretation
      interpretation <- interpret_anova(anova_result[[1]], values$posthoc_results, 
                                        input$anova_dependent, input$anova_factor)
      
      output$anova_interpretation <- renderText({
        interpretation
      })
      
      showNotification("ANOVA selesai", type = "message")
      
    }, error = function(e) {
      showNotification(paste("Error dalam ANOVA:", e$message), type = "error")
      cat("Error in ANOVA:", e$message, "\n")
    })
  })
  
  # ===================================================================
  # REGRESI SECTION
  # ===================================================================
  
  # Load data untuk regresi
  observeEvent(input$load_regresi_data, {
    tryCatch({
      if(input$regresi_data_source == "processed" && !is.null(values$processed_data)) {
        values$regresi_data <- values$processed_data
        source_text <- "Data dari Manajemen Data"
      } else if(input$regresi_data_source == "upload" && !is.null(input$regresi_data_file)) {
        values$regresi_data <- load_data_from_file(input$regresi_data_file$datapath)
        source_text <- paste("File upload:", input$regresi_data_file$name)
      } else {
        showNotification("Pilih sumber data terlebih dahulu", type = "warning")
        return()
      }
      
      if(!is.null(values$regresi_data)) {
        # Update variable choices
        numeric_cols <- names(values$regresi_data)[sapply(values$regresi_data, is.numeric)]
        
        # Update choices untuk regresi
        updateSelectInput(session, "reg_dependent", choices = numeric_cols, 
                          selected = if(length(numeric_cols) > 0) numeric_cols[1] else NULL)
        updateSelectizeInput(session, "reg_predictors", choices = numeric_cols,
                             selected = if(length(numeric_cols) > 1) numeric_cols[2:min(4, length(numeric_cols))] else NULL)
        
        showNotification(paste("Data berhasil dimuat:", source_text), type = "message")
      }
    }, error = function(e) {
      showNotification(paste("Error loading data:", e$message), type = "error")
    })
  })
  
  # Data info untuk regresi
  output$regresi_data_info <- renderText({
    if(input$regresi_data_source == "processed" && !is.null(values$processed_data)) {
      paste0("Data Manajemen: ", nrow(values$processed_data), " obs, ", 
             ncol(values$processed_data), " vars")
    } else if(input$regresi_data_source == "upload" && !is.null(input$regresi_data_file)) {
      paste0("File: ", input$regresi_data_file$name, "\n",
             "Size: ", round(input$regresi_data_file$size/1024, 2), " KB")
    } else {
      "Belum ada data yang dipilih"
    }
  })
  
  # Preview data untuk regresi
  output$regresi_data_preview <- DT::renderDataTable({
    if(!is.null(values$regresi_data)) {
      DT::datatable(values$regresi_data, options = list(scrollX = TRUE, pageLength = 10),
                    caption = paste("Preview Data - Total:", nrow(values$regresi_data), "observasi"))
    } else {
      DT::datatable(data.frame(Message = "Load data terlebih dahulu"))
    }
  })
  
  # Model Selection Logic
  observe({
    if(!is.null(values$regresi_data)) {
      numeric_cols <- names(values$regresi_data)[sapply(values$regresi_data, is.numeric)]
      
      updateSelectInput(session, "sel_dependent", choices = numeric_cols, 
                        selected = if(length(numeric_cols) > 0) numeric_cols[1] else NULL)
      updateSelectizeInput(session, "sel_predictors", choices = numeric_cols,
                           selected = if(length(numeric_cols) > 1) numeric_cols[2:min(6, length(numeric_cols))] else NULL)
    }
  })
  
  output$candidate_info <- renderText({
    if(is.null(input$sel_predictors) || length(input$sel_predictors) == 0) {
      "Pilih variabel prediktor"
    } else {
      paste0("Kandidat prediktor: ", length(input$sel_predictors), " variabel\n",
             "Total model yang akan dievaluasi: ", 2^length(input$sel_predictors) - 1, " model\n",
             "Metode: ", switch(input$sel_method,
                                "backward" = "Backward Elimination",
                                "forward" = "Forward Selection", 
                                "both" = "Stepwise (Both)"))
    }
  })
  
  # Run comprehensive model selection
  observeEvent(input$run_model_selection, {
    req(input$sel_dependent, input$sel_predictors, values$regresi_data)
    
    tryCatch({
      current_data <- values$regresi_data
      
      # Prepare data
      all_vars <- c(input$sel_dependent, input$sel_predictors)
      model_data <- current_data %>%
        select(all_of(all_vars)) %>%
        na.omit()
      
      if(nrow(model_data) < length(input$sel_predictors) + 5) {
        showNotification("Data tidak cukup untuk seleksi model", type = "warning")
        return()
      }
      
      # Create full and null models
      full_formula <- as.formula(paste(input$sel_dependent, "~", paste(input$sel_predictors, collapse = " + ")))
      null_formula <- as.formula(paste(input$sel_dependent, "~ 1"))
      
      full_model <- lm(full_formula, data = model_data)
      null_model <- lm(null_formula, data = model_data)
      
      # Perform stepwise selection for different methods
      models_list <- list()
      
      # Backward
      model_backward <- step(full_model, direction = "backward", trace = 0)
      models_list[["Backward"]] <- model_backward
      
      # Forward  
      model_forward <- step(null_model, scope = list(upper = full_formula, lower = null_formula),
                            direction = "forward", trace = 0)
      models_list[["Forward"]] <- model_forward
      
      # Stepwise
      model_stepwise <- step(full_model, scope = list(upper = full_formula, lower = null_formula),
                             direction = "both", trace = 0)
      models_list[["Stepwise"]] <- model_stepwise
      
      # Add full model for comparison
      models_list[["Full Model"]] <- full_model
      
      # Calculate all criteria for each model
      calculate_criteria <- function(model) {
        n <- nobs(model)
        p <- length(model$coefficients)
        
        # Basic metrics
        r_squared <- summary(model)$r.squared
        adj_r_squared <- summary(model)$adj.r.squared
        mse <- mean(residuals(model)^2)
        rmse <- sqrt(mse)
        
        # Information criteria
        aic_val <- AIC(model)
        bic_val <- BIC(model)  # Same as SBC
        
        # PRESS (Prediction Error Sum of Squares)
        press_val <- tryCatch({
          hat_vals <- hatvalues(model)
          residuals_val <- residuals(model)
          press_residuals <- residuals_val / (1 - hat_vals)
          sum(press_residuals^2, na.rm = TRUE)
        }, error = function(e) NA)
        
        # Cp Mallows
        cp_val <- tryCatch({
          sigma2_full <- summary(full_model)$sigma^2
          (sum(residuals(model)^2) + 2 * p * sigma2_full) / sigma2_full - n + 2 * p
        }, error = function(e) NA)
        
        return(list(
          Variables = p - 1,  # Exclude intercept
          R_squared = r_squared,
          Adj_R_squared = adj_r_squared,
          MSE = mse,
          RMSE = rmse,
          AIC = aic_val,
          BIC = bic_val,
          PRESS = press_val,
          Cp_Mallows = cp_val
        ))
      }
      
      # Calculate criteria for all models
      criteria_results <- data.frame()
      
      for(method_name in names(models_list)) {
        model <- models_list[[method_name]]
        criteria <- calculate_criteria(model)
        criteria$Method <- method_name
        criteria$Formula <- deparse(formula(model))
        
        criteria_df <- data.frame(
          Method = method_name,
          Formula = deparse(formula(model)),
          Variables = criteria$Variables,
          R_squared = criteria$R_squared,
          Adj_R_squared = criteria$Adj_R_squared,
          MSE = criteria$MSE,
          RMSE = criteria$RMSE,
          AIC = criteria$AIC,
          BIC = criteria$BIC,
          PRESS = criteria$PRESS,
          Cp_Mallows = criteria$Cp_Mallows,
          stringsAsFactors = FALSE
        )
        
        criteria_results <- rbind(criteria_results, criteria_df)
      }
      # Reorder columns
      criteria_results <- criteria_results %>%
        select(Method, Formula, Variables, R_squared, Adj_R_squared, MSE, RMSE, AIC, BIC, PRESS, Cp_Mallows)
      
      values$model_selection_results <- criteria_results
      
      # Determine best model for each criterion
      best_models <- data.frame(
        Criterion = c("MSE", "R_squared", "Adj_R_squared", "AIC", "BIC", "PRESS", "Cp_Mallows"),
        Best_Method = c(
          criteria_results$Method[which.min(criteria_results$MSE)],
          criteria_results$Method[which.max(criteria_results$R_squared)],
          criteria_results$Method[which.max(criteria_results$Adj_R_squared)],
          criteria_results$Method[which.min(criteria_results$AIC)],
          criteria_results$Method[which.min(criteria_results$BIC)],
          criteria_results$Method[which.min(criteria_results$PRESS)],
          criteria_results$Method[which.min(criteria_results$Cp_Mallows)]
        ),
        Best_Value = c(
          min(criteria_results$MSE, na.rm = TRUE),
          max(criteria_results$R_squared, na.rm = TRUE),
          max(criteria_results$Adj_R_squared, na.rm = TRUE),
          min(criteria_results$AIC, na.rm = TRUE),
          min(criteria_results$BIC, na.rm = TRUE),
          min(criteria_results$PRESS, na.rm = TRUE),
          min(criteria_results$Cp_Mallows, na.rm = TRUE)
        ),
        Objective = c("Minimize", "Maximize", "Maximize", "Minimize", "Minimize", "Minimize", "Minimize")
      )
      
      # Display results
      output$best_models_table <- DT::renderDataTable({
        DT::datatable(best_models, options = list(scrollX = TRUE, pageLength = 7),
                      caption = "Model Terbaik per Kriteria") %>%
          DT::formatRound(columns = "Best_Value", digits = 4)
      })
      
      output$all_criteria_table <- DT::renderDataTable({
        DT::datatable(criteria_results, options = list(scrollX = TRUE, pageLength = 5),
                      caption = "Perbandingan Semua Model dan Kriteria") %>%
          DT::formatRound(columns = c("R_squared", "Adj_R_squared", "MSE", "RMSE", "AIC", "BIC", "PRESS", "Cp_Mallows"), digits = 4)
      })
      
      # Determine overall recommended model (majority vote)
      method_votes <- table(best_models$Best_Method)
      recommended_method <- names(method_votes)[which.max(method_votes)]
      recommended_model <- models_list[[recommended_method]]
      
      values$selected_best_model <- recommended_model
      values$recommended_formula <- formula(recommended_model)
      
      output$recommended_model <- renderText({
        paste0(
          "MODEL YANG DIREKOMENDASIKAN\n",
          "============================\n",
          "Metode: ", recommended_method, "\n",
          "Formula: ", deparse(values$recommended_formula), "\n",
          "Variabel: ", length(recommended_model$coefficients) - 1, "\n",
          "R-squared: ", round(summary(recommended_model)$r.squared, 4), "\n",
          "Adj R-squared: ", round(summary(recommended_model)$adj.r.squared, 4), "\n",
          "AIC: ", round(AIC(recommended_model), 2), "\n",
          "BIC: ", round(BIC(recommended_model), 2), "\n\n",
          "Alasan: Model ini dipilih berdasarkan mayoritas kriteria\n",
          "Votes: ", paste(names(method_votes), method_votes, sep = ":", collapse = ", ")
        )
      })
      
      # Visualization  
      viz_data <- criteria_results[, c("Method", "AIC", "BIC", "MSE", "Adj_R_squared")]
      
      # Manual pivot_longer equivalent
      criteria_long <- data.frame()
      for(criterion in c("AIC", "BIC", "MSE", "Adj_R_squared")) {
        temp_df <- data.frame(
          Method = viz_data$Method,
          Criterion = criterion,
          Value = viz_data[[criterion]],
          stringsAsFactors = FALSE
        )
        criteria_long <- rbind(criteria_long, temp_df)
      }
      
      p <- ggplot(criteria_long, aes(x = Method, y = Value, fill = Method)) +
        geom_col(alpha = 0.8) +
        facet_wrap(~Criterion, scales = "free_y") +
        scale_fill_brewer(type = "qual", palette = "Set2") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none") +
        labs(title = "Perbandingan Kriteria Seleksi Model", x = "Metode", y = "Nilai")
      
      output$criteria_comparison_plot <- renderPlotly({
        ggplotly(p)
      })
      
      # Interpretation
      output$selection_complete_interpretation <- renderText({
        n_backward <- length(model_backward$coefficients) - 1
        n_forward <- length(model_forward$coefficients) - 1
        n_stepwise <- length(model_stepwise$coefficients) - 1
        n_full <- length(full_model$coefficients) - 1
        
        paste0(
          "INTERPRETASI LENGKAP SELEKSI MODEL\n",
          "===================================\n\n",
          
          "1. PERBANDINGAN METODE:\n",
          "Backward: ", n_backward, " variabel (AIC: ", round(AIC(model_backward), 2), ")\n",
          "Forward: ", n_forward, " variabel (AIC: ", round(AIC(model_forward), 2), ")\n",
          "Stepwise: ", n_stepwise, " variabel (AIC: ", round(AIC(model_stepwise), 2), ")\n",
          "Full Model: ", n_full, " variabel (AIC: ", round(AIC(full_model), 2), ")\n\n",
          
          "2. KRITERIA TERBAIK:\n",
          paste(apply(best_models, 1, function(x) 
            paste0(x[1], ": ", x[2], " (", round(as.numeric(x[3]), 4), ")")), collapse = "\n"), "\n\n",
          
          "3. REKOMENDASI FINAL:\n",
          "Model ", recommended_method, " dipilih sebagai model terbaik\n",
          "Formula: ", deparse(values$recommended_formula), "\n\n",
          
          "4. LANGKAH SELANJUTNYA:\n",
          "✓ Klik 'Gunakan Model Terpilih untuk Building' untuk melanjutkan\n",
          "✓ Model akan otomatis digunakan di tab Model Building\n",
          "✓ Lakukan uji asumsi untuk validasi model"
        )
      })
      
      showNotification("Seleksi model selesai! Model terbaik telah ditentukan.", type = "message")
      
    }, error = function(e) {
      showNotification(paste("Error dalam seleksi model:", e$message), type = "error")
      cat("Error in model selection:", e$message, "\n")
    })
  })
  
  # Apply selected model to Model Building
  observeEvent(input$apply_selected_model, {
    req(values$recommended_formula, values$selected_best_model)
    
    tryCatch({
      # Extract variables from recommended formula
      formula_vars <- all.vars(values$recommended_formula)
      dependent_var <- formula_vars[1]
      predictor_vars <- formula_vars[-1]
      
      # Update Model Building inputs
      updateSelectInput(session, "reg_dependent", selected = dependent_var)
      updateSelectizeInput(session, "reg_predictors", selected = predictor_vars)
      
      # Store the model directly
      values$regression_model <- values$selected_best_model
      
      # Update Model Building display
      output$model_summary <- renderText({
        model_summary <- summary(values$selected_best_model)
        
        # Format coefficients table
        coeffs <- model_summary$coefficients
        coeff_text <- "\nCOEFFICIENTS:\n"
        coeff_text <- paste0(coeff_text, sprintf("%-15s %10s %10s %10s %10s\n", 
                                                 "Variable", "Estimate", "Std.Error", "t value", "Pr(>|t|)"))
        coeff_text <- paste0(coeff_text, paste(rep("-", 70), collapse = ""), "\n")
        
        for(i in 1:nrow(coeffs)) {
          stars <- ifelse(coeffs[i, 4] < 0.001, "***",
                          ifelse(coeffs[i, 4] < 0.01, "**",
                                 ifelse(coeffs[i, 4] < 0.05, "*",
                                        ifelse(coeffs[i, 4] < 0.1, ".", ""))))
          
          coeff_text <- paste0(coeff_text, sprintf("%-15s %10.4f %10.4f %10.4f %10.4f %s\n",
                                                   rownames(coeffs)[i], coeffs[i, 1], coeffs[i, 2], 
                                                   coeffs[i, 3], coeffs[i, 4], stars))
        }
        
        paste0(
          "MULTIPLE LINEAR REGRESSION (FROM SELECTION)\n",
          "============================================\n",
          "Formula: ", deparse(values$recommended_formula), "\n",
          "Observations: ", nobs(values$selected_best_model), "\n",
          "R-squared: ", round(model_summary$r.squared, 4), 
          " (", round(model_summary$r.squared * 100, 1), "%)\n",
          "Adjusted R-squared: ", round(model_summary$adj.r.squared, 4), "\n",
          "F-statistic: ", round(model_summary$fstatistic[1], 3), 
          " on ", model_summary$fstatistic[2], " and ", model_summary$fstatistic[3], " DF\n",
          "p-value: ", round(pf(model_summary$fstatistic[1], model_summary$fstatistic[2], 
                                model_summary$fstatistic[3], lower.tail = FALSE), 6), "\n",
          coeff_text, "\n",
          "Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1\n\n",
          "✓ Model ini berasal dari hasil seleksi model terbaik"
        )
      })
      
      # Generate model interpretation
      interpretation <- interpret_regression(values$selected_best_model, NULL)
      
      output$model_interpretation <- renderText({
        paste0(interpretation, "\n\n",
               "CATATAN: Model ini dipilih berdasarkan seleksi model terbaik dengan kriteria AIC, BIC, MSE, R², dan lainnya.")
      })
      
      showNotification("Model terpilih berhasil diterapkan ke Model Building!", type = "message")
      
    }, error = function(e) {
      showNotification(paste("Error applying selected model:", e$message), type = "error")
    })
  })
  
  # Download handlers for model selection
  output$download_selection_results <- downloadHandler(
    filename = function() paste("model_selection_results_", Sys.Date(), ".xlsx", sep = ""),
    content = function(file) {
      if(!is.null(values$model_selection_results)) {
        tryCatch({
          wb <- openxlsx::createWorkbook()
          
          # Sheet 1: All Models Comparison
          openxlsx::addWorksheet(wb, "Model_Comparison")
          openxlsx::writeData(wb, "Model_Comparison", values$model_selection_results)
          
          # Sheet 2: Best Models per Criteria
          if(!is.null(values$model_selection_results)) {
            criteria_results <- values$model_selection_results
            
            best_models <- data.frame(
              Criterion = c("MSE", "R_squared", "Adj_R_squared", "AIC", "BIC", "PRESS", "Cp_Mallows"),
              Best_Method = c(
                criteria_results$Method[which.min(criteria_results$MSE)],
                criteria_results$Method[which.max(criteria_results$R_squared)],
                criteria_results$Method[which.max(criteria_results$Adj_R_squared)],
                criteria_results$Method[which.min(criteria_results$AIC)],
                criteria_results$Method[which.min(criteria_results$BIC)],
                criteria_results$Method[which.min(criteria_results$PRESS)],
                criteria_results$Method[which.min(criteria_results$Cp_Mallows)]
              ),
              Best_Value = c(
                min(criteria_results$MSE, na.rm = TRUE),
                max(criteria_results$R_squared, na.rm = TRUE),
                max(criteria_results$Adj_R_squared, na.rm = TRUE),
                min(criteria_results$AIC, na.rm = TRUE),
                min(criteria_results$BIC, na.rm = TRUE),
                min(criteria_results$PRESS, na.rm = TRUE),
                min(criteria_results$Cp_Mallows, na.rm = TRUE)
              ),
              Objective = c("Minimize", "Maximize", "Maximize", "Minimize", "Minimize", "Minimize", "Minimize"),
              stringsAsFactors = FALSE
            )
            
            openxlsx::addWorksheet(wb, "Best_Models")
            openxlsx::writeData(wb, "Best_Models", best_models)
          }
          
          # Sheet 3: Recommended Model Details
          if(!is.null(values$selected_best_model)) {
            recommended_summary <- summary(values$selected_best_model)
            
            model_details <- data.frame(
              Aspect = c("Formula", "Variables", "Observations", "R-squared", "Adj R-squared", 
                         "F-statistic", "p-value", "AIC", "BIC", "RMSE"),
              Value = c(
                deparse(values$recommended_formula),
                length(values$selected_best_model$coefficients) - 1,
                nobs(values$selected_best_model),
                round(recommended_summary$r.squared, 6),
                round(recommended_summary$adj.r.squared, 6),
                round(recommended_summary$fstatistic[1], 4),
                format.pval(pf(recommended_summary$fstatistic[1], 
                               recommended_summary$fstatistic[2], 
                               recommended_summary$fstatistic[3], 
                               lower.tail = FALSE), digits = 6),
                round(AIC(values$selected_best_model), 4),
                round(BIC(values$selected_best_model), 4),
                round(sqrt(mean(residuals(values$selected_best_model)^2)), 6)
              ),
              stringsAsFactors = FALSE
            )
            
            openxlsx::addWorksheet(wb, "Recommended_Model")
            openxlsx::writeData(wb, "Recommended_Model", model_details)
            
            # Add coefficients
            coeffs_table <- data.frame(
              Variable = rownames(recommended_summary$coefficients),
              Estimate = recommended_summary$coefficients[, "Estimate"],
              Std_Error = recommended_summary$coefficients[, "Std. Error"],
              t_value = recommended_summary$coefficients[, "t value"],
              p_value = recommended_summary$coefficients[, "Pr(>|t|)"],
              Significant = recommended_summary$coefficients[, "Pr(>|t|)"] < 0.05,
              stringsAsFactors = FALSE
            )
            
            openxlsx::addWorksheet(wb, "Model_Coefficients")
            openxlsx::writeData(wb, "Model_Coefficients", coeffs_table)
          }
          
          # Sheet 4: Selection Summary
          selection_summary <- data.frame(
            Info = c("Selection Date", "Dependent Variable", "Candidate Predictors", "Selection Method", 
                     "Total Models Evaluated", "Recommended Method", "Recommended Formula"),
            Detail = c(
              as.character(Sys.Date()),
              input$sel_dependent,
              paste(input$sel_predictors, collapse = ", "),
              switch(input$sel_method,
                     "backward" = "Backward Elimination",
                     "forward" = "Forward Selection", 
                     "both" = "Stepwise (Both)"),
              if(!is.null(values$model_selection_results)) nrow(values$model_selection_results) else "NA",
              if(!is.null(values$selected_best_model)) {
                # Determine recommended method
                if(!is.null(best_models)) {
                  method_votes <- table(best_models$Best_Method)
                  names(method_votes)[which.max(method_votes)]
                } else "NA"
              } else "NA",
              if(!is.null(values$recommended_formula)) deparse(values$recommended_formula) else "NA"
            ),
            stringsAsFactors = FALSE
          )
          
          openxlsx::addWorksheet(wb, "Selection_Summary")
          openxlsx::writeData(wb, "Selection_Summary", selection_summary)
          
          openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
          
        }, error = function(e) {
          # If Excel creation fails, create simple CSV
          write.csv(values$model_selection_results, file = gsub("\\.xlsx$", ".csv", file), row.names = FALSE)
        })
      } else {
        # Create empty file with error message
        openxlsx::write.xlsx(data.frame(Message = "Model selection results not available"), file)
      }
    }
  )
  
  # Download comprehensive PDF report
  output$download_selection_report <- downloadHandler(
    filename = function() paste("model_selection_report_", Sys.Date(), ".pdf", sep = ""),
    content = function(file) {
      if(!is.null(values$model_selection_results)) {
        tryCatch({
          library(gridExtra)
          
          # Create header
          header_text <- paste0(
            "LAPORAN LENGKAP SELEKSI MODEL SIGABISA\n",
            "======================================\n",
            "Tanggal: ", format(Sys.Date(), "%d %B %Y"), "\n",
            "Variabel Terikat: ", input$sel_dependent, "\n",
            "Kandidat Prediktor: ", paste(input$sel_predictors, collapse = ", "), "\n",
            "Metode Seleksi: ", switch(input$sel_method,
                                       "backward" = "Backward Elimination",
                                       "forward" = "Forward Selection", 
                                       "both" = "Stepwise (Both)"), "\n",
            paste(rep("=", 80), collapse = "")
          )
          
          header_grob <- gridExtra::tableGrob(
            data.frame(Header = header_text),
            theme = gridExtra::ttheme_minimal(
              base_size = 10,
              core = list(fg_params = list(hjust = 0, x = 0.02)),
              colhead = list(fg_params = list(hjust = 0, x = 0.02))
            ),
            rows = NULL, cols = NULL
          )
          
          # Create criteria comparison plot
          viz_data <- values$model_selection_results[, c("Method", "AIC", "BIC", "MSE", "Adj_R_squared")]
          
          criteria_long <- data.frame()
          for(criterion in c("AIC", "BIC", "MSE", "Adj_R_squared")) {
            temp_df <- data.frame(
              Method = viz_data$Method,
              Criterion = criterion,
              Value = viz_data[[criterion]],
              stringsAsFactors = FALSE
            )
            criteria_long <- rbind(criteria_long, temp_df)
          }
          
          criteria_plot <- ggplot(criteria_long, aes(x = Method, y = Value, fill = Method)) +
            geom_col(alpha = 0.8) +
            facet_wrap(~Criterion, scales = "free_y") +
            scale_fill_brewer(type = "qual", palette = "Set2") +
            theme_minimal() +
            theme(axis.text.x = element_text(angle = 45, hjust = 1), 
                  legend.position = "none",
                  strip.text = element_text(size = 10, face = "bold")) +
            labs(title = "Perbandingan Kriteria Seleksi Model", x = "Metode", y = "Nilai")
          
          # Create summary table
          summary_table_data <- values$model_selection_results[, c("Method", "Variables", "R_squared", "Adj_R_squared", "AIC", "BIC", "MSE")]
          
          # Format for better display
          summary_display <- data.frame(
            Method = summary_table_data$Method,
            Vars = summary_table_data$Variables,
            R2 = round(summary_table_data$R_squared, 4),
            AdjR2 = round(summary_table_data$Adj_R_squared, 4),
            AIC = round(summary_table_data$AIC, 2),
            BIC = round(summary_table_data$BIC, 2),
            MSE = round(summary_table_data$MSE, 6),
            stringsAsFactors = FALSE
          )
          
          summary_table_grob <- gridExtra::tableGrob(
            summary_display,
            theme = gridExtra::ttheme_default(
              base_size = 9,
              core = list(fg_params = list(hjust = 0.5)),
              colhead = list(fg_params = list(hjust = 0.5, fontface = "bold"))
            ),
            rows = NULL
          )
          
          # Create interpretation text
          if(!is.null(values$selected_best_model)) {
            criteria_results <- values$model_selection_results
            
            best_models <- data.frame(
              Criterion = c("MSE", "R_squared", "Adj_R_squared", "AIC", "BIC", "PRESS", "Cp_Mallows"),
              Best_Method = c(
                criteria_results$Method[which.min(criteria_results$MSE)],
                criteria_results$Method[which.max(criteria_results$R_squared)],
                criteria_results$Method[which.max(criteria_results$Adj_R_squared)],
                criteria_results$Method[which.min(criteria_results$AIC)],
                criteria_results$Method[which.min(criteria_results$BIC)],
                criteria_results$Method[which.min(criteria_results$PRESS)],
                criteria_results$Method[which.min(criteria_results$Cp_Mallows)]
              ),
              stringsAsFactors = FALSE
            )
            
            method_votes <- table(best_models$Best_Method)
            recommended_method <- names(method_votes)[which.max(method_votes)]
            
            interpretation_text <- paste0(
              "INTERPRETASI HASIL SELEKSI MODEL\n\n",
              "1. MODEL YANG DIEVALUASI:\n",
              paste(apply(summary_display, 1, function(x) 
                paste0("   ", x[1], ": ", x[2], " variabel (AIC: ", x[5], ", R²: ", x[3], ")")), collapse = "\n"), "\n\n",
              
              "2. MODEL TERBAIK PER KRITERIA:\n",
              paste(apply(best_models, 1, function(x) 
                paste0("   ", x[1], ": ", x[2])), collapse = "\n"), "\n\n",
              
              "3. REKOMENDASI FINAL:\n",
              "   Model ", recommended_method, " dipilih sebagai model terbaik\n",
              "   Formula: ", deparse(values$recommended_formula), "\n",
              "   Alasan: Unggul di ", max(method_votes), " dari 7 kriteria evaluasi\n\n",
              
              "4. KESIMPULAN:\n",
              "   Model terpilih memberikan keseimbangan terbaik antara\n",
              "   akurasi prediksi dan parsimony (kesederhanaan model).\n",
              "   Siap untuk digunakan dalam analisis selanjutnya."
            )
          } else {
            interpretation_text <- "Model selection results not available for interpretation."
          }
          
          interpretation_lines <- unlist(strsplit(interpretation_text, "\n"))
          interpretation_formatted <- data.frame(
            Interpretasi = interpretation_lines[interpretation_lines != ""]
          )
          
          interpretation_grob <- gridExtra::tableGrob(
            interpretation_formatted,
            theme = gridExtra::ttheme_minimal(
              base_size = 9,
              core = list(
                fg_params = list(hjust = 0, x = 0.02),
                bg_params = list(fill = "white")
              ),
              colhead = list(
                fg_params = list(hjust = 0, x = 0.02, fontface = "bold"),
                bg_params = list(fill = "lightgray")
              )
            ),
            rows = NULL
          )
          
          # Combine all elements
          final_report <- gridExtra::grid.arrange(
            header_grob,
            criteria_plot,
            summary_table_grob,
            interpretation_grob,
            ncol = 1,
            heights = c(1, 3, 1.5, 2.5)
          )
          
          ggsave(file, final_report, width = 14, height = 18, device = "pdf", dpi = 300)
          
        }, error = function(e) {
          # If PDF creation fails, create text file
          text_file <- gsub("\\.pdf$", ".txt", file)
          
          summary_text <- paste0(
            "MODEL SELECTION REPORT\n",
            "======================\n",
            "Date: ", Sys.Date(), "\n",
            "Dependent Variable: ", input$sel_dependent, "\n",
            "Predictors: ", paste(input$sel_predictors, collapse = ", "), "\n\n",
            "ERROR: Could not generate PDF report\n",
            "Error message: ", e$message, "\n\n",
            "Please check the Excel download for detailed results."
          )
          
          writeLines(summary_text, text_file)
          file.rename(text_file, file)
        })
      } else {
        # Create simple text file if no results
        writeLines("Model selection results not available", file)
      }
    }
  )
  
  # Build regression model
  observeEvent(input$build_model, {
    req(input$reg_dependent, input$reg_predictors)
    
    tryCatch({
      current_data <- values$processed_data
      
      # Check if all variables exist
      all_vars <- c(input$reg_dependent, input$reg_predictors)
      missing_vars <- setdiff(all_vars, names(current_data))
      
      if(length(missing_vars) > 0) {
        showNotification(paste("Variabel tidak ditemukan:", paste(missing_vars, collapse = ", ")), type = "error")
        return()
      }
      
      # Prepare data
      model_data <- current_data %>%
        select(all_of(all_vars)) %>%
        na.omit()  # Remove rows with any missing values
      
      if(nrow(model_data) < length(input$reg_predictors) + 5) {
        showNotification("Data tidak cukup untuk membangun model regresi", type = "warning")
        return()
      }
      
      # Build regression formula
      formula_str <- paste(input$reg_dependent, "~", paste(input$reg_predictors, collapse = " + "))
      reg_formula <- as.formula(formula_str)
      
      # Fit model
      regression_model <- lm(reg_formula, data = model_data)
      values$regression_model <- regression_model
      
      # Display model summary
      output$model_summary <- renderText({
        model_summary <- summary(regression_model)
        
        # Format coefficients table
        coeffs <- model_summary$coefficients
        coeff_text <- "\nCOEFFICIENTS:\n"
        coeff_text <- paste0(coeff_text, sprintf("%-15s %10s %10s %10s %10s\n", 
                                                 "Variable", "Estimate", "Std.Error", "t value", "Pr(>|t|)"))
        coeff_text <- paste0(coeff_text, paste(rep("-", 70), collapse = ""), "\n")
        
        for(i in 1:nrow(coeffs)) {
          stars <- ifelse(coeffs[i, 4] < 0.001, "***",
                          ifelse(coeffs[i, 4] < 0.01, "**",
                                 ifelse(coeffs[i, 4] < 0.05, "*",
                                        ifelse(coeffs[i, 4] < 0.1, ".", ""))))
          
          coeff_text <- paste0(coeff_text, sprintf("%-15s %10.4f %10.4f %10.4f %10.4f %s\n",
                                                   rownames(coeffs)[i], coeffs[i, 1], coeffs[i, 2], 
                                                   coeffs[i, 3], coeffs[i, 4], stars))
        }
        
        paste0(
          "MULTIPLE LINEAR REGRESSION\n",
          "==========================\n",
          "Formula: ", formula_str, "\n",
          "Observations: ", nobs(regression_model), "\n",
          "R-squared: ", round(model_summary$r.squared, 4), "\n",
          "Adjusted R-squared: ", round(model_summary$adj.r.squared, 4), "\n",
          "F-statistic: ", round(model_summary$fstatistic[1], 4), 
          " on ", model_summary$fstatistic[2], " and ", model_summary$fstatistic[3], " DF\n",
          "p-value: ", round(pf(model_summary$fstatistic[1], model_summary$fstatistic[2], 
                                model_summary$fstatistic[3], lower.tail = FALSE), 6), "\n",
          coeff_text, "\n",
          "Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1"
        )
      })
      
      # Generate model interpretation
      interpretation <- interpret_regression(regression_model, NULL)
      
      output$model_interpretation <- renderText({
        interpretation
      })
      
      showNotification("Model regresi berhasil dibuat", type = "message")
      
    }, error = function(e) {
      showNotification(paste("Error dalam membangun model:", e$message), type = "error")
      cat("Error in regression model:", e$message, "\n")
    })
  })
  
  # Regression assumptions
  observeEvent(input$run_regression_assumptions, {
    req(values$regression_model)
    
    tryCatch({
      model <- values$regression_model
      
      # Extract residuals and fitted values
      residuals <- residuals(model)
      fitted_values <- fitted(model)
      
      # Test 1: Normality of residuals (Shapiro-Wilk)
      if(length(residuals) <= 5000) {
        normality_test <- shapiro.test(residuals)
      } else {
        # Sample for large datasets
        sample_residuals <- sample(residuals, 5000)
        normality_test <- shapiro.test(sample_residuals)
      }
      
      normality_result <- ifelse(normality_test$p.value > 0.05, "PASSED", "FAILED")
      
      # Test 2: Homoscedasticity (Breusch-Pagan test)
      bp_test <- lmtest::bptest(model)
      homoscedasticity_result <- ifelse(bp_test$p.value > 0.05, "PASSED", "FAILED")
      
      # Test 3: Independence (Durbin-Watson test)
      dw_test <- lmtest::dwtest(model)
      independence_result <- ifelse(dw_test$p.value > 0.05, "PASSED", "FAILED")
      
      # Test 4: Multicollinearity (VIF)
      if(length(model$coefficients) > 2) {  # More than just intercept
        vif_values <- car::vif(model)
        max_vif <- max(vif_values)
        multicollinearity_result <- ifelse(max_vif < 5, "PASSED", 
                                           ifelse(max_vif < 10, "WARNING", "FAILED"))
      } else {
        vif_values <- NA
        max_vif <- NA
        multicollinearity_result <- "N/A (single predictor)"
      }
      
      # Store results
      assumptions_result <- list(
        normality = normality_result,
        homoscedasticity = homoscedasticity_result,
        autocorrelation = independence_result,
        multicollinearity = multicollinearity_result
      )
      
      values$regression_assumptions <- assumptions_result
      
      # Display results
      output$regression_assumptions_results <- renderText({
        vif_text <- if(!is.na(max_vif)) {
          paste0("VIF values: ", paste(names(vif_values), round(vif_values, 2), sep = "=", collapse = ", "), "\n",
                 "Maximum VIF: ", round(max_vif, 2), "\n")
        } else {
          "VIF: Not applicable (single predictor)\n"
        }
        
        paste0(
          "REGRESSION ASSUMPTIONS TEST\n",
          "===========================\n\n",
          "1. NORMALITY OF RESIDUALS (Shapiro-Wilk)\n",
          "Statistic: ", round(normality_test$statistic, 4), "\n",
          "p-value: ", round(normality_test$p.value, 4), "\n",
          "Result: ", normality_result, "\n\n",
          
          "2. HOMOSCEDASTICITY (Breusch-Pagan)\n",
          "Statistic: ", round(bp_test$statistic, 4), "\n",
          "p-value: ", round(bp_test$p.value, 4), "\n",
          "Result: ", homoscedasticity_result, "\n\n",
          
          "3. INDEPENDENCE (Durbin-Watson)\n",
          "Statistic: ", round(dw_test$statistic, 4), "\n",
          "p-value: ", round(dw_test$p.value, 4), "\n",
          "Result: ", independence_result, "\n\n",
          
          "4. MULTICOLLINEARITY (VIF)\n",
          vif_text,
          "Result: ", multicollinearity_result, "\n\n",
          
          "OVERALL ASSESSMENT:\n",
          "Normality: ", normality_result, "\n",
          "Homoscedasticity: ", homoscedasticity_result, "\n",
          "Independence: ", independence_result, "\n",
          "Multicollinearity: ", multicollinearity_result
        )
      })
      
      # Create assumption plots
      assumptions_data <- data.frame(
        fitted = fitted_values,
        residuals = residuals,
        std_residuals = scale(residuals),
        sqrt_std_residuals = sqrt(abs(scale(residuals)))
      )
      
      # Plot 1: Residuals vs Fitted
      p1 <- ggplot(assumptions_data, aes(x = fitted, y = residuals)) +
        geom_point(alpha = 0.6, color = "#dc2626") +
        geom_hline(yintercept = 0, linetype = "dashed", color = "#991b1b") +
        geom_smooth(se = FALSE, color = "#059669") +
        theme_minimal() +
        labs(title = "Residuals vs Fitted", x = "Fitted Values", y = "Residuals")
      
      # Plot 2: Q-Q plot for normality
      p2 <- ggplot(assumptions_data, aes(sample = std_residuals)) +
        stat_qq(color = "#dc2626", alpha = 0.6) +
        stat_qq_line(color = "#991b1b") +
        theme_minimal() +
        labs(title = "Normal Q-Q Plot", x = "Theoretical Quantiles", y = "Standardized Residuals")
      
      # Plot 3: Scale-Location (homoscedasticity)
      p3 <- ggplot(assumptions_data, aes(x = fitted, y = sqrt_std_residuals)) +
        geom_point(alpha = 0.6, color = "#dc2626") +
        geom_smooth(se = FALSE, color = "#059669") +
        theme_minimal() +
        labs(title = "Scale-Location", x = "Fitted Values", y = "√|Standardized Residuals|")
      
      # Plot 4: Residuals vs Order (independence)
      assumptions_data$order <- 1:nrow(assumptions_data)
      p4 <- ggplot(assumptions_data, aes(x = order, y = residuals)) +
        geom_point(alpha = 0.6, color = "#dc2626") +
        geom_hline(yintercept = 0, linetype = "dashed", color = "#991b1b") +
        geom_smooth(se = FALSE, color = "#059669") +
        theme_minimal() +
        labs(title = "Residuals vs Order", x = "Observation Order", y = "Residuals")
      
      # Combine plots using subplot
      combined_plot <- subplot(
        ggplotly(p1), ggplotly(p2), 
        ggplotly(p3), ggplotly(p4),
        nrows = 2, margin = 0.05
      ) %>%
        layout(title = "Regression Assumptions Diagnostic Plots")
      
      output$regression_assumptions_plot <- renderPlotly({
        combined_plot
      })
      
      # Generate interpretation
      interpretation <- paste0(
        "INTERPRETASI UJI ASUMSI REGRESI\n\n",
        "1. NORMALITAS RESIDUAL: ", ifelse(normality_result == "PASSED", 
                                           "✓ Asumsi terpenuhi. Residual berdistribusi normal.",
                                           "✗ Asumsi tidak terpenuhi. Residual tidak berdistribusi normal. Pertimbangkan transformasi data."), "\n\n",
        
        "2. HOMOSKEDASTISITAS: ", ifelse(homoscedasticity_result == "PASSED",
                                         "✓ Asumsi terpenuhi. Varians residual konstan.",
                                         "✗ Asumsi tidak terpenuhi. Terdapat heteroskedastisitas. Pertimbangkan weighted least squares."), "\n\n",
        
        "3. INDEPENDENSI: ", ifelse(independence_result == "PASSED",
                                    "✓ Asumsi terpenuhi. Tidak ada autokorelasi.",
                                    "✗ Asumsi tidak terpenuhi. Terdapat autokorelasi. Pertimbangkan time series model."), "\n\n",
        
        "4. MULTIKOLINEARITAS: ", ifelse(multicollinearity_result == "PASSED",
                                         "✓ Tidak ada masalah multikolinearitas.",
                                         ifelse(multicollinearity_result == "WARNING",
                                                "⚠ Multikolinearitas sedang. Monitor interpretasi koefisien.",
                                                "✗ Multikolinearitas tinggi. Pertimbangkan menghapus variabel prediktor.")), "\n\n",
        
        "REKOMENDASI:\n",
        ifelse(all(c(normality_result, homoscedasticity_result, independence_result) == "PASSED") & 
                 multicollinearity_result %in% c("PASSED", "N/A (single predictor)"),
               "✓ Semua asumsi terpenuhi. Model regresi valid untuk interpretasi dan prediksi.",
               "⚠ Beberapa asumsi tidak terpenuhi. Interpretasi hasil harus hati-hati. Pertimbangkan transformasi data atau metode alternatif.")
      )
      
      output$regression_assumptions_interpretation <- renderText({
        interpretation
      })
      
      showNotification("Uji asumsi regresi selesai", type = "message")
      
    }, error = function(e) {
      showNotification(paste("Error dalam uji asumsi regresi:", e$message), type = "error")
      cat("Error in regression assumptions:", e$message, "\n")
    })
  })
  
  # Regression diagnostics
  observe({
    if(!is.null(values$regression_model)) {
      model <- values$regression_model
      
      # Create diagnostic plots
      diagnostic_data <- data.frame(
        fitted = fitted(model),
        residuals = residuals(model),
        std_residuals = rstandard(model),
        leverage = hatvalues(model),
        cooks_distance = cooks.distance(model)
      )
      
      # Plot 1: Residuals vs Fitted
      p1 <- ggplot(diagnostic_data, aes(x = fitted, y = residuals)) +
        geom_point(alpha = 0.6, color = "#dc2626") +
        geom_hline(yintercept = 0, linetype = "dashed") +
        geom_smooth(se = FALSE, color = "#059669") +
        theme_minimal() +
        labs(title = "Residuals vs Fitted", x = "Fitted Values", y = "Residuals")
      
      # Plot 2: Normal Q-Q
      p2 <- ggplot(diagnostic_data, aes(sample = std_residuals)) +
        stat_qq(color = "#dc2626", alpha = 0.6) +
        stat_qq_line(color = "#991b1b") +
        theme_minimal() +
        labs(title = "Normal Q-Q", x = "Theoretical Quantiles", y = "Standardized Residuals")
      
      # Plot 3: Scale-Location
      p3 <- ggplot(diagnostic_data, aes(x = fitted, y = sqrt(abs(std_residuals)))) +
        geom_point(alpha = 0.6, color = "#dc2626") +
        geom_smooth(se = FALSE, color = "#059669") +
        theme_minimal() +
        labs(title = "Scale-Location", x = "Fitted Values", y = "√|Standardized Residuals|")
      
      # Plot 4: Cook's Distance
      diagnostic_data$index <- 1:nrow(diagnostic_data)
      p4 <- ggplot(diagnostic_data, aes(x = index, y = cooks_distance)) +
        geom_point(alpha = 0.6, color = "#dc2626") +
        geom_hline(yintercept = 4/nrow(diagnostic_data), linetype = "dashed", color = "#991b1b") +
        theme_minimal() +
        labs(title = "Cook's Distance", x = "Observation Index", y = "Cook's Distance")
      
      output$diagnostic_plots <- renderPlotly({
        subplot(
          ggplotly(p1), ggplotly(p2),
          ggplotly(p3), ggplotly(p4),
          nrows = 2, margin = 0.05
        ) %>%
          layout(title = "Regression Diagnostic Plots")
      })
      
      # Diagnostic interpretation
      influential_obs <- which(diagnostic_data$cooks_distance > 4/nrow(diagnostic_data))
      high_leverage <- which(diagnostic_data$leverage > 2*length(model$coefficients)/nrow(diagnostic_data))
      
      output$diagnostic_interpretation <- renderText({
        paste0(
          "INTERPRETASI DIAGNOSTIK MODEL\n\n",
          "1. RESIDUALS vs FITTED:\n",
          "Pola acak menunjukkan linearitas yang baik.\n",
          "Pola kurvilinear menunjukkan non-linearitas.\n\n",
          
          "2. NORMAL Q-Q PLOT:\n",
          "Titik-titik mengikuti garis menunjukkan normalitas residual.\n\n",
          
          "3. SCALE-LOCATION:\n",
          "Penyebaran acak menunjukkan homoskedastisitas.\n\n",
          
          "4. COOK'S DISTANCE:\n",
          "Observasi berpengaruh: ", ifelse(length(influential_obs) > 0, 
                                            paste(influential_obs, collapse = ", "), "Tidak ada"), "\n",
          "High leverage points: ", ifelse(length(high_leverage) > 0,
                                           paste(high_leverage, collapse = ", "), "Tidak ada"), "\n\n",
          
          "REKOMENDASI:\n",
          ifelse(length(influential_obs) > 0,
                 "⚠ Periksa observasi berpengaruh dan pertimbangkan untuk menghapus outlier atau transformasi data.",
                 "✓ Tidak ada observasi yang sangat berpengaruh.")
        )
      })
    }
  })
  
  # Dynamic prediction inputs
  output$prediction_inputs <- renderUI({
    if(!is.null(values$regression_model)) {
      predictors <- names(values$regression_model$coefficients)[-1]  # Exclude intercept
      
      input_list <- lapply(predictors, function(var) {
        numericInput(paste0("pred_", var), paste("Value for", var, ":"), value = 0)
      })
      
      do.call(tagList, input_list)
    }
  })
  
  # Make predictions
  observeEvent(input$make_prediction, {
    req(values$regression_model)
    
    tryCatch({
      model <- values$regression_model
      predictors <- names(model$coefficients)[-1]
      
      # Get input values
      pred_values <- list()
      for(var in predictors) {
        pred_values[[var]] <- input[[paste0("pred_", var)]]
      }
      
      # Create prediction dataframe
      pred_df <- data.frame(pred_values)
      
      # Make prediction
      prediction <- predict(model, newdata = pred_df, interval = "prediction")
      
      output$prediction_results <- renderText({
        paste0(
          "HASIL PREDIKSI\n",
          "=============\n",
          "Input values:\n",
          paste(names(pred_values), sapply(pred_values, function(x) round(x, 2)), sep = " = ", collapse = "\n"), "\n\n",
          "Predicted value: ", round(prediction[1], 4), "\n",
          "95% Prediction Interval: [", round(prediction[2], 4), ", ", round(prediction[3], 4), "]"
        )
      })
      
      # Create prediction vs actual plot
      model_data <- model$model
      actual_vs_fitted <- data.frame(
        actual = model_data[, 1],  # Dependent variable
        fitted = fitted(model)
      )
      
      p <- ggplot(actual_vs_fitted, aes(x = fitted, y = actual)) +
        geom_point(alpha = 0.6, color = "#dc2626") +
        geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "#991b1b") +
        geom_point(data = data.frame(fitted = prediction[1], actual = prediction[1]), 
                   aes(x = fitted, y = actual), color = "#059669", size = 4, shape = 17) +
        theme_minimal() +
        labs(title = "Actual vs Fitted Values", 
             x = "Fitted Values", y = "Actual Values",
             subtitle = "Green triangle = New prediction")
      
      output$prediction_plot <- renderPlotly({
        ggplotly(p)
      })
      
      showNotification("Prediksi berhasil dibuat", type = "message")
      
    }, error = function(e) {
      showNotification(paste("Error dalam prediksi:", e$message), type = "error")
    })
  })
  
  # ===================================================================
  # CLUSTERING SECTION
  # ===================================================================
  
  # Load data untuk clustering
  observeEvent(input$load_clustering_data, {
    tryCatch({
      if(input$clustering_data_source == "processed" && !is.null(values$processed_data)) {
        values$clustering_data <- values$processed_data
        source_text <- "Data dari Manajemen Data"
      } else if(input$clustering_data_source == "upload" && !is.null(input$clustering_data_file)) {
        values$clustering_data <- load_data_from_file(input$clustering_data_file$datapath)
        source_text <- paste("File upload:", input$clustering_data_file$name)
      } else {
        showNotification("Pilih sumber data terlebih dahulu", type = "warning")
        return()
      }
      
      if(!is.null(values$clustering_data)) {
        # Update variable choices
        numeric_cols <- names(values$clustering_data)[sapply(values$clustering_data, is.numeric)]
        
        # Update choices untuk clustering
        updateSelectizeInput(session, "cluster_variables", choices = numeric_cols,
                             selected = if(length(numeric_cols) >= 3) numeric_cols[1:3] else numeric_cols)
        updateSelectizeInput(session, "fgwc_variables", choices = numeric_cols,
                             selected = if(length(numeric_cols) >= 3) numeric_cols[1:3] else numeric_cols)
        
        showNotification(paste("Data berhasil dimuat:", source_text), type = "message")
      }
    }, error = function(e) {
      showNotification(paste("Error loading data:", e$message), type = "error")
    })
  })
  
  # Data info untuk clustering
  output$clustering_data_info <- renderText({
    if(input$clustering_data_source == "processed" && !is.null(values$processed_data)) {
      paste0("Data Manajemen: ", nrow(values$processed_data), " obs, ", 
             ncol(values$processed_data), " vars")
    } else if(input$clustering_data_source == "upload" && !is.null(input$clustering_data_file)) {
      paste0("File: ", input$clustering_data_file$name, "\n",
             "Size: ", round(input$clustering_data_file$size/1024, 2), " KB")
    } else {
      "Belum ada data yang dipilih"
    }
  })
  
  # Preview data untuk clustering
  output$clustering_data_preview <- DT::renderDataTable({
    if(!is.null(values$clustering_data)) {
      DT::datatable(values$clustering_data, options = list(scrollX = TRUE, pageLength = 10),
                    caption = paste("Preview Data - Total:", nrow(values$clustering_data), "observasi"))
    } else {
      DT::datatable(data.frame(Message = "Load data terlebih dahulu"))
    }
  })
  
  # K-means clustering
  observeEvent(input$run_clustering, {
    req(input$cluster_variables)
    
    tryCatch({
      current_data <- values$processed_data
      
      # Check if variables exist
      available_vars <- intersect(input$cluster_variables, names(current_data))
      if(length(available_vars) == 0) {
        showNotification("Variabel clustering tidak tersedia", type = "warning")
        return()
      }
      
      # Prepare clustering data
      cluster_data <- current_data %>%
        select(DISTRICTCODE, REGION, all_of(available_vars)) %>%
        na.omit()
      
      if(nrow(cluster_data) < 10) {
        showNotification("Data tidak cukup untuk clustering", type = "warning")
        return()
      }
      
      # Scale the data
      scaled_data <- scale(cluster_data[, available_vars])
      
      # Determine optimal k using elbow method
      wss <- sapply(1:8, function(k) {
        kmeans(scaled_data, centers = k, nstart = 25)$tot.withinss
      })
      
      # Plot elbow curve
      elbow_data <- data.frame(k = 1:8, wss = wss)
      
      # Determine optimal k
      if(input$k_method == "elbow") {
        # Simple elbow detection - look for the largest decrease
        decreases <- diff(wss)
        optimal_k <- which.min(decreases) + 1
        optimal_k <- max(2, min(optimal_k, 6))  # Constrain to reasonable range
      } else {
        optimal_k <- input$manual_k
      }
      
      elbow_plot <- ggplot(elbow_data, aes(x = k, y = wss)) +
        geom_line(color = "#dc2626", size = 1) +
        geom_point(color = "#991b1b", size = 3) +
        geom_vline(xintercept = optimal_k, linetype = "dashed", color = "#059669", size = 1) +
        annotate("text", x = optimal_k, y = max(wss) * 0.8, 
                 label = paste("Optimal k =", optimal_k), 
                 color = "#059669", fontface = "bold", hjust = -0.1) +
        theme_minimal() +
        labs(title = "Elbow Method For Optimal k", 
             x = "Number of Clusters (k)", y = "Within-cluster Sum of Squares")
      
      output$elbow_plot <- renderPlotly({
        ggplotly(elbow_plot)
      })
      
      # Determine k
      if(input$k_method == "elbow") {
        # Simple elbow detection - look for the largest decrease
        decreases <- diff(wss)
        optimal_k <- which.min(decreases) + 1
        optimal_k <- max(2, min(optimal_k, 6))  # Constrain to reasonable range
      } else {
        optimal_k <- input$manual_k
      }
      
      # Perform k-means clustering
      set.seed(123)
      kmeans_result <- kmeans(scaled_data, centers = optimal_k, nstart = 25)
      
      # Add cluster assignments to data
      cluster_data$cluster <- as.factor(kmeans_result$cluster)
      values$kmeans_result <- kmeans_result
      values$cluster_data <- cluster_data
      
      # Display clustering results
      output$cluster_results <- DT::renderDataTable({
        display_data <- cluster_data %>%
          select(DISTRICTCODE, REGION, cluster, all_of(available_vars))
        
        # Fix the color mapping issue
        unique_clusters <- sort(unique(cluster_data$cluster))
        cluster_colors <- RColorBrewer::brewer.pal(min(max(3, length(unique_clusters)), 8), "Set2")
        
        DT::datatable(display_data, options = list(scrollX = TRUE, pageLength = 10),
                      caption = paste("K-Means Clustering Results (k =", optimal_k, ")")) %>%
          DT::formatStyle("cluster", backgroundColor = DT::styleEqual(
            unique_clusters,
            cluster_colors[1:length(unique_clusters)]
          ))
      })
      
      # Generate clustering interpretation
      interpretation <- interpret_clustering(kmeans_result, scaled_data, "kmeans")
      
      output$clustering_interpretation <- renderText({
        interpretation
      })
      
      showNotification(paste("K-means clustering selesai dengan k =", optimal_k), type = "message")
      
      if(!"kmeans" %in% values$cluster_method_available) {
        values$cluster_method_available <- c(values$cluster_method_available, "kmeans")
      }
      
    }, error = function(e) {
      showNotification(paste("Error dalam k-means clustering:", e$message), type = "error")
      cat("Error in k-means clustering:", e$message, "\n")
    })
  })
  
  # FGWC clustering
  observeEvent(input$run_fgwc, {
    # Pastikan semua parameter required tersedia
    req(input$fgwc_variables, input$fgwc_k, input$fgwc_m, 
        input$fgwc_spatial_method, input$fgwc_bandwidth, input$fgwc_lambda,
        values$clustering_data)
    
    tryCatch({
      current_data <- values$clustering_data
      
      # Check if variables exist
      available_vars <- intersect(input$fgwc_variables, names(current_data))
      if(length(available_vars) == 0) {
        showNotification("Variabel FGWC tidak tersedia", type = "warning")
        return()
      }
      
      # Prepare FGWC data
      fgwc_data <- current_data %>%
        select(DISTRICTCODE, REGION, all_of(available_vars)) %>%
        na.omit()
      
      if(nrow(fgwc_data) < 10) {
        showNotification("Data tidak cukup untuk FGWC", type = "warning")
        return()
      }
      
      # Scale the data
      scaled_data <- scale(fgwc_data[, available_vars])
      
      # Urutkan fgwc_data berdasarkan DISTRICTCODE secara ascending
      fgwc_data <- fgwc_data[order(fgwc_data$DISTRICTCODE), ]
      
      # Scale the data setelah diurutkan
      scaled_data <- scale(fgwc_data[, available_vars])
      
      # Load distance matrix - simplified karena sudah terurut
      dist_matrix <- distance_matrix
      
      if(!is.null(dist_matrix)) {
        # Cek apakah semua DISTRICTCODE ada di distance matrix
        available_districts <- intersect(rownames(dist_matrix), as.character(fgwc_data$DISTRICTCODE))
        
        if(length(available_districts) >= nrow(fgwc_data) * 0.8) {
          # Filter data untuk district yang ada di distance matrix
          fgwc_data <- fgwc_data[fgwc_data$DISTRICTCODE %in% available_districts, ]
          
          # Urutkan ulang untuk memastikan konsistensi
          fgwc_data <- fgwc_data[order(fgwc_data$DISTRICTCODE), ]
          
          # Subset distance matrix sesuai dengan data yang ada
          district_indices <- match(as.character(fgwc_data$DISTRICTCODE), rownames(dist_matrix))
          dist_matrix <- dist_matrix[district_indices, district_indices]
          
          # Update scaled_data
          scaled_data <- scale(fgwc_data[, available_vars])
          
          cat("Distance matrix matching berhasil untuk", nrow(fgwc_data), "districts\n")
          cat("Range DISTRICTCODE:", min(fgwc_data$DISTRICTCODE), "-", max(fgwc_data$DISTRICTCODE), "\n")
          
        } else {
          dist_matrix <- NULL
          cat("Insufficient matching, menggunakan FGWC tanpa constraint spasial\n")
        }
        
      } else {
        cat("Distance matrix tidak tersedia\n")
      }
      
      # Perform FGWC clustering dengan parameter yang sudah ada di UI
      fgwc_result <- fgwc_clustering(
        data = scaled_data,
        distance_matrix = dist_matrix,
        k = input$fgwc_k,
        m = input$fgwc_m,
        spatial_method = input$fgwc_spatial_method,
        bandwidth = input$fgwc_bandwidth,
        lambda = input$fgwc_lambda
      )
      
      # Add cluster assignments to data
      fgwc_data$cluster <- as.factor(fgwc_result$clusters)
      values$fgwc_result <- fgwc_result
      
      # Display FGWC results dengan info parameter
      output$fgwc_results <- DT::renderDataTable({
        display_data <- fgwc_data %>%
          select(DISTRICTCODE, REGION, cluster, all_of(available_vars))
        
        caption_text <- paste0("FGWC Results (k=", input$fgwc_k, ", m=", input$fgwc_m, 
                               ", method=", input$fgwc_spatial_method, 
                               ", bandwidth=", input$fgwc_bandwidth, "km",
                               ", λ=", input$fgwc_lambda, 
                               ", iterations=", fgwc_result$iterations, ")")
        
        DT::datatable(display_data, options = list(scrollX = TRUE, pageLength = 10),
                      caption = caption_text) %>%
          DT::formatStyle("cluster", backgroundColor = DT::styleEqual(
            levels(fgwc_data$cluster),
            RColorBrewer::brewer.pal(min(input$fgwc_k, 8), "Set3")
          ))
      })
      
      # Display membership matrix (top 10 rows) dengan info tambahan
      membership_display <- data.frame(
        DISTRICTCODE = fgwc_data$DISTRICTCODE[1:min(10, nrow(fgwc_data))],
        fgwc_result$membership[1:min(10, nrow(fgwc_data)), ]
      )
      
      colnames(membership_display)[-1] <- paste0("Cluster_", 1:input$fgwc_k)
      
      output$fgwc_membership <- DT::renderDataTable({
        DT::datatable(membership_display, options = list(scrollX = TRUE, pageLength = 10),
                      caption = paste("FGWC Membership Matrix (Top 10) - Objective Value:", 
                                      round(fgwc_result$objective, 2))) %>%
          DT::formatRound(columns = 2:(input$fgwc_k + 1), digits = 3)
      })
      
      # Generate enhanced FGWC interpretation
      interpretation <- interpret_clustering_enhanced(fgwc_result, scaled_data, "fgwc", 
                                                      input$fgwc_spatial_method, input$fgwc_bandwidth, input$fgwc_lambda)
      
      output$fgwc_interpretation <- renderText({
        interpretation
      })
      
      showNotification(paste("FGWC clustering selesai dengan", fgwc_result$iterations, "iterasi"), type = "message")
      
      if(!"fgwc" %in% values$cluster_method_available) {
        values$cluster_method_available <- c(values$cluster_method_available, "fgwc")
      }
      
    }, error = function(e) {
      showNotification(paste("Error dalam FGWC clustering:", e$message), type = "error")
      cat("Error in FGWC clustering:", e$message, "\n")
    })
  })
  
  observe({
    if(!is.null(values$kmeans_result) && !is.null(values$fgwc_result)) {
      
      # Pastikan menggunakan variabel yang sama untuk perbandingan
      cluster_vars <- intersect(input$cluster_variables, names(values$cluster_data))
      if(length(cluster_vars) == 0) {
        cluster_vars <- intersect(input$fgwc_variables, names(values$clustering_data))
      }
      
      if(length(cluster_vars) > 0) {
        
        # Ambil data yang konsisten untuk kedua metode
        kmeans_data <- values$cluster_data %>%
          select(DISTRICTCODE, all_of(cluster_vars)) %>%
          na.omit()
        
        fgwc_data <- values$clustering_data %>%
          select(DISTRICTCODE, all_of(cluster_vars)) %>%
          na.omit()
        
        # Cari common districts
        common_districts <- intersect(kmeans_data$DISTRICTCODE, fgwc_data$DISTRICTCODE)
        
        if(length(common_districts) > 10) {
          
          # Filter data untuk common districts saja
          kmeans_filtered <- kmeans_data %>%
            filter(DISTRICTCODE %in% common_districts) %>%
            arrange(DISTRICTCODE)
          
          fgwc_filtered <- fgwc_data %>%
            filter(DISTRICTCODE %in% common_districts) %>%
            arrange(DISTRICTCODE)
          
          # Scale data yang sudah difilter
          scaled_data_comparison <- scale(kmeans_filtered[, cluster_vars])
          data_dist <- dist(scaled_data_comparison)
          
          # Ambil cluster assignments yang sesuai dengan common districts
          kmeans_clusters_filtered <- values$kmeans_result$cluster[match(common_districts, values$cluster_data$DISTRICTCODE)]
          fgwc_clusters_filtered <- values$fgwc_result$clusters[match(common_districts, values$clustering_data$DISTRICTCODE)]
          
          # Remove NA values
          valid_indices <- !is.na(kmeans_clusters_filtered) & !is.na(fgwc_clusters_filtered)
          kmeans_clusters_final <- kmeans_clusters_filtered[valid_indices]
          fgwc_clusters_final <- fgwc_clusters_filtered[valid_indices]
          data_dist_final <- as.dist(as.matrix(data_dist)[valid_indices, valid_indices])
          
          if(length(kmeans_clusters_final) > 5) {
            
            # Calculate silhouette for both methods
            sil_kmeans <- cluster::silhouette(kmeans_clusters_final, data_dist_final)
            sil_fgwc <- cluster::silhouette(fgwc_clusters_final, data_dist_final)
            
            # Store results
            values$silhouette_comparison <- list(
              kmeans = sil_kmeans,
              fgwc = sil_fgwc,
              kmeans_avg = mean(sil_kmeans[, 3]),
              fgwc_avg = mean(sil_fgwc[, 3]),
              common_districts = length(kmeans_clusters_final)
            )
            
            cat("Silhouette comparison calculated for", length(kmeans_clusters_final), "common districts\n")
            cat("K-Means avg silhouette:", round(values$silhouette_comparison$kmeans_avg, 3), "\n")
            cat("FGWC avg silhouette:", round(values$silhouette_comparison$fgwc_avg, 3), "\n")
          }
        }
      }
    }
  })
  
  # Display cluster map status
  output$cluster_map_status <- renderText({
    available_methods <- values$cluster_method_available
    
    if(length(available_methods) == 0) {
      "Status: Belum ada metode clustering yang dijalankan"
    } else {
      kmeans_status <- ifelse("kmeans" %in% available_methods, "✓ K-Means", "✗ K-Means")
      fgwc_status <- ifelse("fgwc" %in% available_methods, "✓ FGWC", "✗ FGWC")
      paste("Status:", kmeans_status, "|", fgwc_status)
    }
  })
  
  # Cluster visualization
  observe({
    if(!is.null(values$kmeans_result) || !is.null(values$fgwc_result)) {
      observeEvent(input$update_cluster_map, {
        req(input$cluster_map_method)
        
        if(!input$cluster_map_method %in% values$cluster_method_available) {
          showNotification(paste("Jalankan", ifelse(input$cluster_map_method == "kmeans", "K-Means", "FGWC"), "clustering terlebih dahulu"), type = "warning")
          return()
        }
        
        # Prepare data based on selected method
        if(input$cluster_map_method == "kmeans") {
          if(is.null(values$kmeans_result) || is.null(values$cluster_data)) {
            showNotification("Data K-means tidak tersedia", type = "error")
            return()
          }
          map_data <- values$cluster_data
          method_title <- "K-Means Clustering"
          
        } else if(input$cluster_map_method == "fgwc") {
          if(is.null(values$fgwc_result)) {
            showNotification("Data FGWC tidak tersedia", type = "error")
            return()
          }
          
          # Create FGWC map data
          current_data <- values$clustering_data
          fgwc_data <- current_data %>%
            select(DISTRICTCODE, REGION, all_of(intersect(input$fgwc_variables, names(current_data)))) %>%
            na.omit()
          fgwc_data$cluster <- as.factor(values$fgwc_result$clusters)
          map_data <- fgwc_data
          method_title <- "FGWC Clustering"
        }
        
        values$current_cluster_map_data <- list(data = map_data, method = method_title)
      })
      
      # Render cluster map
      output$cluster_map <- renderLeaflet({
        if(is.null(values$current_cluster_map_data)) {
          return(leaflet() %>%
                   addTiles() %>%
                   setView(lng = 118, lat = -2, zoom = 5) %>%
                   addMarkers(lng = 118, lat = -2, popup = "Klik 'Update Peta' untuk menampilkan hasil clustering"))
        }
        
        tryCatch({
          map_data <- values$current_cluster_map_data$data
          method_title <- values$current_cluster_map_data$method
          
          if(is.null(geojson_data) || nrow(map_data) == 0) {
            return(leaflet() %>%
                     addTiles() %>%
                     setView(lng = 118, lat = -2, zoom = 5) %>%
                     addMarkers(lng = 118, lat = -2, popup = "Data tidak tersedia"))
          }
          
          # Convert both columns to character before join
          cluster_geo <- geojson_data %>%
            mutate(idkab = as.character(idkab)) %>%
            left_join(
              map_data %>% 
                select(DISTRICTCODE, cluster) %>%
                mutate(DISTRICTCODE = as.character(DISTRICTCODE)), 
              by = c("idkab" = "DISTRICTCODE")
            ) %>%
            filter(!is.na(cluster))
          
          if(nrow(cluster_geo) == 0) {
            return(leaflet() %>%
                     addTiles() %>%
                     setView(lng = 118, lat = -2, zoom = 5) %>%
                     addMarkers(lng = 118, lat = -2, popup = "No matching districts found"))
          }
          
          # Create color palette
          n_clusters <- length(unique(cluster_geo$cluster))
          colors <- RColorBrewer::brewer.pal(min(max(3, n_clusters), 8), "Set2")
          pal <- colorFactor(colors, domain = cluster_geo$cluster)
          
          leaflet(cluster_geo) %>%
            addTiles() %>%
            setView(lng = 118, lat = -2, zoom = 5) %>%
            addPolygons(
              fillColor = ~pal(cluster),
              weight = 1, opacity = 1, color = "white", fillOpacity = 0.7,
              popup = ~paste0("<strong>Kabupaten:</strong> ", 
                              ifelse("nmkab" %in% names(cluster_geo), nmkab, "Unknown"), "<br/>",
                              "<strong>Method:</strong> ", method_title, "<br/>",
                              "<strong>Cluster:</strong> ", cluster),
              label = ~ifelse("nmkab" %in% names(cluster_geo), nmkab, "Unknown")
            ) %>%
            addLegend(pal = pal, values = ~cluster, opacity = 0.7, 
                      title = paste("Cluster (", method_title, ")"), position = "bottomright")
          
        }, error = function(e) {
          return(leaflet() %>%
                   addTiles() %>%
                   setView(lng = 118, lat = -2, zoom = 5) %>%
                   addMarkers(lng = 118, lat = -2, popup = paste("Error:", e$message)))
        })
      })
      
      # Silhouette analysis
      if(!is.null(values$kmeans_result) && !is.null(values$cluster_data)) {
        # Calculate silhouette for k-means
        cluster_vars <- intersect(input$cluster_variables, names(values$cluster_data))
        scaled_data <- scale(values$cluster_data[, cluster_vars])
        
        sil <- cluster::silhouette(values$kmeans_result$cluster, dist(scaled_data))
        sil_df <- data.frame(
          cluster = factor(sil[, 1]),
          silhouette_width = sil[, 3]
        )
        
        avg_sil <- mean(sil_df$silhouette_width)
        
        p <- ggplot(sil_df, aes(x = reorder(1:nrow(sil_df), silhouette_width), 
                                y = silhouette_width, fill = cluster)) +
          geom_col() +
          coord_flip() +
          scale_fill_brewer(type = "qual", palette = "Set2") +
          theme_minimal() +
          labs(title = paste("Silhouette Analysis (Average width:", round(avg_sil, 3), ")"),
               x = "Observations", y = "Silhouette Width") +
          geom_hline(yintercept = avg_sil, linetype = "dashed", color = "red")
        
        output$silhouette_plot <- renderPlotly({
          
          if(!is.null(values$silhouette_comparison)) {
            
            # Data untuk K-Means silhouette
            sil_kmeans <- values$silhouette_comparison$kmeans
            kmeans_df <- data.frame(
              cluster = factor(sil_kmeans[, 1]),
              silhouette_width = sil_kmeans[, 3],
              method = "K-Means"
            )
            
            # Data untuk FGWC silhouette  
            sil_fgwc <- values$silhouette_comparison$fgwc
            fgwc_df <- data.frame(
              cluster = factor(sil_fgwc[, 1]),
              silhouette_width = sil_fgwc[, 3],
              method = "FGWC"
            )
            
            # Combine data
            combined_df <- rbind(kmeans_df, fgwc_df)
            combined_df$observation <- rep(1:nrow(kmeans_df), 2)
            
            # Create comparison plot
            p <- ggplot(combined_df, aes(x = reorder(observation, silhouette_width), 
                                         y = silhouette_width, fill = cluster)) +
              geom_col() +
              facet_wrap(~method, scales = "free_x") +
              coord_flip() +
              scale_fill_brewer(type = "qual", palette = "Set2") +
              theme_minimal() +
              labs(title = paste("Silhouette Analysis Comparison"),
                   subtitle = paste("K-Means avg:", round(values$silhouette_comparison$kmeans_avg, 3),
                                    "| FGWC avg:", round(values$silhouette_comparison$fgwc_avg, 3),
                                    "| n =", values$silhouette_comparison$common_districts),
                   x = "Districts (sorted by silhouette width)", 
                   y = "Silhouette Width",
                   fill = "Cluster") +
              geom_hline(data = data.frame(method = c("K-Means", "FGWC"), 
                                           avg = c(values$silhouette_comparison$kmeans_avg, 
                                                   values$silhouette_comparison$fgwc_avg)),
                         aes(yintercept = avg), linetype = "dashed", color = "red", alpha = 0.8)
            
            return(ggplotly(p))
            
          } else if(!is.null(values$kmeans_result) && !is.null(values$cluster_data)) {
            
            # Fallback ke K-Means saja jika FGWC belum ada
            cluster_vars <- intersect(input$cluster_variables, names(values$cluster_data))
            scaled_data <- scale(values$cluster_data[, cluster_vars])
            
            sil <- cluster::silhouette(values$kmeans_result$cluster, dist(scaled_data))
            sil_df <- data.frame(
              cluster = factor(sil[, 1]),
              silhouette_width = sil[, 3]
            )
            
            avg_sil <- mean(sil_df$silhouette_width)
            
            p <- ggplot(sil_df, aes(x = reorder(1:nrow(sil_df), silhouette_width), 
                                    y = silhouette_width, fill = cluster)) +
              geom_col() +
              coord_flip() +
              scale_fill_brewer(type = "qual", palette = "Set2") +
              theme_minimal() +
              labs(title = paste("K-Means Silhouette Analysis (avg:", round(avg_sil, 3), ")"),
                   x = "Districts", y = "Silhouette Width") +
              geom_hline(yintercept = avg_sil, linetype = "dashed", color = "red")
            
            return(ggplotly(p))
            
          } else {
            return(plotly_empty() %>% layout(title = "Run clustering analysis first"))
          }
        })
      }
      
      # Cluster visualization interpretation
      output$cluster_visual_interpretation <- renderText({
        
        interpretation <- "INTERPRETASI VISUALISASI CLUSTER\n\n"
        
        # K-Means interpretation
        if(!is.null(values$kmeans_result)) {
          n_clusters_kmeans <- length(unique(values$cluster_data$cluster))
          interpretation <- paste0(interpretation,
                                   "K-MEANS CLUSTERING:\n",
                                   "- Jumlah cluster: ", n_clusters_kmeans, "\n",
                                   "- Metode: Partitioning berdasarkan centroid\n",
                                   "- Fokus: Similarity dalam feature space\n\n")
        }
        
        # FGWC interpretation
        if(!is.null(values$fgwc_result)) {
          interpretation <- paste0(interpretation,
                                   "FGWC CLUSTERING:\n",
                                   "- Jumlah cluster: ", input$fgwc_k, "\n",
                                   "- Metode: Fuzzy clustering dengan constraint geografis\n",
                                   "- Spatial parameter lambda: ", input$fgwc_lambda, "\n",
                                   "- Fokus: Similarity + kedekatan geografis\n\n")
        }
        
        # Silhouette comparison
        if(!is.null(values$silhouette_comparison)) {
          kmeans_avg <- round(values$silhouette_comparison$kmeans_avg, 3)
          fgwc_avg <- round(values$silhouette_comparison$fgwc_avg, 3)
          
          better_method <- ifelse(kmeans_avg > fgwc_avg, "K-Means", "FGWC")
          
          interpretation <- paste0(interpretation,
                                   "PERBANDINGAN KUALITAS CLUSTERING:\n",
                                   "- K-Means Silhouette: ", kmeans_avg, "\n",
                                   "- FGWC Silhouette: ", fgwc_avg, "\n",
                                   "- Metode terbaik (silhouette): ", better_method, "\n",
                                   "- Districts dibandingkan: ", values$silhouette_comparison$common_districts, "\n\n")
          
          # Interpretasi nilai silhouette
          interpret_sil <- function(val) {
            if(val > 0.7) return("sangat baik")
            else if(val > 0.5) return("baik")
            else if(val > 0.25) return("cukup")
            else return("lemah")
          }
          
          interpretation <- paste0(interpretation,
                                   "KUALITAS STRUKTUR CLUSTER:\n",
                                   "- K-Means: ", interpret_sil(kmeans_avg), "\n",
                                   "- FGWC: ", interpret_sil(fgwc_avg), "\n\n")
        }
        
        # Rekomendasi
        interpretation <- paste0(interpretation,
                                 "REKOMENDASI PENGGUNAAN:\n",
                                 "- K-Means: Untuk analisis berdasarkan karakteristik sosial murni\n",
                                 "- FGWC: Untuk kebijakan yang mempertimbangkan kedekatan geografis\n",
                                 "- Silhouette analysis: Evaluasi kualitas internal clustering\n",
                                 "- Pilih metode terbaik berdasarkan nilai silhouette dan konteks pengaplikasian")
        return(interpretation)
      })
    }
  })
  
  # ===================================================================
  # DOWNLOAD HANDLERS - COMPLETE IMPLEMENTATION
  # ===================================================================
  
  # Original data downloads (keeping existing implementation)
  output$download_csv <- downloadHandler(
    filename = function() paste("sovi_data_original_", Sys.Date(), ".csv", sep = ""),
    content = function(file) write.csv(sovi_data, file, row.names = FALSE)
  )
  
  output$download_excel <- downloadHandler(
    filename = function() paste("sovi_data_original_", Sys.Date(), ".xlsx", sep = ""),
    content = function(file) openxlsx::write.xlsx(sovi_data, file)
  )
  
  # Processed data downloads (keeping existing implementation)
  output$download_processed_csv <- downloadHandler(
    filename = function() paste("sovi_data_processed_", Sys.Date(), ".csv", sep = ""),
    content = function(file) {
      tryCatch({
        if(!is.null(values$processed_data) && nrow(values$processed_data) > 0) {
          write.csv(values$processed_data, file, row.names = FALSE)
          cat("Processed CSV downloaded successfully with", nrow(values$processed_data), "rows and", ncol(values$processed_data), "columns\n")
        } else {
          write.csv(sovi_data, file, row.names = FALSE)
          cat("Original data downloaded (no processed data available)\n")
        }
      }, error = function(e) {
        cat("Error in downloading processed CSV:", e$message, "\n")
        write.csv(data.frame(Error = paste("Download error:", e$message)), file)
      })
    }
  )
  
  output$download_processed_excel <- downloadHandler(
    filename = function() paste("sovi_data_processed_", Sys.Date(), ".xlsx", sep = ""),
    content = function(file) {
      tryCatch({
        if(!is.null(values$processed_data) && nrow(values$processed_data) > 0) {
          wb <- openxlsx::createWorkbook()
          
          openxlsx::addWorksheet(wb, "Processed_Data")
          openxlsx::writeData(wb, "Processed_Data", values$processed_data)
          
          openxlsx::addWorksheet(wb, "Processing_Summary")
          
          original_vars <- intersect(numeric_variables, names(sovi_data))
          current_vars <- names(values$processed_data)
          categorical_vars <- current_vars[grepl("_Category$", current_vars)]
          transformed_vars <- current_vars[grepl("_(ln|log10|sqrt|inv|boxcox)$", current_vars)]
          
          summary_data <- data.frame(
            Metric = c("Original variables", "Current variables", "Categorical variables added", 
                       "Transformed variables added", "Total observations", "Processing timestamp"),
            Value = c(length(original_vars), length(current_vars), length(categorical_vars),
                      length(transformed_vars), nrow(values$processed_data), as.character(Sys.time()))
          )
          
          openxlsx::writeData(wb, "Processing_Summary", summary_data)
          
          if(length(categorical_vars) > 0 || length(transformed_vars) > 0) {
            openxlsx::addWorksheet(wb, "New_Variables")
            
            new_vars_data <- data.frame(
              Variable = c(categorical_vars, transformed_vars),
              Type = c(rep("Categorical", length(categorical_vars)), 
                       rep("Transformed", length(transformed_vars))),
              Description = c(
                paste("Categorical version of", gsub("_Category$", "", categorical_vars)),
                paste("Transformed version of", gsub("_(ln|log10|sqrt|inv|boxcox)$", "", transformed_vars))
              )
            )
            
            openxlsx::writeData(wb, "New_Variables", new_vars_data)
          }
          
          openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
          cat("Processed Excel downloaded successfully with", nrow(values$processed_data), "rows\n")
          
        } else {
          openxlsx::write.xlsx(sovi_data, file)
          cat("Original data downloaded as Excel (no processed data available)\n")
        }
      }, error = function(e) {
        cat("Error in downloading processed Excel:", e$message, "\n")
        openxlsx::write.xlsx(data.frame(Error = paste("Download error:", e$message)), file)
      })
    }
  )
  
  # Metadata download (keeping existing implementation)
  output$download_metadata <- downloadHandler(
    filename = function() paste("sovi_metadata_", Sys.Date(), ".xlsx", sep = ""),
    content = function(file) {
      metadata_df <- data.frame(
        Variabel = c("CHILDREN", "FEMALE", "ELDERLY", "FHEAD", "FAMILYSIZE", 
                     "NOELECTRIC", "LOWEDU", "GROWTH", "POVERTY", "ILLITERATE", 
                     "NOTRAINING", "DPRONE", "RENTED", "NOSEWER", "TAPWATER", "POPULATION"),
        Alias = c("Anak-anak", "Perempuan", "Lansia", "KRT Perempuan", "Ukuran Keluarga",
                  "Tanpa Listrik", "Pendidikan Rendah", "Pertumbuhan", "Kemiskinan", "Buta Huruf",
                  "Tanpa Pelatihan", "Rawan Bencana", "Rumah Sewa", "Tanpa Sanitasi", "Air Bersih", "Populasi"),
        Definisi = c("Persentase penduduk berusia di bawah 15 tahun terhadap total populasi",
                     "Persentase penduduk berjenis kelamin perempuan terhadap total populasi",
                     "Persentase penduduk berusia 65 tahun ke atas terhadap total populasi",
                     "Persentase rumah tangga dengan kepala rumah tangga perempuan",
                     "Rata-rata jumlah anggota keluarga per rumah tangga",
                     "Persentase rumah tangga yang tidak memiliki akses listrik",
                     "Persentase penduduk dengan tingkat pendidikan di bawah SMP",
                     "Tingkat pertumbuhan penduduk tahunan dalam persen",
                     "Persentase penduduk yang hidup di bawah garis kemiskinan",
                     "Persentase penduduk usia 15+ yang tidak dapat membaca dan menulis",
                     "Persentase penduduk yang belum pernah mengikuti pelatihan kerja",
                     "Indeks yang mengukur tingkat kerawanan terhadap bencana alam",
                     "Persentase rumah tangga yang tinggal di rumah sewa/kontrak",
                     "Persentase rumah tangga tanpa akses sanitasi yang layak",
                     "Persentase rumah tangga dengan akses air bersih",
                     "Jumlah total penduduk dalam wilayah tersebut"),
        Satuan = c("Persen (%)", "Persen (%)", "Persen (%)", "Persen (%)", "Orang",
                   "Persen (%)", "Persen (%)", "Persen (%)", "Persen (%)", "Persen (%)",
                   "Persen (%)", "Indeks", "Persen (%)", "Persen (%)", "Persen (%)", "Jiwa"),
        Tipe_Data = rep("Numerik", 16),
        Referensi = rep("SUSENAS BPS 2017", 16)
      )
      openxlsx::write.xlsx(metadata_df, file)
    }
  )
  
  # Descriptive statistics download (keeping existing implementation)
  output$download_descriptive <- downloadHandler(
    filename = function() paste("descriptive_stats_", Sys.Date(), ".xlsx", sep = ""),
    content = function(file) {
      if(!is.null(values$current_descriptive)) {
        wb <- openxlsx::createWorkbook()
        openxlsx::addWorksheet(wb, "Descriptive_Statistics")
        openxlsx::addWorksheet(wb, "Interpretation")
        
        openxlsx::writeData(wb, "Descriptive_Statistics", values$current_descriptive)
        
        interpretation_text <- interpret_descriptive_real(values$current_descriptive, 
                                                          values$current_descriptive_data, 
                                                          input$desc_variables, input$desc_province)
        openxlsx::writeData(wb, "Interpretation", data.frame(Interpretation = interpretation_text))
        
        openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
      }
    }
  )
  
  # Visualization download 
  output$download_visualization <- downloadHandler(
    filename = function() paste("visualization_", input$viz_type, "_", Sys.Date(), ".png", sep = ""),
    content = function(file) {
      if(!is.null(values$current_viz_plot)) {
        ggsave(file, values$current_viz_plot, width = 12, height = 8, dpi = 300)
      }
    }
  )
  
  # Plot + Interpretation PDF download 
  output$download_viz_report <- downloadHandler(
    filename = function() paste("viz_report_", input$viz_type, "_", Sys.Date(), ".pdf", sep = ""),
    content = function(file) {
      if(!is.null(values$current_viz_plot) && !is.null(values$current_viz_interpretation)) {
        library(gridExtra)
        
        header_text <- paste0(
          "LAPORAN VISUALISASI DATA SIGABISA\n",
          "Sistem Informasi Geospasial Ancaman Bencana berbasis Indikator Sosial\n",
          "Tanggal: ", format(Sys.Date(), "%d %B %Y"), "\n",
          "Jenis Visualisasi: ", input$viz_type, "\n",
          "Variabel: ", paste(input$viz_variables, collapse = ", "), "\n",
          paste(rep("=", 80), collapse = "")
        )
        
        header_grob <- gridExtra::tableGrob(
          data.frame(Header = header_text),
          theme = gridExtra::ttheme_minimal(
            base_size = 12,
            core = list(fg_params = list(hjust = 0, x = 0.02)),
            colhead = list(fg_params = list(hjust = 0, x = 0.02))
          ),
          rows = NULL, cols = NULL
        )
        
        interpretation_lines <- unlist(strsplit(values$current_viz_interpretation, "\n"))
        interpretation_formatted <- data.frame(
          Interpretasi = interpretation_lines[interpretation_lines != ""]
        )
        
        interpretation_grob <- gridExtra::tableGrob(
          interpretation_formatted,
          theme = gridExtra::ttheme_minimal(
            base_size = 10,
            core = list(
              fg_params = list(hjust = 0, x = 0.02),
              bg_params = list(fill = "white")
            ),
            colhead = list(
              fg_params = list(hjust = 0, x = 0.02, fontface = "bold"),
              bg_params = list(fill = "lightgray")
            )
          ),
          rows = NULL
        )
        
        combined_plot <- gridExtra::grid.arrange(
          header_grob,
          values$current_viz_plot,
          interpretation_grob,
          ncol = 1,
          heights = c(0.8, 3, 1.5)
        )
        
        ggsave(file, combined_plot, width = 12, height = 16, device = "pdf", dpi = 300)
      }
    }
  )
  
  # Map download as HTML (keeping existing implementation)
  output$download_map <- downloadHandler(
    filename = function() paste("spatial_map_", input$map_variable, "_", Sys.Date(), ".html", sep = ""),
    content = function(file) {
      if(!is.null(values$current_map_widget)) {
        htmlwidgets::saveWidget(values$current_map_widget, file, selfcontained = TRUE)
      } else {
        cat("<html><body><h2>Map not available</h2><p>Please create a map first.</p></body></html>", file = file)
      }
    }
  )
  
  # Download interpretasi peta sebagai PDF (keeping existing implementation)
  output$download_map_interpretation <- downloadHandler(
    filename = function() paste("map_interpretation_", input$map_variable, "_", input$map_level, "_", Sys.Date(), ".pdf", sep = ""),
    content = function(file) {
      if(!is.null(values$current_map_data) && !is.null(input$map_variable)) {
        library(gridExtra)
        
        header_text <- paste0(
          "LAPORAN INTERPRETASI PETA SPASIAL SIGABISA\n",
          "Sistem Informasi Geospasial Ancaman Bencana berbasis Indikator Sosial\n",
          "Tanggal: ", format(Sys.Date(), "%d %B %Y"), "\n",
          "Variabel: ", input$map_variable, "\n",
          "Level Agregasi: ", ifelse(input$map_level == "provinsi", "Provinsi", "Kabupaten/Kota"), "\n",
          paste(rep("=", 80), collapse = "")
        )
        
        header_grob <- gridExtra::tableGrob(
          data.frame(Header = header_text),
          theme = gridExtra::ttheme_minimal(
            base_size = 12,
            core = list(fg_params = list(hjust = 0, x = 0.02)),
            colhead = list(fg_params = list(hjust = 0, x = 0.02))
          ),
          rows = NULL, cols = NULL
        )
        
        map_interpretation <- interpret_map_real(input$map_variable, values$current_map_data, input$map_level)
        
        interpretation_lines <- unlist(strsplit(map_interpretation, "\n"))
        interpretation_formatted <- data.frame(
          Interpretasi = interpretation_lines[interpretation_lines != ""]
        )
        
        interpretation_grob <- gridExtra::tableGrob(
          interpretation_formatted,
          theme = gridExtra::ttheme_minimal(
            base_size = 10,
            core = list(
              fg_params = list(hjust = 0, x = 0.02),
              bg_params = list(fill = "white")
            ),
            colhead = list(
              fg_params = list(hjust = 0, x = 0.02, fontface = "bold"),
              bg_params = list(fill = "lightgray")
            )
          ),
          rows = NULL
        )
        
        combined_content <- gridExtra::grid.arrange(
          header_grob,
          interpretation_grob,
          ncol = 1,
          heights = c(1, 4)
        )
        
        ggsave(file, combined_content, width = 12, height = 14, device = "pdf", dpi = 300)
      } else {
        pdf(file, width = 12, height = 14)
        plot(1, type = "n", axes = FALSE, xlab = "", ylab = "", main = "Interpretasi Peta Belum Tersedia")
        text(1, 1, "Silakan buat peta terlebih dahulu dengan menekan tombol 'Update Peta'", 
             cex = 1.2, col = "red")
        dev.off()
      }
    }
  )
  
  # Download peta + interpretasi sebagai ZIP (keeping existing implementation)
  output$download_map_complete <- downloadHandler(
    filename = function() paste("map_complete_", input$map_variable, "_", input$map_level, "_", Sys.Date(), ".zip", sep = ""),
    content = function(file) {
      if(!is.null(values$current_map_widget) && !is.null(values$current_map_data)) {
        temp_dir <- tempdir()
        files_to_zip <- c()
        
        map_file <- file.path(temp_dir, paste0("peta_", input$map_variable, "_", input$map_level, ".html"))
        htmlwidgets::saveWidget(values$current_map_widget, map_file, selfcontained = TRUE)
        files_to_zip <- c(files_to_zip, map_file)
        
        interpretation_file <- file.path(temp_dir, paste0("interpretasi_", input$map_variable, "_", input$map_level, ".pdf"))
        
        library(gridExtra)
        
        header_text <- paste0(
          "LAPORAN INTERPRETASI PETA SPASIAL SIGABISA\n",
          "Tanggal: ", format(Sys.Date(), "%d %B %Y"), "\n",
          "Variabel: ", input$map_variable, "\n",
          "Level: ", ifelse(input$map_level == "provinsi", "Provinsi", "Kabupaten/Kota"), "\n",
          paste(rep("=", 80), collapse = "")
        )
        
        header_grob <- gridExtra::tableGrob(
          data.frame(Header = header_text),
          theme = gridExtra::ttheme_minimal(
            base_size = 12,
            core = list(fg_params = list(hjust = 0, x = 0.02))
          ),
          rows = NULL, cols = NULL
        )
        
        map_interpretation <- interpret_map_real(input$map_variable, values$current_map_data, input$map_level)
        interpretation_lines <- unlist(strsplit(map_interpretation, "\n"))
        interpretation_formatted <- data.frame(
          Interpretasi = interpretation_lines[interpretation_lines != ""]
        )
        
        interpretation_grob <- gridExtra::tableGrob(
          interpretation_formatted,
          theme = gridExtra::ttheme_minimal(
            base_size = 10,
            core = list(fg_params = list(hjust = 0, x = 0.02))
          ),
          rows = NULL
        )
        
        combined_content <- gridExtra::grid.arrange(
          header_grob, interpretation_grob, ncol = 1, heights = c(1, 4)
        )
        
        ggsave(interpretation_file, combined_content, width = 12, height = 14, device = "pdf")
        files_to_zip <- c(files_to_zip, interpretation_file)
        
        summary_file <- file.path(temp_dir, paste0("ringkasan_", input$map_variable, "_", input$map_level, ".txt"))
        summary_text <- paste0(
          "RINGKASAN ANALISIS PETA SPASIAL\n",
          "========================================\n",
          "Tanggal: ", format(Sys.Date(), "%d %B %Y"), "\n",
          "Variabel: ", input$map_variable, "\n",
          "Level Agregasi: ", ifelse(input$map_level == "provinsi", "Provinsi", "Kabupaten/Kota"), "\n\n",
          "File yang disertakan:\n",
          "1. ", basename(map_file), " - Peta interaktif dalam format HTML\n",
          "2. ", basename(interpretation_file), " - Interpretasi lengkap dalam format PDF\n",
          "3. ", basename(summary_file), " - Ringkasan ini\n\n",
          "Cara menggunakan:\n",
          "- Buka file HTML untuk melihat peta interaktif\n",
          "- Baca file PDF untuk analisis mendalam dan rekomendasi\n",
          "- Gunakan untuk perencanaan mitigasi bencana dan alokasi sumber daya"
        )
        writeLines(summary_text, summary_file)
        files_to_zip <- c(files_to_zip, summary_file)
        
        zip(file, files_to_zip, flags = "-j")
        
      } else {
        temp_dir <- tempdir()
        error_file <- file.path(temp_dir, "error.txt")
        writeLines("Error: Peta belum dibuat. Silakan buat peta terlebih dahulu.", error_file)
        zip(file, error_file, flags = "-j")
      }
    }
  )
  
  # NEW DOWNLOAD HANDLERS FOR ASSUMPTION TESTS
  
  # Normality test downloads
  output$download_normality <- downloadHandler(
    filename = function() paste("normality_test_", Sys.Date(), ".xlsx", sep = ""),
    content = function(file) {
      if(!is.null(values$normality_results)) {
        wb <- openxlsx::createWorkbook()
        openxlsx::addWorksheet(wb, "Normality_Results")
        openxlsx::addWorksheet(wb, "Interpretation")
        
        openxlsx::writeData(wb, "Normality_Results", values$normality_results)
        
        if(!is.null(values$normality_interpretation)) {
          openxlsx::writeData(wb, "Interpretation", data.frame(Interpretation = values$normality_interpretation))
        }
        
        openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
      } else {
        openxlsx::write.xlsx(data.frame(Message = "Normality test results not available"), file)
      }
    }
  )
  
  output$download_normality_report <- downloadHandler(
    filename = function() paste("normality_report_", Sys.Date(), ".pdf", sep = ""),
    content = function(file) {
      if(!is.null(values$normality_results) && !is.null(values$normality_plots)) {
        library(gridExtra)
        
        header_text <- paste0(
          "LAPORAN UJI NORMALITAS SIGABISA\n",
          "Tanggal: ", format(Sys.Date(), "%d %B %Y"), "\n",
          "Variabel: ", paste(input$norm_variables, collapse = ", "), "\n",
          "Uji: ", paste(input$norm_tests, collapse = ", "), "\n",
          paste(rep("=", 80), collapse = "")
        )
        
        header_grob <- gridExtra::tableGrob(
          data.frame(Header = header_text),
          theme = gridExtra::ttheme_minimal(base_size = 12),
          rows = NULL, cols = NULL
        )
        
        interpretation_lines <- unlist(strsplit(values$normality_interpretation, "\n"))
        interpretation_formatted <- data.frame(
          Interpretasi = interpretation_lines[interpretation_lines != ""]
        )
        
        interpretation_grob <- gridExtra::tableGrob(
          interpretation_formatted,
          theme = gridExtra::ttheme_minimal(base_size = 10),
          rows = NULL
        )
        
        combined_plot <- gridExtra::grid.arrange(
          header_grob,
          values$normality_plots,
          interpretation_grob,
          ncol = 1,
          heights = c(0.8, 3, 1.5)
        )
        
        ggsave(file, combined_plot, width = 12, height = 16, device = "pdf", dpi = 300)
      }
    }
  )
  
  # Homogeneity test downloads
  output$download_homogeneity <- downloadHandler(
    filename = function() paste("homogeneity_test_", Sys.Date(), ".xlsx", sep = ""),
    content = function(file) {
      if(!is.null(values$homogeneity_results)) {
        wb <- openxlsx::createWorkbook()
        openxlsx::addWorksheet(wb, "Homogeneity_Results")
        openxlsx::addWorksheet(wb, "Interpretation")
        
        # Create summary table
        levene_result <- values$homogeneity_results$levene
        bartlett_result <- values$homogeneity_results$bartlett
        
        results_table <- data.frame(
          Test = c("Levene's Test", "Bartlett's Test"),
          Statistic = c(levene_result$`F value`[1], bartlett_result$statistic),
          P_Value = c(levene_result$`Pr(>F)`[1], bartlett_result$p.value),
          Conclusion = c(
            ifelse(levene_result$`Pr(>F)`[1] > 0.05, "Homogeneous", "Heterogeneous"),
            ifelse(bartlett_result$p.value > 0.05, "Homogeneous", "Heterogeneous")
          )
        )
        
        openxlsx::writeData(wb, "Homogeneity_Results", results_table)
        
        if(!is.null(values$homogeneity_interpretation)) {
          openxlsx::writeData(wb, "Interpretation", data.frame(Interpretation = values$homogeneity_interpretation))
        }
        
        openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
      } else {
        openxlsx::write.xlsx(data.frame(Message = "Homogeneity test results not available"), file)
      }
    }
  )
  
  output$download_homogeneity_report <- downloadHandler(
    filename = function() paste("homogeneity_report_", Sys.Date(), ".pdf", sep = ""),
    content = function(file) {
      if(!is.null(values$homogeneity_results) && !is.null(values$homogeneity_plots)) {
        library(gridExtra)
        
        header_text <- paste0(
          "LAPORAN UJI HOMOGENITAS SIGABISA\n",
          "Tanggal: ", format(Sys.Date(), "%d %B %Y"), "\n",
          "Variabel: ", input$homog_variable, "\n",
          "Pengelompokan: ", input$homog_group, "\n",
          paste(rep("=", 80), collapse = "")
        )
        
        header_grob <- gridExtra::tableGrob(
          data.frame(Header = header_text),
          theme = gridExtra::ttheme_minimal(base_size = 12),
          rows = NULL, cols = NULL
        )
        
        interpretation_lines <- unlist(strsplit(values$homogeneity_interpretation, "\n"))
        interpretation_formatted <- data.frame(
          Interpretasi = interpretation_lines[interpretation_lines != ""]
        )
        
        interpretation_grob <- gridExtra::tableGrob(
          interpretation_formatted,
          theme = gridExtra::ttheme_minimal(base_size = 10),
          rows = NULL
        )
        
        combined_plot <- gridExtra::grid.arrange(
          header_grob,
          values$homogeneity_plots,
          interpretation_grob,
          ncol = 1,
          heights = c(0.8, 3, 1.5)
        )
        
        ggsave(file, combined_plot, width = 12, height = 16, device = "pdf", dpi = 300)
      }
    }
  )
  
  # T-test downloads
  output$download_ttest <- downloadHandler(
    filename = function() paste("ttest_results_", Sys.Date(), ".xlsx", sep = ""),
    content = function(file) {
      if(!is.null(values$ttest_results)) {
        wb <- openxlsx::createWorkbook()
        openxlsx::addWorksheet(wb, "TTest_Results")
        openxlsx::addWorksheet(wb, "Interpretation")
        
        # Create results table
        ttest_summary <- data.frame(
          Statistic = c("Sample Mean", "Hypothesized Mean", "t-statistic", "df", "p-value", "95% CI Lower", "95% CI Upper"),
          Value = c(
            values$ttest_results$estimate,
            input$mu_value,
            values$ttest_results$statistic,
            values$ttest_results$parameter,
            values$ttest_results$p.value,
            values$ttest_results$conf.int[1],
            values$ttest_results$conf.int[2]
          )
        )
        
        openxlsx::writeData(wb, "TTest_Results", ttest_summary)
        
        interpretation_text <- interpret_ttest(values$ttest_results, input$t_variable, input$mu_value)
        openxlsx::writeData(wb, "Interpretation", data.frame(Interpretation = interpretation_text))
        
        openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
      } else {
        openxlsx::write.xlsx(data.frame(Message = "T-test results not available"), file)
      }
    }
  )
  
  output$download_ttest_report <- downloadHandler(
    filename = function() paste("ttest_report_", Sys.Date(), ".pdf", sep = ""),
    content = function(file) {
      if(!is.null(values$ttest_results)) {
        # Create plot for t-test
        current_data <- values$processed_data
        var_data <- current_data[[input$t_variable]]
        var_data_clean <- var_data[!is.na(var_data) & is.finite(var_data)]
        
        hist_data <- data.frame(value = var_data_clean)
        
        p <- ggplot(hist_data, aes(x = value)) +
          geom_histogram(bins = 30, fill = "#dc2626", alpha = 0.7, color = "white") +
          geom_vline(xintercept = mean(var_data_clean), color = "#991b1b", linetype = "solid", size = 1) +
          geom_vline(xintercept = input$mu_value, color = "#059669", linetype = "dashed", size = 1) +
          annotate("text", x = mean(var_data_clean), y = Inf, 
                   label = paste("Sample mean:", round(mean(var_data_clean), 2)), 
                   vjust = 2, color = "#991b1b") +
          annotate("text", x = input$mu_value, y = Inf, 
                   label = paste("H0 value:", input$mu_value), 
                   vjust = 4, color = "#059669") +
          theme_minimal() +
          labs(title = paste("Distribution of", input$t_variable), 
               x = input$t_variable, y = "Frequency")
        
        library(gridExtra)
        
        header_text <- paste0(
          "LAPORAN UJI T SIGABISA\n",
          "Tanggal: ", format(Sys.Date(), "%d %B %Y"), "\n",
          "Variabel: ", input$t_variable, "\n",
          "Hipotesis μ₀: ", input$mu_value, "\n",
          paste(rep("=", 80), collapse = "")
        )
        
        header_grob <- gridExtra::tableGrob(
          data.frame(Header = header_text),
          theme = gridExtra::ttheme_minimal(base_size = 12),
          rows = NULL, cols = NULL
        )
        
        interpretation_text <- interpret_ttest(values$ttest_results, input$t_variable, input$mu_value)
        interpretation_lines <- unlist(strsplit(interpretation_text, "\n"))
        interpretation_formatted <- data.frame(
          Interpretasi = interpretation_lines[interpretation_lines != ""]
        )
        
        interpretation_grob <- gridExtra::tableGrob(
          interpretation_formatted,
          theme = gridExtra::ttheme_minimal(base_size = 10),
          rows = NULL
        )
        
        combined_plot <- gridExtra::grid.arrange(
          header_grob,
          p,
          interpretation_grob,
          ncol = 1,
          heights = c(0.8, 3, 1.5)
        )
        
        ggsave(file, combined_plot, width = 12, height = 16, device = "pdf", dpi = 300)
      }
    }
  )
  
  # ANOVA downloads
  output$download_anova <- downloadHandler(
    filename = function() paste("anova_results_", Sys.Date(), ".xlsx", sep = ""),
    content = function(file) {
      if(!is.null(values$anova_results)) {
        wb <- openxlsx::createWorkbook()
        openxlsx::addWorksheet(wb, "ANOVA_Results")
        openxlsx::addWorksheet(wb, "PostHoc_Results")
        openxlsx::addWorksheet(wb, "Interpretation")
        
        # ANOVA table
        anova_table <- values$anova_results[[1]]
        anova_summary <- data.frame(
          Source = c(input$anova_factor, "Residuals"),
          Df = anova_table$Df,
          Sum_Sq = anova_table$`Sum Sq`,
          Mean_Sq = anova_table$`Mean Sq`,
          F_value = c(anova_table$`F value`[1], NA),
          Pr_F = c(anova_table$`Pr(>F)`[1], NA)
        )
        
        openxlsx::writeData(wb, "ANOVA_Results", anova_summary)
        
        # Post-hoc results
        if(!is.null(values$posthoc_results)) {
          openxlsx::writeData(wb, "PostHoc_Results", values$posthoc_results)
        } else {
          openxlsx::writeData(wb, "PostHoc_Results", data.frame(Message = "Post-hoc test not performed (ANOVA not significant)"))
        }
        
        # Interpretation
        interpretation_text <- interpret_anova(anova_table, values$posthoc_results, input$anova_dependent, input$anova_factor)
        openxlsx::writeData(wb, "Interpretation", data.frame(Interpretation = interpretation_text))
        
        openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
      } else {
        openxlsx::write.xlsx(data.frame(Message = "ANOVA results not available"), file)
      }
    }
  )
  
  output$download_anova_report <- downloadHandler(
    filename = function() paste("anova_report_", Sys.Date(), ".pdf", sep = ""),
    content = function(file) {
      if(!is.null(values$anova_results)) {
        # Create ANOVA plot
        current_data <- values$processed_data
        anova_data <- current_data %>%
          select(!!input$anova_dependent, !!input$anova_factor) %>%
          filter(!is.na(!!sym(input$anova_dependent)), !is.na(!!sym(input$anova_factor)))
        
        p <- ggplot(anova_data, aes_string(x = input$anova_factor, y = input$anova_dependent)) +
          geom_boxplot(fill = "#dc2626", alpha = 0.7, outlier.color = "#991b1b") +
          geom_point(position = position_jitter(width = 0.2), alpha = 0.5, color = "#059669") +
          stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "white", color = "black") +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
          labs(title = paste("ANOVA:", input$anova_dependent, "by", input$anova_factor),
               x = input$anova_factor, y = input$anova_dependent)
        
        library(gridExtra)
        
        header_text <- paste0(
          "LAPORAN ANOVA SIGABISA\n",
          "Tanggal: ", format(Sys.Date(), "%d %B %Y"), "\n",
          "Variabel Terikat: ", input$anova_dependent, "\n",
          "Faktor: ", input$anova_factor, "\n",
          paste(rep("=", 80), collapse = "")
        )
        
        header_grob <- gridExtra::tableGrob(
          data.frame(Header = header_text),
          theme = gridExtra::ttheme_minimal(base_size = 12),
          rows = NULL, cols = NULL
        )
        
        interpretation_text <- interpret_anova(values$anova_results[[1]], values$posthoc_results, 
                                               input$anova_dependent, input$anova_factor)
        interpretation_lines <- unlist(strsplit(interpretation_text, "\n"))
        interpretation_formatted <- data.frame(
          Interpretasi = interpretation_lines[interpretation_lines != ""]
        )
        
        interpretation_grob <- gridExtra::tableGrob(
          interpretation_formatted,
          theme = gridExtra::ttheme_minimal(base_size = 10),
          rows = NULL
        )
        
        combined_plot <- gridExtra::grid.arrange(
          header_grob,
          p,
          interpretation_grob,
          ncol = 1,
          heights = c(0.8, 3, 1.5)
        )
        
        ggsave(file, combined_plot, width = 12, height = 16, device = "pdf", dpi = 300)
      }
    }
  )
  
  # Regression downloads
  output$download_model <- downloadHandler(
    filename = function() paste("regression_model_", Sys.Date(), ".xlsx", sep = ""),
    content = function(file) {
      if(!is.null(values$regression_model)) {
        wb <- openxlsx::createWorkbook()
        openxlsx::addWorksheet(wb, "Model_Summary")
        openxlsx::addWorksheet(wb, "Coefficients")
        openxlsx::addWorksheet(wb, "Interpretation")
        
        # Model summary
        model_summary <- summary(values$regression_model)
        summary_stats <- data.frame(
          Statistic = c("R-squared", "Adjusted R-squared", "F-statistic", "p-value", "Residual SE", "Observations"),
          Value = c(
            model_summary$r.squared,
            model_summary$adj.r.squared,
            model_summary$fstatistic[1],
            pf(model_summary$fstatistic[1], model_summary$fstatistic[2], model_summary$fstatistic[3], lower.tail = FALSE),
            model_summary$sigma,
            nobs(values$regression_model)
          )
        )
        
        openxlsx::writeData(wb, "Model_Summary", summary_stats)
        
        # Coefficients
        coeffs_table <- data.frame(
          Variable = rownames(model_summary$coefficients),
          Estimate = model_summary$coefficients[, "Estimate"],
          Std_Error = model_summary$coefficients[, "Std. Error"],
          t_value = model_summary$coefficients[, "t value"],
          p_value = model_summary$coefficients[, "Pr(>|t|)"],
          Significant = model_summary$coefficients[, "Pr(>|t|)"] < 0.05
        )
        
        openxlsx::writeData(wb, "Coefficients", coeffs_table)
        
        # Interpretation
        interpretation_text <- interpret_regression(values$regression_model, values$regression_assumptions)
        openxlsx::writeData(wb, "Interpretation", data.frame(Interpretation = interpretation_text))
        
        openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
      } else {
        openxlsx::write.xlsx(data.frame(Message = "Regression model not available"), file)
      }
    }
  )
  
  output$download_regression_assumptions <- downloadHandler(
    filename = function() paste("regression_assumptions_", Sys.Date(), ".xlsx", sep = ""),
    content = function(file) {
      if(!is.null(values$regression_assumptions)) {
        wb <- openxlsx::createWorkbook()
        openxlsx::addWorksheet(wb, "Assumptions_Summary")
        openxlsx::addWorksheet(wb, "Interpretation")
        
        # Assumptions summary
        assumptions_summary <- data.frame(
          Assumption = c("Normality", "Homoscedasticity", "Independence", "Multicollinearity"),
          Result = c(
            values$regression_assumptions$normality,
            values$regression_assumptions$homoscedasticity,
            values$regression_assumptions$autocorrelation,
            values$regression_assumptions$multicollinearity
          )
        )
        
        openxlsx::writeData(wb, "Assumptions_Summary", assumptions_summary)
        
        # Interpretation would be added here
        interpretation_text <- paste0(
          "INTERPRETASI UJI ASUMSI REGRESI\n\n",
          "1. NORMALITAS RESIDUAL: ", values$regression_assumptions$normality, "\n",
          "2. HOMOSKEDASTISITAS: ", values$regression_assumptions$homoscedasticity, "\n",
          "3. INDEPENDENSI: ", values$regression_assumptions$autocorrelation, "\n",
          "4. MULTIKOLINEARITAS: ", values$regression_assumptions$multicollinearity
        )
        
        openxlsx::writeData(wb, "Interpretation", data.frame(Interpretation = interpretation_text))
        
        openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
      } else {
        openxlsx::write.xlsx(data.frame(Message = "Regression assumptions test not performed"), file)
      }
    }
  )
  
  output$download_regression_assumptions_report <- downloadHandler(
    filename = function() paste("regression_assumptions_report_", Sys.Date(), ".pdf", sep = ""),
    content = function(file) {
      if(!is.null(values$regression_model) && !is.null(values$regression_assumptions)) {
        # Create assumptions plots (similar to what's shown in the UI)
        model <- values$regression_model
        residuals <- residuals(model)
        fitted_values <- fitted(model)
        
        assumptions_data <- data.frame(
          fitted = fitted_values,
          residuals = residuals,
          std_residuals = scale(residuals),
          sqrt_std_residuals = sqrt(abs(scale(residuals)))
        )
        
        # Create 4 diagnostic plots
        p1 <- ggplot(assumptions_data, aes(x = fitted, y = residuals)) +
          geom_point(alpha = 0.6, color = "#dc2626") +
          geom_hline(yintercept = 0, linetype = "dashed", color = "#991b1b") +
          geom_smooth(se = FALSE, color = "#059669") +
          theme_minimal() +
          labs(title = "Residuals vs Fitted", x = "Fitted Values", y = "Residuals")
        
        p2 <- ggplot(assumptions_data, aes(sample = std_residuals)) +
          stat_qq(color = "#dc2626", alpha = 0.6) +
          stat_qq_line(color = "#991b1b") +
          theme_minimal() +
          labs(title = "Normal Q-Q Plot", x = "Theoretical Quantiles", y = "Standardized Residuals")
        
        p3 <- ggplot(assumptions_data, aes(x = fitted, y = sqrt_std_residuals)) +
          geom_point(alpha = 0.6, color = "#dc2626") +
          geom_smooth(se = FALSE, color = "#059669") +
          theme_minimal() +
          labs(title = "Scale-Location", x = "Fitted Values", y = "√|Standardized Residuals|")
        
        assumptions_data$order <- 1:nrow(assumptions_data)
        p4 <- ggplot(assumptions_data, aes(x = order, y = residuals)) +
          geom_point(alpha = 0.6, color = "#dc2626") +
          geom_hline(yintercept = 0, linetype = "dashed", color = "#991b1b") +
          geom_smooth(se = FALSE, color = "#059669") +
          theme_minimal() +
          labs(title = "Residuals vs Order", x = "Observation Order", y = "Residuals")
        
        # Combine plots
        combined_plots <- gridExtra::grid.arrange(p1, p2, p3, p4, ncol = 2)
        
        library(gridExtra)
        
        header_text <- paste0(
          "LAPORAN UJI ASUMSI REGRESI SIGABISA\n",
          "Tanggal: ", format(Sys.Date(), "%d %B %Y"), "\n",
          "Model: ", deparse(formula(values$regression_model)), "\n",
          paste(rep("=", 80), collapse = "")
        )
        
        header_grob <- gridExtra::tableGrob(
          data.frame(Header = header_text),
          theme = gridExtra::ttheme_minimal(base_size = 12),
          rows = NULL, cols = NULL
        )
        
        interpretation_text <- paste0(
          "INTERPRETASI UJI ASUMSI REGRESI\n\n",
          "1. NORMALITAS RESIDUAL: ", values$regression_assumptions$normality, "\n",
          "2. HOMOSKEDASTISITAS: ", values$regression_assumptions$homoscedasticity, "\n",
          "3. INDEPENDENSI: ", values$regression_assumptions$autocorrelation, "\n",
          "4. MULTIKOLINEARITAS: ", values$regression_assumptions$multicollinearity
        )
        
        interpretation_lines <- unlist(strsplit(interpretation_text, "\n"))
        interpretation_formatted <- data.frame(
          Interpretasi = interpretation_lines[interpretation_lines != ""]
        )
        
        interpretation_grob <- gridExtra::tableGrob(
          interpretation_formatted,
          theme = gridExtra::ttheme_minimal(base_size = 10),
          rows = NULL
        )
        
        final_plot <- gridExtra::grid.arrange(
          header_grob,
          combined_plots,
          interpretation_grob,
          ncol = 1,
          heights = c(0.8, 4, 1.5)
        )
        
        ggsave(file, final_plot, width = 14, height = 18, device = "pdf", dpi = 300)
      }
    }
  )
  
  output$download_diagnostic <- downloadHandler(
    filename = function() paste("diagnostic_plots_", Sys.Date(), ".png", sep = ""),
    content = function(file) {
      if(!is.null(values$regression_model)) {
        model <- values$regression_model
        
        diagnostic_data <- data.frame(
          fitted = fitted(model),
          residuals = residuals(model),
          std_residuals = rstandard(model),
          leverage = hatvalues(model),
          cooks_distance = cooks.distance(model)
        )
        
        p1 <- ggplot(diagnostic_data, aes(x = fitted, y = residuals)) +
          geom_point(alpha = 0.6, color = "#dc2626") +
          geom_hline(yintercept = 0, linetype = "dashed") +
          geom_smooth(se = FALSE, color = "#059669") +
          theme_minimal() +
          labs(title = "Residuals vs Fitted", x = "Fitted Values", y = "Residuals")
        
        p2 <- ggplot(diagnostic_data, aes(sample = std_residuals)) +
          stat_qq(color = "#dc2626", alpha = 0.6) +
          stat_qq_line(color = "#991b1b") +
          theme_minimal() +
          labs(title = "Normal Q-Q", x = "Theoretical Quantiles", y = "Standardized Residuals")
        
        p3 <- ggplot(diagnostic_data, aes(x = fitted, y = sqrt(abs(std_residuals)))) +
          geom_point(alpha = 0.6, color = "#dc2626") +
          geom_smooth(se = FALSE, color = "#059669") +
          theme_minimal() +
          labs(title = "Scale-Location", x = "Fitted Values", y = "√|Standardized Residuals|")
        
        diagnostic_data$index <- 1:nrow(diagnostic_data)
        p4 <- ggplot(diagnostic_data, aes(x = index, y = cooks_distance)) +
          geom_point(alpha = 0.6, color = "#dc2626") +
          geom_hline(yintercept = 4/nrow(diagnostic_data), linetype = "dashed", color = "#991b1b") +
          theme_minimal() +
          labs(title = "Cook's Distance", x = "Observation Index", y = "Cook's Distance")
        
        combined_plot <- gridExtra::grid.arrange(p1, p2, p3, p4, ncol = 2)
        ggsave(file, combined_plot, width = 12, height = 10, dpi = 300)
      }
    }
  )
  
  output$download_diagnostic_report <- downloadHandler(
    filename = function() paste("diagnostic_report_", Sys.Date(), ".pdf", sep = ""),
    content = function(file) {
      if(!is.null(values$regression_model)) {
        # Similar to download_diagnostic but as PDF with interpretation
        model <- values$regression_model
        
        diagnostic_data <- data.frame(
          fitted = fitted(model),
          residuals = residuals(model),
          std_residuals = rstandard(model),
          leverage = hatvalues(model),
          cooks_distance = cooks.distance(model)
        )
        
        # Create plots (same as above)
        p1 <- ggplot(diagnostic_data, aes(x = fitted, y = residuals)) +
          geom_point(alpha = 0.6, color = "#dc2626") +
          geom_hline(yintercept = 0, linetype = "dashed") +
          geom_smooth(se = FALSE, color = "#059669") +
          theme_minimal() +
          labs(title = "Residuals vs Fitted", x = "Fitted Values", y = "Residuals")
        
        p2 <- ggplot(diagnostic_data, aes(sample = std_residuals)) +
          stat_qq(color = "#dc2626", alpha = 0.6) +
          stat_qq_line(color = "#991b1b") +
          theme_minimal() +
          labs(title = "Normal Q-Q", x = "Theoretical Quantiles", y = "Standardized Residuals")
        
        p3 <- ggplot(diagnostic_data, aes(x = fitted, y = sqrt(abs(std_residuals)))) +
          geom_point(alpha = 0.6, color = "#dc2626") +
          geom_smooth(se = FALSE, color = "#059669") +
          theme_minimal() +
          labs(title = "Scale-Location", x = "Fitted Values", y = "√|Standardized Residuals|")
        
        diagnostic_data$index <- 1:nrow(diagnostic_data)
        p4 <- ggplot(diagnostic_data, aes(x = index, y = cooks_distance)) +
          geom_point(alpha = 0.6, color = "#dc2626") +
          geom_hline(yintercept = 4/nrow(diagnostic_data), linetype = "dashed", color = "#991b1b") +
          theme_minimal() +
          labs(title = "Cook's Distance", x = "Observation Index", y = "Cook's Distance")
        
        combined_plots <- gridExtra::grid.arrange(p1, p2, p3, p4, ncol = 2)
        
        library(gridExtra)
        
        header_text <- paste0(
          "LAPORAN DIAGNOSTIK REGRESI SIGABISA\n",
          "Tanggal: ", format(Sys.Date(), "%d %B %Y"), "\n",
          "Model: ", deparse(formula(values$regression_model)), "\n",
          paste(rep("=", 80), collapse = "")
        )
        
        header_grob <- gridExtra::tableGrob(
          data.frame(Header = header_text),
          theme = gridExtra::ttheme_minimal(base_size = 12),
          rows = NULL, cols = NULL
        )
        
        # Generate diagnostic interpretation
        influential_obs <- which(diagnostic_data$cooks_distance > 4/nrow(diagnostic_data))
        high_leverage <- which(diagnostic_data$leverage > 2*length(model$coefficients)/nrow(diagnostic_data))
        
        interpretation_text <- paste0(
          "INTERPRETASI DIAGNOSTIK MODEL\n\n",
          "1. RESIDUALS vs FITTED:\n",
          "Pola acak menunjukkan linearitas yang baik.\n",
          "Pola kurvilinear menunjukkan non-linearitas.\n\n",
          
          "2. NORMAL Q-Q PLOT:\n",
          "Titik-titik mengikuti garis menunjukkan normalitas residual.\n\n",
          
          "3. SCALE-LOCATION:\n",
          "Penyebaran acak menunjukkan homoskedastisitas.\n\n",
          
          "4. COOK'S DISTANCE:\n",
          "Observasi berpengaruh: ", ifelse(length(influential_obs) > 0, 
                                            paste(influential_obs, collapse = ", "), "Tidak ada"), "\n",
          "High leverage points: ", ifelse(length(high_leverage) > 0,
                                           paste(high_leverage, collapse = ", "), "Tidak ada"), "\n\n",
          
          "REKOMENDASI:\n",
          ifelse(length(influential_obs) > 0,
                 "⚠ Periksa observasi berpengaruh dan pertimbangkan untuk menghapus outlier.",
                 "✓ Tidak ada observasi yang sangat berpengaruh.")
        )
        
        interpretation_lines <- unlist(strsplit(interpretation_text, "\n"))
        interpretation_formatted <- data.frame(
          Interpretasi = interpretation_lines[interpretation_lines != ""]
        )
        
        interpretation_grob <- gridExtra::tableGrob(
          interpretation_formatted,
          theme = gridExtra::ttheme_minimal(base_size = 10),
          rows = NULL
        )
        
        final_plot <- gridExtra::grid.arrange(
          header_grob,
          combined_plots,
          interpretation_grob,
          ncol = 1,
          heights = c(0.8, 4, 1.5)
        )
        
        ggsave(file, final_plot, width = 14, height = 18, device = "pdf", dpi = 300)
      }
    }
  )
  
  output$download_prediction <- downloadHandler(
    filename = function() paste("prediction_results_", Sys.Date(), ".xlsx", sep = ""),
    content = function(file) {
      if(!is.null(values$regression_model)) {
        wb <- openxlsx::createWorkbook()
        openxlsx::addWorksheet(wb, "Prediction_Summary")
        openxlsx::addWorksheet(wb, "Model_Performance")
        
        # This would contain prediction results if they exist
        openxlsx::writeData(wb, "Prediction_Summary", data.frame(Message = "Make a prediction first to download results"))
        
        # Model performance metrics
        model <- values$regression_model
        model_summary <- summary(model)
        
        performance <- data.frame(
          Metric = c("R-squared", "Adjusted R-squared", "RMSE", "MAE"),
          Value = c(
            model_summary$r.squared,
            model_summary$adj.r.squared,
            sqrt(mean(residuals(model)^2)),
            mean(abs(residuals(model)))
          )
        )
        
        openxlsx::writeData(wb, "Model_Performance", performance)
        
        openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
      } else {
        openxlsx::write.xlsx(data.frame(Message = "Regression model not available"), file)
      }
    }
  )
  
  # Clustering downloads
  output$download_clustering <- downloadHandler(
    filename = function() paste("clustering_results_", Sys.Date(), ".xlsx", sep = ""),
    content = function(file) {
      if(!is.null(values$kmeans_result) && !is.null(values$cluster_data)) {
        wb <- openxlsx::createWorkbook()
        openxlsx::addWorksheet(wb, "Cluster_Results")
        openxlsx::addWorksheet(wb, "Cluster_Summary")
        openxlsx::addWorksheet(wb, "Interpretation")
        
        # Cluster results
        openxlsx::writeData(wb, "Cluster_Results", values$cluster_data)
        
        # Cluster summary
        cluster_summary <- values$cluster_data %>%
          group_by(cluster) %>%
          summarise(
            Count = n(),
            across(where(is.numeric), mean, na.rm = TRUE),
            .groups = 'drop'
          )
        
        openxlsx::writeData(wb, "Cluster_Summary", cluster_summary)
        
        # Interpretation
        interpretation_text <- interpret_clustering(values$kmeans_result, NULL, "kmeans")
        openxlsx::writeData(wb, "Interpretation", data.frame(Interpretation = interpretation_text))
        
        openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
      } else {
        openxlsx::write.xlsx(data.frame(Message = "K-means clustering results not available"), file)
      }
    }
  )
  
  output$download_clustering_report <- downloadHandler(
    filename = function() paste("clustering_report_", Sys.Date(), ".pdf", sep = ""),
    content = function(file) {
      if(!is.null(values$kmeans_result) && !is.null(values$cluster_data)) {
        # Create elbow plot
        cluster_vars <- intersect(input$cluster_variables, names(values$cluster_data))
        scaled_data <- scale(values$cluster_data[, cluster_vars])
        
        wss <- sapply(1:8, function(k) {
          kmeans(scaled_data, centers = k, nstart = 25)$tot.withinss
        })
        
        elbow_data <- data.frame(k = 1:8, wss = wss)
        
        elbow_plot <- ggplot(elbow_data, aes(x = k, y = wss)) +
          geom_line(color = "#dc2626", size = 1) +
          geom_point(color = "#991b1b", size = 3) +
          theme_minimal() +
          labs(title = "Elbow Method For Optimal k", 
               x = "Number of Clusters (k)", y = "Within-cluster Sum of Squares")
        
        # Create cluster distribution plot
        cluster_dist <- ggplot(values$cluster_data, aes(x = cluster, fill = cluster)) +
          geom_bar(alpha = 0.7) +
          scale_fill_brewer(type = "qual", palette = "Set2") +
          theme_minimal() +
          labs(title = "Cluster Distribution", x = "Cluster", y = "Count") +
          theme(legend.position = "none")
        
        library(gridExtra)
        
        header_text <- paste0(
          "LAPORAN K-MEANS CLUSTERING SIGABISA\n",
          "Tanggal: ", format(Sys.Date(), "%d %B %Y"), "\n",
          "Variabel: ", paste(input$cluster_variables, collapse = ", "), "\n",
          "Jumlah Cluster: ", length(unique(values$cluster_data$cluster)), "\n",
          paste(rep("=", 80), collapse = "")
        )
        
        header_grob <- gridExtra::tableGrob(
          data.frame(Header = header_text),
          theme = gridExtra::ttheme_minimal(base_size = 12),
          rows = NULL, cols = NULL
        )
        
        # Combine plots
        combined_plots <- gridExtra::grid.arrange(elbow_plot, cluster_dist, ncol = 2)
        
        # Interpretation
        interpretation_text <- interpret_clustering(values$kmeans_result, scaled_data, "kmeans")
        interpretation_lines <- unlist(strsplit(interpretation_text, "\n"))
        interpretation_formatted <- data.frame(
          Interpretasi = interpretation_lines[interpretation_lines != ""]
        )
        
        interpretation_grob <- gridExtra::tableGrob(
          interpretation_formatted,
          theme = gridExtra::ttheme_minimal(base_size = 10),
          rows = NULL
        )
        
        final_plot <- gridExtra::grid.arrange(
          header_grob,
          combined_plots,
          interpretation_grob,
          ncol = 1,
          heights = c(0.8, 3, 2)
        )
        
        ggsave(file, final_plot, width = 14, height = 16, device = "pdf", dpi = 300)
      }
    }
  )
  
  output$download_fgwc <- downloadHandler(
    filename = function() paste("fgwc_results_", Sys.Date(), ".xlsx", sep = ""),
    content = function(file) {
      if(!is.null(values$fgwc_result)) {
        wb <- openxlsx::createWorkbook()
        openxlsx::addWorksheet(wb, "FGWC_Results")
        openxlsx::addWorksheet(wb, "Membership_Matrix")
        openxlsx::addWorksheet(wb, "Cluster_Centers")
        openxlsx::addWorksheet(wb, "Interpretation")
        
        # FGWC cluster assignments
        current_data <- values$processed_data
        fgwc_data <- current_data %>%
          select(DISTRICTCODE, REGION, all_of(intersect(input$fgwc_variables, names(current_data)))) %>%
          na.omit()
        
        fgwc_data$cluster <- as.factor(values$fgwc_result$clusters)
        openxlsx::writeData(wb, "FGWC_Results", fgwc_data)
        
        # Membership matrix
        membership_data <- data.frame(
          DISTRICTCODE = fgwc_data$DISTRICTCODE,
          values$fgwc_result$membership
        )
        colnames(membership_data)[-1] <- paste0("Cluster_", 1:input$fgwc_k)
        openxlsx::writeData(wb, "Membership_Matrix", membership_data)
        
        # Cluster centers
        centers_data <- data.frame(
          Cluster = 1:nrow(values$fgwc_result$centers),
          values$fgwc_result$centers
        )
        openxlsx::writeData(wb, "Cluster_Centers", centers_data)
        
        # Interpretation
        interpretation_text <- interpret_clustering(values$fgwc_result, NULL, "fgwc")
        openxlsx::writeData(wb, "Interpretation", data.frame(Interpretation = interpretation_text))
        
        openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
      } else {
        openxlsx::write.xlsx(data.frame(Message = "FGWC clustering results not available"), file)
      }
    }
  )
  
  output$download_fgwc_report <- downloadHandler(
    filename = function() paste("fgwc_report_", Sys.Date(), ".pdf", sep = ""),
    content = function(file) {
      if(!is.null(values$fgwc_result)) {
        # Create FGWC specific plots
        current_data <- values$processed_data
        fgwc_data <- current_data %>%
          select(DISTRICTCODE, REGION, all_of(intersect(input$fgwc_variables, names(current_data)))) %>%
          na.omit()
        
        fgwc_data$cluster <- as.factor(values$fgwc_result$clusters)
        
        # Membership uncertainty plot
        max_membership <- apply(values$fgwc_result$membership, 1, max)
        uncertainty <- 1 - max_membership
        
        uncertainty_data <- data.frame(
          index = 1:length(uncertainty),
          uncertainty = uncertainty,
          cluster = fgwc_data$cluster
        )
        
        uncertainty_plot <- ggplot(uncertainty_data, aes(x = index, y = uncertainty, color = cluster)) +
          geom_point(alpha = 0.7) +
          scale_color_brewer(type = "qual", palette = "Set3") +
          theme_minimal() +
          labs(title = "FGWC Membership Uncertainty", 
               x = "Observation Index", y = "Uncertainty (1 - Max Membership)")
        
        # Cluster distribution
        cluster_dist <- ggplot(fgwc_data, aes(x = cluster, fill = cluster)) +
          geom_bar(alpha = 0.7) +
          scale_fill_brewer(type = "qual", palette = "Set3") +
          theme_minimal() +
          labs(title = "FGWC Cluster Distribution", x = "Cluster", y = "Count") +
          theme(legend.position = "none")
        
        library(gridExtra)
        
        header_text <- paste0(
          "LAPORAN FGWC CLUSTERING SIGABISA\n",
          "Tanggal: ", format(Sys.Date(), "%d %B %Y"), "\n",
          "Variabel: ", paste(input$fgwc_variables, collapse = ", "), "\n",
          "Jumlah Cluster: ", input$fgwc_k, "\n",
          "Fuzziness Parameter (m): ", input$fgwc_m, "\n",
          "Iterasi: ", values$fgwc_result$iterations, "\n",
          paste(rep("=", 80), collapse = "")
        )
        
        header_grob <- gridExtra::tableGrob(
          data.frame(Header = header_text),
          theme = gridExtra::ttheme_minimal(base_size = 12),
          rows = NULL, cols = NULL
        )
        
        # Combine plots
        combined_plots <- gridExtra::grid.arrange(uncertainty_plot, cluster_dist, ncol = 1)
        
        # Interpretation
        interpretation_text <- interpret_clustering(values$fgwc_result, NULL, "fgwc")
        interpretation_lines <- unlist(strsplit(interpretation_text, "\n"))
        interpretation_formatted <- data.frame(
          Interpretasi = interpretation_lines[interpretation_lines != ""]
        )
        
        interpretation_grob <- gridExtra::tableGrob(
          interpretation_formatted,
          theme = gridExtra::ttheme_minimal(base_size = 10),
          rows = NULL
        )
        
        final_plot <- gridExtra::grid.arrange(
          header_grob,
          combined_plots,
          interpretation_grob,
          ncol = 1,
          heights = c(1, 3, 2)
        )
        
        ggsave(file, final_plot, width = 12, height = 16, device = "pdf", dpi = 300)
      }
    }
  )
  
  output$download_cluster_viz <- downloadHandler(
    filename = function() paste("cluster_visualization_", Sys.Date(), ".png", sep = ""),
    content = function(file) {
      if(!is.null(values$kmeans_result) && !is.null(values$fgwc_result)) {
        # Create silhouette comparison visualization instead
        if(!is.null(values$silhouette_comparison)) {
          # Create silhouette comparison plot for download
          sil_kmeans <- values$silhouette_comparison$kmeans
          kmeans_df <- data.frame(
            cluster = factor(sil_kmeans[, 1]),
            silhouette_width = sil_kmeans[, 3],
            method = "K-Means"
          )
          
          sil_fgwc <- values$silhouette_comparison$fgwc
          fgwc_df <- data.frame(
            cluster = factor(sil_fgwc[, 1]),
            silhouette_width = sil_fgwc[, 3],
            method = "FGWC"
          )
          
          combined_df <- rbind(kmeans_df, fgwc_df)
          combined_df$observation <- rep(1:nrow(kmeans_df), 2)
          
          p <- ggplot(combined_df, aes(x = reorder(observation, silhouette_width), 
                                       y = silhouette_width, fill = cluster)) +
            geom_col() +
            facet_wrap(~method, scales = "free_x") +
            coord_flip() +
            scale_fill_brewer(type = "qual", palette = "Set2") +
            theme_minimal() +
            labs(title = "Silhouette Analysis Comparison: K-Means vs FGWC",
                 subtitle = paste("K-Means avg:", round(values$silhouette_comparison$kmeans_avg, 3),
                                  "| FGWC avg:", round(values$silhouette_comparison$fgwc_avg, 3)),
                 x = "Districts (sorted by silhouette width)", 
                 y = "Silhouette Width",
                 fill = "Cluster")
          
          ggsave(file, p, width = 12, height = 8, dpi = 300)
        }
      } else if(!is.null(values$kmeans_result)) {
        # Create k-means only visualization
        cluster_vars <- intersect(input$cluster_variables, names(values$cluster_data))
        
        if(length(cluster_vars) >= 2) {
          # Create scatter plot with first two variables
          p <- ggplot(values$cluster_data, aes_string(x = cluster_vars[1], y = cluster_vars[2], color = "cluster")) +
            geom_point(alpha = 0.7, size = 2) +
            scale_color_brewer(type = "qual", palette = "Set2") +
            theme_minimal() +
            labs(title = "K-Means Clustering Visualization",
                 x = cluster_vars[1], y = cluster_vars[2])
          
          ggsave(file, p, width = 10, height = 8, dpi = 300)
        }
      } else {
        # Create placeholder image
        png(file, width = 800, height = 600)
        plot(1, type = "n", axes = FALSE, xlab = "", ylab = "", main = "Cluster visualization not available")
        text(1, 1, "Please run clustering analysis first", cex = 1.5, col = "red")
        dev.off()
      }
    }
  )
  
  output$download_cluster_complete <- downloadHandler(
    filename = function() paste("clustering_complete_", Sys.Date(), ".zip", sep = ""),
    content = function(file) {
      temp_dir <- tempdir()
      files_to_zip <- c()
      
      # K-means results
      if(!is.null(values$kmeans_result) && !is.null(values$cluster_data)) {
        kmeans_file <- file.path(temp_dir, "kmeans_results.xlsx")
        
        wb <- openxlsx::createWorkbook()
        openxlsx::addWorksheet(wb, "Cluster_Results")
        openxlsx::addWorksheet(wb, "Cluster_Summary")
        
        openxlsx::writeData(wb, "Cluster_Results", values$cluster_data)
        
        cluster_summary <- values$cluster_data %>%
          group_by(cluster) %>%
          summarise(
            Count = n(),
            across(where(is.numeric), mean, na.rm = TRUE),
            .groups = 'drop'
          )
        
        openxlsx::writeData(wb, "Cluster_Summary", cluster_summary)
        openxlsx::saveWorkbook(wb, kmeans_file, overwrite = TRUE)
        files_to_zip <- c(files_to_zip, kmeans_file)
      }
      
      # FGWC results
      if(!is.null(values$fgwc_result)) {
        fgwc_file <- file.path(temp_dir, "fgwc_results.xlsx")
        
        current_data <- values$processed_data
        fgwc_data <- current_data %>%
          select(DISTRICTCODE, REGION, all_of(intersect(input$fgwc_variables, names(current_data)))) %>%
          na.omit()
        
        fgwc_data$cluster <- as.factor(values$fgwc_result$clusters)
        
        wb2 <- openxlsx::createWorkbook()
        openxlsx::addWorksheet(wb2, "FGWC_Results")
        openxlsx::addWorksheet(wb2, "Membership_Matrix")
        
        openxlsx::writeData(wb2, "FGWC_Results", fgwc_data)
        
        membership_data <- data.frame(
          DISTRICTCODE = fgwc_data$DISTRICTCODE,
          values$fgwc_result$membership
        )
        colnames(membership_data)[-1] <- paste0("Cluster_", 1:input$fgwc_k)
        openxlsx::writeData(wb2, "Membership_Matrix", membership_data)
        
        openxlsx::saveWorkbook(wb2, fgwc_file, overwrite = TRUE)
        files_to_zip <- c(files_to_zip, fgwc_file)
      }
      
      # Visualization
      if(!is.null(values$kmeans_result)) {
        viz_file <- file.path(temp_dir, "cluster_visualization.png")
        
        cluster_vars <- intersect(input$cluster_variables, names(values$cluster_data))
        
        if(length(cluster_vars) >= 2) {
          p <- ggplot(values$cluster_data, aes_string(x = cluster_vars[1], y = cluster_vars[2], color = "cluster")) +
            geom_point(alpha = 0.7, size = 2) +
            scale_color_brewer(type = "qual", palette = "Set2") +
            theme_minimal() +
            labs(title = "K-Means Clustering Visualization",
                 x = cluster_vars[1], y = cluster_vars[2])
          
          ggsave(viz_file, p, width = 10, height = 8, dpi = 300)
          files_to_zip <- c(files_to_zip, viz_file)
        }
      }
      
      # Summary report
      summary_file <- file.path(temp_dir, "clustering_summary.txt")
      summary_text <- paste0(
        "RINGKASAN ANALISIS CLUSTERING SIGABISA\n",
        "=====================================\n",
        "Tanggal: ", format(Sys.Date(), "%d %B %Y"), "\n\n",
        
        ifelse(!is.null(values$kmeans_result),
               paste0("K-MEANS CLUSTERING:\n",
                      "- Jumlah cluster: ", length(unique(values$cluster_data$cluster)), "\n",
                      "- Variabel: ", paste(input$cluster_variables, collapse = ", "), "\n",
                      "- Total observasi: ", nrow(values$cluster_data), "\n\n"),
               ""),
        
        ifelse(!is.null(values$fgwc_result),
               paste0("FGWC CLUSTERING:\n",
                      "- Jumlah cluster: ", input$fgwc_k, "\n",
                      "- Fuzziness parameter: ", input$fgwc_m, "\n",
                      "- Iterasi konvergen: ", values$fgwc_result$iterations, "\n",
                      "- Variabel: ", paste(input$fgwc_variables, collapse = ", "), "\n\n"),
               ""),
        
        "FILE YANG DISERTAKAN:\n",
        "- kmeans_results.xlsx: Hasil K-Means clustering\n",
        "- fgwc_results.xlsx: Hasil FGWC clustering\n",
        "- cluster_visualization.png: Visualisasi cluster\n",
        "- clustering_summary.txt: Ringkasan ini\n\n",
        
        "INTERPRETASI SINGKAT:\n",
        "- K-Means: Clustering berdasarkan kemiripan karakteristik sosial\n",
        "- FGWC: Clustering dengan mempertimbangkan kedekatan geografis\n",
        "- Gunakan hasil untuk strategi mitigasi bencana yang spesifik per cluster\n"
      )
      
      writeLines(summary_text, summary_file)
      files_to_zip <- c(files_to_zip, summary_file)
      
      # Create ZIP file
      if(length(files_to_zip) > 0) {
        zip(file, files_to_zip, flags = "-j")
      } else {
        # Create empty zip with error message
        error_file <- file.path(temp_dir, "error.txt")
        writeLines("Error: No clustering results available. Please run clustering analysis first.", error_file)
        zip(file, error_file, flags = "-j")
      }
    }
  )
  
  # Remaining placeholder downloads for management section
  output$download_summary <- downloadHandler(
    filename = function() paste("summary_", Sys.Date(), ".txt", sep = ""),
    content = function(file) {
      current_data <- if(is.null(values$processed_data)) sovi_data else values$processed_data
      
      summary_text <- paste0(
        "SIGABISA Dashboard - Data Summary\n",
        "Generated on: ", Sys.Date(), "\n\n",
        "Dataset Information:\n",
        "- Total Districts: ", nrow(current_data), "\n",
        "- Variables: ", ncol(current_data)-2, "\n",
        "- Regions: ", length(unique(current_data$REGION)), "\n\n",
        "Regional Distribution:\n",
        paste(capture.output(print(table(current_data$REGION))), collapse = "\n"), "\n\n",
        
        "Data Processing Status:\n",
        "- Original data: ", nrow(sovi_data), " observations\n",
        "- Processed data: ", nrow(current_data), " observations\n",
        "- Data reduction: ", nrow(sovi_data) - nrow(current_data), " observations\n\n",
        
        "Variable Summary:\n",
        "Original variables: ", length(intersect(numeric_variables, names(sovi_data))), "\n",
        "Current variables: ", length(names(current_data)), "\n",
        "Added variables: ", length(names(current_data)) - length(intersect(numeric_variables, names(sovi_data))), "\n"
      )
      writeLines(summary_text, file)
    }
  )
  
  # Enhanced download all management
  output$download_all_management <- downloadHandler(
    filename = function() paste("SIGABISA_Data_Keseluruhan_", Sys.Date(), ".zip", sep = ""),
    content = function(file) {
      temp_dir <- tempdir()
      files <- c()
      
      # Create main header file
      header_file <- file.path(temp_dir, "README_Data_Keseluruhan.txt")
      header_content <- paste0(
        "SIGABISA - DATA KESELURUHAN\n",
        "===============================================\n",
        "Sistem Informasi Geospasial Ancaman Bencana\n",
        "berbasis Indikator Sosial\n",
        "===============================================\n\n",
        "Tanggal Export: ", format(Sys.Date(), "%d %B %Y"), "\n",
        "Waktu Export: ", format(Sys.time(), "%H:%M:%S WIB"), "\n",
        "Sumber: SIGABISA Dashboard\n",
        "Dataset: SOVI Indonesia 2017 (SUSENAS BPS)\n\n",
        
        "ISI PAKET DATA:\n",
        "================\n",
        "1. sovi_data_original.csv\n",
        "   - Data asli 511 kabupaten/kota dengan 16 variabel SOVI\n",
        "   - Sumber: SUSENAS BPS 2017\n",
        "   - Status: Unprocessed, siap untuk analisis\n\n",
        
        ifelse(!is.null(values$processed_data),
               paste0(
                 "2. sovi_data_processed.csv\n",
                 "   - Data hasil pengolahan dengan SIGABISA\n",
                 "   - Observasi: ", nrow(values$processed_data), " baris\n",
                 "   - Variabel: ", ncol(values$processed_data), " kolom\n",
                 "   - Status: Processed, siap untuk analisis lanjutan\n\n"
               ), 
               "2. Data processed tidak tersedia\n\n"
        ),
        
        "3. metadata.xlsx\n",
        "   - Dokumentasi lengkap semua variabel\n",
        "   - Definisi, satuan, dan referensi data\n",
        "   - Panduan interpretasi variabel\n\n",
        
        "4. README_Data_Keseluruhan.txt\n",
        "   - File panduan ini\n\n",
        
        "PANDUAN PENGGUNAAN:\n",
        "=====================\n",
        "- Mulai dengan data original untuk eksplorasi awal\n",
        "- Gunakan data processed untuk analisis statistik lanjutan\n",
        "- Baca metadata untuk memahami setiap variabel\n",
        "- Untuk analisis spasial, gunakan file GeoJSON terpisah\n\n",
        
        "VARIABEL UTAMA (16 Indikator SOVI):\n",
        "====================================\n",
        "Demografi: CHILDREN, FEMALE, ELDERLY, FAMILYSIZE\n",
        "Sosial-Ekonomi: POVERTY, ILLITERATE, LOWEDU, NOTRAINING\n",
        "Infrastruktur: NOELECTRIC, NOSEWER, TAPWATER\n",
        "Perumahan: FHEAD, RENTED\n",
        "Bencana: DPRONE\n",
        "Lainnya: GROWTH, POPULATION\n\n",
        
        "TIPS ANALISIS:\n",
        "===============\n",
        "- Variabel dengan nilai tinggi = kerentanan tinggi\n",
        "- Gunakan analisis cluster untuk pengelompokan wilayah\n",
        "- Analisis korelasi untuk memahami hubungan antar variabel\n",
        "- Pertimbangkan faktor geografis dalam interpretasi\n\n",
        
        "KONTAK & REFERENSI:\n",
        "=====================\n",
        "Dashboard: SIGABISA (Shiny Application)\n",
        "Data Source: SUSENAS BPS 2017\n",
        "Coverage: 511 Kabupaten/Kota Indonesia\n",
        "Last Updated: ", format(Sys.time(), "%Y-%m-%d"), "\n\n",
        
        "PENTING:\n",
        "==========\n",
        "- Data ini untuk keperluan penelitian dan kebijakan\n",
        "- Selalu cantumkan sumber: SUSENAS BPS 2017\n",
        "- Interpretasi hasil harus mempertimbangkan konteks lokal\n",
        "- Untuk penggunaan komersial, hubungi BPS\n\n",
        
        "===============================================\n",
        "© 2024 SIGABISA Dashboard | Built with R Shiny\n",
        "==============================================="
      )
      
      writeLines(header_content, header_file)
      files <- c(files, header_file)
      
      # Original CSV file
      csv_file <- file.path(temp_dir, "sovi_data_original.csv")
      write.csv(sovi_data, csv_file, row.names = FALSE)
      files <- c(files, csv_file)
      
      # Processed CSV file (if available)
      if(!is.null(values$processed_data)) {
        processed_csv_file <- file.path(temp_dir, "sovi_data_processed.csv")
        write.csv(values$processed_data, processed_csv_file, row.names = FALSE)
        files <- c(files, processed_csv_file)
      }
      
      # Enhanced metadata file
      metadata_file <- file.path(temp_dir, "metadata.xlsx")
      metadata_df <- data.frame(
        Variabel = c("CHILDREN", "FEMALE", "ELDERLY", "FHEAD", "FAMILYSIZE", 
                     "NOELECTRIC", "LOWEDU", "GROWTH", "POVERTY", "ILLITERATE", 
                     "NOTRAINING", "DPRONE", "RENTED", "NOSEWER", "TAPWATER", "POPULATION"),
        Alias = c("Anak-anak", "Perempuan", "Lansia", "KRT Perempuan", "Ukuran Keluarga",
                  "Tanpa Listrik", "Pendidikan Rendah", "Pertumbuhan", "Kemiskinan", "Buta Huruf",
                  "Tanpa Pelatihan", "Rawan Bencana", "Rumah Sewa", "Tanpa Sanitasi", "Air Bersih", "Populasi"),
        Definisi = c("Persentase penduduk berusia di bawah 15 tahun terhadap total populasi",
                     "Persentase penduduk berjenis kelamin perempuan terhadap total populasi",
                     "Persentase penduduk berusia 65 tahun ke atas terhadap total populasi",
                     "Persentase rumah tangga dengan kepala rumah tangga perempuan",
                     "Rata-rata jumlah anggota keluarga per rumah tangga",
                     "Persentase rumah tangga yang tidak memiliki akses listrik",
                     "Persentase penduduk dengan tingkat pendidikan di bawah SMP",
                     "Tingkat pertumbuhan penduduk tahunan dalam persen",
                     "Persentase penduduk yang hidup di bawah garis kemiskinan",
                     "Persentase penduduk usia 15+ yang tidak dapat membaca dan menulis",
                     "Persentase penduduk yang belum pernah mengikuti pelatihan kerja",
                     "Indeks yang mengukur tingkat kerawanan terhadap bencana alam",
                     "Persentase rumah tangga yang tinggal di rumah sewa/kontrak",
                     "Persentase rumah tangga tanpa akses sanitasi yang layak",
                     "Persentase rumah tangga dengan akses air bersih",
                     "Jumlah total penduduk dalam wilayah tersebut"),
        Satuan = c("Persen (%)", "Persen (%)", "Persen (%)", "Persen (%)", "Orang",
                   "Persen (%)", "Persen (%)", "Persen (%)", "Persen (%)", "Persen (%)",
                   "Persen (%)", "Indeks", "Persen (%)", "Persen (%)", "Persen (%)", "Jiwa"),
        Tipe_Data = rep("Numerik", 16),
        Referensi = rep("SUSENAS BPS 2017", 16),
        Kategori_SOVI = c("Demografi", "Demografi", "Demografi", "Perumahan", "Demografi",
                          "Infrastruktur", "Sosial-Ekonomi", "Demografi", "Sosial-Ekonomi", "Sosial-Ekonomi",
                          "Sosial-Ekonomi", "Bencana", "Perumahan", "Infrastruktur", "Infrastruktur", "Demografi")
      )
      
      wb_meta <- openxlsx::createWorkbook()
      openxlsx::addWorksheet(wb_meta, "Metadata_Variabel")
      openxlsx::addWorksheet(wb_meta, "Info_Dataset")
      
      openxlsx::writeData(wb_meta, "Metadata_Variabel", metadata_df)
      
      # Dataset info
      dataset_info <- data.frame(
        Aspek = c("Nama Dataset", "Sumber Data", "Tahun", "Coverage", "Total Observasi", 
                  "Total Variabel", "Tingkat Administratif", "Proyeksi Koordinat", "Format File"),
        Detail = c("Social Vulnerability Index (SOVI) Indonesia", "SUSENAS BPS", "2017", 
                   "511 Kabupaten/Kota Indonesia", "511", "16 + identifiers", 
                   "Kabupaten/Kota", "WGS84", "CSV, GeoJSON"))
      
      openxlsx::writeData(wb_meta, "Info_Dataset", dataset_info)
      openxlsx::saveWorkbook(wb_meta, metadata_file, overwrite = TRUE)
      files <- c(files, metadata_file)
      
      # Create zip file
      zip(file, files, flags = "-j")
      
      cat("Complete data package created successfully\n")
      cat("Files included:", length(files), "\n")
      cat("Original data:", nrow(sovi_data), "observations\n")
      if(!is.null(values$processed_data)) {
        cat("Processed data:", nrow(values$processed_data), "observations\n")
      }
    }
  )
}

# ===================================================================
# RUN APPLICATION
# ===================================================================

# Run the application
shinyApp(ui = ui, server = server)
library(tidyverse)
library(bayestestR)

generate_panel_data_summary <- function(data, output_file, var_names, dist_var_names) {
  
  # Check if the specified var_names are in the data
  if (!all(var_names %in% names(data))) {
    stop("Some variables in var_names are not present in the data.")
  }
  
  # Open a PDF device to write output to a file
  pdf(file = output_file, width = 12, height = 8)
  
  ### First Plot: Heatmap of Missing Values by Year ###
  cat("Generating heatmap by year...\n")
  title <- textGrob(
    "Heatmap of Missing Values by Year", gp=gpar(fontsize=20, fontface="bold")
  )
  
  # Select only the variables from var_names and year
  data_selected_year <- data %>% select(year, all_of(var_names))
  
  # Calculate the percentage of missing values per year and variable
  missing_data_year <- data_selected_year %>%
    group_by(year) %>%
    summarize(
      across(all_of(var_names), ~mean(is.na(.)) * 100, .names = "missing_{col}")
    )
  
  # Reshape for ggplot using tidyr::pivot_longer
  melted_missing_year <- pivot_longer(
    missing_data_year, 
    cols = -year, 
    names_to = "variable", 
    values_to = "missing_percentage"
  )
  
  # Clean up variable names for the plot
  melted_missing_year$variable <- gsub(
    "missing_", "", melted_missing_year$variable
  )
  
  # Plot missing data heatmap by year
  missing_plot_year <- ggplot(
    melted_missing_year, aes(x = variable, y = as.factor(year))
  ) +
    geom_tile(aes(fill = missing_percentage), color = "white") +
    scale_fill_gradient(
      low = "black", high = "white", na.value = "red", limits = c(0, 100), 
      name = "% Missing"
    ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
          axis.text.y = element_text(size = 8)) +
    labs(title = "Missing Data Heatmap by Year", x = "Variables", y = "Year")
  
  print(missing_plot_year)
  
  ### Second Plot: Heatmap of Missing Values by Group (ISIC4) ###
  cat("Generating heatmap by isic4...\n")
  title <- textGrob(
    "Heatmap of Missing Values by ISIC4", gp=gpar(fontsize=20, fontface="bold")
  )
  
  
  data_selected_isic4 <- data %>% select(isic4, company_name, all_of(var_names))
  
  # Select only the variables from var_names, isic4, and company_name
  missing_data_isic4 <- data_selected_isic4 %>%
    group_by(isic4) %>%
    summarize(
      across(all_of(var_names), ~mean(is.na(.)) * 100, .names = "missing_{col}"),
      group_size = n_distinct(company_name)
    )
  
  
  # Append group size to ISIC4 labels
  missing_data_isic4$isic4 <- paste0(
    missing_data_isic4$isic4, " (n=", missing_data_isic4$group_size, ")"
  )
  
  # Reshape for ggplot using tidyr::pivot_longer
  melted_missing_isic4 <- pivot_longer(missing_data_isic4, 
                                       cols = -c(isic4, group_size), 
                                       names_to = "variable", 
                                       values_to = "missing_percentage")
  
  # Clean up variable names for the plot
  melted_missing_isic4$variable <- gsub(
    "missing_", "", melted_missing_isic4$variable
  )
  
  # Plot missing data heatmap by isic4 with group size
  missing_plot_isic4 <- ggplot(
    melted_missing_isic4, aes(x = variable, y = isic4)
  ) +
    geom_tile(aes(fill = missing_percentage), color = "white") +
    scale_fill_gradient(
      low = "black", high = "white", na.value = "red", limits = c(0, 100), 
      name = "% Missing"
    ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
          axis.text.y = element_text(size = 8)) +
    labs(
      title = paste0(
        "Missing Data Heatmap by ISIC4 (n=", n_distinct(data$company_name), ")"
      ),
      x = "Variables", y = "ISIC4 Group"
    )
  
  print(missing_plot_isic4)

  non_missing_vars <- var_names
  
  numeric_data <- data %>%
    select(all_of(non_missing_vars)) %>%
    select(where(is.numeric))
  
  # Check if there are numeric variables left for correlation matrix
  if (ncol(numeric_data) > 1) {
    cor_matrix <- cor(numeric_data, use = "pairwise.complete.obs")
    
    corrplot(cor_matrix, method = "color", addCoef.col = "black", type = "lower",
             tl.cex = 0.35, number.cex = 0.3, cl.cex = 0.5)
  } else {
    title <- textGrob(
      "Correlation Matrix (No numeric variables)",
      gp=gpar(fontsize=11, fontface="bold")
    )
    grid.arrange(title)
  }
  
  
  dist_data <- data %>%
    select(year, company_name, all_of(dist_var_names))
  
  for (var in dist_var_names) {
    xmin <- dist_data %>%
      pull(var) %>%
      quantile(0.075, na.rm = TRUE)
    xmax <- dist_data %>%
      pull(var) %>%
      quantile(0.925, na.rm = TRUE)
    
    # First plot: Distribution plot for the variable
    dist_plot <- dist_data %>%
      mutate(
        year = as.character(year)
      ) %>%
      select(-company_name) %>%
      ggplot(aes(x = !!sym(var), fill = year)) +
      geom_density(alpha = 0.4, color = NA) +
      scale_fill_viridis_d() +
      xlim(xmin, xmax)
    
    # Print the distribution plot
    print(dist_plot)
    

    # First plot: Distribution plot for the variable
    dist_data_log <- dist_data %>%
      mutate(
        year = as.character(year)
      ) %>%
      group_by(company_name) %>%
      mutate(
        shift = case_when(
          min(!!sym(var), na.rm = TRUE) <= 0 ~ - min(!!sym(var), na.rm = TRUE) + 1,
          TRUE ~ 0
        ),
        !!sym(var) := log(!!sym(var) + shift)
      ) %>%
      ungroup()
    
    dist_plot_log <- dist_data_log %>%
      ggplot(aes(x = !!sym(var), fill = year)) +
      geom_density(alpha = 0.4, color = NA) +
      scale_fill_viridis_d() +
      labs(title = "log(x + s) distribution where s = -min(x) + 1 if min(x) <= 0")
    
    # Print the distribution plot
    print(dist_plot_log)
    
    # Time series line plot with faceting by groups
    panel_dt_temp <- data %>%
      group_by(company_name) %>%
      mutate(mean_value = mean(!!sym(var), na.rm = TRUE)) %>%
      ungroup()
    
    # Filter out companies with NA mean values
    panel_dt_temp <- panel_dt_temp %>%
      filter(!is.na(mean_value))
    
    # Split companies into 4 groups based on the distribution of mean values
    panel_dt_temp <- panel_dt_temp %>%
      mutate(
        group = ntile(mean_value, 4),
        group_label = case_when(
          group == 1 ~ "Q1 (Lowest Quartile)",
          group == 2 ~ "Q2 (Lower-Mid Quartile)",
          group == 3 ~ "Q3 (Upper-Mid Quartile)",
          group == 4 ~ "Q4 (Highest Quartile)"
        )
      )
    
    # Create the time series line plot
    line_plot <- ggplot(
      panel_dt_temp,
      aes(
        x = as.numeric(year), y = !!sym(var),
        group = company_name, color = isic4_dh
      )
    ) +
      geom_line() +
      facet_wrap(~group_label, scales = "free_y") +
      labs(
        title = paste("Trend of", var, "over Time by ISIC4 (Grouped by Mean Value)"),
        x = "Year", y = paste0(var)
      ) +
      theme_minimal() +
      theme(legend.position = "bottom") +
      scale_color_viridis_d() 
    
    # Print the line plot
    print(line_plot)
  }
  
  # Close the PDF device
  dev.off()
  
  cat("PDF report has been saved to:", output_file, "\n")
}


calc_clusters_manual <- function(
    data, method = "kmeans",
    max_clusters = 10, random_seed = 123,
    plot = TRUE, dir = dir2
    ) {
  set.seed(random_seed)
  
  # Detect the number of available cores and create a cluster
  num_cores <- detectCores() - 1
  cl <- makeCluster(num_cores)
  
  clusterEvalQ(cl, {
    library(cluster)
    library(ggplot2)
  })
  
  clusterExport(cl, varlist = c("data", "random_seed", "method", "max_clusters"), envir = environment())
  
  if (method == "kmeans") {
    # K-means: Calculate WSS for each number of clusters
    kmeans_wss <- parSapply(cl, 1:max_clusters, function(k) {
      set.seed(random_seed)
      kmeans_result <- kmeans(data, centers = k, nstart = 10)
      return(kmeans_result$tot.withinss)
    })
    
    wss_diff <- diff(kmeans_wss) / kmeans_wss[-1]
    
    # Only consider WSS differences starting from the third cluster
    if (length(wss_diff) >= 3) {
      wss_diff_start3 <- wss_diff[3:length(wss_diff)]
      optimal_k <- which.min(wss_diff_start3) + 3
    } else {
      optimal_k <- 2  # Fallback in case max_clusters is small
    }
    
    stopCluster(cl)
    
    # Plot WSS vs. number of clusters
    if (plot) {
      wss_plot <- ggplot(
        data.frame(Clusters = 1:max_clusters, WSS = kmeans_wss),
        aes(x = Clusters, y = WSS)
      ) +
        geom_line() + geom_point() +
        geom_vline(xintercept = optimal_k, linetype = "dashed", color = "red") +
        ggtitle(paste("K-means WSS vs Clusters (Optimal:", optimal_k, ")")) +
        xlab("Number of Clusters") + ylab("WSS") +
        theme_minimal(base_size = 12) +
        theme(
          plot.title = element_text(size = 22, face = "bold", hjust = 0.5),
          axis.title = element_text(size = 20),
          axis.text = element_text(size = 18),
          legend.title = element_text(size = 20),
          legend.text = element_text(size = 18)
        )
      
      nclust_vis_name <- paste0(dir, "suggested_clusters_WSS.png")
      ggsave(
        nclust_vis_name, wss_plot, bg = "white", 
        width = 16,  # cm
        height = 10, # cm
        dpi = 300
      )
      
      print(wss_plot)
    }
    
    return(optimal_k)
    
  } else if (method == "hierarchical") {
    # Hierarchical Clustering with Ward's method
    dist_matrix <- dist(data)  # Compute distance matrix
    hc <- hclust(dist_matrix, method = "ward.D2")
    
    # Use silhouette score to determine the optimal number of clusters
    silhouette_scores <- parSapply(cl, 2:max_clusters, function(k) {
      cluster_assignments <- cutree(hc, k)
      silhouette_result <- silhouette(cluster_assignments, dist_matrix)
      return(mean(silhouette_result[, 3]))  # Return average silhouette score
    })
    
    # Find the maximum drop in silhouette scores
    silhouette_diffs <- diff(silhouette_scores)
    max_drop_index <- which.max(-silhouette_diffs)  # Find the index of the maximum drop
    
    # Set the optimal number of clusters as the index before the maximum drop
    optimal_k <- max_drop_index + 1
    
    stopCluster(cl)
    
    # Plot silhouette score vs. number of clusters
    if (plot) {
      silhouette_plot <- ggplot(
        data.frame(Clusters = 2:max_clusters, Silhouette = silhouette_scores),
        aes(x = Clusters, y = Silhouette)
      ) +
        geom_line() + geom_point() +
        geom_vline(xintercept = optimal_k, linetype = "dashed", color = "red") +
        ggtitle(
          paste("Hierarchical Clustering Silhouette Scores (Optimal:", optimal_k, ")")
        ) +
        xlab("Number of Clusters") + ylab("Average Silhouette Score") +
        theme_minimal(base_size = 12) +
        theme(
          plot.title = element_text(size = 22, face = "bold", hjust = 0.5),
          axis.title = element_text(size = 20),
          axis.text = element_text(size = 18),
          legend.title = element_text(size = 20),
          legend.text = element_text(size = 18)
        )
      
      nclust_vis_name <- paste0(dir, "suggested_clusters_HC_Wards.png")
      ggsave(
        nclust_vis_name, silhouette_plot, bg = "white",
        width = 16,  # cm
        height = 10, # cm
        dpi = 300
      )
      print(silhouette_plot)
    }
    
    return(optimal_k)
    
  } else if (method == "complete_linkage") {
    # Complete Linkage Hierarchical Clustering
    data_matrix <- as.matrix(data)
    
    # Step 1: Compute the distance matrix
    dist_matrix <- dist(data_matrix, method = "euclidean")
    
    # Step 2: Perform Hierarchical Clustering with complete linkage
    hc <- hclust(dist_matrix, method = "complete")
    
    # Step 3: Compute average silhouette scores for each number of clusters
    silhouette_scores <- sapply(2:max_clusters, function(k) {
      cluster_assignments <- cutree(hc, k = k)  # Cut tree at k clusters
      silhouette_result <- silhouette(cluster_assignments, dist_matrix)
      mean(silhouette_result[, 3])
    })
    
    # Step 4: Find the maximum drop in silhouette scores
    silhouette_diffs <- diff(silhouette_scores)
    max_drop_index <- which.max(-silhouette_diffs)
    
    # Step 5: Set the optimal number of clusters as the index before the max
    optimal_k <- max_drop_index + 1 
    
    # Step 6: Plot silhouette scores vs. number of clusters
    if (plot) {
      silhouette_plot <- ggplot(
        data.frame(Clusters = 2:max_clusters, Silhouette = silhouette_scores),
        aes(x = Clusters, y = Silhouette)
      ) +
        geom_line() + geom_point() +
        geom_vline(xintercept = optimal_k, linetype = "dashed", color = "red") +
        ggtitle(
          paste(
            "Complete Linkage Clustering Silhouette Scores\nOptimal Number of Clusters:",
            optimal_k
          )
        ) +
        xlab("Number of Clusters") + ylab("Average Silhouette Score") +
        theme_minimal(base_size = 12) +
        theme(
          plot.title = element_text(size = 22, face = "bold", hjust = 0.5),
          axis.title = element_text(size = 20),
          axis.text = element_text(size = 18),
          legend.title = element_text(size = 20),
          legend.text = element_text(size = 18)
        )
      
      nclust_vis_name <- paste0(dir, "suggested_clusters_HC_complete_linkage.png")
      ggsave(
        nclust_vis_name, silhouette_plot, bg = "white",
        width = 16,  # cm
        height = 10, # cm
        dpi = 300
      )
      
      print(silhouette_plot)
    }
    
    return(optimal_k)
    
  } else {
    stopCluster(cl)
    stop("Unsupported method")
  }
}

# additional functions ---------------------------------------------------------

sanitize_param_name <- function(param) {
  # Replace special characters with underscores
  gsub("[^[:alnum:]_]", "_", param)
}


# Function to check diagnostics and flag issues
check_model_diagnostics <- function(model) {
  posterior_samples <- as.matrix(model)  # Extract posterior samples
  
  diagnostics <- monitor(posterior_samples)  # Compute Rhat and Neff/N
  
  rhat_values <- diagnostics[, "Rhat"]
  neff_values <- diagnostics[, "n_eff"]
  
  # Check Rhat status
  rhat_status <- if (all(rhat_values < 1.1)) {
    "Good"
  } else {
    paste(
      "Rhat issue in:",
      paste(names(rhat_values[rhat_values > 1.1]), collapse = ", ")
    )
  }
  
  # Check Neff/N ratio status (Neff/N should ideally be > 0.1)
  total_samples <- nrow(posterior_samples)
  neff_ratio <- neff_values / total_samples
  
  neff_status <- if (all(neff_ratio > 0.1)) {
    "Good"
  } else {
    paste(
      "Low Neff/N ratio in:",
      paste(names(neff_ratio[neff_ratio <= 0.1]), collapse = ", ")
    )
  }
  
  return(list(rhat_status = rhat_status, neff_status = neff_status))
}

# Function to save plot based on ggtitle or plot name
save_plot <- function(plot, plot_name, dir_name) {
  dir_path <- file.path(dir4, dir_name)
  if (!dir.exists(dir_path)) dir.create(dir_path)
  
  # Extract ggtitle if available
  title <- plot$labels$title
  if (!is.null(title)) {
    file_name <- paste0(gsub(" ", "_", gsub("[^a-zA-Z0-9 ]", "_", title)), ".png")
  } else {
    file_name <- paste0(plot_name, ".png")
  }
  
  # Save the plot as PNG
  ggsave(filename = file.path(dir_path, file_name), plot = plot, device = "png",
         width = 16,  # cm
         height = 10, # cm
         dpi = 300)
}
# updated version of overlap info function -------------------------------------

# Helper function: overlap info for a single parameter (unchanged)
get_overlap_info_single_param <- function(df_param, ci = 0.95) {
  
  # 1) Compute each group's HDI
  hdi_df <- df_param %>%
    group_by(control_variable) %>%
    summarize(hdi = list(hdi(value, ci = ci))) %>%
    unnest_wider(hdi)  # => columns: CI_low, CI_high
  
  # 2) Find common overlap across all groups
  overlap_lower <- max(hdi_df$CI_low)
  overlap_upper <- min(hdi_df$CI_high)
  
  # If there's no valid overlap, set bounds to NA
  if (overlap_upper < overlap_lower) {
    overlap_lower <- NA_real_
    overlap_upper <- NA_real_
  }
  
  # 3) Fraction in overlap for each group
  mass_df <- df_param %>%
    group_by(control_variable) %>%
    summarize(
      fraction_in_overlap = mean(
        value >= overlap_lower & value <= overlap_upper,
        na.rm = TRUE
      )
    )
  
  # 4) A single median of overlap region ACROSS all groups
  overlap_values <- df_param$value[
    df_param$value >= overlap_lower & df_param$value <= overlap_upper
  ]
  overlap_median <- median(overlap_values, na.rm = TRUE)
  
  # 5) Return a tidy summary
  tibble(
    parameter         = unique(df_param$parameter),
    overlap_CI_lower  = overlap_lower,
    overlap_CI_upper  = overlap_upper,
    overlap_median    = overlap_median
  ) %>%
    left_join(mass_df, by = character())
}

# Helper: compute overlap info for a single parameter
get_overlap_info_single_param <- function(df_param, ci = 0.95) {
  # df_param has columns: parameter, control_variable, value (all for one parameter)
  
  # 1) Compute each group's HDI
  hdi_df <- df_param %>%
    group_by(control_variable) %>%
    summarize(hdi = list(hdi(value, ci = ci))) %>%
    unnest_wider(hdi)  # => columns: CI_low, CI_high
  
  # 2) Intersect all HDIs => overlap region
  overlap_lower <- max(hdi_df$CI_low)
  overlap_upper <- min(hdi_df$CI_high)
  
  # If there's no valid overlap, set bounds to NA
  if (overlap_upper < overlap_lower) {
    overlap_lower <- NA_real_
    overlap_upper <- NA_real_
  }
  
  # 3) For each group, fraction of samples in [overlap_lower, overlap_upper]
  mass_df <- df_param %>%
    group_by(control_variable) %>%
    summarize(
      fraction_in_overlap = mean(
        value >= overlap_lower & value <= overlap_upper,
        na.rm = TRUE
      )
    )
  
  # 4) A single median across *all* overlap samples (pooled)
  overlap_values <- df_param$value[
    df_param$value >= overlap_lower & df_param$value <= overlap_upper
  ]
  overlap_median <- median(overlap_values, na.rm = TRUE)
  
  # 5) Return overlap info
  tibble(
    parameter         = unique(df_param$parameter),
    overlap_CI_lower  = overlap_lower,
    overlap_CI_upper  = overlap_upper,
    overlap_median    = overlap_median
  ) %>%
    left_join(mass_df, by = character())
}

get_overlap_info <- function(
    df, 
    ci              = 0.95, 
    do_plot         = FALSE, 
    out_dir         = NULL, 
    filename_prefix = "overlap_density_", 
    plot_title      = "Overlap of Posterior Densities for",
    output_type = ".png"
) {
  param_list <- df %>% group_by(parameter) %>% group_split()
  overlap_results <- purrr::map_dfr(
    param_list, get_overlap_info_single_param, ci = ci
  )
  
  if (do_plot) {
    split_data <- split(df, df$parameter)
    
    overlap_data_list <- list()
    plot_list         <- list()
    
    for (param in names(split_data)) {
      
      df_param <- split_data[[param]]
      
      # Grab the overlap interval + median
      param_overlap <- overlap_results %>%
        filter(parameter == param) %>%
        distinct(parameter, .keep_all = TRUE)
      
      ov_low     <- param_overlap$overlap_CI_lower
      ov_high    <- param_overlap$overlap_CI_upper
      ov_median  <- param_overlap$overlap_median
      
      # Subset to overlap region
      overlap_df <- df_param %>%
        filter(value >= ov_low, value <= ov_high)
      
      overlap_data_list[[param]] <- overlap_df
      
      # Plot only the overlap distribution (all CVs combined)
      p <- ggplot(overlap_df, aes(x = value)) +
        geom_density(alpha = 0.4, fill = "blue", color = "blue") +
        theme_bw(base_size = 12) +
        labs(
          # title = paste(plot_title, param),
          title = plot_title,
          x     = "Overlapping Coefficients",
          y     = ""
        ) +
        theme(
          plot.title   = element_text(size = 22, face = "bold", hjust = 0.5),
          axis.title   = element_text(size = 20),
          axis.text    = element_text(size = 18),
          legend.title = element_text(size = 20),
          legend.text  = element_text(size = 18)
        )
      
      # Vertical line at overlap median (if not NA)
      if (!is.na(ov_median)) {
        p <- p + geom_vline(
          xintercept = ov_median, 
          color = "black", 
          linetype = "dotted",
          size = 1.2
        )
      }
      
      # Vertical line at 0
      p <- p + geom_vline(
        xintercept = 0,
        color = "red",
        linetype = "dotted",
        size = 1.2
      )
      
      plot_list[[param]] <- p
    }
    
    if (!is.null(out_dir)) {
      dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
      
      for (param in names(plot_list)) {
        p <- plot_list[[param]]
        
        # Use your sanitize_param_name() for the file name
        file_name <- paste0(filename_prefix, sanitize_param_name(param), output_type)
        file_path <- file.path(out_dir, file_name)
        
        ggsave(
          filename = file_path,
          plot     = p,
          bg       = "white",
          width    = 16,  # cm
          height   = 10,  # cm
          dpi      = 300
        )
      }
    }
    
    return(list(
      overlap_results   = overlap_results,
      plot_list         = plot_list,
      posterior_overlap = overlap_data_list
    ))
  }
  
  return(overlap_results)
}


# Main function, producing ONE plot with boxplots of the overlap across all par
get_overlap_info_box <- function(
    df, 
    ci = 0.95,
    out_dir = NULL, 
    filename_prefix = "overlap_box_", 
    plot_title = "Overlap Box-plot of Posterior Densities for",
    output_type = ".png"
) {
  # 1) Compute the overlap interval for each parameter
  param_list <- df %>%
    group_by(parameter) %>%
    group_split()

  overlap_bounds <- map_dfr(param_list, get_overlap_info_single_param, ci = ci)
  
  # 2) Build a *combined* data frame of only the overlap values for each par
  big_overlap_df <- map_dfr(split(df, df$parameter), function(df_param) {
    # get that parameter's overlap interval
    this_param <- unique(df_param$parameter)
    bounds     <- overlap_bounds %>%
      filter(parameter == this_param)
    
    if (nrow(bounds) == 0) {
      return(NULL)  # no overlap bounds found
    }
    
    ov_low  <- bounds$overlap_CI_lower
    ov_high <- bounds$overlap_CI_upper
    
    df_param %>%
      filter(value >= ov_low, value <= ov_high)
  })
  
  param <- ifelse(
    all(grepl("log_cum_rd", unique(big_overlap_df$parameter))),
    "lag(log_cum_rd)",
    "lag(log_rd)"
  )
  
  param_regex <- ifelse(
    all(grepl("log_cum_rd", unique(big_overlap_df$parameter))),
    "lag\\(log_cum_rd\\) ",
    "lag\\(log_rd\\) "
  )
  
  p <- big_overlap_df %>%
    mutate(parameter = gsub(param_regex, "", parameter)) %>%
    ggplot(aes(x = value, y = parameter)) +
    geom_boxplot(fill = "skyblue", alpha = 0.5, color = "black") +
    theme_bw(base_size = 12) +
    geom_vline(xintercept = 0, color = "red", linetype = "dotted", size = 1.2) +
    labs(
      # title = paste(plot_title, param),
      title = plot_title,
      x     = "Overlapping Coefficients",
      y     = "Parameter"
    ) +
    theme(
      plot.title   = element_text(size = 22, face = "bold", hjust = 0.5),
      axis.title   = element_text(size = 20),
      axis.text    = element_text(size = 18),
      legend.title = element_text(size = 20),
      legend.text  = element_text(size = 18)
    )
  
  if (!is.null(out_dir)) {

    file_name <- paste0(output_type)
    file_path <- paste0(out_dir, file_name)
    
    ggsave(
      filename = file_path,
      plot     = p,
      bg       = "white",
      width    = 16,   # cm
      height   = 10,   # cm
      dpi      = 300
    )
  }
  
  return(p)
}

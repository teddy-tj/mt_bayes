rm(list = ls())

library(rstanarm)
library(ggplot2)
library(viridis)
library(bayesplot)
library(gridExtra)
library(dplyr)
library(tidyr)
library(rstan)
# Additional libraries required for table generation
library(knitr)
library(kableExtra)
library(purrr)

source("functions.R")

# directories
dir1 <- "../Data/intermediate/"
dir2 <- paste0(dir1, "final/")
dir3 <- "../Data/final/"
dir_out <- paste0(dir3, "07_random_slopes_comparison/")

if (!dir.exists(dir_out)) {
  dir.create(dir_out)
}

model_prefix <- "stan_model_"

control_variables <- c(
  "capex_control", "ns_control", "emp_control",
  "ns_and_emp_control", "capex_and_emp_control",
  "ns_and_capex_control", "ns_and_emp_and_capex_control"
)

all_models_list <- lapply(
  control_variables,
  function(control_var) {
  dir4 <- paste0(dir3, control_var, "/")
  mdl_files <- grep("models.Rdata", list.files(dir4), value = TRUE)
  
  for (i in mdl_files) {
    load(paste0(dir4, i))
  }
  gelist <- as.list(environment())
  models_list <- gelist[grep("^stan_model", names(gelist))]
  return(models_list)
}
)

names(all_models_list) <- control_variables


model_sufix <- gsub(model_prefix, "", names(all_models_list[[1]]))

model_types <- unique(gsub("_[[:digit:]]{1}$", "", model_sufix))

model_sufix <- unique(gsub("^[[:digit:]]{1}", "", model_sufix))

for (m_type in model_types) {
  # m_type <- model_types[2]
  for (m_suf in model_sufix) {
    # m_suf <- model_sufix[3]
    
    temp_mdl_list <- lapply(control_variables, function(cv) {
      all_models_list[[cv]][[ paste0(model_prefix, m_type, m_suf) ]]
    })
    names(temp_mdl_list) <- control_variables
    
    rs_posterior_list <- lapply(
      control_variables, function(cv) {
        model_summary <- summary(temp_mdl_list[[cv]])
        posterior_samples <- as.matrix(temp_mdl_list[[cv]])
        all_params <- colnames(posterior_samples)
        ramdom_slope_params <- grep(
          "^b\\[\\lag\\(rd|^b\\[\\lag\\(log_rd|^b\\[\\lag\\(log_cum_rd",
          all_params,
          value = TRUE
        )
        random_slope_subset <- as.data.frame(posterior_samples) %>%
          select(all_of(ramdom_slope_params)) %>%
          mutate(
            control_variable = cv
          )
        return(random_slope_subset)
      }
    )
    # names(rs_posterior_list) <- control_variables
    
    rs_posterior <- bind_rows(rs_posterior_list)
    
    colnames(rs_posterior) <- gsub("^b\\[|\\]", "", colnames(rs_posterior))
    
    rs_posterior_long <- rs_posterior %>%
      pivot_longer(
        cols = where(is.numeric),
        names_to = "parameter",
        values_to = "value"
      )
    
    # combined plots -----------------------------------------------------------
    # Plot the densities, facetted by parameter, and colored by control_variable
    
    param <- ifelse(
      all(grepl("log_cum_rd", unique(rs_posterior_long$parameter))),
      "lag(log_cum_rd)",
      "lag(log_rd)"
    )

    param_regex <- ifelse(
      all(grepl("log_cum_rd", unique(rs_posterior_long$parameter))),
      "lag\\(log_cum_rd\\) ",
      "lag\\(log_rd\\) "
    )
    
    combined_density_comparison <- rs_posterior_long %>%
      mutate(parameter = gsub(param_regex, "", parameter)) %>%
      ggplot(
        aes(x = value, color = control_variable, fill = control_variable)
      ) +
      geom_density(alpha = 0.3) +
      facet_wrap(~ parameter, scales = "free") +
      theme_minimal() +
      labs(
        title = paste0("Posterior Densities for model ", m_type, m_suf),
        x     = "Coefficient",
        y = ""
      ) +
      theme_bw(base_size = 12) +
      theme(
        plot.title = element_text(size = 22, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 18),
        legend.text = element_text(size = 14)
      )
    combined_density_name <- paste0(
      dir_out, "combined_density_comparison_", m_type, m_suf, ".pdf"
    )
    ggsave(combined_density_name, combined_density_comparison, bg = "white",
           width = 16,  # cm
           height = 10, # cm
           dpi = 300)
    # separate plots -----------------------------------------------------------
    
    # Create a named list of data frames, one per "parameter"
    split_data <- rs_posterior_long %>%
      split(.$parameter)
    
    # Create a named list of plots, one plot per parameter
    plot_list <- map(names(split_data), function(param) {
      ggplot(
        split_data[[param]],
        aes(x = value, color = control_variable, fill = control_variable)
      ) +
        geom_density(alpha = 0.3) +
        theme_minimal() +
        labs(
          title = paste0(
            "Posterior Densities for model ", m_type, m_suf
          ),
          x = "Coefficient",
          y = ""
        ) +
        theme_bw(base_size = 12) +
        theme(
          plot.title = element_text(size = 22, face = "bold", hjust = 0.5),
          axis.title = element_text(size = 20),
          axis.text = element_text(size = 18),
          legend.title = element_text(size = 20),
          legend.text = element_text(size = 18)
        )
    })
    
    # Optional: name each plot by its parameter
    names(plot_list) <- names(split_data)
    
    # Print each plot in turn
    for (p in names(plot_list)) {
      dir_out_density <- paste0(dir_out, "density_comparison_",
                                m_type, m_suf, "/")
      
      if (!dir.exists(dir_out_density)) {
        dir.create(dir_out_density)
      }
      density_name <- paste0(
        dir_out_density, "parameter_", sanitize_param_name(p), ".pdf"
      )
      print(density_name)
      ggsave(density_name, plot_list[[p]], bg = "white",
             width = 16,  # cm
             height = 10, # cm
             dpi = 300)
    }
    
    # intersecting interval ----------------------------------------------------
    
    overlap_name <- paste0(
      dir_out, "density_overlap_", m_type, m_suf, "/"
    )
    
    overlap_results <- get_overlap_info(
      df = rs_posterior_long,
      ci = 1,
      do_plot = TRUE,
      out_dir = overlap_name,
      filename_prefix = "parameter_",
      plot_title = paste0(
        "Posterior Distributions Overlap for model ", m_type, m_suf
      ),
      output_type = ".pdf"
    )
    
    overlap_results_agg <- overlap_results$overlap_results %>%
      mutate(Parameter = gsub(param_regex, "", parameter)) %>%
      group_by(Parameter) %>%
      summarise(
        Min = mean(overlap_CI_lower),
        Median = mean(overlap_median),
        Max = mean(overlap_CI_upper),
        `Mean fraction in overlap` = mean(fraction_in_overlap)
      )
    
    # Create LaTeX table for company-specific intercepts
    overlap_latex <- kable(
      overlap_results_agg, "latex", booktabs = TRUE, 
      caption = paste0(
        "Posterior Distributions Overlap Summary for model ", m_type, m_suf
      ), 
      digits = 3
    ) %>%
      kable_styling(latex_options = c("hold_position"))
    
    # Save the LaTeX file
    overlap_file <- paste0(dir_out, "overlap_summary_",  m_type, m_suf, ".txt")
    write(overlap_latex, overlap_file)
    
    overlap_box_name <- paste0(
      dir_out, "density_overlap_boxplot_", m_type, m_suf
    )
    
    get_overlap_info_box(
      df = rs_posterior_long,
      ci = 1,
      out_dir  = overlap_box_name,
      filename_prefix = "parameter_",
      plot_title = paste0(
        "Box-Plot of Posterior Distributions Overlap for model ", m_type, m_suf
      ),
      output_type = ".pdf"
    )
    
    # standard deviation of posterior distribution -----------------------------
    
    mdl_dt <- temp_mdl_list$ns_and_emp_and_capex_control$data
    
    if (
      all(grepl("industry_alternative:", unique(rs_posterior_long$parameter)))
    ) {
      mdl_dt_group_size <- mdl_dt %>%
        group_by(industry_alternative) %>%
        summarise(
          `Group Size` = n_distinct(company_name)
        ) %>%
        ungroup() %>%
        mutate(
          Parameter = paste0("industry_alternative:", industry_alternative)
        ) %>%
        select(-industry_alternative)
    } else if(
      all(grepl("industry:", unique(rs_posterior_long$parameter)))
    ) {
      mdl_dt_group_size <- mdl_dt %>%
        group_by(industry) %>%
        summarise(
          `Group Size` = n_distinct(company_name)
        ) %>%
        ungroup() %>%
        mutate(
          Parameter = paste0("industry:", industry)
        ) %>%
        select(-industry)
    } else {
      mdl_dt_group_size <- mdl_dt %>%
        group_by(clusters) %>%
        summarise(
          `Group Size` = n_distinct(company_name)
        ) %>%
        ungroup() %>%
        mutate(
          Parameter = paste0("clusters:", clusters)
        ) %>%
        select(-clusters)
    }
    coefficient_summary <- rs_posterior_long %>%
      mutate(parameter = gsub(param_regex, "", parameter)) %>%
      group_by(control_variable, parameter) %>%
      summarise(
        Median = median(value),
        Mean = mean(value),
        SD = sd(value)
      ) %>%
      ungroup() %>%
      rename(
        `Control Variable` = control_variable,
        Parameter = parameter
      ) %>%
      left_join(
        mdl_dt_group_size,
        by = "Parameter"
      )
    
    coef_summary_latex <- kable(
      coefficient_summary, "latex", booktabs = TRUE, 
      caption = paste0(
        "Descriptive statistics of posterior distribution for model ",
        m_type, m_suf
      ), 
      digits = 3
    ) %>%
      kable_styling(latex_options = c("hold_position"))
    
    # Save the LaTeX file
    coef_summary_file <- paste0(
      dir_out, "coef_summary_",  m_type, m_suf, ".txt"
    )
    write(coef_summary_latex, coef_summary_file)
    
    coefficient_summary_agg <- coefficient_summary %>%
      group_by(Parameter) %>%
      summarise(
        `Average Median` = mean(Median),
        `Average Mean` = mean(Mean),
        `Average SD` = mean(SD),
        `Group Size` = unique(`Group Size`)
      )
    
    coef_summary_agg_latex <- kable(
      coefficient_summary_agg, "latex", booktabs = TRUE, 
      caption = paste0(
        "Descriptive statistics of aggregated posterior distributions for model ",
        m_type, m_suf
      ), 
      digits = 3
    ) %>%
      kable_styling(latex_options = c("hold_position"))
    
    # Save the LaTeX file
    coef_summary_agg_file <- paste0(
      dir_out, "coef_summary_agg_",  m_type, m_suf, ".txt"
    )
    write(coef_summary_agg_latex, coef_summary_agg_file)
    
    # some probabilities -------------------------------------------------------
    
    # Calculate Probability of Positive Random Effects -------------------------
    
    prob_positive <- rs_posterior_long %>%
      mutate(parameter = gsub(param_regex, "", parameter)) %>%
      group_by(control_variable, parameter) %>%
      summarise(
        P_positive = mean(value >= 0)  # Use plain text here
      ) %>%
      ungroup()
    
    # Aggregate by Parameter (Mean Probability)
    prob_positive_agg <- prob_positive %>%
      group_by(parameter) %>%
      summarise(
        mean_P_positive = mean(P_positive),
        sd_P_positive = sd(P_positive)# Use plain text
      ) %>%
      ungroup()
    
    # Save Non-Aggregated Results as LaTeX Table -------------------------------
    
    prob_positive_latex <- kable(
      prob_positive, "latex", booktabs = TRUE,
      col.names = c("Control Variable", "Parameter", "$P(b_i \\geq 0)$"),
      caption = paste0(
        "Probability of Positive Random Effects for ",
        param, " (", m_type, m_suf, ")"
      ),
      digits = 3
    ) %>%
      kable_styling(latex_options = c("hold_position"))
    
    prob_positive_file <- paste0(
      dir_out, "prob_positive_",  m_type, m_suf, ".txt"
    )
    write(prob_positive_latex, prob_positive_file)
    
    # Save Aggregated Results as LaTeX Table -----------------------------------
    
    prob_positive_agg_latex <- kable(
      prob_positive_agg, "latex", booktabs = TRUE,
      col.names = c("Parameter", "$Mean P(b_i \\geq 0)$", "$SD P(b_i \\geq 0)$"),
      caption = paste0(
        "Aggregated Probability of Positive Random Effects for ",
        param, " (", m_type, m_suf, ")"
      ),
      digits = 3
    ) %>%
      kable_styling(latex_options = c("hold_position"))
    
    
    prob_positive_agg_file <- paste0(
      dir_out, "prob_positive_agg_",  m_type, m_suf, ".txt"
    )
    write(prob_positive_agg_latex, prob_positive_agg_file)
    
  }
}

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

source("functions.R")

# directories
dir1 <- "../Data/intermediate/"
dir2 <- paste0(dir1, "final/")

control_variables <- c(
  "capex_control", "ns_control", "emp_control",
  "ns_and_emp_control", "capex_and_emp_control", "ns_and_capex_control",
  "ns_and_emp_and_capex_control"
)

for (control_var in control_variables) {
  dir3 <- paste0("../Data/final/", control_var, "/")
  dir4 <- paste0(dir3, "07_models_summary/")
  
  if (!dir.exists(dir4)) {
    dir.create(dir4)
  }

  mdl_files <- grep("models.Rdata", list.files(dir3), value = TRUE)
  
  for (i in mdl_files) {
    load(paste0(dir3, i))
  }
  
  
  gelist <- as.list(.GlobalEnv)
  
  
  models_list <- gelist[grep("^stan_model", names(gelist))]
  
  long_post_data_list <- list()
  
  theme_set(theme_bw()) 
  
  # General model output
  for (i in seq_along(models_list)) {
    
    # Extract the current model
    model <- models_list[[i]]
    
    # Get the model name for easy tracking
    model_name <- names(models_list)[i]
    
    # Create a directory specific to the current model
    dir_mdl <- paste0(dir4, "/", model_name, "/")
    if (!dir.exists(dir_mdl)) {
      dir.create(dir_mdl)
    }
    
    # Model formula
    model_formula <- formula(model)
    print(paste("Model Formula:", model_formula))
    
    # Priors used in the model
    model_priors <- prior_summary(model)
    print("Model Priors:")
    print(model_priors)
    
    # new code for additional latex outputs ----------------------------------------
    
    # Posterior Summary for LaTeX tables
    # Extract posterior summary including group-specific estimates
    model_summary <- summary(model)
    
    posterior_samples <- as.matrix(model)
    
    all_params <- colnames(posterior_samples)
    
    company_params <- grep(
      "^b\\[\\(Intercept\\) company_name", all_params, value = TRUE
    )
    
    if (length(company_params) > 0) {
      # Extract posterior samples for company-specific intercepts
      
      company_summary_subset <- model_summary[company_params, , drop = FALSE]
      
      # Function to compute the required summary statistics for each column
      summarize_column <- function(param_samples) {
        return(c(
          Min = min(param_samples),
          `1st Qu.` = quantile(param_samples, 0.25),
          Median = median(param_samples),
          Mean = mean(param_samples),
          `3rd Qu.` = quantile(param_samples, 0.75),
          Max = max(param_samples)
        ))
      }
      
      # Apply this summary function to each column of the company_summary_subset
      company_summary <- apply(company_summary_subset, 2, summarize_column)
      
      # Transpose the result to get a clean output with parameters as rows
      company_summary <- t(company_summary)
      
      
      # Create LaTeX table for company-specific intercepts
      company_latex <- kable(
        company_summary, "latex", booktabs = TRUE, 
        caption = paste(
          "Aggregated distributions of Company-Specific Intercepts -",
          model_name
        ), 
        digits = 3
      ) %>%
        kable_styling(latex_options = c("hold_position"))
      
      # Save the LaTeX file
      company_file <- paste0(
        dir_mdl, "company_intercept_summary_", model_name, ".txt"
      )
      write(company_latex, company_file)
    }
    
    
    # **ISIC4-Specific Slopes**
    ramdom_slope_params <- grep(
      "^b\\[\\lag\\(rd|^b\\[\\lag\\(log_rd|^b\\[\\lag\\(log_cum_rd",
      all_params, value = TRUE
    )
    
    if (length(ramdom_slope_params) > 0) {
      # Extract posterior samples for random slopes
      random_slope_summary_subset <- model_summary[ramdom_slope_params, , drop = FALSE]
      
      # Apply the same summary function to each column of the random slope sub
      random_slope_summary <- apply(
        random_slope_summary_subset, 2, summarize_column
      )
      
      # Transpose the result to get a clean output with parameters as rows
      random_slope_summary <- t(random_slope_summary)
      
      # Create LaTeX table for random slopes
      random_slope_latex <- kable(
        random_slope_summary, "latex", booktabs = TRUE, 
        caption = paste(
          "Aggregated distributions of Random Slopes -", model_name
        ), 
        digits = 3
      ) %>%
        kable_styling(latex_options = c("hold_position"))
      
      # Save the LaTeX file
      random_slope_file <- paste0(dir_mdl, "slope_summary_", model_name, ".txt")
      write(random_slope_latex, random_slope_file)
    }
    
    # **Non-Group-Specific Estimates**
    # These are the remaining parameters
    non_group_params <- setdiff(
      all_params,
      c(
        company_params,
        ramdom_slope_params,
        grep("^b\\[\\(Intercept\\)", all_params, value = TRUE)
      )
    )
    
    if (length(non_group_params) > 0) {
      # Extract posterior samples for non-group-specific parameters
      non_group_summary <- model_summary[non_group_params, , drop = FALSE]
      
      # Create LaTeX table for non-group-specific parameters
      non_group_latex <- kable(
        non_group_summary, "latex", booktabs = TRUE, 
        caption = paste(
          "Aggregated distributions of Fixed Effects -", model_name
        ), 
        digits = 3
      ) %>%
        kable_styling(latex_options = c("hold_position"))
      
      # Save the LaTeX file
      non_group_file <- paste0(
        dir_mdl, "non_group_summary_", model_name, ".txt"
      )
      write(non_group_latex, non_group_file)
    }
    
    # Assuming model_summary has diagnostic stats like Rhat and ESS
    fit_diagnostics <- data.frame(
      model_summary[, c("Rhat", "n_eff", "mcse")]
    )
    
    fit_diagnostics <- apply(fit_diagnostics, 2, summarize_column)
    
    # Transpose the result to get a clean output with parameters as rows
    fit_diagnostics <- t(fit_diagnostics)
    
    # Create LaTeX table for fit diagnostics
    fit_diagnostics_latex <- kable(
      fit_diagnostics, "latex", booktabs = TRUE,
      caption = paste("Fit Diagnostics for ", model_name), 
      digits = 3
    ) %>%
      kable_styling(latex_options = c("hold_position"))
    
    # Save the LaTeX file
    fit_diagnostics_file <- paste0(
      dir_mdl, "fit_diagnostics_", model_name, ".txt"
    )
    write(fit_diagnostics_latex, fit_diagnostics_file)
    
    # end of new code for latex outputs ----------------------------------------
    
    # Validation metric: Rhat plot using built-in rstanarm plot function
    rhat_plot <- plot(model, "rhat") + 
      ggtitle(paste("Rhat Plot for", model_name)) +
      theme_bw(base_size = 12) +
      theme(
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_text(size = 22, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 18),
        legend.title = element_text(size = 20)
      )  # Ensure white background
    
    # Save the Rhat plot
    rhat_plot_name <- paste0(dir_mdl, "rhat_plot_", model_name, ".png")
    ggsave(rhat_plot_name, rhat_plot, bg = "white",
           width = 16, 
           height = 10,
           dpi = 300)
    
    # Validation metric: ESS plot using built-in rstanarm plot function
    ess_plot <- plot(model, "ess") +
      ggtitle(paste("ESS Plot for", model_name)) +
      theme_bw(base_size = 12) +
      theme(axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.title.y = element_blank(),
            plot.title = element_text(size = 22, face = "bold", hjust = 0.5),
            axis.title = element_text(size = 20),
            axis.text = element_text(size = 18),
            legend.title = element_text(size = 20))
    
    # Save the ESS plot
    ess_plot_name <- paste0(dir_mdl, "ess_plot_", model_name, ".png")
    ggsave(ess_plot_name, ess_plot, bg = "white",
           width = 16, 
           height = 10,
           dpi = 300)
    
    # Create posterior distribution plots
    
    post <- as.array(model)
    
    # Get all parameters and exclude the intercepts
    all_params <- dimnames(post)$parameters
    
    params_to_plot <- setdiff(
      all_params, grep("^b\\[\\(Intercept\\)", all_params, value = TRUE)
    )
    
    
    group_slope_pattern <- "^b\\[lag\\("
    
    # Separate group slopes from other parameters
    group_slope_params <- grep(
      group_slope_pattern, params_to_plot, value = TRUE
    )
    other_params <- setdiff(params_to_plot, group_slope_params)
    
    # Create one plot for all group slopes if they exist
    if (length(group_slope_params) > 0) {
      
      # Sanitize group slope parameter names for saving
      sanitized_group_params <- sapply(group_slope_params, sanitize_param_name)
      
      # Trace plot for all group-specific slopes
      group_trace_plot <- mcmc_trace(post, pars = group_slope_params) +
        ggtitle(paste("Trace Plot for Group-Specific Slopes -", model_name)) +
        theme_bw(base_size = 12) +
        theme(
          plot.title = element_text(size = 22, face = "bold", hjust = 0.5),
          axis.title = element_text(size = 20),
          axis.text = element_text(size = 18),
          legend.title = element_text(size = 20),
          legend.text = element_text(size = 18)
        )  # Ensure white background
      
      # Save the group trace plot
      group_trace_plot_name <- paste0(
        dir_mdl, "group_trace_plot_", model_name, ".png"
      )
      ggsave(group_trace_plot_name, group_trace_plot, bg = "white",
             width = 16, 
             height = 10,
             dpi = 300)
      
      # Density plot for all group-specific slopes
      group_density_plot <- mcmc_areas(post, pars = group_slope_params) +
        ggtitle(paste("Density Plot for Group-Specific Slopes -", model_name)) +
        theme_bw(base_size = 12) +
        theme(
          plot.title = element_text(size = 22, face = "bold", hjust = 0.5),
          axis.title = element_text(size = 20),
          axis.text = element_text(size = 18),
          legend.title = element_text(size = 20),
          legend.text = element_text(size = 18)
        )  # Ensure white background
      
      # Save the group density plot
      group_density_plot_name <- paste0(
        dir_mdl, "group_density_plot_", model_name, ".png"
      )
      ggsave(group_density_plot_name, group_density_plot, bg = "white",
             width = 16, 
             height = 10,
             dpi = 300)
      
      # intervals plot for all group-specific slopes
      group_intervals_plot <- plot(model, pars = group_slope_params) +
        geom_vline(xintercept = 0, linetype = "dashed", color = "red", size = 1) +
        ggtitle(paste("Interval Plot for Group-Specific Slopes -", model_name)) +
        theme_bw(base_size = 12) +
        theme(
          plot.title = element_text(size = 22, face = "bold", hjust = 0.5),
          axis.title = element_text(size = 20),
          axis.text = element_text(size = 18),
          legend.title = element_text(size = 20),
          legend.text = element_text(size = 18)
        )  # Ensure white background
      
      # Save the group density plot
      group_intervals_plot_name <- paste0(
        dir_mdl, "group_interval_plot_", model_name, ".png"
      )
      ggsave(group_intervals_plot_name, group_intervals_plot, bg = "white",
             width = 16, 
             height = 10,
             dpi = 300)
    
      # Density plot for all group-specific slopes
      group_acf_plot <- plot(model, "acf", pars = group_slope_params) +
        ggtitle(paste("ACF by chain of Group-Specific Slopes -", model_name)) +
        theme_bw(base_size = 12) +
        theme(
          plot.title = element_text(size = 22, face = "bold", hjust = 0.5),
          axis.title = element_text(size = 20),
          axis.text = element_text(size = 18),
          legend.title = element_text(size = 20),
          legend.text = element_text(size = 18)
        )  # Ensure white background
      
      # Save the group density plot
      group_acf_plot_name <- paste0(
        dir_mdl, "group_slopes_acf_", model_name, ".png"
      )
      ggsave(group_acf_plot_name, group_acf_plot, bg = "white",
             width = 16, 
             height = 10,
             dpi = 300)
      

      }
    
    # Loop through each non-group parameter to create and save separate plots
    for (param in other_params) {
      
      # Sanitize the parameter name for saving
      sanitized_param <- sanitize_param_name(param)
      
      # Trace plot for the current parameter
      trace_plot <- mcmc_trace(post, pars = param) +
        ggtitle(paste("Trace Plot for", param, "-", model_name)) +
        theme_bw(base_size = 12) +
        theme(axis.text.y = element_blank(),
              axis.ticks.y = element_blank(),
              axis.title.y = element_blank(),
              plot.title = element_text(size = 22, face = "bold", hjust = 0.5),
              axis.title = element_text(size = 20),
              axis.text = element_text(size = 18),
              legend.title = element_text(size = 20))  # Ensure white background
      
      # Save trace plot
      trace_plot_name <- paste0(dir_mdl, "trace_plot_", sanitized_param, ".png")
      ggsave(trace_plot_name, trace_plot, bg = "white",
             width = 16, 
             height = 10,
             dpi = 300)
      
      # Density plot for the current parameter
      density_plot <- mcmc_areas(post, pars = param) +
        ggtitle(paste("Density Plot for", param, "-", model_name)) +
        theme_bw(base_size = 12) +
        theme(axis.text.y = element_blank(),
              axis.ticks.y = element_blank(),
              axis.title.y = element_blank(),
              plot.title = element_text(size = 22, face = "bold", hjust = 0.5),
              axis.title = element_text(size = 20),
              axis.text = element_text(size = 18),
              legend.title = element_text(size = 20))
      
      # Save density plot
      density_plot_name <- paste0(
        dir_mdl, "density_plot_", sanitized_param, ".png"
      )
      ggsave(density_plot_name, density_plot, bg = "white",
             width = 16, 
             height = 10,
             dpi = 300)
      
      # Caterpillar plot for company-specific intercepts
      # Extract company-level intercepts from the posterior draws
      intercepts <- grep("^b\\[\\(Intercept\\)", all_params, value = TRUE)
      
      if (length(intercepts) > 0) {
        intercept_sims <- as.data.frame(as.matrix(model)) %>%
          select(all_of(intercepts))
        
        intercept_long <- intercept_sims %>%
          pivot_longer(
            cols = everything(), names_to = "company", values_to = "value"
          )
        intercept_long$company <- gsub(
          "b\\[\\(Intercept\\) company_name:|\\]", "", intercept_long$company
        )
        
        # Compute mean, SD, and credible intervals for each intercept
        intercept_summary <- intercept_long %>%
          group_by(company) %>%
          summarise(mean = mean(value),
                    sd = sd(value),
                    Q2.5 = quantile(value, 0.025),
                    Q50 = quantile(value, 0.50),
                    Q97.5 = quantile(value, 0.975)) %>%
          ungroup()
        
        # Sort by posterior mean to get a ranking for the plot
        intercept_summary <- intercept_summary %>%
          arrange(mean) %>%
          mutate(rank = row_number())
        
        extreme_companies_min <- intercept_summary %>%
          filter(rank == min(rank))  # Min value and 4 highest
        extreme_companies_max <- intercept_summary %>%
          filter(rank %in% tail(rank, 4))
        
        # Create the caterpillar plot
        caterpillar_plot <- ggplot(intercept_summary, aes(x = rank, y = mean)) +
          geom_pointrange(aes(ymin = Q2.5, ymax = Q97.5), 
                          position = position_jitter(width = 0.1)) +
          geom_hline(yintercept = mean(intercept_summary$mean), color = "red") +
          scale_x_continuous("Rank") +
          scale_y_continuous(expression(paste("Varying intercept, ", alpha[j]))) +
          ggtitle(
            paste("Caterpillar Plot for Company-Specific Intercepts -", model_name)
          ) +
          theme_bw(base_size = 12) +
          theme(
            plot.title = element_text(size = 22, face = "bold", hjust = 0.5),
            axis.title = element_text(size = 20),
            axis.text = element_text(size = 18),
            legend.title = element_text(size = 20),
            legend.text = element_text(size = 18)
          ) +  # Ensure white background
          # Add company names to the most extreme values
          geom_label(
            data = extreme_companies_min, aes(label = company), 
            nudge_x = 30, nudge_y = 0, hjust = "left", size = 3, color = "blue"
          )  +
          geom_label(
            data = extreme_companies_max, aes(label = company), 
            nudge_x = -30, nudge_y = 0, hjust = "right", size = 3, color = "blue"
          )
        
        
        # Save the caterpillar plot
        caterpillar_plot_name <- paste0(
          dir_mdl, "intercept_by_company_plot_", model_name, ".png"
        )
        ggsave(caterpillar_plot_name, caterpillar_plot, bg = "white",
               width = 16, 
               height = 10,
               dpi = 300)
      }
      
      # Slope-specific caterpillar plot
      slope_params <- grep(
        "^b\\[\\lag\\(rd|^b\\[\\lag\\(log_rd|^b\\[\\lag\\(log_cum_rd",
        all_params, value = TRUE
      )
      
      if (length(slope_params) > 0) {
        # Extract posterior samples for slope-specific parameters
        slope_sims <- as.data.frame(as.matrix(model)) %>%
          select(all_of(slope_params))
        
        # Reshape to long format for easier manipulation
        slope_long <- slope_sims %>%
          pivot_longer(
            cols = everything(), names_to = "slope_group", values_to = "value"
          )
        
        slope_long$slope_group <- gsub(
          "^b\\[lag\\(rd\\) isic4:|\\]", "", slope_long$slope_group
        )
        
        # Compute mean, SD, and credible intervals for each slope parameter
        slope_summary <- slope_long %>%
          group_by(slope_group) %>%
          summarise(mean = mean(value),
                    sd = sd(value),
                    Q2.5 = quantile(value, 0.025),
                    Q50 = quantile(value, 0.50),
                    Q97.5 = quantile(value, 0.975)) %>%
          ungroup()
        
        # Sort by posterior mean to get a ranking for the plot
        slope_summary <- slope_summary %>%
          arrange(mean) %>%
          mutate(rank = row_number())
        
        # Create the caterpillar plot for slope-specific parameters
        caterpillar_plot_slope <- ggplot(
          slope_summary, aes(x = rank, y = mean, color = slope_group)
        ) +
          geom_pointrange(aes(ymin = Q2.5, ymax = Q97.5), 
                          position = position_jitter(width = 0.1)) +
          geom_hline(yintercept = mean(slope_summary$mean), color = "red") +
          scale_x_continuous("Rank") +
          scale_y_continuous(expression(paste("Varying slope, ", beta[j]))) +
          labs(color = "ISIC4 Code") +
          scale_color_viridis(discrete = TRUE) +
          ggtitle(
            paste("Caterpillar Plot for Group-Specific Slopes (R&D) -", model_name)
          ) +
          theme_bw(base_size = 12) +
          theme(
            plot.title = element_text(size = 22, face = "bold", hjust = 0.5),
            axis.title = element_text(size = 20),
            axis.text = element_text(size = 18),
            legend.title = element_text(size = 20),
            legend.text = element_text(size = 18)
          )
        
        # Save the caterpillar plot for slope-specific parameters
        caterpillar_plot_name_slope <- paste0(
          dir_mdl, "slope_plot_", model_name, ".png"
        )
        ggsave(
          caterpillar_plot_name_slope, caterpillar_plot_slope, bg = "white",
          width = 16, 
          height = 10,
          dpi = 300
        )
      }
    }
    # Print a message to confirm the model has been processed
    print(paste("Processed:", model_name))
  }
  
  cat("\n--- Custom slope plotting start ---\n")
  for (i in seq_along(models_list)) { 
    
    model_name <- names(models_list)[i]
    dir_mdl <- paste0(dir4, model_name, "/")
    
    # custom slope graphs ------------------------------------------------------
    
    post <- as.data.frame(models_list[[i]])
    all_params <- colnames(post)
    
    random_slopes <- grep(
      "^b\\[\\lag\\(rd|^b\\[\\lag\\(log_rd|^b\\[\\lag\\(log_cum_rd",
      all_params, value = TRUE
    )
    fixed_slope <- grep(
      "^lag\\(rd|^lag\\(log_rd|^lag\\(log_cum_rd", all_params, value = TRUE
    )
    
    
    if (length(fixed_slope) == 0) {
      cat("- No fixed slope ")
      posterior_samples <- as.data.frame(post[, random_slopes])
      colnames(posterior_samples) <- random_slopes
    } else if (length(random_slopes) == 0) {
      cat("- No group specific slopes ")
      posterior_samples <- as.data.frame(post[, fixed_slope])
      colnames(posterior_samples) <- fixed_slope
    } else {
      posterior_samples <- as.data.frame(
        post[, random_slopes] + post[, fixed_slope]
      )
      colnames(posterior_samples) <- random_slopes
    }
    
    pdf(paste0(dir_mdl, "custom_slope_graphs.pdf"))
    for (col in colnames(posterior_samples)) {
      # Calculate the 80% and 95% credible intervals
      ci_80 <- quantile(posterior_samples[, col], probs = c(0.1, 0.9))
      ci_95 <- quantile(posterior_samples[, col], probs = c(0.025, 0.975))
      
      # Create a ggplot density plot with shaded credible intervals
      plot <- ggplot(
        data = data.frame(samples = posterior_samples[, col]), aes(x = samples)
      ) +
        geom_density(fill = "skyblue", alpha = 0.6) +  # Density plot with color
        geom_vline(
          aes(xintercept = mean(posterior_samples[, col])),
          color = "blue", linetype = "dashed", size = 1
        ) +
        geom_ribbon(aes(ymin = 0, ymax = ..density.., x = samples), 
                    data = data.frame(samples = posterior_samples[, col]), 
                    stat = "density", 
                    fill = "blue", alpha = 0.2) +  # Shaded 95% interval
        geom_vline(
          aes(xintercept = ci_80[1]),
          linetype = "dotted", color = "red", size = 1
        ) +
        geom_vline(
          aes(xintercept = ci_80[2]),
          linetype = "dotted", color = "red", size = 1
        ) +
        geom_vline(
          aes(xintercept = ci_95[1]),
          linetype = "dotted", color = "darkred", size = 1
        ) +
        geom_vline(
          aes(xintercept = ci_95[2]),
          linetype = "dotted", color = "darkred", size = 1
        ) +
        ggtitle(paste0("Posterior Distribution with Credible Intervals ", col)) +
        theme_minimal(base_size = 12) +
        theme(
          plot.title = element_text(size = 22, face = "bold", hjust = 0.5),
          axis.title = element_text(size = 20),
          axis.text = element_text(size = 18),
          legend.title = element_text(size = 20),
          legend.text = element_text(size = 18)
        )
      print(plot)
    }
    dev.off()
  }
}

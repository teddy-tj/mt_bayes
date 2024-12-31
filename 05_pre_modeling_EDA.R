rm(list = ls())

library(loo)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(car)
library(MASS)
library(ggpubr)
library(grid)
library(performance)
library(stats)
library(moments)
library(kableExtra)
library(ggcorrplot)
library(purrr)
source("functions.R")

# directories

dir1 <- "../Data/intermediate/"
dir2 <- paste0(dir1, "final/")
dir3 <- "../Data/final/"
# options

if (!dir.exists(dir3)) {
  dir.create(dir3)
}

dir4 <- paste0(dir3, "EDA_plots/")
if (!dir.exists(dir4)) {
  dir.create(dir4)
}


load(paste0(dir2, "03_final_data.Rdata"))
load(paste0(dir2, "03_additional_data.Rdata"))
load(paste0(dir2, "04_cluster_results_all.RData"))

# Assuming rnd_data is your dataset
rnd_data <- results_list$rnd_data_clutered

rnd_data <- rnd_data %>%
  rename(
    industry = isic4_dh,
    industry_alternative = isic4_alternative
  )

# Fit an approximate linear model to check for collinearity
vif_model <- lm(op_comp_shifted ~ lag(log_rd) + log_capex, data = rnd_data)

# Calculate VIF values
vif_values <- vif(vif_model)

# Print VIF values to console
print(vif_values)

# Write the VIF values to a text file
capture.output(vif_values, file = paste0(dir4, "vif_output.txt"))


# profitability summary by group -----------------------------------------------

# prof_col <- c('operating_margin', 'profit_per_employee')

summarize_column <- function(column) {
  summary <- c(
    Min = min(column, na.rm = TRUE),
    `1st Qu.` = quantile(column, 0.25, na.rm = TRUE),
    Median = median(column, na.rm = TRUE),
    Mean = mean(column, na.rm = TRUE),
    `3rd Qu.` = quantile(column, 0.75, na.rm = TRUE),
    Max = max(column, na.rm = TRUE),
    SD = sd(column, na.rm = TRUE)
  )
  return(summary)
}

# isic
profitability_by_group <- rnd_data %>%
  mutate(
    Industry = industry,
    operating_margin = op / ns,
    profit_per_employee = op / emp
  ) %>%
  group_by(Industry) %>%
  summarize(
    summary_1 = list(summarize_column(operating_margin)),
    summary_2 = list(summarize_column(profit_per_employee))
  ) 

profitability_by_group_1 <- profitability_by_group %>%
  dplyr::select(-summary_2) %>%
  unnest_wider(summary_1)
profitability_by_group_2 <- profitability_by_group %>%
  dplyr::select(-summary_1) %>%
  unnest_wider(summary_2)

profitability_summary_1_latex <- kable(
  profitability_by_group_1, "latex", booktabs = TRUE, 
  caption = paste("Operating margin summary by industry."), 
  digits = 3
) %>%
  kable_styling(latex_options = c("hold_position"))
profitability_1_file <- paste0(dir4, "profitability_margin_summary_isic.txt")
write(profitability_summary_1_latex, profitability_1_file)

profitability_summary_2_latex <- kable(
  profitability_by_group_2, "latex", booktabs = TRUE, 
  caption = paste("Profitability per employee summary by industry."), 
  digits = 3
) %>%
  kable_styling(latex_options = c("hold_position"))
profitability_2_file <- paste0(
  dir4, "profitability_per_employee_summary_isic.txt"
)
write(profitability_summary_2_latex, profitability_2_file)

# isic alternative
profitability_by_group <- rnd_data %>%
  mutate(
    `Industry Alternative` = industry_alternative,
    operating_margin = op / ns,
    profit_per_employee = op / emp
  ) %>%
  group_by(`Industry Alternative`) %>%
  summarize(
    summary_1 = list(summarize_column(operating_margin)),
    summary_2 = list(summarize_column(profit_per_employee))
  ) 

profitability_by_group_1 <- profitability_by_group %>%
  dplyr::select(-summary_2) %>%
  unnest_wider(summary_1)
profitability_by_group_2 <- profitability_by_group %>%
  dplyr::select(-summary_1) %>%
  unnest_wider(summary_2)

profitability_summary_1_latex <- kable(
  profitability_by_group_1, "latex", booktabs = TRUE, 
  caption = paste("Operating margin summary by industry alternative."), 
  digits = 3
) %>%
  kable_styling(latex_options = c("hold_position"))
profitability_1_file <- paste0(
  dir4, "profitability_margin_summary_isic_alternative.txt"
)
write(profitability_summary_1_latex, profitability_1_file)

profitability_summary_2_latex <- kable(
  profitability_by_group_2, "latex", booktabs = TRUE, 
  caption = paste(
    "Profitability per employee summary by industry alternative."
  ), 
  digits = 3
) %>%
  kable_styling(latex_options = c("hold_position"))
profitability_2_file <- paste0(
  dir4, "profitability_per_employee_summary_isic_alternative.txt"
)
write(profitability_summary_2_latex, profitability_2_file)

# cluster
profitability_by_group <- rnd_data %>%
  mutate(
    Cluster = clusters,
    operating_margin = op / ns,
    profit_per_employee = op / emp
  ) %>%
  group_by(Cluster) %>%
  summarize(
    summary_1 = list(summarize_column(operating_margin)),
    summary_2 = list(summarize_column(profit_per_employee))
  ) 

profitability_by_group_1 <- profitability_by_group %>%
  dplyr::select(-summary_2) %>%
  unnest_wider(summary_1)
profitability_by_group_2 <- profitability_by_group %>%
  dplyr::select(-summary_1) %>%
  unnest_wider(summary_2)

profitability_summary_1_latex <- kable(
  profitability_by_group_1, "latex", booktabs = TRUE, 
  caption = paste("Operating margin summary by cluster."), 
  digits = 3
) %>%
  kable_styling(latex_options = c("hold_position"))
profitability_1_file <- paste0(dir4, "profitability_margin_summary_cluster.txt")
write(profitability_summary_1_latex, profitability_1_file)

profitability_summary_2_latex <- kable(
  profitability_by_group_2, "latex", booktabs = TRUE, 
  caption = paste("Profitability per employee summary by cluster."), 
  digits = 0
) %>%
  kable_styling(latex_options = c("hold_position"))
profitability_2_file <- paste0(
  dir4, "profitability_per_employee_summary_cluster.txt"
)
write(profitability_summary_2_latex, profitability_2_file)

# rd intensity summary by group ------------------------------------------------

# isic
rd_intensity_by_group <- rnd_data %>%
  mutate(Industry = industry) %>%
  group_by(Industry) %>%
  summarize(
    summary = list(summarize_column(rd_intensity))
  ) %>%
  unnest_wider(summary)
rd_summary_1_latex <- kable(
  rd_intensity_by_group, "latex", booktabs = TRUE, 
  caption = paste("R&D intensity summary by industry."), 
  digits = 3
) %>%
  kable_styling(latex_options = c("hold_position"))
rd_summary_1_file <- paste0(dir4, "rd_intensity_summary_isic.txt")
write(rd_summary_1_latex, rd_summary_1_file)

# isic alternative
rd_intensity_by_group <- rnd_data %>%
  mutate(`Industry Alternative` = industry_alternative) %>%
  group_by(`Industry Alternative`) %>%
  summarize(
    summary = list(summarize_column(rd_intensity))
  ) %>%
  unnest_wider(summary)
rd_summary_1_latex <- kable(
  rd_intensity_by_group, "latex", booktabs = TRUE, 
  caption = paste("R&D intensity summary by industry alternative."), 
  digits = 3
) %>%
  kable_styling(latex_options = c("hold_position"))
rd_summary_1_file <- paste0(dir4, "rd_intensity_summary_isic_alternative.txt")
write(rd_summary_1_latex, rd_summary_1_file)

# cluster
rd_intensity_by_group <- rnd_data %>%
  mutate(Cluster = clusters) %>%
  group_by(Cluster) %>%
  summarize(
    summary = list(summarize_column(rd_intensity))
  ) %>%
  unnest_wider(summary)
rd_summary_1_latex <- kable(
  rd_intensity_by_group, "latex", booktabs = TRUE, 
  caption = paste("R&D intensity summary by cluster."), 
  digits = 3
) %>%
  kable_styling(latex_options = c("hold_position"))
rd_summary_1_file <- paste0(dir4, "rd_intensity_summary_clusters.txt")
write(rd_summary_1_latex, rd_summary_1_file)


# summary statistics -----------------------------------------------------------

numeric_columns_clean <- c('rd', 'op', 'ns', 'capex', 'emp')

summaries <- lapply(rnd_data[, numeric_columns_clean], function(col) {
  if (is.numeric(col)) summarize_column(col) else NULL
})

# Combine summaries into a data frame for easier viewing
summaries_df <- bind_rows(summaries, .id = "variable")

summaries_df_scientific <- summaries_df
summaries_df_scientific[] <- lapply(summaries_df, function(x) {
  if (is.numeric(x)) format(x, scientific = TRUE, digits = 3) else x
})

# Create the LaTeX table
summary_latex <- kable(summaries_df_scientific, "latex", booktabs = TRUE, 
                       caption = paste("Descriptive statistics"), 
                       digits = 3) %>%
  kable_styling(latex_options = c("hold_position"))

# Save the LaTeX file
summary_file <- paste0(dir4, "descriptive_summary.txt")
write(summary_latex, summary_file)

# ------------------------------------------------------------------------------

# Function to remove outliers using IQR method
# Remove outliers for rd and op (by manually calculating IQR without function)
rnd_data_filtered <- rnd_data %>%
  mutate(rd_lag = lag(rd)) %>%
  group_by(industry) %>%
  mutate(
    # Remove outliers for 'rd'
    Q1_rd = quantile(rd, 0.25, na.rm = TRUE),
    Q3_rd = quantile(rd, 0.75, na.rm = TRUE),
    IQR_rd = Q3_rd - Q1_rd,
    lower_bound_rd = Q1_rd - 1.5 * IQR_rd,
    upper_bound_rd = Q3_rd + 1.5 * IQR_rd,
    rd_clean = ifelse(rd >= lower_bound_rd & rd <= upper_bound_rd, rd, NA),
    # Remove outliers for 'rd'
    Q1_rd = quantile(rd_lag, 0.25, na.rm = TRUE),
    Q3_rd = quantile(rd_lag, 0.75, na.rm = TRUE),
    IQR_rd = Q3_rd - Q1_rd,
    lower_bound_rd = Q1_rd - 1.5 * IQR_rd,
    upper_bound_rd = Q3_rd + 1.5 * IQR_rd,
    rd_lag_clean = ifelse(rd_lag >= lower_bound_rd & rd_lag <= upper_bound_rd, rd_lag, NA),
    
    # Remove outliers for 'op'
    Q1_op = quantile(op, 0.25, na.rm = TRUE),
    Q3_op = quantile(op, 0.75, na.rm = TRUE),
    IQR_op = Q3_op - Q1_op,
    lower_bound_op = Q1_op - 1.5 * IQR_op,
    upper_bound_op = Q3_op + 1.5 * IQR_op,
    op_clean = ifelse(op >= lower_bound_op & op <= upper_bound_op, op, NA)
  ) %>%
  ungroup() 

# Drop the temporary columns used for outlier calculation
rnd_data_filtered <- rnd_data_filtered %>%
    dplyr::select(
    -c(
      "Q1_rd", "Q3_rd", "IQR_rd", "lower_bound_rd", "upper_bound_rd",
      "Q1_op", "Q3_op", "IQR_op", "lower_bound_op", "upper_bound_op"
    )
  )

# just renaming variables for plotting -----------------------------------------

# NOTE: here log_op == log_shifted_op
rnd_data_filtered <- rnd_data_filtered %>%
  mutate(
    log_rd_lag = log(rd_lag),
    log_rd_lag_clean = log(rd_lag_clean)
  )

# 1. Density plots by group (industry) for rd and op
density_rd_plot <- ggplot(
  rnd_data_filtered,
  aes(x = rd_clean, color = as.factor(industry), fill = as.factor(industry))
) +
  geom_density(alpha = 0.4) +
  ggtitle("Density Plot of rd by industry (no outliers)") +
  theme(legend.title = element_blank()) +
  ylab("") +
  theme(legend.key.size = unit(0.4, "cm"),
        plot.title = element_text(size = 22, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 18),
        legend.text = element_text(size = 18))
save_plot(density_rd_plot, "z", "density_plots")

density_rd_log_plot <- ggplot(
  rnd_data_filtered,
  aes(x = log_rd, color = as.factor(industry), fill = as.factor(industry))
) +
  geom_density(alpha = 0.4) +
  ggtitle("Density Plot of rd by industry") +
  theme(legend.title = element_blank(),
        plot.title = element_text(size = 22, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 18),
        legend.text = element_text(size = 18),
        legend.key.size = unit(0.4, "cm")) +
  ylab("")
save_plot(density_rd_log_plot, "z", "density_plots")


density_op_plot <- ggplot(
  rnd_data_filtered,
  aes(x = op_clean, color = as.factor(industry), fill = as.factor(industry))
) +
  geom_density(alpha = 0.4) +
  ggtitle("Density Plot of op by industry (no outliers)") +
  theme(legend.title = element_blank(),
        plot.title = element_text(size = 22, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 18),
        legend.text = element_text(size = 18),
        legend.key.size = unit(0.4, "cm")) +
  ylab("")
save_plot(density_op_plot, "z", "density_plots")

density_op_log_plot <- ggplot(
  rnd_data_filtered,
  aes(
    x = log_comp_shifted_op,
    color = as.factor(industry),
    fill = as.factor(industry)
  )
) +
  geom_density(alpha = 0.4) +
  ggtitle("Density Plot of op by industry, company level shift") +
  theme(legend.title = element_blank(),
        plot.title = element_text(size = 22, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 18),
        legend.text = element_text(size = 18),
        legend.key.size = unit(0.4, "cm")) +
  ylab("")
save_plot(density_op_log_plot, "z", "density_plots")

density_op_log_plot_2 <- ggplot(
  rnd_data_filtered,
  aes(
    x = log_isic_shifted_op,
    color = as.factor(industry),
    fill = as.factor(industry)
  )
) +
  geom_density(alpha = 0.4) +
  ggtitle("Density Plot of op by industry, isic4 level shift") +
  theme(legend.title = element_blank(),
        plot.title = element_text(size = 22, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 18),
        legend.text = element_text(size = 18),
        legend.key.size = unit(0.4, "cm")) +
  ylab("")
save_plot(density_op_log_plot_2, "z", "density_plots")

density_op_log_plot_3 <- ggplot(
  rnd_data_filtered,
  aes(
    x = log_shifted_op, color = as.factor(industry), fill = as.factor(industry)
  )
) +
  geom_density(alpha = 0.4) +
  ggtitle("Density Plot of op by industry, general shift") +
  theme(legend.title = element_blank(),
        plot.title = element_text(size = 22, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 18),
        legend.text = element_text(size = 18),
        legend.key.size = unit(0.4, "cm")) +
  ylab("")
save_plot(density_op_log_plot_3, "z", "density_plots")


# Create a PDF to save the plots
pdf(paste0(dir4, "distribution_analysis_by_isic4.pdf"), onefile = TRUE)

# Combine density plots
print(density_rd_plot)
print(density_rd_log_plot)
# print(density_rd_log_plot_c)
print(density_op_plot)
print(density_op_log_plot)
print(density_op_log_plot_2)
print(density_op_log_plot_3)
dev.off()

# 1. Density plots by group (industry_alternative) for rd and op
density_rd_plot <- ggplot(
  rnd_data_filtered,
  aes(
    x = rd_clean,
    color = as.factor(industry_alternative),
    fill = as.factor(industry_alternative)
  )
) +
  geom_density(alpha = 0.4) +
  ggtitle("Density Plot of rd by industry_alternative (no outliers)") +
  theme(legend.title = element_blank(),
        plot.title = element_text(size = 22, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 18),
        legend.text = element_text(size = 18),
        legend.key.size = unit(0.4, "cm")) +
  ylab("")
save_plot(density_rd_plot, "z", "density_plots")

density_rd_log_plot <- ggplot(
  rnd_data_filtered,
  aes(
    x = log_rd,
    color = as.factor(industry_alternative),
    fill = as.factor(industry_alternative)
  )
) +
  geom_density(alpha = 0.4) +
  ggtitle("Density Plot of rd by industry_alternative") +
  theme(legend.title = element_blank(),
        plot.title = element_text(size = 22, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 18),
        legend.text = element_text(size = 18),
        legend.key.size = unit(0.4, "cm")) +
  ylab("")
save_plot(density_rd_log_plot, "z", "density_plots")

density_op_plot <- ggplot(
  rnd_data_filtered,
  aes(
    x = op_clean,
    color = as.factor(industry_alternative),
    fill = as.factor(industry_alternative)
  )
) +
  geom_density(alpha = 0.4) +
  ggtitle("Density Plot of op by industry_alternative (no outliers)") +
  theme(legend.title = element_blank(),
        plot.title = element_text(size = 22, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 18),
        legend.text = element_text(size = 18),
        legend.key.size = unit(0.4, "cm")) +
  ylab("")
save_plot(density_op_plot, "z", "density_plots")

density_op_log_plot <- ggplot(
  rnd_data_filtered,
  aes(
    x = log_comp_shifted_op,
    color = as.factor(industry_alternative),
    fill = as.factor(industry_alternative)
  )
) +
  geom_density(alpha = 0.4) +
  ggtitle("Density Plot of op by industry_alternative, company level shift") +
  theme(legend.title = element_blank(),
        plot.title = element_text(size = 22, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 18),
        legend.text = element_text(size = 18),
        legend.key.size = unit(0.4, "cm")) +
  ylab("")
save_plot(density_op_log_plot, "z", "density_plots")

density_op_log_plot_2 <- ggplot(
  rnd_data_filtered,
  aes(
    x = log_isic_shifted_op,
    color = as.factor(industry_alternative),
    fill = as.factor(industry_alternative)
  )
) +
  geom_density(alpha = 0.4) +
  ggtitle("Density Plot of op by industry_alternative, isic4 level shift") +
  theme(legend.title = element_blank(),
        plot.title = element_text(size = 22, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 18),
        legend.text = element_text(size = 18),
        legend.key.size = unit(0.4, "cm")) +
  ylab("")
save_plot(density_op_log_plot_2, "z", "density_plots")

density_op_log_plot_3 <- ggplot(
  rnd_data_filtered,
  aes(
    x = log_shifted_op,
    color = as.factor(industry_alternative),
    fill = as.factor(industry_alternative)
  )
) +
  geom_density(alpha = 0.4) +
  ggtitle("Density Plot of op by industry_alternative, general shift") +
  theme(legend.title = element_blank(),
        plot.title = element_text(size = 22, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 18),
        legend.text = element_text(size = 18),
        legend.key.size = unit(0.4, "cm")) +
  ylab("")
save_plot(density_op_log_plot_3, "z", "density_plots")



# Create a PDF to save the plots
pdf(
  paste0(dir4, "distribution_analysis_by_industry_alternative.pdf"),
  onefile = TRUE
)

# Combine density plots
print(density_rd_plot)
print(density_rd_log_plot)
print(density_op_plot)
print(density_op_log_plot)
print(density_op_log_plot_2)
print(density_op_log_plot_3)
dev.off()



# 1. Density plots by group (clusters) for rd and op
density_rd_plot <- ggplot(
  rnd_data_filtered,
  aes(x = rd_clean, color = as.factor(clusters), fill = as.factor(clusters))
) +
  geom_density(alpha = 0.4) +
  ggtitle("Density Plot of rd by cluster") +
  theme(legend.title = element_blank(),
        plot.title = element_text(size = 22, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 18),
        legend.text = element_text(size = 18),
        legend.key.size = unit(0.4, "cm")) +
  ylab("")
save_plot(density_rd_plot, "z", "density_plots")

density_rd_log_plot <- ggplot(
  rnd_data_filtered,
  aes(x = log_rd, color = as.factor(clusters), fill = as.factor(clusters))
) +
  geom_density(alpha = 0.4) +
  ggtitle("Density Plot of rd by cluster") +
  theme(legend.title = element_blank(),
        plot.title = element_text(size = 22, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 18),
        legend.text = element_text(size = 18),
        legend.key.size = unit(0.4, "cm")) +
  ylab("")
save_plot(density_rd_log_plot, "z", "density_plots")

density_op_plot <- ggplot(
  rnd_data_filtered,
  aes(x = op_clean, color = as.factor(clusters), fill = as.factor(clusters))
) +
  geom_density(alpha = 0.4) +
  ggtitle("Density Plot of op by cluster") +
  theme(legend.title = element_blank(),
        plot.title = element_text(size = 22, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 18),
        legend.text = element_text(size = 18),
        legend.key.size = unit(0.4, "cm")) +
  ylab("")
save_plot(density_op_plot, "z", "density_plots")

density_op_log_plot <- ggplot(
  rnd_data_filtered,
  aes(
    x = log_comp_shifted_op,
    color = as.factor(clusters),
    fill = as.factor(clusters)
  )
) +
  geom_density(alpha = 0.4) +
  ggtitle("Density Plot of op by cluster") +
  theme(legend.title = element_blank(),
        plot.title = element_text(size = 22, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 18),
        legend.text = element_text(size = 18),
        legend.key.size = unit(0.4, "cm")) +
  ylab("")
save_plot(density_op_log_plot, "z", "density_plots")

density_op_log_plot_2 <- ggplot(
  rnd_data_filtered,
  aes(
    x = log_isic_shifted_op,
    color = as.factor(clusters),
    fill = as.factor(clusters)
  )
) +
  geom_density(alpha = 0.4) +
  ggtitle("Density Plot of op by cluster") +
  theme(legend.title = element_blank(),
        plot.title = element_text(size = 22, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 18),
        legend.text = element_text(size = 18),
        legend.key.size = unit(0.4, "cm")) +
  ylab("")
save_plot(density_op_log_plot_2, "z", "density_plots")

density_op_log_plot_3 <- ggplot(
  rnd_data_filtered,
  aes(
    x = log_shifted_op,
    color = as.factor(clusters),
    fill = as.factor(clusters)
  )
) +
  geom_density(alpha = 0.4) +
  ggtitle("Density Plot of op by cluster") +
  theme(legend.title = element_blank(),
        plot.title = element_text(size = 22, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 18),
        legend.text = element_text(size = 18),
        legend.key.size = unit(0.4, "cm")) +
  ylab("")
save_plot(density_op_log_plot_3, "z", "density_plots")

# Create a PDF to save the plots
pdf(paste0(dir4, "distribution_analysis_by_clusters.pdf"), onefile = TRUE)

# Combine density plots
print(density_rd_plot)
print(density_rd_log_plot)
print(density_op_plot)
print(density_op_log_plot)
print(density_op_log_plot_2)
print(density_op_log_plot_3)
# print(density_op_log_plot_cc)
dev.off()

# 2. Skewness and kurtosis for the cleaned data (no outliers)
skewness_values <- sapply(
  rnd_data_filtered %>% dplyr::select(rd, op, ns, capex, emp), 
  function(x) skewness(x, na.rm = TRUE)
)

kurtosis_values <- sapply(
  rnd_data_filtered %>% dplyr::select(rd, op, ns, capex, emp), 
  function(x) kurtosis(x, na.rm = TRUE)
)
skew_kurt_table <- data.frame(
  Variable = names(kurtosis_values),
  Skewness = skewness_values,
  Kurtosis = kurtosis_values
)

# Create the LaTeX table
kurt_latex <- kable(skew_kurt_table, "latex", booktabs = TRUE, 
                       caption = paste("Descriptive statistics"), 
                       digits = 3) %>%
  kable_styling(latex_options = c("hold_position"))

# Save the LaTeX file
kurt_file <- paste0(dir4, "kurt_summary.txt")
write(kurt_latex, kurt_file)

# 2. Skewness and kurtosis for the cleaned data (no outliers)
skewness_values_log <- sapply(
  rnd_data_filtered %>% dplyr::select(starts_with("log_")), 
  function(x) skewness(x, na.rm = TRUE)
)

kurtosis_values_log <- sapply(
  rnd_data_filtered %>% dplyr::select(starts_with("log_")), 
  function(x) kurtosis(x, na.rm = TRUE)
)
skew_kurt_table_log <- data.frame(
  Variable = names(kurtosis_values_log),
  Skewness = skewness_values_log,
  Kurtosis = kurtosis_values_log
)

# Create the LaTeX table
kurt_latex_log <- kable(skew_kurt_table_log, "latex", booktabs = TRUE, 
                    caption = paste("Descriptive statistics"), 
                    digits = 3) %>%
  kable_styling(latex_options = c("hold_position"))

# Save the LaTeX file
kurt_file_log <- paste0(dir4, "kurt_log_summary.txt")
write(kurt_latex_log, kurt_file_log)

# Capture the tables as grobs
grob_table1 <- tableGrob(skew_kurt_table)
grob_table2 <- tableGrob(skew_kurt_table_log)

# Plot Skewness and Kurtosis table
pdf(paste0(dir4, "skew_kurtosis.pdf"))

grid.arrange(
  grob_table1,
  grob_table2,
  nrow = 2  
)

dev.off()

# 3. QQ plots for cleaned rd and op
qq_rd <- ggplot(rnd_data_filtered, aes(sample = rd_clean)) +
  stat_qq() +
  stat_qq_line() +
  ggtitle("QQ Plot for rd (no outliers)") +
  ylab("") +
  xlab("") + theme(
    plot.title = element_text(size = 22, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 18),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 18)
  )
save_plot(qq_rd, "y", "qq_plots")

qq_rd_log <- ggplot(rnd_data_filtered, aes(sample = log_rd)) +
  stat_qq() +
  stat_qq_line() +
  ggtitle("QQ Plot for log(rd)") +
  ylab("") +
  xlab("") + theme(
    plot.title = element_text(size = 22, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 18),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 18)
  )
save_plot(qq_rd_log, "y", "qq_plots")

qq_op <- ggplot(rnd_data_filtered, aes(sample = op_clean)) +
  stat_qq() +
  stat_qq_line() +
  ggtitle("QQ Plot for op (no outliers)") +
  ylab("") +
  xlab("") + theme(
    plot.title = element_text(size = 22, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 18),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 18)
  )
save_plot(qq_op, "y", "qq_plots")

qq_op_log <- ggplot(rnd_data_filtered, aes(sample = log_comp_shifted_op)) +
  stat_qq() +
  stat_qq_line() +
  ggtitle("QQ Plot for log(op + s), company level shift") +
  ylab("") +
  xlab("") + theme(
    plot.title = element_text(size = 22, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 18),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 18)
  )
save_plot(qq_op_log, "y", "qq_plots")

qq_op_log_2 <- ggplot(rnd_data_filtered, aes(sample = log_isic_shifted_op)) +
  stat_qq() +
  stat_qq_line() +
  ggtitle("QQ Plot for log(op + s), isic4 level shift") +
  ylab("") +
  xlab("") + theme(
    plot.title = element_text(size = 22, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 18),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 18)
  )
save_plot(qq_op_log_2, "y", "qq_plots")

qq_op_log_3 <- ggplot(rnd_data_filtered, aes(sample = log_shifted_op)) +
  stat_qq() +
  stat_qq_line() +
  ggtitle("QQ Plot for log(op + s), general shift") +
  ylab("") +
  xlab("") + theme(
    plot.title = element_text(size = 22, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 18),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 18)
  )
save_plot(qq_op_log_3, "y", "qq_plots")


# Combine QQ plots
pdf(paste0(dir4, "qq_plots.pdf"))
print(qq_rd)
print(qq_rd_log)
print(qq_op)
print(qq_op_log)
print(qq_op_log_2)
print(qq_op_log_3)
dev.off()

# 3.2 QQ plots by group
qq_rd <- ggplot(rnd_data_filtered, aes(sample = rd)) +
  stat_qq() +
  stat_qq_line() +
  ggtitle("QQ Plot for rd") + 
  facet_wrap(~ industry, scales = "free") +
  ylab("") +
  xlab("") + theme(
    plot.title = element_text(size = 22, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 18),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 18)
  )
save_plot(qq_rd, "y", "qq_plots")

qq_rd_log <- ggplot(rnd_data_filtered, aes(sample = log_rd)) +
  stat_qq() +
  stat_qq_line() +
  ggtitle("QQ Plot for log(rd)") + 
  facet_wrap(~ industry, scales = "free") +
  ylab("") +
  xlab("") + theme(
    plot.title = element_text(size = 22, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 18),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 18)
  )
save_plot(qq_rd_log, "y", "qq_plots")

qq_op <- ggplot(rnd_data_filtered, aes(sample = op)) +
  stat_qq() +
  stat_qq_line() +
  ggtitle("QQ Plot for op") + 
  facet_wrap(~ industry, scales = "free") +
  ylab("") +
  xlab("") + theme(
    plot.title = element_text(size = 22, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 18),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 18)
  )
save_plot(qq_op, "y", "qq_plots")

qq_op_log <- ggplot(rnd_data_filtered, aes(sample = log_comp_shifted_op)) +
  stat_qq() +
  stat_qq_line() +
  ggtitle("QQ Plot for log(op + s), company level shift") + 
  facet_wrap(~ industry, scales = "free") +
  ylab("") +
  xlab("") + theme(
    plot.title = element_text(size = 22, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 18),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 18)
  )
save_plot(qq_op_log, "y", "qq_plots")

qq_op_log_2 <- ggplot(rnd_data_filtered, aes(sample = log_isic_shifted_op)) +
  stat_qq() +
  stat_qq_line() +
  ggtitle("QQ Plot for log(op + s), isic4 level shift") + 
  facet_wrap(~ industry, scales = "free") +
  ylab("") +
  xlab("") + theme(
    plot.title = element_text(size = 22, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 18),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 18)
  )
save_plot(qq_op_log_2, "y", "qq_plots")

qq_op_log_3 <- ggplot(rnd_data_filtered, aes(sample = log_shifted_op)) +
  stat_qq() +
  stat_qq_line() +
  ggtitle("QQ Plot for log(op + s), general shift") + 
  facet_wrap(~ industry, scales = "free") +
  ylab("") +
  xlab("") + theme(
    plot.title = element_text(size = 22, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 18),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 18)
  )
save_plot(qq_op_log_3, "y", "qq_plots")


# Combine QQ plots
pdf(paste0(dir4, "qq_plots_by_isic4.pdf"))
print(qq_rd)
print(qq_rd_log)
print(qq_op)
print(qq_op_log)
print(qq_op_log_2)
print(qq_op_log_3)
dev.off()

# 3.2 QQ plots by cluster group 
qq_rd_c <- ggplot(rnd_data_filtered, aes(sample = rd)) +
  stat_qq() +
  stat_qq_line() +
  ggtitle("QQ Plot for rd") + 
  facet_wrap(~ clusters, scales = "free") +
  ylab("") +
  xlab("") + theme(
    plot.title = element_text(size = 22, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 18),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 18)
  )
save_plot(qq_rd_c, "y", "qq_plots")

qq_rd_log_c <- ggplot(rnd_data_filtered, aes(sample = log_rd)) +
  stat_qq() +
  stat_qq_line() +
  ggtitle("QQ Plot for log(rd)") + 
  facet_wrap(~ clusters, scales = "free") +
  ylab("") +
  xlab("") + theme(
    plot.title = element_text(size = 22, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 18),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 18)
  )
save_plot(qq_rd_log_c, "y", "qq_plots")

qq_op_c <- ggplot(rnd_data_filtered, aes(sample = op)) +
  stat_qq() +
  stat_qq_line() +
  ggtitle("QQ Plot for op") + 
  facet_wrap(~ clusters, scales = "free") +
  ylab("") +
  xlab("") + theme(
    plot.title = element_text(size = 22, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 18),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 18)
  )
save_plot(qq_op_c, "y", "qq_plots")

qq_op_log_c <- ggplot(rnd_data_filtered, aes(sample = log_comp_shifted_op)) +
  stat_qq() +
  stat_qq_line() +
  ggtitle("QQ Plot for log(op + s), company level shift") + 
  facet_wrap(~ clusters, scales = "free") +
  ylab("") +
  xlab("") + theme(
    plot.title = element_text(size = 22, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 18),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 18)
  )
save_plot(qq_op_log_c, "y", "qq_plots")

qq_op_log_2_c <- ggplot(rnd_data_filtered, aes(sample = log_isic_shifted_op)) +
  stat_qq() +
  stat_qq_line() +
  ggtitle("QQ Plot for log(op + s), isic4 level shift") + 
  facet_wrap(~ clusters, scales = "free") +
  ylab("") +
  xlab("") + theme(
    plot.title = element_text(size = 22, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 18),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 18)
  )
save_plot(qq_op_log_2_c, "y", "qq_plots")

qq_op_log_3_c <- ggplot(rnd_data_filtered, aes(sample = log_shifted_op)) +
  stat_qq() +
  stat_qq_line() +
  ggtitle("QQ Plot for log(op + s), general shift") + 
  facet_wrap(~ clusters, scales = "free") +
  ylab("") +
  xlab("") + theme(
    plot.title = element_text(size = 22, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 18),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 18)
  )
save_plot(qq_op_log_3_c, "y", "qq_plots")

# Combine QQ plots
pdf(paste0(dir4, "qq_plots_by_clusters.pdf"))
print(qq_rd_c)
print(qq_rd_log_c)
print(qq_op_c)
print(qq_op_log_c)
print(qq_op_log_2_c)
print(qq_op_log_3_c)
dev.off()


# 3.2 QQ plots by cluster_country group
qq_rd_c <- ggplot(rnd_data_filtered, aes(sample = rd)) +
  stat_qq() +
  stat_qq_line() +
  ggtitle("QQ Plot for rd") + 
  facet_wrap(~ industry_alternative, scales = "free") +
  theme(axis.text.y = element_blank(),  # Remove y-axis text
        axis.ticks.y = element_blank(),  # Remove y-axis ticks
        axis.title.y = element_blank(),
        plot.title = element_text(size = 22, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 18),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 18)) 
save_plot(qq_rd_c, "y", "qq_plots")

qq_rd_log_c <- ggplot(rnd_data_filtered, aes(sample = log_rd)) +
  stat_qq() +
  stat_qq_line() +
  ggtitle("QQ Plot for log(rd)") + 
  facet_wrap(~ industry_alternative, scales = "free") +
  theme(axis.text.y = element_blank(),  # Remove y-axis text
        axis.ticks.y = element_blank(),  # Remove y-axis ticks
        axis.title.y = element_blank(),
        plot.title = element_text(size = 22, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 18),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 18)) 
save_plot(qq_rd_log_c, "y", "qq_plots")

qq_op_c <- ggplot(rnd_data_filtered, aes(sample = op)) +
  stat_qq() +
  stat_qq_line() +
  ggtitle("QQ Plot for op") + 
  facet_wrap(~ industry_alternative, scales = "free") +
  theme(axis.text.y = element_blank(),  # Remove y-axis text
        axis.ticks.y = element_blank(),  # Remove y-axis ticks
        axis.title.y = element_blank(),
        plot.title = element_text(size = 22, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 18),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 18)) 
save_plot(qq_op_c, "y", "qq_plots")

qq_op_log_c <- ggplot(rnd_data_filtered, aes(sample = log_comp_shifted_op)) +
  stat_qq() +
  stat_qq_line() +
  ggtitle("QQ Plot for log(op + s), company level shift") + 
  facet_wrap(~ industry_alternative, scales = "free") +
  theme(axis.text.y = element_blank(),  # Remove y-axis text
        axis.ticks.y = element_blank(),  # Remove y-axis ticks
        axis.title.y = element_blank(),
        plot.title = element_text(size = 22, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 18),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 18))  
save_plot(qq_op_log_c, "y", "qq_plots")

qq_op_log_2_c <- ggplot(rnd_data_filtered, aes(sample = log_isic_shifted_op)) +
  stat_qq() +
  stat_qq_line() +
  ggtitle("QQ Plot for log(op + s), isic4 level shift") + 
  facet_wrap(~ industry_alternative, scales = "free") +
  theme(axis.text.y = element_blank(),  # Remove y-axis text
        axis.ticks.y = element_blank(),  # Remove y-axis ticks
        axis.title.y = element_blank(),
        plot.title = element_text(size = 22, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 18),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 18))  
save_plot(qq_op_log_2_c, "y", "qq_plots")

qq_op_log_3_c <- ggplot(rnd_data_filtered, aes(sample = log_shifted_op)) +
  stat_qq() +
  stat_qq_line() +
  ggtitle("QQ Plot for log(op + s), general shift") + 
  facet_wrap(~ industry_alternative, scales = "free") +
  theme(axis.text.y = element_blank(),  # Remove y-axis text
        axis.ticks.y = element_blank(),  # Remove y-axis ticks
        axis.title.y = element_blank(),
        plot.title = element_text(size = 22, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 18),
        legend.title = element_text(size = 20),
        legend.text = element_text(size = 18))  
save_plot(qq_op_log_3_c, "y", "qq_plots")

# Combine QQ plots
pdf(paste0(dir4, "qq_plots_by_industry_alternative.pdf"))
print(qq_rd_c)
print(qq_rd_log_c)
print(qq_op_c)
print(qq_op_log_c)
print(qq_op_log_2_c)
print(qq_op_log_3_c)
dev.off()

# 4. Scatter plot with a smooth line (op ~ rd)
scatter_plot <- ggplot(rnd_data_filtered, aes(x = rd_lag_clean, y = op_clean)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  ggtitle("Scatter plot of op ~ rd_lag (no outliers)") +
  ylab("op") +
  xlab("rd") + theme(
    plot.title = element_text(size = 22, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 18),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 18)
  )
save_plot(scatter_plot, "x", "scatter_plots")

scatter_plot_log <- ggplot(
  rnd_data_filtered, aes(x = log_rd_lag, y = log_shifted_op)
) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  ggtitle("Scatter plot of log(op) ~ log(rd_lag), general shift") +
  ylab("op") +
  xlab("rd") + theme(
    plot.title = element_text(size = 22, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 18),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 18)
  )
save_plot(scatter_plot_log, "x", "scatter_plots")

scatter_plot_by_group <- ggplot(
  rnd_data_filtered, aes(x = rd_lag_clean, y = op_clean)
) +
  geom_point(alpha = 0.5) +  # Scatter plot points
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  ggtitle("Scatter plot of op ~ rd by industry (no outliers)") + 
  facet_wrap(~ industry, scales = "free") +
  ylab("op") +
  xlab("rd") + theme(
    plot.title = element_text(size = 22, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 18),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 18)
  )
save_plot(scatter_plot_by_group, "x", "scatter_plots")

scatter_plot_by_group_log <- ggplot(
  rnd_data_filtered, aes(x = log_rd_lag, y = log_comp_shifted_op)
) +
  geom_point(alpha = 0.5) +  # Scatter plot points
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  ggtitle("Scatter plot of log(op + s) ~ log(rd_lag) by industry, company level shift") + 
  facet_wrap(~ industry, scales = "free") +
  ylab("op") +
  xlab("rd") + theme(
    plot.title = element_text(size = 22, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 18),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 18)
  ) 
save_plot(scatter_plot_by_group_log, "x", "scatter_plots")

scatter_plot_by_group_log_2 <- ggplot(
  rnd_data_filtered, aes(x = log_rd_lag, y = log_isic_shifted_op)
) +
  geom_point(alpha = 0.5) +  # Scatter plot points
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  ggtitle("Scatter plot of log(op + s) ~ log(rd_lag) by industry, isic4 level shift") + 
  facet_wrap(~ industry, scales = "free") +
  ylab("op") +
  xlab("rd") + theme(
    plot.title = element_text(size = 22, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 18),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 18)
  )
save_plot(scatter_plot_by_group_log_2, "x", "scatter_plots")

scatter_plot_by_group_log_3 <- ggplot(
  rnd_data_filtered, aes(x = log_rd_lag, y = log_shifted_op)
) +
  geom_point(alpha = 0.5) +  # Scatter plot points
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  ggtitle("Scatter plot of log(op + s) ~ log(rd_lag) by industry, general shift") + 
  facet_wrap(~ industry, scales = "free") +
  ylab("op") +
  xlab("rd") + theme(
    plot.title = element_text(size = 22, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 18),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 18)
  )
save_plot(scatter_plot_by_group_log_3, "x", "scatter_plots")

################################################################################

scatter_plot_2 <- ggplot(rnd_data_filtered, aes(x = rd_lag_clean, y = op_clean)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  ggtitle("Scatter plot of op ~ rd_lag (no outliers)") +
  ylab("op") +
  xlab("rd") + theme(
    plot.title = element_text(size = 22, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 18),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 18)
  )
save_plot(scatter_plot_2, "x", "scatter_plots")

scatter_plot_log_2 <- ggplot(
  rnd_data_filtered, aes(x = log_rd_lag, y = log_shifted_op)
) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  ggtitle("Scatter plot of log(op) ~ log(rd_lag), general shift") +
  ylab("op") +
  xlab("rd") + theme(
    plot.title = element_text(size = 22, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 18),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 18)
  )
save_plot(scatter_plot_log_2, "x", "scatter_plots")

scatter_plot_by_group_2 <- ggplot(
  rnd_data_filtered, aes(x = rd_lag_clean, y = op_clean)
) +
  geom_point(alpha = 0.5) +  # Scatter plot points
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  ggtitle("Scatter plot of op ~ rd by industry_alternative (no outliers)") + 
  facet_wrap(~ industry_alternative, scales = "free") +
  ylab("op") +
  xlab("rd") + theme(
    plot.title = element_text(size = 22, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 18),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 18)
  )
save_plot(scatter_plot_by_group_2, "x", "scatter_plots")

scatter_plot_by_group_log_2 <- ggplot(
  rnd_data_filtered, aes(x = log_rd_lag, y = log_comp_shifted_op)
) +
  geom_point(alpha = 0.5) +  # Scatter plot points
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  ggtitle("Scatter plot of log(op + s) ~ log(rd_lag) by industry_alternative, company level shift") + 
  facet_wrap(~ industry_alternative, scales = "free") +
  ylab("op") +
  xlab("rd") + theme(
    plot.title = element_text(size = 22, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 18),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 18)
  )
save_plot(scatter_plot_by_group_log_2, "x", "scatter_plots")

scatter_plot_by_group_log_2_2 <- ggplot(
  rnd_data_filtered, aes(x = log_rd_lag, y = log_isic_shifted_op)
) +
  geom_point(alpha = 0.5) +  # Scatter plot points
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  ggtitle("Scatter plot of log(op + s) ~ log(rd_lag) by industry_alternative, isic4 level shift") + 
  facet_wrap(~ industry_alternative, scales = "free") +
  ylab("op") +
  xlab("rd") + theme(
    plot.title = element_text(size = 22, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 18),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 18)
  )
save_plot(scatter_plot_by_group_log_2_2, "x", "scatter_plots")

scatter_plot_by_group_log_2_3 <- ggplot(
  rnd_data_filtered, aes(x = log_rd_lag, y = log_shifted_op)
) +
  geom_point(alpha = 0.5) +  # Scatter plot points
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  ggtitle("Scatter plot of log(op + s) ~ log(rd_lag) by industry_alternative, general shift") + 
  facet_wrap(~ industry_alternative, scales = "free") +
  ylab("op") +
  xlab("rd") + theme(
    plot.title = element_text(size = 22, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 18),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 18)
  )
save_plot(scatter_plot_by_group_log_2_3, "x", "scatter_plots")

scatter_plot_by_group_c <- ggplot(
  rnd_data_filtered, aes(x = rd_lag_clean, y = op_clean)
) +
  geom_point(alpha = 0.5) +  # Scatter plot points
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  ggtitle("Scatter plot of op ~ rd by clusters (no outliers)") + 
  facet_wrap(~ clusters, scales = "free") +
  ylab("op") +
  xlab("rd") + theme(
    plot.title = element_text(size = 22, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 18),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 18)
  )
save_plot(scatter_plot_by_group_c, "x", "scatter_plots")

scatter_plot_by_group_log_c <- ggplot(
  rnd_data_filtered, aes(x = log_rd_lag, y = log_comp_shifted_op)
) +
  geom_point(alpha = 0.5) +  # Scatter plot points
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  ggtitle("Scatter plot of log(op + s) ~ log(rd_lag) by clusters, company level shift") + 
  facet_wrap(~ clusters, scales = "free") +
  ylab("op") +
  xlab("rd") + theme(
    plot.title = element_text(size = 22, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 18),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 18)
  )
save_plot(scatter_plot_by_group_log_c, "x", "scatter_plots")

scatter_plot_by_group_log_c_2 <- ggplot(
  rnd_data_filtered, aes(x = log_rd_lag, y = log_isic_shifted_op)
) +
  geom_point(alpha = 0.5) +  # Scatter plot points
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  ggtitle("Scatter plot of log(op + s) ~ log(rd_lag) by clusters, isic4 level shift") + 
  facet_wrap(~ clusters, scales = "free") +
  ylab("op") +
  xlab("rd") + theme(
    plot.title = element_text(size = 22, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 18),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 18)
  )
save_plot(scatter_plot_by_group_log_c_2, "x", "scatter_plots")

scatter_plot_by_group_log_c_3 <- ggplot(
  rnd_data_filtered, aes(x = log_rd_lag, y = log_shifted_op)
) +
  geom_point(alpha = 0.5) +  # Scatter plot points
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  ggtitle("Scatter plot of log(op + s) ~ log(rd_lag) by clusters, general shift") + 
  facet_wrap(~ clusters, scales = "free") +
  ylab("op") +
  xlab("rd") + theme(
    plot.title = element_text(size = 22, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 18),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 18)
  )
save_plot(scatter_plot_by_group_log_c_3, "x", "scatter_plots")

scatter_plot_by_group_log_cc <- ggplot(
  rnd_data_filtered, aes(x = log_rd_lag, y = log_comp_shifted_op)
) +
  geom_point(alpha = 0.5) +  # Scatter plot points
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  ggtitle("Scatter plot of log(op + s) ~ log(rd_lag) by clusters, company level shift") + 
  facet_wrap(~ clusters_ctry_group_agg, scales = "free") +
  theme(
    axis.text.y = element_blank(),  # Remove y-axis text
    axis.ticks.y = element_blank(),  # Remove y-axis ticks
    axis.title.y = element_blank(),
    plot.title = element_text(size = 22, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 18),
    legend.title = element_text(size = 20),
    legend.text = element_text(size = 18)
  ) 
save_plot(scatter_plot_by_group_log_cc, "x", "scatter_plots")


pdf(paste0(dir4, "scatter_plot_by_group.pdf"))
print(scatter_plot)
print(scatter_plot_by_group)
print(scatter_plot_by_group_log)
print(scatter_plot_by_group_log_2)
print(scatter_plot_by_group_log_3)
print(scatter_plot_2)
print(scatter_plot_by_group_2)
print(scatter_plot_by_group_log_2)
print(scatter_plot_by_group_log_2_2)
print(scatter_plot_by_group_log_2_3)
print(scatter_plot_by_group_c)
print(scatter_plot_by_group_log_c)
print(scatter_plot_by_group_log_c_2)
print(scatter_plot_by_group_log_c_3)
print(scatter_plot_by_group_log_cc)
dev.off()

# 5. Correlation matrix for the cleaned numeric variables 

numeric_columns <- c(
  'rd', 'rd_lag', 'cumulative_rd', 'rd_intensity', 'op', 'ns', 'capex', 'emp'
)
numeric_columns_clean <- c('rd_lag_clean', 'op_clean', 'ns', 'capex', 'emp')
numeric_columns_log <- grep(
  "log_",
  grep(
    paste0(numeric_columns, collapse = "|"),
    colnames(rnd_data_filtered),
    value = TRUE
  ),
  value = TRUE
)

# Function to calculate average correlation matrix across companies
average_correlation <- function(data, columns, group_var) {
  # Split by company (or group_var)
  cor_matrices <- data %>%
    group_by(!!sym(group_var)) %>%
    group_map(~ {
      if (nrow(.x) > 1) {  # Only calculate if there are multiple rows
        cor_matrix <- cor(.x[columns], use = "pairwise.complete.obs")
        return(cor_matrix)
      } else {
        return(matrix(NA, nrow = length(columns), ncol = length(columns)))
      }
    }, .keep = TRUE) %>%
    discard(~ all(is.na(.)))  # Remove NA matrices from the list
  
  # Average correlations
  n_vars <- length(columns)
  avg_cor_matrix <- matrix(0, nrow = n_vars, ncol = n_vars)
  valid_counts <- matrix(0, nrow = n_vars, ncol = n_vars)
  
  for (mat in cor_matrices) {
    non_na <- !is.na(mat)
    avg_cor_matrix[non_na] <- avg_cor_matrix[non_na] + mat[non_na]
    valid_counts[non_na] <- valid_counts[non_na] + 1
  }
  
  # Avoid division by zero
  avg_cor_matrix[valid_counts > 0] <- avg_cor_matrix[valid_counts > 0] /
    valid_counts[valid_counts > 0]
  
  colnames(avg_cor_matrix) <- columns
  rownames(avg_cor_matrix) <- columns
  
  return(avg_cor_matrix)
}

# Define the column for company grouping
company_var <- "company_name"

# Calculate the average correlation matrices
cor_matrix_std <- average_correlation(
  rnd_data_filtered, numeric_columns, company_var
)
cor_matrix_clean <- average_correlation(
  rnd_data_filtered, numeric_columns_clean, company_var
)
cor_matrix_log <- average_correlation(
  rnd_data_filtered, numeric_columns_log, company_var
)

# Create correlation matrix plots
corr_plot_std <- ggcorrplot::ggcorrplot(cor_matrix_std, lab = TRUE) + 
  ggtitle("Average Correlation Matrix") + theme(
    plot.title = element_text(size = 22, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 20),                           
    axis.text = element_text(size = 18),                           
    legend.title = element_text(size = 20),                        
    legend.text = element_text(size = 18)                           
  )
save_plot(corr_plot_std, "cor_1", "corr_plots")

corr_plot_clean <- ggcorrplot::ggcorrplot(cor_matrix_clean, lab = TRUE) + 
  ggtitle("Average Correlation Matrix for Cleaned Data") + theme(
    plot.title = element_text(size = 22, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 20),                           
    axis.text = element_text(size = 18),                           
    legend.title = element_text(size = 20),                       
    legend.text = element_text(size = 18)                        
  )
save_plot(corr_plot_clean, "cor_2", "corr_plots")

corr_plot_log <- ggcorrplot::ggcorrplot(cor_matrix_log, lab = TRUE) + 
  ggtitle("Average Correlation Matrix for Transformed Data") + theme(
    plot.title = element_text(size = 22, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 20),                         
    axis.text = element_text(size = 18),                       
    legend.title = element_text(size = 20),                     
    legend.text = element_text(size = 18)                         
  )
save_plot(corr_plot_log, "cor_3", "corr_plots")

# Save all correlation matrix plots in a PDF
pdf(paste0(dir4, "average_correlation_matrix.pdf"))
print(corr_plot_std)
print(corr_plot_clean)
print(corr_plot_log)
dev.off()

cat("All PDFs have been created successfully in the directory:", dir4)

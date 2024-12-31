rm(list = ls())

library("dplyr")
library("tidyr")
library("ggplot2")
library("naniar")
library("corrplot")
library("grid")
library("gridExtra")
library("reshape2")
library("ggpubr")
library("bestNormalize")
library("rlang")
library("stringr")

source("functions.R")

source("00_variables_options.R")

# directories
dir0 <- "../Data/input/"
dir1 <- "../Data/intermediate/"
dir2 <- paste0(dir1, "final/")

# options

add_log_transf <- TRUE
add_comp_lvl_shift <- TRUE
add_isic_lvl_shift <- TRUE

# Define the variables you want to check for missing values
variables_for_na_drop <- c("rd", "ns", "op", "emp")
na_threshold_company <- 66.6
na_threshold_year <- 50

rnd_vars <- c("rd", "ns", "capex", "op", "emp" )

rnd_id_vars <- c(
  "company_name", "ctry_code"
)

# read data

isic_map <- readxl::read_excel(paste0(dir0, "isic4_class.xls")) %>%
  filter(`Annotation 1 - Hierarchical Level` == "Hierarchical level 2") %>%
  select("Code Value", "Code Description") %>%
  rename(
    isic4 = `Code Value`,
    isi4_desc = `Code Description`
  ) %>%
  mutate(
    isic4_dh = gsub("[0-9]{2}$", "", isic4),
    isic4 = gsub("^[A-Z]{1}", "", isic4),
    isic4 = gsub("^0", "", isic4),
  )

isic_map_2 <- readxl::read_excel(paste0(dir0, "isic4_class.xls")) %>%
  filter(`Annotation 1 - Hierarchical Level` == "Hierarchical level 1") %>%
  select("Code Value", "Code Description") %>%
  rename(
    isic4_dh = `Code Value`,
    isi4_desc_dh = `Code Description`
  ) 

isic_map <- left_join(
  isic_map,
  isic_map_2,
  by = "isic4_dh"
)


# comp_data <- read.csv(paste0(dir1, "company_level_variables.csv"))

rnd_data <- read.csv(paste0(dir1, "financial_data_after_adj.csv"))


if (!dir.exists(dir2)) {
  dir.create(dir2)
}

# adding some additional drivers
rnd_data <- rnd_data %>%
  mutate(
    rd_intensity = rd / ns
  )

rnd_vars <- c(rnd_vars, "rd_intensity")

# adding isic info to the rnd data ---------------------------------------------

rnd_data <- rnd_data %>%
  filter(!is.na(isic4) & !(isic4 == ""))

rnd_isic <- rnd_data %>%
  select(
    company_name, ctry_code, isic4
  ) %>%
  mutate(
    isic4_original = isic4,
    isic4_temp = gsub("-", ":", isic4),
    isic4_new = if_else(
      str_detect(isic4, "-"),
      sapply(strsplit(isic4, "-"), function(x) {
        if (length(x) == 2 && !any(is.na(as.numeric(x)))) {
          paste(seq(as.numeric(x[1]), as.numeric(x[2])), collapse = ", ")
        } else {
          NA_character_  # Return NA if the range is invalid
        }
      }),
      isic4  # If no range, leave isic4 as is
    )
  ) %>%
  select(-c(isic4, isic4_temp))

rnd_isic_flat <- rnd_isic %>%
  separate_rows(isic4_new, sep = ", ")

isic_map_temp <- rnd_isic_flat %>%
  left_join(isic_map, by = c("isic4_new" = "isic4")) %>%
  distinct()

isic_dh_check <- isic_map_temp %>%
  group_by(company_name, ctry_code, isic4_original) %>%
  summarise(
    n_isic = n_distinct(isic4_new),
    n_isic_dh = n_distinct(isic4_dh)
  ) %>%
  filter(n_isic_dh > 1)

if (nrow(isic_dh_check) > 1) {
  View(isic_dh_check)
  warning("\n--- Some companies have multiple isic4 codes of higher hierarchy !!! ---\n")
}

isic_map_final <- isic_map_temp %>%
  select(company_name, ctry_code, isic4_original, isic4_dh) %>%
  distinct() %>%
  rename(isic4 = isic4_original)

rnd_data <- left_join(
  rnd_data,
  isic_map_final,
  by = setdiff(colnames(isic_map_final), "isic4_dh")
)

# final dropping of companies based on number of missing values ----------------

if (!is.null(variables_for_na_drop)) {
  rnd_grid <- expand.grid(
    company_name = unique(rnd_data$company_name),
    year = unique(rnd_data$year)
  )
  
  rnd_grid_full <- left_join(
    rnd_grid,
    rnd_data,
    by = c("company_name", "year")
  )
  
  # flagging of really extreme values 
  outlier_flags_temp <- rnd_grid_full %>%
    group_by(company_name) %>%
    arrange(company_name, year) %>%
    mutate(
      # calculate the Z-scores for the specified variables
      across(
        all_of(variables_for_na_drop), 
        ~ (.-mean(., na.rm = TRUE)) / sd(., na.rm = TRUE),
        .names = "z_{col}"
      ),
      
      # calculate the year-over-year growth for the specified variables
      across(
        all_of(variables_for_na_drop),
        ~ (.-lag(.)) / lag(.),
        .names = "growth_{col}"
      ),
      
      # flag as outlier if the Z-score is above the threshold and gr rate
      is_outlier = rowSums(
        across(starts_with("z_"), ~ abs(.) > 3) &
          across(starts_with("growth_"), ~ abs(.) > 10), na.rm = TRUE
      ) > 0,
      what_is_outlier = apply(
        across(starts_with("z_"), ~ abs(.) > 3) &
          across(starts_with("growth_"), ~ abs(.) > 10),
        1,
        function(x) paste(names(which(x)), collapse = ", ")
      ),
      
      # number of non-missing observations
      non_missing_count = sum(!is.na(worldrank))
    )
  
  outlier_flags <- outlier_flags_temp %>%
    summarise(
      outlier_count = sum(is_outlier, na.rm = TRUE),
      what_is_outlier = paste(
        unique(what_is_outlier[what_is_outlier != ""]), collapse = "; "
      ),
      non_missing_count = unique(non_missing_count),
      outlier_share = outlier_count / non_missing_count * 100
    ) %>%
    filter(outlier_count != 0)
  
  outlier_comps <- outlier_flags %>%
    pull(company_name)
  
  if (nrow(outlier_flags) > 10) {
    outlier_flags_temp %>%
      filter(company_name %in% outlier_comps) %>%
      View()
    stop("Check the extreme outliers manually !!!")
  } else {
    outlier_flags_temp %>%
      filter(company_name %in% outlier_comps) %>%
      View()
  }
  
  cat(
    paste0(
      "\n--- ",
      length(unique(outlier_comps)),
      " (out of ", length(unique(rnd_data$company_name)), ")",
      " Companies droped due to extreme observations (outliers) ---\n"
    )
  )
  
  
  na_summary_by_year <- rnd_grid_full %>%
    group_by(year) %>%  # or company_id if you prefer
    summarise(
      na_share_all = mean(
        unlist(across(all_of(variables_for_na_drop), is.na))
      ) * 100
    ) %>%
    ungroup() %>%
    filter(na_share_all > na_threshold_year) %>%
    mutate(year_diff = year - lag(year))
  
  n_period_check <- length(unique(na_summary_by_year$year)) /
    length(unique(rnd_grid_full$year))
  
  if (
    max(na_summary_by_year$year_diff, na.rm = TRUE) > 1 | n_period_check > 0.2
  ) {
    stop("Problem with [na_summary_by_year] variable, check it manually !!!")
  } else {
    periods_to_drop <- unique(na_summary_by_year$year)
  }
  
  # View the summary of missing values by company
  cat(
    paste0(
      "\n--- ",
      paste0(periods_to_drop, collapse = ", "),
      " Periods droped due to high number of missing values ---\n"
    )
  )

  rnd_grid_full_original <- rnd_grid_full
  
  rnd_grid_full <- rnd_grid_full %>%
    filter(!(year %in% periods_to_drop))
  
# Summarise missing data for companies
na_summary <- rnd_grid_full %>%
  group_by(company_name) %>%  # or company_id if you prefer
  summarise(
    na_share_all = mean(
      unlist(across(all_of(variables_for_na_drop), is.na))
    ) * 100
  ) %>%
  ungroup() %>%
  filter(na_share_all > na_threshold_company)

  
  # View the summary of missing values by company
  cat(
    paste0(
      "\n--- ",
      length(unique(na_summary$company_name)),
      " (out of ", length(unique(rnd_data$company_name)), ")",
      " Companies droped due to high number of missing values ---\n"
    )
  )
  
  rnd_data <- rnd_data %>%
    filter(
      !(company_name %in% c(outlier_comps, unique(na_summary$company_name)))
    )
}

# drop companies with negative values for certain variables --------------------

neg_vals_comp <- rnd_data %>%
  filter(ns <= 0 | emp <= 0 | capex <= 0) %>%
  pull(company_name) %>%
  unique()

if (length(neg_vals_comp) != 0) {
  cat(
    paste0(
      "\n--- ",
      length(neg_vals_comp),
      " (out of ", length(unique(rnd_data$company_name)), ")",
      " Companies droped due to high number of missing values ---\n"
    )
  )
  
  rnd_data <- rnd_data %>%
    filter(!(company_name %in% neg_vals_comp))
  
} 

# add cumulative rd variable ---------------------------------------------------

rnd_data <- rnd_data %>%
  arrange(company_name, year) %>%  # Ensure data is ordered by company and year
  group_by(company_name) %>%
  mutate(
    cumulative_rd = cumsum(ifelse(is.na(rd), 0, rd)),
    cumulative_rd = case_when(
      cumulative_rd == 0 ~ NA_real_,
      TRUE ~ cumulative_rd
    )
  ) %>%  # Calculate cumulative sum of rd for each company
  ungroup()

# adding transformations -------------------------------------------------------


if (add_log_transf) {
  min_op <- min(rnd_data$op, na.rm = TRUE)
  rnd_data <- rnd_data %>%
    mutate(
      log_rd = log(rd),
      log_cum_rd = log(cumulative_rd),
      log_ns = log(ns),
      log_capex = log(capex),
      log_emp = log(emp),
      shift = - min_op + 1,
      op_shifted = op + shift,
      log_shifted_op = log(op_shifted)
    )
  if (add_comp_lvl_shift) {
    rnd_data <- rnd_data %>%
      group_by(company_name) %>%
      mutate(
        comp_shift = case_when(
          min(op, na.rm = TRUE) <= 0 ~ - min(op, na.rm = TRUE) + 1,
          TRUE ~ 0
        ),
        op_comp_shifted = op + comp_shift,
        log_comp_shifted_op = log(op_comp_shifted)
      ) %>%
      ungroup()
    
  }
  if (add_isic_lvl_shift) {
    rnd_data <- rnd_data %>%
      group_by(isic4_dh) %>%
      mutate(
        isic_shift = case_when(
          min(op, na.rm = TRUE) <= 0 ~ - min(op, na.rm = TRUE) + 1,
          TRUE ~ 0
        ),
        op_isic_shifted = op + isic_shift,
        log_isic_shifted_op = log(op_isic_shifted)
      ) %>%
      ungroup()
  }
}

rnd_vars <- c(rnd_vars, "cumulative_rd")

generate_panel_data_summary(
  rnd_data,
  output_file = paste0(dir2, "03_panel_summary_data.pdf"),
  var_names = rnd_vars,
  dist_var_names = rnd_vars
)

save(
  rnd_data, isic_map,
  file = paste0(dir2, "03_final_data.Rdata")
)

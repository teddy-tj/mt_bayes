rm(list = ls())

library(OECD)
library(dplyr)
library(purrr)
library(tidyverse)

# directories ------------------------------------------------------------------

dir_dt <- "../Data/input/"
dir_out <- "../Data/intermediate/"

# options ----------------------------------------------------------------------

dt_names <- c("cor&dip_company_financial.txt", "cor&dip_company_list.txt")

dt_names_patent <- c(
  "cor&dip_patent_classes.txt",
  "cor&dip_patent_portfolio.txt",
  "cor&dip_company_list.txt"
)

# drop comps with missing rd or op in the middle of ts
drop_missing <- TRUE 

drop_changed <- TRUE

rnd_vars <- c("rd", "ns", "capex", "op", "emp" )

# data preparation -------------------------------------------------------------

if (!dir.exists(file.path(dir_out))) {
  dir.create(file.path(dir_out))
} else {
  cat("\n--- Intermediate directory already exists ---\n")
}

folders <- list.files(dir_dt, pattern = "-COR&DIP")

all_data_list <- lapply(
  folders, 
  function(folder){
    files <- list.files(paste0(dir_dt, folder), pattern = ".txt")
    temp_dt_folder <- lapply(
      files, 
      function(file){
        temp_file <- read.table(
          file = paste0(dir_dt, folder, "/", file), header = TRUE, sep = "|"
        )
        colnames(temp_file) <- gsub(
          ".3d",
          "3",
          gsub(
            "_stan38",
            "",
            tolower(colnames(temp_file))
          )
        )
        temp_file <- temp_file %>% mutate(
          edition = gsub("\\-.*", "", file)
        )
        temp_file[, "icb3"] <- NULL
        return(temp_file)
      }
    )
    names(temp_dt_folder) <- tolower(
      gsub("TM", "trademark", gsub("^[[:digit:]]{4}-", "", files))
    )
    return(temp_dt_folder)
  }
)
names(all_data_list) <- folders

save(all_data_list, file = "full_data.RData")

cat("\n--- All datasets available:\n")

for (i in folders) {
  cat(paste0("\n", i, ":\n"))
  print(names(all_data_list[[i]]))
}
cat("\n---------------------------\n")


dt_of_interest_list <- lapply(
  names(all_data_list),
  function(name) {
    temp_dt <- purrr::reduce(
      all_data_list[[name]][dt_names],
      dplyr::left_join,
      by = c("company_id", "edition")
    ) %>%
      mutate(
        company_id = paste0(company_id, "-", edition),
        company_name = gsub("[[:punct:]]", "", company_name) %>%
          trimws()  
      )
    return(temp_dt)
  }
)

dt_patents <- lapply(
  names(all_data_list),
  function(name) {
    temp_dt <- 
      left_join(
        all_data_list[[name]]$`cor&dip_company_list.txt`,
        all_data_list[[name]]$`cor&dip_patent_portfolio.txt`,
        by = c("company_id", "edition")
    ) %>%
      mutate(
        company_id = paste0(company_id, "-", edition)
      ) %>%
      mutate(
        year = gsub(
          "-[[:digit:]]{2}-[[:digit:]]{2}$", "", patent_filing_date
        ),
        family_filing_date = gsub(
          "-[[:digit:]]{2}-[[:digit:]]{2}$", "", family_filing_date
        )
      ) %>%
      mutate(across(everything(), as.character))
    return(temp_dt)
  }
)

dt_patents <- bind_rows(dt_patents) %>%
  group_by(company_id, company_name, ctry_code, year, edition) %>%
  summarise(n_patent = n_distinct(patent_publn_nr)) %>%
  ungroup() %>%
  group_by(company_id, company_name, ctry_code) %>%
  arrange(year) %>%
  group_by(company_id, company_name, ctry_code, year) %>%
  arrange(desc(edition)) %>%
  slice(1) %>%
  ungroup()


for (i in 1:length(dt_of_interest_list)) {
  print(colnames(dt_of_interest_list[[i]]))
}

# dropping companies with missing/discontinued observations --------------------

dt_of_interest <- bind_rows(dt_of_interest_list)

if (drop_missing) {
  
  missing_check <- dt_of_interest %>%
    group_by(company_name, year, ctry_code) %>% 
    mutate(
      missing_important = case_when(
        is.na(rd) | is.na(op) ~ TRUE,
        TRUE ~ FALSE
      ),
      missing = all(is.na(c_across(all_of(rnd_vars)))),
    ) %>%
    ungroup() %>%
    group_by(company_name, ctry_code) %>% 
    mutate(
      missing_important_random = case_when(
        missing_important & lag(!missing_important) ~ TRUE,
        TRUE ~ FALSE
      ),
      missing_random = case_when(
        missing & lag(!missing) ~ TRUE,
        TRUE ~ FALSE
      )
    ) %>%
    ungroup()
  
  comps_to_drop <- missing_check %>%
    filter(missing_random|missing_important_random) %>%
    pull(company_name) %>%
    unique()
  
  dt_of_interest <- dt_of_interest %>%
    filter(!(company_name %in% comps_to_drop))
  
  comp_final <- dt_of_interest %>%
    filter(year == max(dt_of_interest$year, na.rm = TRUE)) %>%
    pull(company_name) %>%
    unique()
  
  dt_of_interest <- dt_of_interest %>%
    filter(company_name %in% comp_final)
  
}

dt_of_interest <- dt_of_interest %>%
  group_by(ctry_code, company_name, year) %>%
  arrange(desc(edition)) %>%
  slice(1) %>%
  ungroup()

# additional checks and removal of some companies ------------------------------

# dropping companies with changes in isic4 -------------------------------------


if (drop_changed) {
  chenged_check <- dt_of_interest %>%
    group_by(company_name) %>%
    summarise(
      n_distinct_isic = n_distinct(isic4),
      n_distinct_ctr = n_distinct(ctry_code)
    ) %>%
    filter(n_distinct_isic > 1 | n_distinct_ctr > 1)
  
  comps_to_drop <- chenged_check %>%
    pull(company_name) %>%
    unique()
  
  dt_of_interest <- dt_of_interest %>%
    filter(!(company_name %in% comps_to_drop))
  
}

# detecting data errors and adjusting them ------------------------------------- 

characteristics_dt_1 <- dt_of_interest %>%
  group_by(ctry_code, company_name) %>%
  summarise(
    mean_op = mean(op, na.rm = TRUE),
    median_op = median(op, na.rm = TRUE),
    sd_op = sd(op, na.rm = TRUE)
  ) %>%
  ungroup()

dt_flaged_cases <- dt_of_interest %>%
  left_join(
    characteristics_dt_1,
    by = c("ctry_code", "company_name")
  ) %>%
  group_by(ctry_code, company_name) %>%
  mutate(
    gr_op = op / lag(op),
    flag = case_when(
      (abs(gr_op) > 100000 | abs(gr_op) < 0.00001) ~ TRUE,
      TRUE ~ FALSE
    )
  ) %>%
  filter(flag) %>%
  select(ctry_code, company_name, flag) %>%
  distinct()

# checking edition level info
flaged_check <- left_join(
  dt_flaged_cases,
  dt_of_interest,
  by = c("ctry_code", "company_name")
) %>% group_by(
  ctry_code, company_name, edition
) %>%
  summarise(
    med_op = median(abs(op), na.rm = TRUE)
  ) %>% group_by(
    edition
  ) %>%
  summarise(
    mean_median_op = mean(med_op, na.rm = TRUE)
  )

# 2019 data clearly has wrong units so flagged observations for that
# edition will be devided by 10^6 

dt_of_interest_final <- left_join(
  dt_of_interest,
  dt_flaged_cases,
  by = c("ctry_code", "company_name")
) %>%
  mutate(
    flag = case_when(
      is.na(flag) | flag == FALSE ~ FALSE,
      TRUE ~ TRUE
    ),
    across(
      c(rd, ns, capex, op), 
      ~ case_when(
        flag  & edition == "2019" ~ . / 10^6,
        TRUE ~ .
      )
    )
  ) %>%
  select(-flag)

dt_flaged_cases_2 <- dt_of_interest_final %>%
  group_by(ctry_code, company_name) %>%
  mutate(
    gr_op = op / lag(op),
    flag = case_when(
      (abs(gr_op) > 100000 | abs(gr_op) < 0.00001) ~ TRUE,
      TRUE ~ FALSE
    )
  ) %>%
  filter(flag)

if (nrow(dt_flaged_cases_2) > 0) {
  cat("\n--- High growth combinations exist! Please check [dt_flaged_cases_2] and [dt_of_interest_final] --\n")
  
  View(dt_flaged_cases_2)
  
  View(dt_of_interest_final)
}


# transforming millions -> units

dt_of_interest_final <- dt_of_interest_final %>%
  mutate(
    rd = rd * 1e+06,
    ns = ns * 1e+06,
    capex = capex * 1e+06,
    op = op * 1e+06,
  )

dt_of_interest <- dt_of_interest %>%
  mutate(
    rd = rd * 1e+06,
    ns = ns * 1e+06,
    capex = capex * 1e+06,
    op = op * 1e+06,
  )

cat(
  paste0("\n---  Saving financial data to: [", dir_out, "financial_data.csv]\n")
)

write.csv(
  dt_of_interest,
  paste0(dir_out, "financial_data_before_adj.csv"),
  row.names = FALSE
)

write.csv(
  dt_flaged_cases,
  paste0(dir_out, "financial_data_flaged_cases.csv"),
  row.names = FALSE
)

write.csv(
  dt_of_interest_final,
  paste0(dir_out, "financial_data_after_adj.csv"),
  row.names = FALSE
)

write.csv(
  dt_of_interest_final[, c("company_name", "ctry_code", "year")],
  paste0(dir_out, "companies_id.csv"),
  row.names = FALSE
)

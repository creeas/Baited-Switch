# title: 01_filter_artis

# load packages
library(arrow)
library(data.table)
library(tidyverse)

# load data

# set database folder location
db_folder <- "/Users/gephart/Library/CloudStorage/Dropbox/Research/ARTIS/ARTIS_2.0_FAO_2025_09_09"

db_date <- "2025-09-12"
db_version <- "v2.0"

# load attribute tables
prod <- fread(file.path(db_folder, "attribute_tables", "prod.csv"))
sciname <- fread(file.path(db_folder, "attribute_tables", "sciname_metadata.csv"))
code_max_resolved_taxa <- fread(file.path(db_folder, "attribute_tables", "code_max_resolved_taxa.csv"))

# define shark scinames
sciname_sharks <- sciname %>%
  filter(Superclass == "chondrichthyes")
sciname_sharks <- c(sciname_sharks$sciname, "elasmobranchii")

# Filter prod to sharks
prod_sharks <- prod %>%
  filter(sciname %in% sciname_sharks)

# load consumption data set
# NOTE: do not load consumption for now - only load if needed due to size. 
# consider aggregating consumption above further to reduce size
# # point to the dataset folder
artis_ds <- arrow::open_dataset(file.path(db_folder, "datasets", paste("ARTIS_", db_version, "_consumption_FAO_mid_all_HS_yrs_", db_date, ".parquet", sep = "")))

# Summarize consumption file by source country
artis_ts_result <- artis_ds %>%
  filter(
    end_use == "direct human consumption",
    sciname %in% sciname_sharks,
    (hs_version == "HS96" & year <= 2003) |
      (hs_version == "HS02" & year >= 2004 & year <= 2009) |
      (hs_version == "HS07" & year >= 2010 & year <= 2012) |
      (hs_version == "HS12" & year >= 2013 & year <= 2020) |
      (hs_version == "HS17" & year >= 2021 & year <= 2023)
  )

consumption <- artis_ts_result %>%
  collect()

write.csv(consumption, "data/consumption_v2_0_sharks.csv", row.names = FALSE)
write.csv(sciname, "data/sciname.csv", row.names = FALSE)

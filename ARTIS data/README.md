# ARTIS v2.0 FAO 2025-09-09


This dataset represents the outputs of the ARTIS (Aquatic Resource Trade in Species) model, version 2.0. Model outputs include disaggregated aquatic trade and consumption data, formatted for efficient access using the Parquet file format. 
A filtered dataset containing all shark and ray meat trade flows can be found here: [artis_2.0_sharks](https://www.dropbox.com/scl/fi/3lvqtksotfjj9n5pyv0pw/consumption_v2_0_sharks.csv?rlkey=zwi68qb5voplqh5pun2okh321&st=oa54gne5&dl=1). This file is large but can be read directly into r using the readr package:


```
# Install the 'readr' package if you don't have it (optional, read.csv is base R)
# install.packages("readr")
library(readr)

# The modified direct download link
artis_url <- "https://www.dropbox.com/scl/fi/3lvqtksotfjj9n5pyv0pw/consumption_v2_0_sharks.csv"

# Read the data directly into a data frame
my_data <- read_csv(artis_url) # or use read.csv(artis_url)
```





For details on querying the full artis database see below:





## Authors and Contact for ARTIS 2.0


Jessica Gephart (author, copyright holder, funder)
University of Washington
School of Aquatic Fishery Sciences 
gephart@uw.edu


Althea Marks (maintainer)
University of Washington
School of Aquatic Fishery Sciences 
amarks1@uw.edu


## File Format: Parquet


ARTIS outputs are stored as [Parquet files](https://parquet.apache.org/) — a columnar, compressed format that enables:
- Efficient storage and sharing of large datasets
- On-disk filtering using `arrow::open_dataset()` (predicate pushdown)
- Fast data access without loading the entire file into memory


## Version Details


- **Model version:** ARTIS v2.0
  -`artis-model` Github repository commit where model was run from <https://github.com/Seafood-Globalization-Lab/artis-model/tree/acf4b1d2458c8d519efbfbd770ad6cbcd372cbb1>
  - `artis-hpc` Github repository commit where model was deployed on AWS <https://github.com/Seafood-Globalization-Lab/artis-hpc/tree/881dd7c669f4c9f83e60bcbc457781be5bd47e5e>
- **Production data source:** FAO
- **Disaggregation method:** Midpoint
- **HS versions used:** HS96, HS02, HS07, HS12, HS17
- **Years covered:** 1996–2020
- **Column descriptions:** <https://github.com/Seafood-Globalization-Lab/artis-model/wiki/ARTIS-Database-Tables>
- **Output files:** 


```
ARTIS_v2.0_FAO/
├── datasets/                                 # Main model output (Parquet format)
│   ├── ARTIS_v2.0_trade_FAO_mid_all_HS_yrs_2025-09-12.parquet
│   └── ARTIS_v2.0_consumption_FAO_mid_all_HS_yrs_2025-09-12.parquet
│
├── attribute-tables/                         # Supplementary metadata tables
│   ├── baci.csv                              # Raw trade values by HS code
│   ├── code_max_resolved_taxa.csv            # Enhanced taxonomic resolution
│   ├── isscaap_metadata.csv                  # ISSCAAP group classification
│   ├── prod.csv                              # Standardized production data
│   ├── sciname_metadata.csv                  # Scientific/common name metadata
│   └── sciname_habitat_method_metadata.csv   # Taxa × habitat × method mapping
│
├── 07-post-processing-validation.html        # Rendered QA and summary reports
│
└── README.md                                 # User guide and code examples
```




## Example: Working with ARTIS Parquet Files in R


```r
# Load required packages
library(arrow)
library(dplyr)


# Open the Parquet dataset (does not read it into memory)
artis_ds <- open_dataset("your/dir/path/to/artis_file.parquet")


# Inspect schema and number of rows
glimpse(artis_ds)
names(artis_ds)


# Filter (this does NOT load into memory)
# This filter defines the custom ARTIS time series for each HS version
artis_filtered <- artis_ds %>%
  filter(
    (hs_version == "HS96" & year <= 2003) |
    (hs_version == "HS02" & year >= 2004 & year <= 2009) |
    (hs_version == "HS07" & year >= 2010 & year <= 2012) |
    (hs_version == "HS12" & year >= 2013 & year <= 2020)
  )


# Load into memory with collect()
artis_df <- artis_filtered %>%
  collect()


# Optional: Save filtered output to a new Parquet file
write_parquet(artis_df, "filtered_artis_data.parquet")


# Reload it later like this:
# artis_df <- read_parquet("filtered_artis_data.parquet")
```

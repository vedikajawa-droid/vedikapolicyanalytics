# Load required packages
library(tidyverse)
library(sf)
library(here)
library(lubridate)
library(tidycensus)

# Optional (for cleaner model outputs later)
library(modelsummary)


# PART 1

# Read the GeoJSON file
data_dir <- path.expand("data")
philly_sales <- file.path(data_dir, "opa_properties_public.geojson")


# Inspect the data
glimpse(philly_sales)

# Check CRS
st_crs(philly_sales)


# Convert sale_date from character to Date format
# This allows us to filter by year correctly
philly_sales <- philly_sales %>%
  mutate(
    sale_date = as.Date(sale_date)
  )

# Check summary to confirm conversion worked
summary(philly_sales$sale_date)


# Filter dataset to include only properties whose most recent sale
# occurred in 2023 or 2024.
# Also remove missing or zero sale prices.
philly_sales_2324 <- philly_sales %>%
  filter(
    year(sale_date) %in% c(2023, 2024),
    !is.na(sale_price),
    sale_price > 0
  )

# Check how many observations remain
nrow(philly_sales)        # Original dataset size
nrow(philly_sales_2324)   # After filtering


# Examine building_code_description to identify residential categories
philly_sales_2324 %>%
  st_drop_geometry() %>%   # Remove spatial geometry
  count(building_code_description, sort = TRUE) %>%
  head(20)


# Filter to residential property types
# Keep rows where building_code_description contains:
# ROW, RES CONDO, SEMI/DET, S/D, APT, Remove vacnt land and mixed use

philly_sales_res <- philly_sales_2324 %>%
  filter(
    str_detect(building_code_description, 
               "ROW|RES CONDO|SEMI/DET|S/D|APT")
  ) %>%
  # Remove vacant land explicitly
  filter(!str_detect(building_code_description, "VACANT"))

# Check resulting count
nrow(philly_sales_res)


# Examine key structural variables for unrealistic values
summary(philly_sales_res$sale_price)
summary(philly_sales_res$total_livable_area)
summary(philly_sales_res$year_built)
summary(philly_sales_res$number_of_bedrooms)
summary(philly_sales_res$number_of_bathrooms)


# Remove unrealistic structural observations
philly_sales_res <- philly_sales_res %>%
  filter(
    total_livable_area > 200,        # Remove tiny or data errors
    total_livable_area < 20000,      # Remove extreme outliers
    sale_price > 10000,              # Remove nominal transfers
    sale_price < 10000000,           # Remove extreme luxury outliers
    year_built > 1800,               # Remove impossible early years
    year_built <= 2024               # Remove future build errors
  )

nrow(philly_sales_res)


# Check missing values in key modeling variables
colSums(is.na(philly_sales_res[, c(
  "sale_price",
  "total_livable_area",
  "number_of_bedrooms",
  "number_of_bathrooms",
  "year_built"
)]))


philly_sales_res <- philly_sales_res %>%
  drop_na(
    sale_price,
    total_livable_area,
    year_built
  )


# Create property age variable
class(philly_sales_res$year_built)
summary(philly_sales_res$year_built)

# Convert year_built to numeric
philly_sales_res <- philly_sales_res %>%
  mutate(
    year_built = as.numeric(year_built)
  )

philly_sales_res <- philly_sales_res %>%
  mutate(
    age = 2024 - year_built
  )



st_crs(philly_sales_res)

# Transform to NAD83 / Pennsylvania South (feet)
philly_sales_res <- philly_sales_res %>%
  st_transform(2272)

# Confirm CRS changed
st_crs(philly_sales_res)









# PART 2

# Pull 2023 ACS tract-level data for Philadelphia County
philly_tracts <- get_acs(
  geography = "tract",
  variables = c(
    median_income = "B19013_001",
    poverty = "B17001_002",
    total_pop = "B01003_001",
    bachelors = "B15003_022"
  ),
  state = "PA",
  county = "Philadelphia",
  geometry = TRUE,
  year = 2023
)

# Inspect structure
glimpse(philly_tracts)


# Transform tracts to same projected CRS as sales data
philly_tracts <- philly_tracts %>%
  st_transform(2272)

# Confirm CRS
st_crs(philly_tracts)


# Reshape ACS data to wide format
philly_tracts_wide <- philly_tracts %>%
  select(GEOID, variable, estimate, geometry) %>%
  pivot_wider(
    names_from = variable,
    values_from = estimate
  )

# Inspect result
glimpse(philly_tracts_wide)


# Create poverty rate and education share
philly_tracts_wide <- philly_tracts_wide %>%
  mutate(
    poverty_rate = poverty / total_pop,
    bachelors_share = bachelors / total_pop
  )


# Spatially join each sale to its census tract
philly_sales_census <- philly_sales_res %>%
  st_join(philly_tracts_wide, join = st_intersects)


# Check missing joins
sum(is.na(philly_sales_census$median_income))

# Remove observations with missing census variables
philly_sales_census <- philly_sales_census %>%
  drop_na(median_income)



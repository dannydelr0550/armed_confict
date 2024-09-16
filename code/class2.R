library(here)
here()
matmort <- read.csv(here('original', 'maternalmortality.csv'), header = TRUE)

View(matmort)

matmort_filt <- dplyr::select(matmort, c(Country.Name, X2000:X2019))

View(matmort_filt)
library(dplyr)
matmort_filt_long <- matmort_filt %>%
  tidyr::pivot_longer(
    cols = dplyr::starts_with("X"),  # Select columns starting with 'X'
    names_to = "Year",        # New column name for year
    names_prefix = "X",       # Remove prefix 'X' from column names
    values_to = "MatMor"      # New column name for values
  ) %>%
  mutate(Year = as.numeric(Year))  # Convert 'Year' to numeric


disaster <- read.csv(here('original', 'disaster.csv'), header = TRUE)


View(disaster)

disaster_filtered <- filter(disaster, Year %in% 2000:2019, Disaster.Type %in% c('Earthquake', 'Drought'))
disaster_filtered_select <- dplyr::select(disaster_filtered, c('Year', 'ISO', 'Disaster.Type')) %>%
  mutate(drought = if_else(Disaster.Type == 'Drought', 1, 0)) %>%
  mutate(earthquake = if_else(Disaster.Type == 'Earthquake', 1, 0))

disaster_filtered_select <- disaster_filtered_select %>% group_by(Year, ISO) %>%
  summarise(drought = max(drought), earthquake = max(earthquake))





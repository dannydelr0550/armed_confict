library(here)
library(dplyr)
library(purrr)
library(tidyr)

here()
infant <- read.csv(here('original', 'infantmortality.csv'), header = TRUE)
neonatal <- read.csv(here('original', 'neonatalmortality.csv'), header = TRUE)
under5 <- read.csv(here('original', 'under5mortality.csv'), header = TRUE)
matmort <- read.csv(here('original', 'maternalmortality.csv'), header = TRUE)

clean_data <- function(dataframe){
  dataframe_name <- deparse(substitute(dataframe))  # Get the name of the dataframe as a string
  dataframe_filt <- dplyr::select(dataframe, c(Country.Name, X2000:X2019))
  dataframe_filt_long <- dataframe_filt %>%
    tidyr::pivot_longer(
      cols = dplyr::starts_with("X"),  # Select columns starting with 'X'
      names_to = "Year",        # New column name for year
      names_prefix = "X",       # Remove prefix 'X' from column names
      values_to = dataframe_name      # New column name for values
    ) %>%
    mutate(Year = as.numeric(Year))  # Convert 'Year' to numeric
  return(dataframe_filt_long)  # Return the transformed dataframe
}

matmort_cleaned <- clean_data(matmort)
neonatal_cleaned <- clean_data(neonatal)
under5_cleaned <- clean_data(under5)
infant_cleaned <- clean_data(infant)

data_list <- list(under5_cleaned, matmort_cleaned, neonatal_cleaned, infant_cleaned)

merged_data <- reduce(data_list, full_join, by = c("Country.Name", "Year"))


merged_data$ISO <- countrycode(merged_data$Country.Name,
                            origin = "country.name",
                            destination = "iso3c")

merged_data <- merged_data[,-1]



View(merged_data)
disaster <- read.csv(here('original', 'disaster.csv'), header = TRUE)


disaster_filtered <- filter(disaster, Year %in% 2000:2019, Disaster.Type %in% c('Earthquake', 'Drought'))
disaster_filtered_select <- dplyr::select(disaster_filtered, c('Year', 'ISO', 'Disaster.Type')) %>%
  mutate(drought = if_else(Disaster.Type == 'Drought', 1, 0)) %>%
  mutate(earthquake = if_else(Disaster.Type == 'Earthquake', 1, 0))

disaster_filtered_select <- disaster_filtered_select %>% group_by(Year, ISO) %>%
  summarise(drought = max(drought), earthquake = max(earthquake))

conflictdata <- read.csv(here('original', 'conflictdata.csv'), header = TRUE)


conflictdata_select <- conflictdata %>% group_by(year, ISO) %>%
  summarise(best = max(best))  %>%
  mutate(best = replace_na(best, 0)) %>%
  mutate(conflict = if_else(best > 1, 1, 0))  %>%
  mutate(year = year + 1)


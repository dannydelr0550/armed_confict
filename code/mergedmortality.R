library(here)
library(dplyr)
library(purrr)
library(tidyr)
library(countrycode)

infant <- read.csv(here('original', 'infantmortality.csv'), header = TRUE)
neonatal <- read.csv(here('original', 'neonatalmortality.csv'), header = TRUE)
under5 <- read.csv(here('original', 'under5mortality.csv'), header = TRUE)
matmort <- read.csv(here('original', 'maternalmortality.csv'), header = TRUE)

clean_data <- function(dataframe){
  dataframe_name <- deparse(substitute(dataframe))  # Get the name of the dataframe as a string
  dataframe_filt <- dplyr::select(dataframe, c(Country.Name, X2000:X2019))
  dataframe_filt_long <- dataframe_filt %>%
    tidyr::pivot_longer(
      cols = dplyr::starts_with("X"),  
      names_to = "Year",        
      names_prefix = "X",       
      values_to = dataframe_name      
    ) %>%
    mutate(Year = as.numeric(Year))  
  return(dataframe_filt_long)  
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
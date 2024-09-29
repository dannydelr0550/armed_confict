library(here)
here()
library(dplyr)

matmort <- read.csv(here('original', 'maternalmortality.csv'), header = TRUE)


matmort_filt <- dplyr::select(matmort, c(Country.Name, X2000:X2019))
matmort_filt_long <- matmort_filt %>%
  tidyr::pivot_longer(
    cols = dplyr::starts_with("X"),  # Select columns starting with 'X'
    names_to = "Year",        # New column name for year
    names_prefix = "X",       # Remove prefix 'X' from column names
    values_to = "MatMor"      # New column name for values
  ) %>%
  mutate(Year = as.numeric(Year))  # Convert 'Year' to numeric

View(matmort_filt_long)
library(here)
library(dplyr)
library(purrr)
library(tidyr)

conflictdata <- read.csv(here('original', 'conflictdata.csv'), header = TRUE)


conflictdata_select <- conflictdata %>% 
  group_by(year, ISO) %>%
  summarise(totaldeath = sum(best))  %>%
  mutate(conflict = if_else(totaldeath < 25, 0, 1))  %>%
  mutate(year = year + 1)
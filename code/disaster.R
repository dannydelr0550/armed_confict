library(here)
library(dplyr)

here()

disaster <- read.csv(here('original', 'disaster.csv'), header = TRUE)


disaster_filtered <- filter(disaster, Year %in% 2000:2019, Disaster.Type %in% c('Earthquake', 'Drought'))
disaster_filtered_select <- dplyr::select(disaster_filtered, c('Year', 'ISO', 'Disaster.Type')) %>%
  mutate(drought = if_else(Disaster.Type == 'Drought', 1, 0)) %>%
  mutate(earthquake = if_else(Disaster.Type == 'Earthquake', 1, 0))

disaster_filtered_select <- disaster_filtered_select %>% group_by(Year, ISO) %>%
  summarise(drought = max(drought), earthquake = max(earthquake))

View(disaster_filtered_select)





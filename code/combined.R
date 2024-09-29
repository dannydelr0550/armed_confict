library(here)
library(dplyr)

source(here('code', 'mergedmortality.R'))  
source(here('code', 'mergedconflict.R'))        
source(here('code', 'disaster.R'))      
covs <- read.csv(here('original', 'covariates.csv'))

mortdata<- merged_data #dr. mitani, I am 'renaming' this because it makes more sense under this name in this script. Perhaps I should have considered this initially when naming it. I'm sorry if this is bad practice!


colnames(disaster_filtered_select)[1] <- 'year'
colnames(mortdata)[1] <- 'year'

alllist <- list(conflictdata_select, mortdata, disaster_filtered_select)

final_mort_data <- reduce(alllist, full_join, by = c("ISO", "year"))


#this will give a table with the same amount of rows as covs, with the extra columns appended 
final_data <- left_join(covs, final_mort_data, by = c("ISO", "year"))

final_data <- final_data %>%
  mutate(drought = replace_na(drought, 0),
         earthquake = replace_na(earthquake, 0),
         totaldeath = replace_na(totaldeath, 0))

write.csv(final_data, file = here("output_files", "finaldata.csv"), row.names = FALSE)
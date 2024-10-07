library(here)
library(dplyr)
library(purrr)
library(tidyr)
library(table1)
library(ggplot2)

finaldata <- read.csv(here('output_files', 'finaldata.csv'), header = TRUE)

finaldata_mod <- finaldata %>%
  dplyr::select(country_name, ISO, year, matmort) %>%
  dplyr::filter(year < 2018) %>%
  arrange(ISO, year) %>%
  group_by(ISO) %>%
  mutate(diffmatmor = matmort - matmort[1L])%>%
  filter(year == 2017 & diffmatmor > 0)
  
key_countries <- finaldata_mod$ISO

finaldata_filt <- finaldata %>%
  filter(ISO %in% key_countries)

# Create the plot and save it as an object
maternal_mortality_plot <- finaldata_filt |>
  ggplot(aes(x = year, y = log(matmort + 1), group = ISO, color = country_name)) +
  geom_line(alpha = 1) +
  xlim(c(2000, 2017)) +
  labs(y = "Maternal mortality (log)", x = "Year", color = "Country",
       title = 'Trend in Countries with Increased Maternal Mortality') + 
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))
# Save the plot using ggsave
ggsave(
  filename = "maternal_mortality_trend.png", # or use other formats like .pdf, .jpeg
  plot = maternal_mortality_plot,
  path = "output_files", # specify the folder where you want to save the plot
  width = 10, # adjust the width as needed
  height = 6, # adjust the height as needed
  dpi = 500 # specify resolution for a high-quality image
)




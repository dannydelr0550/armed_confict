library(here)
library(dplyr)
library(purrr)
library(tidyr)
library(table1)

finaldata <- read.csv(here('output_files', 'finaldata.csv'), header = TRUE)


# Summarize data
arm_conf_year <- finaldata %>%
  group_by(ISO) %>%
  summarise(
    `GDP (1000s)` = mean(gdp1000, na.rm = TRUE),
    OECD = mean(OECD, na.rm = TRUE),
    `OECD In 2023` = mean(OECD2023, na.rm = TRUE),
    `Pop. Density` = mean(popdens, na.rm = TRUE),
    Urban = mean(urban, na.rm = TRUE),
    `Age Dependence` = mean(agedep, na.rm = TRUE),
    `Male Education` = mean(male_edu, na.rm = TRUE),
    Temperature = mean(temp, na.rm = TRUE),
    Rainfall = mean(rainfall1000, na.rm = TRUE),
    armconfsum = sum(conflict)
  ) %>%
  mutate(
    conflict_groups = case_when(
      armconfsum == 0 ~ 0,
      armconfsum > 0 & armconfsum <= 5 ~ 1,
      armconfsum > 5 ~ 2
    )
  )


# Custom rendering function
my.render.cont <- function(x) {
  stats <- stats.default(x)
  stats <- stats.apply.rounding(stats, digits = 2)
  c("", "Median [Min, Max]" = sprintf("%s [%s, %s]", stats$MEDIAN, stats$MIN, stats$MAX))
}

caption <- "Key country demographic variables broken down by conflict level"

arm_conf_year$conflict_groups <- factor(arm_conf_year$conflict_groups, 
                         levels=c(0,1,2),
                         labels=c("No conflict",
                                  "Less than 5 years of  conflict", 
                                  "More than 5 years of conflict"))

table1(
  ~ `GDP (1000s)` + OECD + `OECD In 2023` + `Pop. Density` + Urban + `Age Dependence` + `Male Education` + Temperature + Rainfall | factor(conflict_groups), 
  data = arm_conf_year, render.continuous = my.render.cont, render.missing = NULL, caption = caption, overall = c(left = "Total")
)







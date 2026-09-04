# Week 2 Practice with dplyr and tidyverse
# MUSA 5080
# 9/4/2026
# Tatar Anurakwongsri

library(tidyverse)
library(tidycensus)

pa_income = get_acs(geography = "county",
                    variables = "B19013_001",
                    state = "PA",
                    year = 2023,
                    survey = "acs5")

dim(pa_income)
glimpse(pa_income)
head(pa_income)

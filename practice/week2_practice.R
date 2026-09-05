# Week 2 Practice with dplyr and tidyverse
# MUSA 5080
# 9/4/2026
# Tatar Anurakwongsri

library(tidyverse)
library(tidycensus)

pa_income <- get_acs(geography = "county",
                    variables = "B19013_001",
                    state = "PA",
                    year = 2023,
                    survey = "acs5")

dim(pa_income)
glimpse(pa_income)
head(pa_income)
# There are 67 rows (counties), which matches

pa_income$GEOID
as.numeric("01001") # gets rids of 0 in front

## 4. Filter
filter(pa_income, estimate > 60000) # probably less than 10
filter(pa_income, moe > 3000)
filter(pa_income, estimate < 50000)

## 5. Select
select(pa_income, NAME, estimate, moe) # Don't show GEOID
select(pa_income, GEOID, estimate) # Show only GEOID and estimate

## 6. Mutate
pa_income <- mutate(pa_income, moe_pct = moe/estimate*100)
pa_income
# moe_pct is the percentage of moe relative to the estimate

## 7. Arrange
arrange(pa_income, moe_pct) # arrange by moe_pct (ascending)
arrange(pa_income, desc(moe_pct)) # Cameron County, Pennsylvania

## 8. PIPE
pa_income %>%
  filter(moe_pct > 5) %>% # more than 5 moe_pct
  arrange(desc(moe_pct)) %>% # arrange in descending
  select(NAME, estimate, moe, moe_pct) # no GEOID

# Keep counties with moe_pct over 8, sort by estimate, show NAME and moe_pct
worst <- pa_income |>
  filter(moe_pct > 8) |>
  arrange(estimate) |>
  select(NAME, moe_pct)

## 9. group_by and summary
pa_income <- mutate(pa_income, reliable = moe_pct < 5)
pa_income %>%
  group_by(reliable) %>%
  summarize(n = n(),
            avg_income = mean(estimate)) # I predict 2 rows

## 10. case_when
pa_income <- pa_income %>%
  mutate(reliability = case_when(
    moe_pct < 3 ~ "High confidence",
    moe_pct < 6 ~ "Moderate",
    TRUE        ~ "Low confidence"
  ))

count(pa_income, reliability) # high (26), moderate (34), low (7)

## 11. Two variables, and a shape problem
# Is the margin of error bigger in small counties?

pa_two <- get_acs(
  geography = "county",
  variables = c("B19013_001", "B01003_001"),
  state = "PA", year = 2023, survey = "acs5"
)
pa_two # 134 rows because we added another table

pa_wide <- get_acs(
  geography = "county",
  variables = c(income = "B19013_001",
                pop    = "B01003_001"),
  state = "PA", year = 2023, survey = "acs5",
  output = "wide"
)
pa_wide # E from estimate, M from margin of error

pa_plot <- pa_wide %>%
  mutate(moe_pct = incomeM / incomeE * 100) %>%
  arrange(desc(moe_pct)) %>%
  select(NAME, popE, incomeE, moe_pct)

plot(pa_plot$moe_pct, pa_plot$popE)


## Additional Practice

# 1.
me_wide <- get_acs(
  geography = "county",
  variables = c(income = "B19013_001",
                pop    = "B01003_001"),
  state = "ME", year = 2023, survey = "acs5",
  output = "wide"
)

me_plot <- me_wide %>%
  mutate(moe_pct = incomeM / incomeE * 100) %>%
  arrange(desc(moe_pct)) %>%
  select(NAME, popE, incomeE, moe_pct)

plot(me_plot$moe_pct, me_plot$popE)

# The pattern is still discernible, but not as clear as Pennsylvania

# 2.
vars <- load_variables(2023, "acs5")
county_vars <- vars %>%
  filter(geography == "county", label == "Estimate!!Total:")

# B08603_001: Travel Time to Work for Workplace Geography
# B08601_001: Means of Transportation to Work for Workplace Geography

pa_wide2 <- get_acs(
  geography = "county",
  variables = c(travel_time        = "B08603_001",
                transport_method   = "B08601_001"),
  state = "PA", year = 2023, survey = "acs5",
  output = "wide"
)
pa_wide2

# 3.
pa_wide3 <- get_acs(
  geography = "tract",
  variables = c(income = "B19013_001",
                pop    = "B01003_001"),
  state = "PA", year = 2023, survey = "acs5",
  output = "wide"
)
pa_wide3
# The margin of error for income is way higher and there is a margin of error for population now


# For fun
tract_vars <- vars %>%
  filter(geography == "tract", label == "Estimate!!Total:")

# B05001_001: Nativity and Citizenship Status in the United States
# C27016_001 Health Insurance Coverage Status by Ratio of Income to Poverty
me_tract <- get_acs(
  geography = "tract",
  variables = c(nativity        = "B05001_001",
                insurance_ratio = "C27016_001"),
  state = "ME", year = 2023, survey = "acs5",
  output = "wide"
)



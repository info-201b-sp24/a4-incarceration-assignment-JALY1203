library(tidyverse)

# load data
pris_pop <- read.csv("https://github.com/melaniewalsh/Neat-Datasets/blob/main/us-prison-pop.csv?raw=true", stringsAsFactors = FALSE)

summary_info <- list()

# state with highest prison population in the latest available year

summary_info$state_highest_pop_now <- pris_pop %>% 
  filter(year == 2016) %>%
  filter(total_prison_pop == max(total_prison_pop, na.rm = TRUE)) %>%
  select(state, year)

# state with lowest prison population in the latest available year

summary_info$state_lowest_pop_now <- pris_pop %>% 
  filter(year == 2016) %>%
  slice_min(total_prison_pop, with_ties = FALSE) %>%
  select(state)

# state with highest black prisoner ratio

summary_info$state_highest_black_prison_ratio <- pris_pop %>%
  select(state, black_prison_pop, total_prison_pop) %>%
  group_by(state) %>%
  summarise(black_prison_pop = sum(black_prison_pop, na.rm = TRUE),
            total_prison_pop = sum(total_prison_pop, na.rm = TRUE)) %>%
  mutate(ratio = black_prison_pop / total_prison_pop) %>%
  filter(ratio == max(ratio, na.rm = TRUE)) %>%
  pull(state)

# highest POCs imprisoned

summary_info$highest_poc_prison_pop <- pris_pop %>%
  summarize(
    highest_poc_prison_pop = max(
      aapi_prison_pop,
      black_prison_pop,
      latinx_prison_pop,
      native_prison_pop,
      na.rm = TRUE
    )) %>%
  select(highest_poc_prison_pop)

# year with highest amount of prisoners

summary_info$year_highest_prisoners <- pris_pop %>%
  filter(total_prison_pop == max(total_prison_pop, na.rm = TRUE)) %>%
  select(year, state)

print(summary_info)
  
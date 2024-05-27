library(tidyverse)
library(maps)
library(usmap)

prison_pop <- read.csv("https://github.com/melaniewalsh/Neat-Datasets/blob/main/us-prison-pop.csv?raw=true", stringsAsFactors = FALSE)

# proportion of black prisoners to whole population in prison
race_pop <- prison_pop %>%
  group_by(state) %>%
  summarize(black_pop = sum(black_prison_pop, na.rm = TRUE),
            total_pop = sum(total_prison_pop, na.rm = TRUE))

race_pop <- race_pop %>%
  mutate(black_prop = black_pop / total_pop)


# load map
state_map <- map_data("state")

# load map data frame
map_df <- data.frame(state = state.abb, region = tolower(state.name))

# join map data and race data
race_pop <- race_pop %>%
  left_join(map_df)

# plot map
ggplot() + 
  geom_map(data = state_map, map = state_map, aes(x = long, y = lat, map_id = region)) +
  geom_map(data = race_pop, map = state_map, aes(fill = black_prop, map_id = region)) +
  scale_fill_continuous(name = "Proportion of Black Prisoners to Total Prisoners") +
  coord_map("albers", lat0 = 40, lat1 = 45) +
  labs(title = "Proportion of Black Prisoners to Total Prisoners in The United States", x = "Latitutde", y = "Longitude" ) +
  theme_minimal()


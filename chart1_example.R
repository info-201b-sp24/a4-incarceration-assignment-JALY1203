library(tidyverse)

prison_pop <- read.csv("https://github.com/melaniewalsh/Neat-Datasets/blob/main/us-prison-pop.csv?raw=true",stringsAsFactors = FALSE)

race_ratio <- prison_pop %>%
  filter(year < 2017) %>%
  group_by(year) %>%
  summarise(total_pop = sum(total_prison_pop,na.rm = TRUE),
            black_pop = sum(black_prison_pop,na.rm = TRUE),
            white_pop = sum(white_prison_pop,na.rm = TRUE)) %>%
  mutate(black_ratio = black_pop / total_pop,
         white_ratio = white_pop / total_pop)

race_ratio <- race_ratio %>%
  select(year, black_ratio, white_ratio)

# Create the line graph
ggplot(race_ratio, aes(x = year)) +
  geom_line(aes(y = black_ratio, color = "Black Ratio"), size = 1.2) +
  geom_line(aes(y = white_ratio, color = "White Ratio"), size = 1.2) +
  labs(
    title = "Trends in Black and White Prison Population Ratios",
    x = "Year",
    y = "Prison Population Ratio",
    color = "Population Group"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12)
  )
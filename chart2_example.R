library(tidyverse)

prison_pop <- read.csv("https://github.com/melaniewalsh/Neat-Datasets/blob/main/us-prison-pop.csv?raw=true",stringsAsFactors = FALSE)

highest_black_prison_pop <- prison_pop %>%
  summarize(highest_black_prison_pop = max(black_prison_pop,na.rm = TRUE)) %>%
  select(highest_black_prison_pop)

highest_white_prison_pop <- prison_pop %>%
  summarize(highest_white_prison_pop = max(white_prison_pop, na.rm = TRUE)) %>%
  select(highest_white_prison_pop)

# combine into one data frame
highest_prison_pops <- data.frame(
  Group = c("Black", "White"),
  Population = c(highest_black_prison_pop$highest_black_prison_pop, highest_white_prison_pop$highest_white_prison_pop)
)

# create bar graph
ggplot(highest_prison_pops, aes(x = Group, y = Population, fill = Group)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Highest Prison Populations by Group",
    x = "Race Group",
    y = "Prison Population"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    legend.position = "none"
  )

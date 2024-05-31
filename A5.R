us_prison_rates <- read.csv("https://raw.githubusercontent.com/melaniewalsh/Neat-Datasets/main/us-prison-jail-rates.csv")
yr_wa <- read.csv("https://raw.githubusercontent.com/melaniewalsh/Neat-Datasets/main/us-prison-jail-rates-1990-WA.csv")

library("dplyr")
library("tidyverse")
library("ggplot2")
library("tidyr")


## Questions with the values: 
# How many rows and columns are in the most used data set?
row <- nrow(us_prison_rates)
col <- ncol(us_prison_rates)
row1 <- nrow(yr_wa)
col2 <- ncol(yr_wa)
# Where is the black prison population rate the highest? (county/state)
black_pop_highest <- us_prison_rates %>%
  filter(black_prison_pop_rate == max(black_prison_pop_rate, na.rm = TRUE)) %>%
  pull(county_name)

# Where is the black prison population the lowest? 
black_pop_lowest <- us_prison_rates %>%
  filter(black_prison_pop_rate == min(black_prison_pop_rate, na.rm = TRUE)) %>%
  pull(county_name)


# What urban (city) county has the highest rate of white prisoners?
white_prison_urban <- us_prison_rates %>%
  filter(urbanicity == "urban") %>%
  filter(white_prison_pop_rate == max(white_prison_pop_rate, na.rm = TRUE)) %>%
  pull(county_name)

# What urban (city) has the highest rate of black prisoners? 
black_prison_urban <- us_prison_rates %>%
  filter(urbanicity == "urban") %>%
  filter(black_prison_pop_rate == max(black_prison_pop_rate, na.rm = TRUE)) %>%
  pull(county_name)
  
# What is the average black prison rate in King County, Washington at the most recent year?
king_county_black_prison_rates <- yr_wa %>%
  filter(!is.na(black_prison_pop_rate)) %>%
  filter(year == max(year)) %>%
  filter(county_name == "King County") %>%
  filter(black_prison_pop_rate == mean(black_prison_pop_rate, na.rm = TRUE)) %>%
  pull(black_prison_pop_rate)
  
  
# What are the white prison rates in King County, Washington at the most recent year?
king_county_white_prison_rates <- yr_wa %>%
  filter(!is.na(white_prison_pop_rate)) %>%
  filter(year == max(year)) %>%
  filter(county_name == "King County") %>%
  filter(white_prison_pop_rate == mean(white_prison_pop_rate, na.rm = TRUE)) %>%
  pull(white_prison_pop_rate)

  
# Chart 1: Trends over Time, Black Incarceration rate in King County, Washington
years_king <- yr_wa %>%
  filter(county_name == "King County") %>%
  pull(year)

black_prison_rate_king <- yr_wa %>%
  filter(county_name == "King County") %>%
  pull(black_prison_pop_rate)

white_prison_rate_king <- yr_wa %>%
  filter(county_name == "King County") %>%
  pull(white_prison_pop_rate)

ggplot() +
  geom_line(aes(x = years_king, y = black_prison_rate_king, colour = "Black Prison Rate")) +
  geom_line(aes(x = years_king, y = white_prison_rate_king, colour = "White Prison Rate")) +
  labs(
    title = "Trends in Black and White Prison Rates in King County, WA",
    x = "Year",
    y = "Black vs. White Prison Rates",
    color = "Mean Prison Rates"
  )


# Chart 2: Comparing Variables 
king_county_bpr <- yr_wa %>%
  filter(county_name == "King County") %>%
  pull(black_prison_pop_rate)

king_county_total_pop <- yr_wa %>%
  filter(county_name == "King County") %>%
  pull(total_pop)

data <- data.frame(king_county_total_pop, king_county_bpr)

ggplot(data = data) +
  geom_point(aes(x = king_county_total_pop,  y = king_county_bpr, color = "Percent of Black Prisoners according to the total population")) +
  scale_color_manual(values = c("Black", "Blue")) +
  labs(
    title = "Comparison of the Total Population versus Black Prison Populations in King County",
    x = "King County Total Population",
    y = "King County Black Prison Population",
    color = "Prison Rate/Population"
  )


# Map
# black prison pop across  
us_states <- us_prison_rates %>%
  group_by(state) %>%
  filter(!is.na(black_prison_pop_rate)) %>%
  filter(year == max(year)) %>%
  summarise(mean_black_prison_rate = mean(black_prison_pop_rate, na.rm = TRUE))

map_data <- map_data("state")


states_map <- map_data("state") %>%
  rename(state = region) %>%
  left_join(us_states, by = "state") %>%
  distinct(state, long, lat, group, .keep_all = TRUE) %>%
  reframe(
    mean_black_prison_rate = unique(us_states$mean_black_prison_rate),
    state = unique(us_states$state),
    long = long,
    lat = lat
  ) %>%
  filter(!is.na(mean_black_prison_rate))
  

  ggplot(states_map) +
  geom_polygon(mapping = aes(x = long, y = lat, group = group, fill = mean_black_prison_rate), color = "white", linewidth = .1) +
  labs(
    title = "Black Prison Rates Throughout the States in the U.S. in 2018",
    fill = "Black Prison Rate",
  ) +
  coord_map() +
  scale_fill_continuous(low = "#78787879", high = "#155555")





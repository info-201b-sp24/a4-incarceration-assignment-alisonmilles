states_map <- map_data("state") %>%
  rename(state = region) %>%
  filter(states_map$state == unique(states_map$state)) %>%
  group_by(long, lat, group, state) %>%
  left_join(us_states, by = "state") %>%
  reframe(mean_black_prison_rate = us_states$mean_black_prison_rate) %>%
  filter(!is.na(mean_black_prison_rate))

## if i cant figure out the map, i can go back to this even tho it half works

states_map <- map_data("state") %>%
  rename(state = region) %>%
  left_join(us_states, by = "state") %>%
  group_by(state) %>%
  reframe(
    mean_black_prison_rate = unique(us_states$mean_black_prison_rate),
    state = unique(us_states$state),
    long = mean(long),
    lat = mean(lat),
    group = first(group)
  ) %>%
  filter(!is.na(mean_black_prison_rate))

library(ggplot2)
library(dplyr)
library(knitr)
library(tidyr)
library(leaflet)
# summary information computations
shootings_2018 <- read.csv("data/shootings-2018.csv")

number_of_shootings <- nrow(shootings_2018) #340 shootings

number_killed <- summarize(shootings_2018,
    total_killed = sum(num_killed)
  ) #373 killed total

most_impacted <- shootings_2018 %>%
 group_by(city) %>%
  summarise(all_injured = sum(num_injured)) %>%
  filter(all_injured == max(all_injured)) %>%
  pull(city) #Chicago
  
avg_deaths <- mean(shootings_2018$num_killed) #1.097 deaths

max_injured <- max(shootings_2018$num_injured) #19 injured

# summary table

four_states <- select(shootings_2018, state, num_killed, num_injured) %>%
filter(state == "Illinois" | state == "California" | state == "New York"
 | state == "Michigan")

cali_summary <- four_states %>%
 group_by(state) %>%
 filter(state == "California") %>%
 summarise(total_killed = sum(num_killed),
 total_injured = sum(num_injured))
  

illi_summary <- four_states %>%
  group_by(state) %>%
  filter(state == "Illinois") %>%
  summarise(total_killed = sum(num_killed),
            total_injured = sum(num_injured))

mich_summary <- four_states %>%
  group_by(state) %>%
  filter(state == "Michigan") %>%
  summarise(total_killed = sum(num_killed),
            total_injured = sum(num_injured))

ny_summary <- four_states %>%
  group_by(state) %>%
  filter(state == "New York") %>%
  summarise(total_killed = sum(num_killed),
            total_injured = sum(num_injured))

four_st_summ <- rbind(cali_summary, illi_summary, mich_summary, ny_summary)
#combines each state's information.
kable(four_st_summ, col.names =  c("state", "total_killed",
                                  "total_injured")) #yeet

#Particular Incident
#  date of incident
inc_date <- shootings_2018 %>%
  select(date) %>%
  filter(date == "May 1, 2018") %>%
pull(date)

#  city where incident occurred
inc_city <- shootings_2018 %>%
  group_by(city) %>%
  filter(date == "May 1, 2018") %>%
pull(city)

#  state where incident ocurred
inc_state <- shootings_2018 %>%
  group_by(state) %>%
  filter(date == "May 1, 2018") %>%
pull(state)

#  number of people killed in incident
inc_killed <- shootings_2018 %>%
  group_by(num_killed) %>%
  filter(date == "May 1, 2018") %>%
  pull(num_killed)
#  number of people injured
inc_injured <- shootings_2018 %>%
  group_by(num_injured) %>%
  filter(date == "May 1, 2018") %>%
  pull(num_injured)

#interactive plot
#  making a map that contains coordinates of each shooting and then containing
#   information of city, state, number of injured, and number of killed
shootings_map <- leaflet(shootings_2018) %>%
  addTiles() %>%
  addMarkers(
    lng = ~long, lat = ~lat, label = ~paste0(city, ", ", state,
           ", injured: ", num_injured, ", deaths: ", num_killed )
  )

# My own plot
#  I made a bar graph that displays four major states that are geographically
#   within a lot of shootings
condensed_states <- shootings_2018 %>%
  group_by(state) %>%
  filter(state == "Illinois" | state == "New York" | state == "Florida" |
           state == "California" | state == "Tennessee")

my_graph <- ggplot(condensed_states) +
  geom_col(mapping = aes(
    x = state,
    y = num_injured
  ))
my_graph
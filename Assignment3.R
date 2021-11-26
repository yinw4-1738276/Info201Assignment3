library(ggplot2)
library(tidyverse)

df <- read.csv("/Users/yin/assignment-3---incarceration-yinw4-1738276/incarceration_trends.csv") #don't forget the ""

# population each year from 1970 to 2018
tot_pop_df <- df %>%
  select(year, total_pop) %>%
  arrange(year) %>%
  group_by(year) %>%
  summarize(total = sum(total_pop))

ggplot(tot_pop_df, aes(x = year, y = total)) +
  geom_point()+
  geom_smooth(method=lm, se=FALSE)+
  labs(titles = "Total population throughout the years")

# US population by race
race_df <- df %>%
  select(year, aapi_pop_15to64, black_pop_15to64, latinx_pop_15to64, native_pop_15to64, white_pop_15to64) %>%
  group_by(year) %>%
  summarize(aapi = sum(aapi_pop_15to64), black = sum(black_pop_15to64), latin = sum(latinx_pop_15to64), native = sum(native_pop_15to64), white = sum(white_pop_15to64)) %>%
  na.omit()

ggplot(race_df) +
  geom_line(aes(x = year, y = white, colour = "White"))+
  geom_line(aes(x = year, y = latin, colour = "Latin")) +
  geom_line(aes(x = year, y = black, colour = "Black")) +
  geom_line(aes(x = year, y = aapi, colour = "Asian American")) +
  geom_line(aes(x = year, y = native, colour = "Native American")) +
  theme_bw()+
  labs(title = "Total popultaion vary by race", x = "year", y = "popultaion")

# Variable 1: jail_tot_population/ US population each year
jail_pop_df <- df %>%
  select(year, total_jail_pop) %>%
  na.omit() %>%
  arrange(year) %>%
  group_by(year) %>%
  summarize(total = sum(total_jail_pop))

combine_jail_tot <- merge(tot_pop_df, jail_pop_df, by = "year")
combine_jail_tot <- combine_jail_tot %>%
  summarize(year, ratio = total.y / total.x)

decrease <- (combine_jail_tot[39,2] - combine_jail_tot[49,2])/(combine_jail_tot[39,2]) * 100

ggplot(jail_pop_df, aes(x = year, y = total))+
  geom_point()

# Variable 2 & 3: male and female percentage jail inmate change from 2008-2018
gender_diff_df <- df %>%
  select(year, female_jail_pop, male_jail_pop) %>%
  na.omit() %>%
  arrange(year) %>%
  group_by(year) %>%
  summarize(female = sum(female_jail_pop), male =  sum(male_jail_pop)) %>%
  filter(year >= 2008)

percent_female_increase <- (gender_diff_df[11,2] - gender_diff_df[1,2])/(gender_diff_df[1,2]) * 100
percent_male_decrease <- (gender_diff_df[1,3] - gender_diff_df[11,3])/(gender_diff_df[1,3]) * 100

# Variable 4 & 5: percentage cahnge of the jail incarceration rate for whites and blacks from 2008-2018
race_jail_df <- df %>%
  select(year, aapi_jail_pop_rate, black_jail_pop_rate, latinx_jail_pop_rate, native_jail_pop_rate, white_jail_pop_rate) %>%
  na.omit() %>%
  group_by(year) %>%
  summarize(aapi = sum(aapi_jail_pop_rate), black = sum(black_jail_pop_rate), latin = sum(latinx_jail_pop_rate), 
                  native = sum(native_jail_pop_rate), white = sum(white_jail_pop_rate))

race_08to18 <- race_jail_df%>%
  filter(year >= 2008)

percent_race <- (race_08to18[1,] - race_08to18[11,]) / race_08to18[1,] *100
white <- -1* (percent_race$white)
black <- percent_race$black

#------------

prison_rate_df <- df %>%
  select(year, total_prison_pop_rate) %>%
  na.omit() %>%
  arrange(year) %>%
  group_by(year) %>%
  summarize(total = sum(total_prison_pop_rate)) 

ggplot(prison_rate_df, aes(x = year, y = total)) +
  geom_point()

increasep <- prison_rate_df[47,2] / prison_rate_df[1,2]

# Total jail population throughout the year
tot_jail_adm_df <- df %>%
  select(year, total_jail_adm) %>%
  na.omit() %>%
  arrange(year) %>%
  group_by(year) %>%
  summarize(total = sum(total_jail_adm)) %>%
  filter(year > 1979)

max_jail_adm <- tot_jail_adm_df %>%
  filter(total == max(total)) %>%
  pull(year) #2004

sec_max <- tot_jail_adm_df %>%
  filter(year != max_jail_adm) %>%
  filter(total == max(total)) %>%
  pull(year) #2008

ggplot(tot_jail_adm_df, aes(x = year, y = total)) +
  geom_line()

# total prison population throughout the year
tot_prison_adm_df <- df %>%
  select(year, total_prison_adm) %>%
  na.omit() %>%
  arrange(year) %>%
  group_by(year) %>%
  summarize(total = sum(total_prison_adm)) %>%
  filter(year > 1979)


max_prison_adm <- tot_prison_adm_df %>%
  filter(total == max(total)) %>%
  pull(year) #2008

ggplot(tot_prison_adm_df, aes(x = year, y = total)) +
  geom_line()


# combine jail and prison ggplot2
combined_df <- merge(x = tot_jail_adm_df, y = tot_prison_adm_df, by = "year")
ratio_df <- combined_df %>%
  summarize(year, ratio = total.y/total.x)
ggplot(ratio_df, aes(x = year, y = ratio)) +
  geom_line()

ggplot(combined_df)+
  geom_line(aes(x = year, y = total.x))+
  geom_line(aes(x = year, y = total.y))


## Chart 1: A chart that shows trends over time for a variable of your choice
## jail by race
race_jail_df <- df %>%
  select(year, aapi_jail_pop_rate, black_jail_pop_rate, latinx_jail_pop_rate, native_jail_pop_rate, white_jail_pop_rate) %>%
  na.omit() %>%
  group_by(year) %>%
  summarize(aapi = sum(aapi_jail_pop_rate), black = sum(black_jail_pop_rate), latin = sum(latinx_jail_pop_rate), native = sum(native_jail_pop_rate), white = sum(white_jail_pop_rate))

ch1 <- ggplot(race_jail_df) +
  geom_line(aes(x = year, y = white, colour = "White"))+
  geom_line(aes(x = year, y = latin, colour = "Hispanic")) +
  geom_line(aes(x = year, y = black, colour = "Black")) +
  geom_line(aes(x = year, y = aapi, colour = "Asian American")) +
  geom_line(aes(x = year, y = native, colour = "Native American")) +
  theme_bw()+
  labs(title = "Jail Incarceration Rate by Race", x = "year", y = "incarceration rate")


## Chart 2: How two different (continuous) variables are related to one another.
# The difference between men and women throughout the year
gender_diff_df <- df %>%
  select(year, female_jail_pop, male_jail_pop) %>%
  na.omit() %>%
  arrange(year) %>%
  group_by(year) %>%
  summarize(female = sum(female_jail_pop), male =  sum(male_jail_pop))

tot_jail_pop_df <- df %>%
  select(year, total_jail_pop) %>%
  na.omit() %>%
  arrange(year) %>%
  group_by(year) %>%
  summarize(total = sum(total_jail_pop))

ch2 <- ggplot(gender_diff_df) +
#  geom_line(aes(x = year, y = tot_jail_pop_df$total)) +
  geom_area(aes(x = year, y = male, fill = "male")) +
  geom_area(aes(x = year, y = female, fill = "female")) +
  labs(title = "Gender vs total jail population", x = "year", y = "jail population")


## Chart 3: Map
map <- map_data("state") #%>% as_tibble()

map_df <- df %>%
  filter(year >= 2008) %>%
  select(state, total_jail_pop_rate) %>%
  na.omit() %>%
  group_by(state) %>%
  summarize(tot = sum(total_jail_pop_rate))
map_df$state = state.name[match(map_df$state,state.abb)]
map_df$state = tolower(map_df$state)
map_df <- map_df %>%
  na.omit()


ch3 <- ggplot(map_df, aes(fill = tot)) +
  geom_map(aes(map_id = state), map = map) +
  expand_limits(x = map$long, y = map$lat) +
  coord_map("ortho", orientation = c(39, -98, 0)) +
  theme_bw() +
  scale_fill_gradient2() +
  labs(title = "Total JailPopulation by State")


# -----------Not related to what's used in the Rmd file

jail_rate_df <- df %>%
  select(year, total_jail_pop_rate) %>%
  na.omit() %>%
  arrange(year) %>%
  group_by(year) %>%
  summarize(total = sum(total_jail_pop_rate))

ggplot(jail_rate_df, aes(x = year, y = total)) +
  geom_point()

increase <- jail_rate_df[49,2] / jail_rate_df[1,2]
increase10 <- jail_rate_df[39,2] / jail_rate_df[1,2]


state_rate_df <- df %>%
  select(year, state, total_jail_pop_rate) %>%
  na.omit() %>%
  arrange(year) %>%
  group_by(year, state) %>%
  summarize(total = sum(total_jail_pop_rate)) %>%
  filter(year == 1970 | year == 2018) %>%
  group_by(state) %>%
  summarize(diff = diff(total)) %>%
  arrange(- diff)

fisrt <- state_rate_df[1,1]
second <- state.name[match(state_rate_df[2,1],state.abb)]
third <-state.name[match(state_rate_df[3,1],state.abb)]
fourth <-state.name[match(state_rate_df[4,1],state.abb)]
fifth <- state.name[match(state_rate_df[5,1],state.abb)]

ggplot(state_rate_df, aes(x = state, y = diff))+
  geom_point()

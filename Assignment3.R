library(ggplot2)
library(tidyverse)

df <- read.csv("/Users/yin/assignment-3---incarceration-yinw4-1738276/incarceration_trends.csv") #don't forget the ""

# The total number of jurisdiction population each year from 1970 to 2018
tot_pop_df <- df %>%
  select(year, total_pop) %>%
  arrange(year) %>%
  group_by(year) %>%
  summarize(total = sum(total_pop))

ggplot(tot_pop_df, aes(x = year, y = total)) +
  geom_point()+
  geom_smooth(method=lm, se=FALSE)+
  labs(titles = "Total population throughout the years")

# The difference between men and women throughout the year
gender_diff_df <- df %>%
  select(year, female_pop_15to64, male_pop_15to64) %>%
  arrange(year) %>%
  group_by(year) %>%
  summarize(difference = abs(sum(female_pop_15to64) - sum(male_pop_15to64)))

ggplot(gender_diff_df, aes(x = year, y = difference)) +
  geom_point()+
  geom_smooth(method=lm, se=FALSE)


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

ggplot(combined_df)+
  geom_line(aes(x = year, y = total.x))+
  geom_line(aes(x = year, y = total.y))

#-------------race----------------
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
  labs(title = "Total popultaion of different race over the last 30 years", x = "year", y = "popultaion")
  
## Chart 1: A chart that shows trends over time for a variable of your choice


## Chart 2: How two different (continuous) variables are related to one another.


## Chart 3: Map
map_data <- map_data("state") #%>% as_tibble()

map_data %>%
  ggplot(aes(x = long, y = lat, map_id = region)) +
      geom_map(map = map_data, color = "gray80", fill = "gray10", size = 0.3)+
      coord_map("ortho", orientation = c(39, -98, 0))



# Hi

library(tidyverse)
library(readr)
library(zoo)

install.packages("readxl")

library(readxl)

home = read_csv("data/landdata-states.csv")

library(readxl)
PopulationEstimates_1_ <- read_excel("data/PopulationEstimates (1).xls", skip = 2)

p2 = PopulationEstimates_1_ %>%
  select(fips = FIPStxt, state = State, Area_Name, pop2019 = POP_ESTIMATE_2019) %>%
  group_by(state) %>%
  slice_max(pop2019, n = 1)

summary(p2)
str(p2)

names(home)
fivenum(home$Land.Value)

home %>%
  filter(State %in% c("HI", "CA", "NY"))
  group_by(State) %>%
  summarize(minLV = min(Land.Value),
            meanLV = mean(Land.Value),
            maxLV = max(Land.Value))

p1 = home %>%
  filter(State %in% c("HI", "TX", "AL")) %>%
  ggplot(aes(x = Date, y = Land.Value)) +
  geom_line(aes(color = State)) +
  facet_wrap(-State, scale = "free_y", ncol = 1) +
  theme_linedraw()

ggsave(p1, file = "img/lv-plot.png")


# Question 1

# Table 1: Counties with Most Cumulative Cases per Capita.

library(tidyverse)
library(readr)

home = read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")

library(readxl)
us_counties <- read_excel("uscounties.csv")

p2 = us_counties %>%
  select(fips, state, date, cases) %>%
  group_by(cases) %>%
  slice_max(cases, n = 5)

# Table 2: 5 Counties with most new cases per capita.

library(tidyverse)
library(readr)

home = read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")

library(readxl)
us_counties <- read_excel("uscounties.csv")

p1 = home %>%
  select(state, date)
  group_by(date) %>%
  slice_max(cases, n = 5)

summary(p2)
str(p2)

names(home)
fivenum(home$cases)

# Question 2

p1 = home %>%
  filter(State %in% c("NY", "CA", "LA", "FL")) %>%
  group_by(State) %>%
  summarize(mean.Date())
  ggplot(aes(x = Date, y = cases)) +
  geom_line(aes(color = State)) +
  facet_wrap(-State, scale = "free_y", ncol = 1) +
  theme_linedraw()
  theme("state", fill = "light-blue", color = "white")
  merge.data.frame()
  labs(title = "New Daily Cases",
       subtitle = "From New York Times Data",
       x = "Date",
       y = "Cases",
       caption = "Lab 2, GEOG 13")








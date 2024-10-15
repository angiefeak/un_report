gapminder_data <- read.csv("data/gapminder_data.csv")
gapminder_data <- read_csv("data/gapminder_data.csv")
library(tidyverse)
gapminder_data <- read_csv("data/gapminder_data.csv")
gapminder_data <- read_csv("data/gapminder_data.csv")
summarize(gapminder_data, averageLifeExp=mean(lifeExp))
gapminder_data %>% (averageLifeExp=mean(lifeExp))
gapminder_data %>%
  summarize(averageLifeExp=mean(lifeExp))
gapminder_data_summarized <- gapminder_data %>%
  summarize(averageLifeExp=mean(lifeExp))
gapminder_data %>% summarize(averagepop=mean(pop))
gapminder_data %>%  
  summarize(recent_year = max(year))
gapminder_data %>%
  filter(year == 2007) %>% 
  summarize(average=mean(lifeExp))
gapminder_data %>%  
  summarize(recent_year = min(year))
gapminder_data %>%
  filter(year == min(year)) %>%
  summarize(average_gdp=mean(gdpPercap))

#Grouping the Data by Continent-------------
  group_by(year) %>%
  summarize(average=mean(lifeExp))
gapminder_data %>%
  filter(year == min(year)) %>%
  group_by(continent) %>%
  summarize(average=mean(lifeExp))
gapminder_data %>%
  filter(year == max(year)) %>%
  group_by(continent) %>%
  summarize(average=mean(lifeExp))

#Summarize with multiple columns------------

gapminder_data %>%
  group_by(continent) %>% 
  summarize(average=mean(lifeExp), min=min(lifeExp))

#Make new variables with Mutate ---------------

gapminder_data %>%
  mutate(gdp = pop * gdpPercap)
gapminder_data %>% 
  mutate(popinmillions=pop/1000000)

#Subset columns using Select()-----------------
gapminder_data %>% 
 select(pop, year)
gapminder_data %>% 
  select(-continent)
gapminder_data %>% 
  select(country, continent, year, lifeExp)
gapminder_data %>% 
  select(year, starts_with("c"))
gapminder_data %>% 
  select(year, ends_with("p"))

#changing the shape of the data using tidyr pivot functions-----------

gapminder_data %>% 
  select(country, continent, year, lifeExp) %>% 
  pivot_wider(names_from = year, values_from = lifeExp)
gapminder_data_2007 <- read_csv("Data/gapminder_data.csv") %>%
  filter(year==2007 & continent=="Americas") %>%
  select(-year, -continent)

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

# Cleaning up the Data ------------

read_CSV("data/co2-un-data.csv")
library(tidyverse)
read_csv("data/co2-un-data.csv")
read_csv("data/co2-un-data.csv", skip = 1)
co2_emissions_dirty <- read_csv("data/co2-un-data.csv", skip=2, col_names=c("region", "country", "year", "series", "value", "footnotes", "source"))
co2_emissions_dirty
co2_emissions_dirty %>% 
  select(country, year, series, value)

# Recode () to rename columns -----------
co2_emissions <- co2_emissions_dirty %>% 
  select(country, year, series, value) %>% 
  mutate(series = recode(series, "Emissions (thousand metric tons of carbon dioxide)" = "total_emissions", "Emissions per capita (metric tons of carbon dioxide)" = "per_capita_emissions")) %>% 
  pivot_wider(names_from=series, values_from=value) %>% 
  filter(year==2005) %>% 
  select(-year)

# Joining Data Frames--------------------
gapminder_data_2007 <- read_csv("data/gapminder_data.csv") %>%
  filter(year == 2007 & continent == "Americas") %>%
  select(-year, -continent)
inner_join(gapminder_data_2007, co2_emissions, by="country")
anti_join(gapminder_data_2007, co2_emissions, by="country")

# Recoding the co2 dataset countries
co2_emissions <- read_csv("data/co2-un-data.csv", skip=2,
                          col_names=c("region", "country", "year",
                                      "series", "value", "footnotes", "source")) %>%
  select(country, year, series, value) %>%
  mutate(series = recode(series, "Emissions (thousand metric tons of carbon dioxide)" = "total",
                         "Emissions per capita (metric tons of carbon dioxide)" = "per_capita")) %>%
  pivot_wider(names_from=series, values_from=value) %>%
  filter(year==2005) %>%
  select(-year) %>%
  mutate(country=recode(country, "Bolivia (Plurin. State of)" = "Bolivia", "United States of America" = "United States", "Venezuela (Boliv. Rep. of)" = "Venezuela"))
#recoding puerto rico in gapminder data and combining with USA
gapminder_data_2007 <- read_csv("data/gapminder_data.csv") %>%
  filter(year == 2007 & continent == "Americas") %>%
  select(-year, -continent) %>%
  mutate(country = recode(country, "Puerto Rico" = "United States"))
gapminder_data <- read_csv("data/gapminder_data.csv") %>%
  filter(year == 2007 & continent == "Americas") %>%
  select(-year, -continent) %>%
  mutate(country = recode(country, "Puerto Rico" = "United States")) %>%
  group_by(country) %>%
  summarize(lifeExp = sum(lifeExp * pop)/sum(pop),
            gdpPercap = sum(gdpPercap * pop)/sum(pop),
            pop = sum(pop))
#back to joining
gapminder_co2 <- inner_join(gapminder_data, co2_emissions, by="country")

gapminder_co2 %>%  
  mutate(region = if_else(country == "Canada" | country == "United States" | country == "Mexico", "north", "south"))
write_csv(gapminder_co2, "data/gapminder_co2.csv")

#Plotting the joined data
ggplot(gapminder_co2)+
  aes(x=gdpPercap, y=per_capita)+
  geom_point()+
  labs(x="GDP (per capita)", y="CO2 emitted (per capita)", title="There is a strong correlation between GDP and \nemissions") +
  geom_smooth(method = lm)
gapminder_co2 %>%
  mutate(region = if_else(country == "Canada" | country == "United States" | country == "Mexico", "north", "south")) %>% 
  group_by(region) %>%
  summarize(sumtotal = sum(total),
            sumpop = sum(pop))


library(tidyverse)
library(dslabs)

#Les data. Merk at begge benyttede datasett her er innebygd i R. 

data("heights")

# Inspeksjon av heights-datasett

head(heights)
mean(heights$height)

#Sammenligning av h??yde blant menn og kvinner- boxplot 

heights %>% 
  group_by(sex) %>%
  summarise(mean_height = mean(height))
heights %>%
  ggplot(aes(x = sex, y = height)) +
  geom_boxplot()

# Les inn gapminder datasett

data("gapminder")
head(gapminder)


#Sammenligning av gjennomsnittlig levealder blant kontinenter- boxplot

gapminder %>%
  group_by(continent) %>%
  summarise(mean_life = mean(life_expectancy))
gapminder %>%
  ggplot(aes(x = continent, y = life_expectancy)) +
geom_boxplot()


#Les inn gapminder p?? nytt

data("gapminder")
head(gapminder)

#Inspeksjon.

names(gapminder)
glimpse(gapminder)

#Omkoding av gdp.

gapminder <- gapminder %>%
  mutate(gdp_per_capita = gdp / population)

#Utforsker forskjellige visualiseringsalternativer til forventet levealder. 

gapminder %>%
  ggplot(aes(x = gdp_per_capita, y = life_expectancy)) +
  geom_point()


gapminder %>%
  ggplot(aes(x = gdp_per_capita, y = life_expectancy)) +
  geom_point() +
  scale_x_log10()


gapminder %>%
  ggplot(aes(x = gdp_per_capita, y = life_expectancy, color = continent)) +
  geom_point() +
  scale_x_log10()

#Gjennomsnittlig levealder blant inng??ende kontinenter.

gapminder %>%
  group_by(continent) %>%
  summarise(
    mean_life = mean(life_expectancy),
    mean_gdp = mean(gdp_per_capita)
  )

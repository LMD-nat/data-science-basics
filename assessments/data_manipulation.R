### Extract the variables name and population from the cities data frame shown below.

library(dplyr)
cities %>% 
  select(name, population)
  
### Sort, in ascending order, the rows in the cities data frame by the unemployed and population variables.

cities %>% 
  mutate(unemployed = population * unemployment_rate/100) %>% 
  arrange(unemployed, population)
  
### Given the data df, shown below, re-order the data so that it is ordered by ascending Group and descending Obs.

library(dplyr)
df %>% arrange(Group, desc(Obs))

### Create a horizontal bar chart to show the number of site users who were in each of the control and treatment groups. 

ggplot(experiment, aes(group)) + 
  geom_bar() + 
  coord_flip()
  
### Update the column so that all industries are in lowercase.

library(dplyr)

companies <- companies %>%
  mutate(industry = tolower(industry))

head(companies)
  
### Sort the dataset by industry (in alphabetical order), and then by size such that the largest companies appear first.

library(dplyr)

companies %>%
  arrange(industry, desc(size)) %>%
  head()

### Add the year column to the data. 

library(dplyr)

years <- c(2018, 2018, 2017, 2016, 2017, 2016, 2018, 2016, 2016)

wine <- wine %>%  
  mutate(year = years)

head(wine)

### Calculate the mean and median of the tips received by the waiters and waitresses.

library(dplyr)

tips %>% summarize(mean = mean(tip), median = median(tip))

### Drop the variable unemployment_rate from the data frame shown below.

library(dplyr)

df %>%
 select(-unemployment_rate)
  
### The friends data is shown below. Preview only the first 2 rows of this data.

head(friends, 2)

### Visualize the relationship between the horsepower and the weight in the data frame auto previewed below.

ggplot(auto, aes(x = horsepower, y = weight)) +
 geom_point()
 
### Return the data for China for the year 1982.

gapminder %>% 
  filter(year == 1982, country == "China")
  
### Return the last six rows of the cities data frame shown below.

gapminder %>% 
  filter(year == 1982, country == "China")
  
### Create a bubble chart with levels on the y-axis and nsites on the x-axis. The size of the bubbles should be determined by towns.

ggplot(contamination, aes(nsites, levels, size = towns)) + 
  geom_point()
  
### Change the transparency of the points on the scatterplot. Use a transparency value of 0.3.

library(ggplot2)

ggplot(wine, aes(`citric acid`, pH)) + 
  geom_point(alpha=0.3)
 

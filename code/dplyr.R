# --------------------------------------------------------- Introduction to the Tidyverse 1:4 ------------------------------------------------#

library(gapminder)
library(dplyr)

# ---- Filter ---- #

# Filter the gapminder dataset for the year 1957
gapminder %>%
filter(year == 1957)

# Filter for China in 2002
gapminder %>%
filter(year == 2002, country == "China")

# ---- Arrange ---- #

gapminder %>%
# Sort in ascending order of lifeExp
arrange(lifeExp)
 
# Sort in descending order of lifeExp
gapminder%>%
arrange(desc(lifeExp))

# ---- Combine Filter and Arrange ---- #

# Filter for the year 1957, then arrange in descending order of population
gapminder %>%
filter(year == 1957) %>%
arrange(desc(pop))

# ---- Mutate ---- #

# Use mutate to change lifeExp to be in months
gapminder%>%
mutate(lifeExp = lifeExp * 12)

# Use mutate to create a new column called lifeExpMonths
gapminder %>%
mutate(lifeExpMonths = 12* lifeExp)

# ---- Combine Filter, Mutate and Arrange ---- #

# Filter, mutate, and arrange the gapminder dataset
gapminder %>%
filter(year == 2007)%>%
mutate(lifeExpMonths = 12 * lifeExp) %>%
arrange(desc(lifeExpMonths))

# ---- ggplot ---- #

# Create gapminder_1952
gapminder_1952 <- gapminder %>%
  filter(year == 1952)

# Change to put pop on the x-axis and gdpPercap on the y-axis
ggplot(gapminder_1952, aes(x = pop, y = gdpPercap)) +
  geom_point()

# Create a scatter plot with pop on the x-axis and lifeExp on the y-axis
ggplot(gapminder_1952, aes(x = pop, y = lifeExp)) +
  geom_point()

# Change this plot to put the x-axis on a log scale
ggplot(gapminder_1952, aes(x = pop, y = lifeExp)) +
  geom_point()+
  scale_x_log10()

# Scatter plot comparing pop and gdpPercap, with both axes on a log scale
ggplot(gapminder_1952, aes(x = pop, y = gdpPercap)) +
  geom_point()+
  scale_x_log10()
  scale_y_log10()

# Scatter plot comparing pop and lifeExp, with color representing continent
ggplot(gapminder_1952, aes(x = pop, y = lifeExp, color = continent)) +
  geom_point() +
  scale_x_log10()

# Add the size aesthetic to represent a country's gdpPercap
ggplot(gapminder_1952, aes(x = pop, y = lifeExp, color = continent,
size = gdpPercap)) +
  geom_point() +
  scale_x_log10()

# Scatter plot comparing pop and lifeExp, faceted by continent
ggplot(gapminder_1952, aes(x = pop, y = lifeExp, color = continent,
size = gdpPercap)) +
geom_point() +
scale_x_log10() +
facet_wrap(~ continent)

# Scatter plot comparing gdpPercap and lifeExp, with color representing continent
# and size representing population, faceted by year
ggplot(gapminder, aes(x = gdpPercap, y = lifeExp, color = continent,
size = pop)) +
geom_point() +
scale_x_log10() +
facet_wrap(~ year)

# ---- Summarize ---- #

# Summarize to find the median life expectancy
gapminder%>%
summarize(medianLifeExp =median(lifeExp))

# Filter for 1957 then summarize the median life expectancy
gapminder%>%
filter(year == 1957) %>%
summarize(medianLifeExp =median(lifeExp))

# Filter for 1957 then summarize the median life expectancy and the maximum GDP per capita
gapminder%>%
filter(year == 1957) %>%
summarize(medianLifeExp =median(lifeExp),maxGdpPercap = max(gdpPercap))

# ---- Group By ---- #

# Find median life expectancy and maximum GDP per capita in each year
gapminder %>%
group_by(year) %>%
summarize(medianLifeExp = median(lifeExp),
maxGdpPercap = max(gdpPercap))

# Find median life expectancy and maximum GDP per capita in each continent in 1957
gapminder %>%
filter(year == 1957) %>%
group_by(continent) %>%
summarize(medianLifeExp = median(lifeExp),
maxGdpPercap = max(gdpPercap))

# Find median life expectancy and maximum GDP per capita in each continent/year combination
gapminder %>%
group_by(continent, year) %>%
summarize(medianLifeExp = median(lifeExp),
maxGdpPercap = max(gdpPercap))

# Create a scatter plot showing the change in medianLifeExp over time
by_year <- gapminder %>%
  group_by(year) %>%
  summarize(medianLifeExp = median(lifeExp),
            maxGdpPercap = max(gdpPercap))

ggplot(by_year, aes(x = year, y = medianLifeExp)) +
geom_point() +
expand_limits(y = 0)

# Summarize medianGdpPercap within each continent within each year: by_year_continent
by_year_continent <- gapminder %>%
  group_by(continent, year) %>%
  summarize(medianGdpPercap = median(gdpPercap))

# Plot the change in medianGdpPercap in each continent over time
ggplot(by_year_continent, aes(x = year, y = medianGdpPercap, color = continent)) +
geom_point() +
expand_limits(y = 0)

# Summarize the median GDP and median life expectancy per continent in 2007
by_continent_2007 <- gapminder %>%
  filter(year == 2007) %>%
  group_by(continent) %>%
  summarize(medianGdpPercap = median(gdpPercap),
            medianLifeExp = median(lifeExp))

# Use a scatter plot to compare the median GDP and median life expectancy
ggplot(by_continent_2007, aes(x = medianGdpPercap, y = medianLifeExp, color = continent)) +
  geom_point()

# Summarize the median gdpPercap by year, then save it as by_year
by_year<- gapminder %>%
  group_by(year) %>%
  summarize(medianGdpPercap = median(gdpPercap))

# Create a line plot showing the change in medianGdpPercap over time
ggplot(by_year, aes(x = year, y = medianGdpPercap)) +
geom_line() +
expand_limits(y = 0)

# Summarize the median gdpPercap by year & continent, save as by_year_continent
by_year_continent <- gapminder %>%
  group_by(year, continent) %>%
  summarize(medianGdpPercap = median(gdpPercap))

# Create a line plot showing the change in medianGdpPercap by continent over time
ggplot(by_year_continent, aes(x = year, y = medianGdpPercap, color = continent)) +
geom_line() +
expand_limits(y = 0)

# Summarize the median gdpPercap by continent in 1952
by_continent <- gapminder %>%
  filter(year == 1952) %>%
  group_by(continent) %>%
  summarize(medianGdpPercap = median(gdpPercap))

# Create a bar plot showing medianGdp by continent
ggplot(by_continent, aes(x = continent, y = medianGdpPercap)) +
  geom_col()

# Filter for observations in the Oceania continent in 1952
oceania_1952 <- gapminder %>%
  filter(continent == "Oceania", year == 1952)

# Create a bar plot of gdpPercap by country
ggplot(oceania_1952, aes(x = country, y = gdpPercap)) +
  geom_col()

gapminder_1952 <- gapminder %>%
  filter(year == 1952) %>%
  mutate(pop_by_mil = pop / 1000000)

# Create a histogram of population (pop_by_mil)
ggplot(gapminder_1952, aes(x = pop_by_mil)) + geom_histogram(bins=50)

gapminder_1952 <- gapminder %>%
  filter(year == 1952)

# Create a histogram of population (pop), with x on a log scale
ggplot(gapminder_1952, aes(x = pop)) +
geom_histogram() + scale_x_log10()

gapminder_1952 <- gapminder %>%
  filter(year == 1952)

# Create a boxplot comparing gdpPercap among continents
ggplot(gapminder_1952, aes(x = continent, y = gdpPercap)) +
geom_boxplot() + scale_y_log10()

gapminder_1952 <- gapminder %>%
  filter(year == 1952)

# Add a title to this graph: "Comparing GDP per capita across continents"
ggplot(gapminder_1952, aes(x = continent, y = gdpPercap)) +
  geom_boxplot() +
  scale_y_log10() + 
  ggtitle("Comparing GDP per capita across continents")

# --------------------------------------------------------- Data Manipulation with dplyr 1:4 ------------------------------------------------#

# ---- Selecting verbs ---- #

counties %>%
  # Select the columns
  select(state, county, population, poverty) 

## Select and arrange (in descending order)
counties_selected <- counties %>%
  select(state, county, population, private_work, public_work, self_employed)

counties_selected %>%
  # Add a verb to sort in descending order of public_work
  arrange(desc(public_work))

## A complicated filter
# Filter for Texas and more than 10000 people; sort in descending order of private_work
counties_selected %>%
  # Filter for Texas and more than 10000 people
  filter(state == "Texas",
         population > 10000) %>%
  # Sort in descending order of private_work
arrange(desc(private_work))

## use the semicolon to select multiple columns without writing them out
counties %>%
  # Select state, county, population, and industry-related columns
  select(state, county, population, professional:production) %>%
  # Arrange service in descending order 
  arrange(desc(service))

# ---- Mutating verbs ---- #

## Mutate examples
counties_selected %>%
  # Add a new column public_workers with the number of people employed in public work
  mutate(public_workers = public_work * population / 100)

counties_selected %>%
  # Calculate proportion_women as the fraction of the population made up of women
  mutate(proportion_women = women / population)

# ---- Counting verbs ---- #

## a regular count
counties_selected %>%
  count(region, sort = TRUE)

## a count with a weight correction
# Find number of counties per state, weighted by citizens, sorted in descending order
counties_selected %>%
  count(state, wt = citizens, sort = TRUE)

## find the top/bottom observations in a dataset
counties_selected %>%
  # Group by region
  group_by(region) %>%
  # Find the greatest number of citizens who walk to work
  top_n(1, walk)

## find the top/bottom observations in a dataset for a calculated variables
counties_selected %>%
  group_by(region, state) %>%
  # Calculate average income
  summarize(average_income = mean(income)) %>%
  # Find the highest income state in each region
  top_n(1, average_income)

# ---- Summarizing verbs ---- #

counties_selected %>%
  # Summarize to find minimum population, maximum unemployment, and average income
  summarize(min_population = min(population),
            max_unemployment = max(unemployment),
            average_income = mean(income))

counties_selected %>%
  # Group by state 
  group_by(state) %>%
  # Find the total area and population
  summarize(total_area = sum(land_area),
            total_population = sum(population))

counties_selected %>%
  group_by(state) %>%
  summarize(total_area = sum(land_area),
            total_population = sum(population)) %>%
  # Add a density column
  mutate(density = total_population / total_area) %>%
  # Sort by density in descending order
  arrange(desc(density))

counties_selected %>%
  # Group and summarize to find the total population
  group_by(region, state) %>%
  summarize(total_pop = sum(population)) %>%
  # Calculate the average_pop and median_pop columns 
  summarize(average_pop = mean(total_pop),
            median_pop = median(total_pop))

# ---- Answer one question qith five verbs ---- #
## aggregation: count(), group_by(), summarize(), ungroup(), and top_n()
## In how many states do more people live in metro areas than non-metro areas?

counties_selected %>%
  # Find the total population for each combination of state and metro
  group_by(state, metro) %>%
  summarize(total_pop = sum(population)) %>%
  # Extract the most populated row for each state
  top_n(1, total_pop) %>%
  # Count the states with more people in Metro or Nonmetro areas
  ungroup() %>% 
  count(metro)

#ANS: 
#1 Metro       44
#2 Nonmetro     6

# ---- Rename verbs ---- #

counties %>%
  # Count the number of counties in each state
  count(state) %>%
  # Rename the n column to num_counties
  rename(num_counties = n)

## You can also assign a new name when you use select and the equals sign
counties %>%
  # Select state, county, and poverty as poverty_rate
  select(state, county, poverty_rate = poverty)

# ---- Transmute verbs ---- #

## The transmute verb allows you to control which variables you keep, which variables you calculate, and which variables you drop

counties %>%
  # Keep the state, county, and populations columns, and add a density column
  transmute(state, county, population, density = population / land_area) %>%
  # Filter for counties with a population greater than one million 
  filter(population > 1000000) %>%
  # Sort density in ascending order 
  arrange(density)

counties %>%
  transmute(state, county, employment_rate = employed / population)

# ---- Some more examples using the babynames dataset ---- #

babynames %>%
  # Filter for the year 1990
  filter(year == 1990) %>%
  # Sort the number column in descending order 
    arrange(desc(number))

babynames %>%
  # Find the most common name in each year
  group_by(year) %>%
  top_n(1, number)

selected_names <- babynames %>%
  # Filter for the names Steven, Thomas, and Matthew 
  filter(name %in% c("Steven", "Thomas", "Matthew"))

# Calculate the fraction of people born each year with the same name
babynames %>%
  group_by(year) %>%
  mutate(year_total = sum(number)) %>%
  ungroup() %>%
  mutate(fraction = number / year_total) %>%
  # Find the year each name is most common
  group_by(name) %>%
  top_n(1, fraction)

babynames %>%
  # Add columns name_total and name_max for each name
  group_by(name) %>%
  mutate(name_total = sum(number),
         name_max = max(number)) %>%
  # Ungroup the table 
  ungroup() %>%
  # Add the fraction_max column containing the number by the name maximum 
  mutate(fraction_max = number / name_max)

names_filtered <- names_normalized %>%
  # Filter for the names Steven, Thomas, and Matthew
  filter(name %in% c("Steven", "Thomas", "Matthew"))

# Visualize these names over time
ggplot(names_filtered, aes(x = year, y = fraction_max, color = name)) +
  geom_line()

babynames_fraction %>%
  # Arrange the data in order of name, then year 
  arrange(name, year) %>%
  # Group the data by name
  group_by(name) %>%
  # Add a ratio column that contains the ratio of fraction between each year 
  mutate(ratio = fraction / lag(fraction))

babynames_ratios_filtered <- babynames_fraction %>%
                     arrange(name, year) %>%
                     group_by(name) %>%
                     mutate(ratio = fraction / lag(fraction)) %>%
                     filter(fraction >= 0.00001)

babynames_ratios_filtered %>%
  # Extract the largest ratio from each name 
  top_n(1, ratio) %>%
  # Sort the ratio column in descending order 
  arrange(desc(ratio)) %>%
  # Filter for fractions greater than or equal to 0.001
  filter(fraction >= 0.001)

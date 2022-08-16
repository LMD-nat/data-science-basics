### Create a scatterplot of page_views vs time_on_page. Use size to indicate the number of visits.

ggplot(metrics, aes(time_on_page, page_views, size = visits)) + 
  geom_point()
  
### Create a scatterplot of value against. Add linear regression line without confidence intervals to the plot.

ggplot(valuation, aes(age, value)) + 
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)
  
### Add a horizontal line to the plot to indicate the average cost per mile and to quickly draw attention to cases that are above this value.

ggplot(fleet, aes(x=miles, y=cost)) + 
    geom_point(position = "jitter") + 
    labs(y="Cost per Mile ($)", x="Miles driven per month") +
    geom_hline(yintercept = mean(fleet$cost), linetype = "dashed")

### Add a grid to this plot because there are too many genres. 

ggplot(movies, aes(audience_score, critics_score)) +
  geom_point() +
  grid.arrange(genre)
  facet_wrap(facets = vars(genre))
  
### You are creating a plot of Generosity Against Happiness Score using the World Happiness data from 2019. Add this title to the graphic.

ggplot(happiness, aes(Score, Generosity)) +
  geom_point() +
  labs(title = "Generosity Against Happiness Score")
  
### You are creating a scatter plot to compare culmen length to body mass of male and female Chinstrap penguins.
### Use the more common term "Bill Length (mm)" as a label for the x-axis.

ggplot(chinstrap, aes(x = `Culmen Length (mm)`, y = `Body Mass (g)`, color = Sex)) + 
  geom_point(position = "jitter") +
  labs(x = "Beak Length (mm)", y = "Body Mass (g)")
  
### Create a boxplot of the number of breaks per wool type.

ggplot(warpbreaks, aes(x = wool, y = breaks)) + 
  geom_boxplot()
  
### Create a density plot of the time taken (Time) by the students.

ggplot(test_data, aes(Time)) + geom_density()

### Create a scatterplot of Score as a function of GDP per capita.

### Use size to indicate the relative Generosity of countries.
### The data is contained in the happiness data frame.
### Complete the code to return the output

ggplot(happiness, aes(`GDP per capita`, `Score`, size = `Generosity`)) + 
  geom_point()
  
### You are creating a plot of Generosity Against Happiness Score using the World Happiness data from 2019. Add this title to the graphic.

ggplot(happiness, aes(Score, Generosity)) +
  geom_point() +
  labs(title = "Generosity Against Happiness Score")
  
### Rotate the x-axis labels by by 20 degrees. Rotate the labels on only this axis.

ggplot(tools, aes(Week, Searches, group = Tool)) +
  geom_line()  +
  theme(axis.text.x = element_text(angle = 20))
  
### Create a density plot of the ages (age) of all passengers aboard the titanic.

ggplot(titanic, aes(x = age)) + geom_density()

### Create a visualization that compares the distribution of the delay for each airline.
 
ggplot(arrivals, aes(x = airline, y = delay)) + 
  geom_boxplot()
  
### Create a plot of the price of the Ethereum cryptocurrency as shown in the preview. The data points should be marked on the plot.

ggplot(ethereum, aes(Date, Price)) +
	geom_line()+ 
	geom_point()
  
### Create a horizontal bar chart to show the number of site users who were in each of the control and treatment groups. 

ggplot(experiment, aes(group)) + 
  geom_bar() + 
  coord_flip()

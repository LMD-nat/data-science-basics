### Create a boxplot of the number of breaks per wool type.

library(ggplot2)

ggplot(warpbreaks, aes(y = breaks, x = wool)) + 
  geom_boxplot()
  
### Create a visualization of the distribution of the delay, ensuring that the bins of the histogram cover 7 minute intervals. 

ggplot(arrivals, aes(delay)) + 
  geom_histogram(binwidth = 7)
  
### Create a density plot of the time taken (Time) by the students.

library(ggplot2)

ggplot(test_data, aes(x = Time)) + geom_density()

### On the same set of axes, plot the distribution of ages of male and female passengers aboard the titanic.

library(ggplot2)

ggplot(titanic, aes(age, color=sex)) + geom_density()

### Perform a t-test to compare the two sets of responses.

results <- t.test(satisfaction_2021, 
          y = satisfaction_2020,
          alternative = "greater",
          paired = TRUE)

results$p.value

### Draw a sample of a 1000 numbers from a normal distribution with a mean of 2.5 and standard deviation of 1. 

my_sample <- rnorm(1000, mean=2.5, sd=1)
cat("Sample size: ", length(my_sample), " Sample mean: ", mean(my_sample), " Sample SD: ", sd(my_sample))

### Create a visualization that compares the distribution of the delay for each airline.

ggplot(arrivals, aes(x = airline, y = delay)) + 
  geom_boxplot()
  
### Your Null hypothesis is that there is a statistically significant difference in the mean temperature between the two months. 
### Perform a t-test to investigate this hypothesis.  

t.test(Temp ~ Month, data = summer)

### Customers arrive at a grocery at an average of 2 customers per minute. Simulate the arrival of customers for the next 20 minutes.
# Poisson distribution
# rpois(events, lambda)

arrival <- rpois(20, 2)
arrival

### Sample 25% of the dataset.

library(dplyr)

set.seed(42)
diamonds_sample <- diamonds %>%
   slice_sample(prop = 0.25)

head(diamonds_sample)

### Calculate the required sample size for each group using a two-sided t-test, given the specifications provided.
### Power analysis. Detect a moderate effect size of 0.5, a power of 0.80, and using a significance level of 0.05.

library(pwr)

results <- pwr.t.test(n = NULL,
         			d = 0.50,
         			sig.level = 0.05,
         			power = 0.80,
        		    alternative = 'two.sided')

results$n

### If you approach 10 people in an afternoon, what is the probability that precisely 2 people will donate blood?
### Binomial test. There is a 25% likelihood that you will succeed in convincing them to go donate blood.

Select the code to return the output
dbinom(2, 10, 0.25)

### A dataset has a mean of 170 cm and a standard deviation of 10 cm.
### Given this information, calculate the probability that a participant has a height between 160 cm and 180 cm.

pnorm(180, mean = 170, sd = 10) - 
pnorm(160, mean = 170, sd = 10)

### Based on past experience, you know that on average, you talk to approximately 5 people per day.
### Given this information, what is the probability you only talk to 3 or fewer people on a given day?
### You're calculating a probability from a poisson distribution. 

ppois(3, lambda = 5)

### Based on the test, you observed a small reduction in mean levels of anxiety after the program, with a p-value of 0.07.
### 7% chance to observe an effect at least as large if the program was ineffective. 





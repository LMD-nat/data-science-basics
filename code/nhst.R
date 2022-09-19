#### 1.1 Summary statistics


# Histogram of amount with 10 bins
ggplot(amir_deals, aes(amount)) + geom_histogram(bins = 10)

###### normal distributions

# Probability of deal < 7500 = 
pnorm(7500, mean = 5000, sd = 2000)

# Probability of deal > 1000
pnorm(1000, mean = 5000, sd = 2000, lower.tail =FALSE)

# Probability of deal between 3000 and 7000
pnorm(7000, mean = 5000, sd = 2000) - pnorm(3000, mean = 5000, sd = 2000)

# Calculate amount that 75% of deals will be more than
qnorm(0.75, mean = 5000, sd = 2000, lower.tail = FALSE)



# Calculate new average amount
new_mean <- 5000 + 5000*0.20

# Calculate new standard deviation
new_sd <- 2000 + 2000*0.30

# Simulate 36 sales
new_sales <- new_sales %>% 
  mutate(amount = rnorm(36, mean=new_mean, sd=new_sd))

# Create histogram with 10 bins
ggplot(new_sales, aes(amount)) + geom_histogram(bins = 10)




# Create a histogram of num_users
ggplot(amir_deals, aes(num_users)) + geom_histogram(bins = 10)

# Set seed to 104
set.seed(104)

# Sample 20 num_users with replacement from amir_deals
sample(amir_deals$num_users, 20, replace =TRUE) %>%
  # Take mean
  mean()
  
  # Repeat the above 100 times
sample_means <- replicate(100, sample(amir_deals$num_users, size = 20, replace = TRUE) %>% mean())

# Create data frame for plotting
samples <- data.frame(mean = sample_means)

# Histogram of sample means
ggplot(samples, aes(mean)) + geom_histogram(bins = 10)



# Set seed to 321
set.seed(321)

# Take 30 samples of 20 values of num_users, take mean of each sample
sample_means <- replicate(30, sample(all_deals$num_users, 20) %>% mean())

# Calculate mean of sample_means
mean(sample_means)

# Calculate mean of num_users in amir_deals
mean(amir_deals$num_users)


###### poisson distributions

# Probability of 5 responses
dpois(5, lambda =4)

# Probability of 5 responses from coworker
dpois(5, lambda =5.5)

# Probability of 2 or fewer responses
ppois(2, lambda =4)

# Probability of > 10 responses
ppois(10, lambda =4, lower.tail =FALSE)

###### exponential distributions

# Probability response takes < 1 hour
pexp(1, rate =0.4)

# Probability response takes > 4 hours
pexp(4, rate = 1/2.5, lower.tail = FALSE)

# Probability response takes 3-4 hours
pexp(4, rate = 1/2.5) - pexp(3, rate = 1/2.5)

#### 1.2 Probability


 # probability = event over possible values
 
 # Calculate probability of picking a deal with each product
amir_deals %>%
  count(product) %>%
  mutate(prob = n/sum(n))
  
  DM.result = 3
  
  
# Set random seed to 31
set.seed(31)

# Sample 5 deals without replacement
amir_deals %>%
  sample_n(5)
  
# Sample 5 deals with replacement
amir_deals %>%
  sample_n(5, replace = TRUE)
  


# Create a histogram of group_size
ggplot(restaurant_groups, aes(group_size)) +
  geom_histogram(bins = 5)

# Create probability distribution
size_distribution <- restaurant_groups %>%
  # Count number of each group size
  count(group_size) %>%
  # Calculate probability
  mutate(probability = n / sum(n))

size_distribution

# Create probability distribution
size_distribution <- restaurant_groups %>%
  count(group_size) %>%
  mutate(probability = n / sum(n))

# Calculate expected group size
expected_val <- sum(size_distribution$group_size * size_distribution$probability)
expected_val

# Create probability distribution
size_distribution <- restaurant_groups %>%
  count(group_size) %>%
  mutate(probability = n / sum(n))

# Calculate probability of picking group of 4 or more
size_distribution %>%
  # Filter for groups of 4 or larger
  filter(group_size >= 4) %>%
  # Calculate prob_4_or_more by taking sum of probabilities
  summarize(prob_4_or_more = sum(probability))
  


# Min and max wait times for back-up that happens every 30 min
min <- 0
max <- 30

# Calculate probability of waiting more than 5 mins
prob_greater_than_5 <- 1 - (5/30)
prob_greater_than_5

# Calculate probability of waiting less than 5 mins
prob_less_than_5 <- 5/30
prob_less_than_5

# Calculate probability of waiting more than 5 mins
prob_greater_than_5 <- 1 - (5/30)
prob_greater_than_5

# Calculate probability of waiting 10-20 mins
prob_between_10_and_20 <- punif(20, min, max) - punif(10, min, max)
prob_between_10_and_20



# Set random seed to 334
set.seed(334)

# Generate 1000 wait times between 0 and 30 mins, save in time column
wait_times %>%
  mutate(time = runif(1000, min = 0, max = 30)) %>%
  # Create a histogram of simulated times
  ggplot(aes(time)) +
  geom_histogram(bins =30)
  
  
# Set random seed to 10
set.seed(10)

# Simulate a single deal
rbinom(1,1,0.3)

# Simulate 1 week of 3 deals
rbinom(1,3,0.3)

# Simulate 52 weeks of 3 deals
deals <- rbinom(52,3,0.3)

# Calculate mean deals won per week
mean(deals)



# Probability of closing 3 out of 3 deals
dbinom(3,3,0.3)

# Probability of closing <= 1 deal out of 3 deals
pbinom(1,3,0.3)

# Probability of closing > 1 deal out of 3 deals
pbinom(1,3,0.3, lower.tail =FALSE)



# Expected number won with 30% win rate
won_30pct <- 3 * 0.3
won_30pct

# Expected number won with 25% win rate
won_25pct <- 3 * 0.25
won_25pct

# Expected number won with 35% win rate
won_35pct <- 3 * 0.35


#### 1.3 Central limit theorem

# Probability of deal < 7500
pnorm(7500, mean = 5000, sd = 2000)

# Probability of deal > 1000
pnorm(1000, mean = 5000, sd = 2000, lower.tail =FALSE)

# Probability of deal between 3000 and 7000
pnorm(7000, mean = 5000, sd = 2000) - pnorm(3000, mean = 5000, sd = 2000)

# Calculate amount that 75% of deals will be more than
qnorm(0.75, mean = 5000, sd = 2000, lower.tail = FALSE)



# Calculate new average amount
new_mean <- 5000 + 5000*0.20

# Calculate new standard deviation
new_sd <- 2000 + 2000*0.30

# Simulate 36 sales
new_sales <- new_sales %>% 
  mutate(amount = rnorm(36, mean=new_mean, sd=new_sd))

# Create histogram with 10 bins
ggplot(new_sales, aes(amount)) + geom_histogram(bins = 10)



# Set seed to 104
set.seed(104)

# Sample 20 num_users from amir_deals and take mean
sample(amir_deals$num_users, size = 20, replace = TRUE) %>%
  mean()

# Repeat the above 100 times
sample_means <- replicate(100, sample(amir_deals$num_users, size = 20, replace = TRUE) %>% mean())

# Create data frame for plotting
samples <- data.frame(mean = sample_means)

# Histogram of sample means
ggplot(samples, aes(mean)) + geom_histogram(bins = 10)



# Set seed to 321
set.seed(321)

# Take 30 samples of 20 values of num_users, take mean of each sample
sample_means <- replicate(30, sample(all_deals$num_users, 20) %>% mean())

# Calculate mean of sample_means
mean(sample_means)

# Calculate mean of num_users in amir_deals
mean(amir_deals$num_users)


# Probability of 5 responses
dpois(5, lambda =4)

# Probability of > 10 responses
ppois(10, lambda =4, lower.tail =FALSE)



# Probability response takes < 1 hour
pexp(1, rate =0.4)

# Probability response takes > 4 hours
pexp(4, rate = 1/2.5, lower.tail = FALSE)

# Probability response takes 3-4 hours
pexp(4, rate = 1/2.5) - pexp(3, rate = 1/2.5)


#### 1.4 Correlation

# Create a scatterplot of happiness_score vs. life_exp
ggplot(world_happiness, aes(life_exp, happiness_score)) + geom_point()

# Add a linear trendline to scatterplot
ggplot(world_happiness, aes(life_exp, happiness_score)) +
  geom_point() +
  geom_smooth(method ="lm", se =FALSE)
  
# Add a linear trendline to scatterplot
ggplot(world_happiness, aes(life_exp, happiness_score)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

# Correlation between life_exp and happiness_score
cor(world_happiness$life_exp, world_happiness$happiness_score)



# Scatterplot of gdp_per_cap and life_exp
ggplot(world_happiness, aes(gdp_per_cap, life_exp))+  geom_point()+  geom_smooth(method ="lm", se =FALSE)

# Scatterplot of gdp_per_cap and life_exp
ggplot(world_happiness, aes(gdp_per_cap, life_exp)) +
  geom_point()

# Correlation between gdp_per_cap and life_exp
cor(world_happiness$gdp_per_cap, world_happiness$life_exp)



# Scatterplot of happiness_score vs. gdp_per_cap
ggplot(world_happiness, aes(gdp_per_cap, happiness_score))+  geom_point()+  geom_smooth(method ="lm", se =FALSE)

# Calculate correlation
cor(world_happiness$happiness_score, world_happiness$gdp_per_cap)

# Create log_gdp_per_cap column
world_happiness <- world_happiness %>%
  mutate(log_gdp_per_cap = log(gdp_per_cap))

# Scatterplot of happiness_score vs. log_gdp_per_cap
ggplot(world_happiness, aes(log_gdp_per_cap, happiness_score)) +
  geom_point()

# Calculate correlation
cor(world_happiness$log_gdp_per_cap, world_happiness$happiness_score)



# Scatterplot of grams_sugar_per_day and happiness_score
ggplot(world_happiness, aes(grams_sugar_per_day, happiness_score)) +
  geom_point()

# Correlation between grams_sugar_per_day and happiness_score
 cor(world_happiness$grams_sugar_per_day, world_happiness$happiness_score) 

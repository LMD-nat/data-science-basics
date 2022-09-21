#### 1.1 Simple Linear Regression
# Draw a scatter plot of n_convenience vs. price_twd_msq
ggplot(taiwan_real_estate,  aes(n_convenience, price_twd_msq))+  geom_point()


# Make points 50% transparent
ggplot(taiwan_real_estate, aes(n_convenience, price_twd_msq)) +
  geom_point(alpha = 0.5)
  
  
# Add a linear trend line without a confidence ribbon
ggplot(taiwan_real_estate, aes(n_convenience, price_twd_msq)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method='lm')
  

# Run a linear regression of price_twd_msq vs. n_convenience
lm(formula = price_twd_msq ~ n_convenience, data = taiwan_real_estate)


# Using taiwan_real_estate, plot price_twd_msq
ggplot(taiwan_real_estate, aes(price_twd_msq)) +
  # Make it a histogram with 10 bins
  geom_histogram(bins =10) +
  # Facet the plot so each house age group gets its own panel
  facet_wrap(vars(house_age_years))
  
  
summary_stats <- taiwan_real_estate %>% 
  # Group by house age
  group_by(house_age_years) %>% 
  # Summarize to calculate the mean house price/area
  summarize(mean_by_group = mean(price_twd_msq))

# See the result
summary_stats


# Run a linear regression of price_twd_msq vs. house_age_years
mdl_price_vs_age <- lm(price_twd_msq~house_age_years, data=taiwan_real_estate)

# See the result
mdl_price_vs_age

# Update the model formula to remove the intercept
mdl_price_vs_age_no_intercept <- lm(
  price_twd_msq ~ house_age_years + 0, 
  data = taiwan_real_estate
)

# See the result
mdl_price_vs_age_no_intercept

#### 1.2 Predictions and model objects

# Create a tibble with n_convenience column from zero to ten
explanatory_data <- tibble(
  n_convenience = 0:10
)

# Edit this, so predictions are stored in prediction_data
prediction_data <- explanatory_data %>% 
  mutate(
    price_twd_msq = predict(mdl_price_vs_conv, explanatory_data)
  )

# See the result
prediction_data



# Add to the plot
ggplot(taiwan_real_estate, aes(n_convenience, price_twd_msq)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  # Add a point layer of prediction data, colored yellow
geom_point(data = prediction_data, color = 'yellow')
  
  
  
# Define a tibble where n_convenience is -1
minus_one <- tibble(n_convenience = -1)

# Define a tibble where n_convenience is 2.5
two_pt_five <- tibble(n_convenience = 2.5)


# Get the model coefficients of mdl_price_vs_conv
coef(mdl_price_vs_conv)
# Get the fitted values of mdl_price_vs_conv
fitted(mdl_price_vs_conv)
# Get the residuals of mdl_price_vs_conv
residuals(mdl_price_vs_conv)
# Print a summary of mdl_price_vs_conv
summary(mdl_price_vs_conv)



# Get the coefficients of mdl_price_vs_conv
coeffs <- coef(mdl_price_vs_conv)

# Get the intercept
intercept <- coeffs[1]

# Get the slope
slope <- coeffs[2]

explanatory_data %>% 
  mutate(
    # Manually calculate the predictions
    price_twd_msq = intercept + slope * n_convenience
  )

# Compare to the results from predict()
predict(mdl_price_vs_conv, explanatory_data)



# Get the coefficient-level elements of the model
# lmfit <- lm(mpg ~ wt + qsec, mtcars) 
tidy(mdl_price_vs_conv)
# Get the observation-level elements of the model
augment(mdl_price_vs_conv)
# Get the model-level elements of the model
glance(mdl_price_vs_conv) 



# Plotting consecutive portfolio returns
# Using sp500_yearly_returns, plot return_2019 vs. return_2018
ggplot(sp500_yearly_returns, aes(return_2018, return_2019)) +
  # Make it a scatter plot
  geom_point() +
  # Add a line at y = x, colored green, size 1
  geom_abline(
  color = 'green',
  size = 1) +
  # Add a linear regression trend line, no std. error ribbon
  geom_smooth(method = 'lm', se = F) +
  # Fix the coordinate ratio
  coord_fixed(ratio = 1, xlim = NULL, ylim = NULL, expand = TRUE, clip = "on")
  
  
  
# Run a linear regression on return_2019 vs. return_2018
# using sp500_yearly_returns
mdl_returns <- lm(formula = return_2019 ~ return_2018, data = sp500_yearly_returns)

# See the result
mdl_returns

# Run a linear regression on return_2019 vs. return_2018 using sp500_yearly_returns
mdl_returns <- lm(
  return_2019 ~ return_2018, 
  data = sp500_yearly_returns
)

# Create a data frame with return_2018 at -1, 0, and 1 
explanatory_data <- tibble(
  return_2018 = c(-1, 0, 1)
)

# Use mdl_returns to predict with explanatory_data
predict(mdl_returns, explanatory_data)



# Run the code to see the plot
# Edit so x-axis is square root of dist_to_mrt_m
ggplot(taiwan_real_estate, aes(sqrt(dist_to_mrt_m), price_twd_msq)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)
  
# Run a linear regression of price_twd_msq vs. 
# square root of dist_to_mrt_m using taiwan_real_estate
mdl_price_vs_dist <- lm(formula = price_twd_msq ~ sqrt(dist_to_mrt_m), data = taiwan_real_estate)

# See the result
mdl_price_vs_dist  

# From previous step
mdl_price_vs_dist <- lm(
  price_twd_msq ~ sqrt(dist_to_mrt_m), 
  data = taiwan_real_estate
)

# Use this explanatory data
explanatory_data <- tibble(
  dist_to_mrt_m = seq(0, 80, 10) ^ 2
)

# Use mdl_price_vs_dist to predict explanatory_data
prediction_data <- explanatory_data %>% 
mutate(price_twd_msq = predict(mdl_price_vs_dist, explanatory_data))

# See the result
prediction_data

# From previous steps
mdl_price_vs_dist <- lm(
  price_twd_msq ~ sqrt(dist_to_mrt_m), 
  data = taiwan_real_estate
)
explanatory_data <- tibble(
  dist_to_mrt_m = seq(0, 80, 10) ^ 2
)
prediction_data <- explanatory_data %>% 
  mutate(
    price_twd_msq = predict(mdl_price_vs_dist, explanatory_data)
  )

ggplot(taiwan_real_estate, aes(sqrt(dist_to_mrt_m), price_twd_msq)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  # Add points from prediction_data, colored green, size 5
  geom_point(data = prediction_data, color = 'green', size = 5)
  
  
 
 
# Run the code to see the plot
# Edit to raise x, y aesthetics to power 0.25
ggplot(ad_conversion, aes(n_impressions ^ 0.25, n_clicks ^ 0.25)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)
  
# Run a linear regression of n_clicks to the power 0.25 vs. n_impressions to the power 0.25 using ad_conversion
mdl_click_vs_impression <- lm(
  I(n_clicks ^ 0.25) ~ I(n_impressions ^ 0.25),
  data = ad_conversion
)

# Run a linear regression of n_clicks to the power 0.25 vs. n_impressions to the power 0.25 using ad_conversion
mdl_click_vs_impression <- lm(
  I(n_clicks ^ 0.25) ~ I(n_impressions ^ 0.25),
  data = ad_conversion
)

explanatory_data <- tibble(
  n_impressions = seq(0, 3e6, 5e5)
)

prediction_data <- explanatory_data %>% 
  mutate(
    # Use mdl_click_vs_impression to predict n_clicks ^ 0.25
    n_clicks_025 = predict(mdl_click_vs_impression, explanatory_data),
    # Back transform to get n_clicks
    n_clicks = n_clicks_025 ^ 4
  )
  
# From previous steps
mdl_click_vs_impression <- lm(
  I(n_clicks ^ 0.25) ~ I(n_impressions ^ 0.25),
  data = ad_conversion
)
explanatory_data <- tibble(
  n_impressions = seq(0, 3e6, 5e5)
)
prediction_data <- explanatory_data %>% 
  mutate(
    n_clicks_025 = predict(mdl_click_vs_impression, explanatory_data),
    n_clicks = n_clicks_025 ^ 4
  )

ggplot(ad_conversion, aes(n_impressions ^ 0.25, n_clicks ^ 0.25)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  # Add points from prediction_data, colored green
  geom_point(data = prediction_data, color = 'green')
   
#### 1.3 Assessing model fit

# Print a summary of mdl_click_vs_impression_orig
summary(mdl_click_vs_impression_orig)

# Print a summary of mdl_click_vs_impression_trans
summary(mdl_click_vs_impression_trans)

# Get coeff of determination for mdl_click_vs_impression_orig
mdl_click_vs_impression_orig %>% 
  # Get the model-level details
  glance() %>% 
  # Pull out r.squared
  pull(r.squared)

# Do the same for the transformed model
mdl_click_vs_impression_trans %>% 
  # Get the model-level details
  glance() %>% 
  # Pull out r.squared
  pull(r.squared)
  
 
 
 ### Residual standard error (RSE) is a measure of the typical size of the residuals. 
 ### Equivalently, it's a measure of how badly wrong you can expect predictions to be. 
 ### Smaller numbers are better, with zero being a perfect fit to the data.

# Get RSE for mdl_click_vs_impression_orig
mdl_click_vs_impression_orig %>% 
  # Get the model-level details
  glance() %>% 
  # Pull out sigma
  pull(sigma)

# Do the same for the transformed model
mdl_click_vs_impression_trans %>% 
  # Get the model-level details
  glance() %>% 
  # Pull out sigma
  pull(sigma)
  
 
 
# Plot the three diagnostics for mdl_price_vs_conv
autoplot(
mdl_price_vs_conv,
which = 1:3,
nrow = 3,
ncol = 1
)



mdl_price_vs_dist %>% 
  # Augment the model
  augment() %>%
  # Arrange rows by descending leverage
  arrange(desc(leverage = .hat)) %>%
  # Get the head of the dataset
  head()
  
mdl_price_vs_dist %>% 
  # Augment the model
  augment() %>%
  # Arrange rows by descending Cook's distance
  arrange(desc(cooks_dist =.cooksd)) %>%
  # Get the head of the dataset
  head()
  
# Plot the three outlier diagnostics for mdl_price_vs_conv
autoplot(
mdl_price_vs_dist,
which = 4:6,
nrow = 3,
ncol = 1
)

#### 1.4 Simple logistic regression

# Using churn, plot time_since_last_purchase
ggplot(churn, aes(x=time_since_last_purchase)) + 
  # as a histogram with binwidth 0.25
  geom_histogram(binwidth = 0.25) +
  # faceted in a grid with has_churned on each row
  facet_grid(rows = vars(has_churned))
  
  
# Redraw the plot with time_since_first_purchase
ggplot(churn, aes(x=time_since_first_purchase)) + 
  # as a histogram with binwidth 0.25
  geom_histogram(binwidth = 0.25) +
  # faceted in a grid with has_churned on each row
  facet_grid(rows = vars(has_churned))
  


# Using churn plot has_churned vs. time_since_first_purchase
ggplot(churn, aes(time_since_first_purchase, has_churned)) +
  # Make it a scatter plot
  geom_point() +
  # Add an lm trend line, no std error ribbon, colored red
  geom_smooth(method = "lm", se = F, color = 'red')
  
ggplot(churn, aes(time_since_first_purchase, has_churned)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  # Add a glm trend line, no std error ribbon, binomial family
  geom_smooth(method = "glm", 
    method.args = list(family = "binomial"), 
    se = FALSE) 
    
       
# Fit a logistic regression of churn vs. 
# length of relationship using the churn dataset
mdl_churn_vs_relationship <- glm(has_churned ~ time_since_first_purchase, data = churn, family = binomial)

# See the result
mdl_churn_vs_relationship 



# Make a data frame of predicted probabilities
prediction_data <- explanatory_data %>% 
  mutate(   
    has_churned = predict(
      mdl_churn_vs_relationship, 
      explanatory_data, 
      type = "response"
    )
  )

# See the result
prediction_data

# Make a data frame of predicted probabilities
prediction_data <- explanatory_data %>% 
  mutate(   
    has_churned = predict(mdl_churn_vs_relationship, explanatory_data, type = "response")
  )

# Update the plot
plt_churn_vs_relationship +
  # Add points from prediction_data, colored yellow, size 2
  geom_point(data = prediction_data, color = "yellow", size = 2)
  
  
  
# Update the data frame
prediction_data <- explanatory_data %>% 
  mutate(   
    has_churned = predict(mdl_churn_vs_relationship, explanatory_data, type = "response"),
    # Add the most likely churn outcome
    most_likely_outcome = round(has_churned)
  )

# See the result
prediction_data



# Update the data frame
prediction_data <- explanatory_data %>% 
  mutate(   
    has_churned = predict(mdl_churn_vs_relationship, explanatory_data, type = "response"),
    most_likely_outcome = round(has_churned)
  )

# Update the plot
plt_churn_vs_relationship +
  # Add most likely outcome points from prediction_data, colored yellow, size 2
  geom_point(
    aes(y = most_likely_outcome), 
    data = prediction_data, 
    color = "yellow",
    size = 2
  )
  
  
  # Update the data frame
prediction_data <- explanatory_data %>% 
  mutate(   
    has_churned = predict(
      mdl_churn_vs_relationship, explanatory_data, 
      type = "response"
    ),
    # Add the odds ratio
    odds_ratio = has_churned / (1 - has_churned)
  )

# See the result
prediction_data



# From previous step
prediction_data <- explanatory_data %>% 
  mutate(   
    has_churned = predict(mdl_churn_vs_relationship, explanatory_data, type = "response"),
    odds_ratio = has_churned / (1 - has_churned)
  )

# Using prediction_data, plot odds_ratio vs. time_since_first_purchase
ggplot(prediction_data,aes(time_since_first_purchase, odds_ratio)) +
  # Make it a line plot
  geom_line() +
  # Add a dotted horizontal line at y = 1
  geom_hline(yintercept = 1, linetype = "dotted")
  
  
  
# Update the data frame
prediction_data <- explanatory_data %>% 
  mutate(   
    has_churned = predict(mdl_churn_vs_relationship, explanatory_data, type = "response"),
    odds_ratio = has_churned / (1 - has_churned),
    # Add the log odds ratio from odds_ratio
    log_odds_ratio = log(odds_ratio),
    # Add the log odds ratio using predict()
    log_odds_ratio2 = predict(mdl_churn_vs_relationship, explanatory_data))

# See the result
prediction_data


# From previous step
prediction_data <- explanatory_data %>% 
  mutate(   
    has_churned = predict(mdl_churn_vs_relationship, explanatory_data, type = "response"),
    odds_ratio = has_churned / (1 - has_churned),
    log_odds_ratio = log(odds_ratio)
  )

# Update the plot
ggplot(prediction_data, aes(time_since_first_purchase, odds_ratio)) +
  geom_line() +
  geom_hline(yintercept = 1, linetype = "dotted") +
  # Use a logarithmic y-scale
  scale_y_log10()
  
  

# Get the actual responses from the dataset
actual_response <- churn$has_churned

# Get the "most likely" responses from the model
predicted_response <- round(fitted(mdl_churn_vs_relationship))

# Create a table of counts
outcomes <- table(predicted_response, actual_response)

# See the result
outcomes


# Convert outcomes to a yardstick confusion matrix
confusion <- conf_mat(outcomes)

# Plot the confusion matrix
autoplot(confusion)

# Get performance metrics for the confusion matrix
summary(confusion, event_level = "second")


#### 2.1 Parallel Slopes

# Fit a linear regr'n of price_twd_msq vs. n_convenience
mdl_price_vs_conv <- lm(price_twd_msq ~ n_convenience, data = taiwan_real_estate)

# See the result
mdl_price_vs_conv


# Fit a linear regr'n of price_twd_msq vs. house_age_years, no intercept
mdl_price_vs_age <- lm(price_twd_msq ~ house_age_years + 0, data = taiwan_real_estate)

# See the result
mdl_price_vs_age


# Fit a linear regr'n of price_twd_msq vs. n_convenience plus house_age_years, no intercept
mdl_price_vs_both <- lm(price_twd_msq ~ n_convenience + house_age_years + 0, data = taiwan_real_estate)

# See the result
mdl_price_vs_both



# Using taiwan_real_estate, plot price_twd_msq vs. n_convenience
ggplot(taiwan_real_estate, aes(y = price_twd_msq, x = n_convenience)) +
  # Add a point layer
  geom_point() +
  # Add a smooth trend line using linear regr'n, no ribbon
  geom_smooth(method ="lm", se =FALSE)


# Using taiwan_real_estate, plot price_twd_msq vs. house_age_years
ggplot(taiwan_real_estate, aes(house_age_years, price_twd_msq)) +
  # Add a box plot layer
  geom_boxplot()



# Using taiwan_real_estate, plot price_twd_msq vs. n_convenience colored by house_age_years
ggplot(taiwan_real_estate, aes(n_convenience, price_twd_msq, color = house_age_years)) +
  # Add a point layer
  geom_point() +
  # Add parallel slopes, no ribbon
  geom_parallel_slopes(se =FALSE)



# Make a grid of explanatory data
explanatory_data <- expand_grid(
  # Set n_convenience to zero to ten
  n_convenience = seq(0,10),
  # Set house_age_years to the unique values of that variable
  house_age_years = unique(taiwan_real_estate$house_age_years)
)

# See the result
explanatory_data



# From previous step
explanatory_data <- expand_grid(
  n_convenience = 0:10,
  house_age_years = unique(taiwan_real_estate$house_age_years)
)

# Add predictions to the data frame
prediction_data <- explanatory_data %>% 
  mutate(p = predict(mdl_price_vs_both, explanatory_data
  ))

# See the result
prediction_data



# From previous steps
explanatory_data <- expand_grid(
  n_convenience = 0:10,
  house_age_years = unique(taiwan_real_estate$house_age_years)
)
prediction_data <- explanatory_data %>% 
  mutate(
    price_twd_msq = predict(mdl_price_vs_both, explanatory_data)
  )

taiwan_real_estate %>% 
  ggplot(aes(n_convenience, price_twd_msq, color = house_age_years)) +
  geom_point() +
  geom_parallel_slopes(se = FALSE) +
  # Add points using prediction_data, with size 5 and shape 15
  geom_point(data = prediction_data, size =5, shape =15)




# Get the coefficients from mdl_price_vs_both
coeffs <- coefficients(mdl_price_vs_both)

# Extract the slope coefficient
slope <- coeffs[1]

# Extract the intercept coefficient for 0 to 15
intercept_0_15 <- coeffs[2]

# Extract the intercept coefficient for 15 to 30
intercept_15_30 <- coeffs[3]

# Extract the intercept coefficient for 30 to 45
intercept_30_45 <- coeffs[4]


# From previous step
coeffs <- coefficients(mdl_price_vs_both)
slope <- coeffs[1]
intercept_0_15 <- coeffs[2]
intercept_15_30 <- coeffs[3]
intercept_30_45 <- coeffs[4]

prediction_data <- explanatory_data %>% 
  mutate(
    # Consider the 3 cases to choose the intercept
    intercept = case_when(
      house_age_years == "0 to 15" ~ intercept_0_15,
      house_age_years == "15 to 30" ~ intercept_15_30,
      house_age_years == "30 to 45" ~ intercept_30_45 
    ),
    # Manually calculate the predictions
    price_twd_msq = intercept + slope * n_convenience
  )

# See the results
prediction_data




mdl_price_vs_conv %>% 
  # Get the model-level coefficients
  glance() %>% 
  # Select the coeffs of determination
  select(r.squared, adj.r.squared)

# Get the coeffs of determination for mdl_price_vs_age
mdl_price_vs_age %>% 
  glance() %>% 
  select(r.squared, adj.r.squared)

# Get the coeffs of determination for mdl_price_vs_both
mdl_price_vs_both %>% 
  glance() %>% 
  select(r.squared, adj.r.squared)



mdl_price_vs_conv %>% 
  # Get the model-level coefficients
  glance() %>% 
  # Pull out the RSE
  pull(sigma)

# Get the RSE for mdl_price_vs_age
mdl_price_vs_age %>%
  glance() %>%  
  pull(sigma)


# Get the RSE for mdl_price_vs_both
mdl_price_vs_both %>% 
  glance() %>% 
  pull(sigma)

#### 2.2 Interactions

# Filter for rows where house age is 0 to 15 years
taiwan_0_to_15 <- taiwan_real_estate %>%  filter(house_age_years =="0 to 15")

# Filter for rows where house age is 15 to 30 years
taiwan_15_to_30 <- taiwan_real_estate %>%  filter(house_age_years =="15 to 30")

# Filter for rows where house age is 30 to 45 years
taiwan_30_to_45 <- taiwan_real_estate %>%  filter(house_age_years =="30 to 45")



# From previous step
taiwan_0_to_15 <- taiwan_real_estate %>%
  filter(house_age_years == "0 to 15")
taiwan_15_to_30 <- taiwan_real_estate %>%
  filter(house_age_years == "15 to 30")
taiwan_30_to_45 <- taiwan_real_estate %>%
  filter(house_age_years == "30 to 45")

# Model price vs. no. convenience stores using 0 to 15 data
mdl_0_to_15 <- lm(price_twd_msq ~ n_convenience, data = taiwan_0_to_15)

# Model price vs. no. convenience stores using 15 to 30 data
mdl_15_to_30 <- lm(price_twd_msq ~ n_convenience, data = taiwan_15_to_30)

# Model price vs. no. convenience stores using 30 to 45 data
mdl_30_to_45 <- lm(price_twd_msq ~ n_convenience, data = taiwan_30_to_45)

# See the results
mdl_0_to_15
mdl_15_to_30
mdl_30_to_45



# Create a tibble of explanatory data, setting no. of conv stores to 0 to 10
explanatory_data <- tibble(
  n_convenience = 0:10
)

# From previous step
explanatory_data <- tibble(
  n_convenience = 0:10
)

# Add column of predictions using "0 to 15" model and explanatory data 
prediction_data_0_to_15 <- explanatory_data %>% 
  mutate(price_twd_msq = predict(mdl_0_to_15, explanatory_data))

# Same again, with "15 to 30"
prediction_data_15_to_30 <- explanatory_data %>% 
  mutate(price_twd_msq = predict(mdl_15_to_30, explanatory_data))

# Same again, with "30 to 45"
prediction_data_30_to_45 <- explanatory_data %>% 
  mutate(price_twd_msq = predict(mdl_30_to_45, explanatory_data))



# Using taiwan_real_estate, plot price vs. no. of conv. stores colored by house age
ggplot(taiwan_real_estate, aes(n_convenience, price_twd_msq, color = house_age_years)) +
  # Make it a scatter plot
  geom_point() +
  # Add smooth linear regression trend lines, no ribbon
  geom_smooth(method ="lm", se =FALSE)

# Get the coeff. of determination for mdl_all_ages
mdl_all_ages %>%  glance()%>%  pull(r.squared)

# Get the coeff. of determination for mdl_0_to_15
mdl_0_to_15 %>%  glance()%>%  pull(r.squared)

# Get the coeff. of determination for mdl_15_to_30
mdl_15_to_30 %>%  glance()%>%  pull(r.squared)

# Get the coeff. of determination for mdl_30_to_45
mdl_30_to_45 %>%  glance()%>%  pull(r.squared)



# Get the coeff. of determination for mdl_all_ages
mdl_all_ages %>%  glance()%>%  pull(r.squared)

# Get the coeff. of determination for mdl_0_to_15
mdl_0_to_15 %>%  glance()%>%  pull(r.squared)

# Get the coeff. of determination for mdl_15_to_30
mdl_15_to_30 %>%  glance()%>%  pull(r.squared)

# Get the coeff. of determination for mdl_30_to_45
mdl_30_to_45 %>%  glance()%>%  pull(r.squared)

# Get the RSE for mdl_all
mdl_all_ages %>%  glance()%>%  pull(sigma)

# Get the RSE for mdl_0_to_15
mdl_0_to_15 %>%  glance()%>%  pull(sigma)

# Get the RSE for mdl_15_to_30
mdl_15_to_30 %>%  glance()%>%  pull(sigma)

# Get the RSE for mdl_30_to_45
mdl_30_to_45 %>%  glance()%>%  pull(sigma)



# Model price vs both with an interaction using "times" syntax
lm(price_twd_msq ~ n_convenience*house_age_years, data = taiwan_real_estate)

# Model price vs both with an interaction using "colon" syntax
lm(price_twd_msq ~ n_convenience + house_age_years + n_convenience:house_age_years, data = taiwan_real_estate)



# Model price vs. house age plus an interaction, no intercept
mdl_readable_inter <- lm(price_twd_msq ~ house_age_years + n_convenience:house_age_years + 0, data = taiwan_real_estate)

# See the result
mdl_readable_inter
coeffs <- coefficients(mdl_readable_inter)
coeffs
# Get coefficients for mdl_0_to_15
slope_0_to_15 <- coeffs[1]

# Get coefficients for mdl_15_to_30
slope_15_to_30 <- coeffs[2]

# Get coefficients for mdl_30_to_45
slope_30_to_45 <- coeffs[3]



# Make a grid of explanatory data
explanatory_data <- expand_grid(
  # Set n_convenience to zero to ten
  n_convenience = 0:10,
  # Set house_age_years to the unique values of that variable
  house_age_years = unique(taiwan_real_estate$house_age_years)
)

# See the result
explanatory_data


# From previous step
explanatory_data <- expand_grid(
  n_convenience = 0:10,
  house_age_years = unique(taiwan_real_estate$house_age_years)
)

# Add predictions to the data frame
prediction_data <- explanatory_data %>%  mutate(predict_price = predict(mdl_price_vs_both_inter, explanatory_data))

# See the result
prediction_data


# From previous step
explanatory_data <- expand_grid(
  n_convenience = 0:10,
  house_age_years = unique(taiwan_real_estate$house_age_years)
)
prediction_data <- explanatory_data %>% 
  mutate(
    price_twd_msq = predict(mdl_price_vs_both_inter, explanatory_data)
  )

# Using taiwan_real_estate, plot price vs. no. of convenience stores, colored by house age
ggplot(taiwan_real_estate, aes(n_convenience, price_twd_msq, color = house_age_years))+
  # Make it a scatter plot
  geom_point() +
  # Add linear regression trend lines, no ribbon
  geom_smooth(method ="lm", se =FALSE) +
  # Add points from prediction_data, size 5, shape 15
  geom_point(data = prediction_data, size =5, shape =15)




# Get the coefficients from mdl_price_vs_both_inter
coeffs <- coefficients(mdl_price_vs_both_inter)
print(coeffs)

# Get the intercept for 0 to 15 year age group
intercept_0_15 <- coeffs[1]

# Get the intercept for 15 to 30 year age group
intercept_15_30 <- coeffs[2]

# Get the intercept for 30 to 45 year age group
intercept_30_45 <- coeffs[3]

# Get the slope for 0 to 15 year age group
slope_0_15 <- coeffs[4]

# Get the slope for 15 to 30 year age group
slope_15_30 <- coeffs[5]

# Get the slope for 30 to 45 year age group
slope_30_45 <- coeffs[6]

# From previous step
coeffs <- coefficients(mdl_price_vs_both_inter)
intercept_0_15 <- coeffs[1]
intercept_15_30 <- coeffs[2]
intercept_30_45 <- coeffs[3]
slope_0_15 <- coeffs[4]
slope_15_30 <- coeffs[5]
slope_30_45 <- coeffs[6]

prediction_data <- explanatory_data %>% 
  mutate(
    # Consider the 3 cases to choose the price
    price_twd_msq = case_when(
      house_age_years == "0 to 15" ~ intercept_0_15 + slope_0_15 * n_convenience,
      house_age_years == "15 to 30" ~ intercept_15_30 + slope_15_30 * n_convenience,
      house_age_years == "30 to 45" ~ intercept_30_45 + slope_30_45 * n_convenience 
    ) 
  )

# See the result
prediction_data

# Take a glimpse at the dataset
glimpse(auctions)

# Model price vs. opening bid using auctions
mdl_price_vs_openbid <- lm( price ~ openbid,  data = auctions)

# See the result
mdl_price_vs_openbid

# Using auctions, plot price vs. opening bid as a scatter plot with linear regression trend lines
ggplot(auctions, aes(openbid, price))+  geom_point()+  geom_smooth(method ="lm", se =FALSE)

# Fit linear regression of price vs. opening bid and auction type, with an interaction.
mdl_price_vs_both <- lm(price ~ openbid*auction_type,  data = auctions)
# See the result
mdl_price_vs_both

# Using auctions, plot price vs. opening bid colored by auction type as a scatter plot with linear regr'n trend lines
ggplot(auctions, aes(openbid, price, color = auction_type))+  geom_point()+  geom_smooth(method ="lm", se =FALSE)

#### 2.3 Multiple Linear Regression

# With taiwan_real_estate, draw a 3D scatter plot of no. of conv. stores, sqrt dist to MRT, and price
scatter3D(taiwan_real_estate$n_convenience, sqrt(taiwan_real_estate$dist_to_mrt_m), taiwan_real_estate$price_twd_msq)
# Using taiwan_real_estate, plot sqrt dist to MRT vs. no. of conv stores, colored by price
  # Make it a scatter plot
  # Use the continuous viridis plasma color scale

ggplot(taiwan_real_estate, aes(n_convenience, sqrt(dist_to_mrt_m), color = price_twd_msq))+  geom_point()+  scale_color_viridis_c(option ="plasma")

#+  geom_point(data = prediction_data, shape =15, size =3)



# Fit a linear regression of price vs. no. of conv. stores and sqrt dist. to nearest MRT, no interaction
mdl_price_vs_conv_dist <- lm(formula = price_twd_msq ~ n_convenience + sqrt(dist_to_mrt_m), data = taiwan_real_estate)



# See the result
mdl_price_vs_conv_dist



# From previous step 
mdl_price_vs_conv_dist <- lm(price_twd_msq ~ n_convenience + sqrt(dist_to_mrt_m), data = taiwan_real_estate)

# Create expanded grid of explanatory variables with no. of conv. stores and  dist. to nearest MRT
explanatory_data <- expand_grid(
  n_convenience = 0:10,
  dist_to_mrt_m = seq(0, 80, 10) ^ 2
)

# Add predictions using mdl_price_vs_conv_dist and explanatory_data
prediction_data <- explanatory_data %>% 
  mutate(
    price_twd_msq = predict(mdl_price_vs_conv_dist, explanatory_data)
  )

# See the result
prediction_data



# From previous steps
mdl_price_vs_conv_dist <- lm(price_twd_msq ~ n_convenience + sqrt(dist_to_mrt_m), data = taiwan_real_estate)
explanatory_data <- expand_grid(n_convenience = 0:10, dist_to_mrt_m = seq(0, 80, 10) ^ 2)
prediction_data <- explanatory_data %>% 
  mutate(price_twd_msq = predict(mdl_price_vs_conv_dist, explanatory_data))

# Add predictions to plot
ggplot(
  taiwan_real_estate, 
  aes(n_convenience, sqrt(dist_to_mrt_m), color = price_twd_msq)
) + 
  geom_point() +
  scale_color_viridis_c(option = "plasma")+
  # Add prediction points colored yellow, size 3
  geom_point( data = prediction_data, color='yellow', size =3)



# Fit a linear regression of price vs. no. of conv. stores and sqrt dist. to nearest MRT, with interaction
mdl_price_vs_conv_dist <- lm(formula = price_twd_msq ~ n_convenience * sqrt(dist_to_mrt_m), data = taiwan_real_estate)

# See the result
mdl_price_vs_conv_dist

# From previous step 
mdl_price_vs_conv_dist <- lm(price_twd_msq ~ n_convenience * sqrt(dist_to_mrt_m), data = taiwan_real_estate)

# Create expanded grid of explanatory variables with no. of conv. stores and  dist. to nearest MRT
explanatory_data <- expand_grid(
  n_convenience = 0:10,
  dist_to_mrt_m = seq(0, 80, 10) ^ 2
)

# Add predictions using mdl_price_vs_conv_dist and explanatory_data
prediction_data <- explanatory_data %>% 
  mutate(
    price_twd_msq = predict(mdl_price_vs_conv_dist, explanatory_data)
  )

# See the result
prediction_data

# From previous steps
mdl_price_vs_conv_dist <- lm(price_twd_msq ~ n_convenience * sqrt(dist_to_mrt_m), data = taiwan_real_estate)
explanatory_data <- expand_grid(n_convenience = 0:10, dist_to_mrt_m = seq(0, 80, 10) ^ 2)
prediction_data <- explanatory_data %>% 
  mutate(price_twd_msq = predict(mdl_price_vs_conv_dist, explanatory_data))

# Add predictions to plot
ggplot(
  taiwan_real_estate, 
  aes(n_convenience, sqrt(dist_to_mrt_m), color = price_twd_msq)
) + 
  geom_point() +
  scale_color_viridis_c(option = "plasma") +
  # Add prediction points colored yellow, size 3
  geom_point(data = prediction_data, color = 'yellow', size =3)



# Using taiwan_real_estate, no. of conv. stores vs. sqrt of dist. to MRT, colored by plot house price
ggplot(taiwan_real_estate,  aes(sqrt(dist_to_mrt_m),n_convenience, color = price_twd_msq)) +
  # Make it a scatter plot
  geom_point() +
  # Use the continuous viridis plasma color scale
  scale_color_viridis_c(option ="plasma") +
  # Facet, wrapped by house age
  facet_wrap(vars(house_age_years))



# Model price vs. no. of conv. stores, sqrt dist. to MRT station & house age, no global intercept, no interactions
mdl_price_vs_all_no_inter <- lm(price_twd_msq ~ sqrt(dist_to_mrt_m) + n_convenience + house_age_years + 0, data = taiwan_real_estate)

# See the result
mdl_price_vs_all_no_inter

# Model price vs. sqrt dist. to MRT station, no. of conv. stores & house age, no global intercept, 3-way interactions
mdl_price_vs_all_3_way_inter <- lm(  
    price_twd_msq ~ sqrt(dist_to_mrt_m) + n_convenience + 
    house_age_years + sqrt(dist_to_mrt_m):n_convenience +  sqrt(dist_to_mrt_m):house_age_years + n_convenience:house_age_years +
    sqrt(dist_to_mrt_m):n_convenience:house_age_years +
     0, data = taiwan_real_estate)

# See the result
mdl_price_vs_all_3_way_inter

# Model price vs. sqrt dist. to MRT station, no. of conv. stores & house age, no global intercept, 2-way interactions
mdl_price_vs_all_2_way_inter <- lm(formula = price_twd_msq ~ sqrt(dist_to_mrt_m) + n_convenience + 
    house_age_years + sqrt(dist_to_mrt_m):n_convenience + sqrt(dist_to_mrt_m):house_age_years + 
    n_convenience:house_age_years + 
    0, data = taiwan_real_estate)

# See the result
mdl_price_vs_all_2_way_inter



# Make a grid of explanatory data
explanatory_data <- expand_grid(
  # Set dist_to_mrt_m a seq from 0 to 80 by 10s, squared
  dist_to_mrt_m = seq(0, 80, 10) ^ 2,
  # Set n_convenience to 0 to 10
  n_convenience = 0:10,
  # Set house_age_years to the unique values of that variable
  house_age_years = unique(taiwan_real_estate$house_age_years)
)

# See the result
explanatory_data

# From previous step
explanatory_data <- expand_grid(
  dist_to_mrt_m = seq(0, 80, 10) ^ 2,
  n_convenience = 0:10,
  house_age_years = unique(taiwan_real_estate$house_age_years)
)

# Add predictions to the data frame
prediction_data <- explanatory_data %>% 
  mutate(
    price_twd_msq = predict(mdl_price_vs_all_3_way_inter, explanatory_data)
  )

# See the result
prediction_data

# From previous step
explanatory_data <- expand_grid(
  dist_to_mrt_m = seq(0, 80, 10) ^ 2,
  n_convenience = 0:10,
  house_age_years = unique(taiwan_real_estate$house_age_years)
)
prediction_data <- explanatory_data %>% 
  mutate(price_twd_msq = predict(mdl_price_vs_all_3_way_inter, explanatory_data))

# Extend the plot
ggplot(
  taiwan_real_estate, 
  aes(sqrt(dist_to_mrt_m), n_convenience, color = price_twd_msq)
) +
  geom_point() +
  scale_color_viridis_c(option = "plasma") +
  facet_wrap(vars(house_age_years)) +
  # Add points from prediction data, size 3, shape 15
  geom_point(data = prediction_data, size =3, shape =15)



# Set the intercept to 10
intercept <- 10

# Set the slope to 1
slope <- 1

# Calculate the predicted y values
y_pred <- intercept + slope * x_actual

# Calculate the differences between actual and predicted
y_diff <- y_actual - y_pred

# Calculate the sum of squares
sum(y_diff ^ 2)

calc_sum_of_squares <- function(coeffs) {
  # Get the intercept coeff
  intercept <- coeffs[1]

  # Get the slope coeff
  slope <- coeffs[2]

  # Calculate the predicted y values
  y_pred <- intercept + slope * x_actual

  # Calculate the differences between actual and predicted
  y_diff <- y_actual - y_pred

  # Calculate the sum of squares
  sum(y_diff ^ 2)
}

# From previous step
calc_sum_of_squares <- function(coeffs) {
  intercept <- coeffs[1]
  slope <- coeffs[2]
  y_pred <- intercept + slope * x_actual
  y_diff <- y_actual - y_pred
  sum(y_diff ^ 2)
}

# Optimize the metric
optim(
  # Initially guess 0 intercept and 0 slope
  par = c(intercept = 0, slope = 0), 
  # Use calc_sum_of_squares as the optimization fn 
  fn = calc_sum_of_squares
)

# Compare the coefficients to those calculated by lm()
lm(price_twd_msq ~ n_convenience, data = taiwan_real_estate)

#### 2.4 Multiple Logistic Regression

# Using churn, plot recency vs. length of relationship colored by churn status
ggplot(churn, aes(time_since_first_purchase,time_since_last_purchase,  color = has_churned)) +
  # Make it a scatter plot, with transparency 0.5
  geom_point(alpha = 0.5) +
  # Use a 2-color gradient split at 0.5
  scale_color_gradient2(midpoint =0.5) +
  # Use the black and white theme
  theme_bw()


# Fit a logistic regression of churn status vs. length of relationship, recency, and an interaction
mdl_churn_vs_both_inter <- glm(has_churned ~ time_since_first_purchase + time_since_last_purchase + time_since_first_purchase * time_since_last_purchase, data = churn, family = binomial)


# See the result
mdl_churn_vs_both_inter



# Make a grid of explanatory data
explanatory_data <- expand_grid(
  # Set len. relationship to seq from -2 to 4 in steps of 0.1
  time_since_first_purchase = seq(-2, 4, by= 0.1),
  # Set recency to seq from -1 to 6 in steps of 0.1
  time_since_last_purchase = seq(-1, 6, by= 0.1),
)

# See the result
explanatory_data

# From previous steps
explanatory_data <- expand_grid(
  time_since_first_purchase = seq(-2, 4, 0.1),
  time_since_last_purchase = seq(-1, 6, 0.1)
)

# Add a column of predictions using mdl_churn_vs_both_inter and explanatory_data with type response
prediction_data <- explanatory_data %>%  mutate(
  has_churned = predict(mdl_churn_vs_both_inter, explanatory_data, type ="response")
  )

# See the result
prediction_data

# From previous steps
explanatory_data <- expand_grid(
  time_since_first_purchase = seq(-2, 4, 0.1),
  time_since_last_purchase = seq(-1, 6, 0.1)
)
prediction_data <- explanatory_data %>% 
  mutate(
    has_churned = predict(mdl_churn_vs_both_inter, explanatory_data, type = "response")
  )

# Extend the plot
ggplot(
  churn, 
  aes(time_since_first_purchase, time_since_last_purchase, color = has_churned)
) +
  geom_point(alpha = 0.5) +
  scale_color_gradient2(midpoint = 0.5) +
  theme_bw() +
  # Add points from prediction_data with size 3 and shape 15
  geom_point(data = prediction_data, size = 3, shape = 15)



# Get the actual responses from churn
actual_response <- churn$has_churned

# Get the predicted responses from the model
predicted_response <- round(fitted(mdl_churn_vs_both_inter))

# Get a table of these values
outcomes <- table(predicted_response, actual_response)

# Convert the table to a conf_mat object
confusion <- conf_mat(outcomes)

# See the result
confusion

# From previous step
actual_response <- churn$has_churned
predicted_response <- round(fitted(mdl_churn_vs_both_inter))
outcomes <- table(predicted_response, actual_response)
confusion <- conf_mat(outcomes)

# "Automatically" plot the confusion matrix
autoplot(confusion)

# Get summary metrics
summary(confusion, event_level ="second")



logistic_distn_cdf <- tibble(
  # Make a seq from -10 to 10 in steps of 0.1
  x = seq(-10, 10, 0.1),
  # Transform x with built-in logistic CDF
  logistic_x = plogis(x),
  # Transform x with manual logistic
  logistic_x_man = 1 / (1 + exp(-x))
) 

# Check that each logistic function gives the same results
all.equal(
  logistic_distn_cdf$logistic_x, 
  logistic_distn_cdf$logistic_x_man
)

# From previous step
logistic_distn_cdf <- tibble(
  x = seq(-10, 10, 0.1),
  logistic_x = plogis(x),
  logistic_x_man = 1 / (1 + exp(-x))
)

# Using logistic_distn_cdf, plot logistic_x vs. x
ggplot(logistic_distn_cdf, aes(x, logistic_x)) +
  # Make it a line plot
  geom_line()



logistic_distn_inv_cdf <- tibble(
  # Make a seq from 0.001 to 0.999 in steps of 0.001
  p = seq(0.001, 0.999, 0.001),
  # Transform with built-in logistic inverse CDF
  logit_p = qlogis(p),
  # Transform with manual logit
  logit_p_man = log(p / (1 - p))
) 

# Check that each logistic function gives the same results
all.equal(
  logistic_distn_inv_cdf$logit_p,
  logistic_distn_inv_cdf$logit_p_man
)

# From previous step
logistic_distn_inv_cdf <- tibble(
  p = seq(0.001, 0.999, 0.001),
  logit_p = qlogis(p),
  logit_p_man = log(p / (1 - p))
)

# Using logistic_distn_inv_cdf, plot logit_p vs. p
ggplot(logistic_distn_inv_cdf, aes(p, logit_p)) +
  # Make it a line plot
  geom_line()


# Look at the structure of binomial() function
str(binomial())

# Call the link inverse on x
linkinv_x <- binomial()$linkinv(x)

# Check linkinv_x and plogis() of x give same results 
all.equal(linkinv_x, plogis(x))

# Call the link fun on p
linkfun_p <- binomial()$linkfun(p)

# Check linkfun_p and qlogis() of p give same results  
all.equal(linkfun_p, plogis(p))



# Set the intercept to 1
intercept <- 1

# Set the slope to 0.5
slope <- 0.5

# Calculate the predicted y values
y_pred <- plogis(intercept + slope * x_actual)

# Calculate the log-likelihood for each term
log_likelihoods <- log(y_pred) * y_actual + log(1 - y_pred) * (1 - y_actual)

# Calculate minus the sum of the log-likelihoods for each term
-sum(log_likelihoods)



calc_neg_log_likelihood <- function(coeffs) {
  # Get the intercept coeff
  intercept <- coeffs[1]

  # Get the slope coeff
  slope <- coeffs[2]

  # Calculate the predicted y values
  y_pred <- plogis(intercept + slope * x_actual)

  # Calculate the log-likelihood for each term
  log_likelihoods <- log(y_pred) * y_actual + log(1 - y_pred) * (1 - y_actual)

  # Calculate minus the sum of the log-likelihoods for each term
  -sum(log_likelihoods)
}


# From previous step
calc_neg_log_likelihood <- function(coeffs) {
  intercept <- coeffs[1]
  slope <- coeffs[2]
  y_pred <- plogis(intercept + slope * x_actual)
  log_likelihoods <- log(y_pred) * y_actual + log(1 - y_pred) * (1 - y_actual)
  -sum(log_likelihoods)
}

# Optimize the metric
optim(
  # Initially guess 0 intercept and 1 slope
  par = c(intercept = 0, slope = 1),
  # Use calc_neg_log_likelihood as the optimization fn 
  fn = calc_neg_log_likelihood
)

# Compare the coefficients to those calculated by glm()
glm(has_churned ~ time_since_last_purchase, data = churn, family = binomial)

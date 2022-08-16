### You are analyzing data from stock markets and notice that some stocks seem to have a high covariance. 
### You want to reduce the variables to those that explain most of the variability in the data.
### Which technique would you use to do that?

print("Principal component analysis")

### Fit a linear regression of y versus x.

model <- lm(y ~ x, data = dist)
coef(model)

### Build and fit an appropriate model in order to estimate the income of new individuals.

model <- lm(income ~ ., data = credit)
coef(model)

### Fit a logistic regression model on the response variable Acceptance as a function of the feature variable GPA.

model <- glm(
  formula = Acceptance ~ GPA,
  data = MedGPA,
  family = binomial,
)
coef(model)

### Available in this session is the dataset train. Implement k-means clustering on train with a pre-determined set of 3 clusters.

model <- kmeans(train, centers = 3)
model$centers

### Use the gradient boosting model to predict the expected final grade for the new_students variable that contains the 
### past results of two new pupils that have just arrived on campus.

library(gbm)

model <- gbm(
  formula = Final ~ .,
  data = train,
  distribution = "gaussian",
  n.trees = 100)

predicted <- predict(model, newdata = new_students)

new_students$predicted=predicted
new_students

### Using the fitted model, predict estimates of price for the test data.

model <- lm(price ~ status, data = train)
pred <- predict(model, test)
head(pred)

### Using the train dataset and rpart package, fit a classification decision tree model on the response variable species 
### as a function of the other variables in the dataset.

library(rpart)

model <- rpart( formula = species ~ .,
  data = train, method = "class",
)
model

### Fit a linear regression model to data consisting of 200 observations of a single independent variable, x1, and a dependent variable y.

model <- lm(y ~ x1, data = df)
coef(model
     
### Which one of the following statements describes an unsupervised learning problem?

print("Understand whether observations fit into distinct groups based on their characteristics.")     
     
### Fit a linear regression model to data consisting of a 200 observations of a 3 dependent (x1, x2, x3) and a single response variable y.
     
model <- lm(y ~ x1 + x2 + x3, data = df)
coef(model)     

### Randomly split the dataset into a train dataset and test dataset. The train dataset should contain 70% of the data.
     
set.seed(123)
sample_size <- round(nrow(wine) * 0.7)

train_indices <- sample(1:nrow(wine), 
                        sample_size, 
                        replace = FALSE)
     
wine_train <- wine[train_indices, ]
wine_test <- wine[-train_indices, ]

nrow(wine_train)
nrow(wine_test)
     
### Use k-means clustering to identify the centers for each group.
     
transaction_clusters <- kmeans(transactions, centers = 2)
transaction_clusters$centers
     
### Consider the scree plot below. Based on this plot, what is the optimal number of principal components in the dataset?

print("The elbow occurs at 2.")
     
### Which of the following metrics would not be used when assessing the performance of a classification model?

print("Median absolute error.")     

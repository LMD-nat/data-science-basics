### Complete calculate_subsidy function below that calculates the passenger efficiency and returns 0 if the 
### passenger efficiency is below the specified minimum efficiency. 
### If the passenger efficiency is above the minimum efficiency, then the function should calculate the subsidy.

calculate_subsidy <- function(n, d, l, min_efficiency, subsidy_factor) {
  efficiency = n*d/l
  if(efficiency < min_efficiency) {
    return(0)
  }
  efficiency * subsidy_factor
}

subsidy = calculate_subsidy(4, 75, 11, 25, 0.20)
cat("Subsidy $:", subsidy)
subsidy = calculate_subsidy(2, 60, 10.5, 25, 0.20)
cat("Subsidy $:", subsidy)

### Create a new summary method for the object x of class x_class.

x <- c(10, 20, 30)

class(x) <- "x_class"

summary.x_class <- function(x, ...) {
  list(
    mean = mean(x), 
    median = median(x), 
    geomean = exp(mean(log(x)))
  )
}

summary(x)

### Complete the code to return the output

df <- data.frame(
  x = 1:5
)

df$y <- 6:10
df

### The code below should take each vector in the values list in turn and add 5 to each element of that vector.

values <- list(
  A = c(9, 1, 8, 4, 3),
  B = c(7,  3,  4,  4,  1,  1, 10,  8)
)

as.list(values, vales + 5)
map(values, ~.x + 5)

### Complete the code to return the output. Should return TRUE

x <- c(1, 2, 3, 4)

4 %in% x

### Calculate the range (range) for each column, ensuring that you return a list of vectors, with one list element per column.

as.list(food, range)
map(food, range)

### Complete the code to return the output.

x <- rep(TRUE, 50)

length(x)
x

### Use a suitable function to run the user defined, measures_of_center(), on each element of the list vectors, returning a list of values

measures_of_center <- function(x){
  c(mean = mean(x), median = median(x))
}

vectors <- list(
  A = c(9, 1, 8, 4, 3),
  B = c(7,  3,  4,  4,  1,  1, 10,  8)
)

measures_of_center(vectors, A, B)
map(vectors, measures_of_center)

### Use a suitable function to run the mean() on each element of the list vectors, returning a list of values

vectors <- list(
  A = c(9, 1, 8, 4, 3),
  B = c(7,  3,  4,  4,  1,  1, 10,  8)
)

lapply(vectors, mean)

### Complete the code to return the output. Should be TRUE
x <- NA
is.na(x)

### Complete the code to return the output. Should return [1] 1 5 3.
x <- c(1, 2, 3)
x[2] = 5
x

### Complete the code to return the output. 
### Should return
#[1] "April"
#[1] "May"
#[1] "September"

x <- c("April", "May", "September")

for(i in x){
  print(i)
}

### The central_tendency function can calculate the mean or the median of an input. 
### If the user sets the calc_mean argument to TRUE then it calculates the mean and if calc_mean is FALSE, 
### then it calculates the median. The function should check that the value of the calc_mean argument is valid.

central_tendency <- function(x, calc_mean = TRUE) {
  if(!is.logical(calc_mean)) {
   return("The calc_mean argument must be logical")
  } 
  if(calc_mean) {
    return(mean(x))
    }
  else {
    return(median(x))
    }
}

central_tendency(c(11, 32, 32, 52, 45), calc_mean = "Yes")

### Complete the code to return the output. Should return [1] "x must be numeric". 

square <- function(x) {
  if(!is.numeric(x)){
    print("x must be numeric")
  } else {
    x ^ 2
  }
}

square("Ten")

### Use a suitable tidyverse operator to calculate the mean of the lengths, removing missing values

lengths <- c(7,  NA,  4,  4,  1,  NA, 10,  8)

lengths %>% 
  mean(lengths, na.rm = TRUE)
  mean(na.rm = TRUE)

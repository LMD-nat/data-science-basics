# Data Science Basics

### Code and case study from the Data Scientist with R career track from DataCamp (October 2022)

I have included the code that I wrote, but not the datasets because they don't belong to me. Also I didn't write the case study, just the solutions and workflow. 

**Courses on the syllabus:**

I think the courses might have changed since I got the certification, so I've listed the courses here. Also because sometimes they become obsolete. 

1. Introduction to R
2. Intermediate R
3. Introduction to the Tidyverse
4. Data Manipulation with dplyr
5. Joining Data with dplyr
6. Introduction to Data Visualization with ggplot2
7. Intermediate Data Visualization with ggplot2
8. Reporting with R Markdown
9. Introduction to Importing Data in R
10. Intermediate Importing Data in R
11. Cleaning Data in R
12. Working with Dates and Times in R
13. Introduction to Writing Functions in R
14. Exploratory Data Analysis in R
15. Case Study: Exploratory Data Analysis in R
16. Introduction to Statistics in R
17. Introduction to Regression in R
18. Intermediate Regression in R
19. Supervised Learning in R: Classification
20. Supervised Learning in R: Regression
21. Unsupervised Learning in R
22. Cluster Analysis in R

**Code organization** 

I organized code based on themes while weeding out repetition, because some courses cover the same content. 

1. `basics.R` includes basic stuff in Base R, ifelse statements, sapply, lapply, grep, sub, and for loops. 
2. `classification.R` includes machine learning tools for grouping observations. Includes k-NN, naive bayes, and classification and regression trees (rpart). 
3. `cleaning.R` includes basically all things designed to clean out a dataset. Record linkage, operations with dplyr and checking stuff (assert_is) is written here. 
4. `do_eda.R` has all the steps from start to finish of an exploratory data analysis. 
5. `dplyr.R` includes all the dplyr and plyr functions, and discusses the pipe operator in detail.
6. `eda_case_study.R` is an example case study using the code from `do_eda.R`. 
7. `imports.R` has basically every import method for basically any type of dataset (.txt, .csv, sas datasets, etc.)
8. `lubridate.R` has functions for working with dates and time series data. 
9. `markdown.Rmd` describes how to write reports in MarkDown. 
10. `nhst.R` includes all the statistical analysis functions and code that ascribe meaning to p-values with null hypothesis significance testing (NHST).  
11. `regression.R` includes logistic, simple, and multiple regression. Also includes info on using parallel slopes, cross-validation, and prediction.
12. `unsupervised_learning.R` includes code to run k-means models and decision trees. 
13. `visualization.R` includes code with ggplot to visualize and plot all sorts of data. 
14. `writing_functions.R` is all about writing code that serves a particular need or solves a particular problem, if a function that does the operation you are looking for doesn't already exist. It describes the syntax of functions and how to call them. 

**Assesments:** 

1. Data Manipulation in R
2. Importing and Cleaning Data in R
3. R Programming

These were the obligatory assessments, but I completed many more! 

**Case study:**

1. Moped case study (Jupyter notebook with R code snippets)

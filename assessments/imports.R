### Import "photo.xlsx" and import all the columns as text.

library(readxl)

read_xlsx("photo.xlsx", col_types = "text")

### Import the third sheet, titled "chickwts" of character_excel. 
### Since the first row is skipped you must specify the column names as "weight" and "feed" in that order.

library(readxl)
read_excel(character_excel, sheet = 3, col_names = c("weight", "feed"), skip =  1)

### Import the third sheet, titled "chickwts" of the character_excel, importing both columns as "text".

library(readxl)
read_excel(character_excel, sheet = 3, col_types = "text")

### The cars data has been imported and is shown below. Restructure this to be in a more appropriate form for analysis.

library(tidyr)

pivot_wider(cars, 
           names_from = attribute, 
           values_from = value)
           
### The data movie_reviews has been imported in the format shown below. Convert this to a suitable structure so that it can be used for plotting using ggplot2.

library(tidyr)

movie_reviews %>% 
	pivot_longer(everything(), 
               names_to = "movie", 
               values_to = "rating")
               
### Due to a data entry error, some of the dates entered in the sales data, previewed below, contain a forward slash (/) instead of a dash (-). 
### Correct the format so that all values in the date column have the same structure.

library(stringr)

sales %>% 
  mutate(date = str_replace_all(date, "/", "-")) %>%
  head()
  
### Update the data to replace the missing values in the items_sold column.

library(tidyr)

sales %>%
  fill(items_sold)
  
### Separate dates (MMM-DD-YYYY) so that they have one column for each element, month, day and year.

library(tidyr)

sales %>% 
  separate(date, c("month", "day", "year"), "-") %>%
  head()
  
### Using the data shown below, df, create two new columns, color and direction, that split the values in data at the dash (-).

library(tidyr)

separate(df, col = data, into = c("color", "direction"), sep="-")

### Given the following data, yearly_price, replace all missing values in the value column with the last non-missing value.

library(tidyr)

yearly_price %>%
  fill(value)
  
### Restructure the data, df, so that there is a column for each course giving the rating for each student. Where there is no rating, insert a 0.

df %>% 
  pivot_wider(names_from = course, 
              values_from = rating, 
              values_fill = 0)  
           
### The photo.csv file contains data on photos and the details of the camera settings. 
### Import the first 4 rows of the file. The file path as been stored in photo_file.

library(tidyverse)
read_csv(photo_file, n_max = 4)

### Remove rows where there are missing values only in the price column.

library(dplyr)
library(tidyr)

wine %>%
  drop_na(price)
  
### Remove the leading spaces from all entries in the water_requirement column of the plants data.

library(stringr)
library(dplyr)

plants %>% 
  mutate(water_requirement = trimws(water_requirement))
  
### Split the data into Name, Street, City and Country columns so that it would be possible to determine how many characters are from each country.

library(tidyr)

data  %>% separate(Address, 
              into = c('Name', 'Street', 'City', 'Country'), sep=',')

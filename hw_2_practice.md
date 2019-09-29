hw\_2\_practice
================
Kodiak Soled
9/28/2019

\#Homework Problem \#1

\#\#Mr. Trash Wheel

\#\#\#First, we will need to load the tidyverse package for some of the
datacleaning we will perform:

``` r
library(tidyverse)
```

\#\#\#Then we can import the Excel file data, specify the sheets in the
Excel file we want with the `sheets` function, omit non-data entries by
specifying the columns we want to include with the `range` function.
Next, we can omit the rows that do not include dumpster-specific data by
using the `drop_na` function. Lastly, we will round the number of sports
balls to the nearest integer and converts the result to an integer
variable.

``` r
library(readxl)
mr_trash_wheel = read_excel("./data/HealthyHarborWaterWheelTotals.xlsx", sheet = "Mr. Trash Wheel", range = "A2:N336") %>%
  janitor::clean_names() %>% 
  drop_na(dumpster) %>%
  mutate_if(is.numeric, round, 0)
```

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

\#\#\#Then we can load the `readxl` package in order to import the Excel
file data using `read_excel`, specify the sheet in the Excel file we
want with `sheets`, and omit non-data entries by specifying the columns
and rows we want to include with the `range` function. We also clean up
the names of the variables with the `janitor` function. Next, we can
omit the rows that do not include dumpster-specific data by using
`drop_na`. Lastly, we can round the number of sports balls to the
nearest integer and convert the result to an integer variable using
`as.integer`.

``` r
library(readxl)
mr_trash_wheel = read_excel("./data/HealthyHarborWaterWheelTotals.xlsx", sheet = "Mr. Trash Wheel", range = "A2:N336") %>%
  janitor::clean_names() %>%
  drop_na(dumpster) %>%
  mutate(sports_balls = as.integer(sports_balls, 0))
    
mr_trash_wheel
```

    ## # A tibble: 285 x 14
    ##    dumpster month  year date                weight_tons volume_cubic_ya…
    ##       <dbl> <chr> <dbl> <dttm>                    <dbl>            <dbl>
    ##  1        1 May    2014 2014-05-16 00:00:00        4.31               18
    ##  2        2 May    2014 2014-05-16 00:00:00        2.74               13
    ##  3        3 May    2014 2014-05-16 00:00:00        3.45               15
    ##  4        4 May    2014 2014-05-17 00:00:00        3.1                15
    ##  5        5 May    2014 2014-05-17 00:00:00        4.06               18
    ##  6        6 May    2014 2014-05-20 00:00:00        2.71               13
    ##  7        7 May    2014 2014-05-21 00:00:00        1.91                8
    ##  8        8 May    2014 2014-05-28 00:00:00        3.7                16
    ##  9        9 June   2014 2014-06-05 00:00:00        2.52               14
    ## 10       10 June   2014 2014-06-11 00:00:00        3.76               18
    ## # … with 275 more rows, and 8 more variables: plastic_bottles <dbl>,
    ## #   polystyrene <dbl>, cigarette_butts <dbl>, glass_bottles <dbl>,
    ## #   grocery_bags <dbl>, chip_bags <dbl>, sports_balls <int>,
    ## #   homes_powered <dbl>

\#\#Precipitation Data

\#\#\#First we can read (`read_excel`) and clean (`janitor`) the
preciptation data for 2017 and 2018. Again, we can omit the rows without
the precipitation data by specifying the `range` of rows and columns to
include. Lastly, we can add a variable year to each data set with the
`mutate` function.

``` r
precip_data_2018 = 
  read_excel("./data/HealthyHarborWaterWheelTotals.xlsx", sheet = "2018 Precipitation", range = "A2:B9") %>%
  janitor::clean_names() %>%
  mutate(year = 2018)

precip_data_2018
```

    ## # A tibble: 7 x 3
    ##   month total  year
    ##   <dbl> <dbl> <dbl>
    ## 1     1  0.96  2018
    ## 2     2  5.3   2018
    ## 3     3  2.18  2018
    ## 4     4  3.2   2018
    ## 5     5  9.27  2018
    ## 6     6  0.2   2018
    ## 7     7  2.39  2018

``` r
precip_data_2017 = 
  read_excel("./data/HealthyHarborWaterWheelTotals.xlsx", sheet = "2017 Precipitation", range = "A2:B14") %>%
  janitor::clean_names() %>%
  mutate(year = 2017)
 
precip_data_2017
```

    ## # A tibble: 12 x 3
    ##    month total  year
    ##    <dbl> <dbl> <dbl>
    ##  1     1  2.34  2017
    ##  2     2  1.46  2017
    ##  3     3  3.57  2017
    ##  4     4  3.99  2017
    ##  5     5  5.64  2017
    ##  6     6  1.4   2017
    ##  7     7  7.09  2017
    ##  8     8  4.44  2017
    ##  9     9  1.95  2017
    ## 10    10  0     2017
    ## 11    11  0.11  2017
    ## 12    12  0.94  2017

\#\#\#Finally, we can combine the two datasets with the `full join`
function since they both datasets have the same three variables in each
respective dataframe. We can also convert the month as a numeric (1-12)
to a character variable (january-december) so that the month names
appear using `month.numeric`:

``` r
precip_data = full_join(precip_data_2017, precip_data_2018) %>%
  mutate(
    month = month.name[month],
    month = str_to_lower(month))

precip_data
```

    ## # A tibble: 19 x 3
    ##    month     total  year
    ##    <chr>     <dbl> <dbl>
    ##  1 january    2.34  2017
    ##  2 february   1.46  2017
    ##  3 march      3.57  2017
    ##  4 april      3.99  2017
    ##  5 may        5.64  2017
    ##  6 june       1.4   2017
    ##  7 july       7.09  2017
    ##  8 august     4.44  2017
    ##  9 september  1.95  2017
    ## 10 october    0     2017
    ## 11 november   0.11  2017
    ## 12 december   0.94  2017
    ## 13 january    0.96  2018
    ## 14 february   5.3   2018
    ## 15 march      2.18  2018
    ## 16 april      3.2   2018
    ## 17 may        9.27  2018
    ## 18 june       0.2   2018
    ## 19 july       2.39  2018

\#\#\#The Mr. Trash Wheel dataset contains 14 variables (285
observations) including: dumpster, month, year, date, weight (tons),
etc. from May of 2014 until July of 2018. It is contains 285 rows and 14
columns for a total of 3990 variables. The median number of sports balls
in a dumpster in 2017 was 8.

``` r
#median number of sports balls in a dumpter in 2017
mr_trash_wheel_2017 = read_excel("./data/HealthyHarborWaterWheelTotals.xlsx", sheet = "Mr. Trash Wheel", range = "A2:N336") %>%
  janitor::clean_names() %>%
  drop_na(dumpster) %>%
  filter(year == "2017")

median(pull(mr_trash_wheel_2017, sports_balls))
```

    ## [1] 8

\#\#\#The combined precipition dataset has 18 observations and contains
the total preciptation per month for the year 2017 and half of the year
2018. It is contains 19 rows and 3 columns for a total of 57 variables.
In 2018 the total precipitation was 23.5.

\#Problem 2

\#\#FiveThiryEight

\#\#\#First we import (`read_csv`) and clean (`janitor`) the datasets:
pols-month.csv, unemployment.csv, and snp.csv. Then, we can use the
`separate` function to change the “date” variable into “year”, “month”,
and “day” (note this isn’t necessary for unemployment as it was already
seperated).

\#\#\#For the pols-month data, I replaced the month number with the
abbreviated month name to match the unemployment dataset with
`month.abb` under the `mutate` function (specifying the month as
`as.integer` allowed us to do this). We also can create a new variable
“president” under `mutate` with the `if_else` function and remove the
prez\_gap, prez\_day, and day variable with `select` to clean up the
dataset.

\#\#\#We can then clean the snp data to look similar to the pols-month
data by using many of the same functions as above (e.g., deleting the
day variable with `select`, changing month as a number to a name with
`month.abb` under the `mutate` function, etc.)

\#\#\#The unemployment data needed to be reorganized from a “wide” to
“long” format to match the first two datasets. The `pivot_longer`
function allows us to do this. To match the other two datasets, we also
needed to make the year a character vector which we did with the
`mutate` and `as.character` functions. The cleaned datasets look like
the following:

``` r
pols_month =
  read_csv("./data/pols-month.csv") %>%
  janitor::clean_names() %>%
  separate(mon, into = c("year", "month", "day")) %>%
  mutate(
    month = month.abb[as.integer(month)],
    month = str_to_lower(month),
    president = if_else(prez_gop == 1, "gop", "dem")) %>%
  select(-prez_gop, -prez_dem, -day)

pols_month
```

    ## # A tibble: 822 x 9
    ##    year  month gov_gop sen_gop rep_gop gov_dem sen_dem rep_dem president
    ##    <chr> <chr>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl> <chr>    
    ##  1 1947  jan        23      51     253      23      45     198 dem      
    ##  2 1947  feb        23      51     253      23      45     198 dem      
    ##  3 1947  mar        23      51     253      23      45     198 dem      
    ##  4 1947  apr        23      51     253      23      45     198 dem      
    ##  5 1947  may        23      51     253      23      45     198 dem      
    ##  6 1947  jun        23      51     253      23      45     198 dem      
    ##  7 1947  jul        23      51     253      23      45     198 dem      
    ##  8 1947  aug        23      51     253      23      45     198 dem      
    ##  9 1947  sep        23      51     253      23      45     198 dem      
    ## 10 1947  oct        23      51     253      23      45     198 dem      
    ## # … with 812 more rows

``` r
snp = 
  read_csv("./data/snp.csv") %>%
  janitor::clean_names() %>%
  separate(date, into = c("month", "day", "year")) %>%
  select(-day) %>%
  mutate(
    month = month.abb[as.integer(month)],
    month = str_to_lower(month)) %>%
  select(year, month, close) %>%
  arrange(year, month) %>%
  rename(snp_close = close)

snp
```

    ## # A tibble: 787 x 3
    ##    year  month snp_close
    ##    <chr> <chr>     <dbl>
    ##  1 1950  apr        18.0
    ##  2 1950  aug        18.4
    ##  3 1950  dec        20.4
    ##  4 1950  feb        17.2
    ##  5 1950  jan        17.0
    ##  6 1950  jul        17.8
    ##  7 1950  jun        17.7
    ##  8 1950  mar        17.3
    ##  9 1950  may        18.8
    ## 10 1950  nov        19.5
    ## # … with 777 more rows

``` r
unemployment = 
  read_csv("./data/unemployment.csv") %>%
  janitor::clean_names() %>%
  pivot_longer(
    jan:dec,
    names_to = "month",
    values_to = "count") %>%
  mutate(year = as.character(year)) %>%
  rename(unemployment_rate = count)

unemployment
```

    ## # A tibble: 816 x 3
    ##    year  month unemployment_rate
    ##    <chr> <chr>             <dbl>
    ##  1 1948  jan                 3.4
    ##  2 1948  feb                 3.8
    ##  3 1948  mar                 4  
    ##  4 1948  apr                 3.9
    ##  5 1948  may                 3.5
    ##  6 1948  jun                 3.6
    ##  7 1948  jul                 3.6
    ##  8 1948  aug                 3.9
    ##  9 1948  sep                 3.8
    ## 10 1948  oct                 3.7
    ## # … with 806 more rows

\#\#\#Now we are readt to combine the three datasets. We can first merge
the pols-month and snp datasets with the `left_join` function by the
shared catagory of “month” and “year”. Then we can merge this new
dataset with the final dataset unemployment in the same fashion:

``` r
pol_snp_data = 
  left_join(pols_month, snp, by = c("month", "year"))

pol_snp_unemployment_data = 
  left_join(pol_snp_data, unemployment, by = c("month", "year")) %>%
  select(month, year, president, unemployment_rate, snp_close, everything())

pol_snp_unemployment_data
```

    ## # A tibble: 822 x 11
    ##    month year  president unemployment_ra… snp_close gov_gop sen_gop rep_gop
    ##    <chr> <chr> <chr>                <dbl>     <dbl>   <dbl>   <dbl>   <dbl>
    ##  1 jan   1947  dem                     NA        NA      23      51     253
    ##  2 feb   1947  dem                     NA        NA      23      51     253
    ##  3 mar   1947  dem                     NA        NA      23      51     253
    ##  4 apr   1947  dem                     NA        NA      23      51     253
    ##  5 may   1947  dem                     NA        NA      23      51     253
    ##  6 jun   1947  dem                     NA        NA      23      51     253
    ##  7 jul   1947  dem                     NA        NA      23      51     253
    ##  8 aug   1947  dem                     NA        NA      23      51     253
    ##  9 sep   1947  dem                     NA        NA      23      51     253
    ## 10 oct   1947  dem                     NA        NA      23      51     253
    ## # … with 812 more rows, and 3 more variables: gov_dem <dbl>,
    ## #   sen_dem <dbl>, rep_dem <dbl>

\#\#\#The pols-month dataset contained 822 observations about the
republican and democrat president, govenor, senator, and house
representative from 1947-2015. The snp dataset contained 787
observations about the closing rate of the snp from 1950-2015. The
unemployment dataset contained 68 observations about the umployment rate
by month from 1948-2015. \#\#\#The combined dataset contains data from
1947 to 2015 on the variables: month, year, president, unemployment
rate, the closing rate for the snp, and the govenor, senator, house of
representatives of the democratic and republican parties. It is contains
822 rows and 11 columns for a total of 9042 variables.

\#Problem \#3

\#\#NYC Open

\#\#\#First we need to load (`read_csv`) the dataset pop\_baby\_names
and tidy it. This includes cleaning the variable names (`janitor`),
changing all the case structure of string variables to lower case
(`str_to_lower`), making the string variables under ethnicity similar
(e.g., making “asian and paci” and “asian and pacific islander” have the
same name through the `replace` function), and deduplicating the dataset
(`unique`).

``` r
pop_baby_names =
  read_csv("./data/Popular_Baby_Names.csv") %>%
  janitor::clean_names() %>%
  mutate(
    gender = str_to_lower(gender),
    ethnicity = str_to_lower(ethnicity), 
    childs_first_name = str_to_lower(childs_first_name),
    ethnicity = replace(ethnicity, ethnicity == "asian and paci", "asian and pacific islander"), 
    ethnicity = replace(ethnicity, ethnicity == "black non hisp", "black non hispanic"), 
    ethnicity = replace(ethnicity, ethnicity == "white non hisp", "white non hispanic")
  ) %>%
  unique(incomparables = FALSE)

pop_baby_names
```

    ## # A tibble: 12,181 x 6
    ##    year_of_birth gender ethnicity              childs_first_na… count  rank
    ##            <dbl> <chr>  <chr>                  <chr>            <dbl> <dbl>
    ##  1          2016 female asian and pacific isl… olivia             172     1
    ##  2          2016 female asian and pacific isl… chloe              112     2
    ##  3          2016 female asian and pacific isl… sophia             104     3
    ##  4          2016 female asian and pacific isl… emily               99     4
    ##  5          2016 female asian and pacific isl… emma                99     4
    ##  6          2016 female asian and pacific isl… mia                 79     5
    ##  7          2016 female asian and pacific isl… charlotte           59     6
    ##  8          2016 female asian and pacific isl… sarah               57     7
    ##  9          2016 female asian and pacific isl… isabella            56     8
    ## 10          2016 female asian and pacific isl… hannah              56     8
    ## # … with 12,171 more rows

\#\#\#Then we can create a reader-friendly table that displays the
popularity of the name Olivia from 2011-2016. To do this, we can perform
the same cleaning as above, but we will also need to `filter` for the
name Olivia, reorganize the variable order using `select` then
`arrange`, and then `pivot_wider` the dataframe so that we can produce a
table of the popularity of the name Olivia by ethnicity across time.

``` r
olivia =
  read_csv("./data/Popular_Baby_Names.csv") %>%
  janitor::clean_names() %>%
  mutate(
    gender = str_to_lower(gender),
    ethnicity = str_to_lower(ethnicity), 
    childs_first_name = str_to_lower(childs_first_name),
    ethnicity = replace(ethnicity, ethnicity == "asian and paci", "asian and pacific islander"), 
    ethnicity = replace(ethnicity, ethnicity == "black non hisp", "black non hispanic"), 
    ethnicity = replace(ethnicity, ethnicity == "white non hisp", "white non hispanic")
  ) %>%
  filter(childs_first_name == "olivia") %>%
  unique(incomparables = FALSE) %>%
  select(year_of_birth, rank, ethnicity) %>%
  arrange(year_of_birth, rank) %>%
  pivot_wider(
    names_from = "year_of_birth",
    values_from = "rank"
    )

olivia
```

    ## # A tibble: 4 x 7
    ##   ethnicity                  `2011` `2012` `2013` `2014` `2015` `2016`
    ##   <chr>                       <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>
    ## 1 white non hispanic              2      4      1      1      1      1
    ## 2 asian and pacific islander      4      3      3      1      1      1
    ## 3 black non hispanic             10      8      6      8      4      8
    ## 4 hispanic                       18     22     22     16     16     13

\#\#\#The most popular male child’s name can be identified by filtering
pop\_baby\_names by gender, and seeing the name that ranked \#1. This
was Ethan. Then, the same produce can be repeated from the example above
to produce a simar table of the popularity of the name Ethan by
ethnicity across time.

``` r
ethan =
  read_csv("./data/Popular_Baby_Names.csv") %>%
  janitor::clean_names() %>%
  mutate(
    gender = str_to_lower(gender),
    ethnicity = str_to_lower(ethnicity), 
    childs_first_name = str_to_lower(childs_first_name),
    ethnicity = replace(ethnicity, ethnicity == "asian and paci", "asian and pacific islander"), 
    ethnicity = replace(ethnicity, ethnicity == "black non hisp", "black non hispanic"), 
    ethnicity = replace(ethnicity, ethnicity == "white non hisp", "white non hispanic")
  ) %>%
  filter(childs_first_name == "ethan") %>%
  unique(incomparables = FALSE) %>%
  select(year_of_birth, rank, ethnicity) %>%
  arrange(year_of_birth, rank) %>%
  pivot_wider(
    names_from = "year_of_birth",
    values_from = "rank"
    )

ethan
```

    ## # A tibble: 4 x 7
    ##   ethnicity                  `2011` `2012` `2013` `2014` `2015` `2016`
    ##   <chr>                       <dbl>  <dbl>  <dbl>  <dbl>  <dbl>  <dbl>
    ## 1 asian and pacific islander      1      2      2      2      2      1
    ## 2 black non hispanic              6      3      1      1      5      5
    ## 3 hispanic                        6      4      5      5      3      7
    ## 4 white non hispanic             26     21     23     18     19     20

\#\#\#To produce a scatterplot of male, white non-hispanic children born
in 2016, we need to first `filter` the dataset to screen for male
gender, 2016 year, and white non hispanic ethnicity. Then we can create
a scatterplot using `ggplot` of this new subset of the data that shows
the number of children with a name against the rank in popularity of
that name:

``` r
library(ggridges)
male_wt_2016 = 
  read_csv("./data/Popular_Baby_Names.csv") %>%
  janitor::clean_names() %>%
  mutate(
    gender = str_to_lower(gender),
    ethnicity = str_to_lower(ethnicity), 
    childs_first_name = str_to_lower(childs_first_name),
    ethnicity = replace(ethnicity, ethnicity == "asian and paci", "asian and pacific islander"), 
    ethnicity = replace(ethnicity, ethnicity == "black non hisp", "black non hispanic"), 
    ethnicity = replace(ethnicity, ethnicity == "white non hisp", "white non hispanic")
  ) %>%
  filter(
    gender == "male", 
    year_of_birth == "2016",
    ethnicity == "white non hispanic"
    ) %>%
  unique(incomparables = FALSE)

male_wt_2016
```

    ## # A tibble: 364 x 6
    ##    year_of_birth gender ethnicity          childs_first_name count  rank
    ##            <dbl> <chr>  <chr>              <chr>             <dbl> <dbl>
    ##  1          2016 male   white non hispanic joseph              261     1
    ##  2          2016 male   white non hispanic michael             260     2
    ##  3          2016 male   white non hispanic david               255     3
    ##  4          2016 male   white non hispanic moshe               239     4
    ##  5          2016 male   white non hispanic jacob               236     5
    ##  6          2016 male   white non hispanic james               231     6
    ##  7          2016 male   white non hispanic benjamin            219     7
    ##  8          2016 male   white non hispanic alexander           211     8
    ##  9          2016 male   white non hispanic daniel              196     9
    ## 10          2016 male   white non hispanic henry               196     9
    ## # … with 354 more rows

``` r
Male_Names = ggplot(male_wt_2016, aes(x = rank, y = count)) + geom_point() 
print(Male_Names + ggtitle("Number of Male, White Non-Hispanic Children \nBorn in 2016 by Popularity") + labs(y = "Number of Children with a Name", x = "Rank in Popularity"))
```

![](hw_2_practice_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

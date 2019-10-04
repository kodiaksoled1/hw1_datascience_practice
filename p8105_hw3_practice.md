p8105\_hw3\_practice
================
Kodiak Soled
10/4/2019

*Note to TAs: you may need to* `install.packages(kableExtra)` *to run my
code for problem \#3.*

\#\#\#Setting up the document:

``` r
library(tidyverse) #first load packages you want... 
```

    ## ── Attaching packages ─────────── tidyverse 1.2.1 ──

    ## ✔ ggplot2 3.2.1     ✔ purrr   0.3.2
    ## ✔ tibble  2.1.3     ✔ dplyr   0.8.3
    ## ✔ tidyr   1.0.0     ✔ stringr 1.4.0
    ## ✔ readr   1.3.1     ✔ forcats 0.4.0

    ## ── Conflicts ────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
library(viridis)
```

    ## Loading required package: viridisLite

``` r
knitr::opts_chunk$set(
    echo = TRUE,
    warning = FALSE,
    fig.width = 8, 
  fig.height = 6,
  out.width = "90%"
)

options(
  ggplot2.continuous.colour = "viridis",
  ggplot2.continuous.fill = "viridis"
)

scale_colour_discrete = scale_colour_viridis_d

scale_fill_discrete = scale_fill_viridis_d

theme_set(theme_minimal() + theme(legend.position = "bottom"))
```

# Problem \#1

## Instacart Dataset

### We first need to read in and tidy the instacart dataset:

``` r
library(p8105.datasets)
data("instacart")

cleaned_instacart = 
  instacart %>%
  janitor::clean_names() %>%
  mutate(
    product_name = str_to_lower(product_name)
  )
```

### Description

  - There are 1384617 observations and 15 variables in the Instacart
    dataset.
  - Some key variables include the order and product identifier, the
    name of the product, the name and identifier of the department and
    aisle, and several variables that include information about the
    ordering of the product.
  - For example, Organic Hass Avocados’s product identifier is 47209. It
    is in the fresh fruit aisle (identifier: 24) which is part of the
    produce department (identifier:
4).

## Answering Problem 1 Questions:

### Determing the number of aisles and which aisles most items are ordered from:

``` r
aisle = 
  cleaned_instacart %>% 
  group_by(aisle) %>%
  count() %>%
  arrange(desc(n))

aisle
```

    ## # A tibble: 134 x 2
    ## # Groups:   aisle [134]
    ##    aisle                              n
    ##    <chr>                          <int>
    ##  1 fresh vegetables              150609
    ##  2 fresh fruits                  150473
    ##  3 packaged vegetables fruits     78493
    ##  4 yogurt                         55240
    ##  5 packaged cheese                41699
    ##  6 water seltzer sparkling water  36617
    ##  7 milk                           32644
    ##  8 chips pretzels                 31269
    ##  9 soy lactosefree                26240
    ## 10 bread                          23635
    ## # … with 124 more rows

  - There are 134 aisles in this dataset. The most orders are from the
    fresh vegetables and fresh fruits
aisles.

## Here is a scatterplot that shows the number of items ordered in each aisle for aisles with more than 10,000 items ordered:

``` r
aisle %>%
  filter(n > 10000) %>%
  ggplot(aes(x = n, y = aisle)) + 
  geom_point() +
  labs(
    title = "Number of Items Ordered per Aisles (>10,000)",
    x = "Number of Items Ordered",
    y = "Aisle Name", 
    caption = "Data from the Instacart Dataset"
    )
```

<img src="p8105_hw3_practice_files/figure-gfm/unnamed-chunk-4-1.png" width="90%" />

## Here is a table with the three most popular items in each of the aisles “baking ingredients”, “dog food care”, and “packaged vegetables fruits”:

``` r
library(kableExtra)
```

    ## 
    ## Attaching package: 'kableExtra'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     group_rows

``` r
baking_ingredients = 
  cleaned_instacart %>%
  filter(aisle == "baking ingredients") %>%
  group_by(product_name) %>%
  count() %>%
  arrange(desc(n)) %>%
  filter(n > 330) %>%
  rename(number_times_item_ordered = n) %>%
  knitr::kable(caption = "Three most Popular Items in the Baking Ingredients Aisle") %>%
  kable_styling(bootstrap_options = c("striped", "condensed", font_size = 12))

baking_ingredients
```

<table class="table table-striped table-condensed" style="margin-left: auto; margin-right: auto;">

<caption>

Three most Popular Items in the Baking Ingredients Aisle

</caption>

<thead>

<tr>

<th style="text-align:left;">

product\_name

</th>

<th style="text-align:right;">

number\_times\_item\_ordered

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

light brown sugar

</td>

<td style="text-align:right;">

499

</td>

</tr>

<tr>

<td style="text-align:left;">

pure baking soda

</td>

<td style="text-align:right;">

387

</td>

</tr>

<tr>

<td style="text-align:left;">

cane sugar

</td>

<td style="text-align:right;">

336

</td>

</tr>

</tbody>

</table>

``` r
dog_food_care = 
  cleaned_instacart %>%
  filter(aisle == "dog food care") %>%
  group_by(product_name) %>%
  count() %>%
  arrange(desc(n)) %>%
  filter(n > 25) %>%
  rename(number_times_item_ordered = n) %>%
  knitr::kable(caption = "Three most Popular Items in the Dog Food Care Aisle") %>%
  kable_styling(bootstrap_options = c("striped", "condensed", font_size = 12))

dog_food_care
```

<table class="table table-striped table-condensed" style="margin-left: auto; margin-right: auto;">

<caption>

Three most Popular Items in the Dog Food Care Aisle

</caption>

<thead>

<tr>

<th style="text-align:left;">

product\_name

</th>

<th style="text-align:right;">

number\_times\_item\_ordered

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

snack sticks chicken & rice recipe dog treats

</td>

<td style="text-align:right;">

30

</td>

</tr>

<tr>

<td style="text-align:left;">

organix chicken & brown rice recipe

</td>

<td style="text-align:right;">

28

</td>

</tr>

<tr>

<td style="text-align:left;">

small dog biscuits

</td>

<td style="text-align:right;">

26

</td>

</tr>

</tbody>

</table>

``` r
packaged_vegetables_fruits = 
  cleaned_instacart %>%
  filter(aisle == "packaged vegetables fruits") %>%
  group_by(product_name) %>%
  count() %>%
  arrange(desc(n)) %>%
  filter(n > 4900) %>%
  rename(number_times_item_ordered = n) %>%
  knitr::kable(caption = "Three most Popular Items in the Packaged Vegetables Fruits Aisle") %>%
  kable_styling(bootstrap_options = c("striped", "condensed", font_size = 12))

packaged_vegetables_fruits
```

<table class="table table-striped table-condensed" style="margin-left: auto; margin-right: auto;">

<caption>

Three most Popular Items in the Packaged Vegetables Fruits Aisle

</caption>

<thead>

<tr>

<th style="text-align:left;">

product\_name

</th>

<th style="text-align:right;">

number\_times\_item\_ordered

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:left;">

organic baby spinach

</td>

<td style="text-align:right;">

9784

</td>

</tr>

<tr>

<td style="text-align:left;">

organic raspberries

</td>

<td style="text-align:right;">

5546

</td>

</tr>

<tr>

<td style="text-align:left;">

organic
blueberries

</td>

<td style="text-align:right;">

4966

</td>

</tr>

</tbody>

</table>

### Here is a table of the mean hour of the day at which Pink Lady Apples and Coffee Ice Cream are ordered on each day of the week:

``` r
pink_lady_apples = 
  cleaned_instacart %>%
  filter(product_name == "pink lady apples")

coffee_ice_cream = 
  cleaned_instacart %>%
  filter(product_name == "coffee ice cream")

apples_and_ice_cream =
  bind_rows(pink_lady_apples, coffee_ice_cream) %>%
  select(product_name, order_hour_of_day, order_dow) %>%
  group_by(product_name, order_dow) %>%
  summarize(count = n()) %>%
  arrange(order_dow) %>%
  rename(day_of_week = order_dow) %>%
  pivot_wider(
    names_from = "product_name",
    values_from = "count"
  ) %>%
   knitr::kable(caption = "Mean Hour of Day Pink Lady Apples and Coffee Ice Cream is Ordered Each Day of Week") %>% kable_styling(bootstrap_options = c("striped", "condensed", font_size = 12))

apples_and_ice_cream
```

<table class="table table-striped table-condensed" style="margin-left: auto; margin-right: auto;">

<caption>

Mean Hour of Day Pink Lady Apples and Coffee Ice Cream is Ordered Each
Day of Week

</caption>

<thead>

<tr>

<th style="text-align:right;">

day\_of\_week

</th>

<th style="text-align:right;">

coffee ice cream

</th>

<th style="text-align:right;">

pink lady apples

</th>

</tr>

</thead>

<tbody>

<tr>

<td style="text-align:right;">

0

</td>

<td style="text-align:right;">

31

</td>

<td style="text-align:right;">

34

</td>

</tr>

<tr>

<td style="text-align:right;">

1

</td>

<td style="text-align:right;">

19

</td>

<td style="text-align:right;">

50

</td>

</tr>

<tr>

<td style="text-align:right;">

2

</td>

<td style="text-align:right;">

21

</td>

<td style="text-align:right;">

47

</td>

</tr>

<tr>

<td style="text-align:right;">

3

</td>

<td style="text-align:right;">

22

</td>

<td style="text-align:right;">

32

</td>

</tr>

<tr>

<td style="text-align:right;">

4

</td>

<td style="text-align:right;">

23

</td>

<td style="text-align:right;">

29

</td>

</tr>

<tr>

<td style="text-align:right;">

5

</td>

<td style="text-align:right;">

19

</td>

<td style="text-align:right;">

51

</td>

</tr>

<tr>

<td style="text-align:right;">

6

</td>

<td style="text-align:right;">

36

</td>

<td style="text-align:right;">

16

</td>

</tr>

</tbody>

</table>

# Problem 2

## BRFSS Dataset

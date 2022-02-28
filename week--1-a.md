Week 1A: Calculating and Using a Global Minimum Interaction Date
================
Gabriel Butler
2/17/2022

### Part 1: Loading data and libraries

``` r
# loading relevant libraries

library(lubridate)
library(tidyverse)

# loading data

care_management_anonymized <- read.csv('https://github.com/rladiesPHL/2022_datathon/raw/main/data/care_management_anonymized.csv')

pantry_anonymized <- read.csv('https://github.com/rladiesPHL/2022_datathon/raw/main/data/pantry_anonymized.csv')

volunteer_services_anonymized <- read.csv('https://github.com/rladiesPHL/2022_datathon/raw/main/data/volunteer_services_anonymized.csv')
```

### Part 2: Finding the relevant minimum date in the three relevant datasets

`care_management_anonymized`, `pantry_anonymized` and
`volunteer_services_anonymized` each have a variable called
`assistance_date`. I assume that whenever an observation has a
non-missing value for this variable, this value represents a date on
which a client had an interaction of some kind with ElderNet.

After doing some basic cleaning of these `assistance_date` variables in
each of the three datasets, I merge these different (and renamed) date
variables into a single table and then find the minimum (earliest) value
for each of the three dates. A snapshot of these preliminary results is
below.

As we can see, each client has up to three minimum (earliest) date
values. We need to collapse this table into a smaller one that contains
only the minimum (earliest) date value for each client.

``` r
global_min_client_dates <- care_management_anonymized %>%
  mutate(assistance_date = as.Date(assistance_date)) %>%
  select(anon_ID, care_mgmt_assistance_date = assistance_date) %>%
  left_join(., pantry_anonymized %>%
              mutate(assistance_date = as.Date(assistance_date, format = '%m/%d/%Y')) %>% 
              select(anon_ID, pantry_assistance_date = assistance_date)) %>%
  left_join(., volunteer_services_anonymized %>% 
              mutate(rider_first_ride_date = as.Date(rider_first_ride_date)) %>%
              select(anon_ID, rider_first_ride_date)) %>%
  distinct() %>%
  group_by(anon_ID) %>%
  summarize(min_care_mgmt_assistance_date = min(care_mgmt_assistance_date, na.rm = TRUE),
            min_pantry_assistance_date = min(pantry_assistance_date, na.rm = TRUE),
            min_rider_first_ride_date = min(rider_first_ride_date, na.rm = TRUE))

global_min_client_dates %>% head()
```

    ## # A tibble: 6 x 4
    ##   anon_ID min_care_mgmt_assistance_date min_pantry_assistance_~ min_rider_first~
    ##     <int> <date>                        <date>                  <date>          
    ## 1       1 2021-06-22                    NA                      NA              
    ## 2       2 2019-06-04                    2019-02-28              2015-11-10      
    ## 3       3 2019-07-17                    NA                      2019-08-29      
    ## 4       5 2019-12-26                    2019-01-08              NA              
    ## 5       7 2020-01-02                    NA                      2018-05-24      
    ## 6       8 2019-07-18                    2019-11-22              2015-04-27

After performing some row-wise filtering, we get the final result for
`global_min_client_dates`. Notice that each of the values for
`enrollment_date` below correspond to the earliest dates in each of the
rows from the table above.

Not every client has a value for `enrollment_date`.

``` r
global_min_client_dates <- data.frame(global_min_client_dates$anon_ID, min_date = apply(global_min_client_dates[, 2:ncol(global_min_client_dates)], 1, min, na.rm = TRUE))

names(global_min_client_dates) <- c('anon_ID', 'enrollment_date')

global_min_client_dates %>% head()
```

    ##   anon_ID enrollment_date
    ## 1       1      2021-06-22
    ## 2       2      2015-11-10
    ## 3       3      2019-07-17
    ## 4       5      2019-01-08
    ## 5       7      2018-05-24
    ## 6       8      2015-04-27

### Part 3: Revisiting `time_to_first_ride`

Using this new definition of `enrollment_date` to calculate
`time_to_first_ride` produces a dramatically different distribution of
times. The minimum value for for each time is zero, which is something
we should probably expect to see if we have actually nailed down the
date of the earliest interaction of each client with ElderNet. All
non-missing times are now positive, although only 162 out of 490 clients
have non-missing times.

(Code for `time_to_first_ride_data_revised` generation available upon
request.)

``` r
time_to_first_ride_data_revised <- read.csv('time_to_first_ride_data_revised.csv') %>% select(-X)

time_to_first_ride_data_revised %>%
  summarize(`Average time to first ride` = mean(time_to_first_ride, na.rm = TRUE),
            `Median time to first ride` = median(time_to_first_ride, na.rm = TRUE))
```

    ##   Average time to first ride Median time to first ride
    ## 1                   60.47826                         0

``` r
time_to_first_ride_data_revised %>%
  ggplot(aes(time_to_first_ride)) +
  geom_density(color = 'green', fill = 'green', alpha = 0.3) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  labs(title = 'Figure 1: Distribution of times between enrollment and first ride',
       subtitle = 'n = 162, 328 / 490 clients missing',
       y = 'Density',
       x = 'Time to first ride (days)')
```

![](https://github.com/ghbutler/datathon2022/blob/main/fig1week1a.png?raw=true)<!-- -->

### Part 4: Revisiting `time_to_first_pantry_visit`

Using this new definition of `enrollment_date` to calculate
`time_to_first_pantry_visit` produces a dramatically different
distribution of times. The minimum value for for each time is zero,
which is something we should probably expect to see if we have actually
nailed down the date of the earliest interaction of each client with
ElderNet. All non-missing times are now positive, and only 414 out of
490 clients have non-missing times.

(Code for `time_to_first_pantry_visit` generation available upon
request.)

``` r
time_to_first_pantry_visit_data_revised <- read.csv('time_to_first_pantry_visit_data_revised.csv') %>% select(-X)

time_to_first_pantry_visit_data_revised %>%
  summarize(`Average time to first pantry visit` = mean(time_to_first_pantry_visit, na.rm = TRUE),
            `Median time to first pantry visit` = median(time_to_first_pantry_visit, na.rm = TRUE))
```

    ##   Average time to first pantry visit Median time to first pantry visit
    ## 1                           187.5149                                 0

``` r
time_to_first_pantry_visit_data_revised %>%
  ggplot(aes(time_to_first_pantry_visit)) +
  geom_density(color = 'blue', fill = 'blue', alpha = 0.3) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  labs(title = 'Distribution of times between enrollment and first pantry visit',
       subtitle = 'n = 414, 76 / 490 clients missing',
       y = 'Density',
       x = 'Time to first pantry visit (days)')
```

![](https://github.com/ghbutler/datathon2022/blob/main/fig2week1a.png?raw=true)<!-- -->

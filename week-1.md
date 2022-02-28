Team 1 Week 1 Questions/Insights
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

### Part 2: Some cleaning and assumptions

Each of the `Assistance_` and `Benefit_` variables have a unique value,
`""`, which seems to represent missing information. The unique values of
`Assistance_1` and `Benefit_1` are listed below to illustrate. I have
re-coded this value to `NA` (of character/string type) for all
`Assistance_` and `Benefit_` variables.

Additionally, as we can see below, one of the unique values of
`Benefit_1` is `Benefit_1`. I have also re-coded this to `NA` (of
character/string type) because this value seems to be a data entry
error.

The code I used to make these changes is shown in the third code chunk
below.

``` r
# listing unique values for Benefit_1 variable

unique(care_management_anonymized$Benefit_1)
```

    ##  [1] NA                  "Medical"           "Transportation"   
    ##  [4] "ElderNet"          "Food"              "Housing"          
    ##  [7] ""                  "Legal"             "Financial"        
    ## [10] "Social"            "Utilities"         "Safety"           
    ## [13] "ADL"               "Eldernet"          "Telecommunication"
    ## [16] "Pets"              "Benefit_1"         "Information"      
    ## [19] "Pet"

``` r
# listing unique values for Assignment_1 variable

unique(care_management_anonymized$Assistance_1)
```

    ##  [1] NA             "Information"  "Coordination" "Support"      "Filing"      
    ##  [6] ""             "Enrollment"   "Referral"     "Continuation" "Medical"     
    ## [11] "coordination" "Facilitation"

``` r
# cleaning up assistance_ and benefit_ variables

care_management_anonymized <- care_management_anonymized %>%
  mutate(Benefit_1 = ifelse(Benefit_1 == '', NA_character_, Benefit_1),
         Benefit_1 = ifelse(Benefit_1 == 'Benefit_1', NA_character_, Benefit_1),
         Benefit_2 = ifelse(Benefit_2 == '', NA_character_, Benefit_2),
         Benefit_3 = ifelse(Benefit_3 == '', NA_character_, Benefit_3),
         Assistance_1 = ifelse(Assistance_1 == '', NA_character_, Assistance_1),
         Assistance_2 = ifelse(Assistance_2 == '', NA_character_, Assistance_2),
         Assistance_3 = ifelse(Assistance_3 == '', NA_character_, Assistance_3))
```

### Part 3: Missing data trends in `care_management_anonymized.csv`

When glancing at the contents of `care_management_anonymized`, I noticed
that there seemed to be a lot of missing data. In particular, there
seemed to be only missing values for the following variables for the
first several thousand observations in this table.

``` r
names(care_management_anonymized)[6:14]
```

    ## [1] "CommType"     "Party"        "InitiatedBy"  "Benefit_1"    "Assistance_1"
    ## [6] "Benefit_2"    "Assistance_2" "Benefit_3"    "Assistance_3"

To get a more precise idea of how much data is missing, I calculated the
average proportion of missing observations for each of the variables
listed above in each of the available years.

The first figure below shows that in 2019, there is no information for
any of these variables in this table. However, the proportion of missing
observations for each variables drops considerably over time. The
proportion of missing observations for `Assistance_2`, `Assistance_3`,
`Benefit_2` and `Benefit_3` remain high.

However, this is probably because throughout most of 2020 and 2021, the
average interaction resulted in between 1 and 1.25 forms of assistance
and between 1 and 1.25 forms of benefits. This is illustrated in Figure
2 below.

In other words, I have concluded that `Assistance_` and `Benefit_`
variables are given non-missing values in numeric order. I.e., in a
typical interaction, a client will receive about one benefit and one
form of assistance, so non-missing values will be recorded for
`Assistance_1` and `Benefit_1`, but `Assistance_2`, `Assistance_3`,
`Benefit_2` and `Benefit_3` will all be given missing values because the
total number of forms of assistance and benefits that the client
received is less than two or three.

``` r
# summarizing proportion of missing observations for commtype, party, initiatedby, benefits/assistance on each assistance_date

missing_care_dat <- care_management_anonymized %>%
  group_by(assistance_date) %>%
  summarize(pct_missing = across(5:13, ~ sum(is.na(.x)) / n()))

# pasting information about missing data together with assistance_date values

missing_care_dat <- data.frame(assistance_date = missing_care_dat$assistance_date,
                               missing_care_dat$pct_missing)

# reshaping the above table from wide to long to make it compatible with visualization library ggplot2

missing_care_dat_long <- missing_care_dat %>%
  pivot_longer(names(missing_care_dat)[2:length(names(missing_care_dat))],
               names_to = 'variable',
               values_to = 'pct_missing')

# calculating average proportion of missing values for each care management variable

missing_care_dat_long %>%
  mutate(year = year(assistance_date)) %>%
  group_by(year, variable) %>%
  summarize(mean_pct_missing = mean(pct_missing)) %>%
  ggplot(aes(variable, mean_pct_missing)) +
  geom_col(color = 'black', aes(fill = variable)) +
  facet_wrap(~ year) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, vjust = -0.05),
        axis.title.x = element_text(vjust = -0.05),
        legend.position = 'none',
        plot.title = element_text(hjust = 0.5)) +
  labs(y = 'Mean proportion missing',
       x = 'Variable',
       title = 'Figure 1: Proportion of missing observations in care management data') +
  coord_flip()
```

![](https://github.com/ghbutler/datathon2022/blob/main/unnamed-chunk-6-1.png?raw=true)<!-- -->

``` r
# create columns to count the number of benefits and assistances(?) in each observation

care_management_anonymized <- care_management_anonymized %>%
  mutate(n_benefits = case_when(!is.na(Benefit_1) & !is.na(Benefit_2) & !is.na(Benefit_3) ~ 3,
                                !is.na(Benefit_1) & !is.na(Benefit_2) ~ 2,
                                !is.na(Benefit_1) ~ 1,
                                TRUE ~ NA_real_),
         n_assistance = case_when(!is.na(Assistance_1) & !is.na(Assistance_2) & !is.na(Assistance_3) ~ 3,
                                  !is.na(Assistance_1) & !is.na(Assistance_2) ~ 2,
                                  !is.na(Assistance_1) ~ 1,
                                  TRUE ~ NA_real_),
         year = year(assistance_date))

# create month/year variable

care_management_anonymized$assistance_date_my <- zoo::as.yearmon(care_management_anonymized$assistance_date)

# calculating average number of assistance types and average number of benefits per month-year and filtering out NA results

mean_benefits_and_assistance <- care_management_anonymized %>%
  group_by(assistance_date_my) %>%
  summarize(mean_assistance = mean(n_assistance, na.rm = TRUE),
            mean_benefits = mean(n_benefits, na.rm = TRUE)) %>%
  filter(!is.na(mean_assistance), !is.na(mean_benefits))

# visualizing trend in results

mean_benefits_and_assistance %>%
  ggplot() +
  geom_line(aes(assistance_date_my, 
                mean_assistance, 
                color = 'Number of assistance types'), 
            size = 1,
            linetype = 'dashed') +
  geom_line(aes(assistance_date_my, 
                mean_benefits, 
                color = 'Number of benefit types'), 
            size = 1,
            linetype = 'dotted') +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = 'top',
        legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  scale_y_continuous(limits = c(0, 1.5)) +
  scale_color_manual(values = c('red', 'blue')) +
  labs(title = 'Figure 2: Average number of assistance/benefit types per interaction by month',
       subtitle = 'April 2020 - June 2021')
```

![](https://github.com/ghbutler/datathon2022/blob/main/fig2week1.png?raw=true)<!-- -->

### Part 4: Attempting to find program enrollment dates for each client

As we saw in Part 2 above, one of the unique values of the `Assistance_`
variables is `Enrollment`. This means that we may be able to use these
variables to figure out the date on which each client enrolled in the
program. However, it appears that almost none of the clients have
interactions that resulted in this specific type of assistance.

``` r
care_management_anonymized %>%
  group_by(anon_ID) %>%
  summarize(n_enrollments = sum(ifelse(Assistance_1 == 'Enrollment' | Assistance_2 == 'Enrollment' | Assistance_3 == 'Enrollment',
                                       TRUE, FALSE))) %>%
  summarize(`# of clients with missing enrollment dates` = sum(is.na(n_enrollments)),
            `Prop. of clients with missing enrollment dates` = sum(is.na(n_enrollments)) / n())
```

    ## # A tibble: 1 x 2
    ##   `# of clients with missing enrollment dates` `Prop. of clients with missing ~`
    ##                                          <int>                             <dbl>
    ## 1                                          466                             0.951

Another possible approach is to find the earliest unique
`assistance_date` for each client. I have done this using the code
below. (I use `distinct()` at the end because there are a number of
clients who had multiple interactions on their first interaction date.)
This seems like a reasonable approach, at least for now, because every
client has a value for this variable.

``` r
care_management_anonymized <- care_management_anonymized %>%
  mutate(assistance_date = as.Date(assistance_date))

enrollment_dates <- care_management_anonymized %>%
  group_by(anon_ID) %>%
  slice_min(assistance_date) %>%
  select(anon_ID, enrollment_date = assistance_date) %>%
  distinct()

enrollment_dates %>%
  ungroup() %>%
  summarize(`# unique clients` = n_distinct(anon_ID),
            `# clients w/ missing enrollment dates` = sum(is.na(enrollment_date)))
```

    ## # A tibble: 1 x 2
    ##   `# unique clients` `# clients w/ missing enrollment dates`
    ##                <int>                                   <int>
    ## 1                490                                       0

### Part 5: Time between enrollment date and `rider_first_ride_date`

Using the data from above for enrollment dates, I calculated the
difference between `rider_first_ride_date` and `enrollment_date` for
each client. About 30% of clients did not have this information
available, perhaps because they have never used this service.

A plot of the distribution of times between enrollment date as it is
defined above and date of first ride is shown in Figure 3 below. As we
can see, a large majority of non-missing ride times are negative. This
raises a few questions.

-   Does this distribution of times seem sensible?
-   Is there something wrong with the definition of `enrollment_date`
    here?
-   What are some other possible definitions of `enrollment_date` that
    we should try?
    -   E.g., merge all datasets and use earliest interaction date as
        enrollment date.

``` r
# merging volunteer services data with enrollment dates data in order to calculate time to first ride

time_to_first_ride_data <- volunteer_services_anonymized %>%
  mutate(rider_first_ride_date = as.Date(rider_first_ride_date)) %>%
  left_join(., enrollment_dates) %>%
  select(anon_ID, rider_first_ride_date, enrollment_date) %>%
  distinct() %>%
  mutate(time_to_first_ride = rider_first_ride_date - enrollment_date)

time_to_first_ride_data %>%
  summarize(`# of clients w/o first ride times` = sum(is.na(time_to_first_ride)),
            `Prop. of clients w/o first ride times` = sum(is.na(time_to_first_ride)) / n())
```

    ##   # of clients w/o first ride times Prop. of clients w/o first ride times
    ## 1                                47                             0.2901235

``` r
time_to_first_ride_data %>%
  ggplot(aes(time_to_first_ride)) +
  geom_density(color = 'green', fill = 'green', alpha = 0.3) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(title = 'Figure 3: Distribution of times between enrollment date and first ride date',
       y = 'Density',
       x = 'Time to first ride (days)')
```

![](https://github.com/ghbutler/datathon2022/blob/main/unnamed-chunk-10-1.png?raw=true)<!-- -->

### Part 6: Time between enrollment date and first pantry visit

Continuing with the same enrollment date data, I calculated the
difference between first pantry visit date and enrollment date,
`time_to_first_pantry_visit`. I defined `first_pantry_visit` (a date
variable) as the earliest value for `assistance_date` for each client if
it was available. A value for `first_pantry_visit` as it is defined here
was available for 303 clients and unavailable for 111 clients.

A plot of the distribution of values for `time_to_first_pantry_visit` is
shown below. These times are more evenly distributed between positive
and negative than the `time_to_first_ride` values were.

I have the same question about these results as I did in Part 5.

``` r
# merging pantry data with enrollment dates data

pantry_anonymized <- pantry_anonymized %>%
  mutate(assistance_date = as.Date(assistance_date, '%m/%d/%Y'))

pantry_anonymized <- pantry_anonymized %>%
  group_by(anon_ID) %>%
  mutate(first_pantry_visit = ifelse(assistance_date == min(assistance_date), assistance_date, NA_Date_))

pantry_anonymized$first_pantry_visit <- as.Date(pantry_anonymized$first_pantry_visit, origin = '1970-01-01')

# calculating time to first pantry visit for each client

time_to_first_pantry_visit_data <- pantry_anonymized %>%
  left_join(., enrollment_dates) %>%
  select(anon_ID, first_pantry_visit, enrollment_date) %>%
  distinct() %>%
  mutate(time_to_first_pantry_visit = first_pantry_visit - enrollment_date) %>%
  filter(!is.na(time_to_first_pantry_visit))

# plotting distribution of times

time_to_first_pantry_visit_data %>%
  ggplot(aes(time_to_first_pantry_visit)) +
  geom_density(color = 'blue', fill = 'blue', alpha = 0.3) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  labs(title = 'Figure 4: Distribution of times between enrollment and first pantry visit',
       subtitle = '# clients = 303, 111 clients missing',
       y = 'Density',
       x = 'Time to first pantry visit (days)')
```

![](https://github.com/ghbutler/datathon2022/blob/main/unnamed-chunk-11-1.png?raw=true)<!-- -->

### Part 7: Conclusion

I am unsatisfied with the results I have gotten with my current
definition of `enrollment_date`, so I’m going to try calculating a
global minimum date for each client and recalculate my results from Part
5 and Part 6 using it. I will report these results in a separate
document.

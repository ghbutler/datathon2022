Team 1 Week 2 Questions/Insights
================
Gabriel Butler
2/24/2022

### Part 1: Loading data and libraries

``` r
library(ggalluvial)
library(tidyverse)

care_management_anonymized <- read.csv('care_management_anonymized.csv') %>% select(-X)
```

### Part 2: Tabulating `Benefit_1` data

I decided to focus on `Benefit_1` because as I found [last
week](https://github.com/ghbutler/datathon2022/blob/main/week-1.md), a
very large majority of data is missing for `Benefit_2` and `Benefit_3`
because across the period for which data is available for all of the
variables, the average client only has about 1.2 interactions with
ElderNet. This means that more often than not, there is nothing to
report for these variables.

In order to construct an [alluvial
plot](https://cran.r-project.org/web/packages/ggalluvial/vignettes/ggalluvial.html),
I created a variable called `nth_assistance_interaction` to count in
sequence the number of interactions each unique client had with ElderNet
in the `care_management_anonymized` dataset. I also filtered out all
observations with missing values for `Benefit_1` so that `NA` would not
be among the interaction types.

The snapshot of the results of my calculations below show that for
client `5`, their first interaction with ElderNet that was recorded in
`care_management_anonymized` was on `2020-04-14` and the benefit they
received was `ElderNet`. We know this was the first interaction because
the value for `nth_assistance_interaction` in this observation is `1`
and because it is the earliest of the two dates for which `Benefit_1`
has a non-missing value. Their second interaction with ElderNet that was
recorded in this dataset was on `2020-09-01` and the benefit they
received was `Housing`. We know this was the second interaction because
the value for `nth_assistance_interaction` in this observation is `2`.

``` r
benefit_1_tabulation <- care_management_anonymized %>%
  arrange(anon_ID) %>%
  select(anon_ID, assistance_date, Benefit_1) %>%
  group_by(anon_ID) %>%
  filter(!is.na(Benefit_1)) %>%
  mutate(nth_assistance_interaction = sequence(n()))

benefit_1_tabulation %>% head()
```

    ## # A tibble: 6 x 4
    ## # Groups:   anon_ID [4]
    ##   anon_ID assistance_date Benefit_1 nth_assistance_interaction
    ##     <int> <chr>           <chr>                          <int>
    ## 1       2 2020-05-01      ElderNet                           1
    ## 2       3 2020-04-10      ElderNet                           1
    ## 3       5 2020-04-14      ElderNet                           1
    ## 4       5 2020-09-01      Housing                            2
    ## 5       8 2020-04-03      ElderNet                           1
    ## 6       8 2020-04-07      Safety                             2

By finding the maximum value for `nth_assistance_interaction` for each
client, we get the total number of interactions between a unique client
and ElderNet. The distribution of number of interactions with ElderNet
only covers 2020 and 2021 because as I found [last
week](https://github.com/ghbutler/datathon2022/blob/main/week-1.md),
there is almost no data in `care_management_anonymized` for 2019. The
plot below shows that for clients with `Benefit_1` data available, a
very large majority have had less than 50 `Benefit_1` interactions with
ElderNet.

``` r
benefit_1_tabulation %>%
  group_by(anon_ID) %>%
  summarize(max_n_interactions = max(nth_assistance_interaction)) %>%
  ggplot(aes(max_n_interactions)) +
  geom_density(color = 'black', fill = 'black', alpha = 0.3) +
  theme_bw() +
  labs(x = 'Total Benefit_1 interactions',
       y = 'Density',
       title = 'Figure 1: Distribution of total # of interactions with ElderNet in 2020 & 2021',
       subtitle = '299 / 490 clients') +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))
```

![](https://github.com/ghbutler/datathon2022/blob/main/unnamed-chunk-3-1.png?raw=true)<!-- -->

### Part 3: Alluvial plot of `Benefit_1` interactions for clients with up to 5 interactions

An alluvial plot for unique clients with up to five interactions with
ElderNet below indicates that the most popular type of `Benefit_1` is
`ElderNet`, but it is unclear what this value refers to. This is
troubling because this is a benefit that many clients return repeatedly
for and because it is the most popular type of benefit. My guess is that
`ElderNet` is a data entry error and this value corresponds to any one
of the other unique values in each observation.

The plot also suggests that during the period for which data is
available, there is a lot of client attrition from ElderNet: more than
half of the clients who had one interaction did not go on to have a
fifth one.

One question about these results:

-   What do we do about the `ElderNet` value for `Benefit_1`?
    -   We could exclude it to make the alluvial plot more readable
    -   This may be fine as long as we are transparent about what the
        plot shows and what has been excluded

``` r
benefit_1_tabulation %>%
  filter(nth_assistance_interaction <= 5) %>%
  ggplot(aes(x = nth_assistance_interaction, 
             stratum = Benefit_1, 
             alluvium = anon_ID,
             fill = Benefit_1,
             label = Benefit_1)) +
  geom_flow(stat = 'alluvium', lode.guidance = 'frontback') +
  geom_stratum() +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  labs(x = 'Assistance interaction',
       y = 'Number of clients',
       title = 'Figure 2: ElderNet Client Interactions')
```

![](https://github.com/ghbutler/datathon2022/blob/main/unnamed-chunk-4-1.png?raw=true)<!-- -->

This is what Figure 2 looks like with the `Benefit_1` = `ElderNet`
observations omitted. It is much easier to interpret: the plot indicates
that for this subset of observations, after their initial interaction
with ElderNet, most clients continue to interact with ElderNet. Only
about 20% of clients with one interaction fail to go on to have a fifth
one during the period for which data is available. And between the first
and fifth interactions, some clients use a variety of services. But it
is more popular for clients to return repeatedly for the same needs,
with `Financial`, `Food`, `Housing`, `Legal` and `Medical` consistently
being relatively popular. This is much harder to see in Figure 2 because
`ElderNet` crowds out everything else.

Questions about Figure 3:

-   How can we improve this plot?
    -   Would it be useful to make two different ones for 2020 and 2021?

``` r
care_management_anonymized %>%
  arrange(anon_ID) %>%
  select(anon_ID, assistance_date, Benefit_1) %>%
  group_by(anon_ID) %>%
  filter(!is.na(Benefit_1), Benefit_1 != 'ElderNet') %>%
  mutate(nth_assistance_interaction = sequence(n())) %>%
  filter(nth_assistance_interaction <= 5) %>%
  ggplot(aes(x = nth_assistance_interaction, 
             stratum = Benefit_1, 
             alluvium = anon_ID,
             fill = Benefit_1,
             label = Benefit_1)) +
  geom_flow(stat = 'alluvium', lode.guidance = 'frontback') +
  geom_stratum() +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  labs(x = 'Assistance interaction',
       y = 'Number of clients',
       title = 'Figure 3: ElderNet Client Interactions',
       subtitle = 'Benefit_1 = ElderNet observations omitted, 220 / 490 clients at start')
```

![](https://github.com/ghbutler/datathon2022/blob/main/unnamed-chunk-5-1.png?raw=true)<!-- -->

### Part 4: Clients with 5 or more interactions

The strong right skew of Figure 1 indicates that there is a subset of
ElderNet clients who have been relatively heavy users of ElderNet
services over the period for which complete data is available. A closer
look at the `Benefit_1` data shows that when `Benefit_1` = `ElderNet`
observations are included, a significant proportion of clients have 5 or
more, 10 or more, 15 or more or even 20 or more interactions with
ElderNet. The greatest number of total interactions was 211.

``` r
benefit_1_tabulation %>%
  group_by(anon_ID) %>%
  summarize(max_n_interactions = max(nth_assistance_interaction)) %>%
  summarize(`5+ interactions` = sum(max_n_interactions >= 5) / 490,
            `10+ interactions` = sum(max_n_interactions >= 10) / 490,
            `15+ interactions` = sum(max_n_interactions >= 15) / 490,
            `20+ interactions` = sum(max_n_interactions >= 20) / 490)
```

    ## # A tibble: 1 x 4
    ##   `5+ interactions` `10+ interactions` `15+ interactions` `20+ interactions`
    ##               <dbl>              <dbl>              <dbl>              <dbl>
    ## 1             0.282                0.2              0.157              0.124

An alluvial plot of interactions between these power users and ElderNet
(with `Benefit_1` = `ElderNet` observations omitted) shows that for the
fifth, tenth and fifteenth interactions between clients and ElderNet,
the most popular `Benefit_1` categories are still `Financial`, `Food`,
`Housing` and `Medical`. By the 20th interaction, `Food` and `Financial`
basically disappear, but `Housing` and `Medical` remained popular on the
20th interaction.

``` r
care_management_anonymized %>%
  arrange(anon_ID) %>%
  select(anon_ID, assistance_date, Benefit_1) %>%
  group_by(anon_ID) %>%
  filter(!is.na(Benefit_1), Benefit_1 != 'ElderNet') %>%
  mutate(nth_assistance_interaction = sequence(n())) %>%
  group_by(anon_ID) %>%
  filter(nth_assistance_interaction %in% c(5, 10, 15, 20)) %>%
  ggplot(aes(x = as.factor(nth_assistance_interaction), 
             stratum = Benefit_1, 
             alluvium = anon_ID,
             fill = Benefit_1,
             label = Benefit_1)) +
  geom_flow(stat = 'alluvium', lode.guidance = 'frontback') +
  geom_stratum() +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  labs(x = 'Assistance interaction',
       y = 'Number of clients',
       title = 'Figure 4: ElderNet Power Client Interactions',
       subtitle = 'Benefit_1 = ElderNet observations omitted, 111 / 490 clients at start')
```

![](https://github.com/ghbutler/datathon2022/blob/main/unnamed-chunk-7-1.png?raw=true)<!-- -->

### Part 5: Conclusion

In the `client_management_anonymized` dataset, we only have complete,
detailed data for at least one `Benefits_1` interaction for 220 unique
clients. Data about these interactions is only available for part of
2020 and 2021, and there is no data at all for 2019.

That said, for this subset of 220 unique clients, the most popular types
of benefits in the first five `Benefits_1` interactions are `Financial`,
`Food`, `Housing`, `Legal` and `Medical`. 4/5 of these benefit types
seem like they address acute needs that if otherwise left unaddressed
could result in significant calamity for the client.

About 20% of clients had 10 or more `Benefit_1` interactions with
ElderNet over the period of observation. On the 10th interaction, the
three most popular benefit types were `Financial`, `Housing` and
`Medical`. On the 20th interaction, `Housing` and `Medical` were the
most popular benefit types by a wide margin.

Clients with a relatively high total number of interactions with
ElderNet seem to be more dependent on these services, especially for
assistance with housing and medical needs. It is likely that these are
ElderNet’s most vulnerable clients, and that without access to these
services, these clients may not be able to live independently.

Finally, it is important to emphasize that this is just a snapshot of
ElderNet’s benefit interactions over a limited period of time. Clients
who have had fewer interactions with ElderNet in the past may have gone
on to have more as their needs have changed, and ElderNet has likely
taken on plenty of new clients since this data was released.

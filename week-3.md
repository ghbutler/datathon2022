Team 1 Week 3 Questions/Insights
================
Gabriel Butler
3/5/2022

``` r
library(ggalluvial)
library(tidyverse)

care_management_anonymized <- read.csv('care_management_anonymized.csv') %>% select(-X)
```

### Part 1: Some measurements of interaction sequences and intensity

This is my (incomplete) attempt at Katherine’s second question from the
pathways channel.

> If a client’s interaction starts with one service, what is the
> probability of them returning to that service or a different service
> type?

First, I created a wide table with five columns, one for each of the
first five interactions a client has had with ElderNet. As part of this
process, I re-coded any value for `Benefit_1` that was not `Financial`,
`Food`, `Housing` or `Medical` as `Other`. I collapsed all of the unique
benefit categories into these five because as I found [last
week](https://github.com/ghbutler/datathon2022/blob/main/week-2.md) in
Figure 3, these are the most popular types of `Benefit_1` values for
these early interactions.

``` r
`%nin%` <- Negate(`%in%`)

# creating wide table with columns for first five interactions by anon_ID

first_5_benefit_1_wide <- care_management_anonymized %>%
  arrange(anon_ID) %>%
  select(anon_ID, assistance_date, Benefit_1) %>%
  group_by(anon_ID) %>%
  filter(!is.na(Benefit_1), Benefit_1 != 'ElderNet') %>%
  mutate(nth_assistance_interaction = sequence(n()),
         Benefit_1 = case_when(Benefit_1 %nin% c('Financial', 'Food', 'Housing', 'Medical') ~ 'Other',
                               Benefit_1 == 'Financial' ~ 'Financial',
                               Benefit_1 == 'Food' ~ 'Food',
                               Benefit_1 == 'Housing' ~ 'Housing',
                               Benefit_1 == 'Medical' ~ 'Medical',
                               TRUE ~ NA_character_)) %>%
  filter(nth_assistance_interaction <= 5) %>%
  select(-assistance_date) %>%
  pivot_wider(names_from = nth_assistance_interaction, values_from = Benefit_1)

# renaming columns of the above result

names(first_5_benefit_1_wide) <- c('anon_ID',
                                   'first',
                                   'second',
                                   'third',
                                   'fourth',
                                   'fifth')

# preview of result

first_5_benefit_1_wide %>% head()
```

    ## # A tibble: 6 x 6
    ## # Groups:   anon_ID [6]
    ##   anon_ID first     second    third   fourth    fifth  
    ##     <int> <chr>     <chr>     <chr>   <chr>     <chr>  
    ## 1       5 Housing   <NA>      <NA>    <NA>      <NA>   
    ## 2       8 Other     Other     Medical Other     Medical
    ## 3      11 Housing   Other     Medical Financial Housing
    ## 4      15 Financial Financial Housing <NA>      <NA>   
    ## 5      20 Food      <NA>      <NA>    <NA>      <NA>   
    ## 6      22 Housing   Housing   <NA>    <NA>      <NA>

Next, I created some new variables to test whether or not sequences of
interactions with ElderNet were identical. For example,
`first_equals_second` tests whether or not `first` and `second` have the
same values. The snapshot of the underlying table above indicates that
for `anon_ID` = `15`, `first_equals_second` should be `TRUE` because for
this client, `first` = `second` = `Financial`. The next variable,
`first_three_equal` does the same thing, but for the first three
interactions. This is repeated until the full sequence of the first five
interactions with ElderNet are tested. I then calculated the proportions
of consecutive identical interactions by first interaction, and I
omitted `Other` from the plot results. These interactions are still
present in the underlying data.

The Financial panel in Figure 1 shows which proportion of observations
with `Financial` as the first type of `Benefit_1` interaction also had
`Financial` for the second, third, fourth and fifth interactions. About
half of clients whose first `Benefit_1` interaction with ElderNet is
`Financial` go back for the same kind of `Benefit_1` value in their
second interaction. A little over a quarter return for the same type of
help in their third interaction.

The results suggest that it is fairly common for ElderNet clients who
have more than one `Benefit_1` interaction to return for the same exact
benefit the second time. But a substantial majority of clients who
return three or more times are back for a `Benefit_1` value that is
different from the one in their first interaction.

Note that the results include clients who had less than five total
interactions with ElderNet.

``` r
first_5_benefit_1_wide %>%
  mutate(first_equals_second = first == second,
         first_three_equal = first == second & second == third,
         first_four_equal = first == second & second == third & third == fourth,
         all_five_equal = first == second & second == third & third == fourth & fourth == fifth) %>%
  ungroup() %>%
  filter(first != 'Other') %>%
  group_by(first) %>%
  summarize(`2` = sum(first_equals_second, na.rm = TRUE) / n(),
            `3` = sum(first_three_equal, na.rm = TRUE) / n(),
            `4` = sum(first_four_equal, na.rm = TRUE) / n(),
            `5` = sum(all_five_equal, na.rm = TRUE) / n()) %>%
  pivot_longer(2:5) %>%
  ggplot(aes(name, value)) +
  geom_col(color = 'black') +
  scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
  facet_wrap(~ first) +
  theme_bw() +
  theme(axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  labs(x = 'Number of consecutive identical interactions',
       title = 'Figure 1A: Percentage of consecutive \nidentical interactions by first interaction',
       subtitle = '220 / 490 clients, including clients with <5 interactions')
```

![](week-3_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

``` r
first_5_benefit_1_wide %>%
  filter_at(2:6, all_vars(. %in% c('Financial', 'Food', 'Housing', 'Medical', 'Other'))) %>%
  mutate(first_equals_second = first == second,
         first_three_equal = first == second & second == third,
         first_four_equal = first == second & second == third & third == fourth,
         all_five_equal = first == second & second == third & third == fourth & fourth == fifth) %>%
  ungroup() %>%
  filter(first != 'Other') %>%
  group_by(first) %>%
  summarize(`2` = sum(first_equals_second, na.rm = TRUE) / n(),
            `3` = sum(first_three_equal, na.rm = TRUE) / n(),
            `4` = sum(first_four_equal, na.rm = TRUE) / n(),
            `5` = sum(all_five_equal, na.rm = TRUE) / n()) %>%
  pivot_longer(2:5) %>%
  ggplot(aes(name, value)) +
  geom_col(color = 'black') +
  scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
  facet_wrap(~ first) +
  theme_bw() +
  theme(axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  labs(x = 'Number of consecutive identical interactions',
       title = 'Figure 1B: Percentage of consecutive \nidentical interactions by first interaction',
       subtitle = '111 / 490 clients, including clients with at least 5 total interactions')
```

![](week-3_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

The next figure is an incomplete attempt at answering Katherine
McAulay’s first question in the pathways channel.

> How consistent are clients’ interactions with ElderNet over time? Do
> any specific interactions correlate with consistent use or gaps in use
> of ElderNet services?

Figure 1C is similar to the two figures before it, except it plots the
percentage of non-missing interactions by first interaction with
ElderNet. There are clearly some differences in the persistence of
interactions with ElderNet depending on why contact with ElderNet is
initiated. Clients who seek `Financial` or especially `Medical`
assistance as their first `Benefit_1` interaction tend to have a more
intense relationship with ElderNet (measured in total interactions over
the period of observation) than those who seek `Housing` or `Food`.

One thing missing from the figure below is the composition of the
different interaction types. This figure only shows whether or not there
were clients with a certain number of interactions as that number
increases.

``` r
# creating a wide table that tests whether or not the nth interaction has a missing value for a client

very_wide_benefits <- care_management_anonymized %>%
  arrange(anon_ID) %>%
  select(anon_ID, assistance_date, Benefit_1) %>%
  group_by(anon_ID) %>%
  filter(!is.na(Benefit_1), Benefit_1 != 'ElderNet') %>%
  mutate(nth_assistance_interaction = sequence(n()),
         Benefit_1 = case_when(Benefit_1 %nin% c('Financial', 'Food', 'Housing', 'Medical') ~ 'Other',
                               Benefit_1 == 'Financial' ~ 'Financial',
                               Benefit_1 == 'Food' ~ 'Food',
                               Benefit_1 == 'Housing' ~ 'Housing',
                               Benefit_1 == 'Medical' ~ 'Medical',
                               TRUE ~ NA_character_)) %>%
  select(-assistance_date) %>%
  pivot_wider(names_from = nth_assistance_interaction, values_from = Benefit_1)

very_wide_benefits <- very_wide_benefits %>%
  ungroup() %>%
  mutate(across(names(very_wide_benefits)[2:length(names(very_wide_benefits))], is.na, .names = 'na_{col}'))

# reshaping above result

very_long_benefits <- very_wide_benefits %>%
  ungroup() %>%
  filter(`1` != 'Other') %>%
  group_by(`1`) %>%
  summarize(across(starts_with('na_'), ~ sum(.x == FALSE) / n())) %>%
  select(-na_1) %>%
  pivot_longer(2:ncol(.),
               names_to = 'nth_assistance_interaction',
               values_to = 'pct_non_missing')

names(very_long_benefits)[1] <- 'first'

very_long_benefits$nth_assistance_interaction <- as.numeric(gsub('na_', '', very_long_benefits$nth_assistance_interaction))

# plotting results

very_long_benefits %>%
  filter(nth_assistance_interaction <= 50) %>%
  ggplot(aes(nth_assistance_interaction, pct_non_missing)) +
  geom_col(color = 'black') +
  scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
  facet_wrap(~ first) +
  theme_bw() +
  theme(axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  labs(x = 'Number of interactions',
       title = 'Figure 1C: Percentage of non-missing \ninteractions by first interaction',
       subtitle = '220 / 490 clients') +
  scale_x_continuous(breaks = c(2, seq(10, 50, 10)))
```

![](week-3_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

### Part 2: Alluvial plot and stacked bar plot of power client interactions with ElderNet

One way of making an alluvial plot that is relatively easy to read is to
focus on a relatively small number of clients who use ElderNet benefits
much more often than most users. The median number of interactions in
`care_management_anonymized` is 5 and the mean is almost 11. Only 21
clients have 30 or more interactions with ElderNet, and when we use this
subset of clients to create an alluvial plot, it is much easier to see
how each client interacts with the service across visits. One caveat,
however, is that this flow plot only updates once every ten
interactions. A lot of information is lost between updates, which may
make it a lot less informative.

Nevertheless, it is clear that for clients who appear to rely a lot on
ElderNet, `Medical` looks like it might be the most valuable service of
all, because it’s one that all four clients with a 60th interaction were
using in that interaction, and because it accounted for a relatively
large proportion of interactions on the 50th, 40th and 30th interactions
as well.

``` r
care_management_anonymized %>%
  arrange(anon_ID) %>%
  select(anon_ID, assistance_date, Benefit_1) %>%
  group_by(anon_ID) %>%
  filter(!is.na(Benefit_1), Benefit_1 != 'ElderNet') %>%
  mutate(nth_assistance_interaction = sequence(n())) %>%
  summarize(max_assistance_interaction = max(nth_assistance_interaction)) %>%
  summarize(`Median # of interactions` = median(max_assistance_interaction),
            `Mean # of interactions` = mean(max_assistance_interaction),
            `# of clients with 30+ interactions` = sum(max_assistance_interaction >= 30))
```

    ## # A tibble: 1 x 3
    ##   `Median # of interactions` `Mean # of interactions` `# of clients with 30+ i~`
    ##                        <dbl>                    <dbl>                      <int>
    ## 1                          5                     10.9                         21

``` r
#

care_management_anonymized %>%
  arrange(anon_ID) %>%
  select(anon_ID, assistance_date, Benefit_1) %>%
  group_by(anon_ID) %>%
  filter(!is.na(Benefit_1), Benefit_1 != 'ElderNet') %>%
  mutate(nth_assistance_interaction = sequence(n())) %>%
  filter(nth_assistance_interaction %in% seq(30, 60, 10)) %>%
  ggplot(aes(x = as.factor(nth_assistance_interaction), 
             stratum = Benefit_1, 
             alluvium = anon_ID,
             fill = Benefit_1,
             label = Benefit_1)) +
  geom_flow(stat = 'alluvium', 
            lode.guidance = 'forward',
            color = 'black') +
  geom_stratum() +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  labs(x = 'Assistance interaction',
       y = 'Number of clients',
       title = 'Figure 2: ElderNet Power Client Interactions',
       subtitle = '21 / 490 clients')
```

![](week-3_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

A bar plot for all interactions numbered between 30 and 60 inclusive
shows that over this domain, the most popular `Benefit_1` values are
usually `Housing` and `Medical`, highlighting the overall importance of
these `Benefit_1` types for “power clients”. One advantage of the bar
plot is that it shows the composition of all interactions. One
disadvantage is it doesn’t show how individual client interactions
change over time.

``` r
care_management_anonymized %>%
  arrange(anon_ID) %>%
  select(anon_ID, assistance_date, Benefit_1) %>%
  group_by(anon_ID) %>%
  filter(!is.na(Benefit_1), Benefit_1 != 'ElderNet') %>%
  mutate(nth_assistance_interaction = sequence(n())) %>%
  filter(nth_assistance_interaction %in% seq(30, 60)) %>%
  ggplot(aes(x = nth_assistance_interaction,
             fill = Benefit_1)) +
  geom_bar(color = 'white') +
  scale_x_continuous(breaks = seq(30, 60, 5)) +
  theme_bw() +
  theme(axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.position = 'bottom',
        legend.title = element_blank()) +
  labs(x = 'Assistance interaction',
       title = 'Figure 3: ElderNet Power Client Interactions',
       subtitle = '21 / 490 clients')
```

![](week-3_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

### Part 3: Some questions/comments

Question about showing interaction probabilities:

-   Should we try to show the probabilities of sequences of different
    types of interactions other than the ones shown in Figure 1A-C?
    -   I guess someone has probably already tried and has interesting
        results, but I am having trouble imagining other types of
        interaction sequences that would be easy to calculate and
        interpret.

Question about Figure 2:

-   Given the missing interactions between each of the bars in Figure 2,
    might it be better to just use a stacked bar plot instead of an
    alluvial plot?

Question about bar plots vs. alluvial plots:

-   Might there still be some value in showing a ‘busier’ alluvial plot
    like [Figure 3 from last
    week](https://github.com/ghbutler/datathon2022/blob/main/week-2.md)
    together with Figure 1 from this week?
    -   Last week’s Figure 3 is a glimpse of how early interactions with
        ElderNet unfold: there is a lot of continuity across the first
        five interactions, but also some discontinuity. But the figure
        is difficult to read and interpret on its own.
    -   Figure 1A-C from this week may complement Figure 3 from last
        week because it makes certain things that the latter figure is
        supposed to show clearer and easier to interpret.

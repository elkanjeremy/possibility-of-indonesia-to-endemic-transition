COVID-19 From Pandemic to Endemic Transition in Indonesia: A Review
================

A Short Analysis by: Elkan Jeremy Ishak

# Introduction

It’s been two years since the WHO officially declared COVID-19 as a
global pandemic and countries still battling this never ending disease.
World’s economy has been impacted for the last two years, including
Indonesia. Business disrupted. Lay-off everywhere. One question arises:
When will it end?

Now, as the latest wave of infections driven by the fast-spreading
Omicron variant rapidly subsides, many are beginning to question: Is
COVID-19 becoming endemic?

Indonesia’s closest neighbors have declared that a “transition to
endemic” phase will be implemented. Malaysia, starting 1 April 2022,
will begin the transition. Followed with Thailand in 1 July 2022. How
about Indonesia? Are we ready for this life-changing challenge? In this
short analysis, I’m going to present about Indonesia’s readiness for
implementing the transition of pandemic.

There are three key aspects that will be explored and discussed:
-Vaccination effects on Case Fatality Rate (CFR) in Indonesia -Severity
of Delta vs Omicron variant -Indonesia’s CFR comparison with neighboring
countries

Notice that CFR is mentioned many times. In short, CFR is a parameter to
measure of how ‘severe’ is a disease by comparing the total of death
caused by an infection with the total cases of an infection. Details
will be presented later on.

# Environment Setup, Data Retrieval, and Data Processing

## Import Libraries

``` r
library(dplyr) 
library(readr)
library(tidyr)
library(ggplot2)
library(plotly)
```

Dataset is retrieved from the COVID-19 data from John Hopkins
University.

URL:
<https://www.kaggle.com/datasets/antgoldbloom/covid19-data-from-john-hopkins-university>

**Updated on: 21 March 2022**

PS: All scripts in this document are designed to handle any future
dataset updates from the URL above. Replace files in the ‘./Dataset/’
path with newest downloaded files from the URL.

## Import Datasets

``` r
confirmed <- read_csv('Dataset/CONVENIENT_global_confirmed_cases.csv') #Total recorded COVID-19 cases from day to day in every country
```

    ## New names:
    ## * Australia -> Australia...11
    ## * Australia -> Australia...12
    ## * Australia -> Australia...13
    ## * Australia -> Australia...14
    ## * Australia -> Australia...15
    ## * ...

    ## Rows: 789 Columns: 285
    ## -- Column specification --------------------------------------------------------
    ## Delimiter: ","
    ## chr  (90): Country/Region, Australia...11, Australia...12, Australia...13, A...
    ## dbl (195): Afghanistan, Albania, Algeria, Andorra, Angola, Antarctica, Antig...
    ## 
    ## i Use `spec()` to retrieve the full column specification for this data.
    ## i Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
deaths <- read_csv('Dataset/CONVENIENT_global_deaths.csv') #Total recorded COVID-19 deaths from day to day in every country
```

    ## New names:
    ## * Australia -> Australia...11
    ## * Australia -> Australia...12
    ## * Australia -> Australia...13
    ## * Australia -> Australia...14
    ## * Australia -> Australia...15
    ## * ...
    ## Rows: 789 Columns: 285-- Column specification --------------------------------------------------------
    ## Delimiter: ","
    ## chr  (90): Country/Region, Australia...11, Australia...12, Australia...13, A...
    ## dbl (195): Afghanistan, Albania, Algeria, Andorra, Angola, Antarctica, Antig...
    ## i Use `spec()` to retrieve the full column specification for this data.
    ## i Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
vaccine <- read_csv('Dataset/vaccine.csv')
```

    ## Rows: 169939 Columns: 3
    ## -- Column specification --------------------------------------------------------
    ## Delimiter: ","
    ## chr (2): location, date
    ## dbl (1): people_fully_vaccinated_per_hundred
    ## 
    ## i Use `spec()` to retrieve the full column specification for this data.
    ## i Specify the column types or set `show_col_types = FALSE` to quiet this message.

## Datasets Cleaning

Dataset cleaning task for `confirmed` and `deaths`:

  - It is found that Australia, Canada, China, Denmark, France,
    Netherlands, New Zealand, and United Kingdom each has more than 1
    column (each province/region/state are separated in the dataset) and
    R automatically rename their column names. Specific data on each
    province/ region are not needed. Hence, we need first calculate the
    sum cases of those countries.
  - ‘Province/State’ row is not needed.
  - Rename ‘Country/Region’ column name to ‘Time’
  - Convert ‘Time’ column (which contains date) from character to date
    datatype.
  - Countries name are a variable which should be in one column.
    `pivot_longer` should be done to compact countries name into one
    column.

<!-- end list -->

``` r
confirmed <- confirmed %>%
  filter(`Country/Region` != 'Province/State') %>% #Remove 'Province/State row'
  mutate(`Country/Region` = as.Date(`Country/Region`, format='%m/%d/%y')) %>% #Convert to Date format
  rename(Time = `Country/Region`) %>%
  mutate_if(is.character, as.numeric) %>% #convert all columns (except Time) to numeric
  rowwise() %>%
  mutate(Australia = sum(across(starts_with("Australia")))) %>% #Sum Australia...11, Australia...12, ..., Australia...18 to Australia
  mutate(Canada = sum(across(starts_with("Canada")))) %>%
  mutate(Denmark = sum(across(starts_with("Denmark")))) %>%
  mutate(China = sum(across(starts_with("China")))) %>%
  mutate(France = sum(across(starts_with("France")))) %>%
  mutate(Netherlands = sum(across(starts_with("Netherlands")))) %>%
  mutate(`New Zealand` = sum(across(starts_with("New Zealand")))) %>%
  mutate(`United Kingdom` = sum(across(starts_with("United Kingdom")))) %>%
  select(-(Australia...11:Australia...18)) %>%
  select(-(Canada...42:Canada...57)) %>%
  select(-(China...61:China...94)) %>%
  select(-(Denmark...105:Denmark...107)) %>%
  select(-(France...122:France...133)) %>%
  select(-(Netherlands...196:Netherlands...200)) %>%
  select(-(`New Zealand...201`:`New Zealand...202`)) %>%
  select(-(`United Kingdom...262`:`United Kingdom...275`)) %>%
  pivot_longer(!Time, names_to='Country', values_to='ConfirmedCase') %>% #Compact data
  arrange(Country) #Sort by country's name
```

``` r
head(confirmed)
```

    ## # A tibble: 6 x 3
    ##   Time       Country     ConfirmedCase
    ##   <date>     <chr>               <dbl>
    ## 1 2020-01-23 Afghanistan             0
    ## 2 2020-01-24 Afghanistan             0
    ## 3 2020-01-25 Afghanistan             0
    ## 4 2020-01-26 Afghanistan             0
    ## 5 2020-01-27 Afghanistan             0
    ## 6 2020-01-28 Afghanistan             0

``` r
deaths <- deaths %>%
  filter(`Country/Region` != 'Province/State') %>% #Remove 'Province/State row'
  mutate(`Country/Region` = as.Date(`Country/Region`, format='%m/%d/%y')) %>% #Convert to Date format
  rename(Time = `Country/Region`) %>%
  mutate_if(is.character, as.numeric) %>% #convert all columns (except Time) to numeric
  rowwise() %>%
  mutate(Australia = sum(across(starts_with("Australia")))) %>% #Sum Australia...11, Australia...12, ..., Australia...18 to Australia
  mutate(Canada = sum(across(starts_with("Canada")))) %>%
  mutate(Denmark = sum(across(starts_with("Denmark")))) %>%
  mutate(China = sum(across(starts_with("China")))) %>%
  mutate(France = sum(across(starts_with("France")))) %>%
  mutate(Netherlands = sum(across(starts_with("Netherlands")))) %>%
  mutate(`New Zealand` = sum(across(starts_with("New Zealand")))) %>%
  mutate(`United Kingdom` = sum(across(starts_with("United Kingdom")))) %>%
  select(-(Australia...11:Australia...18)) %>%
  select(-(Canada...42:Canada...57)) %>%
  select(-(China...61:China...94)) %>%
  select(-(Denmark...105:Denmark...107)) %>%
  select(-(France...122:France...133)) %>%
  select(-(Netherlands...196:Netherlands...200)) %>%
  select(-(`New Zealand...201`:`New Zealand...202`)) %>%
  select(-(`United Kingdom...262`:`United Kingdom...275`)) %>%
  pivot_longer(!Time, names_to='Country', values_to='ConfirmedDeath') %>% #Compact data
  arrange(Country) #Sort by country's name
```

``` r
head(deaths)
```

    ## # A tibble: 6 x 3
    ##   Time       Country     ConfirmedDeath
    ##   <date>     <chr>                <dbl>
    ## 1 2020-01-23 Afghanistan              0
    ## 2 2020-01-24 Afghanistan              0
    ## 3 2020-01-25 Afghanistan              0
    ## 4 2020-01-26 Afghanistan              0
    ## 5 2020-01-27 Afghanistan              0
    ## 6 2020-01-28 Afghanistan              0

``` r
vaccine <- vaccine %>%
  mutate(date = as.Date(date, format='%m/%d/%Y')) %>%
  rename(c("Time" = "date", "Country" = "location", "FullyVaccinatedPercent" = "people_fully_vaccinated_per_hundred")) 
```

``` r
head(vaccine)
```

    ## # A tibble: 6 x 3
    ##   Country     Time       FullyVaccinatedPercent
    ##   <chr>       <date>                      <dbl>
    ## 1 Afghanistan 2020-02-24                     NA
    ## 2 Afghanistan 2020-02-25                     NA
    ## 3 Afghanistan 2020-02-26                     NA
    ## 4 Afghanistan 2020-02-27                     NA
    ## 5 Afghanistan 2020-02-28                     NA
    ## 6 Afghanistan 2020-02-29                     NA

## Joining Confirmed Case, Death, and Vaccine into one dataframe

``` r
df <- confirmed %>%
  inner_join(deaths, by=c('Time','Country')) %>%
  left_join(vaccine, by=c('Time','Country'))
```

``` r
head(df)
```

    ## # A tibble: 6 x 5
    ##   Time       Country     ConfirmedCase ConfirmedDeath FullyVaccinatedPercent
    ##   <date>     <chr>               <dbl>          <dbl>                  <dbl>
    ## 1 2020-01-23 Afghanistan             0              0                     NA
    ## 2 2020-01-24 Afghanistan             0              0                     NA
    ## 3 2020-01-25 Afghanistan             0              0                     NA
    ## 4 2020-01-26 Afghanistan             0              0                     NA
    ## 5 2020-01-27 Afghanistan             0              0                     NA
    ## 6 2020-01-28 Afghanistan             0              0                     NA

**Final dataframe has been established. Hence, we can proceed to the
data exploration step.**

# Analysis of Vaccination Effect on Indonesia’s Case Fatality Rate

## Case Fatality Rate

Case Fatality Rate is a parameter that measures the severity of the
disease that causes death. CFR is the proportion of people diagnosed
with a certain disease, who end up dying of it. For example, among a
total of 200 patients with disease A, 20 of them died from the same
disease within 30 days. Thus,

The 30-day case fatality rate = ![\\frac{20}{200} \\times 100\\%
= 10\\%](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Cfrac%7B20%7D%7B200%7D%20%5Ctimes%20100%5C%25%20%3D%2010%5C%25
"\\frac{20}{200} \\times 100\\% = 10\\%")

## CFR Calculation

Actually, CFR is not designed to be calculated on an ongoing pandemic.
Ideally, CFR should be calculated after the end of a pandemic, because
there will be a bias. From the CFR definition above, CFR is calculated
by simply dividing death caused by a disease with the total confirmed
cases. At glance, we are tempted to simply divide the total death with
the total confirmed new cases in each days. But, one should be noted
that people are not die instantly from a disease. A person whom die by a
COVID-19 had been infected in around 10 days before. Thus, there is a
‘delay’ bias. We will explore this bias and how to ease this bias.

First, we need to find the delay. This are done by plotting
ConfirmedCase and ConfirmedDeath to find each ‘peak’.

``` r
indonesia <- df %>%
  filter(Country == 'Indonesia')
```

``` r
tail(indonesia)
```

    ## # A tibble: 6 x 5
    ##   Time       Country   ConfirmedCase ConfirmedDeath FullyVaccinatedPercent
    ##   <date>     <chr>             <dbl>          <dbl>                  <dbl>
    ## 1 2022-03-15 Indonesia         14408            308                   54.9
    ## 2 2022-03-16 Indonesia         13018            230                   55.2
    ## 3 2022-03-17 Indonesia         11532            237                   55.3
    ## 4 2022-03-18 Indonesia          9528            199                   55.5
    ## 5 2022-03-19 Indonesia          7951            188                   55.6
    ## 6 2022-03-20 Indonesia          5922            139                   55.8

### Making the dataframe for plotting on ggplot

``` r
indonesia_for_ggplot <- indonesia %>%
  pivot_longer(cols=c(ConfirmedCase, ConfirmedDeath, FullyVaccinatedPercent), names_to='Type', values_to='Values')
```

``` r
tail(indonesia_for_ggplot)
```

    ## # A tibble: 6 x 4
    ##   Time       Country   Type                   Values
    ##   <date>     <chr>     <chr>                   <dbl>
    ## 1 2022-03-19 Indonesia ConfirmedCase          7951  
    ## 2 2022-03-19 Indonesia ConfirmedDeath          188  
    ## 3 2022-03-19 Indonesia FullyVaccinatedPercent   55.6
    ## 4 2022-03-20 Indonesia ConfirmedCase          5922  
    ## 5 2022-03-20 Indonesia ConfirmedDeath          139  
    ## 6 2022-03-20 Indonesia FullyVaccinatedPercent   55.8

### Plotting Indonesia’s Confirmed Case, Death vs Time

``` r
ggplot(indonesia_for_ggplot, aes(x=Time, y=Values, colour=Type, group=Type)) +
  geom_line() +
  facet_grid(Type ~ ., scales = 'free_y') +
  labs(title='Indonesia\'s COVID-19 Confirmed Case and Confirmed Death')
```

    ## Warning: Removed 371 row(s) containing missing values (geom_path).

![](Master_files/figure-gfm/Plot%20of%20Indonesia's%20Confirmed%20Cases,%20Confirmed%20Death,%20and%20Vaccination%20Rate-1.png)<!-- -->
From the presented plot above, it can be seen that there are 3 waves of
COVID-19 in Indonesia. If we take a look at Wave 2, which is happened
around July 2021, it is obvious that there is a unnoticeable interval
between the `ConfirmedCase` and `ConfirmedDeath` peak. If we look
closer, the peak of `ConfirmedCase` happened on 15 July 2021. However,
the peak of `ConfirmedDeath` happened on 27 July 2021. We can conclude
that there are 12 days of delay in Indonesia. Thus, to estimate the CFR
on an ongoing COVID-19, we need to shift the `ConfirmedDeath` by 12
days.

> Reference: Thomas, B. S., & Marks, N. A. (2021). Estimating the case
> fatality ratio for COVID-19 using a time-shifted distribution
> analysis. Epidemiology and infection, 149, e197.
> <https://doi.org/10.1017/S0950268821001436>

### Dataframe with Shifted Death

``` r
indonesia_death_shifted <- indonesia %>%
  select(Time, ConfirmedDeath) %>%
  mutate(Time = Time - 12) %>% #Shift Date by 12 days before
  rename(ConfirmedDeathShifted = ConfirmedDeath)
```

``` r
tail(indonesia_death_shifted)
```

    ## # A tibble: 6 x 2
    ##   Time       ConfirmedDeathShifted
    ##   <date>                     <dbl>
    ## 1 2022-03-03                   308
    ## 2 2022-03-04                   230
    ## 3 2022-03-05                   237
    ## 4 2022-03-06                   199
    ## 5 2022-03-07                   188
    ## 6 2022-03-08                   139

``` r
indonesia_final_cfr <- indonesia %>%
  inner_join(indonesia_death_shifted)
```

    ## Joining, by = "Time"

``` r
tail(indonesia_final_cfr)
```

    ## # A tibble: 6 x 6
    ##   Time       Country   ConfirmedCase ConfirmedDeath FullyVaccinatedPercent
    ##   <date>     <chr>             <dbl>          <dbl>                  <dbl>
    ## 1 2022-03-03 Indonesia         37259            232                   52.9
    ## 2 2022-03-04 Indonesia         26347            328                   53.0
    ## 3 2022-03-05 Indonesia         30156            322                   53.3
    ## 4 2022-03-06 Indonesia         24867            254                   53.5
    ## 5 2022-03-07 Indonesia         21380            258                   53.6
    ## 6 2022-03-08 Indonesia         30148            401                   53.8
    ## # ... with 1 more variable: ConfirmedDeathShifted <dbl>

Lastly, we created a dataframe to calculate CFR on each day. \#\# Final
Indonesia’s CFR data

``` r
indonesia_final_cfr <- indonesia_final_cfr %>%
  mutate(CFR = ConfirmedDeathShifted/ConfirmedCase*100) %>% #CFR Calculation
  select(-ConfirmedDeath) %>%
  filter(ConfirmedCase > 200)
indonesia_final_cfr[is.na(indonesia_final_cfr)] = 0 #Make all NaN in the CFR column to 0
```

``` r
tail(indonesia_final_cfr)
```

    ## # A tibble: 6 x 6
    ##   Time       Country   ConfirmedCase FullyVaccinatedPerc~ ConfirmedDeathS~   CFR
    ##   <date>     <chr>             <dbl>                <dbl>            <dbl> <dbl>
    ## 1 2022-03-03 Indonesia         37259                 52.9              308 0.827
    ## 2 2022-03-04 Indonesia         26347                 53.0              230 0.873
    ## 3 2022-03-05 Indonesia         30156                 53.3              237 0.786
    ## 4 2022-03-06 Indonesia         24867                 53.5              199 0.800
    ## 5 2022-03-07 Indonesia         21380                 53.6              188 0.879
    ## 6 2022-03-08 Indonesia         30148                 53.8              139 0.461

## Dataframe for plotting

``` r
indonesia_for_cfr_vaccine_plot <- indonesia_final_cfr %>%
  pivot_longer(cols = c(CFR, FullyVaccinatedPercent), names_to='Type', values_to='Value')
```

``` r
head(indonesia_for_cfr_vaccine_plot)
```

    ## # A tibble: 6 x 6
    ##   Time       Country   ConfirmedCase ConfirmedDeathShifted Type            Value
    ##   <date>     <chr>             <dbl>                 <dbl> <chr>           <dbl>
    ## 1 2020-04-06 Indonesia           218                    15 CFR              6.88
    ## 2 2020-04-06 Indonesia           218                    15 FullyVaccinate~  0   
    ## 3 2020-04-07 Indonesia           247                    47 CFR             19.0 
    ## 4 2020-04-07 Indonesia           247                    47 FullyVaccinate~  0   
    ## 5 2020-04-08 Indonesia           218                     8 CFR              3.67
    ## 6 2020-04-08 Indonesia           218                     8 FullyVaccinate~  0

## Final plot of CFR and Vaccination Rate vs Time

``` r
vaccine_effect <- ggplot(indonesia_for_cfr_vaccine_plot, aes(x=Time, y=Value)) + facet_grid(Type~., scales = "free_y") +
  geom_smooth(data=subset(indonesia_for_cfr_vaccine_plot, Type == 'CFR')) +
  geom_line(data=subset(indonesia_for_cfr_vaccine_plot, Type == 'FullyVaccinatedPercent'), size=1, color='green') +
  labs(title='Effect of Vaccination on Case Fatality Rate in Indonesia', x='Date', y='Value (%)') +
  theme(
     plot.background = element_rect(fill = "#3d989b",colour = "#3d989b"),
     # gets rid of white border around plot: 
     panel.border = element_blank(),
     plot.title = element_text(colour="#3b365f", face='bold'),
     panel.background = element_rect(fill='#faede4')
   )
vaccine_effect
```

    ## `geom_smooth()` using method = 'loess' and formula 'y ~ x'

![](Master_files/figure-gfm/unnamed-chunk-10-1.png)<!-- --> From the
plot above, it can be seen that vaccination give a significant effect in
decreasing the CFR in Indonesia. With vaccination, severity of COVID-19
is lowered. Vaccination program held by Indonesia’s government is proven
to be successful.

# Delta and Omicron Severity Comparison

Many claims that Omicron is less severe compared to the Delta variant.
In the following section, we will prove by their CFR compared to each
other. From the data, we know that Delta variant is the most dominant
variant from 9 August 2021 to 13 December 2021 in Indonesia. And there
were a transition period from the Delta variant to Omicron between 13
December 2021 and 7 February 2022. Since 7 February 2022, Omicron has
been regarded as the most dominant variant in Indonesia.

We will explore the distribution of CFR for each variant.

Data Source:
<https://ourworldindata.org/grapher/covid-variants-bar?country=~IDN>

## Dataframe preparation

``` r
delta <- indonesia_final_cfr %>%
  filter(Time > '2021-08-09') %>%
  filter(Time < '2021-12-13')
delta$Variant <- "Delta"

omicron <- indonesia_final_cfr %>%
  filter(Time > '2022-02-07')
omicron$Variant <- "Omicron"

indonesia_variant <- delta %>%
  full_join(omicron)
```

    ## Joining, by = c("Time", "Country", "ConfirmedCase", "FullyVaccinatedPercent",
    ## "ConfirmedDeathShifted", "CFR", "Variant")

## Plot with box plot

``` r
ggplot(indonesia_variant, aes(Variant, CFR)) +
  geom_boxplot(color = '#3b365f', fill='#a4ebe1') +
  labs(title="Delta vs Omicron Case Fatality Rate Distribution") +
  theme(
     plot.background = element_rect(fill = "#a4ebe1",colour = "#a4ebe1"),
     # gets rid of white border around plot: 
     panel.border = element_blank(),
     plot.title = element_text(colour="#3b365f", face='bold'),
     panel.background = element_rect(fill='#faede4')
   )
```

![](Master_files/figure-gfm/unnamed-chunk-12-1.png)<!-- --> \#\#
Hypothesis testing with t-test

``` r
t_test <- t.test(delta$CFR, omicron$CFR)
t_test
```

    ## 
    ##  Welch Two Sample t-test
    ## 
    ## data:  delta$CFR and omicron$CFR
    ## t = 20.749, df = 134.07, p-value < 2.2e-16
    ## alternative hypothesis: true difference in means is not equal to 0
    ## 95 percent confidence interval:
    ##  2.260217 2.736517
    ## sample estimates:
    ## mean of x mean of y 
    ## 3.1372609 0.6388937

From the plot above, it can be seen that the Omicron variant has much
lower CFR compared to the Delta variant. With t-test, it is also
concluded that with 95% confidence level, the probability that Omicron
variant is not less severe that the Delta variant is ![2
\\times 10^{-16}](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;2%20%5Ctimes%2010%5E%7B-16%7D
"2 \\times 10^{-16}"). Thus, we can conclude that the Omicron variant is
proven statistically that less severe compared to the Delta variant.

# Indonesia’s Situation compared with Malaysia and Thailand

In the following section, CFR of Malaysia and Thailand will be compared
to Indonesia’s CFR. Since Malaysia and Thailand are going to treat
COVID-19 as an endemic, this section will predict Indonesia’s situation
compared to those countries. Steps are similar with analysing
Indonesia’s CFR, where the death data shifting will be done.

``` r
malaysia <- df %>%
  filter(Country == 'Malaysia')
```

``` r
#new death dataframe
malaysia_death_shifted <- malaysia %>%
  select(Time, ConfirmedDeath) %>%
  mutate(Time = Time - 16) %>% #Shift Date by 16 days before
  rename(ConfirmedDeathShifted = ConfirmedDeath)
tail(malaysia_death_shifted)
```

    ## # A tibble: 6 x 2
    ##   Time       ConfirmedDeathShifted
    ##   <date>                     <dbl>
    ## 1 2022-02-27                    95
    ## 2 2022-02-28                   105
    ## 3 2022-03-01                    86
    ## 4 2022-03-02                    59
    ## 5 2022-03-03                    85
    ## 6 2022-03-04                    71

``` r
malaysia_final_cfr <- malaysia %>%
  inner_join(malaysia_death_shifted)
```

    ## Joining, by = "Time"

``` r
tail(malaysia_final_cfr)
```

    ## # A tibble: 6 x 6
    ##   Time       Country  ConfirmedCase ConfirmedDeath FullyVaccinatedPercent
    ##   <date>     <chr>            <dbl>          <dbl>                  <dbl>
    ## 1 2022-02-27 Malaysia         24466             40                   78.6
    ## 2 2022-02-28 Malaysia         23100             75                   78.6
    ## 3 2022-03-01 Malaysia         25854             78                   78.6
    ## 4 2022-03-02 Malaysia         27500            115                   78.6
    ## 5 2022-03-03 Malaysia         32467             86                   78.6
    ## 6 2022-03-04 Malaysia         33209             78                   78.6
    ## # ... with 1 more variable: ConfirmedDeathShifted <dbl>

``` r
malaysia_final_cfr <- malaysia_final_cfr %>%
  mutate(CFR = ConfirmedDeathShifted/ConfirmedCase*100) %>% #CFR Calculation
  select(-ConfirmedDeath) %>%
  filter(ConfirmedCase > 50)
malaysia_final_cfr[is.na(malaysia_final_cfr)] = 0 #Make all NaN in the CFR column to 0
tail(malaysia_final_cfr)
```

    ## # A tibble: 6 x 6
    ##   Time       Country  ConfirmedCase FullyVaccinatedPerce~ ConfirmedDeathS~   CFR
    ##   <date>     <chr>            <dbl>                 <dbl>            <dbl> <dbl>
    ## 1 2022-02-27 Malaysia         24466                  78.6               95 0.388
    ## 2 2022-02-28 Malaysia         23100                  78.6              105 0.455
    ## 3 2022-03-01 Malaysia         25854                  78.6               86 0.333
    ## 4 2022-03-02 Malaysia         27500                  78.6               59 0.215
    ## 5 2022-03-03 Malaysia         32467                  78.6               85 0.262
    ## 6 2022-03-04 Malaysia         33209                  78.6               71 0.214

``` r
ggplot(malaysia_final_cfr, aes(Time, CFR)) +
  geom_point() +
  geom_smooth()
```

    ## `geom_smooth()` using method = 'loess' and formula 'y ~ x'

![](Master_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

``` r
thai <- df %>%
  filter(Country == 'Thailand')
head(thai)
```

    ## # A tibble: 6 x 5
    ##   Time       Country  ConfirmedCase ConfirmedDeath FullyVaccinatedPercent
    ##   <date>     <chr>            <dbl>          <dbl>                  <dbl>
    ## 1 2020-01-23 Thailand             0              0                     NA
    ## 2 2020-01-24 Thailand             1              0                     NA
    ## 3 2020-01-25 Thailand             1              0                     NA
    ## 4 2020-01-26 Thailand             2              0                     NA
    ## 5 2020-01-27 Thailand             0              0                     NA
    ## 6 2020-01-28 Thailand             6              0                     NA

``` r
#new death dataframe
thai_death_shifted <- thai %>%
  select(Time, ConfirmedDeath) %>%
  mutate(Time = Time - 5) %>% #Shift Date by 5 days before
  rename(ConfirmedDeathShifted = ConfirmedDeath)
tail(thai_death_shifted)
```

    ## # A tibble: 6 x 2
    ##   Time       ConfirmedDeathShifted
    ##   <date>                     <dbl>
    ## 1 2022-03-10                    70
    ## 2 2022-03-11                    77
    ## 3 2022-03-12                    80
    ## 4 2022-03-13                    87
    ## 5 2022-03-14                    84
    ## 6 2022-03-15                    88

``` r
thai_final_cfr <- thai %>%
  inner_join(malaysia_death_shifted)
```

    ## Joining, by = "Time"

``` r
tail(thai_final_cfr)
```

    ## # A tibble: 6 x 6
    ##   Time       Country  ConfirmedCase ConfirmedDeath FullyVaccinatedPercent
    ##   <date>     <chr>            <dbl>          <dbl>                  <dbl>
    ## 1 2022-02-27 Thailand         22311             42                   71  
    ## 2 2022-02-28 Thailand             0              0                   71.0
    ## 3 2022-03-01 Thailand         42617             88                   71.0
    ## 4 2022-03-02 Thailand         23618             49                   71.1
    ## 5 2022-03-03 Thailand         23834             54                   71.1
    ## 6 2022-03-04 Thailand         22818             52                   71.2
    ## # ... with 1 more variable: ConfirmedDeathShifted <dbl>

``` r
thai_final_cfr <- thai_final_cfr %>%
  mutate(CFR = ConfirmedDeathShifted/ConfirmedCase*100) %>% #CFR Calculation
  select(-ConfirmedDeath) %>%
  filter(ConfirmedCase > 30)
thai_final_cfr[is.na(thai_final_cfr)] = 0 #Make all NaN in the CFR column to 0
tail(thai_final_cfr)
```

    ## # A tibble: 6 x 6
    ##   Time       Country  ConfirmedCase FullyVaccinatedPerce~ ConfirmedDeathS~   CFR
    ##   <date>     <chr>            <dbl>                 <dbl>            <dbl> <dbl>
    ## 1 2022-02-26 Thailand         24719                  71.0               92 0.372
    ## 2 2022-02-27 Thailand         22311                  71                 95 0.426
    ## 3 2022-03-01 Thailand         42617                  71.0               86 0.202
    ## 4 2022-03-02 Thailand         23618                  71.1               59 0.250
    ## 5 2022-03-03 Thailand         23834                  71.1               85 0.357
    ## 6 2022-03-04 Thailand         22818                  71.2               71 0.311

``` r
ggplot(thai_final_cfr, aes(Time, CFR)) +
  geom_point() +
  geom_smooth()
```

    ## `geom_smooth()` using method = 'loess' and formula 'y ~ x'

![](Master_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->

``` r
indo_malay_thai_cfr <- indonesia_final_cfr %>%
  full_join(malaysia_final_cfr) %>%
  full_join(thai_final_cfr)
```

    ## Joining, by = c("Time", "Country", "ConfirmedCase", "FullyVaccinatedPercent",
    ## "ConfirmedDeathShifted", "CFR")
    ## Joining, by = c("Time", "Country", "ConfirmedCase", "FullyVaccinatedPercent",
    ## "ConfirmedDeathShifted", "CFR")

``` r
tail(indo_malay_thai_cfr)
```

    ## # A tibble: 6 x 6
    ##   Time       Country  ConfirmedCase FullyVaccinatedPerce~ ConfirmedDeathS~   CFR
    ##   <date>     <chr>            <dbl>                 <dbl>            <dbl> <dbl>
    ## 1 2022-02-26 Thailand         24719                  71.0               92 0.372
    ## 2 2022-02-27 Thailand         22311                  71                 95 0.426
    ## 3 2022-03-01 Thailand         42617                  71.0               86 0.202
    ## 4 2022-03-02 Thailand         23618                  71.1               59 0.250
    ## 5 2022-03-03 Thailand         23834                  71.1               85 0.357
    ## 6 2022-03-04 Thailand         22818                  71.2               71 0.311

``` r
ggplot(indo_malay_thai_cfr, aes(Time, CFR, group = Country, color=Country)) +
  geom_smooth() + 
  labs(title='COVID-19 Case Fatality Rate in Indonesia, Malaysia, and Thailand') +
  theme(
     plot.background = element_rect(fill = "#3d989b",colour = "#3d989b"),
     # gets rid of white border around plot: 
     panel.border = element_blank(),
     plot.title = element_text(colour="#3b365f", face='bold'),
     panel.background = element_rect(fill='#faede4')
   )
```

    ## `geom_smooth()` using method = 'loess' and formula 'y ~ x'

![](Master_files/figure-gfm/unnamed-chunk-18-1.png)<!-- --> It can be
seen that now Indonesia’s CFR lies in similar value with Malaysia and
Thailand’s CFR. The severity of COVID-19 in Indonesia should be the same
as Malaysia and Thailand.

# Conclusions

1.  Indonesia vaccination program has succeeded to suppress the severity
    of COVID-19 in Indonesia. It is proven by the decrease of CFR and
    increasing vaccination rate in Indonesia. The decreasing CFR value
    is also contributed by the domination of Omicron variant in
    Indonesia. Vaccination is the key aspect to endemic transition.
    Mutation to Omicron variant is also prove that the virus exists not
    to infect people severely, but only to retain its existence.
2.  Comparing to Malaysia and Thailand, Indonesia has similar COVID-19
    situation in terms of severity. One can assume that Indonesia has
    similar vaccination and healthcare condition compared to Malaysia
    and Thailand.
3.  It is still not safe to say that Indonesia is ready for the
    transition, because this short analysis only covers from the terms
    of COVID-19’s severity and vaccination success. But, it is worth for
    the Government to do more research of the feasibility. Economics and
    social-politic aspects haven’t covered in this analysis.

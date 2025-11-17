Data Cleaning
================
2025-11-16

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ## ✔ ggplot2   3.5.2     ✔ tibble    3.3.0
    ## ✔ lubridate 1.9.4     ✔ tidyr     1.3.1
    ## ✔ purrr     1.1.0     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
# all demographic data. currently selecting for name, fips, lat and long, race, population 2019, deaths by homicide, life expectancy, fatal police shootings 2017,2018,2019, 2020, average income, dem/gop for 2016, 2020, poverty rate, children in poverty, 80th and 20th percentile incomes, violent crime rate. This one contains everything. I've divided them into subcategories, so that if someone wants to use them for a particular analysis, they can select from it easily to create some mini dfs. 

# made lots of changes to naming within the df so it could be easily joined with nyt data. 

counties_demo_df = 
  read_csv(file = "./data/counties.csv") |>
  janitor::clean_names() |>
  select(name, fips, state, longitude_deg, latitude_deg, 
         # race for each county 
         race_non_hispanic_white_alone_male,race_non_hispanic_white_alone_female,
         race_black_alone_male,race_black_alone_female,
         race_asian_alone_male,race_asian_alone_female,
         race_hispanic_male, race_hispanic_female,
         # overall population
         population_2019,
         # firearm and police shooting info
         deaths_firearm_suicides,deaths_homicides, 
         fatal_police_shootings_total_2017, 
         fatal_police_shootings_unarmed_2017, fatal_police_shootings_firearmed_2017, 
         fatal_police_shootings_total_2018, 
         fatal_police_shootings_unarmed_2018, fatal_police_shootings_firearmed_2018, 
         fatal_police_shootings_total_2019, 
         fatal_police_shootings_unarmed_2017, fatal_police_shootings_firearmed_2019, 
         fatal_police_shootings_total_2020, 
         fatal_police_shootings_unarmed_2017, fatal_police_shootings_firearmed_2020, 
         police_deaths,
         health_violent_crime_rate,
         # income/ poverty related 
         avg_income, 
         poverty_rate,
         health_percent_children_in_poverty,
         health_80th_percentile_income, 
         health_20th_percentile_income, 
         # elections
         elections_2016_total, elections_2016_dem, elections_2016_gop,
         elections_2020_total, elections_2020_dem, elections_2020_gop,
         # general health
         life_expectancy, 
         health_percent_smokers
         ) |>
  rename(county = name) |>
  mutate(
    county = sub(" county", "", county, fixed = TRUE), 
    county = sub(" parish", "", county, fixed = TRUE)
    ) |>
  mutate(state = state.name[ match(state, state.abb) ])
```

    ## Rows: 3142 Columns: 237
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr   (4): name, fips, state, zip-codes
    ## dbl (233): land_area (km^2), area (km^2), longitude (deg), latitude (deg), n...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
# nyt covid data per year, with all the info in them. 
covid_2020_df = 
  read_csv(file = "./data/us-counties-2020.csv") |>
  janitor::clean_names() |>
  mutate(year = 2020)
```

    ## Rows: 884737 Columns: 6
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (3): county, state, fips
    ## dbl  (2): cases, deaths
    ## date (1): date
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
covid_2021_df = 
  read_csv(file = "./data/us-counties-2021.csv") |>
  janitor::clean_names() |>
  mutate(year = 2021)
```

    ## Rows: 1185373 Columns: 6
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (3): county, state, fips
    ## dbl  (2): cases, deaths
    ## date (1): date
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
covid_2022_df = 
  read_csv(file = "./data/us-counties-2022.csv") |>
  janitor::clean_names() |>
  mutate(year = 2022)
```

    ## Rows: 1188042 Columns: 6
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (3): county, state, fips
    ## dbl  (2): cases, deaths
    ## date (1): date
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
covid_2023_df = 
  read_csv(file = "./data/us-counties-2023.csv") |>
  janitor::clean_names() |>
  mutate(year = 2023)
```

    ## Rows: 267009 Columns: 6
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (3): county, state, fips
    ## dbl  (2): cases, deaths
    ## date (1): date
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
# nyt masking data, all from july 2020, peak covid, from survey data as well. Divided into high mask use and low mask use columns for simplicity. 

covid_masking_df =
  read_csv(file = "./data/mask-use-by-county.csv") |>
  mutate(
    COUNTYFP = sprintf("%05d", as.numeric(COUNTYFP)),
    high_mask_use = FREQUENTLY + ALWAYS,
    low_mask_use  = NEVER + RARELY + SOMETIMES
  )
```

    ## Rows: 3142 Columns: 6
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (1): COUNTYFP
    ## dbl (5): NEVER, RARELY, SOMETIMES, FREQUENTLY, ALWAYS
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
# all the covid data with sums per year of cases and deaths per county per year. 

df_covid_per_year =
  bind_rows(covid_2020_df, covid_2021_df, covid_2022_df, covid_2023_df) |>
  group_by(county, state, fips, year) |>
  summarise(
    total_cases = sum(cases, na.rm = TRUE),
    total_deaths = sum(deaths, na.rm = TRUE),
    .groups = "drop"
  ) |>
    pivot_wider(
    names_from = year,
    values_from = c(total_cases, total_deaths),
    names_glue = "{.value}_{year}"
  ) |>
  mutate(fips = sprintf("%05d", as.numeric(fips))) |>
  left_join(
    covid_masking_df |>
      select(COUNTYFP, high_mask_use, low_mask_use),
    by = c("fips" = "COUNTYFP")
  )

# for Evan's analysis : all the COVID data by county, but not with sums/totals, all raw data. Also including masking survey data in here in case. 

df_covid_all_raw =
  bind_rows(covid_2020_df, covid_2021_df, covid_2022_df, covid_2023_df) |>
  pivot_wider(
    id_cols = c(fips, county, state),
    names_from = c(date),
    values_from = c(cases, deaths),
    names_glue = "{date}_{.value}"
  ) |>
  mutate(fips = sprintf("%05d", as.numeric(fips))) |>
  left_join(
    covid_masking_df |>
      select(COUNTYFP, high_mask_use, low_mask_use),
    by = c("fips" = "COUNTYFP")
  )

# covid data joined with counties demographic data. 
# some things counties_demo_df data treats as counties, such as boroughs in nyc and alaska boroughs, not in the nyt data. So just matched whatever we could for now. We can discuss how we want to handle this later. 

df_covid_per_year[["county"]] <- tolower(trimws(df_covid_per_year[["county"]]))

final_df <- counties_demo_df |>
  left_join(
    df_covid_per_year,
    by = c("fips", "county", "state")
  )
```

For covid data: totals of each

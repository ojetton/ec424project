Class Project
================
Owen Jetton
2/20/2021

``` r
knitr::opts_chunk$set(echo = TRUE)
 
library(pacman)

p_load(readr, dplyr, ggplot2, readxl, magrittr, janitor)
```

``` r
# CO2 data from 2016 - 19
co2_data_2016 = read_xlsx("C:/Users/ojett/Documents/EC424/ec424project/emissions2016.xlsx",
                     sheet = 1)

co2_data_2017 = read_xlsx("C:/Users/ojett/Documents/EC424/ec424project/emissions2017.xlsx",
                     sheet = 1)

co2_data_2018 = read_xlsx("C:/Users/ojett/Documents/EC424/ec424project/emissions2018.xlsx",
                     sheet = 1)

co2_data_2019 = read_xlsx("C:/Users/ojett/Documents/EC424/ec424project/emissions2019.xlsx",
                     sheet = 1)

# Merging to get our final data set
data_full = do.call("rbind", list(co2_data_2016, co2_data_2017,
                                  co2_data_2018, co2_data_2019))

  # Cleaning the full data set
data_full %<>% clean_names() %>%
  filter(generation_k_wh > 0) %>% 
  select(-c(eia_balancing_authority_region, balancing_authority_name, nerc_region, 
            fuel_units, metric_tonnes_of_co2_emissions, fuel_code,
            plant_name)) %>%
  mutate(sector_code = factor(sector_code))
```

``` r
# Graphs Checking out the data
ggplot(data_full, aes(x = log(generation_k_wh), y = log(tons_of_co2_emissions))) +
  geom_point(aes(color = aggregated_fuel_group)) +
  labs(x = "Generation (KWH)", y = "Tons of CO2 Emissions",
       title = "Power Generated Against CO2 Emissions by Fuel Group (Logged Values)",
      color = "Fuel Group")
```

![](project_markdown_files/figure-gfm/Initial%20Graphs-1.png)<!-- -->

``` r
ggplot(data_full, aes(y = aggregated_fuel_group, fill = aggregated_fuel_group)) + 
  geom_bar(stat = "count") +
  labs(x = "Total", y = "Fuel Group") +
  theme_bw(base_size = 15) +
  theme(legend.position = "none")
```

![](project_markdown_files/figure-gfm/Initial%20Graphs-2.png)<!-- -->

``` r
ggplot(data_full, aes(x = factor(1), fill = aggregated_fuel_group)) + 
  geom_bar(width = 1, color="white") + coord_polar("y") +
  theme_bw(base_size = 15) +
  theme_void()
```

![](project_markdown_files/figure-gfm/Initial%20Graphs-3.png)<!-- -->

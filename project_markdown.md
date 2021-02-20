Class Project
================
Owen Jetton
2/20/2021

``` r
knitr::opts_chunk$set(echo = TRUE)
 
library(pacman)

p_load(readr, dplyr, ggplot2, readxl, magrittr, janitor)


# Read data on cooling systems
cool_data = read_xlsx("C:/Users/ojett/Downloads/cooling_summary_2019.xlsx")

  # cleaning a bit of the data that will make merging easier later
cool_data %<>% 
  group_by(`Plant Code`, `Month`) %>% 
  summarize_if(is.numeric, sum)

# Reading the CO2 emissions data
co2_data = read_xlsx("C:/Users/ojett/Downloads/emissions2019.xlsx", sheet = 1)

  # removing repetitive and useless variables
co2_data %<>% select(-c("State", "Plant Name", "Fuel Code", "Prime Mover"))


# Merging to get our final data set
data_full = merge(cool_data, co2_data, by = c("Plant Code"))

  # Cleaning the full data set
data_full %<>% clean_names() %>%
  filter(generation_k_wh > 0) %>% 
  select(-c(utility_id, eia_balancing_authority_region, balancing_authority_code,
            balancing_authority_name, nerc_region, fuel_units)) %>%
  distinct()

# Graphs Checking out the data
ggplot(data_full, aes(x = log(generation_k_wh), y = log(tons_of_co2_emissions))) +
  geom_point(aes(color = aggregated_fuel_group)) +
  labs(x = "Generation (KWH)", y = "Tons of CO2 Emissions",
       title = "Power Generated Against CO2 Emissions by Fuel Group (Logged Values)",
      color = "Fuel Group")
```

![](project_markdown_files/figure-gfm/setup-1.png)<!-- -->

``` r
ggplot(data_full, aes(x = aggregated_fuel_group, fill = aggregated_fuel_group)) + 
  geom_bar(stat = "count") +
  xlab("Fuel Group") + ylab("Frequency") +
  theme_bw(base_size = 15) +
  theme(legend.position = "none")
```

![](project_markdown_files/figure-gfm/setup-2.png)<!-- -->
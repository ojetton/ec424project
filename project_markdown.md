Class Project
================
Owen Jetton
2/20/2021

# Data

``` r
knitr::opts_chunk$set(echo = TRUE)
 
# Download and load packages
library(pacman)

p_load(readr, dplyr, ggplot2, readxl, magrittr, janitor, utils, tidyverse, tidymodels,
       tune, glmnet, recipes)
```

``` r
# Read CO2 data from 2016 - 19
co2_data_2016 = read_xlsx("C:/Users/ojett/Documents/EC424/ec424project/emissions2016.xlsx",
                     sheet = 1) %>% mutate(year = 2016)
                        # add year variable which will be helpful for merging data later

co2_data_2017 = read_xlsx("C:/Users/ojett/Documents/EC424/ec424project/emissions2017.xlsx",
                     sheet = 1) %>% mutate(year = 2017)

co2_data_2018 = read_xlsx("C:/Users/ojett/Documents/EC424/ec424project/emissions2018.xlsx",
                     sheet = 1) %>% mutate(year = 2018)

co2_data_2019 = read_xlsx("C:/Users/ojett/Documents/EC424/ec424project/emissions2019.xlsx",
                     sheet = 1) %>% mutate(year = 2019)


# Upload heating and cooling days data
heat_cool_df = read.delim("https://www.ncei.noaa.gov/pub/orders/CDODiv6010178275591.txt",
                          sep = ",") 
```

# Cleaning

``` r
# Merging CO2 to get larger data set
data_full = do.call("rbind", list(co2_data_2016, co2_data_2017,
                                  co2_data_2018, co2_data_2019))


# Cleaning the full CO2 data
data_full %<>% clean_names() %>%
  
    # only care about power plants that generate electricity
  filter(generation_k_wh > 0) %>% 
  
    # remove irrelevent or repretitive variables
  select(-c(eia_balancing_authority_region, balancing_authority_name, nerc_region, 
            fuel_units, metric_tonnes_of_co2_emissions, plant_name))



# Cleaning the Heating and Cooling days data
heat_cool_df %<>% clean_names() %>% 
    # select relevant variables
  select(state_code, year_month, cdd, hdd) %>%
  
    # make year variable from year_month variable
  mutate(year = strtrim(year_month,4)) %>%
    
    # grouping by months, and taking the sum to get the total number
      # of heating and cooling days in a year
  group_by(state_code, year) %>%
  mutate(cooling_days = sum(cdd),
         heating_days = sum(hdd),
         state_code = factor(state_code)) %>%
  
    # selecting relevant 
  select(state_code, year, cooling_days, heating_days) %>%
  distinct()

# Need the state names to correspond with the right regions:
  # read dataset that has state names for corresponding regions
  states_region = read_xlsx("C:/Users/ojett/Documents/EC424/ec424project/region_to_state.xlsx")
  
  # add state names to respective region in 
heat_cool_df = merge(heat_cool_df, states_region, by = "state_code")


# Add heating and cooling data to main dataframe

data_full = merge(data_full, heat_cool_df, by = c("state", "year"))


# Renaming CO2 emissions variable to "emissions"
  # I'm doing this because it is an important variable and will need to be written a lot
data_full %<>% rename(emissions = tons_of_co2_emissions) %>%
  
  # If there is no balancing authority
  mutate(balancing_authority_code = if_else(is.na(balancing_authority_code), "NONE",
                                            balancing_authority_code)) %>%
  
  # Also turn certain numerical variables into factors
  mutate_at(.vars = vars("sector_code", "balancing_authority_code", "state_code"),
            .funs = factor) 
```

``` r
# Graphs Checking out the data
ggplot(data_full, aes(x = log(generation_k_wh), y = log(emissions))) +
  geom_point(aes(color = aggregated_fuel_group)) +
  labs(x = "Generation (KWH)", y = "Tons of CO2 Emissions",
       title = "Power Generated Against CO2 Emissions by Fuel Group (Logged Values)",
      color = "Fuel Group")
```

![](project_markdown_files/figure-gfm/Initial%20Graphs-1.png)<!-- -->

``` r
ggplot(data_full, aes(y = aggregated_fuel_group, fill = aggregated_fuel_group)) + 
  geom_bar(stat = "count") +
  labs(x = "Total", y = "Fuel Group", 
       caption = "PET = Petroleum, MSW = Municipal solid waste, GEO = Geothermal, GAS = Natural Gas, COAL = Coal") +
  theme_bw() +
  theme(legend.position = "none")
```

![](project_markdown_files/figure-gfm/Initial%20Graphs-2.png)<!-- -->

``` r
  # Note: Gas is ALL natural gas. MSW is Munincipal Solid Waste. PET is petroleum. 
          # Coal is mostly Bituminous Coal. PET is petroleum, mostly Distillate Fuel Oil
          # GEO is geothermal
  # full table explaining fuel codes here:
    # https://www.eia.gov/electricity/annual/html/epa_a_03.html
```

# Tuning and Training

### Split into Training and Testing Data

``` r
# Set seed before splitting
set.seed(231240)

# Initial 80/20 split for the training/testing data
data_split = data_full %>% initial_split(prop = 0.8)

  # Create training data
train_df = data_split %>% training()

  # Create testing data
test_df = data_split %>% testing()
```

### Create and Tune 4 models on Training data

``` r
# ------------------------------ Set up --------------------------------
# Creating Recipe used for multiple models below
train_recipe = train_df %>% recipe(emissions ~ .) %>%
          
                      # update role
                      update_role(plant_code, new_role = "id variable") %>%
  
                      step_dummy(all_nominal()) %>%

  
                      # Mean imputation for numeric predictors
                      step_meanimpute(all_predictors() & all_numeric()) %>% 
  
                      # normalize
                      step_normalize(all_predictors() & all_numeric()) %>%
                      
                      # Remove low-variance
                      step_nzv(all_predictors() & all_numeric()) %>%
  
                      # Remove highly correlated
                      step_corr(all_predictors() & all_numeric()) %>%
  
                      # Remove linearly dependent predictors
                      step_lincomb(all_predictors() & all_numeric()) 
                            

                      
  
                      # Log the energy generation variable
                      #step_log(generation_k_wh)
  
                      # Make interaction variables

  # Meaning of interactions: 
   # sector_group*fuel_code - relationship b/w consumers and types of fuel burnt for energy
   # state_code*cooling_days, state_code*heating_days - how different areas of the country
   # demand energy at different levels depending on temperature
  

  
train_recipe %>% prep() %>% juice()
```

    ## # A tibble: 16,278 x 35
    ##     year plant_code useful_thermal_~ fuel_consumptio~ quantity_of_fue~
    ##    <dbl>      <dbl>            <dbl>            <dbl>            <dbl>
    ##  1 -1.33       7482           -0.197           -0.311           -0.323
    ##  2 -1.33       6338           -0.197           -0.311           -0.323
    ##  3 -1.33       7169           -0.197           -0.313           -0.324
    ##  4 -1.33      54305           -0.186           -0.308           -0.322
    ##  5 -1.33       6323           -0.197           -0.312           -0.324
    ##  6 -1.33        421           -0.197           -0.313           -0.324
    ##  7 -1.33       6341           -0.197           -0.312           -0.324
    ##  8 -1.33       6314           -0.197           -0.311           -0.323
    ##  9 -1.33       7174           -0.195           -0.311           -0.323
    ## 10 -1.33       6566           -0.197           -0.286           -0.315
    ## # ... with 16,268 more rows, and 30 more variables: cooling_days <dbl>,
    ## #   heating_days <dbl>, emissions <dbl>, state_CA <dbl>, state_NY <dbl>,
    ## #   state_TX <dbl>, sector_group_ELECTRIC.POWER <dbl>, sector_code_X2 <dbl>,
    ## #   sector_code_X3 <dbl>, sector_code_X5 <dbl>, sector_code_X7 <dbl>,
    ## #   prime_mover_CA <dbl>, prime_mover_CT <dbl>, prime_mover_GT <dbl>,
    ## #   prime_mover_IC <dbl>, prime_mover_ST <dbl>, fuel_code_DFO <dbl>,
    ## #   aggregated_fuel_group_GAS <dbl>, balancing_authority_code_CISO <dbl>,
    ## #   balancing_authority_code_ISNE <dbl>, balancing_authority_code_MISO <dbl>,
    ## #   balancing_authority_code_PJM <dbl>, balancing_authority_code_SWPP <dbl>,
    ## #   state_code_X102 <dbl>, state_code_X103 <dbl>, state_code_X104 <dbl>,
    ## #   state_code_X105 <dbl>, state_code_X107 <dbl>, state_code_X108 <dbl>,
    ## #   state_code_X109 <dbl>

``` r
# setting new seed for CV folds
set.seed(1387219452)
  # Create Cross Validcation folds
train_cv = train_df %>% vfold_cv(v = 5)
```

``` r
# --------------------- Basic linear regression ----------------------

# Fitting the linear regression model to training data
reg_model = 
  linear_reg() %>%
  set_engine("lm")

# Create workflow
workflow_reg = workflow() %>%
  add_model(reg_model) %>%
  add_recipe(train_recipe)

# Train model on validation sets, set metric to root mean square error
cv_reg = workflow_reg %>% 
  fit_resamples(train_cv, metrics = metric_set(rmse))
```

    ## 
    ## Attaching package: 'rlang'

    ## The following objects are masked from 'package:purrr':
    ## 
    ##     %@%, as_function, flatten, flatten_chr, flatten_dbl, flatten_int,
    ##     flatten_lgl, flatten_raw, invoke, list_along, modify, prepend,
    ##     splice

    ## The following object is masked from 'package:magrittr':
    ## 
    ##     set_names

    ## 
    ## Attaching package: 'vctrs'

    ## The following object is masked from 'package:tibble':
    ## 
    ##     data_frame

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     data_frame

``` r
# ------------------------ Elasticnet --------------------------------

# DEFINE ELASTICNET MODEL
en_model = linear_reg(penalty = tune(), mixture = tune()) %>% 
            set_engine("glmnet")

  # Define workflow
workflow_en = workflow() %>%
  add_model(en_model) %>%
  add_recipe(train_recipe) 

  # Define Penalties
lambdas = 10^seq(from = 5, to = -2, length = 1e2)
alphas = seq(from = 0, to = 1, by = 0.1)

  # Cross Validation Tuning with range of penalites and mixtures
cv_en = workflow_en %>%
  tune_grid(
    train_cv,
    grid = expand_grid(mixture = alphas, penalty = lambdas),
    metrics = metric_set(rmse)
  )



  # Finalize workflow for final model
final_en = workflow_en %>%
  finalize_workflow(select_best(cv_en, 'rmse'))

# Wants penalty = 0.01, mixture = 0.3



# ------------------------------ Random Forest --------------------------------


# One more (Logistic Regression?)
```

### Estimate CV Error

``` r
#  Estimate your error (with an appropriately chosen metric) using cross validation

# Linear Regression Root Mean Square Error
cv_reg %>% collect_metrics()
```

    ## # A tibble: 1 x 6
    ##   .metric .estimator    mean     n std_err .config             
    ##   <chr>   <chr>        <dbl> <int>   <dbl> <fct>               
    ## 1 rmse    standard   111122.     5  11556. Preprocessor1_Model1

``` r
# Good place to Create figures!!!!
```

# Prediction

### Test on test data

### Turn in:

1.  Link to this github page with (commented) analysis code and figures
2.  1-page “executive summary”
3.  4-slide summary following paragraphs 2–5 of the executive summary
4.  Evaluation of your group member’s contribution (doesn’t apply)


<!-- README.md is generated from README.Rmd. Please edit that file -->

# wizard2

## Windowed Summarization for Autoregressive Data

This package uses windowed summarization to convert time series data
into a form that can be modeled by prediction models.

<!-- badges: start -->

<!-- badges: end -->

## Installation

You can install the GitHub version of wizard2 with:

``` r
remotes::install_github('ML4LHS/wizard2')
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(wizard2)

wf = wiz_frame(fixed_data = sample_fixed_data,
               temporal_data = sample_temporal_data %>% dplyr::filter(id %in% 1:5),
               fixed_id = 'id',
               fixed_start = 'admit_time',
               temporal_id = 'id',
               temporal_time = 'time',
               temporal_variable = 'variable',
               temporal_value = 'value',
               step = lubridate::hours(6),
               output_folder = 'Z:/kdpsingh/wizard_test_output')

wiz_pred = wf %>% 
  wiz_add_predictors(variable = 'cr',
                     lookback = lubridate::hours(12), 
                     window = lubridate::hours(6), 
                     stats = c(mean = mean,
                               min = min,
                               max = max,
                               median = median,
                               length = length),
                     output_file = FALSE)
#> Anticipated number of rows in intermediate output: 548
#> Anticipated number of rows in final output: 274
#> Number of ids to process: 5
#>   |                                                                                                       |                                                                                               |   0%  |                                                                                                       |===================                                                                            |  20%  |                                                                                                       |======================================                                                         |  40%  |                                                                                                       |=========================================================                                      |  60%  |                                                                                                       |============================================================================                   |  80%  |                                                                                                       |===============================================================================================| 100%

wiz_outcome = wf %>% 
  wiz_add_outcomes(variable = 'cr',
                   lookahead = lubridate::hours(24), 
                   stats = c(max = max),
                   output_file = FALSE)
#> Anticipated number of rows in intermediate output: 274
#> Anticipated number of rows in final output: 274
#> Number of ids to process: 5
#>   |                                                                                                       |                                                                                               |   0%  |                                                                                                       |===================                                                                            |  20%  |                                                                                                       |======================================                                                         |  40%  |                                                                                                       |=========================================================                                      |  60%  |                                                                                                       |============================================================================                   |  80%  |                                                                                                       |===============================================================================================| 100%

model_data = wiz_combine(wf, wiz_pred, wiz_outcome)
#> Joining, by = "id"
#> Joining, by = c("id", "time")

dplyr::glimpse(model_data)
#> Rows: 274
#> Columns: 18
#> $ id                [3m[38;5;246m<int>[39m[23m 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,...
#> $ sex               [3m[38;5;246m<chr>[39m[23m "male", "male", "male", "male", "male", "male", "male", "male", "male", "m...
#> $ age               [3m[38;5;246m<dbl>[39m[23m 66.15955, 66.15955, 66.15955, 66.15955, 66.15955, 66.15955, 66.15955, 66.1...
#> $ race              [3m[38;5;246m<chr>[39m[23m "asian", "asian", "asian", "asian", "asian", "asian", "asian", "asian", "a...
#> $ baseline_cr       [3m[38;5;246m<dbl>[39m[23m 1.001175, 1.001175, 1.001175, 1.001175, 1.001175, 1.001175, 1.001175, 1.00...
#> $ admit_time        [3m[38;5;246m<dttm>[39m[23m 2019-06-02 00:49:23, 2019-06-02 00:49:23, 2019-06-02 00:49:23, 2019-06-02...
#> $ time              [3m[38;5;246m<dbl>[39m[23m 0, 6, 12, 18, 24, 30, 36, 42, 48, 54, 60, 66, 72, 78, 84, 90, 96, 102, 108...
#> $ cr_length_06      [3m[38;5;246m<dbl>[39m[23m 0, 3, 1, 1, 1, 3, 2, 1, 2, 1, 1, 3, 1, 2, 2, 1, 2, 1, 1, 1, 3, 0, 2, 3, 0,...
#> $ cr_length_12      [3m[38;5;246m<dbl>[39m[23m 0, 0, 3, 1, 1, 1, 3, 2, 1, 2, 1, 1, 3, 1, 2, 2, 1, 2, 1, 1, 1, 3, 0, 2, 3,...
#> $ cr_max_06         [3m[38;5;246m<dbl>[39m[23m NA, 1.2638285, 1.0766478, 1.1130560, 1.2141608, 1.0844537, 1.0819543, 1.06...
#> $ cr_max_12         [3m[38;5;246m<dbl>[39m[23m NA, NA, 1.2638285, 1.0766478, 1.1130560, 1.2141608, 1.0844537, 1.0819543, ...
#> $ cr_mean_06        [3m[38;5;246m<dbl>[39m[23m NA, 1.1086468, 1.0766478, 1.1130560, 1.2141608, 1.0472493, 1.0331888, 1.06...
#> $ cr_mean_12        [3m[38;5;246m<dbl>[39m[23m NA, NA, 1.1086468, 1.0766478, 1.1130560, 1.2141608, 1.0472493, 1.0331888, ...
#> $ cr_median_06      [3m[38;5;246m<dbl>[39m[23m NA, 1.1826917, 1.0766478, 1.1130560, 1.2141608, 1.0334277, 1.0331888, 1.06...
#> $ cr_median_12      [3m[38;5;246m<dbl>[39m[23m NA, NA, 1.1826917, 1.0766478, 1.1130560, 1.2141608, 1.0334277, 1.0331888, ...
#> $ cr_min_06         [3m[38;5;246m<dbl>[39m[23m NA, 0.8794202, 1.0766478, 1.1130560, 1.2141608, 1.0238665, 0.9844234, 1.06...
#> $ cr_min_12         [3m[38;5;246m<dbl>[39m[23m NA, NA, 0.8794202, 1.0766478, 1.1130560, 1.2141608, 1.0238665, 0.9844234, ...
#> $ outcome_cr_max_24 [3m[38;5;246m<dbl>[39m[23m 1.263829, 1.214161, 1.214161, 1.214161, 1.201832, 1.201832, 1.201832, 1.24...
```

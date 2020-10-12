
<!-- README.md is generated from README.Rmd. Please edit that file -->

# wizard

## Windowed Summarization for Autoregressive Data

This package uses windowed summarization to convert time series data
into a form that can be modeled by prediction models.

<!-- badges: start -->

[![Lifecycle:
maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
<!-- badges: end -->

## Installation

You can install the GitHub version of wizard with:

``` r
remotes::install_github('ML4LHS/wizard')
```

## How to set up a wiz\_frame()

Start by loading and package and defining your `wiz_frame()`. A
`wiz_frame` is simply a list with the class `wiz_frame` and contains all
the key information needed to describe both your fixed dataset (such as
demographics, one row per patient) and your temporal dataset (one row
per observation linked to a timestamp).

``` r
library(wizard)
```

``` r
library(magrittr)
library(lubridate)

future::plan('multisession')

unlink(file.path(tempdir(), 'wizard_dir', '*.*'))

wf = wiz_frame(fixed_data = sample_fixed_data,
               temporal_data = sample_temporal_data %>% dplyr::filter(id %in% 1:100),
               fixed_id = 'id',
               fixed_start = 'admit_time',
               fixed_end = 'dc_time',
               temporal_id = 'id',
               temporal_time = 'time',
               temporal_variable = 'variable',
               temporal_category = 'category',
               temporal_value = 'value',
               step = hours(6),
               max_length = days(7), # optional parameter to limit to first 7 days of hospitalization
               output_folder = file.path(tempdir(), 'wizard_dir'),
               create_folder = TRUE)
```

## Let’s look at the automatically generated data dictionaries

``` r
names(wf)
#>  [1] "fixed_data"         "temporal_data"      "fixed_id"           "fixed_start"       
#>  [5] "fixed_end"          "temporal_id"        "temporal_time"      "temporal_variable" 
#>  [9] "temporal_value"     "temporal_category"  "step"               "max_length"        
#> [13] "step_units"         "output_folder"      "fixed_data_dict"    "temporal_data_dict"
#> [17] "batch_size"

wf$step
#> [1] 6

wf$step_units
#> [1] "hour"

wf$fixed_data_dict
#>      variable     class
#> 1          id   integer
#> 2         sex character
#> 3         age   numeric
#> 4        race character
#> 5 baseline_cr   numeric
#> 6  admit_time   POSIXct
#> 7     dc_time   POSIXct

wf$temporal_data_dict
#>   variable     class
#> 1       cr   numeric
#> 2  cr_abnl character
#> 3  cr_high character
#> 4      med character
```

## Let’s dummy code the temporal categorical variables

``` r
wf = wf %>% 
  wiz_dummy_code()
```

This affects only the temporal data and not the fixed data.

``` r
wf$fixed_data_dict
#>      variable     class
#> 1          id   integer
#> 2         sex character
#> 3         age   numeric
#> 4        race character
#> 5 baseline_cr   numeric
#> 6  admit_time   POSIXct
#> 7     dc_time   POSIXct

wf$temporal_data_dict
#>              variable   class
#> 1                  cr numeric
#> 2        cr_abnl_high numeric
#> 3         cr_abnl_low numeric
#> 4      cr_abnl_normal numeric
#> 5          cr_high_no numeric
#> 6         cr_high_yes numeric
#> 7   med_acetaminophen numeric
#> 8         med_aspirin numeric
#> 9 med_diphenhydramine numeric
```

## Let’s add some predictors and outcomes

The default method writes output to the folder defined in your
`wiz_frame`. When you write your output to file, you are allowed to
chain together `add_predictors()` and `add_outcomes()` functions. This
is possble because these functions invisibly return a `wiz_frame`.

If, however, you set `output_file` to `FALSE`, then your actual output
is returned (rather than the `wiz_frame`) so you cannot chain functions.

``` r
wf %>%           
  wiz_add_predictors(variables = 'cr', # Note: You can supply a vector of variables
                     lookback = hours(12), 
                     window = hours(6), 
                     stats = c(mean = mean,
                               min = min,
                               max = max,
                               median = median,
                               length = length)) %>%
  wiz_add_baseline_predictors(variables = 'cr', # add baseline creatinine
                              lookback = days(90),
                              offset = hours(10),
                              stats = c(min = min)) %>%
  wiz_add_predictors(category = 'med', # Note: category is always a regular expression 
                     lookback = days(7),
                     stats = c(sum = sum)) %>% 
  wiz_add_outcomes(variables = 'cr',
                   lookahead = hours(24), 
                   stats = c(max = max))
#> Joining, by = "id"
#> Processing variables: cr...
#> Allocating memory...
#> Number of rows in final output: 1540
#> Parallel processing is ENABLED.
#> Determining missing values for each statistic...
#> Beginning calculation...
#>  Progress: --------------------------------------                           100% Progress: ----------------------------------------------------------       100% Progress: --------------------------------------------------------------   100% Progress: ---------------------------------------------------------------  100% Progress: ---------------------------------------------------------------  100% Progress: ---------------------------------------------------------------- 100%
#> Completed calculation.
#> The output file was written to: C:\Users\kdpsingh\AppData\Local\Temp\2\RtmpAJcfQs/wizard_dir/temporal_predictors_variables_cr_2020_10_12_17_41_19.csv
#> Joining, by = "id"
#> Processing variables: cr...
#> Allocating memory...
#> Number of rows in final output: 100
#> Parallel processing is ENABLED.
#> Determining missing values for each statistic...
#> Beginning calculation...
#> Completed calculation.
#> The output file was written to: C:\Users\kdpsingh\AppData\Local\Temp\2\RtmpAJcfQs/wizard_dir/temporal_predictors_variables_cr_2020_10_12_17_41_21.csv
#> Joining, by = "id"
#> Processing category: med...
#> Allocating memory...
#> Number of rows in final output: 1540
#> Parallel processing is ENABLED.
#> Determining missing values for each statistic...
#> Beginning calculation...
#>  Progress: -------------------------------------------                      100% Progress: --------------------------------------------------------------   100% Progress: ---------------------------------------------------------------  100% Progress: ---------------------------------------------------------------- 100%
#> Completed calculation.
#> The output file was written to: C:\Users\kdpsingh\AppData\Local\Temp\2\RtmpAJcfQs/wizard_dir/temporal_predictors_category_med_2020_10_12_17_41_41.csv
#> Joining, by = "id"
#> Processing variables: cr...
#> Allocating memory...
#> Number of rows in final output: 1540
#> Parallel processing is ENABLED.
#> Determining missing values for each statistic...
#> Beginning calculation...
#>  Progress: -------------------------------------------------                100% Progress: ---------------------------------------------------------------  100% Progress: ---------------------------------------------------------------- 100%
#> Completed calculation.
#> The output file was written to: C:\Users\kdpsingh\AppData\Local\Temp\2\RtmpAJcfQs/wizard_dir/temporal_outcomes_variables_cr_2020_10_12_17_42_00.csv
```

## Let’s combine our output into a single data frame

You can either provide `wiz_combine()` with a set of data frames or
files separated by commas. Or, now you can provide a vector of file
names using the `files` argument.

This resulting frame is essentially ready for modeling (using
`tidymodels`, for example). Make sure to keep individual patients in the
same fold if you divide this dataset into multiple
folds.

``` r
model_data = wiz_combine(wf, files = dir(file.path(tempdir(), 'wizard_dir'), pattern = '*.csv'))
#> Joining, by = "id"
#> Joining, by = c("id", "time")
#> Joining, by = c("id", "time")
#> Joining, by = "id"

head(model_data)
#>   id  sex      age  race baseline_cr          admit_time             dc_time time
#> 1  1 male 66.15955 asian    1.001175 2019-06-02 00:49:23 2019-06-08 10:38:23    0
#> 2  1 male 66.15955 asian    1.001175 2019-06-02 00:49:23 2019-06-08 10:38:23    6
#> 3  1 male 66.15955 asian    1.001175 2019-06-02 00:49:23 2019-06-08 10:38:23   12
#> 4  1 male 66.15955 asian    1.001175 2019-06-02 00:49:23 2019-06-08 10:38:23   18
#> 5  1 male 66.15955 asian    1.001175 2019-06-02 00:49:23 2019-06-08 10:38:23   24
#> 6  1 male 66.15955 asian    1.001175 2019-06-02 00:49:23 2019-06-08 10:38:23   30
#>   outcome_cr_max_24 med_acetaminophen_sum_168 med_aspirin_sum_168 med_diphenhydramine_sum_168
#> 1          1.217020                         0                   0                           0
#> 2          1.217020                         0                   0                           0
#> 3          1.217020                         1                   0                           0
#> 4          1.179722                         1                   0                           0
#> 5          1.274939                         1                   0                           0
#> 6          1.274939                         1                   0                           0
#>   cr_length_06 cr_length_12 cr_max_06 cr_max_12 cr_mean_06 cr_mean_12 cr_median_06 cr_median_12
#> 1            1            1  1.003659  1.030098   1.003659   1.030098     1.003659     1.030098
#> 2            0            1  1.003659  1.003659   1.003659   1.003659     1.003659     1.003659
#> 3            1            0  1.039322        NA   1.039322         NA     1.039322           NA
#> 4            2            1  1.217020  1.039322   1.109985   1.039322     1.109985     1.039322
#> 5            1            2  1.179722  1.217020   1.179722   1.109985     1.179722     1.109985
#> 6            3            1  1.165989  1.179722   1.069630   1.179722     1.096827     1.179722
#>   cr_min_06 cr_min_12 baseline_cr_min_2160
#> 1 1.0036587  1.030098                   NA
#> 2 1.0036587  1.003659                   NA
#> 3 1.0393216        NA                   NA
#> 4 1.0029506  1.039322                   NA
#> 5 1.1797219  1.002951                   NA
#> 6 0.9460735  1.179722                   NA
```

## Testing wiz\_frame without writing output to files

If you want to simply test `wiz_frame`, you may prefer not to write your
output to file. You can accomplish this by setting `output_file` to
`FALSE`.

``` r
wf %>% 
  wiz_add_predictors(variables = 'cr',
                     lookback = hours(12), 
                     window = hours(6), 
                     stats = c(mean = mean,
                               min = min,
                               max = max,
                               median = median,
                               length = length),
                     output_file = FALSE) %>% 
  head()
#> Joining, by = "id"
#> Processing variables: cr...
#> Allocating memory...
#> Number of rows in final output: 1540
#> Parallel processing is ENABLED.
#> Determining missing values for each statistic...
#> Beginning calculation...
#>  Progress: ------------------------------------------                       100% Progress: --------------------------------------------------------------   100% Progress: ---------------------------------------------------------------  100% Progress: ---------------------------------------------------------------- 100%
#> Completed calculation.
#>   id time cr_length_06 cr_length_12 cr_max_06 cr_max_12 cr_mean_06 cr_mean_12 cr_median_06
#> 1  1    0            1            1  1.003659  1.030098   1.003659   1.030098     1.003659
#> 2  1    6            0            1  1.003659  1.003659   1.003659   1.003659     1.003659
#> 3  1   12            1            0  1.039322        NA   1.039322         NA     1.039322
#> 4  1   18            2            1  1.217020  1.039322   1.109985   1.039322     1.109985
#> 5  1   24            1            2  1.179722  1.217020   1.179722   1.109985     1.179722
#> 6  1   30            3            1  1.165989  1.179722   1.069630   1.179722     1.096827
#>   cr_median_12 cr_min_06 cr_min_12
#> 1     1.030098 1.0036587  1.030098
#> 2     1.003659 1.0036587  1.003659
#> 3           NA 1.0393216        NA
#> 4     1.039322 1.0029506  1.039322
#> 5     1.109985 1.1797219  1.002951
#> 6     1.179722 0.9460735  1.179722
```

## You can also supply a vector of variables

``` r
wf %>% 
  wiz_add_predictors(variables = c('cr', 'med_aspirin'),
                     lookback = weeks(1), 
                     stats = c(length = length),
                     output_file = FALSE) %>% 
  head()
#> Joining, by = "id"
#> Processing variables: cr, med_aspirin...
#> Allocating memory...
#> Number of rows in final output: 1540
#> Parallel processing is ENABLED.
#> Determining missing values for each statistic...
#> Beginning calculation...
#>  Progress: ------------------------------------------                       100% Progress: ------------------------------------------------------------     100% Progress: ---------------------------------------------------------------  100% Progress: ---------------------------------------------------------------- 100%
#> Completed calculation.
#>   id time cr_length_168 med_aspirin_length_168
#> 1  1    0             2                      0
#> 2  1    6             2                      0
#> 3  1   12             3                      0
#> 4  1   18             5                      0
#> 5  1   24             6                      0
#> 6  1   30             9                      0
```

## Category accepts regular expressions

``` r
wf %>% 
  wiz_add_predictors(category = 'lab|med',
                     lookback = hours(12), 
                     stats = c(length = length),
                     output_file = FALSE) %>% 
  head()
#> Joining, by = "id"
#> Processing category: lab|med...
#> Allocating memory...
#> Number of rows in final output: 1540
#> Parallel processing is ENABLED.
#> Determining missing values for each statistic...
#> Beginning calculation...
#>  Progress: ------------------------------------------                       100% Progress: ------------------------------------------------------------     100% Progress: ---------------------------------------------------------------  100% Progress: ---------------------------------------------------------------- 100%
#> Completed calculation.
#>   id time cr_length_12 med_acetaminophen_length_12 med_aspirin_length_12
#> 1  1    0            2                           0                     0
#> 2  1    6            1                           0                     0
#> 3  1   12            1                           1                     0
#> 4  1   18            3                           1                     0
#> 5  1   24            3                           0                     0
#> 6  1   30            4                           0                     0
#>   med_diphenhydramine_length_12
#> 1                             0
#> 2                             0
#> 3                             0
#> 4                             0
#> 5                             0
#> 6                             0
```

## Let’s benchmark the performance on our package

### Running in parallel

``` r
benchmark_results = list()

future::plan('multisession')

benchmark_results[['multisession']] = 
  microbenchmark::microbenchmark(
    wf %>% 
      wiz_add_predictors(variable = 'cr',
                         lookback = hours(48), 
                         window = hours(6), 
                         stats = c(mean = mean,
                                   min = min,
                                   max = max,
                                   median = median,
                                   length = length)),
    times = 1
  )
#>  Progress: ---------------------------------------                          100% Progress: -----------------------------------------------------------      100% Progress: ---------------------------------------------------------------  100% Progress: ---------------------------------------------------------------- 100%
```

### Running in parallel with a batch\_size of 20

``` r

wf_with_chunks = wf
wf_with_chunks$batch_size = 20

benchmark_results[['multisession with batch_size 20']] = 
  microbenchmark::microbenchmark(
    wf_with_chunks %>% 
      wiz_add_predictors(variable = 'cr',
                         lookback = hours(48), 
                         window = hours(6), 
                         stats = c(mean = mean,
                                   min = min,
                                   max = max,
                                   median = median,
                                   length = length)),
    times = 1
  )
#> Processing chunk # 1 out of 5...
#> Joining, by = "id"
#> Processing variables: cr...
#> Allocating memory...
#> Number of rows in final output: 270
#> Parallel processing is ENABLED.
#> Determining missing values for each statistic...
#> Beginning calculation...
#>  Progress: ---------------------------------------------------------------- 100%
#> Completed calculation.
#> The output file was written to: C:\Users\kdpsingh\AppData\Local\Temp\2\RtmpAJcfQs/wizard_dir/chunk_1_temporal_predictors_variables_cr_2020_10_12_17_43_49.csv
#> Processing chunk # 2 out of 5...
#> Joining, by = "id"
#> Processing variables: cr...
#> Allocating memory...
#> Number of rows in final output: 294
#> Parallel processing is ENABLED.
#> Determining missing values for each statistic...
#> Beginning calculation...
#>  Progress: ---------------------------------------------------------------- 100%
#> Completed calculation.
#> The output file was written to: C:\Users\kdpsingh\AppData\Local\Temp\2\RtmpAJcfQs/wizard_dir/chunk_2_temporal_predictors_variables_cr_2020_10_12_17_43_54.csv
#> Processing chunk # 3 out of 5...
#> Joining, by = "id"
#> Processing variables: cr...
#> Allocating memory...
#> Number of rows in final output: 309
#> Parallel processing is ENABLED.
#> Determining missing values for each statistic...
#> Beginning calculation...
#>  Progress: ---------------------------------------------------------------- 100%
#> Completed calculation.
#> The output file was written to: C:\Users\kdpsingh\AppData\Local\Temp\2\RtmpAJcfQs/wizard_dir/chunk_3_temporal_predictors_variables_cr_2020_10_12_17_44_00.csv
#> Processing chunk # 4 out of 5...
#> Joining, by = "id"
#> Processing variables: cr...
#> Allocating memory...
#> Number of rows in final output: 345
#> Parallel processing is ENABLED.
#> Determining missing values for each statistic...
#> Beginning calculation...
#>  Progress: ---------------------------------------------------------------- 100%
#> Completed calculation.
#> The output file was written to: C:\Users\kdpsingh\AppData\Local\Temp\2\RtmpAJcfQs/wizard_dir/chunk_4_temporal_predictors_variables_cr_2020_10_12_17_44_06.csv
#> Processing chunk # 5 out of 5...
#> Joining, by = "id"
#> Processing variables: cr...
#> Allocating memory...
#> Number of rows in final output: 322
#> Parallel processing is ENABLED.
#> Determining missing values for each statistic...
#> Beginning calculation...
#>  Progress: ---------------------------------------------------------------- 100%
#> Completed calculation.
#> The output file was written to: C:\Users\kdpsingh\AppData\Local\Temp\2\RtmpAJcfQs/wizard_dir/chunk_5_temporal_predictors_variables_cr_2020_10_12_17_44_12.csv
```

### Running in serial

``` r
future::plan('sequential')

benchmark_results[['sequential']] = 
  microbenchmark::microbenchmark(
  wf %>% 
    wiz_add_predictors(variable = 'cr',
                       lookback = hours(48), 
                       window = hours(6), 
                       stats = c(mean = mean,
                                 min = min,
                                 max = max,
                                 median = median,
                                 length = length)),
  times = 1
  )
```

## Benchmark results

``` r

benchmark_results
#> $multisession
#> Unit: seconds
#>                                                                                                                                                                               expr
#>  wf %>% wiz_add_predictors(variable = "cr", lookback = hours(48),      window = hours(6), stats = c(mean = mean, min = min, max = max,          median = median, length = length))
#>       min       lq     mean   median       uq      max neval
#>  22.38963 22.38963 22.38963 22.38963 22.38963 22.38963     1
#> 
#> $`multisession with batch_size 20`
#> Unit: seconds
#>                                                                                                                                                                                           expr
#>  wf_with_chunks %>% wiz_add_predictors(variable = "cr", lookback = hours(48),      window = hours(6), stats = c(mean = mean, min = min, max = max,          median = median, length = length))
#>      min      lq    mean  median      uq     max neval
#>  26.7992 26.7992 26.7992 26.7992 26.7992 26.7992     1
#> 
#> $sequential
#> Unit: seconds
#>                                                                                                                                                                               expr
#>  wf %>% wiz_add_predictors(variable = "cr", lookback = hours(48),      window = hours(6), stats = c(mean = mean, min = min, max = max,          median = median, length = length))
#>       min       lq     mean   median       uq      max neval
#>  200.8695 200.8695 200.8695 200.8695 200.8695 200.8695     1
```

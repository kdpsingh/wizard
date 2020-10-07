
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
               temporal_data = sample_temporal_data %>% dplyr::filter(id %in% 1:10),
               fixed_id = 'id',
               fixed_start = 'admit_time',
               fixed_end = 'dc_time',
               temporal_id = 'id',
               temporal_time = 'time',
               temporal_variable = 'variable',
               temporal_category = 'category',
               temporal_value = 'value',
               step = hours(6),
               output_folder = file.path(tempdir(), 'wizard_dir'),
               save_wiz_frame = TRUE)
```

## Let’s look at the automatically generated data dictionaries

``` r
names(wf)
#>  [1] "fixed_data"         "temporal_data"      "fixed_id"           "fixed_start"       
#>  [5] "fixed_end"          "temporal_id"        "temporal_time"      "temporal_variable" 
#>  [9] "temporal_value"     "temporal_category"  "step"               "step_units"        
#> [13] "output_folder"      "fixed_data_dict"    "temporal_data_dict"

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

## Let’s dummy-code the temporal categorical variables

``` r
wf = wf %>% 
  wiz_dummy_code(save_wiz_frame = TRUE)
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
#> 3      cr_abnl_normal numeric
#> 4          cr_high_no numeric
#> 5         cr_high_yes numeric
#> 6   med_acetaminophen numeric
#> 7         med_aspirin numeric
#> 8 med_diphenhydramine numeric
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
                               length = length),
                     log_file = TRUE) %>%
  wiz_add_baseline_predictors(variables = 'cr', # add baseline creatinine
                     lookback = days(90),
                     offset = hours(10),
                     stats = c(min = min)) %>%
  wiz_add_predictors(category = 'med', # Note: category is always a regular expression 
                     lookback = days(7),
                     offset = days(1),
                     stats = c(sum = sum,
                               any = any),
                     impute = FALSE) %>% # Note: do *not* perform carry-forward imputation 
  wiz_add_outcomes(variables = 'cr',
                   lookahead = hours(24), 
                   stats = c(max = max))
#> Joining, by = "id"
#> Processing variables: cr...
#> Anticipated number of rows in intermediate output: 272
#> Anticipated number of rows in final output: 136
#> Allocating memory...
#> Parallel processing is ENABLED.
#> Beginning calculation...
#>  Progress: ---------------------------------------------------------------- 100%
#> Completed calculation.
#> Performing LOCF imputation...
#> Completed data cleanup.
#> The output file was written to: C:\Users\kdpsingh\AppData\Local\Temp\2\RtmpQVphPi/wizard_dir/temporal_predictors_variables_cr_2020_10_07_01_30_14.csv
#> Joining, by = "id"
#> Processing variables: cr...
#> Anticipated number of rows in intermediate output: 10
#> Anticipated number of rows in final output: 10
#> Allocating memory...
#> Parallel processing is ENABLED.
#> Beginning calculation...
#> Completed calculation.
#> Performing LOCF imputation...
#> Completed data cleanup.
#> The output file was written to: C:\Users\kdpsingh\AppData\Local\Temp\2\RtmpQVphPi/wizard_dir/temporal_predictors_variables_cr_2020_10_07_01_30_15.csv
#> Joining, by = "id"
#> Warning in .Primitive("any")(c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, : coercing argument of type
#> 'double' to logical
#> Processing category: med...
#> Anticipated number of rows in intermediate output: 10
#> Anticipated number of rows in final output: 10
#> Allocating memory...
#> Parallel processing is ENABLED.
#> Beginning calculation...
#> Completed calculation.
#> Completed data cleanup.
#> The output file was written to: C:\Users\kdpsingh\AppData\Local\Temp\2\RtmpQVphPi/wizard_dir/temporal_predictors_category_med_2020_10_07_01_30_15.csv
#> Joining, by = "id"
#> Processing variables: cr...
#> Anticipated number of rows in intermediate output: 10
#> Anticipated number of rows in final output: 10
#> Allocating memory...
#> Parallel processing is ENABLED.
#> Beginning calculation...
#> Completed calculation.
#> Completed data cleanup.
#> The output file was written to: C:\Users\kdpsingh\AppData\Local\Temp\2\RtmpQVphPi/wizard_dir/temporal_outcomes_variables_cr_2020_10_07_01_30_16.csv
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
#> Joining, by = c("id", "time")

head(model_data)
#>   id    sex      age  race baseline_cr          admit_time             dc_time time outcome_cr_max_24
#> 1  1   male 66.15955 asian   1.0011752 2019-06-02 00:49:23 2019-06-08 10:38:23    0          1.217020
#> 2  2 female 60.54697 white   0.7166654 2019-06-07 14:12:21 2019-06-13 18:31:21    0          0.821473
#> 3  3   male 47.25137 white   0.9207885 2019-08-29 12:47:56 2019-08-30 23:56:56    0          1.145234
#> 4  4   male 65.16939 black   1.1288818 2019-03-13 21:24:14 2019-03-15 23:39:14    0          1.312333
#> 5  5 female 79.87402 other   0.9292875 2019-02-11 02:46:16 2019-02-15 01:35:16    0          1.051864
#> 6  6   male 88.90951 asian   1.0568886 2019-09-15 20:38:50 2019-09-17 03:58:50    0          1.357860
#>   med_acetaminophen_any_168 med_acetaminophen_sum_168 med_aspirin_any_168 med_aspirin_sum_168
#> 1                        NA                        NA                  NA                  NA
#> 2                        NA                        NA                  NA                  NA
#> 3                        NA                        NA                  NA                  NA
#> 4                        NA                        NA                  NA                  NA
#> 5                        NA                        NA                  NA                  NA
#> 6                        NA                        NA                  NA                  NA
#>   med_diphenhydramine_any_168 med_diphenhydramine_sum_168 cr_length_06 cr_length_12 cr_max_06
#> 1                          NA                          NA            1            1 1.0036587
#> 2                          NA                          NA            1           NA 0.6381981
#> 3                          NA                          NA            1           NA 0.9695346
#> 4                          NA                          NA            3            1 1.3123332
#> 5                          NA                          NA           NA            1        NA
#> 6                          NA                          NA            2           NA 1.3578598
#>   cr_max_12 cr_mean_06 cr_mean_12 cr_median_06 cr_median_12 cr_min_06 cr_min_12 baseline_cr_min_2160
#> 1 1.0300977  1.0036587  1.0300977    1.0036587    1.0300977 1.0036587 1.0300977                   NA
#> 2        NA  0.6381981         NA    0.6381981           NA 0.6381981        NA                   NA
#> 3        NA  0.9695346         NA    0.9695346           NA 0.9695346        NA                   NA
#> 4 1.0119205  1.2982510  1.0119205    1.3117746    1.0119205 1.2706452 1.0119205                   NA
#> 5 0.9786329         NA  0.9786329           NA    0.9786329        NA 0.9786329            0.9786329
#> 6        NA  1.3380433         NA    1.3380433           NA 1.3182267        NA                   NA
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
#> Anticipated number of rows in intermediate output: 272
#> Anticipated number of rows in final output: 136
#> Allocating memory...
#> Parallel processing is ENABLED.
#> Beginning calculation...
#>  Progress: ---------------------------------------------------------------- 100%
#> Completed calculation.
#> Performing LOCF imputation...
#> Completed data cleanup.
#>   id time cr_length_06 cr_length_12 cr_max_06 cr_max_12 cr_mean_06 cr_mean_12 cr_median_06
#> 1  1    0            1            1  1.003659  1.030098   1.003659   1.030098     1.003659
#> 2  1    6            1            1  1.003659  1.003659   1.003659   1.003659     1.003659
#> 3  1   12            1            1  1.039322  1.003659   1.039322   1.003659     1.039322
#> 4  1   18            2            1  1.217020  1.039322   1.109985   1.039322     1.109985
#> 5  1   24            1            2  1.179722  1.217020   1.179722   1.109985     1.179722
#> 6  1   30            3            1  1.165989  1.179722   1.069630   1.179722     1.096827
#>   cr_median_12 cr_min_06 cr_min_12
#> 1     1.030098 1.0036587  1.030098
#> 2     1.003659 1.0036587  1.003659
#> 3     1.003659 1.0393216  1.003659
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
#> Anticipated number of rows in intermediate output: 136
#> Anticipated number of rows in final output: 136
#> Allocating memory...
#> Parallel processing is ENABLED.
#> Beginning calculation...
#> Completed calculation.
#> Performing LOCF imputation...
#> Completed data cleanup.
#>   id time cr_length_168 med_aspirin_length_168
#> 1  1    0             2                     NA
#> 2  1    6             2                     NA
#> 3  1   12             3                     NA
#> 4  1   18             5                     NA
#> 5  1   24             6                     NA
#> 6  1   30             9                     NA
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
#> Anticipated number of rows in intermediate output: 136
#> Anticipated number of rows in final output: 136
#> Allocating memory...
#> Parallel processing is ENABLED.
#> Beginning calculation...
#> Completed calculation.
#> Performing LOCF imputation...
#> Completed data cleanup.
#>   id time cr_length_12 med_acetaminophen_length_12 med_aspirin_length_12
#> 1  1    0            2                          NA                    NA
#> 2  1    6            1                          NA                    NA
#> 3  1   12            1                           1                    NA
#> 4  1   18            3                           1                    NA
#> 5  1   24            3                          NA                    NA
#> 6  1   30            4                          NA                    NA
#>   med_diphenhydramine_length_12
#> 1                            NA
#> 2                            NA
#> 3                            NA
#> 4                            NA
#> 5                            NA
#> 6                            NA
```

## Let’s benchmark the performance on our package

### Running in parallel

``` r
benchmark_results = list()

future::plan('multiprocess')

benchmark_results[['multiprocess']] = 
  microbenchmark::microbenchmark(
    wf %>% 
      wiz_add_predictors(variable = 'cr',
                         lookback = hours(12), 
                         window = hours(6), 
                         stats = c(mean = mean,
                                   min = min,
                                   max = max,
                                   median = median,
                                   length = length),
                         output_file = FALSE),
    times = 1
  )
#>  Progress: ---------------------------------------------------------------- 100%
```

### Running in serial

``` r
future::plan('sequential')

benchmark_results[['sequential']] = 
  microbenchmark::microbenchmark(
  wf %>% 
    wiz_add_predictors(variable = 'cr',
                       lookback = hours(12), 
                       window = hours(6), 
                       stats = c(mean = mean,
                                 min = min,
                                 max = max,
                                 median = median,
                                 length = length),
                       output_file = FALSE),
  times = 1
  )
```

## Benchmark results

``` r
benchmark_results
#> $multiprocess
#> Unit: seconds
#>                                                                                                                                                                                                    expr
#>  wf %>% wiz_add_predictors(variable = "cr", lookback = hours(12),      window = hours(6), stats = c(mean = mean, min = min, max = max,          median = median, length = length), output_file = FALSE)
#>       min       lq     mean   median       uq      max neval
#>  4.257754 4.257754 4.257754 4.257754 4.257754 4.257754     1
#> 
#> $sequential
#> Unit: seconds
#>                                                                                                                                                                                                    expr
#>  wf %>% wiz_add_predictors(variable = "cr", lookback = hours(12),      window = hours(6), stats = c(mean = mean, min = min, max = max,          median = median, length = length), output_file = FALSE)
#>       min       lq     mean   median       uq      max neval
#>  9.555469 9.555469 9.555469 9.555469 9.555469 9.555469     1
```

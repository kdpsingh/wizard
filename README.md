
<!-- README.md is generated from README.Rmd. Please edit that file -->

# wizard

## Windowed Summarization for Autoregressive Data

This package uses windowed summarization to convert time series data
into a form that can be modeled by prediction models.

<!-- badges: start -->

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

future::plan('multiprocess')

unlink(file.path(tempdir(), 'wiz_frame_dir', '*.*'))

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
               output_folder = file.path(tempdir(), 'wiz_frame_dir'))
```

## Let’s look at the automatically generated data dictionaries

``` r
names(wf)
#>  [1] "fixed_data"         "temporal_data"      "fixed_id"           "fixed_start"        "fixed_end"         
#>  [6] "temporal_id"        "temporal_time"      "temporal_variable"  "temporal_value"     "temporal_category" 
#> [11] "step"               "step_units"         "output_folder"      "fixed_data_dict"    "temporal_data_dict"

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
  wiz_categorical_to_numeric()
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
#> 2      cr_abnl_normal numeric
#> 3          cr_high_no numeric
#> 4   med_acetaminophen numeric
#> 5         med_aspirin numeric
#> 6 med_diphenhydramine numeric
#> 7        cr_abnl_high numeric
#> 8         cr_high_yes numeric
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
  wiz_add_predictors(variable = 'cr', # Note: You can supply a vector of variables
                     lookback = hours(12), 
                     window = hours(6), 
                     stats = c(mean = mean,
                               min = min,
                               max = max,
                               median = median,
                               length = length)) %>% 
  wiz_add_predictors(category = 'med', # Note: category is always a regular expression 
                     lookback = days(7),
                     stats = c(sum = sum,
                               any = any),
                     impute = FALSE) %>% # Note: do *not* perform carry-forward imputation 
  wiz_add_outcomes(variable = 'cr',
                   lookahead = hours(24), 
                   stats = c(max = max))
#> Joining, by = "id"
#> Processing variable cr...
#> Anticipated number of rows in intermediate output: 272
#> Anticipated number of rows in final output: 136
#>  Progress: ---------------------------------------------------------------- 100%
#> The output file was written to: C:\Users\kdpsingh\AppData\Local\Temp\8\RtmpwpjYBx/wiz_frame_dir/temporal_predictors_variable_cr_2020_10_01_03_39_23.csv
#> Joining, by = "id"
#> Processing category med...
#> Anticipated number of rows in intermediate output: 136
#> Anticipated number of rows in final output: 136
#> The output file was written to: C:\Users\kdpsingh\AppData\Local\Temp\8\RtmpwpjYBx/wiz_frame_dir/temporal_predictors_category_med_2020_10_01_03_39_25.csv
#> Joining, by = "id"
#> Processing variable cr...
#> Anticipated number of rows in intermediate output: 136
#> Anticipated number of rows in final output: 136
#> The output file was written to: C:\Users\kdpsingh\AppData\Local\Temp\8\RtmpwpjYBx/wiz_frame_dir/temporal_outcomes_variable_cr_2020_10_01_03_39_26.csv
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
model_data = wiz_combine(wf, files = dir(file.path(tempdir(), 'wiz_frame_dir')))
#> Joining, by = "id"
#> Joining, by = c("id", "time")
#> Joining, by = c("id", "time")

dplyr::glimpse(model_data)
#> Rows: 136
#> Columns: 25
#> $ id                          [3m[90m<int>[39m[23m 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, ...
#> $ sex                         [3m[90m<chr>[39m[23m "male", "male", "male", "male", "male", "male", "male", "male", "male", "male", "mal...
#> $ age                         [3m[90m<dbl>[39m[23m 66.15955, 66.15955, 66.15955, 66.15955, 66.15955, 66.15955, 66.15955, 66.15955, 66.1...
#> $ race                        [3m[90m<chr>[39m[23m "asian", "asian", "asian", "asian", "asian", "asian", "asian", "asian", "asian", "as...
#> $ baseline_cr                 [3m[90m<dbl>[39m[23m 1.0011752, 1.0011752, 1.0011752, 1.0011752, 1.0011752, 1.0011752, 1.0011752, 1.00117...
#> $ admit_time                  [3m[90m<dttm>[39m[23m 2019-06-02 00:49:23, 2019-06-02 00:49:23, 2019-06-02 00:49:23, 2019-06-02 00:49:23,...
#> $ dc_time                     [3m[90m<dttm>[39m[23m 2019-06-08 10:38:23, 2019-06-08 10:38:23, 2019-06-08 10:38:23, 2019-06-08 10:38:23,...
#> $ time                        [3m[90m<int>[39m[23m 0, 6, 12, 18, 24, 30, 36, 42, 48, 54, 60, 66, 72, 78, 84, 90, 96, 102, 108, 114, 120...
#> $ outcome_cr_max_24           [3m[90m<dbl>[39m[23m 1.2170199, 1.2170199, 1.2170199, 1.1797219, 1.2749385, 1.2749385, 1.2749385, 1.27493...
#> $ med_acetaminophen_any_168   [3m[90m<int>[39m[23m NA, NA, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, NA, ...
#> $ med_acetaminophen_sum_168   [3m[90m<int>[39m[23m NA, NA, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, NA, ...
#> $ med_aspirin_any_168         [3m[90m<int>[39m[23m NA, NA, NA, NA, NA, NA, NA, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,...
#> $ med_aspirin_sum_168         [3m[90m<int>[39m[23m NA, NA, NA, NA, NA, NA, NA, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,...
#> $ med_diphenhydramine_any_168 [3m[90m<int>[39m[23m NA, NA, NA, NA, NA, NA, NA, NA, NA, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, ...
#> $ med_diphenhydramine_sum_168 [3m[90m<int>[39m[23m NA, NA, NA, NA, NA, NA, NA, NA, NA, 1, 1, 1, 1, 1, 1, 1, 2, 2, 3, 3, 3, 3, 3, 3, 4, ...
#> $ cr_length_06                [3m[90m<int>[39m[23m 1, 1, 1, 2, 1, 3, 1, 1, 3, 1, 1, 3, 2, 2, 3, 2, 2, 2, 2, 1, 2, 2, 3, 2, 4, 1, 1, 1, ...
#> $ cr_length_12                [3m[90m<int>[39m[23m 1, 1, 1, 1, 2, 1, 3, 1, 1, 3, 1, 1, 3, 2, 2, 3, 2, 2, 2, 2, 1, 2, 2, 3, 2, 4, NA, 1,...
#> $ cr_max_06                   [3m[90m<dbl>[39m[23m 1.0036587, 1.0036587, 1.0393216, 1.2170199, 1.1797219, 1.1659894, 1.1464653, 1.14646...
#> $ cr_max_12                   [3m[90m<dbl>[39m[23m 1.0300977, 1.0036587, 1.0036587, 1.0393216, 1.2170199, 1.1797219, 1.1659894, 1.14646...
#> $ cr_mean_06                  [3m[90m<dbl>[39m[23m 1.0036587, 1.0036587, 1.0393216, 1.1099852, 1.1797219, 1.0696299, 1.1464653, 1.14646...
#> $ cr_mean_12                  [3m[90m<dbl>[39m[23m 1.0300977, 1.0036587, 1.0036587, 1.0393216, 1.1099852, 1.1797219, 1.0696299, 1.14646...
#> $ cr_median_06                [3m[90m<dbl>[39m[23m 1.0036587, 1.0036587, 1.0393216, 1.1099852, 1.1797219, 1.0968267, 1.1464653, 1.14646...
#> $ cr_median_12                [3m[90m<dbl>[39m[23m 1.0300977, 1.0036587, 1.0036587, 1.0393216, 1.1099852, 1.1797219, 1.0968267, 1.14646...
#> $ cr_min_06                   [3m[90m<dbl>[39m[23m 1.0036587, 1.0036587, 1.0393216, 1.0029506, 1.1797219, 0.9460735, 1.1464653, 1.14646...
#> $ cr_min_12                   [3m[90m<dbl>[39m[23m 1.0300977, 1.0036587, 1.0036587, 1.0393216, 1.0029506, 1.1797219, 0.9460735, 1.14646...
```

## Testing wiz\_frame without writing output to files

If you want to simply test `wiz_frame`, you may prefer not to write your
output to file. You can accomplish this by setting `output_file` to
`FALSE`.

``` r
wf %>% 
  wiz_add_predictors(variable = 'cr',
                     lookback = lubridate::hours(12), 
                     window = lubridate::hours(6), 
                     stats = c(mean = mean,
                               min = min,
                               max = max,
                               median = median,
                               length = length),
                     output_file = FALSE) %>% 
  dplyr::glimpse()
#> Joining, by = "id"
#> Processing variable cr...
#> Anticipated number of rows in intermediate output: 272
#> Anticipated number of rows in final output: 136
#>  Progress: ---------------------------------------------------------------- 100%
#> 
#> Rows: 136
#> Columns: 12
#> $ id           [3m[90m<int>[39m[23m 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, ...
#> $ time         [3m[90m<dbl>[39m[23m 0, 6, 12, 18, 24, 30, 36, 42, 48, 54, 60, 66, 72, 78, 84, 90, 96, 102, 108, 114, 120, 126, 132, 138...
#> $ cr_length_06 [3m[90m<dbl>[39m[23m 1, 1, 1, 2, 1, 3, 1, 1, 3, 1, 1, 3, 2, 2, 3, 2, 2, 2, 2, 1, 2, 2, 3, 2, 4, 1, 1, 1, 4, 1, 2, 1, 2, ...
#> $ cr_length_12 [3m[90m<dbl>[39m[23m 1, 1, 1, 1, 2, 1, 3, 1, 1, 3, 1, 1, 3, 2, 2, 3, 2, 2, 2, 2, 1, 2, 2, 3, 2, 4, NA, 1, 1, 4, 1, 2, 1,...
#> $ cr_max_06    [3m[90m<dbl>[39m[23m 1.0036587, 1.0036587, 1.0393216, 1.2170199, 1.1797219, 1.1659894, 1.1464653, 1.1464653, 1.2749385, ...
#> $ cr_max_12    [3m[90m<dbl>[39m[23m 1.0300977, 1.0036587, 1.0036587, 1.0393216, 1.2170199, 1.1797219, 1.1659894, 1.1464653, 1.1464653, ...
#> $ cr_mean_06   [3m[90m<dbl>[39m[23m 1.0036587, 1.0036587, 1.0393216, 1.1099852, 1.1797219, 1.0696299, 1.1464653, 1.1464653, 1.1271614, ...
#> $ cr_mean_12   [3m[90m<dbl>[39m[23m 1.0300977, 1.0036587, 1.0036587, 1.0393216, 1.1099852, 1.1797219, 1.0696299, 1.1464653, 1.1464653, ...
#> $ cr_median_06 [3m[90m<dbl>[39m[23m 1.0036587, 1.0036587, 1.0393216, 1.1099852, 1.1797219, 1.0968267, 1.1464653, 1.1464653, 1.0651233, ...
#> $ cr_median_12 [3m[90m<dbl>[39m[23m 1.0300977, 1.0036587, 1.0036587, 1.0393216, 1.1099852, 1.1797219, 1.0968267, 1.1464653, 1.1464653, ...
#> $ cr_min_06    [3m[90m<dbl>[39m[23m 1.0036587, 1.0036587, 1.0393216, 1.0029506, 1.1797219, 0.9460735, 1.1464653, 1.1464653, 1.0414223, ...
#> $ cr_min_12    [3m[90m<dbl>[39m[23m 1.0300977, 1.0036587, 1.0036587, 1.0393216, 1.0029506, 1.1797219, 0.9460735, 1.1464653, 1.1464653, ...
```

## You can supply a vector of variables

``` r
wf %>% 
  wiz_add_predictors(variable = c('cr', 'cr_high_no'),
                     lookback = lubridate::weeks(1), 
                     stats = c(any = any),
                     output_file = FALSE) %>% 
  dplyr::glimpse()
#> Joining, by = "id"
#> Processing variable cr, cr_high_no...
#> Anticipated number of rows in intermediate output: 136
#> Anticipated number of rows in final output: 136
#> Rows: 136
#> Columns: 4
#> $ id                 [3m[90m<int>[39m[23m 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, ...
#> $ time               [3m[90m<dbl>[39m[23m 0, 6, 12, 18, 24, 30, 36, 42, 48, 54, 60, 66, 72, 78, 84, 90, 96, 102, 108, 114, 120, 126, 13...
#> $ cr_any_168         [3m[90m<lgl>[39m[23m TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRU...
#> $ cr_high_no_any_168 [3m[90m<lgl>[39m[23m TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRU...
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
                         lookback = lubridate::hours(12), 
                         window = lubridate::hours(6), 
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
                       lookback = lubridate::hours(12), 
                       window = lubridate::hours(6), 
                       stats = c(mean = mean,
                                 min = min,
                                 max = max,
                                 median = median,
                                 length = length),
                       output_file = FALSE),
  times = 1
  )
```

### Implementation in the prior version (using `dplyr::group_modify()`)

``` r
benchmark_results[['group_modify']] =
  microbenchmark::microbenchmark(
    wf %>% 
      wiz_add_predictors_group_modify(variable = 'cr',
                                      lookback = lubridate::hours(12), 
                                      window = lubridate::hours(6), 
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
#>                                                                                                                                                                                                                          expr
#>  wf %>% wiz_add_predictors(variable = "cr", lookback = lubridate::hours(12),      window = lubridate::hours(6), stats = c(mean = mean, min = min,          max = max, median = median, length = length), output_file = FALSE)
#>       min       lq     mean   median       uq      max neval
#>  3.733246 3.733246 3.733246 3.733246 3.733246 3.733246     1
#> 
#> $sequential
#> Unit: seconds
#>                                                                                                                                                                                                                          expr
#>  wf %>% wiz_add_predictors(variable = "cr", lookback = lubridate::hours(12),      window = lubridate::hours(6), stats = c(mean = mean, min = min,          max = max, median = median, length = length), output_file = FALSE)
#>       min       lq     mean   median       uq      max neval
#>  8.568165 8.568165 8.568165 8.568165 8.568165 8.568165     1
#> 
#> $group_modify
#> Unit: seconds
#>                                                                                                                                                                                                                                       expr
#>  wf %>% wiz_add_predictors_group_modify(variable = "cr", lookback = lubridate::hours(12),      window = lubridate::hours(6), stats = c(mean = mean, min = min,          max = max, median = median, length = length), output_file = FALSE)
#>       min       lq     mean   median       uq      max neval
#>  5.714483 5.714483 5.714483 5.714483 5.714483 5.714483     1
```

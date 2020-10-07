# wizard 0.0.0.9000

* Added a `NEWS.md` file to track changes to the package.

# wizard 0.0.0.9001 (2020-10-04)

* Changed `variable` argument to `variables` inside both `wiz_add_predictors()` and `wiz_add_outcomes()`. This was done to reflect the fact that the `variables` argument can accept a vector of variable names.
* Added `check_size_only` argument to `wiz_add_predictors()` and `wiz_add_outcomes()`
* Renamed `wiz_categorical_to_numeric()` to `wiz_dummy_code()` to better reflect what the function does. `wiz_dummy_code()` can now be used to dummy code specific variables, which is useful if you only want to dummy code subset a specific set of variables or if you want to dummy code a categorical variable that is currently coded with numbers (such as 1 meaning high and 2 meaning low).
* Important bug fix to encoding of time to allow for non-time-stamps
* Added `check_size_only` argument to `wiz_add_predictors()` to allow you to anticipate the size of the output object/file without actually running the calcualtions.

# wizard 0.0.0.9002 (2020-10-07)
* Added `create_folder` argument to `wiz_frame()` to allow for automatic creation of the `output_folder` if it does not already exist. This is useful for automated creation of directories to override the need for user confirmation prior to directory creation.
* Added several checks when creating a `wiz_frame()` to ensure that there is not missingness or duplication in the data that would be expected to lead to failure of one of the downstream tasks.
* Added `save_wiz_frame` argument to `wiz_frame()` and `wiz_dummy_code()`. If set to `TRUE`, this saves the `wiz_frame` object to the specified `output_folder` with the file_name `wiz_frame.rds`. Note that this will overwrite prior versions of the file.
* Added `log_file` option to `wiz_add_predictors()` and `wiz_add_outcomes()` to create and append to a log file, which is auto-titled `wiz_log.txt` and saved in the `output_folder`.
* Added `wiz_add_baseline_predictors()` function to add baseline predictors with an option to specify an offset (e.g., up to 1 hour prior to admission)

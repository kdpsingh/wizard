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

# wizard 0.0.0.9003 (2020-10-08)

* All windows are now calculated simultaneously for each step, which greatly reduces the number of parallel jobs (and the time and memory taken by the allocating memory step).
* Removed time column from `wiz_add_baseline_predictors()` so that it is treated as a type of "fixed data", and moved its logic into the `wiz_add_predictors()` function.

# wizard 0.0.0.9004 (2020-10-09)

* Added optional `max_length` argument to `wiz_frame()` that limits the maximum time or sequence length for each id.
* Bug fix to incorrect calculation of outcomes (this error was introduced in 0.0.0.9003 due to substantial refactoring of code)
* Fixed LOCF imputation bug: now, imputation only occurs within windows in the lookback period (and not beyond)
* Fixed missingness leading to length stat of NA. Now, every stat's missingness is computed dynamically (e.g., length becomes 0)
* Fixed implicit missingness issue due to some IDs and times ids not having certain variables left after filtering
* Changed defaults so that `save_wiz_frame` and `log_file` are `TRUE`. This makes it more consistent with `output_file = TRUE` in that saving output and logs to file are the default.

# wizard 0.0.0.9005 (2020-10-11)
* Created batch_size option in wiz_frame() to chunk processing into batches. A batch of 1000 means 1000 patients will be processed at once.
* Moved processing code to wizard_internal.R and converted all other functions into wrappers
* Moved implicit missingness and LOCF imputation into the parallel jobs to reduce memory footprint after row-binding parallel jobs
* Bug fix to error introduced in wizard 0.0.0.9004 resulting extra time steps for individuals during "handling implicit missingness" step.
* Note: wiz_combine() does not support batches yet but will in a subsequent version.

# wizard 0.0.0.9006 (2020-10-27)
* Added wiz_add_growing_predictors() for building cumulatively growing windows beginning at time zero
* Changed `batch_size` parameter to `chunk_size` in `wiz_frame()`
* Changed names of files to indicate `baseline`, `growing`, or `rolling` to indicate the type of variables contained within it.
* Made some changes to the `character_language_model.Rmd` vignette to generate a large rolling dataset and test the `growing` window functionality. This vignette needs to be cleaned up for educational use.

# wizard 0.0.0.9007 (2020-11-09)
* Updated `wiz_combine()` to support chunked files and changed interface so that supplying a vector of files is optional.
* Removed `wiz_add_predictors_streaming` functions

# wizard 0.1.0 (2021-02-22)
* Fixed bug with the temporal_data_of_interest being empty due to highly sparse variables
* Also added check for max_step_times_per_id being empty (though this should not occur)

# wizard 0.2.0 (2021-02-22)
* Moved `wiz_calc` out to a separate function to greatly reduce the memory footprint for parallel processing

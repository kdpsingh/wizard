# wizard 0.0.0.9000

* Added a `NEWS.md` file to track changes to the package.

# wizard 0.0.0.9001 (2020-10-04)

* Changed `variable` argument to `variables` inside both `wiz_add_predictors()` and `wiz_add_outcomes()`. This was done to reflect the fact that the `variables` argument can accept a vector of variable names.
* Added `check_size_only` argument to `wiz_add_predictors()` and `wiz_add_outcomes()`
* Renamed `wiz_categorical_to_numeric()` to `wiz_dummy_code()` to better reflect what the function does. `wiz_dummy_code()` can now be used to dummy code specific variables, which is useful if you only want to dummy code subset a specific set of variables or if you want to dummy code a categorical variable that is currently coded with numbers (such as 1 meaning high and 2 meaning low).
* Important bug fix to encoding of time to allow for non-time-stamps

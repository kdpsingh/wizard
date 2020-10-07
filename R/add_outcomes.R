#'
#' @export
wiz_add_outcomes = function(wiz_frame = NULL,
                            variables = NULL,
                            category = NULL,
                            lookahead = hours(48),
                            window = lookahead,
                            stats = c(mean = mean,
                                      min = min,
                                      max = max),
                            impute = FALSE,
                            output_file = TRUE,
                            log_file = FALSE,
                            check_size_only = FALSE) {

  wiz_add_predictors(wiz_frame = wiz_frame,
                     variables = variables,
                     category = category,
                     lookback = -lookahead,
                     window = -window,
                     stats = stats,
                     impute = impute,
                     output_file = output_file,
                     log_file = log_file,
                     check_size_only = check_size_only)
}

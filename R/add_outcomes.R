#'
#' @export
wiz_add_outcomes = function(wiz_frame = NULL,
                            variable = NULL,
                            category = NULL,
                            lookahead = hours(48),
                            window = lookahead,
                            stats = c(mean = mean,
                                      min = min,
                                      max = max),
                            impute = FALSE,
                            output_file = TRUE) {

  wiz_add_predictors(wiz_frame = wiz_frame,
                     variable = variable,
                     category = category,
                     lookback = -lookahead,
                     window = -window,
                     stats = stats,
                     impute = impute,
                     output_file = output_file)
}

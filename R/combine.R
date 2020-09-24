#'
#' @export
wiz_combine = function(wiz_frame, ..., wiz_path = TRUE, dplyr_join = dplyr::inner_join) {
  temporal_dfs = append(list(wiz_frame$fixed_data %>%
                          dplyr::rename(!!rlang::parse_expr(wiz_frame$temporal_id) := !!rlang::parse_expr(wiz_frame$fixed_id))),
                        list(...))
  temporal_dfs =
    temporal_dfs %>%
    lapply(function (x) {
      if ('data.frame' %in% class(x)) {
        return(x)
      } else if (class(x) == 'character' & wiz_path) {
        return(data.table::fread(file.path(wiz_frame$output_folder, x)))
      } else if (class(x) == 'character' & !wiz_path) {
        return(data.table::fread(x))
      } else {
        stop('Error: the ... must be limited to data frames and file paths.')
      }
    })

  Reduce(dplyr_join, temporal_dfs) %>% as.data.frame()
}

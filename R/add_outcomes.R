#'
#' @export
wiz_add_outcomes = function(wiz_frame = NULL,
                            variables = NULL,
                            category = NULL,
                            lookahead = lubridate::hours(48),
                            window = lookahead,
                            stats = c(mean = mean,
                                      min = min,
                                      max = max),
                            impute = FALSE,
                            output_file = TRUE,
                            log_file = TRUE,
                            check_size_only = FALSE,
                            last_chunk_completed = NULL) {

  if (is.null(wiz_frame$chunk_size)) {
    wiz_add_predictors_internal(wiz_frame = wiz_frame,
                                variables = variables,
                                category = category,
                                lookback = -lookahead,
                                window = -window,
                                stats = stats,
                                impute = impute,
                                output_file = output_file,
                                log_file = log_file,
                                check_size_only = check_size_only)
  } else {
    assertthat::assert_that(wiz_frame$chunk_size > 0)

    # Make chunks based on temporal data, not fixed data
    unique_temporal_ids = sort(unique(wiz_frame$temporal_data[[wiz_frame$temporal_id]]))
    chunk_ids = ceiling(seq_len(length(unique_temporal_ids)) / wiz_frame$chunk_size)
    unique_chunks = unique(chunk_ids)
    n_chunks = max(unique_chunks)

    for (chunk_num in unique_chunks) {
      if (!is.null(last_chunk_completed) && chunk_num <= last_chunk_completed) {
        message(paste0('Skipping chunk # ', chunk_num, ' out of ', n_chunks, '...'))
        if (log_file) {
          write(paste0(Sys.time(), ': Skipping chunk # ', chunk_num, ' out of ', n_chunks, '...'),
                file.path(wiz_frame$output_folder, 'wiz_log.txt'), append = TRUE)
        }
        next
      }

      message(paste0('Processing chunk # ', chunk_num, ' out of ', n_chunks, '...'))
      if (log_file) {
        write(paste0(Sys.time(), ': Processing chunk # ', chunk_num, ' out of ', n_chunks, '...'),
              file.path(wiz_frame$output_folder, 'wiz_log.txt'), append = TRUE)
      }

      wiz_frame_chunk = wiz_frame

      wiz_frame_chunk$temporal_data =
        wiz_frame_chunk$temporal_data %>%
        dplyr::filter(!!rlang::parse_expr(wiz_frame_chunk$temporal_id) %in%
                        unique_temporal_ids[chunk_ids == chunk_num])

      wiz_frame_chunk$fixed_data =
        wiz_frame_chunk$fixed_data %>%
        dplyr::filter(!!rlang::parse_expr(wiz_frame_chunk$fixed_id) %in%
                        wiz_frame_chunk$temporal_data[[wiz_frame_chunk$temporal_id]])

      wiz_add_predictors_internal(wiz_frame = wiz_frame_chunk,
                                  variables = variables,
                                  category = category,
                                  lookback = -lookahead,
                                  window = -window,
                                  stats = stats,
                                  impute = impute,
                                  output_file = output_file,
                                  log_file = log_file,
                                  check_size_only = check_size_only,
                                  filename_prefix = paste0('chunk_',
                                                           stringr::str_pad(chunk_num,
                                                                            nchar(n_chunks),
                                                                            pad = '0'),
                                                           '_'))

    }
  }
}

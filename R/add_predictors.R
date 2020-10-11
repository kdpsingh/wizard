#' Function to add baseline predictors
#' Offset of hours(1) would mean that everything would be anchored to 1 hour
#' before fixed_start.
#' @export
wiz_add_predictors = function(wiz_frame = NULL,
                                       variables = NULL,
                                       category = NULL,
                                       lookback = hours(48),
                                       window = lookback,
                                       offset = hours(0),
                                       stats = c(mean = mean,
                                                 min = min,
                                                 max = max),
                                       impute = TRUE,
                                       output_file = TRUE,
                                       log_file = TRUE,
                                       check_size_only = FALSE) {

  if (is.null(wiz_frame$batch_size)) {
    wiz_add_predictors_internal(wiz_frame = wiz_frame,
                                variables = variables,
                                category = category,
                                lookback = lookback,
                                window = window,
                                stats = stats,
                                impute = impute,
                                output_file = output_file,
                                log_file = log_file,
                                check_size_only = check_size_only)
  } else {
    assertthat::assert_that(wiz_frame$batch_size > 0)

    # Make chunks based on temporal data, not fixed data
    unique_temporal_ids = sort(unique(wiz_frame$temporal_data[[wiz_frame$temporal_id]]))
    chunk_ids = ceiling(seq_len(length(unique_temporal_ids)) / wiz_frame$batch_size)
    unique_chunks = unique(chunk_ids)
    n_chunks = max(unique_chunks)

    for (chunk_num in unique_chunks) {
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
                                  lookback = lookback,
                                  window = window,
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



#' Function to add baseline predictors
#' Offset of hours(1) would mean that everything would be anchored to 1 hour
#' before fixed_start.
#' @export
wiz_add_baseline_predictors = function(wiz_frame = NULL,
                                       variables = NULL,
                                       category = NULL,
                                       lookback = hours(48),
                                       window = lookback,
                                       offset = hours(0),
                                       stats = c(mean = mean,
                                                 min = min,
                                                 max = max),
                                       impute = TRUE,
                                       output_file = TRUE,
                                       log_file = TRUE,
                                       check_size_only = FALSE) {

  if (is.null(wiz_frame$batch_size)) {
    wiz_add_predictors_internal(wiz_frame = wiz_frame,
                                variables = variables,
                                category = category,
                                lookback = lookback,
                                window = window,
                                stats = stats,
                                impute = impute,
                                output_file = output_file,
                                log_file = log_file,
                                check_size_only = check_size_only,
                                baseline = TRUE,
                                offset = offset)
  } else {
    assertthat::assert_that(wiz_frame$batch_size > 0)

    # Make chunks based on temporal data, not fixed data
    unique_temporal_ids = sort(unique(wiz_frame$temporal_data[[wiz_frame$temporal_id]]))
    chunk_ids = ceiling(seq_len(length(unique_temporal_ids)) / wiz_frame$batch_size)
    unique_chunks = unique(chunk_ids)
    n_chunks = max(unique_chunks)

    for (chunk_num in unique_chunks) {
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
                                  lookback = lookback,
                                  window = window,
                                  stats = stats,
                                  impute = impute,
                                  output_file = output_file,
                                  log_file = log_file,
                                  check_size_only = check_size_only,
                                  baseline = TRUE,
                                  offset = offset,
                                  filename_prefix = paste0('chunk_',
                                                           stringr::str_pad(chunk_num,
                                                                            nchar(n_chunks),
                                                                            pad = '0'),
                                                           '_'))

    }
  }
}


# Other functions to add:
# wiz_add_final_outcomes() # similar to wiz_add_baseline_predictors() but occurs after the final outcome
# wiz_add_growing_predictors() # cumulatively growing window
# wiz_add_shrinking_outcomes() # cumulatively shrinking window
# for outcomes, will need to respect max_length

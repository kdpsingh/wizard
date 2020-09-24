#' Add predictors
#' @export
wiz_add_predictors = function(wiz_frame = NULL,
                              variable = NULL,
                              category = NULL,
                              lookback = hours(48),
                              window = hours(12),
                              stats = c(mean = mean,
                                        min = min,
                                        max = max),
                              impute = TRUE,
                              output_file = TRUE) {

  if (is.null(variable) && is.null(category)) {
    stop('You must specify either a variable or a category')
  }

  if (!is.null(variable) && !is.null(category)) {
    stop('You must specify only a variable or a category, not both.')
  }

  if (!is.null(variable) && !variable %in% wiz_frame$temporal_data_dict$variable) {
    stop(paste0('The variable ', variable, ' could not be found in the temporal data.'))
  }

  if (!is.null(category) && !category %in% wiz_frame$temporal_data[[wiz_frame$temporal_data_category]]) {
    stop(paste0('The category ', category, ' could not be found in the temporal data.'))
  }

  wiz_variable = variable
  wiz_category = category

  lookback_converted = lubridate::time_length(lookback, unit = wiz_frame$step_units)
  window_converted = lubridate::time_length(window, unit = wiz_frame$step_units)

  if (lookback_converted == 0) {
    stop('lookback/lookahead cannot be 0.')
  }
  if (window_converted == 0) {
    stop('window cannot be 0.')
  }

  if ((lookback_converted > 0 && window_converted < 0) ||
      (lookback_converted < 0 && window_converted > 0)) {
    stop(paste0('The lookback/lookahead and window must either *both* be positive ',
                '(for lookback) or *both* negative (for lookahead).'))
  }

  if (lookback_converted %% window_converted != 0) {
    stop ('The lookback must be divisible by the window (with no remainder).')
  }


  temporal_data_of_interest =
    wiz_frame$temporal_data %>%
    dplyr::group_by(!!rlang::parse_expr(wiz_frame$temporal_id)) %>%
    dplyr::mutate(wiz_step_time =
                    !!rlang::parse_expr(wiz_frame$temporal_time) %/%
                    wiz_frame$step *
                    wiz_frame$step +
                    wiz_frame$step) %>%
    dplyr::mutate(wiz_lookback_time =
                    wiz_step_time - lookback_converted) %>%
    dplyr::ungroup()


  if (wiz_frame$fixed_end != '') { # need to test this
    max_step_times_per_id =
      temporal_data_of_interest %>%
      dplyr::left_join(., wiz_frame$fixed_data %>%
                         dplyr::select(dplyr::all_of(c(wiz_frame$fixed_id, wiz_frame$fixed_end))) %>%
                         dplyr::rename(!!rlang::parse_expr(wiz_frame$temporal_id) := !!rlang::parse_expr(wiz_frame$fixed_id)) %>%
                         dplyr::rename(wiz_fixed_end_time = !!rlang::parse_expr(wiz_frame$fixed_end))
      ) %>%
      dplyr::distinct(!!rlang::parse_expr(wiz_frame$temporal_id), wiz_fixed_end_time) %>%
      dplyr::mutate(wiz_step_time = wiz_fixed_end_time %/% wiz_frame$step) %>% # will select furthest time with complete step data
      dplyr::select(!!rlang::parse_expr(wiz_frame$temporal_id), wiz_step_time)
  } else {
    max_step_times_per_id =
      temporal_data_of_interest %>%
      dplyr::group_by(!!rlang::parse_expr(wiz_frame$temporal_id)) %>%
      dplyr::filter(wiz_step_time == max(wiz_step_time)) %>%
      dplyr::ungroup() %>%
      dplyr::distinct(!!rlang::parse_expr(wiz_frame$temporal_id), wiz_step_time)
  }

  if (!is.null(variable)) {
    temporal_data_of_interest =
      temporal_data_of_interest %>%
      dplyr::filter(!!rlang::parse_expr(wiz_frame$temporal_variable) %in% wiz_variable)
  } else if (!is.null(category)) {
    temporal_data_of_interest =
      temporal_data_of_interest %>%
      dplyr::filter(stringr::str_detect(!!rlang::parse_expr(wiz_frame$temporal_category), category))
  } else {
    stop('This option should not be possible.')
  }

  intermediate_output_rows = max_step_times_per_id %>%
    dplyr::filter(wiz_step_time >= 0) %>%
    dplyr::pull(wiz_step_time) %>%
    {. / wiz_frame$step + 1} %>% # e.g., if max step for an id is 18 and step is 6, there will rows for 0, 6, 12, 18 (or 18/6 + 1 rows)
    {sum(.)} %>%
    {. * lookback_converted/window_converted} # Only for intermediate output

  message(paste0('Anticipated number of rows in intermediate output: ', intermediate_output_rows))

  final_output_rows = max_step_times_per_id %>%
    dplyr::filter(wiz_step_time >= 0) %>%
    dplyr::pull(wiz_step_time) %>%
    {. / wiz_frame$step + 1} %>% # e.g., if max step for an id is 18 and step is 6, there will rows for 0, 6, 12, 18 (or 18/6 + 1 rows)
    {sum(.)}

  message(paste0('Anticipated number of rows in final output: ', final_output_rows))

  #
  # temporal_data_of_interest <<- temporal_data_of_interest

  # output_list = vector(mode = 'list', length = output_rows * lookback_converted/window_converted)

  output_list = list()

  # Display progress bar
  num_ids = length(unique(wiz_frame$temporal_data[[wiz_frame$temporal_id]]))
  current_id_num = 0
  message(paste0('Number of ids to process: ', num_ids))
  pb = txtProgressBar(min = 0, max = num_ids, style = 3)

  # Counter for assigning elements to the output_list
  n = 1
  for (wiz_id in unique(wiz_frame$temporal_data[[wiz_frame$temporal_id]])) {
    for (variable_name in unique(temporal_data_of_interest[[wiz_frame$temporal_variable]])) {
      max_step_time =
        max_step_times_per_id %>%
        dplyr::filter(!!rlang::parse_expr(wiz_frame$temporal_id) == wiz_id) %>%
        dplyr::pull(wiz_step_time)

      if (length(max_step_time) == 0 || max_step_time < 0) { # This should only be the case if someone has no observations in temporal_data after time 0
        next
      } else {
        unique_step_times = seq(0, max_step_time, by = wiz_frame$step)
      }
      for (step_time in unique_step_times) {
        for (window_num in 1:(lookback_converted/window_converted)) {

            if (lookback_converted < 0) { # E.g. if it is a lookahead
              output_item =
                temporal_data_of_interest %>%
                dplyr::filter(!!rlang::parse_expr(wiz_frame$temporal_id) == wiz_id &
                                wiz_step_time > step_time - (window_num-1)*window_converted & # outcome cannot include right now
                                wiz_step_time <= step_time - (window_converted * window_num))
            } else { # if it is a lookback
              output_item =
                temporal_data_of_interest %>%
                dplyr::filter(!!rlang::parse_expr(wiz_frame$temporal_id) == wiz_id &
                                wiz_step_time <= step_time - (window_num-1)*window_converted & # includes now in predictors
                                wiz_step_time > step_time - (window_converted * window_num)) # up to X hours ago but not including X
            }

          suppressWarnings({
            output_item =
              output_item %>%
              dplyr::filter(!!rlang::parse_expr(wiz_frame$temporal_variable) == variable) %>%
              dplyr::summarize_at(
                dplyr::vars(dplyr::all_of(wiz_frame$temporal_value)),
                .funs = stats) %>%
              dplyr::mutate(!!rlang::parse_expr(wiz_frame$temporal_id) := wiz_id,
                            time = step_time,
                            window_time = window_num*window_converted) %>% # New: will always be named the *higher* number
              dplyr::select(dplyr::all_of(wiz_frame$temporal_id),
                            time,
                            window_time,
                            dplyr::everything()) %>%
              dplyr::rename_at(dplyr::vars(!contains(dplyr::all_of(wiz_frame$temporal_id)),
                                           -time, -window_time),
                               .funs = . %>% paste0(variable_name, '_',.))

          })

          if (nrow(output_item) > 0) {
            output_list[[n]] = output_item
            n = n + 1
          }
        }
      }
    }
    current_id_num = current_id_num + 1
    setTxtProgressBar(pb, current_id_num)
  }

  # Close the progress bar
  close(pb)

  if (lookback_converted < 0) {
    file_type = '_outcomes_'
  } else {
    file_type = '_predictors_'
  }

  output_file_name = if (!is.null(category)) {
    file.path(wiz_frame$output_folder,
              paste0('temporal', file_type, category, '_', lubridate::now()) %>%
                janitor::make_clean_names() %>%
                paste0('.csv'))
  } else {
    file.path(wiz_frame$output_folder,
              paste0('temporal',  file_type, variable, '_', lubridate::now()) %>%
                janitor::make_clean_names() %>%
                paste0('.csv'))
  }

  output_frame = dplyr::bind_rows(output_list) %>%
    tidyr::gather(variable, value, -id, -time, -window_time) %>%
    dplyr::mutate(value = dplyr::na_if(value, -Inf)) %>%
    dplyr::mutate(value = dplyr::na_if(value, Inf)) %>%
    {.[is.na(.)] = NA_real_; .}

  if (lookback_converted < 0) { # e.g. if it is a lookahead
    output_frame =
      output_frame %>%
      dplyr::mutate(variable = paste0('outcome_',variable, '_',
                                      stringr::str_pad(abs(window_time),
                                                       nchar(abs(lookback_converted)), pad = '0'))) %>%
      dplyr::select(-window_time)
  } else { # if it is a lookback
    output_frame =
      output_frame %>%
      dplyr::mutate(variable = paste0(variable, '_',
                                      stringr::str_pad(abs(window_time),
                                                       nchar(abs(lookback_converted)), pad = '0'))) %>%
      dplyr::select(-window_time)
  }


  if (impute) {
    output_frame =
      output_frame %>%
      dplyr::group_by(id, variable) %>% # Do not group by time because values need to fill across different times
      dplyr::arrange(id, variable, time) %>%
      tidyr::fill(-id, -variable, -time) %>%
      dplyr::ungroup()
  }

  output_frame =
    output_frame %>%
    tidyr::spread(variable, value)

    if (output_file == TRUE) {
      data.table::fwrite(output_frame, output_file_name)
      return(output_file_name)
    } else {
      return(output_frame)
    }

 # temporal_data_of_interest

  # temporal_data_of_interest %>%
  #  group_by(!!rlang::parse_expr(wiz_frame$temporal_id),
  #           wiz_step_time,
  #           wiz_lookback_time,
  #           !!rlang::parse_expr(wiz_frame$temporal_variable)) %>%
  #  summarize_at(vars(all_of(wiz_frame$temporal_value)), .funs = stats)

  # predictors =
  #   temporal_data_of_interest %>%
  #   pull(!!rlang::parse_expr(wiz_frame$temporal_variable)) %>%
  #   unique()

  # for (predictor in predictors) {
  #
  #   temporal_data_of_interest =
  #     temporal_data_of_interest %>%
  #     mutate(!!parse_expr())
  # }

 #  wiz_frame

}

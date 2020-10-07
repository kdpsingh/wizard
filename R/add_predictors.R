#' New internal helper function
wiz_define_steps = function(groups, temporal_id, step, max_step_times_per_id,
                            lookback_converted, window_converted, output_folder,
                            log_file) {

  # check to make sure no one has missing time in temporal_data
  # check to make sure all patients in temporal_data
  # are accounted for in the fixed_data
  # make sure no missing values in fixed_start or fixed_end

  if (log_file) {
    write(paste0(Sys.time(), ': id: ', groups[[temporal_id]]),
          file.path(output_folder, 'wiz_log.txt'), append = TRUE)
  }

    max_step_time =
    max_step_times_per_id %>%
    dplyr::filter(!!rlang::parse_expr(temporal_id) == groups[[temporal_id]]) %>%
    dplyr::pull(wiz_step_time)

  if (length(max_step_time) == 0 || max_step_time < 0) { # This should only be the case if someone has no observations in temporal_data after time 0
    return(NULL)
  }

  time = seq(0, max_step_time, by = step)

  window_num = 1:(lookback_converted/window_converted)

  window_time = window_num*window_converted

  return_frame =
    tidyr::expand_grid(time, window_time)

  return(return_frame)
}

#' Internal function
wiz_calc_stats = function(groups, temporal_id, temporal_variable, temporal_value, stats,
                          lookback_converted, window_converted, temporal_data_of_interest,
                          total_num_groups, pb) {
  if (lookback_converted < 0) { # E.g. if it is a lookahead
    output_item =
      temporal_data_of_interest %>%
      dplyr::filter(!!rlang::parse_expr(temporal_id) == groups[[temporal_id]] &
                      wiz_step_time > groups$time - groups$window_time + window_converted & # outcome cannot include right now
                      wiz_step_time <= groups$time - groups$window_time)
  } else { # if it is a lookback
    output_item =
      temporal_data_of_interest %>%
      dplyr::filter(!!rlang::parse_expr(temporal_id) == groups[[temporal_id]] &
                      wiz_step_time <= groups$time - groups$window_time + window_converted & # includes now in predictors
                      wiz_step_time > groups$time - groups$window_time) # up to X hours ago but not including X
  }

  suppressWarnings({

    output_item =
      output_item %>%
      # dplyr::filter(!!rlang::parse_expr(temporal_variable) == groups[[temporal_variable]]) %>%
      dplyr::group_by(!!rlang::parse_expr(temporal_variable))%>%
      dplyr::summarize_at(temporal_value,
        .funs = stats) %>%
      tidyr::gather(wiz_stat, wiz_value, -!!rlang::parse_expr(temporal_variable)) # %>%
      # dplyr::rename_all(.funs = . %>% paste0(groups[[temporal_variable]], '_',.))

  })

  if (nrow(output_item) == 0) {
    output_item =
      tidyr::crossing(
        dplyr::tibble(!!rlang::parse_expr(temporal_variable) :=
                        unique(temporal_data_of_interest[[temporal_variable]])),
        dplyr::tibble(wiz_stat = names(stats))
      ) %>%
      dplyr::mutate(wiz_value = NA)
  }

  pb$tick()
  return(output_item)
}



#' New furrr-enabled add_predictors function
#' #' New add_predictors function using group_modify
#' @export
wiz_add_predictors = function(wiz_frame = NULL,
                              variables = NULL,
                              category = NULL,
                              lookback = hours(48),
                              window = lookback,
                              stats = c(mean = mean,
                                        min = min,
                                        max = max),
                              impute = TRUE,
                              output_file = TRUE,
                              log_file = FALSE,
                              check_size_only = FALSE,
                              ...) {

  dots = list(...)

  if (is.null(variables) && is.null(category)) {
    stop('You must specify either a variable or a category.')
  }

  if (!is.null(variables) && !is.null(category)) {
    stop('You must specify only a variable or a category, not both.')
  }

  if (!is.null(variables)) {
    for (variable in variables) {
      if (!variable %in% wiz_frame$temporal_data_dict$variable) {
        stop(paste0('The variable ', variable, ' could not be found in the temporal data.'))
      }
    }
  }

  if (!is.null(category) && !any(grepl(category, wiz_frame$temporal_data[[wiz_frame$temporal_category]]))) {
    stop(paste0('The category ', category, ' could not be found in the temporal data.'))
  }

  wiz_variables = variables
  wiz_category = category

  if (!is.null(wiz_frame$step_units)) {
    lookback_converted = lubridate::time_length(lookback, unit = wiz_frame$step_units)
    window_converted = lubridate::time_length(window, unit = wiz_frame$step_units)
  } else {
    lookback_converted = lookback
    window_converted = window
  }

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


  # fixed_end and fixed_start should *always* be available now because they are now created if not provided
  max_step_times_per_id =
    temporal_data_of_interest %>%
    dplyr::left_join(., wiz_frame$fixed_data %>%
                       dplyr::select_at(c(wiz_frame$fixed_id, wiz_frame$fixed_start, wiz_frame$fixed_end)) %>%
                       dplyr::rename(!!rlang::parse_expr(wiz_frame$temporal_id) := !!rlang::parse_expr(wiz_frame$fixed_id)) %>%
                       dplyr::mutate(wiz_fixed_start_time = !!rlang::parse_expr(wiz_frame$fixed_start)) %>%
                       dplyr::mutate(wiz_fixed_end_time = !!rlang::parse_expr(wiz_frame$fixed_end))
    ) %>%
    dplyr::distinct(!!rlang::parse_expr(wiz_frame$temporal_id), wiz_fixed_start_time, wiz_fixed_end_time)

  if (!is.null(wiz_frame$step_units)) {
    max_step_times_per_id =
      max_step_times_per_id %>%
      dplyr::mutate(wiz_step_time =
                      lubridate::time_length(wiz_fixed_end_time - wiz_fixed_start_time, unit = wiz_frame$step_units) %/% wiz_frame$step * wiz_frame$step) %>% # will select furthest time with complete step data
      dplyr::select(!!rlang::parse_expr(wiz_frame$temporal_id), wiz_step_time)
  } else {
    max_step_times_per_id =
      max_step_times_per_id %>%
      dplyr::mutate(wiz_step_time = (wiz_fixed_end_time - wiz_fixed_start_time) %/% wiz_frame$step * wiz_frame$step) %>% # will select furthest time with complete step data
      dplyr::select(!!rlang::parse_expr(wiz_frame$temporal_id), wiz_step_time)
  }

  if (!is.null(variables)) {
    temporal_data_of_interest =
      temporal_data_of_interest %>%
      dplyr::filter(!!rlang::parse_expr(wiz_frame$temporal_variable) %in% wiz_variables)
  } else if (!is.null(category)) {
    temporal_data_of_interest =
      temporal_data_of_interest %>%
      dplyr::filter(stringr::str_detect(!!rlang::parse_expr(wiz_frame$temporal_category), wiz_category))
  } else {
    stop('This option should not be possible.')
  }

  if ('character' %in% (wiz_frame$temporal_data_dict %>%
                        dplyr::filter(variable %in% temporal_data_of_interest[[wiz_frame$temporal_variable]]) %>%
                        dplyr::pull(class)) &&
      'numeric' %in% (wiz_frame$temporal_data_dict %>%
                      dplyr::filter(variable %in% temporal_data_of_interest[[wiz_frame$temporal_variable]]) %>%
                      dplyr::pull(class))) {
        stop(paste0('Please select variables that are either all numeric or all categorical. ',
                'They cannot be mixed. If both are to be selected, then you must dummy ',
                'code the categorical variables using wiz_dummy_code().'))
  }

  # If all variables are numeric, convert value column to numeric prior to calculating stats
  if (all(wiz_frame$temporal_data_dict %>%
           dplyr::filter(variable %in% temporal_data_of_interest[[wiz_frame$temporal_variable]]) %>%
          dplyr::pull(class) == 'numeric')) {
    temporal_data_of_interest[[wiz_frame$temporal_value]] =
      as.numeric(temporal_data_of_interest[[wiz_frame$temporal_value]])
  } else {
    temporal_data_of_interest[[wiz_frame$temporal_value]] =
      as.character(temporal_data_of_interest[[wiz_frame$temporal_value]])
  }

  # Test to make sure all stats are calculable
  for (stat in stats) {
    tryCatch({
      do.call(stat, list(temporal_data_of_interest[[wiz_frame$temporal_value]]))},
      error = function (e) {
        stop(paste0('The statistic ', stat, ' could not be calculated for the ',
                    'selected variables in the temporal data.'))
      })
  }

  if (!is.null(variables)) {
    message(paste0('Processing variables: ', paste0(variables, collapse = ', '), '...'))
    if (log_file) {
      write(paste0(Sys.time(), ': Processing variables: ', paste0(variables, collapse = ', '), '...'),
            file.path(wiz_frame$output_folder, 'wiz_log.txt'), append = TRUE)
    }
  } else if (!is.null(category)) {
    message(paste0('Processing category: ', category, '...'))
    if (log_file) {
      write(paste0(Sys.time(), ': Processing category: ', category, '...'),
            file.path(wiz_frame$output_folder, 'wiz_log.txt'), append = TRUE)
    }
  }

  intermediate_output_rows = max_step_times_per_id %>%
    dplyr::filter(wiz_step_time >= 0) %>%
    dplyr::pull(wiz_step_time) %>%
    {. / wiz_frame$step + 1} %>% # e.g., if max step for an id is 18 and step is 6, there will rows for 0, 6, 12, 18 (or 18/6 + 1 rows)
    {sum(.)} %>%
    {. * lookback_converted/window_converted} # Only for intermediate output
  # {. * length(unique(temporal_data_of_interest[[wiz_frame$temporal_variable]]))} # num variables

  message(paste0('Anticipated number of rows in intermediate output: ', intermediate_output_rows))
  if (log_file) {
    write(paste0(Sys.time(), ': Anticipated number of rows in intermediate output: ', intermediate_output_rows),
          file.path(wiz_frame$output_folder, 'wiz_log.txt'), append=TRUE)
  }

  final_output_rows = max_step_times_per_id %>%
    dplyr::filter(wiz_step_time >= 0) %>%
    dplyr::pull(wiz_step_time) %>%
    {. / wiz_frame$step + 1} %>% # e.g., if max step for an id is 18 and step is 6, there will rows for 0, 6, 12, 18 (or 18/6 + 1 rows)
    {sum(.)}

  message(paste0('Anticipated number of rows in final output: ', final_output_rows))
  if (log_file) {
    write(paste0(Sys.time(), ': Anticipated number of rows in final output: ', final_output_rows),
          file.path(wiz_frame$output_folder, 'wiz_log.txt'), append = TRUE)
  }

  if (check_size_only) {
    return(intermediate_output_rows)
  }

  message('Allocating memory...')
  if (log_file) {
    write(paste0(Sys.time(), ': Allocating memory...'),
          file.path(wiz_frame$output_folder, 'wiz_log.txt'), append = TRUE)
  }

  output_frame =
    dplyr::tibble(!!rlang::parse_expr(wiz_frame$temporal_id) :=
                    unique(wiz_frame$temporal_data[[wiz_frame$temporal_id]])) %>%
    tidyr::crossing(
      dplyr::tibble(
        !!rlang::parse_expr(wiz_frame$temporal_variable) :=
          unique(temporal_data_of_interest[[wiz_frame$temporal_variable]]))) %>%
    dplyr::group_by(!!rlang::parse_expr(wiz_frame$temporal_id),
                    !!rlang::parse_expr(wiz_frame$temporal_variable)) %>%
    dplyr::group_modify(~wiz_define_steps(groups = .y,
                                         temporal_id = wiz_frame$temporal_id,
                                         step = wiz_frame$step,
                                         max_step_times_per_id = max_step_times_per_id,
                                         lookback_converted = lookback_converted,
                                         window_converted = window_converted,
                                         output_folder = wiz_frame$output_folder,
                                         log_file = log_file))

  total_num_groups = nrow(output_frame)

  if ('sequential' %in% class(future::plan())) {
    strategy = 'sequential'
    message('Parallel processing is DISABLED. Calculations are happening sequentially.')
    if (log_file) {
      write(paste0(Sys.time(), ': Parallel processing is DISABLED. Calculations are happening sequentially.'),
            file.path(wiz_frame$output_folder, 'wiz_log.txt'), append = TRUE)
    }
    pb = progress::progress_bar$new(format = "[:bar] :current/:total (:percent) Time remaining: :eta",
                                    total = intermediate_output_rows)

    pb$tick(0)
  } else {
    strategy = 'parallel'
    message('Parallel processing is ENABLED.')
    if (log_file) {
      write(paste0(Sys.time(), ': Parallel processing is ENABLED.'),
            file.path(wiz_frame$output_folder, 'wiz_log.txt'), append = TRUE)
    }
  }

  message('Beginning calculation...')
  if (log_file) {
    write(paste0(Sys.time(), ': Beginning calculation...'),
          file.path(wiz_frame$output_folder, 'wiz_log.txt'), append = TRUE)
  }

  output_list =
    output_frame %>%
    dplyr::group_by(!!rlang::parse_expr(wiz_frame$temporal_id),
                    time,
                    window_time) %>%
    dplyr::group_split()

  suppressWarnings({
    output_frame =
      output_list %>%
      furrr::future_map_dfr(.f = function(groups = .x,
                                          temporal_id = wiz_frame$temporal_id,
                                          temporal_variable = wiz_frame$temporal_variable,
                                          temporal_value = wiz_frame$temporal_value,
                                          temporal_time = wiz_frame$temporal_time) {

        if (lookback_converted < 0) { # E.g. if it is a lookahead
          output_item =
            temporal_data_of_interest %>%
            dplyr::filter(!!rlang::parse_expr(temporal_id) == groups[[temporal_id]][1] &
                            !!rlang::parse_expr(temporal_time) > groups$time[1] - groups$window_time[1] + window_converted & # outcome cannot include right now
                            !!rlang::parse_expr(temporal_time) <= groups$time[1] - groups$window_time[1])
        } else { # if it is a lookback
          output_item =
            temporal_data_of_interest %>%
            dplyr::filter(!!rlang::parse_expr(temporal_id) == groups[[temporal_id]][1] &
                            !!rlang::parse_expr(temporal_time) <= groups$time[1] - groups$window_time[1] + window_converted & # includes now in predictors
                            !!rlang::parse_expr(temporal_time) > groups$time[1] - groups$window_time[1]) # up to X hours ago but not including X
        }

        output_item =
          output_item %>%
          # dplyr::filter(!!rlang::parse_expr(temporal_variable) == groups[[temporal_variable]]) %>%
          dplyr::group_by(!!rlang::parse_expr(temporal_variable)) %>%
          dplyr::arrange(!!rlang::parse_expr(temporal_variable), !!rlang::parse_expr(temporal_time)) %>%
          dplyr::summarize_at(temporal_value,
            .funs = stats) %>%
          tidyr::gather(wiz_stat, wiz_value, -!!rlang::parse_expr(temporal_variable)) %>%
          dplyr::mutate(!!rlang::parse_expr(temporal_id) := groups[[temporal_id]][1],
                        time = groups$time[1],
                        window_time = groups$window_time[1]) %>%
          dplyr::select(!!rlang::parse_expr(temporal_id), time, window_time,
                        !!rlang::parse_expr(temporal_variable), dplyr::everything())


        if (nrow(output_item) == 0) {
          output_item =
            tidyr::crossing(
              dplyr::tibble(!!rlang::parse_expr(temporal_variable) :=
                              unique(temporal_data_of_interest[[temporal_variable]])),
              dplyr::tibble(wiz_stat = names(stats))
            ) %>%
            dplyr::mutate(!!rlang::parse_expr(temporal_id) := groups[[temporal_id]][1],
                          time = groups$time[1],
                          window_time = groups$window_time[1]) %>%
            dplyr::select(!!rlang::parse_expr(temporal_id), time, window_time,
                          !!rlang::parse_expr(temporal_variable), dplyr::everything()) %>%
            dplyr::mutate(wiz_value = NA)
        }
        if (strategy == 'sequential') {
          pb$tick()
        }
        return(output_item)
      },
      .progress = TRUE) #%>%
    # dplyr::ungroup()
  })

  message('Completed calculation.')
  if (log_file) {
    write(paste0(Sys.time(), ': Completed calculation.'),
          file.path(wiz_frame$output_folder, 'wiz_log.txt'), append = TRUE)
  }

  if (lookback_converted < 0) {
    file_type = '_outcomes_'
  } else {
    file_type = '_predictors_'
  }

  output_file_name = if (!is.null(category)) {
    file.path(wiz_frame$output_folder,
              paste0('temporal', file_type, '_category_', category, '_', lubridate::now()) %>%
                janitor::make_clean_names() %>%
                paste0('.csv'))
  } else {
    file.path(wiz_frame$output_folder,
              paste0('temporal',  file_type, '_variables_', paste0(variables, collapse = '_'),
                     '_', lubridate::now()) %>%
                janitor::make_clean_names() %>%
                paste0('.csv'))
  }

  output_frame =
    output_frame %>%
    dplyr::mutate(wiz_variable =
                    paste0(!!rlang::parse_expr(wiz_frame$temporal_variable),
                           '_', wiz_stat),
                  wiz_value = wiz_value) %>% # removed as.numeric(wiz_value)
    dplyr::select(-!!rlang::parse_expr(wiz_frame$temporal_variable), -wiz_stat) %>%
    # tidyr::gather(variables, value, -!!rlang::parse_expr(wiz_frame$temporal_id),
    #               -wiz_variable, -time, -window_time) %>%
    # # dplyr::filter(stringr::str_detect(variables,
    # #                                  paste0('^', wiz_variable, '_.*?_.*?_[0-9.]+$'))) %>%
    # dplyr::select(-wiz_variable) %>%
    dplyr::mutate(wiz_value = dplyr::na_if(wiz_value, -Inf)) %>%
    dplyr::mutate(wiz_value = dplyr::na_if(wiz_value, Inf))


  if (lookback_converted < 0) { # e.g. if it is a lookahead
    output_frame =
      output_frame %>%
      dplyr::mutate(wiz_variable = paste0('outcome_', wiz_variable, '_',
                                      stringr::str_pad(abs(window_time),
                                                       nchar(abs(lookback_converted)), pad = '0'))) %>%
      dplyr::select(-window_time)
  } else { # if it is a lookback

    if (is.null(dots[['baseline']]) || !dots[['baseline']]) {
    output_frame =
      output_frame %>%
      dplyr::mutate(wiz_variable = paste0(wiz_variable, '_',
                                      stringr::str_pad(abs(window_time),
                                                       nchar(abs(lookback_converted)), pad = '0'))) %>%
      dplyr::select(-window_time)
    } else {
      output_frame =
        output_frame %>%
        dplyr::mutate(wiz_variable = paste0('baseline_', wiz_variable, '_',
                                            stringr::str_pad(abs(window_time),
                                                             nchar(abs(lookback_converted)), pad = '0'))) %>%
        dplyr::select(-window_time)
    }
  }

  if (impute) {
    message('Performing LOCF imputation...')
    if (log_file) {
      write(paste0(Sys.time(), ': Performing LOCF imputation...'),
            file.path(wiz_frame$output_folder, 'wiz_log.txt'), append = TRUE)
    }

    output_frame =
      output_frame %>%
      dplyr::group_by(!!rlang::parse_expr(wiz_frame$temporal_id), wiz_variable) %>% # Do not group by time because values need to fill across different times
      dplyr::arrange(!!rlang::parse_expr(wiz_frame$temporal_id), wiz_variable, time) %>%
      tidyr::fill(-!!rlang::parse_expr(wiz_frame$temporal_id), -wiz_variable, -time) %>%
      dplyr::ungroup()
  }

  output_frame =
    output_frame %>%
    tidyr::spread(wiz_variable, wiz_value) %>%
    as.data.frame()

  message('Completed data cleanup.')
  if (log_file) {
    write(paste0(Sys.time(), ': Completed data cleanup.'),
          file.path(wiz_frame$output_folder, 'wiz_log.txt'), append = TRUE)
  }

  if (output_file == TRUE) {
    data.table::fwrite(output_frame, output_file_name)
    message(paste0('The output file was written to: ', output_file_name))
    return(invisible(wiz_frame))
  }

  return(output_frame)
}

#' Function to add baseline predictors
#' Offset of hours(1) would mean that everything would be anchored to 1 hour
#' before fixed_start.
#' @export
wiz_add_baseline_predictors = function(wiz_frame = NULL,
                                       variables = NULL,
                                       category = NULL,
                                       offset = 0,
                                       lookback = hours(48),
                                       window = lookback,
                                       stats = c(mean = mean,
                                                 min = min,
                                                 max = max),
                                       impute = TRUE,
                                       output_file = TRUE,
                                       log_file = FALSE,
                                       check_size_only = FALSE) {

  wiz_frame$fixed_end = wiz_frame$fixed_start

  wiz_frame$fixed_data[[wiz_frame$fixed_start]] = wiz_frame$fixed_data[[wiz_frame$fixed_start]] - offset

  wiz_add_predictors(wiz_frame = wiz_frame,
                     variables = variables,
                     category = category,
                     lookback = lookback,
                     window = window,
                     stats = stats,
                     impute = impute,
                     output_file = output_file,
                     log_file = log_file,
                     check_size_only = check_size_only,
                     baseline = TRUE)

}




#' Internal function
#' Does not work because furrr cannot find it when it is in plan('multisession') on Windows
#' So this function has been hard-coded into the add_predictors_function
wiz_calc_stats_furrr = function(groups, temporal_id, temporal_variable, temporal_value, stats,
                          lookback_converted, window_converted, temporal_data_of_interest,
                          total_num_groups) {
  if (lookback_converted < 0) { # E.g. if it is a lookahead
    output_item =
      temporal_data_of_interest %>%
      dplyr::filter(!!rlang::parse_expr(temporal_id) == groups[[temporal_id]] &
                      wiz_step_time > groups$time - groups$window_time + window_converted & # outcome cannot include right now
                      wiz_step_time <= groups$time - groups$window_time)
  } else { # if it is a lookback
    output_item =
      temporal_data_of_interest %>%
      dplyr::filter(!!rlang::parse_expr(temporal_id) == groups[[temporal_id]] &
                      wiz_step_time <= groups$time - groups$window_time + window_converted & # includes now in predictors
                      wiz_step_time > groups$time - groups$window_time) # up to X hours ago but not including X
  }

  suppressWarnings({

    output_item =
      output_item %>%
      # dplyr::filter(!!rlang::parse_expr(temporal_variable) == groups[[temporal_variable]]) %>%
      dplyr::group_by(!!rlang::parse_expr(temporal_variable))%>%
      dplyr::summarize_at(temporal_value,
        .funs = stats) %>%
      tidyr::gather(wiz_stat, wiz_value, -!!rlang::parse_expr(temporal_variable)) %>%
      dplyr::mutate(!!rlang::parse_expr(temporal_id) := groups[[temporal_id]],
                    time = groups$time,
                    window_time = groups$window_time) %>%
      dplyr::select(!!rlang::parse_expr(temporal_id), time, window_time,
                    !!rlang::parse_expr(temporal_variable), dplyr::everything())

  })

  if (nrow(output_item) == 0) {
    output_item =
      tidyr::crossing(
        dplyr::tibble(!!rlang::parse_expr(temporal_variable) :=
                        unique(temporal_data_of_interest[[temporal_variable]])),
        dplyr::tibble(wiz_stat = names(stats))
      ) %>%
      dplyr::mutate(!!rlang::parse_expr(temporal_id) := groups[[temporal_id]],
                    time = groups$time,
                    window_time = groups$window_time) %>%
      dplyr::select(!!rlang::parse_expr(temporal_id), time, window_time,
                    !!rlang::parse_expr(temporal_variable), dplyr::everything()) %>%
      dplyr::mutate(wiz_value = NA)
  }

  # pb$tick()
  return(output_item)
}


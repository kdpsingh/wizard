#' Define wizard frame
#' @export
wiz_frame = function(fixed_data,
                     temporal_data,
                     fixed_id = 'id',
                     fixed_start = NULL,
                     fixed_end = NULL,
                     temporal_id = 'id',
                     temporal_time = 'time',
                     temporal_variable = 'variable',
                     temporal_value = 'value',
                     temporal_category = temporal_variable,
                     step = NULL,
                     max_length = NULL,
                     output_folder = NULL,
                     create_folder = FALSE,
                     save_wiz_frame = FALSE,
                     numeric_threshold = 0.5) {

  assertthat::assert_that('data.frame' %in% class(fixed_data))
  assertthat::assert_that('data.frame' %in% class(temporal_data))

  # To deal with any data.table -> dtplyr weirdness
  fixed_data = as.data.frame(fixed_data)
  temporal_data = as.data.frame(temporal_data)

  if (!is.null(fixed_start)) {
    if (class(fixed_data[[fixed_start]])[1] %in% c('Date', 'POSIXct', 'POSIXt') &&
        class(step) != 'Period') {
      stop('Both the fixed_start column in the fixed_data and step must be in the same units.')
    }
    if (is.numeric(fixed_data[[fixed_start]]) && !is.numeric(step)) {
      stop('Both the fixed_start column in the fixed_data and step must be in the same units.')
    }
    if (class(fixed_data[[fixed_start]])[1] %in% c('character', 'factor')) {
      stop('The fixed_start column cannot be a character or factor column. You must convert it to either a number or a date.')
    }
    if (any(is.na(fixed_data[[fixed_start]]))) {
      stop('The fixed_start column cannot contain missing values.')
    }
  }

  if (!is.null(fixed_end)) {
    if (class(fixed_data[[fixed_end]])[1] %in% c('Date', 'POSIXct', 'POSIXt') &
        class(step) != 'Period') {
      stop('Both the fixed_end column in the fixed_data and step must be in the same units.')
    }
    if (is.numeric(fixed_data[[fixed_end]]) & !is.numeric(step)) {
      stop('Both the fixed_end column in the fixed_data and step must be in the same units.')
    }
    if (class(fixed_data[[fixed_end]])[1] %in% c('character', 'factor')) {
      stop('The fixed_end column cannot be a character or factor column. You must convert it to either a number or a date.')
    }
    if (any(is.na(fixed_data[[fixed_end]]))) {
      stop('The fixed_end column cannot contain missing values.')
    }
  }

  if (!is.null(max_length)) {
    if (class(max_length) != class(step)) {
      stop('Both the max_length and step must be in the same units.')
    }
  }

  if (class(temporal_data[[temporal_time]])[1] %in% c('Date', 'POSIXct', 'POSIXt') &&
      class(step) != 'Period') {
    stop('Both the temporal_time column in the temporal_data and step must be in the same units.')
  }
  if (is.numeric(temporal_data[[temporal_time]]) && !is.numeric(step)) {
    stop('Both the temporal_time column in the temporal_data and step must be in the same units.')
  }
  if (class(temporal_data[[temporal_time]])[1] %in% c('character', 'factor')) {
    stop('The temporal_time column cannot be a character or factor column. You must convert it to either a number or a date.')
  }
  if (any(is.na(temporal_data[[temporal_time]]))) {
    stop('The temporal_time column cannot contain missing values.')
  }


  if (is.null(output_folder)) {
    stop('You must specify an output folder.')
  }

  if (!dir.exists(output_folder)) {
    if (create_folder) {
      dir.create(output_folder)
    } else if (tolower(readline('This folder does not exist. Would you like it to be created (y/n)? ')) %in% c('y', 'yes')) {
      dir.create(output_folder)
    } else {
      stop(paste0('The output folder ', output_folder, ' could not be created.'))
    }
  }

  # check to make sure no one has missing time in temporal_data
  if (any(is.na(temporal_data[[temporal_time]]))) {
    stop('You cannot have any missing time stamps in the temporal_time column.')
  }

  # check to make sure all patients in temporal_data
  # are accounted for in the fixed_data
  if (length(setdiff(temporal_data[[temporal_id]], fixed_data[[fixed_id]])) > 0) {
    stop('All ids in the temporal_data must also be present in the fixed_data.')
  }

  # check for duplicate patients in fixed_data
  if (length(unique(fixed_data[[fixed_id]])) < length(fixed_data[[fixed_id]])) {
    stop('You cannot have multiple rows for with the same id in the fixed_data.')
  }

  # Change step to numeric and set step_units
  step_units = NULL

  if (class(step) == 'Period') {
    if (step@year > 0) {
      step = step@year
      step_units = 'year'
    } else if (step@month > 0) {
      step = step$month
      step_units = 'month'
    } else if (step@day > 0) {
      step = step@day
      step_units = 'day'
    } else if (step@hour > 0) {
      step = step@hour
      step_units = 'hour'
    } else if (step@minute > 0) {
      step = step@minute
      step_units = 'minute'
    }
  }

  if (is.null(fixed_start)) { # if the start time is not provided, then the time will be indexed to min time
    suppressMessages({
      fixed_data =
        fixed_data %>%
        dplyr::left_join(., temporal_data %>%
                           dplyr::select_at(c(temporal_id, temporal_time)) %>%
                           dplyr::group_by(!!rlang::parse_expr(temporal_id)) %>%
                           dplyr::arrange(!!rlang::parse_expr(temporal_time)) %>%
                           dplyr::slice(1) %>% # Pick the first value (temporally)
                           dplyr::ungroup() %>%
                           dplyr::rename(!!rlang::parse_expr(fixed_id) := !!rlang::parse_expr(temporal_id)) %>%
                           dplyr::rename(wiz_start_time = !!rlang::parse_expr(temporal_time)))

      fixed_start = 'wiz_start_time'
    })
  }

  if (is.null(fixed_end)) { # if the start time is not provided, then the time will be indexed to min time
    suppressMessages({
      fixed_data =
        fixed_data %>%
        dplyr::left_join(., temporal_data %>%
                           dplyr::select_at(c(temporal_id, temporal_time)) %>%
                           dplyr::group_by(!!rlang::parse_expr(temporal_id)) %>%
                           dplyr::arrange(!!rlang::parse_expr(temporal_time)) %>%
                           dplyr::slice(dplyr::n()) %>% # Pick the last value (temporally)
                           dplyr::ungroup() %>%
                           dplyr::rename(!!rlang::parse_expr(fixed_id) := !!rlang::parse_expr(temporal_id)) %>%
                           dplyr::rename(wiz_end_time = !!rlang::parse_expr(temporal_time)))

      fixed_end = 'wiz_end_time'
    })
  }

  # check to make sure fixed_start is never greater than fixed_end
  if (any(fixed_data[[fixed_start]] > fixed_data[[fixed_end]])) {
    stop('fixed_start should never be greater than fixed_end.')
  }

  suppressMessages({
    temporal_data =
      temporal_data %>%
      dplyr::left_join(., fixed_data %>%
                         dplyr::select_at(c(fixed_id, fixed_start)) %>%
                         dplyr::rename(!!rlang::parse_expr(temporal_id) := !!rlang::parse_expr(fixed_id)) %>%
                         dplyr::rename(wiz_fixed_start_time = !!rlang::parse_expr(fixed_start))
      )
  })

  if (!is.null(step_units)) {
    temporal_data =
      temporal_data %>%
      dplyr::mutate(!!rlang::parse_expr(temporal_time) :=
                      lubridate::time_length(!!rlang::parse_expr(temporal_time) - wiz_fixed_start_time, unit = step_units)) %>%
      dplyr::select(-wiz_fixed_start_time)
  } else {
    temporal_data =
      temporal_data %>%
      dplyr::mutate(!!rlang::parse_expr(temporal_time) :=
                      !!rlang::parse_expr(temporal_time) - wiz_fixed_start_time) %>%
      dplyr::select(-wiz_fixed_start_time)
  }


  # Transform factors to characters
  fixed_data = fixed_data %>% dplyr::mutate_if(is.factor, as.character)
  temporal_data = temporal_data %>% dplyr::mutate_if(is.factor, as.character)

  # Generate a data dictionary for fixed_data
  fixed_data_dict =
    lapply(fixed_data, class) %>%
    lapply(function (x) x[1]) %>% # If multiple classes, take only the first one (happens with date-times)
    dplyr::as_tibble() %>%
    tidyr::gather(key = 'variable', value = 'class') %>%
    as.data.frame()

  suppressWarnings({
    temporal_data_dict =
      wiz_build_temporal_data_dictionary(temporal_data,
                                         temporal_variable,
                                         temporal_value,
                                         numeric_threshold)
  })


  wiz_frame =
    structure(list(
      fixed_data = as.data.frame(fixed_data),
      temporal_data = as.data.frame(temporal_data),
      fixed_id = fixed_id,
      fixed_start = fixed_start,
      fixed_end = fixed_end,
      temporal_id = temporal_id,
      temporal_time = temporal_time,
      temporal_variable = temporal_variable,
      temporal_value = temporal_value,
      temporal_category = temporal_category,
      step = step,
      max_length = max_length,
      step_units = step_units,
      output_folder = output_folder,
      fixed_data_dict = fixed_data_dict,
      temporal_data_dict = temporal_data_dict),
      class = 'wiz_frame')

  if (save_wiz_frame) {
    saveRDS(wiz_frame, file.path(output_folder, 'wiz_frame.rds'))
  }

  return(wiz_frame)
}



#' Determine the names and types of all of the temporal data variables.
#' This function assumes that the temporal data values may be characters if
#' some variables are categorical. This is an internal function.
#'
wiz_build_temporal_data_dictionary = function (temporal_data,
                                               temporal_variable,
                                               temporal_value,
                                               numeric_threshold = 0.5) {
  temporal_data_dict =
    temporal_data %>%
    dplyr::select_at(temporal_variable) %>%
    dplyr::pull(1) %>%
    unique() %>%
    dplyr::tibble(variable = .) %>%
    dplyr::mutate(class = 'unsure')

  temporal_data_class = class(temporal_data[[temporal_value]])

  if (temporal_data_class %in% c('integer', 'numeric')) {
  # If all variables are numeric/integer
    temporal_data_dict =
      temporal_data_dict %>%
      dplyr::mutate(class = 'numeric')
  } else {
    # If not, check data type for each temporal variable
    for (temporal_data_var in temporal_data_dict$variable) {

       temporal_data_values =
        temporal_data %>%
        dplyr::filter(!!rlang::parse_expr(temporal_variable) == temporal_data_var) %>%
        dplyr::pull(!!rlang::parse_expr(temporal_value))

      temporal_data_class = 'unsure'

      temporal_data_values_not_missing =
        temporal_data_values %>% na.omit() %>% length()

      # Convert to numeric to see how many values go missing
      temporal_data_values_numeric = suppressWarnings(as.numeric(temporal_data_values))
      temporal_data_values_numeric_not_missing =
        temporal_data_values_numeric %>% na.omit() %>% length()

      # Consider a number to be numeric if >= 50% of non-missing values are numeric
      if (temporal_data_values_numeric_not_missing >= numeric_threshold * temporal_data_values_not_missing) {
        temporal_data_class = 'numeric'
      } else {
        temporal_data_class = 'character'
      }

      temporal_data_dict =
        temporal_data_dict %>%
        dplyr::mutate(class = dplyr::if_else(
          variable == temporal_data_var,
          temporal_data_class,
          class))

      # message(temporal_data_var)
      # message(temporal_data_class)
    }
  }

  temporal_data_dict =
    temporal_data_dict %>%
    dplyr::arrange(variable) %>%
    as.data.frame()
  temporal_data_dict
}

#' Function that converts categorical temporal predictors into dummy variables
#'
#' Note that you can you can use this to dummy code variables with numerical values
#' where the values are supposed to map to categorical levels (e.g, 1 means high and 2
#' means low).
#'
#' Either provide a threshold (defaults to 0.5) or provide a vector of variables.
#' If you supply a vector of variables, this takes precedence over the numeric threshold.
#' @export
wiz_dummy_code = function(wiz_frame = NULL,
                          numeric_threshold = 0.5,
                          variables = NULL,
                          save_wiz_frame = FALSE) {

  if (is.null(variables)) { # if you do NOT supply a vector of variables (the default)

    categorical_vars = wiz_frame$temporal_data_dict %>%
      dplyr::filter(class == 'character') %>%
      dplyr::pull(variable)

    if (length(categorical_vars) == 0) {
      stop(paste('There are no categorical variables. There is no need to apply wiz_dummy_code(). ',
                 'To override this, please supply a vector of variable names to the variables argument.'))
    }

    wiz_frame$temporal_data = wiz_frame$temporal_data %>%
      dplyr::mutate(wiz_temp_var = (!!rlang::parse_expr(wiz_frame$temporal_variable)) %in% categorical_vars) %>%
      dplyr::mutate(!!rlang::parse_expr(wiz_frame$temporal_variable) :=
                      dplyr::case_when(
                        wiz_temp_var ~ paste0(!!rlang::parse_expr(wiz_frame$temporal_variable),
                                              '_',
                                              !!rlang::parse_expr(wiz_frame$temporal_value)),
                        TRUE ~ !!rlang::parse_expr(wiz_frame$temporal_variable)))  %>%
      dplyr::mutate(!!rlang::parse_expr(wiz_frame$temporal_value) :=
                      dplyr::case_when(
                        wiz_temp_var ~ '1',
                        TRUE ~ !!rlang::parse_expr(wiz_frame$temporal_value))) %>%
      dplyr::mutate_at(dplyr::vars(!!rlang::parse_expr(wiz_frame$temporal_value)), as.numeric) %>%
      dplyr::select(-wiz_temp_var) %>%
      as.data.frame()
  } else { # if you specify a vector of variables
    wiz_frame$temporal_data = wiz_frame$temporal_data %>%
      dplyr::mutate(wiz_temp_var = (!!rlang::parse_expr(wiz_frame$temporal_variable)) %in% variables) %>%
      dplyr::mutate(!!rlang::parse_expr(wiz_frame$temporal_variable) :=
                      dplyr::case_when(
                        wiz_temp_var ~ paste0(!!rlang::parse_expr(wiz_frame$temporal_variable),
                                              '_',
                                              !!rlang::parse_expr(wiz_frame$temporal_value)),
                        TRUE ~ !!rlang::parse_expr(wiz_frame$temporal_variable)))  %>%
      dplyr::mutate(!!rlang::parse_expr(wiz_frame$temporal_value) :=
                      dplyr::case_when(
                        wiz_temp_var ~ '1',
                        TRUE ~ !!rlang::parse_expr(wiz_frame$temporal_value))) %>%
      dplyr::mutate_at(dplyr::vars(!!rlang::parse_expr(wiz_frame$temporal_value)), as.numeric) %>%
      dplyr::select(-wiz_temp_var) %>%
      as.data.frame()
  }

  suppressWarnings({wiz_frame$temporal_data_dict =
    wiz_build_temporal_data_dictionary(wiz_frame$temporal_data,
                                       wiz_frame$temporal_variable,
                                       wiz_frame$temporal_value,
                                       numeric_threshold)})

  if (save_wiz_frame) {
    saveRDS(wiz_frame, file.path(wiz_frame$output_folder, 'wiz_frame.rds'))
  }

  wiz_frame
}





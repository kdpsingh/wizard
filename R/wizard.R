#' Define wizard frame
#' @export
wiz_frame = function(fixed_data,
                     temporal_data,
                     fixed_id = 'id',
                     fixed_start = '',
                     fixed_end = '',
                     temporal_id = 'id',
                     temporal_time = 'time',
                     temporal_variable = 'variable',
                     temporal_value = 'value',
                     temporal_category = temporal_variable,
                     step = NULL,
                     output_folder = NULL,
                     numeric_threshold = 0.5) {

  assertthat::assert_that('data.frame' %in% class(fixed_data))
  assertthat::assert_that('data.frame' %in% class(temporal_data))

  if (fixed_start != '') {
    if (class(fixed_data[[fixed_start]])[1] %in% c('Date', 'POSIXct', 'POSIXt') &
        class(step) != 'Period') {
      stop('Both the fixed_start column in the fixed_data and step must be in the same units.')
    }
    if (is.numeric(fixed_data[[fixed_start]]) & !is.numeric(step)) {
      stop('Both the fixed_start column in the fixed_data and step must be in the same units.')
    }
  }

  if (fixed_end != '') {
    if (class(fixed_data[[fixed_end]])[1] %in% c('Date', 'POSIXct', 'POSIXt') &
        class(step) != 'Period') {
      stop('Both the fixed_end column in the fixed_data and step must be in the same units.')
    }
    if (is.numeric(fixed_data[[fixed_end]]) & !is.numeric(step)) {
      stop('Both the fixed_end column in the fixed_data and step must be in the same units.')
    }
  }

  if (fixed_end != '') {
    if (class(temporal_data[[temporal_time]])[1] %in% c('Date', 'POSIXct', 'POSIXt') &
        class(step) != 'Period') {
      stop('Both the temporal_time column in the temporal_data and step must be in the same units.')
    }
    if (is.numeric(temporal_data[[temporal_time]]) & !is.numeric(step)) {
      stop('Both the temporal_time column in the temporal_data and step must be in the same units.')
    }
  }

  if (is.null(output_folder)) {
    stop('You must specify an output folder.')
  }

  if (!dir.exists(output_folder)) {
    if (tolower(readline('This folder does not exist. Would you like it to be created (y/n)? ')) %in% c('y', 'yes')) {
      dir.create(output_folder)
    } else {
    stop(paste0('The output folder ', output_folder, ' could not be created.'))
    }
  }

  # Transform factors to characters
  fixed_data = fixed_data %>% dplyr::mutate_if(is.factor, as.character)

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

    if (fixed_start != '') { # if the start time is provided, then the time will be indexed to that time as 0
      suppressMessages({
        temporal_data =
          temporal_data %>%
          dplyr::left_join(., fixed_data %>%
                             dplyr::select(dplyr::all_of(c(fixed_id, fixed_start))) %>%
                             dplyr::rename(!!rlang::parse_expr(temporal_id) := !!rlang::parse_expr(fixed_id)) %>%
                             dplyr::rename(wiz_fixed_start_time = !!rlang::parse_expr(fixed_start))
          ) %>%
          dplyr::mutate(!!rlang::parse_expr(temporal_time) :=
                          lubridate::time_length(!!rlang::parse_expr(temporal_time) - wiz_fixed_start_time, unit = step_units)) %>%
          dplyr::select(-wiz_fixed_start_time)
      })
    } else {
      temporal_data =
        temporal_data %>%
        dplyr::group_by(!!rlang::parse_expr(fixed_id)) %>%
        dplyr::mutate(wiz_fixed_start_time = min(!!rlang::parse_expr(temporal_time), na.rm = TRUE)) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(!!rlang::parse_expr(temporal_time) :=
                        lubridate::time_length(!!rlang::parse_expr(temporal_time) - wiz_fixed_start_time, unit = step_units)) %>%
        dplyr::select(-wiz_fixed_start_time)
    }
  }

  wiz_frame =
    structure(list(
      fixed_data = fixed_data,
      temporal_data = temporal_data,
      fixed_id = fixed_id,
      fixed_start = fixed_start,
      fixed_end = fixed_end,
      temporal_id = temporal_id,
      temporal_time = temporal_time,
      temporal_variable = temporal_variable,
      temporal_value = temporal_value,
      temporal_category = temporal_category,
      step = step,
      step_units = step_units,
      output_folder = output_folder,
      fixed_data_dict = fixed_data_dict,
      temporal_data_dict = temporal_data_dict),
      class = 'wiz_frame')

  # suppressWarnings({
  #   wiz_frame =
  #    wiz_frame %>%
  #    wiz_categorical_to_numeric()
  #})

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
    dplyr::select(dplyr::all_of(temporal_variable)) %>%
    dplyr::pull(1) %>%
    unique() %>%
    dplyr::tibble(variable = .) %>%
    dplyr::mutate(class = 'unsure')


  temporal_data_class = class(temporal_data[[temporal_value]])

  if (temporal_data_class %in% c('integer', 'numeric')) {
  # If all variables are numeric/integer
    temporal_data_dict =
      temporal_data_dict %>%
      dplyr::mutate(class = temporal_data_class)
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

  temporal_data_dict = temporal_data_dict %>% as.data.frame()
  temporal_data_dict
}

#' Function that converts categorical temporal predictors into dummy variables
#'
#' @param encoding How to encode categorical variables. Options include
#' \code{"one_hot"} for one-hot encoding, \code{"ref"} for reference encoding
#' where the first level (alphabetically) becomes the reference class, and
#' \code{"ref_for_binary"}, which reference-encodes categorical variables with
#' 2 levels and one-hot-encodes categorical variables with 3+ levels. Defaults
#' to \code{"one_hot"}.
#'
#' Internal function right now because only supports one_hot encoding
wiz_categorical_to_numeric = function(wiz_frame = NULL,
                                      encoding = 'one_hot',
                                      numeric_threshold = 0.5) {

  categorical_vars = wiz_frame$temporal_data_dict %>%
    dplyr::filter(class == 'character') %>%
    dplyr::pull(variable)

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
    dplyr::select(-wiz_temp_var)

  suppressWarnings({wiz_frame$temporal_data_dict =
    wiz_build_temporal_data_dictionary(wiz_frame$temporal_data,
                                       wiz_frame$temporal_variable,
                                       wiz_frame$temporal_value,
                                       numeric_threshold)})
  wiz_frame
}





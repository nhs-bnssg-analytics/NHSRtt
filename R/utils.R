# url processing ----------------------------------------------------------


#' Identify links within a given url
#'
#' @param url string; url of interest to identify the urls within
#'
#' @return named vector with urls linked to within the provided url
#' @importFrom xml2 read_html
#' @importFrom rvest html_elements html_attr html_text
#' @importFrom stats setNames
#'

obtain_links <- function(url) {
  url_html <- xml2::read_html(url)

  links <- url_html |>
    rvest::html_elements("a") |>
    rvest::html_attr("href")

  link_text <- url_html |>
    rvest::html_elements("a") |>
    rvest::html_text() |>
    trimws()

  links <- stats::setNames(
    links,
    nm = link_text
  )

  return(links)
}

#' Subset mmmyy from a string
#'
#' @param x the string
#'
#' @return subsetted string in the format "mmmyy"
#' @noRd
extract_monyr <- function(x) {
  start_character <- regexpr(
    pattern = "[[:alpha:]]{3}[[:digit:]]{2}",
    text = x
  ) |>
    as.numeric()

  monyr <- substr(
    x, start_character, start_character + 4
  )

  return(monyr)
}


#' @param zip_url string; url of zip file for downloading and unzipping
#' @importFrom dplyr pull
#' @importFrom utils download.file unzip
#' @importFrom rlang .data
#' @return file path location of the csv file stored in the zip file
#' @noRd
download_unzip_files <- function(zip_url) {
  # if (!isTRUE(file.info(directory)$isdir))
  #   dir.create(directory, recursive = TRUE)
  temporary_directory <- tempdir()
  temp <- tempfile()

  download.file(
    zip_url,
    temp,
    quiet = TRUE
  )

  zipped_files <- unzip(
    zipfile = temp,
    list = TRUE
  ) |>
    dplyr::pull(.data$Name)

  extracted_files <- unzip(
    zipfile = temp,
    files = zipped_files,
    exdir = temporary_directory
  )

  unlink(temp)

  return(extracted_files)
}

# data processing ---------------------------------------------------------


#' Removes the first few rows of data if they don't belong to table (eg, they
#' have rows of NAs in the source file)
#'
#' @param data data.frame
#' @importFrom dplyr mutate across starts_with
#' @returns data.frame with fewer rows if all the column names beging with V
#' @noRd
chop_top_off_data <- function(data) {
  if (all(grepl("V", names(data)))) {
    final_na_row <- max(
      match(NA, data$V1)
    )
    new_names <- data[final_na_row + 1, ] |>
      as.character()

    data <- data[-seq_len(final_na_row + 1),] |>
      setNames(nm = new_names) |>
      mutate(
        across(
          starts_with("Gt"),
          as.numeric
        )
      )

  }
  return(data)
}

#' Some months have "Treatment function name" for the "Treatment function code"
#' field. This function will check and fix that problem
#' @noRd
adjust_treatment_function_field_name <- function(data) {
  nms <- names(data)

  if (!("Treatment Function Code" %in% nms)) {
    nms[nms == "Treatment Function Name"] <- "Treatment Function Code"
  }

  names(data) <- nms

  return(data)
}

#' Manipulates the year and period fields in the raw data files so they provide
#' the correct period
#'
#' @param data data.frame
#'
#' @returns data.frame
#' @noRd
make_period_field <- function(data) {
  nms <- names(data)

  period_name <- names(data)[grep("^Period", names(data))]
  period <- unique(data[[period_name]])

  if ("Year" %in% nms) {

    yr <- as.numeric(
      substr(unique(data[["Year"]]), 1, 4)
    )

    mnth <- match(tolower(period), tolower(month.name))

    if (mnth < 4) yr <- yr + 1

    final_period <- as.Date(
      paste(
        yr,
        mnth,
        1,
        sep = "-"
      )
    )
  } else {
    final_period <- as.Date(
      gsub("RTT", "01", period),
      format = "%d-%B-%Y"
    )
  }

  data[[period_name]] <- final_period

  return(data)
}

#' creates table of weights to apply to dates that represent the end of a week.
#' These weights correspond tot he proportion of that week's counts that fall
#' into the month that the "week end date" occurs, and the proportion of the
#' counts that fall into the previous month
#'
#' @description rtt data is published monthly and represents a snapshot of
#'   counts at the end of the given month. The weekly counts are provided within
#'   the data. To convert the weekly counts to monthly counts we must sum the
#'   weekly counts within each month. Some weeks will fall over month ends, so
#'   this function identifies those, and calculates the number of days those
#'   weeks fall within each month. These days are then used to calculate a
#'   weighting, which is the output of the function.
#' @param week_end_dates vector of dates that represent the final date of the
#'   week in consideration (this doesn't have to be a specific day of the week
#'   as the reference point for this purpose is the end of the month which can
#'   fall on any day of the week)
#'
#' @importFrom lubridate mday floor_date %m-%
#' @importFrom dplyr tibble case_when mutate filter
#' @importFrom tidyr pivot_longer
#' @importFrom rlang .data
#'
#' @return a tibble of week_end, wait_start_month, month_weight where week_end
#'   are the dates provided, wait_start_month is the start of the month that the
#'   waiting period began, and month_weight is a value between 0 and
#'   1 to apply to the counts within that week
month_attribution_lkp <- function(week_end_dates) {
  # put dates in a tibble
  all_dates <- dplyr::tibble(
    week_end = unique(week_end_dates),
    # if 7 days or more fall in the month, give it the value of 7, otherwise it
    # is the day of the month
    month_1 = case_when(
      lubridate::mday(.data$week_end) > 7 ~ 7,
      .default = lubridate::mday(.data$week_end)
    ),
    month_0 = 7 - .data$month_1
  ) |>
    # create a longer table with 2 records per week_end date with a record per
    # proportion to apply to current month and previous month
    tidyr::pivot_longer(
      cols = starts_with("month"),
      names_to = "wait_start_month",
      values_to = "month_weight"
    ) |>
    dplyr::mutate(
      month_weight = .data$month_weight / 7,
      wait_start_month = case_when(
        .data$wait_start_month == "month_1" ~
          lubridate::floor_date(.data$week_end, unit = "months"),
        .default = lubridate::floor_date(
          .data$week_end %m-% months(1), unit = "months"
        ),
      )
    ) |>
    # remove record where month_weight is 0
    dplyr::filter(
      .data$month_weight != 0
    )

  return(all_dates)

}


#' Calculates the flow from each stock at each timestep
#'
#' @details for each timestep, the stock is calculated as the current count of
#' patients waiting at that timestep plus the inflow (via referrals or
#' incomplete pathways from previous timestep) minus outflow (pathways have been
#' completed or reneges, which are deduced from the other flows). This stock is
#' divided into "months waited", and patients with incomplete pathways at the
#' end of the timestep are incremented up to an additional month's waiting time
#'
#' For each of the data frames supplied, the period_id and the months_waited_id
#' fields should be numeric/integers. period_id is an integer representing the
#' chronology of the data, and months_waited_id is a numeric representation of
#' the months waited, where 0 represents 0 to 1 month, and 5 represents 5 to 6
#' months.
#'
#' @param referrals data frame with two columns; period_id and referrals. This
#'   represents the count of referrals in each period
#' @param incompletes data from with three columns; period_id, months_waited_id,
#'   and incompletes. This represents the count of incomplete pathways by the
#'   number of months waited for each period
#' @param completes  data from with three columns; period_id, months_waited_id,
#'   and treatments. This represents the count of completed pathways by the
#'   number of months waited for each period
#' @param max_months_waited integer; the maximum number of months to group
#'   patients waiting times by for the analysis. Data are published up to 104
#'   weeks, so 24 is likely to be the maximum useful value for this argument
#'
#' @importFrom rlang .data
#' @return a tibble with fields for months_waited_id, period_id, node_inflow,
#'   waiting_same_node and treatments. These represent all the counts being
#'   moved on a single time step
#'
calculate_timestep_transitions <- function(referrals, incompletes, completes, max_months_waited) {
  # inflows at each time step (period) are a combination of referrals in the
  # period and incomplete counts from the previous time step

  referrals <- referrals |>
    dplyr::filter(
      .data$period_id > min(.data$period_id) # no need for input at timestep t-1
    ) |>
    mutate(
      months_waited_id = 0
    ) |>
    dplyr::select(
      "period_id",
      "months_waited_id",
      node_inflow = "referrals"
    )

  incomplete_at_previous_timeperiod <- incompletes |>
    mutate(
      period_id = .data$period_id + 1,
      months_waited_id = case_when(
        months_waited_id == max_months_waited ~ max_months_waited, # this prevents new bins appearing at the extent of the waiting period
        .default = months_waited_id + 1
      )
    ) |>
    filter(
      # remove final period because no input is needed for the next time step
      period_id != max(.data$period_id)
    ) |>
    dplyr::summarise(
      node_inflow = sum(.data$incompletes),
      .by = c(
        "period_id",
        "months_waited_id"
      )
    )

  inflow <- bind_rows(
    referrals,
    incomplete_at_previous_timeperiod
  )

  # calculate the counts of those waiting at the same node
  incomplete_counts <- incompletes |>
    select(
      "months_waited_id",
      "period_id",
      waiting_same_node = "incompletes"
    )

  transitions <- inflow |>
    left_join(
      completes,
      by = join_by(
        months_waited_id,
        period_id
      )
    ) |>
    left_join(
      incomplete_counts,
      by = join_by(
        months_waited_id,
        period_id
      )
    )


  return(transitions)
}

#' Redistribute cases where there are more treatments and reneges than people in
#' a particular bin for a particular period. This function chooses the
#' redistribute the surplus treatment to the people waiting the longest first
#'
#' @param incomplete_counts numeric; vector of incomplete counts for a bin for a
#'   period
redistribute_incompletes_evenly <- function(incomplete_counts) {

  if (sum(incomplete_counts) < 0) {
    # force the negatives to 0
    incomplete_counts <- rep(0, length(incomplete_counts))
  } else {
    while (any(incomplete_counts < 0)) {
      # total negative counts
      total_negatives <- sum(incomplete_counts[incomplete_counts < 0])

      # force the negatives to 0
      incomplete_counts[incomplete_counts < 0] <- 0

      # count of positive values to proportion the total incomplete counts over
      positive_stocks <- length(
        incomplete_counts[incomplete_counts > 0]
      )

      # adjustment
      adjustment <- total_negatives / positive_stocks

      # adjust positive stocks to account for negative incompletes
      incomplete_counts[incomplete_counts > 0] <-
        incomplete_counts[incomplete_counts > 0] + adjustment

    }
  }
  return(incomplete_counts)
}

#' Redistribute cases where there are more treatments and reneges than people in
#' a particular bin for a particular period. This function chooses the
#' redistribute the surplus treatment to the people waiting the longest first
#'
#' @param incomplete_counts numeric; vector of count of patients with an incomplete
#'   pathway within a bin in a period in order of lowest to highest bin
#'
redistribute_incompletes_optimally <- function(incomplete_counts) {

  # reallocate surplus treatments to highest bins that still require treatment

  # calculate total surplus treatments
  total_surplus_treatments <- abs(sum(incomplete_counts[incomplete_counts < 0]))
  surplus_indices <- incomplete_counts < 0 # used later for amending the surplus

  surplus_treatments <- total_surplus_treatments
  if (total_surplus_treatments > 0) {
    for (i in rev(seq_len(length(incomplete_counts)))) {
      waiting_in_bin <- incomplete_counts[i]

      if (waiting_in_bin > 0 & surplus_treatments > 0) {
        # what number from the bin can be treated
        n_treatments <- min(waiting_in_bin, surplus_treatments)

        # treat the patients waiting in the bin
        incomplete_counts[i] <- incomplete_counts[i] - n_treatments

        # adjust the number of surplus_treatments
        surplus_treatments <- surplus_treatments - n_treatments
      }

    }

    # proportionally redistribute the remaining surplus to the bins that
    # originally had surplus
    incomplete_counts[surplus_indices] <- surplus_treatments *
      (incomplete_counts[surplus_indices] / total_surplus_treatments)
  }
  return(incomplete_counts)
}


#' Calculate the number of incomplete pathways in a bin from the inflow, reneges
#' and treatments. Following the calculations, apply an optional redistribution
#' of the occasions where incompletes are negative (eg, more treatments have
#' been performed than there were people).
#'
#' @param inflow numeric; vector of count of patients moving into a bin in a
#'   period in order of lowest to highest bin
#' @param reneges numeric; vector of count of patients reneging from a bin in a
#'   period in order of lowest to highest bin
#' @param treatments numeric; vector of count of patients being treated from a
#'   bin in a period in order of lowest to highest bin
#' @param redistribution_method string; one of "none", "evenly" or
#'   "prioritise_long_waiters"
#'
calculate_incompletes <- function(inflow, reneges, treatments, redistribution_method) {

  redistribution_method <- match.arg(
    redistribution_method,
    c("none",
      "evenly",
      "prioritise_long_waiters")
  )

  incompletes <- inflow - treatments - reneges

  if (redistribution_method == "none") {
    return(incompletes)
  } else if (redistribution_method == "evenly") {
    incompletes <- redistribute_incompletes_evenly(incompletes)
  } else if (redistribution_method == "prioritise_long_waiters") {
    incompletes <- redistribute_incompletes_optimally(incompletes)
  }

  return(incompletes)
}

#' create distribution of data points based on a weibull curve
#' @param x vector of values between 0 and 1 representing the quantile
#' @noRd
weibull_sample <- function(x) {

  distribution_curve <- stats::rweibull(100, shape = 0.95, scale = 1) |>
    stats::quantile(1 - x)

  return(distribution_curve)
}


#' Pivot the parameters passed into the function so relationships between the
#' parameters remain consistent but giving control to providing more or less
#' focus on extreme bins
#'
#' @param params numeric vector of parameters; assumed to be in order of
#'   increasing waiting times (bins)
#' @param skew numeric; length 1, a multiplier to be used on the final
#'   parameter. A skew of 1 will keep the params identical to the input params.
#'   Value must be greater than 0
#' @param skew_method character; one of "rotate" or "uniform". The "rotate"
#'   method will multiply the highest bin by the skew value, the pivot_bin will
#'   remain the same as its input, and all of the other bins between the pivot
#'   bin and the highest bin will be multiplied by an interpolated value between
#'   1 and the skew value. Bins between the second bin and the pivot bin will be
#'   multiplied by interpolated value between (1 / skew) and 1. If the method is
#'   "uniform", then all of the bins greater than or equal to the pivot bin will
#'   be multiplied by the skew value, and the bins below the pivot bin will be
#'   multiplied by (1 / skew)
#' @param pivot_bin numeric; when applying the skew, whichever method, select a
#'   bin to pivot around. If NULL, the mid-bin between the highest available bin
#'   and the second bin will be used. The first item of the parameter inputs is
#'   bin 0, so a pivot_bin of 2, will be the third item
#'
#' @details The skew parameter is applied to the final item of the params
#'   vector. The inverse of the skew parameter is applied to the second item of
#'   the params vector. The adjustment made to the items in between item to and
#'   item n are calculated by a linear extrapolation between the inverse skew
#'   and the skew. The first item is held constant.
#'
#' @importFrom dplyr tibble
#' @export
#' @returns a revised numeric vector of parameters
#'
#' @examples
#' apply_parameter_skew(
#'   params = c(0.03, 0.02, 0.02, 0.01, 0.04, 0.05),
#'   skew = 1.05
#' )
apply_parameter_skew <- function(params, skew, skew_method = "rotate", pivot_bin = NULL) {

  # check params is numeric
  if (!is.numeric(params))
    stop("params must be numeric")

  # check skew is numeric
  if (!is.numeric(skew))
    stop("skew must be numeric")

  # check skew is length 1
  if (length(skew) != 1)
    stop("skew must be length 1")

  # check skew is not negative
  if (skew <= 0)
    stop("skew must be greater than 0")

  # check skew_method input
  skew_method <- match.arg(
    skew_method,
    c("rotate", "uniform")
  )

  params_length <- length(params)

  if (params_length <= 2) return(params)

  if (is.null(pivot_bin)) {
    pivot_bin <- (params_length / 2)
  }

  #  plus one is to avoid the first compartment
  pivot_bin <- pivot_bin + 1

  if (skew_method == "rotate") {

    rotate_func <- function(bottom_bin, top_bin, skew, location_relative_to_pivot) {

      if (location_relative_to_pivot == "above_pivot") {
        skew_vals <- c(1, skew)

        pivot_rounded <- ceiling(bottom_bin)
        new_x_vals <- seq(pivot_rounded, top_bin)
      } else if (location_relative_to_pivot == "below_pivot") {
        skew_vals <- c((1 / skew), 1)

        pivot_rounded <- floor(top_bin)
        new_x_vals <- seq(bottom_bin, pivot_rounded)
      }

      lm_tbl <- dplyr::tibble(
        x = c(bottom_bin, top_bin),
        y = skew_vals
      )

      fit <- lm(y ~ x, data = lm_tbl)

      multipliers <- predict(
        object = fit,
        newdata = tibble(x = new_x_vals)
      ) |>
        unname()

      return(multipliers)
    }

    below_pivot_multipliers <- rotate_func(
      bottom_bin = 2,
      top_bin = pivot_bin,
      skew = skew,
      location_relative_to_pivot = "below_pivot"
    )

    above_pivot_multipliers <- rotate_func(
      bottom_bin = pivot_bin,
      top_bin = params_length,
      skew = skew,
      location_relative_to_pivot = "above_pivot"
    )

    if (pivot_bin %% 1 == 0) above_pivot_multipliers <- above_pivot_multipliers[-1]

    params_out <- params * c(1, below_pivot_multipliers, above_pivot_multipliers)

  } else if (skew_method == "uniform") {

    if (pivot_bin %% 1 != 0) pivot_bin <- ceiling(pivot_bin)

    params_temp <- params
    params_temp[2:(pivot_bin - 1)] <-
      params_temp[2:(pivot_bin - 1)] * (1 / skew)

    params_temp[pivot_bin:params_length] <-
      params_temp[pivot_bin:params_length] * skew

    params_out <- params_temp
  }

  return(params_out)
}


# string functions --------------------------------------------------------
#' convert the string version of months waited to the numeric id version
#' @param months_waited string; vector with format, by example "2-3"
#' @param max_months_waited integer; the maximum number of months to group
#'   patients waiting times by for the analysis. Data are published up to 104
#'   weeks, so 24 is likely to be the maximum useful value for this argument.
#' @export
#'
#' @examples
#'
#' mnths_waited <- c("<1", "1-2", "2-3", "3-4", "4-5", "5+")
#' convert_months_waited_to_id(
#'   months_waited = mnths_waited,
#'   max_months_waited = 3
#' )
convert_months_waited_to_id <- function(months_waited, max_months_waited) {
  months_waited <- as.character(months_waited)

  # change "<1" to "0"
  months_waited[grepl("^<", months_waited)] <- "0"

  # change ">x" to "x"
  months_waited[grepl("^>", months_waited)] <- gsub(">", "", months_waited[grepl("^>", months_waited)])

  # replace all values with the first numeric value, eg, "10-11" will become 10
  months_waited <- as.numeric(
    sub("\\D*(\\d+).*", "\\1", months_waited)
  )

  # control the max value of months waited
  months_waited[months_waited > max_months_waited] <- max_months_waited

  return(months_waited)
}

parse_number <- function(x) {
  parsed_number <- as.numeric(
    unlist(
      regmatches(
        x,
        gregexpr(
          "[-]{0,1}[[:digit:]]+\\.{0,1}[[:digit:]]*",
          x
        )
      )
    )
  )
  return(parsed_number)
}


parse_first_number <- function(x) {
  parsed_number <- as.numeric(
    unlist(
      regmatches(
        x,
        regexpr(
          "[[:digit:]]{2,3}\\s",
          x
        )
      )
    )
  )
  return(parsed_number)
}

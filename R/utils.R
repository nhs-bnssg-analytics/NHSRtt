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


#' Subset string from the right
#'
#' @param x the string
#' @param n integer; number of letters to subset from on the right
#'
#' @return subsetted string
#'
substr_right <- function(x, n){
  substr(x, nchar(x) - n + 1, nchar(x))
}


#' Download a file from a url to a temporary file
#'
#' @param excel_url sting; url of the file. Must have an xlsx or xls extension
#' @param filename string; name of file
#' @importFrom tools file_ext
#' @importFrom utils download.file
#' @return filepath to where the temporary file is stored
download_temp_file <- function(excel_url, filename) {
  temporary_directory <- tempdir()
  temp_file <- paste0(
    temporary_directory,
    "/",
    filename,
    ".",
    tools::file_ext(excel_url)
  )
  # tmp_file <- tempfile(
  #   pattern = names(excel_url),
  #   fileext = tools::file_ext(excel_url)
  # )

  download.file(
    url = excel_url,
    destfile = temp_file,
    quiet = TRUE,
    mode = "wb"
  )

  return(temp_file)
}


# data processing ---------------------------------------------------------

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
      .data$period_id > 0 # not need for input at timestep t-1
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
        .data$months_waited_id == max_months_waited ~ max_months_waited, # this prevents new bins appearing at the extent of the waiting period
        .default = .data$months_waited_id + 1
      )
    ) |>
    filter(
      # remove final period because no input is needed for the next time step
      period_id != max(.data$period_id)
    ) |>
    dplyr::summarise(
      node_inflow = sum(.data$incompletes),
      .by = c(
        .data$period_id,
        .data$months_waited_id
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

#' Redistribute the counts of incompletes within a vector where they are
#' negative into the positive values
#' @param incomplete_counts numeric; vector of incomplete counts
redistribute_incompletes <- function(incomplete_counts) {

  if (sum(incomplete_counts) < 0) {
    warning("not possible to redistribute incompletes because the sum of incompletes is negative")
    # force the negatives to 0
    incomplete_counts[incomplete_counts < 0] <- 0
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

#' rescale values to between 0 and 1
#' @noRd
weibull_sample <- function(x) {

  distribution_curve <- stats::rweibull(100, shape = 0.95, scale = 1) |>
    stats::quantile(1 - x)

  return(distribution_curve)
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

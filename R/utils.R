# url processing ----------------------------------------------------------


#' Identify links within a given url
#'
#' @param url string; url of interest to identify the urls within
#'
#' @return named vector with urls linked to within the provided url
#' @importFrom xml2 read_html
#' @importFrom rvest html_nodes html_attr html_text
#'

obtain_links <- function(url) {
  url_html <- xml2::read_html(url)

  links <- url_html |>
    rvest::html_nodes("a") |>
    rvest::html_attr("href")

  link_text <- url_html |>
    rvest::html_nodes("a") |>
    rvest::html_text() |>
    trimws()

  links <- setNames(
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
#' @importFrom tools file_ext
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
      lubridate::mday(week_end) > 7 ~ 7,
      .default = lubridate::mday(week_end)
    ),
    month_0 = 7 - month_1
  ) |>
    # create a longer table with 2 records per week_end date with a record per
    # proportion to apply to current month and previous month
    tidyr::pivot_longer(
      cols = starts_with("month"),
      names_to = "wait_start_month",
      values_to = "month_weight"
    ) |>
    dplyr::mutate(
      month_weight = month_weight / 7,
      wait_start_month = case_when(
        wait_start_month == "month_1" ~
          lubridate::floor_date(week_end, unit = "months"),
        .default = lubridate::floor_date(
          week_end %m-% months(1), unit = "months"
        ),
      )
    ) |>
    # remove record where month_weight is 0
    dplyr::filter(
      month_weight != 0
    )

  return(all_dates)

}

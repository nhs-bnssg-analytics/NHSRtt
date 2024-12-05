
#' Download and tidy the referral to treatment data from the NHS Statistics
#' webpage
#'
#' @param url string; url of the NHS Referral to Treatment (RTT) Waiting Times
#' @param date_start date; start date (earliest date is 1st April 2011, but the
#'   default is 1st April 2019)
#' @param date_end; date; end date (defaults to "today")
#'
#' @return a tibble with fields for trust, specialty, period, type,
#'   months_waited and value
#' @importFrom purrr map
#' @export
#'

get_rtt_data <- function(url = "https://www.england.nhs.uk/statistics/statistical-work-areas/rtt-waiting-times/", date_start = as.Date("2019-04-01"), date_end = Sys.Date()) {

  # check date inputs
  if (!inherits(date_start, "Date")) stop("date_start needs to be a date format")
  if (!inherits(date_end, "Date")) stop("date_end needs to be a date format")

  # calculate start year for financial year for date_start
  month_start <- as.numeric(format(date_start, "%m"))
  year_start <- as.numeric(format(date_start, "%Y"))
  if (month_start < 4) {
    year_start <- year_start - 1
  }

  if (year_start <= 2015)
    stop("The data import function currently isn't set up for data prior to April 2016")


  # calculate end financial year for date_end
  month_end <- as.numeric(format(date_end, "%m"))
  year_end <- as.numeric(format(date_end, "%y"))
  if (month_end >= 4) {
    year_end <- year_end + 1
  }

  annual_urls <- obtain_links(url) |>
    (\(x) x[grepl("^[0-9]{4}-[0-9]{2}", names(x))])() |>
    (\(x) x[grepl("/$", x)])() |>
    (\(x) x[as.numeric(substr(names(x), 1, 4)) >= year_start])() |>
    (\(x) x[as.numeric(substr(names(x), 6, 7)) <= year_end])()

  xl_files <- purrr::map(
    annual_urls,
    obtain_links
  ) |>
    unlist() |>
    (function(x) x[grepl("xls$|xlsx$", x)])() |>
    (function(x) x[grepl("Provider", x)])()

  # standardise the names of the links
  names(xl_files) <- gsub("NonAdmitted|Non Admitted|Non-admitted", "Non-Admitted", names(xl_files))
  names(xl_files) <- gsub(".*RTT waiting times data\\.", "", names(xl_files))
  names(xl_files) <- gsub("([[:alpha:]]{3}[0-9]{2}).*", "\\1", names(xl_files))

  dts <- as.Date(paste0("01", substr_right(names(xl_files), 5)), "%d%b%y")

  xl_files <- xl_files[dts >= date_start]
  return(xl_files)
}


#' Find the first row of the table to import
#'
#' @param filepath file path for the excel file
#' @param sheet string; sheet name
#' @importFrom dplyr pull
#' @importFrom readxl read_excel
#' @return integer representing the number of rows to skip in the sheet until
#'   the header column of interest
identify_n_skip_rows <- function(filepath, sheet = "Provider") {
  skip_rows <- readxl::read_excel(
    path = filepath,
    sheet = sheet,
    range = "B1:B20",
    col_names = "test"
  ) |>
    pull(1) |>
    (\(x) grep("Region Code", x) - 1)()

  return(skip_rows)
}


#' Make the excel file a tidy format
#'
#' @inheritParams identify_n_skip_rows
#' @param n_skip number of rows to skip before reading in main table from sheet
#' @importFrom readxl read_excel
#' @importFrom dplyr select mutate rename case_when summarise n if_else
#' @importFrom tidyr pivot_longer replace_na
#' @importFrom glue glue
#' @importFrom stringr str_extract
#' @import lubridate
#' @return a tidy tibble
tidy_file <- function(excel_filepath, sheet = "Provider", n_skip) {

  mnth <- substring(
    excel_filepath,
    first = regexpr("[[[:alpha:]]{3}[0-9]{2}", excel_filepath),
    last = regexpr("[[[:alpha:]]{3}[0-9]{2}", excel_filepath) + 4
  )
  mnth <- as.Date(paste0("01", mnth), "%d%b%y")

  rtt <- readxl::read_excel(
    path = excel_filepath,
    .name_repair = "minimal",
    sheet = sheet,
    skip = n_skip
  ) |>
    dplyr::select(
      trust = "Provider Code",
      specialty = "Treatment Function Code",
      starts_with(">"),
      ends_with("plus"),
      any_of("Number of new RTT clock starts during the month")
    ) |>
    dplyr::mutate(
      type = gsub(" Provider.*$", "", basename(excel_filepath)),
      period = mnth
    )

  if (grepl("New Periods", excel_filepath)) {
    rtt <- rtt |>
      dplyr::rename(
        value = "Number of new RTT clock starts during the month"
      )
  }

  if (grepl("Non-Admitted|Admitted|Incomplete", excel_filepath)) {
    rtt <- rtt |>
      tidyr::pivot_longer(
        cols = !c("trust", "specialty", "type", "period"),
        names_to = "weeks_waited",
        values_to = "value",
        values_transform = as.numeric
      ) |>
      dplyr::mutate(
        row_id = 1:n(),
        week_val = replace_na(as.numeric(str_extract(weeks_waited, "(?<=-)([0-9]{1,3})")), 105),
        month_break = month(period - lubridate::dweeks(week_val - 1)) != month(period - lubridate::dweeks(week_val)),
        balance_1 = if_else(month_break, as.numeric(day(period - lubridate::dweeks(week_val - 1))), 7),
        balance_2 = if_else(month_break, (rollforward(period - lubridate::dweeks(week_val)) -(period - lubridate::dweeks(week_val)))/ddays(1), 0),
        month_1 = interval(rollforward(period - lubridate::dweeks(week_val - 1)), period) %/% months(1),
        month_2 = interval(rollforward(period - lubridate::dweeks(week_val)), period) %/% months(1),
        value_1 = value * balance_1/7,
        value_2 = value * balance_2/7,
        months.waited_1 = case_when(
          month_1 == 0 ~ "<1",
          month_1 == 23 & month_2 == 24 ~ "24+",
          TRUE ~ unclass(glue::glue("{month_1}-{month_1 + 1}")) # need to drop glue class
        ),
        months.waited_2 = case_when(
          month_2 == 0 ~ "<1",
          month_1 == 23 & month_2 == 24 ~ "24+",
          TRUE ~ unclass(glue::glue("{month_2}-{month_2 + 1}")) # need to drop glue class
        )
      ) |>
      dplyr::select(-weeks_waited, -value, -week_val, -month_break, -balance_1, -balance_2, -month_1, -month_2) |>
      tidyr::pivot_longer(cols = c(months.waited_1, months.waited_2, value_1, value_2),
                   names_to = c(".value", "var"),
                   names_sep = "_") |>
      dplyr::rename(months_waited = months.waited) |>
      dplyr::mutate(months_waited = factor(months_waited, levels = c(
        "<1",
        "0-1",
        "1-2",
        "2-3",
        "3-4",
        "4-5",
        "5-6",
        "6-7",
        "7-8",
        "8-9",
        "9-10",
        "10-11",
        "11-12",
        "12-13",
        "13-14",
        "14-15",
        "15-16",
        "16-17",
        "17-18",
        "18-19",
        "19-20",
        "20-21",
        "21-22",
        "22-23",
        "23-24",
        "24+"
      ))) |>
      summarise(
        value = sum(value),
        .by = c(
          trust,
          specialty,
          period,
          months_waited
        )
      )
  }

  unlink(excel_filepath)

  return(rtt)
}

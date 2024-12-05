
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
#' @importFrom dplyr select mutate
#' @importFrom tidyr pivot_longer
#' @return a tidy tibble
tidy_file <- function(excel_filepath, sheet = "Provider", n_skip) {

  mnth <- substring(
    excel_filepath,
    first = regexpr("[[[:alpha:]]{3}[0-9]{2}", excel_filepath),
    last = regexpr("[[[:alpha:]]{3}[0-9]{2}", excel_filepath) + 4
  )

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
      ends_with("plus")
    ) |>
    tidyr::pivot_longer(
      cols = !c("trust", "specialty"),
      names_to = "weeks_waited",
      values_to = "value",
      values_transform = as.numeric
    ) |>
    dplyr::mutate(
      type = gsub(" .*$", "", basename(excel_filepath)),
      period = mnth
    )


  #' trust, specialty, period, type,
  #' #'   months_waited and value
  #' trust, specialty, period, type,
  #' #'   months_waited and value


  # unlink(excel_filepath)

  return(rtt)
}

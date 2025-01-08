
#' Download and tidy the referral to treatment data from the NHS Statistics
#' webpage
#'
#' @param type string; one of "complete", "incomplete" or "referral"
#' @param url string; url of the NHS Referral to Treatment (RTT) Waiting Times
#' @param date_start date; start date (earliest date is 1st April 2016, but the
#'   default is 1st April 2019)
#' @param date_end date; end date (defaults to "today")
#' @param show_progress logical; show progress of downloading and processing
#'   files. Defaults to false
#'
#' @return a tibble with fields for trust, specialty, period, type,
#'   months_waited and value
#' @importFrom purrr map
#' @export
#'
#' @examples
#' \dontrun{
#' monthly_rtt <- NHSRtt::get_rtt_data(
#'   type = "complete",
#'   date_start = as.Date("2024-10-01"),
#'   date_end = as.Date("2024-11-01"),
#'   show_progress = TRUE
#' )
#' }
#'
get_rtt_data <- function(type, url = "https://www.england.nhs.uk/statistics/statistical-work-areas/rtt-waiting-times/",
                         date_start = as.Date("2019-04-01"), date_end = Sys.Date(), show_progress = FALSE) {

  # check date inputs
  if (!inherits(date_start, "Date")) stop("date_start needs to be a date format")
  if (!inherits(date_end, "Date")) stop("date_end needs to be a date format")

  # check show_progress
  if (length(show_progress) != 1) {
    stop("show_progress must be length 1")
  } else if (!is.logical(show_progress)) {
    stop("show_progress must be TRUE or FALSE")
  } else if (is.na(show_progress)) {
    stop("show_progress must be TRUE or FALSE")
  }

  # check inputs for type
  type <- match.arg(
    type,
    c("complete", "incomplete", "referral")
  )

  # calculate start year for financial year for date_start
  month_start <- as.numeric(format(date_start, "%m"))
  year_start <- as.numeric(format(date_start, "%Y"))

  if (month_start == 1) {
    month_start <- 12
    year_start <- year_start - 1
  } else {
    month_start <- month_start - 1
  }

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
    (\(x) x[!grepl("xls$", x)])() |>
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

  # include additional filter for type
  if (type == "complete") {
    type_filter <- "Admitted"
  } else if (type == "incomplete") {
    type_filter <- "Incomplete"
  } else if (type == "referral") {
    type_filter <- "New Periods"
  }

  xl_files <- xl_files[grepl(paste0("^", type_filter), names(xl_files))]

  # filter for dates
  dts <- as.Date(paste0("01", substr_right(names(xl_files), 5)), "%d%b%y")
  xl_files <- xl_files[dplyr::between(dts, date_start, date_end)]

  # update on progress
  if (show_progress == TRUE) cat("Downloading data...\n")

  # download the files into a temporary location
  xl_files <- xl_files |>
    purrr::imap_chr(
      download_temp_file,
      .progress = show_progress
    )

  # update on progress
  if (show_progress == TRUE) cat("Understanding sheet structures...\n")

  # calculate the number of rows to skip at the start of each sheet when reading
  # the files in
  skip_rows <- purrr::map_dbl(
    xl_files,
    identify_n_skip_rows,
    .progress = show_progress
  )

  # update on progress
  if (show_progress == TRUE) cat("Reading and tidying data...\n")

  # read in and tidy the files
  df <- purrr::map2(
    .x = xl_files,
    .y = skip_rows,
    .f = ~ tidy_file(
      excel_filepath = .x,
      n_skip = .y
    ),
    .progress = show_progress
  ) |>
    purrr::list_rbind()

  return(df)
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
#' @param excel_filepath string; file path to the file location
#' @importFrom readxl read_excel
#' @importFrom dplyr select mutate rename case_when summarise left_join join_by starts_with ends_with any_of
#' @importFrom tidyr pivot_longer
#' @importFrom lubridate ceiling_date month year
#' @importFrom rlang .data
#' @return a tidy tibble
tidy_file <- function(excel_filepath, sheet = "Provider", n_skip) {

  mnth <- substring(
    basename(excel_filepath),
    first = regexpr("[[[:alpha:]]{3}[0-9]{2}", basename(excel_filepath)),
    last = regexpr("[[[:alpha:]]{3}[0-9]{2}", basename(excel_filepath)) + 4
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
        # extract the first number from the weeks waited string
        fewest_weeks_waited = as.numeric(
          sub("\\D*(\\d+).*", "\\1", .data$weeks_waited)
        ),
        # calculate the end date of the week relative to the end of the month for
        # the reporting period
        week_end = (lubridate::ceiling_date(
          x = .data$period,
          unit = "months"
        ) - 1) - (7 * .data$fewest_weeks_waited)
      )

    monthly_proportions <- month_attribution_lkp(
      rtt[["week_end"]]
    ) |>
      dplyr::mutate(
        months_waited = (lubridate::month(mnth) -
                           lubridate::month(.data$wait_start_month)) +
          (12 * (lubridate::year(mnth) -
                   lubridate::year(.data$wait_start_month))),
        months_waited = dplyr::case_when(
          .data$months_waited == 0 ~ "<1",
          .data$months_waited >= 24 ~ "24+",
          .default = paste(.data$months_waited, .data$months_waited + 1, sep = "-")
        ),
        months_waited = factor(
          .data$months_waited,
          levels = c("<1", paste(0:23, 1:24, sep = "-"), "24+")
        )
      )

    rtt <- rtt |>
      dplyr::left_join(
        monthly_proportions,
        by = dplyr::join_by(
          week_end
        )
      ) |>
      dplyr::summarise(
        value = sum(.data$value * .data$month_weight),
        .by = c(
          .data$trust,
          .data$specialty,
          .data$period,
          .data$months_waited,
          .data$type
        )
      )
  }

  unlink(excel_filepath)

  return(rtt)
}


#' Create a set of dummy data to put through the functions of the package
#'
#' @param type string; one of "referral", "incomplete" or "complete"
#' @param max_months_waited integer; the maximum number of months to group
#'   patients waiting times by for the analysis. Data are published up to 104
#'   weeks, so 24 is likely to be the maximum useful value for this argument.
#' @param number_periods integer; the intended number of periods in the dataset
#' @param referral_values integer; vector of values that are sampled from for
#'   the count of referrals at each time step
#' @param max_incompletes integer; the maximum number of incomplete pathways
#'   possible in one time step
#' @param max_treatments integer; the maximum number of treatments possible
#' @param seed seed to generate the random data from
#'
#' @importFrom dplyr tibble mutate
#' @importFrom rlang .data
#' @return a tibble whose columns depend on the type input. If type is
#'   "referral" then it will have two fields, period_id and referrals. If type
#'   is "complete" or "incomplete", the fields will be period_id,
#'   months_waited_id and treatments/incompletes, depending on the type value
#' @export
#'
#' @examples
#' create_dummy_data(
#'   type = "referral",
#'   max_months_waited = 4,
#'   number_period = 6
#' )
create_dummy_data <- function(type, max_months_waited, number_periods,
                              referral_values = 500:700,
                              max_incompletes = 500,
                              max_treatments = 500,
                              seed = 123) {
  type <- match.arg(
    type,
    c("referral", "complete", "incomplete")
  )

  set.seed(seed)

  periods <- c(
    0, seq_len(number_periods)
  )

  if (type == "referral") {
    out <- dplyr::tibble(
      period_id = periods,
      referrals = sample(referral_values, length(periods), replace = TRUE)
    )
  } else {
    months <- c(
      0, seq_len(max_months_waited)
    )

    out <- expand.grid(
      period_id = periods,
      months_waited_id = months
    ) |>
      tidyr::complete(
        period_id,
        months_waited_id = 0:24
      ) |>
      mutate(
        percentile = months_waited_id / 24,
        value = weibull_sample(
          .data$percentile
        ),
        .by = period_id
      )

    if (type == "incomplete") {
      scale_value <- max_incompletes
      type_name <- "incompletes"
    } else if (type == "complete") {
      scale_value <- max_treatments
      type_name <- "treatments"
    }

    out <- out |>
      mutate(
        # rescale value to the scale of interest determined by
        # scale_value
        value = scale_value * (.data$value / max(.data$value)),
        months_waited_id = case_when(
          months_waited_id <= max_months_waited ~ months_waited_id,
          .default = max_months_waited
        )
      ) |>
      summarise(
        {{ type_name }} := sum(.data$value),
        .by = c(
          period_id,
          months_waited_id
        )
      )


  }

  return(out)

}

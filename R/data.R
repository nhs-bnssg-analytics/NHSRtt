#' Returns the date of the latest available data
#'
#' @inheritParams get_rtt_data
#' @importFrom lubridate ceiling_date
#' @returns the latest date of available data
#' @export
#'
#' @examples latest_rtt_date()
latest_rtt_date <- function(
  url = "https://www.england.nhs.uk/statistics/statistical-work-areas/rtt-waiting-times/"
) {
  latest_file <- latest_rtt_file(url)

  latest_date <- as.Date(
    paste0(
      '01',
      sub(".*?\\b([A-Za-z]{3}\\d{2}).*", "\\1", latest_file)
    ),
    format = '%d%b%y'
  )

  latest_date <- lubridate::ceiling_date(
    latest_date,
    unit = "months"
  ) -
    1

  return(latest_date)
}

#' Returns the url for the latest available file
#'
#' @inheritParams get_rtt_data
#' @importFrom utils head
latest_rtt_file <- function(
  url = "https://www.england.nhs.uk/statistics/statistical-work-areas/rtt-waiting-times/"
) {
  latest_file <- obtain_links(url) |>
    (\(x) x[grepl("^[0-9]{4}-[0-9]{2}", names(x))])() |>
    (\(x) x[!grepl("xls$", x)])() |>
    head(1) |>
    (\(x) obtain_links(x))() |>
    (\(x) x[grepl("zip$", x)])() |>
    head(1)

  return(latest_file)
}

#' Returns a table of organisations and how they map to one another from the
#' latest available RTT file
#'
#' @inheritParams get_rtt_data
#' @importFrom dplyr distinct as_tibble
#' @importFrom data.table fread
#' @returns a tibble with fields for "NHS Region Code", "NHS Region Name",
#'   "Provider", "Parent Org Code", "Provider Parent Name", "Provider Org Code",
#'   "Provider Org Name", "Commissioner", "Parent Org Code", "Commissioner
#'   Parent Name", "Commissioner Org Code",	"Commissioner Org Name"
#' @export
#'
latest_orgs <- function(
  url = "https://www.england.nhs.uk/statistics/statistical-work-areas/rtt-waiting-times/"
) {
  latest_lkp <- latest_rtt_file(url) |>
    download_unzip_files() |>
    data.table::fread(
      na.strings = ""
    ) |>
    chop_top_off_data() |>
    make_period_field() |>
    adjust_treatment_function_field_name() |>
    dplyr::distinct(
      .data$`Provider Parent Org Code`,
      .data$`Provider Parent Name`,
      .data$`Provider Org Code`,
      .data$`Provider Org Name`,
      .data$`Commissioner Parent Org Code`,
      .data$`Commissioner Parent Name`,
      .data$`Commissioner Org Code`,
      .data$`Commissioner Org Name`
    ) |>
    mutate(
      `Provider Org Name` = case_when(
        .data$`Provider Org Name` == "DUCHY HOSPITAL" ~
          paste0(
            .data$`Provider Org Name`,
            " (",
            gsub(
              "NHS | INTEGRATED CARE BOARD",
              "",
              .data$`Provider Parent Name`
            ),
            ")"
          ),
        .default = .data$`Provider Org Name`
      )
    )

  latest_lkp <- latest_lkp |>
    dplyr::left_join(
      icb_lkp,
      by = join_by(
        `Provider Parent Org Code`
      )
    ) |>
    dplyr::as_tibble()

  return(latest_lkp)
}

#' Download and tidy the referral to treatment data from the NHS Statistics
#' webpage
#'
#' @param url string; url of the NHS Referral to Treatment (RTT) Waiting Times
#' @param date_start date; start date (earliest date is 1st May 2016, but the
#'   default is 1st April 2019)
#' @param date_end date; end date (defaults to "today")
#' @param show_progress logical; show progress of downloading and processing
#'   files. Defaults to false
#' @inheritParams tidy_file
#'
#' @return a tibble with fields for trust_parent_org_code,
#'   commissioner_parent_org_code, commissioner_org_code, trust, specialty,
#'   period, type, months_waited and value
#' @importFrom purrr map
#' @export
#'
#' @examples
#' \dontrun{
#' monthly_rtt <- NHSRtt::get_rtt_data(
#'   date_start = as.Date("2024-10-01"),
#'   date_end = as.Date("2024-11-01"),
#'   show_progress = TRUE
#' )
#' }
#'
get_rtt_data <- function(
  url = "https://www.england.nhs.uk/statistics/statistical-work-areas/rtt-waiting-times/",
  date_start = as.Date("2019-04-01"),
  date_end = Sys.Date(),
  trust_parent_codes = NULL,
  commissioner_parent_codes = NULL,
  commissioner_org_codes = NULL,
  trust_codes = NULL,
  specialty_codes = NULL,
  show_progress = FALSE
) {
  # check date inputs
  if (!inherits(date_start, "Date")) {
    stop("date_start needs to be a date format")
  }
  if (!inherits(date_end, "Date")) {
    stop("date_end needs to be a date format")
  }

  # check show_progress
  if (length(show_progress) != 1) {
    stop("show_progress must be length 1")
  } else if (!is.logical(show_progress)) {
    stop("show_progress must be TRUE or FALSE")
  } else if (is.na(show_progress)) {
    stop("show_progress must be TRUE or FALSE")
  }

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

  if (year_start <= 2015) {
    stop(
      "The data import function currently isn't set up for data prior to April 2016"
    )
  }

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

  zip_files <- purrr::map(
    annual_urls,
    obtain_links
  ) |>
    unlist() |>
    (function(x) x[grepl("zip$", x)])() |>
    # remove the latest version which is duplicated
    (\(x) x[!duplicated(x)])()

  # filter for dates
  dts <- as.Date(paste0("01", extract_monyr(names(zip_files))), "%d%b%y")
  zip_files <- zip_files[dplyr::between(dts, date_start, date_end)]

  # update on progress
  if (show_progress == TRUE) {
    cat("Downloading data...\n")
  }

  # download the files into a temporary location
  csv_files <- zip_files |>
    purrr::imap_chr(
      ~ download_unzip_files(.x),
      .progress = show_progress
    )

  # update on progress
  if (show_progress == TRUE) {
    cat("Reading and tidying data...\n")
  }

  # read in and tidy the files
  df <- purrr::map(
    .x = csv_files,
    .f = ~ tidy_file(
      csv_filepath = .x,
      trust_parent_codes = trust_parent_codes,
      commissioner_parent_codes = commissioner_parent_codes,
      commissioner_org_codes = commissioner_org_codes,
      trust_codes = trust_codes,
      specialty_codes = specialty_codes
    ),
    .progress = show_progress
  ) |>
    purrr::list_rbind()

  return(df)
}

#' Read in and tidy the csv file
#'
#' @param csv_filepath string; file path to the file location
#' @param trust_parent_codes character; vector of trust parent codes to filter
#'   for (optional); eg "QE1" is NHS Lancashire and South Cumbria Integrated
#'   Care Board
#' @param commissioner_parent_codes character; vector of commissioner parent
#'   codes to filter for (optional); eg "QF7" is NHS South Yorkshire Integrated
#'   Care Board
#' @param commissioner_org_codes character; vector of commissioner org codes to
#'   filter for (optional); eg "15C" is the NHS Bristol, North Somerset and
#'   South Gloucestershire (sub-ICB location)
#' @param trust_codes character; vector of trust codes to filter for (optional);
#'   eg "R1A" is  Herefordshire and Worcestershire Health and Care NHS Trust
#' @param specialty_codes character; vector of specialty codes to filter for (optional);
#'   eg "C_999" is "Total" specialties
#' @importFrom data.table fread fcase
#' @importFrom dtplyr lazy_dt
#' @importFrom dplyr select mutate summarise left_join join_by starts_with
#'   distinct as_tibble filter union any_of
#' @importFrom tidyr pivot_longer
#' @importFrom lubridate ceiling_date month year
#' @importFrom rlang .data
#' @return a tidy tibble
tidy_file <- function(
  csv_filepath,
  trust_parent_codes = NULL,
  commissioner_parent_codes = NULL,
  commissioner_org_codes = NULL,
  trust_codes = NULL,
  specialty_codes = NULL
) {
  rtt <- data.table::fread(
    input = csv_filepath,
    na.strings = ""
  ) |>
    chop_top_off_data() |>
    make_period_field() |>
    adjust_treatment_function_field_name() |>
    lazy_dt() |>
    dplyr::select(
      period = any_of(c("Period", "Period Name")),
      trust_parent_org_code = "Provider Parent Org Code",
      commissioner_parent_org_code = "Commissioner Parent Org Code",
      commissioner_org_code = "Commissioner Org Code",
      trust = "Provider Org Code",
      specialty = "Treatment Function Code",
      type = "RTT Part Description",
      starts_with("Gt"), # only includes pathways with known clock starts
      total_all = "Total All"
    ) |>
    filter(
      .data$type %in%
        c(
          "Completed Pathways For Admitted Patients",
          "Completed Pathways For Non-Admitted Patients",
          "Incomplete Pathways",
          "New RTT Periods - All Patients"
        ),
      .data$commissioner_org_code != "NONC"
    ) |>
    dplyr::mutate(
      type = data.table::fcase(
        .data$type == "Incomplete Pathways",
        "Incomplete",
        .data$type == "New RTT Periods - All Patients",
        "Referrals",
        .data$type %in%
          c(
            "Completed Pathways For Admitted Patients",
            "Completed Pathways For Non-Admitted Patients"
          ),
        "Complete"
      )
    )

  if (!is.null(trust_parent_codes)) {
    rtt <- rtt |>
      dplyr::filter(
        .data$trust_parent_org_code %in% trust_parent_codes
      )
  }

  if (!is.null(commissioner_parent_codes)) {
    rtt <- rtt |>
      dplyr::filter(
        .data$commissioner_parent_org_code %in% commissioner_parent_codes
      )
  }

  if (!is.null(commissioner_org_codes)) {
    rtt <- rtt |>
      dplyr::filter(
        .data$commissioner_org_code %in% commissioner_org_codes
      )
  }

  if (!is.null(trust_codes)) {
    rtt <- rtt |>
      dplyr::filter(
        .data$trust %in% trust_codes
      )
  }

  if (!is.null(specialty_codes)) {
    rtt <- rtt |>
      dplyr::filter(
        .data$specialty %in% specialty_codes
      )
  }

  mnth <- unique(dplyr::as_tibble(rtt)[["period"]])

  referrals <- rtt |>
    filter(
      .data$type == "Referrals"
    ) |>
    mutate(
      months_waited = "<1",
      total_all = as.numeric(.data$total_all)
    ) |>
    select(
      "trust_parent_org_code",
      "commissioner_parent_org_code",
      "commissioner_org_code",
      "trust",
      "specialty",
      "period",
      "months_waited",
      "type",
      value = "total_all"
    )

  compl_incompl <- rtt |>
    filter(
      .data$type != "Referrals"
    ) |>
    # aggregate the non-admitted and admitted pathways
    dplyr::summarise(
      across(
        starts_with("Gt"),
        ~ sum(.x, na.rm = TRUE)
      ),
      .by = c(
        "period",
        "trust_parent_org_code",
        "commissioner_parent_org_code",
        "commissioner_org_code",
        "trust",
        "specialty",
        "type"
      )
    ) |>
    tidyr::pivot_longer(
      cols = !c(
        "trust_parent_org_code",
        "commissioner_parent_org_code",
        "commissioner_org_code",
        "trust",
        "specialty",
        "type",
        "period"
      ),
      names_to = "weeks_waited",
      values_to = "value"
    ) |>
    dplyr::mutate(
      # extract the first number from the weeks waited string
      fewest_weeks_waited = parse_first_number(.data$weeks_waited),
      # calculate the end date of the week relative to the end of the month for
      # the reporting period
      week_end = (lubridate::ceiling_date(
        x = mnth,
        unit = "months"
      ) -
        1) -
        (7 * .data$fewest_weeks_waited)
    ) |>
    dplyr::as_tibble()

  monthly_proportions <- compl_incompl |>
    dplyr::distinct(.data$week_end) |>
    dplyr::pull(.data$week_end) |>
    month_attribution_lkp() |>
    dplyr::mutate(
      months_waited = (lubridate::month(mnth) -
        lubridate::month(.data$wait_start_month)) +
        (12 *
          (lubridate::year(mnth) -
            lubridate::year(.data$wait_start_month))),
      months_waited = dplyr::case_when(
        .data$months_waited == 0 ~ "<1",
        .data$months_waited >= 24 ~ "24+",
        .default = paste(
          .data$months_waited,
          .data$months_waited + 1,
          sep = "-"
        )
      ),
      months_waited = factor(
        .data$months_waited,
        levels = c("<1", paste(0:23, 1:24, sep = "-"), "24+")
      )
    )

  rtt <- compl_incompl |>
    dtplyr::lazy_dt() |>
    dplyr::left_join(
      monthly_proportions,
      by = "week_end"
    ) |>
    dplyr::summarise(
      value = sum(.data$value * .data$month_weight, na.rm = TRUE),
      .by = c(
        "trust_parent_org_code",
        "commissioner_parent_org_code",
        "commissioner_org_code",
        "trust",
        "specialty",
        "period",
        "months_waited",
        "type"
      )
    ) |>
    mutate(
      period = as.Date(.data$period)
    ) |>
    dplyr::union(
      referrals
    ) |>
    dplyr::as_tibble()

  if (basename(dirname(csv_filepath)) != "sheets") {
    unlink(csv_filepath)
  }

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
create_dummy_data <- function(
  type,
  max_months_waited,
  number_periods,
  referral_values = 9000:12000,
  max_incompletes = 10000,
  max_treatments = 500,
  seed = 123
) {
  type <- match.arg(
    type,
    c("referral", "complete", "incomplete")
  )

  set.seed(seed)

  periods <- c(
    0,
    seq_len(number_periods)
  )

  if (type == "referral") {
    out <- dplyr::tibble(
      period_id = periods,
      referrals = sample(referral_values, length(periods), replace = TRUE)
    )
  } else {
    months <- c(
      0,
      seq_len(max_months_waited)
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
        .by = "period_id"
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
          "period_id",
          "months_waited_id"
        )
      )
  }

  return(out)
}

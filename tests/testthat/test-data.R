test_that("get_rtt_data error checks", {
  expect_error(
    get_rtt_data(
      type = "complete",
      date_start = "2024-10-01",
      date_end = as.Date("2024-11-01"),
      show_progress = TRUE
    ),
    "date_start needs to be a date format",
    info = "date_start is a date"
  )

  expect_error(
    get_rtt_data(
      type = "complete",
      date_start = as.Date("2024-10-01"),
      date_end = "2024-11-01",
      show_progress = TRUE
    ),
    "date_end needs to be a date format",
    info = "date_end is a date"
  )

  expect_error(
    get_rtt_data(
      type = "complete",
      date_start = as.Date("2024-10-01"),
      date_end = as.Date("2024-11-01"),
      show_progress = c(TRUE, TRUE)
    ),
    "show_progress must be length 1",
    info = "show_progress is length 1"
  )

  expect_error(
    get_rtt_data(
      type = "complete",
      date_start = as.Date("2024-10-01"),
      date_end = as.Date("2024-11-01"),
      show_progress = "test"
    ),
    "show_progress must be TRUE or FALSE",
    info = "show_progress is logical"
  )

  expect_error(
    get_rtt_data(
      type = "complete",
      date_start = as.Date("2024-10-01"),
      date_end = as.Date("2024-11-01"),
      show_progress = NA
    ),
    "show_progress must be TRUE or FALSE",
    info = "show_progress is not NA"
  )

  expect_snapshot(
    get_rtt_data(
      type = "completes",
      date_start = as.Date("2024-10-01"),
      date_end = as.Date("2024-11-01"),
      show_progress = FALSE
    ),
    error = TRUE
  )

  expect_error(
    get_rtt_data(
      type = "complete",
      date_start = as.Date("2013-10-01"),
      date_end = as.Date("2024-11-01"),
      show_progress = FALSE
    ),
    "The data import function currently isn't set up for data prior to April 2016",
    info = "date_start is too early"
  )

})



test_that("get_rtt_data functionality", {
  completes <- get_rtt_data(
    type = "complete",
    date_start = as.Date("2023-01-01"),
    date_end = as.Date("2023-01-31"),
    show_progress = FALSE
  )

  incompletes <- get_rtt_data(
    type = "incomplete",
    date_start = as.Date("2023-01-01"),
    date_end = as.Date("2023-01-31"),
    show_progress = FALSE
  )

  referrals <- get_rtt_data(
    type = "referral",
    date_start = as.Date("2023-05-01"),
    date_end = as.Date("2023-05-31"),
    show_progress = FALSE
  )

  expected_names_comps_incomps <- c(
    "trust", "specialty", "period", "months_waited", "type", "value"
  )

  expected_names_referrals <- c(
    "trust", "specialty", "value", "type", "period"
  )

  # completes
  expect_identical(
    names(completes),
    expected_names_comps_incomps,
    info = "names of completes are expected"
  )

  expect_gt(
    nrow(completes),
    0
  )

  expect_false(
    any(is.na(completes)),
    info = "there are NAs within the completes data"
  )

  expect_identical(
    names(incompletes),
    expected_names_comps_incomps,
    info = "names of incompletes are expected"
  )

  expect_gt(
    nrow(incompletes),
    0
  )

  expect_false(
    any(is.na(incompletes)),
    info = "there are NAs within the incompletes data"
  )

  expect_identical(
    names(referrals),
    expected_names_referrals,
    info = "names of referrals are expected"
  )

  expect_gt(
    nrow(referrals),
    0
  )

  expect_false(
    any(is.na(referrals)),
    info = "there are NAs within the referrals data"
  )


})

test_that("identify_n_skip_rows functionality", {

  rows <- identify_n_skip_rows(
    # filepath = test_sheet("New-Periods-Provider-Apr23.xls"),
    filepath = system.file(
      "extdata", "New-Periods-Provider-Apr23.xls",
      package = "NHSRtt", mustWork = TRUE),
    sheet = "Provider"
  )

  expect_equal(
    rows,
    13,
    info = "correct number of rows to skip"
  )
})

test_that("tidy_file functionality", {

  # tidied_referrals <- tidy_file(
  #   # excel_filepath = test_sheet("New-Periods-Provider-Apr23.xls"),
  #   excel_filepath = system.file(
  #     "extdata", "New-Periods-Provider-Apr23.xls",
  #     package = "NHSRtt", mustWork = TRUE),
  #   sheet = "Provider",
  #   n_skip = 13
  # )


  tidied_referrals <- tidy_file(
    excel_filepath = testthat::test_path(
      testthat::test_path(
        file.path("..", "..", "inst", "extdata", "New-Periods-Provider-Apr23.xls")
      )
    ),
    sheet = "Provider",
    n_skip = 13
  )
  expected_names <- c(
    "trust", "specialty",
    "Number of new RTT clock starts during the month",
    "type", "period"
  )

  expect_equal(
    names(tidied_referrals),
    expected_names,
    info = "names are as expected"
  )

  expect_gt(
    nrow(tidied_referrals),
    0
  )

  expect_false(
    any(is.na(tidied_referrals)),
    info = "there are NAs within the tidied referrals data"
  )
})


test_that("create_dummy_data functionality", {

  expect_snapshot(
    create_dummy_data(
      type = "completes",
      max_months_waited = 4,
      number_periods = 10,
      max_treatments = 50
    ),
    error = TRUE
  )

  mx_treatments <- 50
  mnth_waited_comp <- 24
  pds_comp <- 10
  dummy_complete <- create_dummy_data(
    type = "complete",
    max_months_waited = mnth_waited_comp,
    number_periods = pds_comp,
    max_treatments = mx_treatments
  )

  expect_equal(
    names(dummy_complete),
    c("period_id", "months_waited_id", "treatments"),
    info = "names are as expected"
  )

  expect_equal(
    max(dummy_complete$treatments),
    mx_treatments,
    info = "max number of treatments is expected"
  )

  expect_equal(
    max(dummy_complete$months_waited_id),
    mnth_waited_comp,
    info = "max months waited is expected"
  )

  expect_equal(
    max(dummy_complete$period_id),
    pds_comp,
    info = "max periods is expected"
  )

  expect_gt(
    nrow(dummy_complete),
    0
  )

  expect_false(
    any(is.na(dummy_complete)),
    info = "there are NAs within the dummy_complete data"
  )

  mx_incompletes <- 100
  mnth_waited_inc <- 13
  pds_inc <- 5
  dummy_incomplete <- create_dummy_data(
    type = "incomplete",
    max_months_waited = mnth_waited_inc,
    number_periods = pds_inc,
    max_incompletes = mx_incompletes
  )

  expect_equal(
    names(dummy_incomplete),
    c("period_id", "months_waited_id", "incompletes"),
    info = "names are as expected"
  )

  expect_equal(
    max(dummy_incomplete$incompletes),
    mx_incompletes,
    info = "max number of incompletes is expected"
  )

  expect_equal(
    max(dummy_incomplete$months_waited_id),
    mnth_waited_inc,
    info = "max months waited is expected"
  )

  expect_equal(
    max(dummy_incomplete$period_id),
    pds_inc,
    info = "max periods is expected"
  )

  expect_gt(
    nrow(dummy_incomplete),
    0
  )

  expect_false(
    any(is.na(dummy_incomplete)),
    info = "there are NAs within the dummy_incomplete data"
  )

  referral_vals <- 50:100
  pds_ref <- 2
  dummy_referrals <- create_dummy_data(
    type = "referral",
    number_periods = pds_ref,
    referral_values = referral_vals
  )

  expect_equal(
    names(dummy_referrals),
    c("period_id", "referrals"),
    info = "names are as expected"
  )

  expect_gte(
    min(dummy_referrals$referrals),
    min(referral_vals)
  )

  expect_lte(
    max(dummy_referrals$referrals),
    max(referral_vals)
  )

  expect_equal(
    max(dummy_referrals$period_id),
    pds_ref,
    info = "max periods is expected"
  )

  expect_gt(
    nrow(dummy_referrals),
    0
  )

  expect_false(
    any(is.na(dummy_referrals)),
    info = "there are NAs within the dummy_referrals data"
  )

})

test_that("get_rtt_data error checks", {
  expect_error(
    get_rtt_data(
      date_start = "2024-10-01",
      date_end = as.Date("2024-11-01"),
      show_progress = TRUE
    ),
    "date_start needs to be a date format",
    info = "date_start is a date"
  )

  expect_error(
    get_rtt_data(
      date_start = as.Date("2024-10-01"),
      date_end = "2024-11-01",
      show_progress = TRUE
    ),
    "date_end needs to be a date format",
    info = "date_end is a date"
  )

  expect_error(
    get_rtt_data(
      date_start = as.Date("2024-10-01"),
      date_end = as.Date("2024-11-01"),
      show_progress = c(TRUE, TRUE)
    ),
    "show_progress must be length 1",
    info = "show_progress is length 1"
  )

  expect_error(
    get_rtt_data(
      date_start = as.Date("2024-10-01"),
      date_end = as.Date("2024-11-01"),
      show_progress = "test"
    ),
    "show_progress must be TRUE or FALSE",
    info = "show_progress is logical"
  )

  expect_error(
    get_rtt_data(
      date_start = as.Date("2024-10-01"),
      date_end = as.Date("2024-11-01"),
      show_progress = NA
    ),
    "show_progress must be TRUE or FALSE",
    info = "show_progress is not NA"
  )

  expect_error(
    get_rtt_data(
      date_start = as.Date("2013-10-01"),
      date_end = as.Date("2024-11-01"),
      show_progress = FALSE
    ),
    "The data import function currently isn't set up for data prior to April 2016",
    info = "date_start is too early"
  )

})


test_that("get_rtt_data functionality", {
  df <- get_rtt_data(
    date_start = as.Date("2023-01-01"),
    date_end = as.Date("2023-01-31"),
    trust_codes = "RTE",
    show_progress = FALSE
  )

  expected_names <- c(
    "trust_parent_org_code",
    "commissioner_parent_org_code",
    "commissioner_org_code",
    "trust", "specialty", "period", "months_waited", "type", "value"
  )

  # completes
  expect_identical(
    names(df),
    expected_names,
    info = "names of get_rtt_data are expected"
  )

  expect_gt(
    nrow(df),
    0
  )

  expect_false(
    any(is.na(df |> dplyr::select(!c("commissioner_parent_org_code")))),
    info = "there are NAs within the completes data"
  )


  expect_equal(
    df |>
      dplyr::filter(type == "Referrals") |>
      dplyr::pull(.data$months_waited) |>
      unique() |>
      as.character(),
    "<1",
    info = "all referrals have months waited '<1'"
  )


})

test_that("tidy_file functionality", {

  tidied_rbd <- tidy_file(
    csv_filepath = test_sheet("20241130-RTT-November-2024-RBD.csv")
  )

  expected_names <- c(
    "trust_parent_org_code",
    "commissioner_parent_org_code",
    "commissioner_org_code",
    "trust", "specialty", "period", "months_waited", "type", "value"
  )

  expect_equal(
    names(tidied_rbd),
    expected_names,
    info = "names are as expected"
  )

  expect_gt(
    nrow(tidied_rbd),
    0
  )

  expect_false(
    any(is.na(tidied_rbd |> dplyr::select(!c("commissioner_parent_org_code")))),
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

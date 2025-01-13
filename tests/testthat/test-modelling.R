test_that("calibrate_capacity_renege_params errors", {
  max_months <- 4
  refs <- create_dummy_data(
    type = "referral",
    max_months_waited = max_months,
    number_periods = 6
  )
  incomp <- create_dummy_data(
    type = "incomplete",
    max_months_waited = max_months,
    number_periods = 6
  )

  comp <- create_dummy_data(
    type = "complete",
    max_months_waited = max_months,
    number_periods = 6
  )

  expect_error(
    params <- calibrate_capacity_renege_params(
      referrals = refs,
      incompletes = incomp,
      completes = comp,
      max_months_waited = max_months,
      redistribute_m0_reneges = "error"
    ),
    "adjust_renege_param must be TRUE or FALSE",
    info = "logical redistribute_m0_reneges parameter"
  )


  expect_error(
    calibrate_capacity_renege_params(
      referrals = refs,
      incompletes = incomp,
      completes = comp,
      max_months_waited = max_months,
      redistribute_m0_reneges = NA
    ),
    "adjust_renege_param must be TRUE or FALSE",
    info = "non NA redistribute_m0_reneges parameter"
  )

  expect_error(
    calibrate_capacity_renege_params(
      referrals = refs,
      incompletes = incomp,
      completes = comp,
      max_months_waited = "four",
      redistribute_m0_reneges = TRUE
    ),
    "max_months_waited must be numeric",
    info = "non-string max_months_waited parameter"
  )

  expect_error(
    calibrate_capacity_renege_params(
      referrals = refs,
      incompletes = incomp,
      completes = comp,
      max_months_waited = 4:5,
      redistribute_m0_reneges = TRUE
    ),
    "max_months_waited must be length 1",
    info = "single item length max_months_waited parameter"
  )

  expect_error(
    calibrate_capacity_renege_params(
      referrals = refs |> rename(referral = referrals),
      incompletes = incomp,
      completes = comp,
      max_months_waited = max_months,
      redistribute_m0_reneges = TRUE
    ),
    "the field names for referrals should be period_id and referrals",
    info = "field names for referrals"
  )

  expect_error(
    calibrate_capacity_renege_params(
      referrals = refs,
      incompletes = incomp,
      completes = comp |> rename(completes = treatments),
      max_months_waited = max_months,
      redistribute_m0_reneges = TRUE
    ),
    "the field names for completes should be period_id, months_waited_id and treatments",
    info = "field names for treatments"
  )

  expect_error(
    calibrate_capacity_renege_params(
      referrals = refs,
      incompletes = incomp |> rename(waiting_times = incompletes),
      completes = comp,
      max_months_waited = max_months,
      redistribute_m0_reneges = TRUE
    ),
    "field names for incompletes",
    info = "the field names for incompletes should be period_id, months_waited_id and incompletes"
  )

  expect_error(
    calibrate_capacity_renege_params(
      referrals = dplyr::bind_rows(refs, refs),
      incompletes = incomp,
      completes = comp,
      max_months_waited = max_months,
      redistribute_m0_reneges = TRUE
    ),
    "period_id is repeated in referrals data",
    info = "repeated data in referrals"
  )

  expect_error(
    calibrate_capacity_renege_params(
      referrals = refs,
      incompletes = incomp,
      completes = bind_rows(comp, comp),
      max_months_waited = max_months,
      redistribute_m0_reneges = TRUE
    ),
    "repeated combinations of period_id and months_waited_id in completes data",
    info = "repeated data in completes"
  )

  expect_error(
    calibrate_capacity_renege_params(
      referrals = refs,
      incompletes = bind_rows(incomp, incomp),
      completes = comp,
      max_months_waited = max_months,
      redistribute_m0_reneges = TRUE
    ),
    "repeated combinations of period_id and months_waited_id in incompletes data",
    info = "repeated data in incompletes"
  )

  expect_error(
    calibrate_capacity_renege_params(
      referrals = refs |> dplyr::add_row(period_id = max(refs$period_id) + 1, referrals = max(refs$referrals) + 50),
      incompletes = incomp,
      completes = comp,
      max_months_waited = max_months,
      redistribute_m0_reneges = TRUE
    ),
    "referrals and completes should have the same number of period_ids",
    info = "differing period_ids in referrals and completes"
  )

  expect_error(
    calibrate_capacity_renege_params(
      referrals = refs,
      incompletes = incomp |> dplyr::add_row(
        period_id = max(incomp$period_id) + 1,
        months_waited_id = max(incomp$months_waited_id) + 1,
        incompletes = max(incomp$incompletes) + 50
      ),
      completes = comp,
      max_months_waited = max_months,
      redistribute_m0_reneges = TRUE
    ),
    "completes and incompletes should have the same dimensions",
    info = "completes and incompletes should have different dimensions"
  )

  expect_error(
    calibrate_capacity_renege_params(
      referrals = refs |> filter(period_id != 2),
      incompletes = incomp |> filter(period_id != 2),
      completes = comp |> filter(period_id != 2),
      max_months_waited = max_months,
      redistribute_m0_reneges = TRUE
    ),
    "There is a missing period_id from the referrals, completes and incompletes data",
    info = "all periods are in referrals, completes and incompletes data"
  )

})


test_that("calibrate_capacity_renege_params warnings", {
  max_months <- 4
  refs <- create_dummy_data(
    type = "referral",
    max_months_waited = max_months,
    number_periods = 6
  )
  incomp <- create_dummy_data(
    type = "incomplete",
    max_months_waited = max_months,
    number_periods = 6
  )

  comp <- create_dummy_data(
    type = "complete",
    max_months_waited = max_months,
    number_periods = 6
  )

  expect_warning(
    calibrate_capacity_renege_params(
      referrals = refs,
      incompletes = incomp,
      completes = comp,
      max_months_waited = max_months,
      redistribute_m0_reneges = TRUE
    ),
    "negative renege parameters present, investigate raw data",
    info = "parameter calcs lead to negative reneges"
  )

})


test_that("calibrate_capacity_renege_params functionality", {
  max_months <- 4
  periods <- 3
  refs <- create_dummy_data(
    type = "referral",
    max_months_waited = max_months,
    number_periods = periods
  )
  incomp <- create_dummy_data(
    type = "incomplete",
    max_incompletes = 100,
    max_months_waited = max_months,
    number_periods = periods
  )

  comp <- create_dummy_data(
    type = "complete",
    max_months_waited = max_months,
    number_periods = periods
  )

  params <- suppressWarnings(
    calibrate_capacity_renege_params(
      referrals = refs,
      incompletes = incomp,
      completes = comp,
      max_months_waited = max_months,
      redistribute_m0_reneges = TRUE
    )
  ) |>
    dplyr::as_tibble()

  expected <- tibble::tribble(
    ~`months_waited_id`, ~`renege_param`, ~`capacity_param`,
    0L,  0.37640166060088,     0.5196652828326,
    1L, -2.06558994698596,    2.55465828915496,
    2L,  -4.2180808626746,     4.3484007188955,
    3L, -4.25999018821329,    4.38332515684441,
    4L, -4.32030318523386,    4.43358598769489
  )

  expect_equal(
    params,
    expected,
    info = "calbrate_capacity_renege_params is what is expected"
  )

})

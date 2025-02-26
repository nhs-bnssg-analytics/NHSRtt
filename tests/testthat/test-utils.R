test_that("month_attribution_lkp function tests", {
  week_ends <- seq(
    from = as.Date("2024-11-29"),
    to = as.Date("2024-12-08"),
    by = "days"
  )

  mnth_lkp <- month_attribution_lkp(
    week_end_dates = week_ends
  )

  expect_equal(
    week_ends,
    unique(
      mnth_lkp[["week_end"]]
    ),
    label = "dates are not the same between inputs and outputs"
  )

  expect_identical(
    names(mnth_lkp),
    c("week_end", "wait_start_month", "month_weight"),
    label = "names are expected"
  )

  expect_identical(
    dim(mnth_lkp),
    c(16L, 3L),
    label = "dimensions are expected"
  )
})


test_that("redistribute_incompletes_evenly works", {
  incompletes <- c(1, 2, 3, -1, 5, -4)
  expect_identical(
    sum(redistribute_incompletes_evenly(incompletes)),
    sum(incompletes),
    label = "total number of incompletes remains the same before and after the function is used"
  )

  expect_gte(
    redistribute_incompletes_evenly(incompletes) |>
      min(),
    0,
    label = "there are no negative values returned from the redistribute_incompletes_evenly function"
  )

})

test_that("redistribute_incompletes_optimally works", {
  inflow <- c(11, 20, 20, 12, 16)
  reneges <- c(6, 7, 7, 9, 9)
  treatments <- c(9, 9, 9, 9, 7)

  redistributed_incompletes <- calculate_incompletes(
    inflow = inflow,
    reneges = reneges,
    treatments = treatments,
    redistribution_method = "prioritise_long_waiters"
  )

  expect_identical(
    sum(redistributed_incompletes),
    sum(inflow - reneges - treatments),
    label = "total number of incompletes remains the same before and after the function is used"
  )

  inflow <- c(11, 20, 20, 12, 16)
  reneges <- c(6, 7, 7, 9, 9)
  treatments <- c(9, 9, 9, 2, 7)

  redistributed_incompletes <- calculate_incompletes(
    inflow = inflow,
    reneges = reneges,
    treatments = treatments,
    redistribution_method = "prioritise_long_waiters"
  )

  expect_identical(
    redistributed_incompletes,
    c(0, 4, 1, 0 , 0),
    label = "treatments are redistributed consistently"
  )

})


test_that("apply_parameter_skew works", {
  params_before <- c(0.05, 0.02, 0.02, 0.06, 0.08, 0.1)
  skew_factor <- 1.1

  params_after <- apply_parameter_skew(
    params = params_before,
    skew = skew_factor
  )

  expect_equal(
    length(params_after),
    length(params_before),
    info = "length of output identical to length of input"
  )

  expect_equal(
    params_after[1],
    params_before[1],
    info = "first parameter remains unchanged"
  )

  expect_equal(
    tail(params_after, 1),
    tail(params_before, 1) * skew_factor,
    info = "final value is equal to the final value of the input multiplied by the skew factor"
  )

  # testing uniform skew with a pivot_bin

  piv_bin <- 2
  params_after <- apply_parameter_skew(
    params = params_before,
    skew = skew_factor,
    skew_method = "uniform",
    pivot_bin = piv_bin
  )

  expect_equal(
    params_after[1:piv_bin],
    c(params_before[1], params_before[2:piv_bin] * (1 / skew_factor)),
    info = "outputs below the pivot bin are correct"
  )

  expect_equal(
    params_after[(piv_bin + 1):length(params_after)],
    params_before[(piv_bin + 1):length(params_after)] * skew_factor,
    info = "outputs above the pivot bin are correct"
  )

  # test non-integer pivot_bin
  piv_bin <- 4.5
  params_after <- apply_parameter_skew(
    params = params_before,
    skew = skew_factor,
    skew_method = "rotate",
    pivot_bin = piv_bin
  )

  expect_equal(
    params_after[1],
    params_before[1],
    info = "first compartment remains unchanged"
  )

  expect_true(
    all(params_after[2:floor(piv_bin + 1)] < params_before[2:floor(piv_bin + 1)]),
    info = "all param vals bar the first one that are below the pivot bin are less than the input params"
  )

  expect_true(
    tail(params_after, 1) > tail(params_before, 1),
    info = "final param value is greater than the final input param value"
  )
})

test_that("apply_parameter_skew errors", {

  expect_error(
    apply_parameter_skew(
      params = c("0.05", "0.02", "0.02", "0.06", "0.08", "0.1"),
      skew = 1.1
    ),
    "params must be numeric",
    info = "numeric input for params"
  )

  expect_error(
    apply_parameter_skew(
      params = c(0.05, 0.02, 0.02, 0.06, 0.08, 0.1),
      skew = "1.1"
    ),
    "skew must be numeric",
    info = "numeric input for skew"
  )

  expect_error(
    apply_parameter_skew(
      params = c(0.05, 0.02, 0.02, 0.06, 0.08, 0.1),
      skew = rep(1.1, 2)
    ),
    "skew must be length 1",
    info = "skew must be length 1"
  )

  expect_error(
    apply_parameter_skew(
      params = c(0.05, 0.02, 0.02, 0.06, 0.08, 0.1),
      skew = -0.1
    ),
    "skew must be greater than 0",
    info = "skew must be > 0"
  )

  expect_snapshot(
    apply_parameter_skew(
      params = c(0.05, 0.02, 0.02, 0.06, 0.08, 0.1),
      skew = 1.1,
      skew_method = "unknown"
    ),
    error = TRUE
  )
})

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

  expect_warning(
    redistribute_incompletes_evenly(-5:2),
    "surplus of treatments and reneges exceeds the number of people waiting for treatment"
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

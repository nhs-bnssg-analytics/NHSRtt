test_that("month_attribution_lkp function tests", {
  week_ends <- seq(
    from = as.Date("2024-11-29"),
    to = as.Date("2024-12-08"),
    by = "days"
  )

  mnth_lkp <- month_attribution_lkp(
    week_end_dates = week_ends
  )

  expect_identical(
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

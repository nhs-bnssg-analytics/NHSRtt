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


test_that("apply_params_to_projections errors", {

  params <- tibble::tribble(
    ~`months_waited_id`, ~`renege_param`, ~`capacity_param`,
    0L,  0.37640166060088,     0.5196652828326,
    1L, -2.06558994698596,    2.55465828915496,
    2L,  -4.2180808626746,     4.3484007188955,
    3L, -4.25999018821329,    4.38332515684441,
    4L, -4.32030318523386,    4.43358598769489
  )

  max_months <- 4

  future_capacity <- sample(300:500, 4, replace = TRUE)
  future_referrals <- sample(300:500, 4, replace = TRUE)
  incompletes_t0 <- dplyr::tibble(
    months_waited_id = c(0, seq_len(max_months)),
    incompletes = sample(
      100:200,
      length(c(0, seq_len(max_months))),
      replace = TRUE
    )
  )


  expect_error(
    apply_params_to_projections(
      capacity_projections = head(future_capacity, -1),
      referrals_projections = future_referrals,
      incomplete_pathways = incompletes_t0,
      renege_capacity_params = params,
      max_months_waited = max_months
    ),
    "capacity_projections and referrals_projections must be the same length",
    info = "different lengths for referral and capacity projections"
  )

  expect_error(
    apply_params_to_projections(
      capacity_projections = future_capacity,
      referrals_projections = future_referrals,
      incomplete_pathways = incompletes_t0,
      renege_capacity_params = params,
      max_months_waited = "four"
    ),
    "max_months_waited must be an integer",
    info = "max_months_waited isn't a number"
  )

  expect_error(
    apply_params_to_projections(
      capacity_projections = future_capacity,
      referrals_projections = future_referrals,
      incomplete_pathways = incompletes_t0,
      renege_capacity_params = params |> rename(regene_param = renege_param),
      max_months_waited = max_months
    ),
    "renege_capacity_params must have the column names: months_waited_id, renege_param and capacity_param",
    info = "renege_capacity_params has correct field names"
  )

  expect_error(
    apply_params_to_projections(
      capacity_projections = future_capacity,
      referrals_projections = future_referrals,
      incomplete_pathways = bind_rows(incompletes_t0, incompletes_t0),
      renege_capacity_params = params,
      max_months_waited = max_months
    ),
    "incomplete_pathways must have nrow less than or equal to max_months_waited \\+ 1",
    info = "correct number of records for incomplete_pathways"
  )

  expect_error(
    apply_params_to_projections(
      capacity_projections = future_capacity,
      referrals_projections = future_referrals,
      incomplete_pathways = incompletes_t0 |>  rename(incomplete = incompletes),
      renege_capacity_params = params,
      max_months_waited = max_months
    ),
    "incomplete_pathways must have field names of 'months_waited_id' and 'incompletes'",
    info = "correct field names for incomplete_pathways"
  )
})

test_that("apply_params_to_projections functionality", {
  projections <- apply_params_to_projections(
    capacity_projections = future_capacity,
    referrals_projections = future_referrals,
    incomplete_pathways = incompletes_t0,
    renege_capacity_params = params,
    max_months_waited = max_months
  )

  expected <- dplyr::tibble(
    period_id = c(1L,1L,1L,1L,1L,
                  2L,2L,2L,2L,2L,3L,3L,3L,
                  3L,3L,4L,4L,4L,4L,4L),
    months_waited_id = c(1L,2L,3L,4L,0L,
                         2L,3L,4L,1L,0L,3L,4L,2L,
                         1L,0L,4L,3L,2L,1L,0L),
    calculated_treatments = c(49.1156035218472,
                              51.3493996934086,61.173054609372,
                              143.219967630658,17.1419745447141,
                              75.1560008731345,
                              79.2051968693715,318.886269703902,
                              15.4102002414251,5.34233231216657,
                              65.8801161090067,352.959608625251,
                              13.4006185771742,2.7292965202228,
                              1.03036016834504,346.200855181469,
                              10.9509972266818,2.21261277387852,
                              0.49073497725839,0.144799840712464),
    reneges = c(-406.921219556234,
                -510.387784383627,-609.1785969145,
                -1430.02035431241,
                127.223761283097,-2340.21495149904,
                -2470.95782756741,-9974.74160908883,
                -399.968989466096,124.21254799829,
                -12012.5953860632,-64530.0023776222,
                -2438.86505346469,
                -414.037424153118,140.021417743527,
                -405590.386574545,-12795.5514288448,
                -2580.42466520982,-477.044325819517,
                126.094556301295),
    incompletes = c(554.805616034387,
                    580.038384690218,691.005542305128,
                    1617.80038668175,
                    193.634264172189,2819.86456666029,
                    2971.79101538826,11964.6612683718,
                    578.19305339686,200.445119689543,
                    14766.5798366144,79113.495052757,
                    3003.65748828437,611.753247322438,
                    230.948222088128,499124.260608735,
                    15788.2579199025,3189.96529975838,
                    707.501812930386,208.760643857993),
    input_treatments = c(322L,322L,322L,
                         322L,322L,494L,494L,494L,494L,
                         494L,436L,436L,436L,436L,
                         436L,360L,360L,360L,360L,360L)
  )

  expect_equal(
    projections,
    expected,
    info = "apply_params_to_projections is consistently working"
  )
})

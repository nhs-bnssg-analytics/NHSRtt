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
      completes = comp |> mutate(treatments = treatments * 10),
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

  expected <- dplyr::tibble(
     months_waited_id = c(0L, 1L, 2L, 3L, 4L),
     renege_param = c(0.373061815, 0.463521759, 0.086835849, 0.079501717, 0.068946943),
     capacity_param = c(0.029854199, 0.025546583, 0.043484007, 0.043833252, 0.04433586)
   )

  expect_equal(
    params,
    expected,
    info = "calbrate_capacity_renege_params is what is expected"
  )

})


test_that("apply_params_to_projections errors", {

  params <- dplyr::tibble(
    months_waited_id = 0:4,
    renege_param = c(0.37640166060088, -2.06558994698596, -4.2180808626746,
                     -4.25999018821329, -4.32030318523386),
    capacity_param = c(0.5196652828326, 2.55465828915496, 4.3484007188955,
                       4.38332515684441, 4.43358598769489)
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

  expect_error(
    apply_params_to_projections(
      capacity_projections = future_capacity * -1,
      referrals_projections = future_referrals,
      incomplete_pathways = incompletes_t0,
      renege_capacity_params = params,
      max_months_waited = max_months
    ),
    "capacity_projections must all be greater or equal to zero",
    info = "negative capacity"
  )

  expect_error(
    apply_params_to_projections(
      capacity_projections = future_capacity,
      referrals_projections = future_referrals * -1,
      incomplete_pathways = incompletes_t0,
      renege_capacity_params = params,
      max_months_waited = max_months
    ),
    "referrals_projections must all be greater or equal to zero",
    info = "negative referrals"
  )

})

test_that("apply_params_to_projections functionality", {
  set.seed(1234)
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

  params <- dplyr::tibble(
    months_waited_id = 0:4,
    renege_param = c(0.37640166060088, -2.06558994698596, -4.2180808626746,
                     -4.25999018821329, -4.32030318523386),
    capacity_param = c(0.5196652828326, 2.55465828915496, 4.3484007188955,
                       4.38332515684441, 4.43358598769489)
  )

  projections <- apply_params_to_projections(
    capacity_projections = future_capacity,
    referrals_projections = future_referrals,
    incomplete_pathways = incompletes_t0,
    renege_capacity_params = params,
    max_months_waited = max_months
  )

  expected <- dplyr::tibble(
    period_id = c(
      1L,1L,1L,1L,1L,2L,2L,2L,
      2L,2L,3L,3L,3L,3L,3L,4L,4L,
      4L,4L,4L
    ),
    months_waited_id = c(
      0L,1L,2L,3L,4L,0L,1L,2L,
      3L,4L,0L,1L,2L,3L,4L,0L,1L,
      2L,3L,4L
    ),
    calculated_treatments = c(
      20.04122796,27.63423362,
      42.12923285,81.22443171,155.9708739,
      5.118400036,13.59862424,31.91646707,
      49.0484251,279.3180835,1.156504437,
      3.510835114,15.87698091,37.56314012,
      390.8925394,0.2095794,0.594326398,
      3.071035894,13.99962849,382.1254298
    ),
    reneges = c(
      154.3246808,-237.5428439,
      -434.4623289,-839.2180671,-1615.793391,
      164.111124,-486.7234099,-1370.490791,
      -2110.114129,-12048.53131,
      162.6055174,-551.0384133,-2989.602255,
      -7086.420979,-73939.51857,175.0267722,
      -554.0696711,-3434.775038,-15687.34635,
      -429331.8355
    ),
    incompletes = c(
      235.6340912,324.9086103,
      495.333096,954.9936354,1833.822517,
      266.7704759,708.7588769,1663.482934,
      2556.3988,14558.02938,268.2379782,
      814.2980541,3682.484151,8712.340773,
      90663.0542,289.7636484,821.7133229,
      4246.002057,19355.83087,528325.105
    ),
    input_treatments = c(
      327L,327L,327L,327L,327L,
      379L,379L,379L,379L,379L,449L,
      449L,449L,449L,449L,400L,400L,400L,
      400L,400L
    )
  )

  expect_equal(
    projections,
    expected,
    info = "apply_params_to_projections is consistently working"
  )

  params <- dplyr::tibble(
    months_waited_id = 0:4,
    renege_param = c(0.37640166060088, -2.06558994698596, -4.2180808626746,
                     -4.25999018821329, -4.32030318523386),
    capacity_param = rep(0, 5)
  )

  projections <- apply_params_to_projections(
    capacity_projections = future_capacity,
    referrals_projections = future_referrals,
    incomplete_pathways = incompletes_t0,
    renege_capacity_params = params,
    max_months_waited = max_months
  )

  expect_true(
    all(colSums(is.na(projections)) == 0),
    info = "apply_params_to_projections functions when all capacity parameters are 0"
  )

  params <- dplyr::tibble(
    months_waited_id = 0:4,
    renege_param = rep(0, 5),
    capacity_param = c(0.5196652828326, 2.55465828915496, 4.3484007188955,
                       4.38332515684441, 4.43358598769489)
  )

  projections <- apply_params_to_projections(
    capacity_projections = future_capacity,
    referrals_projections = future_referrals,
    incomplete_pathways = incompletes_t0,
    renege_capacity_params = params,
    max_months_waited = max_months
  )

  expect_true(
    all(colSums(is.na(projections)) == 0),
    info = "apply_params_to_projections functions when all renege parameters are 0"
  )

  # this scenario needs looking at: eg, where no incompletes are passed to the
  # function

  # projections_no_incompletes <- apply_params_to_projections(
  #   capacity_projections = future_capacity,
  #   referrals_projections = future_referrals,
  #   renege_capacity_params = params,
  #   max_months_waited = max_months
  # )

})

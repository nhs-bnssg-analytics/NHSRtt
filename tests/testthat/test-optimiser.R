test_that("optimise_capacity errors", {
  params <- dplyr::tibble(
    months_waited_id = c(0L, 1L, 2L, 3L, 4L),
    renege_param = c(
      0.373061815,
      0.463521759,
      0.086835849,
      0.079501717,
      0.068946943
    ),
    capacity_param = c(
      0.029854199,
      0.025546583,
      0.043484007,
      0.043833252,
      0.04433586
    )
  )

  max_months <- 4

  future_capacity <- create_dummy_data(
    type = "complete",
    max_months_waited = max_months,
    number_periods = 1,
  ) |>
    dplyr::pull(.data$treatments) |>
    sum()

  future_referrals <- create_dummy_data(
    type = "referral",
    max_months_waited = max_months,
    number_periods = 24,
  ) |>
    pull(.data$referrals)

  incompletes_t0 <- create_dummy_data(
    type = "incomplete",
    max_months_waited = max_months,
    number_periods = 1,
  ) |>
    dplyr::summarise(
      incompletes = mean(.data$incompletes),
      .by = "months_waited_id"
    )

  expect_error(
    optimise_capacity(
      t_1_capacity = rep(future_capacity, 2),
      referrals_projections = future_referrals,
      incomplete_pathways = incompletes_t0,
      renege_capacity_params = params,
      target_bin = max_months,
      target = "~-2%",
      tolerance = 0.005
    ),
    "t_1_capacity must be length 1",
    info = "inputs for t_1_capacity are length 1"
  )

  expect_error(
    optimise_capacity(
      t_1_capacity = "green",
      referrals_projections = future_referrals,
      incomplete_pathways = incompletes_t0,
      renege_capacity_params = params,
      target_bin = max_months,
      target = "~-2%",
      tolerance = 0.005
    ),
    "t_1_capacity must be numeric",
    info = "t_1_capacity is numeric"
  )

  expect_error(
    optimise_capacity(
      t_1_capacity = future_capacity,
      referrals_projections = rep("junk", 12),
      incomplete_pathways = incompletes_t0,
      renege_capacity_params = params,
      target_bin = max_months,
      target = "~-2%",
      tolerance = 0.005
    ),
    "referrals must be a numeric vector",
    info = "referrals_projections is numeric"
  )

  expect_error(
    optimise_capacity(
      t_1_capacity = future_capacity,
      referrals_projections = future_referrals,
      incomplete_pathways = incompletes_t0,
      renege_capacity_params = params,
      target_bin = 3.2,
      target = "~-2%",
      tolerance = 0.005
    ),
    "target_bin must be within the incompletes_pathways data set",
    info = "target_bin must be within the incompletes_pathways data set"
  )

  expect_error(
    optimise_capacity(
      t_1_capacity = future_capacity,
      referrals_projections = future_referrals,
      incomplete_pathways = incompletes_t0,
      renege_capacity_params = params |>
        dplyr::rename(renege_params = renege_param),
      target_bin = max_months,
      target = "~-2%",
      tolerance = 0.005
    ),
    "renege_capacity_params must have the column names: months_waited_id, renege_param and capacity_param",
    info = "correct field names for renege_capacity_params"
  )

  expect_error(
    optimise_capacity(
      t_1_capacity = future_capacity,
      referrals_projections = future_referrals,
      incomplete_pathways = incompletes_t0,
      renege_capacity_params = params,
      target_bin = max_months + 5,
      target = "~-2%",
      tolerance = 0.005
    ),
    "target_bin must be within the incompletes_pathways data set",
    info = "target_bin is higher than what is in the data"
  )

  expect_error(
    optimise_capacity(
      t_1_capacity = future_capacity,
      referrals_projections = future_referrals,
      incomplete_pathways = incompletes_t0 |>
        dplyr::rename(junk = "incompletes"),
      renege_capacity_params = params,
      target_bin = max_months,
      target = "~-2%",
      tolerance = 0.005
    ),
    "incomplete_pathways must have field names of 'months_waited_id' and 'incompletes'",
    info = "correct field names for incomplete_pathways"
  )

  expect_error(
    optimise_capacity(
      t_1_capacity = future_capacity,
      referrals_projections = future_referrals,
      incomplete_pathways = incompletes_t0,
      renege_capacity_params = params,
      target_bin = max_months,
      target = "~2",
      tolerance = 0.005
    ),
    "target must have a percentage",
    info = "no percentage symbol in the target"
  )

  expect_error(
    optimise_capacity(
      t_1_capacity = future_capacity,
      referrals_projections = future_referrals,
      incomplete_pathways = incompletes_t0,
      renege_capacity_params = params,
      target_bin = max_months,
      target = "~two%",
      tolerance = 0.005
    ),
    "unable to parse the number from target",
    info = "no number in the target"
  )

  expect_error(
    optimise_capacity(
      t_1_capacity = future_capacity,
      referrals_projections = future_referrals,
      incomplete_pathways = incompletes_t0,
      renege_capacity_params = params,
      target_bin = max_months,
      target = "-2%",
      tolerance = 0.005
    ),
    "absolute target must be between 0% and 100%",
    info = "absolute percentage target must be between 0 and 100"
  )

  expect_error(
    optimise_capacity(
      t_1_capacity = future_capacity,
      referrals_projections = future_referrals,
      incomplete_pathways = incompletes_t0,
      renege_capacity_params = params,
      target_bin = max_months,
      target = "~-2%",
      tolerance = "junk"
    ),
    "tolerance must be numeric",
    info = "tolerance must be numeric"
  )

  expect_error(
    optimise_capacity(
      t_1_capacity = future_capacity,
      referrals_projections = future_referrals,
      incomplete_pathways = incompletes_t0,
      renege_capacity_params = params,
      target_bin = max_months,
      target = "~-2%",
      tolerance = rep(0.005, 2)
    ),
    "tolerance must be length 1",
    info = "tolerance must be length 1"
  )

  expect_snapshot(
    optimise_capacity(
      t_1_capacity = future_capacity,
      referrals_projections = future_referrals,
      incomplete_pathways = incompletes_t0,
      renege_capacity_params = params,
      target_bin = max_months,
      target = "~-2%",
      capacity_profile = "flart",
      tolerance = 0.005
    ),
    error = TRUE
  )
})

test_that("optimise_capacity functionality", {
  params <- dplyr::tibble(
    months_waited_id = c(0L, 1L, 2L, 3L, 4L),
    renege_param = c(
      0.373061815,
      0.463521759,
      0.086835849,
      0.079501717,
      0.068946943
    ),
    capacity_param = c(
      0.029854199,
      0.025546583,
      0.043484007,
      0.043833252,
      0.04433586
    )
  )

  max_months <- 4

  future_capacity <- create_dummy_data(
    type = "complete",
    max_months_waited = max_months,
    number_periods = 1,
  ) |>
    dplyr::pull(.data$treatments) |>
    sum()

  future_referrals <- create_dummy_data(
    type = "referral",
    max_months_waited = max_months,
    number_periods = 24,
  ) |>
    dplyr::pull(.data$referrals)

  incompletes_t0 <- create_dummy_data(
    type = "incomplete",
    max_months_waited = max_months,
    number_periods = 1,
  ) |>
    dplyr::summarise(
      incompletes = mean(.data$incompletes),
      .by = "months_waited_id"
    )

  expect_warning(
    optimise_capacity(
      t_1_capacity = future_capacity,
      referrals_projections = future_referrals,
      incomplete_pathways = incompletes_t0,
      renege_capacity_params = params,
      target_bin = max_months,
      target = "~-2%",
      tolerance = 0.005,
      max_iterations = 2
    ),
    "optimiser failed to converge before maximum iteration reached",
    info = "optimise_capacity fails to converge within given constraints"
  )

  expect_warning(
    optimise_capacity(
      t_1_capacity = future_capacity,
      referrals_projections = future_referrals,
      incomplete_pathways = incompletes_t0,
      renege_capacity_params = params |>
        mutate(capacity_param = 0),
      target_bin = max_months,
      target = "~-2%",
      tolerance = 0.005
    ),
    "Unable to optimise as no treatments in the calibration period",
    info = "No treatments in calibration period prevents optimisation ability"
  )
})


test_that("optimise_capacity functionality", {
  params <- dplyr::tibble(
    months_waited_id = c(0L, 1L, 2L, 3L, 4L),
    renege_param = c(
      0.373061815,
      0.463521759,
      0.086835849,
      0.079501717,
      0.068946943
    ),
    capacity_param = c(
      0.029854199,
      0.025546583,
      0.043484007,
      0.043833252,
      0.04433586
    )
  )

  max_months <- 4

  future_capacity <- create_dummy_data(
    type = "complete",
    max_months_waited = max_months,
    number_periods = 1,
  ) |>
    dplyr::pull(.data$treatments) |>
    sum()

  future_referrals <- create_dummy_data(
    type = "referral",
    max_months_waited = max_months,
    number_periods = 24,
  ) |>
    dplyr::pull(.data$referrals)

  incompletes_t0 <- create_dummy_data(
    type = "incomplete",
    max_months_waited = max_months,
    number_periods = 1,
  ) |>
    dplyr::summarise(
      incompletes = mean(.data$incompletes),
      .by = "months_waited_id"
    )

  expect_equal(
    optimise_capacity(
      t_1_capacity = future_capacity,
      referrals_projections = future_referrals,
      incomplete_pathways = incompletes_t0,
      renege_capacity_params = params,
      target_bin = max_months,
      target = "~-2%",
      tolerance = 0.005
    ),
    c(converged = 0.71875),
    info = "optimise_capacity consistently produces an answer with linear_change profile"
  )

  expect_equal(
    optimise_capacity(
      t_1_capacity = future_capacity,
      referrals_projections = future_referrals,
      incomplete_pathways = incompletes_t0,
      renege_capacity_params = params,
      target_bin = max_months,
      target = "~-2%",
      capacity_profile = "flat",
      tolerance = 0.005
    ),
    c(converged = 0.6875),
    info = "optimise_capacity consistently produces an answer with flat profile"
  )
})

test_that("optimise_capacity functionality with incomplete_adjustment_factor", {
  set.seed(777)
  max_months <- 4

  future_capacity <- create_dummy_data(
    type = "complete",
    max_months_waited = max_months,
    number_periods = 1,
  ) |>
    dplyr::pull(.data$treatments) |>
    sum()

  future_referrals <- create_dummy_data(
    type = "referral",
    max_months_waited = max_months,
    number_periods = 24,
  ) |>
    dplyr::pull(.data$referrals)

  incompletes_t0 <- create_dummy_data(
    type = "incomplete",
    max_months_waited = max_months,
    number_periods = 1,
  ) |>
    dplyr::summarise(
      incompletes = mean(.data$incompletes),
      .by = "months_waited_id"
    )

  refs <- create_dummy_data(
    type = "referral",
    max_months_waited = max_months,
    number_periods = 6
  )

  incomp_multiplier <- dplyr::tibble(
    period_id = 0:6,
    multiplier = seq(1, 3, length.out = 7)
  )

  incomp <- create_dummy_data(
    type = "incomplete",
    max_months_waited = max_months,
    number_periods = 6
  ) |>
    left_join(
      incomp_multiplier,
      by = "period_id"
    ) |>
    mutate(incompletes = incompletes * multiplier) |>
    select(!c("multiplier"))

  comp <- create_dummy_data(
    type = "complete",
    max_months_waited = max_months,
    number_periods = 6
  )

  params <- calibrate_capacity_renege_params(
    referrals = refs,
    incompletes = incomp,
    completes = comp,
    max_months_waited = max_months,
    redistribute_m0_reneges = FALSE,
    allow_negative_params = FALSE
  )

  incomp_adjustment_factor <- calibrate_capacity_renege_params(
    referrals = refs,
    incompletes = incomp,
    completes = comp,
    max_months_waited = max_months,
    redistribute_m0_reneges = FALSE,
    allow_negative_params = FALSE,
    full_breakdown = TRUE
  ) |>
    select(
      "period_id",
      "months_waited_id",
      "adjusted_incompletes" = "waiting_same_node"
    ) |>
    dplyr::left_join(
      incomp,
      by = c("period_id", "months_waited_id")
    ) |>
    mutate(
      monthly_uplift = (.data$adjusted_incompletes / .data$incompletes) - 1
    ) |>
    summarise(
      adjustment_factor = mean(.data$monthly_uplift, na.rm = TRUE),
      .by = "months_waited_id"
    )

  # CHECKS
  # calculate capacity uplift

  target_val <- 0.6
  tol <- 0.005
  cap_uplift <- optimise_capacity(
    t_1_capacity = future_capacity,
    referrals_projections = future_referrals,
    incomplete_pathways = incompletes_t0,
    incomplete_adjustment_factor = incomp_adjustment_factor,
    renege_capacity_params = params,
    target_bin = max_months,
    target = paste0(target_val * 100, "%"),
    tolerance = tol
  )

  # calc monthly capacity increase
  monthly_capacity_increase <- unname(
    (future_capacity * cap_uplift) - future_capacity
  ) /
    12
  # calculate the future capacity profile
  future_cap_profile <- rep(future_capacity, times = length(future_referrals)) +
    (monthly_capacity_increase * (seq_along(future_referrals) - 1))

  # calculate the final performance
  perf_end <- apply_params_to_projections(
    capacity_projections = future_cap_profile,
    referrals_projections = future_referrals,
    incomplete_pathways = incompletes_t0,
    renege_capacity_params = params,
    max_months_waited = max_months
  ) |>
    filter(period_id == max(period_id)) |>
    select("period_id", "months_waited_id", "incompletes") |>
    left_join(
      incomp_adjustment_factor,
      by = "months_waited_id"
    ) |>
    mutate(
      incompletes = incompletes / (1 + adjustment_factor)
    ) |>
    summarise(
      across(
        incompletes,
        \(x) sum(x[max_months + 1]) / sum(x)
      )
    ) |>
    pull(incompletes)

  expect_equal(
    perf_end,
    target_val,
    tolerance = tol / target_val,
    info = "target value is achieved"
  )
})

test_that("optimise_capacity errors", {

  params <- dplyr::tibble(
    months_waited_id = c(0L, 1L, 2L, 3L, 4L),
    renege_param = c(0.373061815, 0.463521759, 0.086835849, 0.079501717, 0.068946943),
    capacity_param = c(0.029854199, 0.025546583, 0.043484007, 0.043833252, 0.04433586)
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
      renege_capacity_params = params |> rename(renege_params = renege_param),
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
      incomplete_pathways = incompletes_t0 |> rename(junk = "incompletes"),
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
})

test_that("optimise_capacity functionality", {

  params <- dplyr::tibble(
    months_waited_id = c(0L, 1L, 2L, 3L, 4L),
    renege_param = c(0.373061815, 0.463521759, 0.086835849, 0.079501717, 0.068946943),
    capacity_param = c(0.029854199, 0.025546583, 0.043484007, 0.043833252, 0.04433586)
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


})


test_that("optimise_capacity functionality", {

  params <- dplyr::tibble(
    months_waited_id = c(0L, 1L, 2L, 3L, 4L),
    renege_param = c(0.373061815, 0.463521759, 0.086835849, 0.079501717, 0.068946943),
    capacity_param = c(0.029854199, 0.025546583, 0.043484007, 0.043833252, 0.04433586)
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
    0.71875,
    info = "optimise_capacity consistently produces an answer"
  )
})

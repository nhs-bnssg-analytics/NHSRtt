test_that("returns correct length", {
  expect_equal(length(geom_fn_s(p1 = 0.3, max_num_months = 24)), 24)
  expect_equal(length(geom_fn_s(p1 = 0.3, max_num_months = 1)), 1)
  expect_equal(length(geom_fn_s(p1 = 0.3, max_num_months = 5)), 5)
})

test_that("throws error when max_num_months out of bounds", {
  expect_error(
    geom_fn_s(p1 = 0.3, max_num_months = 25),
    "max_num_months must be between 1 and 24"
  )
  expect_error(
    geom_fn_s(p1 = 0.3, max_num_months = 0),
    "max_num_months must be between 1 and 24"
  )
})

test_that("returns numeric vector", {
  result <- geom_fn_s(p1 = 0.2, max_num_months = 10)
  expect_type(result, "double")
})

test_that("survival probabilities sum to ~1", {
  result <- geom_fn_s(p1 = 0.2, max_num_months = 24)
  expect_equal(sum(result), 1, tolerance = 1e-8)
})

test_that("values are non-negative and <= 1", {
  result <- geom_fn_s(p1 = 0.5, max_num_months = 24)
  expect_true(all(result >= 0 & result <= 1))
})

#### initialise_removals
test_that("returns a data frame with expected structure", {
  renege_params <- rep(0.1, 12)
  result <- initialise_removals(renege_params, p1 = 0.3, mu = 100)

  expect_s3_class(result, "data.frame")
  expect_equal(names(result), c("months_waited_id", "r", "service"))
  expect_equal(nrow(result), length(renege_params))
})

test_that("months_waited_id starts at 0 and increments by 1", {
  renege_params <- rep(0.1, 6)
  result <- initialise_removals(renege_params, p1 = 0.4, mu = 80)
  expect_equal(result$months_waited_id, 0:5)
})

test_that("renege rate values are passed through correctly", {
  renege_params <- seq(0.05, 0.3, length.out = 5)
  result <- initialise_removals(renege_params, p1 = 0.2, mu = 50)
  expect_equal(result$r, renege_params)
})

test_that("service values sum approximately to mu", {
  renege_params <- rep(0.1, 24)
  mu <- 150
  result <- initialise_removals(renege_params, p1 = 0.5, mu = mu)
  expect_equal(sum(result$service), mu, tolerance = 1)
})

test_that("handles custom lengths of renege_params", {
  renege_params <- rep(0.1, 18)
  result <- initialise_removals(renege_params, p1 = 0.6, mu = 90)
  expect_equal(nrow(result), 18)
})

# calc_wl_sizes
test_that("Basic functionality works correctly", {
  wl <- data.frame(r = c(0.1, 0.2), service = c(5, 3))
  result <- calc_wl_sizes(wl, referrals = 50)

  expect_equal(result$wlsize, c(40, 29))
  expect_equal(result$sigma, c(5, 3))
})

test_that("Handles zero referrals correctly", {
  wl <- data.frame(r = c(0.1, 0.2), service = c(5, 3))
  result <- calc_wl_sizes(wl, referrals = 0)

  expect_equal(result$wlsize, c(0, 0))
  expect_equal(result$sigma, c(0, 0))
})

test_that("Handles zero service correctly", {
  wl <- data.frame(r = c(0.1, 0.2), service = c(0, 0))
  result <- calc_wl_sizes(wl, referrals = 50)

  expect_equal(result$wlsize, c(45, 36))
  expect_equal(result$sigma, c(0, 0))
})

test_that("Handles full removal (r = 1)", {
  wl <- data.frame(r = c(1, 1), service = c(5, 3))
  result <- calc_wl_sizes(wl, referrals = 50)

  expect_equal(result$wlsize, c(0, 0))
  expect_equal(result$sigma, c(0, 0))
})

test_that("Handles no removal (r = 0)", {
  wl <- data.frame(r = c(0, 0), service = c(5, 3))
  result <- calc_wl_sizes(wl, referrals = 50)

  expect_equal(result$wlsize, c(45, 42))
  expect_equal(result$sigma, c(5, 3))
})

test_that("Handles negative resulting wlsize", {
  wl <- data.frame(r = c(0.1, 0.2), service = c(100, 100))
  result <- calc_wl_sizes(wl, referrals = 50)

  expect_equal(result$wlsize, c(0, 0))
  expect_equal(result$sigma, c(45, 0)) # 50 * (1 - 0.1) = 45; second step starts from 0
})

test_that("Handles single row input", {
  wl <- data.frame(r = 0.1, service = 5)
  result <- calc_wl_sizes(wl, referrals = 50)

  expect_equal(result$wlsize, 40)
  expect_equal(result$sigma, 5)
})

test_that("Returns correct structure", {
  wl <- data.frame(r = c(0.1, 0.2), service = c(5, 3))
  result <- calc_wl_sizes(wl, referrals = 50)

  expect_true(all(c("r", "service", "sigma", "wlsize") %in% names(result)))
  expect_equal(nrow(result), 2)
})

# hist_percentile_calc
test_that("Basic percentile calculation works", {
  df <- data.frame(months_waited_id = 0:4, wlsize = c(10, 20, 30, 20, 20))
  result <- hist_percentile_calc(df, percentile = 0.5)
  expect_true(is.numeric(result))
  expect_gt(result, 0)
  expect_lt(result, max(df$months_waited_id))
})

test_that("Percentile at 0 returns earliest time", {
  df <- data.frame(months_waited_id = 0:4, wlsize = c(10, 20, 30, 20, 20))
  result <- hist_percentile_calc(df, percentile = 0)
  expect_equal(result, 0)
})

test_that("Percentile at 1 returns latest time", {
  df <- data.frame(months_waited_id = 0:4, wlsize = c(10, 20, 30, 20, 20))
  result <- hist_percentile_calc(df, percentile = 1)
  expect_equal(result, max(df$months_waited_id) + 1)
})

test_that("Handles zero weights correctly", {
  df <- data.frame(months_waited_id = 0:4, wlsize = c(0, 0, 0, 0, 0))
  result <- hist_percentile_calc(df, percentile = 0.5)
  expect_equal(result, 0)
})

test_that("Handles custom column names", {
  df <- data.frame(time = 0:4, weight = c(10, 20, 30, 20, 20))
  result <- hist_percentile_calc(
    df,
    percentile = 0.5,
    wlsize_col = "weight",
    time_col = "time"
  )
  expect_true(is.numeric(result))
})

test_that("Interpolation is correct within a bin", {
  df <- data.frame(months_waited_id = 0:2, wlsize = c(10, 10, 80))
  result <- hist_percentile_calc(df, percentile = 0.5)
  # 50% of 100 = 50; first two bins = 20; third bin starts at 20, ends at 100
  # Needed in bin = 50 - 20 = 30; bin size = 80; proportion = 30/80 = 0.375
  # Interpolated time = 2 + 0.375 * (3 - 2) = 2.375
  expect_equal(result, 2.375)
})

test_that("Returns numeric scalar", {
  df <- data.frame(months_waited_id = 0:9, wlsize = rep(1, 10))
  result <- hist_percentile_calc(df, percentile = 0.5)
  expect_length(result, 1)
  expect_type(result, "double")
})

test_that("Handles NA weights gracefully", {
  df <- data.frame(months_waited_id = 0:4, wlsize = c(10, NA, 30, 20, 20))
  result <- hist_percentile_calc(df, percentile = 0.5)
  expect_true(is.numeric(result))
  expect_equal(result, 3)
})

test_that("Error if wlsize_col is missing", {
  df <- data.frame(months_waited_id = 0:4, wrong_col = c(10, 20, 30, 40, 50))
  expect_error(
    hist_percentile_calc(df, wlsize_col = "wlsize"),
    "wlsize_col not in column names of wl_structure"
  )
})

test_that("Error if time_col is missing", {
  df <- data.frame(wlsize = c(10, 20, 30, 40, 50), wrong_time = 0:4)
  expect_error(
    hist_percentile_calc(df, time_col = "months_waited_id"),
    "time_col not in column names of wl_structure"
  )
})

test_that("Error if time_col does not start at 0", {
  df <- data.frame(months_waited_id = 1:5, wlsize = c(10, 20, 30, 40, 50))
  expect_error(
    hist_percentile_calc(df),
    "time_col must start from 0, and be the lower bound of the unit of time waiting"
  )
})

test_that("No error if required columns exist and time starts at 0", {
  df <- data.frame(months_waited_id = 0:4, wlsize = c(10, 20, 30, 40, 50))
  expect_silent(hist_percentile_calc(df))
})

# find_p
test_that("Returns valid result for typical input", {
  result <- find_p(
    target_time = 18 / 4.35,
    renege_params = c(0.04, 0.04, 0.03, 0.01, 0.02, 0.02, 0.01),
    mu_1 = 2651.227,
    referrals = 12000,
    tolerance = 0.001,
    max_iterations = 20
  )

  expect_type(result, "list")
  expect_true(!is.na(result$p1))
  expect_true(!is.na(result$time_p))
  expect_true(result$niterations <= 20)
  expect_true(is.data.frame(result$waiting_list))
  expect_type(result$mu, "double")
  expect_type(result$wlsize, "double")
})

test_that("Returns 'Not converged' if solution not found within max_iterations", {
  result <- find_p(
    target_time = 1e6, # unrealistic target
    renege_params = rep(0.01, 7),
    mu_1 = 1000,
    referrals = 5000,
    tolerance = 0.0001,
    max_iterations = 3
  )

  expect_equal(
    result$status,
    "Not converged"
  )
  expect_equal(result$niterations, 3)
})


test_that("Returns expected structure", {
  result <- find_p(
    target_time = 18 / 4.35,
    renege_params = rep(0.02, 7),
    mu_1 = 2000,
    referrals = 10000
  )

  expect_named(
    result,
    c("p1", "time_p", "mu", "wlsize", "waiting_list", "niterations", "status")
  )
})

test_that("Error if p1_lower > p1_upper", {
  expect_error(
    find_p(
      target_time = 18 / 4.35,
      renege_params = rep(0.02, 7),
      mu_1 = 2000,
      referrals = 10000,
      p1_lower = 0.8,
      p1_upper = 0.2
    ),
    "p1_lower must be less than p1_upper"
  )
})

test_that("Error if p1_lower is outside [0, 1]", {
  expect_error(
    find_p(
      target_time = 18 / 4.35,
      renege_params = rep(0.02, 7),
      mu_1 = 2000,
      referrals = 10000,
      p1_lower = -0.1,
      p1_upper = 0.5
    ),
    "p1_lower and p1_upper need to be between 0 and 1"
  )
})

test_that("Error if p1_upper is outside [0, 1]", {
  expect_error(
    find_p(
      target_time = 18 / 4.35,
      renege_params = rep(0.02, 7),
      mu_1 = 2000,
      referrals = 10000,
      p1_lower = 0.2,
      p1_upper = 1.5
    ),
    "p1_lower and p1_upper need to be between 0 and 1"
  )
})

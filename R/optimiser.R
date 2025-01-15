#' Optimise the capacity profile for projections
#'
#' @param t_1_capacity numeric; a single value for the capacity for the first
#'   time period
#' @param referrals_projections numeric; a vector for the number of referrals
#'   for each period in the projected time period
#' @param target string length 1; can be either a percentage point change, eg,
#'   "~-5%" or a percent value, eg, "5%"
#' @param target_bin numeric length 1; the bin that the target refers to. It
#'   must be less than or equal to the max_months_waited value
#' @param tolerance numeric length 1; the tolerance used to compare the absolute
#'   error with in the max_months_waited bin to determine convergence. The
#'   absolute error is calculated on the proportion in the max_months_waited bin
#'   relative to the total waiting (even if a non-percentage target is used)
#' @inheritParams apply_params_to_projections
#'
#' @returns a capacity multiplier representing the annual change in capacity
#'   (from the input t_1_capacity to a capacity at t = 13) to achieve the
#'   desired target within the target tolerance
#' @export
#'
optimise_capacity <- function(t_1_capacity, referrals_projections,
                              incomplete_pathways, renege_capacity_params,
                              target, target_bin,
                              tolerance) {

  # checks

  # check lengths of inputs
  if (length(t_1_capacity) != 1)
    stop("t_1_capacity must be length 1")

  # check numeric inputs for t_1_capacity
  if (!is.numeric(t_1_capacity))
    stop("t_1_capacity must be an numeric")

  if (!is.numeric(referrals_projections))
    stop("referrals must be a numeric vector")

  # check numeric inputs for max_months_waited
  if (!is.numeric(max_months_waited))
    stop("max_months_waited must be an integer")

  # check field names
  if (length(setdiff(names(renege_capacity_params), c("months_waited_id", "renege_param", "capacity_param"))) > 0)
    stop("renege_capacity_params must have the column names: months_waited_id, renege_param and capacity_param")

  # check the number of rows are less than or equal to the number of months
  # waited of interest
  if (nrow(incomplete_pathways) > max_months_waited + 1) {
    stop("incomplete_pathways must have nrow less than or equal to max_months_waited + 1")
  }

  # check the column headers
  if (length(dplyr::setdiff(names(incomplete_pathways), c("months_waited_id", "incompletes"))) > 0) {
    stop("incomplete_pathways must have field names of 'months_waited_id' and 'incompletes'")
  }

  # checks on target
  if (!grepl("%", target))
    stop("target must have a percentage")

  if (!is.numeric(parse_number(target)))
    stop("unable to parse the number from target")

  if (length(parse_number(target)) > 1)
    stop("multiple numbers parsed from target")

  if (!between(parse_number(target), 0, 100) &
      !grepl("~", target))
      stop("absolute target must be between 0% and 100%")

  # checks on tolerance
  if (!is.numeric(tolerance))
    stop("tolerance must be numeric")

  if (length(tolerance) > 1)
    stop("tolerance must be length 1")

  # target calculation
  current_val <- incomplete_pathways |>
    mutate(
      months_waited_id = case_when(
        months_waited_id >= target_bin ~ target_bin,
        .default = months_waited_id
      )
    ) |>
    summarise(
      incompletes = sum(incompletes),
      .by = months_waited_id
    ) |>
    mutate(
      prop = .data$incompletes / sum(.data$incompletes)
    ) |>
    filter(
      .data$months_waited_id == target_bin
    ) |>
    pull(.data$prop)

  target_val <- parse_number(target) / 100

  if (grepl("~", target)) {
    # if tilde in target string, then make the target a relative number
    target_val <- current_val + target_val
  }

  change_proportion <- 1
  converged <- FALSE

  # adjustment is the amount to adjust the change_proportion when attempting to
  # converge; starts at 1
  adjustment <- 1

  # above_target and below_target both need to become TRUE before the adjustment
  # object starts reducing in magnitude
  above_target <- FALSE
  below_target <- FALSE

  while (converged == FALSE) {
    # build linear model for monthly capacity
    lm_fit <- lm(
      capacity ~ period,
      data = tibble(
        capacity = c(t_1_capacity, t_1_capacity * change_proportion),
        period = c(1, 13)
      ))

    capacity_projections <- predict(
      object = lm_fit,
      newdata = tibble(period = 1:length(referrals_projections))
    ) |>
      unname()


    proportion_at_highest_bin <- apply_params_to_projections(
      capacity_projections = capacity_projections,
      referrals_projections = referrals_projections,
      incomplete_pathways = incomplete_pathways,
      renege_capacity_params = renege_capacity_params,
      max_months_waited = max_months_waited
    ) |>
      filter(
        period_id == max(period_id)
      ) |>
      mutate(
        proportion_incomplete = incompletes / sum(incompletes)
      ) |>
      filter(
        .data$months_waited_id == max_months_waited
      ) |>
      pull(
        .data$proportion_incomplete
      )

    compare_with_target <- (proportion_at_highest_bin - target_val)

    if (abs(compare_with_target) < tolerance) {
      converged <- TRUE
    } else {
      if (compare_with_target > 0) {
        above_target <- TRUE

        if (above_target & below_target) {
          # converge the adjustment factor
          adjustment <- adjustment / 2
        }

        # if the proportion waiting in the final bin is higher than the target
        # value then we need to increase capacity
        change_proportion <- change_proportion + adjustment
      } else {
        below_target <- TRUE

        if (above_target & below_target) {
          # converge the adjustment factor
          adjustment <- adjustment / 2
        }

        change_proportion <- change_proportion - adjustment
      }
    }
  }

  return(change_proportion)
}

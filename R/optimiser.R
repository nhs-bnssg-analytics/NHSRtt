#' Optimise the capacity profile for projections
#'
#' @param t_1_capacity numeric; a single value for the capacity for the first
#'   time period
#' @param referrals_projections numeric; a vector for the number of referrals
#'   for each period in the projected time period
#' @param target string length 1; can be either a percentage point change, eg,
#'   "~-5\%" or a percent value, eg, "5\%"
#' @param target_bin numeric length 1; the bin that the target refers to. It
#'   must be less than or equal to the max_months_waited value
#' @param tolerance numeric length 1; the tolerance used to compare the absolute
#'   error with in the max_months_waited bin to determine convergence. The
#'   absolute error is calculated on the proportion in the max_months_waited bin
#'   relative to the total waiting (even if a non-percentage target is used)
#' @param max_iterations numeric; the maximum number of iterations to test for
#'   convergence before providing a warning and an invalid number
#' @inheritParams apply_params_to_projections
#'
#' @importFrom dplyr setdiff mutate case_when summarise filter pull between
#' @importFrom stats lm predict
#' @importFrom rlang .data
#'
#' @returns a capacity multiplier representing the annual change in capacity
#'   (from the input t_1_capacity to a capacity at t = 13) to achieve the
#'   desired target within the target tolerance
#' @export
#'
optimise_capacity <- function(t_1_capacity, referrals_projections,
                              incomplete_pathways, renege_capacity_params,
                              target, target_bin,
                              tolerance, max_iterations = 50) {

  # checks

  # check lengths of inputs
  if (length(t_1_capacity) != 1)
    stop("t_1_capacity must be length 1")

  # check numeric inputs for t_1_capacity
  if (!is.numeric(t_1_capacity))
    stop("t_1_capacity must be numeric")

  if (!is.numeric(referrals_projections))
    stop("referrals must be a numeric vector")

  # check target_bin in incompletes data
  if (!(target_bin %in% incomplete_pathways[["months_waited_id"]]))
    stop("target_bin must be within the incompletes_pathways data set")

  # check field names
  if (length(setdiff(names(renege_capacity_params), c("months_waited_id", "renege_param", "capacity_param"))) > 0)
    stop("renege_capacity_params must have the column names: months_waited_id, renege_param and capacity_param")

  # check whether target_bin is less than the greatest number of months waited
  # in the incompletes dataset
  max_months_waited <- max(incomplete_pathways[["months_waited_id"]])
  if (target_bin > max_months_waited) {
    stop("target_bin is outside the months waited range in the data provided")
  }

  # check the column headers
  if (length(dplyr::setdiff(names(incomplete_pathways), c("months_waited_id", "incompletes"))) > 0) {
    stop("incomplete_pathways must have field names of 'months_waited_id' and 'incompletes'")
  }

  # checks on target
  if (!grepl("%", target))
    stop("target must have a percentage")

  if (length(parse_number(target)) == 0)
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
      incompletes = sum(.data$incompletes),
      .by = "months_waited_id"
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
  iteration <- 1

  # adjustment is the amount to adjust the change_proportion when attempting to
  # converge; starts at 1
  adjustment <- 1

  # above_target and below_target both need to become TRUE before the adjustment
  # object starts reducing in magnitude
  above_target <- FALSE
  below_target <- FALSE

  while (converged == FALSE) {
    # build linear model for monthly capacity
    lm_fit <- stats::lm(
      capacity ~ period,
      data = tibble(
        capacity = c(t_1_capacity, t_1_capacity * change_proportion),
        period = c(1, 13)
      ))

    capacity_projections <- stats::predict(
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
        months_waited_id = case_when(
          months_waited_id >= target_bin ~ target_bin,
          .default = months_waited_id
        )
      ) |>
      summarise(
        incompletes = sum(.data$incompletes),
        .by = "months_waited_id"
      )

    if (sum(proportion_at_highest_bin[["incompletes"]]) == 0) {
      proportion_at_highest_bin <- 0
    } else {
      proportion_at_highest_bin <- proportion_at_highest_bin |>
        mutate(
          proportion_incomplete = .data$incompletes / sum(.data$incompletes)
        ) |>
        filter(
          .data$months_waited_id == target_bin
        ) |>
        pull(
          .data$proportion_incomplete
        )
    }

    compare_with_target <- (proportion_at_highest_bin - target_val)

    if (abs(compare_with_target) < tolerance) {
      converged <- TRUE
    } else {
      iteration <- iteration + 1
      if (iteration > max_iterations) {
        warning("optimiser failed to converge before maximum iteration reached")
        converged <- TRUE # set to true so while loop is exited
        change_proportion <- NA_real_
      }
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

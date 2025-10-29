#' Optimise the capacity profile for projections
#'
#' @param t_1_capacity numeric; a single value for the capacity for the first
#'   time period of the projected time period.
#' @param referrals_projections numeric; a vector for the number of referrals
#'   for each period in the projected time period.
#' @param target string length 1; can be either a percentage point change, eg,
#'   "~-5\%" or a percent value, eg, "5\%". This refers to percentage of
#'   patients on the waiting list in the \code{target_bin} or higher waiting
#'   times. Note, this is the opposite of the NHS RTT targets, which are a
#'   proportion of patients on the waiting list that are below the
#'   \code{target_bin}.
#' @param target_bin numeric length 1; the bin that the target refers to. It
#'   must be less than or equal to the max_months_waited value
#' @param capacity_profile string, one of "linear_change" or "flat"; determines
#'   how the capacity counts vary into the future. Linear change means that the
#'   first point is held stationary at the value of \code{t_1_capacity} and the
#'   end point is varied, with a linear interpolation between the two points.
#'   Flat means that capacity remains constant into the future.
#' @param tolerance numeric length 1; the tolerance used to compare the absolute
#'   error with in the max_months_waited bin to determine convergence. The
#'   absolute error is calculated on the proportion in the max_months_waited bin
#'   relative to the total waiting (even if a non-percentage target is used)
#' @param max_iterations numeric; the maximum number of iterations to test for
#'   convergence before providing a warning and an invalid number.
#' @param incomplete_adjustment_factor tibble; with fields "months_waited_id" and
#'   "adjustment_factor"; this is used if adjusted incomplete counts used in the
#'   \code{incomplete_pathways} parameter, where the adjusted incomplete counts
#'   represent a waiting list that include individuals thatare on the waiting list
#'   without a clock start (eg, they have joined the waiting list partway through
#'   their pathway). This argument is needed as the performance will be calculated
#'   using an unadjusted count of incomplete pathways.
#' @inheritParams apply_params_to_projections
#'
#' @importFrom dplyr setdiff mutate case_when summarise filter pull between
#' @importFrom stats lm predict
#' @importFrom rlang .data
#'
#' @returns A capacity multiplier that is applied to the
#'   \code{t_1_capacity} input to achieve the desired target by the end of the
#'   projection period. Where the \code{capacity_profile} is 'linear_change',
#'   this represent a linear growth in capacity from
#'   \ifelse{html}{\out{t<sub>1</sub>}}{\eqn{t_1}} to
#'   \ifelse{html}{\out{t<sub>13</sub>}}{\eqn{t_13}}, where
#'   \ifelse{html}{\out{t<sub>1</sub>}}{\eqn{t_1}} is equal to
#'   \code{t_1_capacity} and \ifelse{html}{\out{t<sub>13</sub>}}{\eqn{t_13}} is
#'   \code{t_1_capacity} multiplied by the output of the function.
#'
#'   If \code{capacity_profile} is 'flat', then the projected capacity is simply
#'   \code{t_1_capacity} multiplied by the output of the function for the whole
#'   of the projected period.
#'
#'   The name of the returned object provides an indication of whether the
#'   optimiser converged.
#' @export
#'
optimise_capacity <- function(
  t_1_capacity,
  referrals_projections,
  incomplete_pathways,
  incomplete_adjustment_factor = NULL,
  renege_capacity_params,
  target,
  target_bin,
  capacity_profile = "linear_change",
  surplus_treatment_redistribution_method = "evenly",
  tolerance,
  max_iterations = 50
) {
  # checks

  # check lengths of inputs
  if (length(t_1_capacity) != 1) {
    stop("t_1_capacity must be length 1")
  }

  # check numeric inputs for t_1_capacity
  if (!is.numeric(t_1_capacity)) {
    stop("t_1_capacity must be numeric")
  }

  if (!is.numeric(referrals_projections)) {
    stop("referrals must be a numeric vector")
  }

  # check target_bin in incompletes data
  if (!(target_bin %in% incomplete_pathways[["months_waited_id"]])) {
    stop("target_bin must be within the incompletes_pathways data set")
  }

  # check field names
  if (
    length(setdiff(
      names(renege_capacity_params),
      c("months_waited_id", "renege_param", "capacity_param")
    )) >
      0
  ) {
    stop(
      "renege_capacity_params must have the column names: months_waited_id, renege_param and capacity_param"
    )
  }

  # check values of capacity_param
  if (all(renege_capacity_params[["capacity_param"]] == 0)) {
    warning("Unable to optimise as no treatments in the calibration period")
    change_proportion <- setNames(
      NA,
      nm = "no_calibration_treatments"
    )
    return(change_proportion)
  }

  max_months_waited <- max(incomplete_pathways[["months_waited_id"]])

  # check the column headers
  if (
    length(dplyr::setdiff(
      names(incomplete_pathways),
      c("months_waited_id", "incompletes")
    )) >
      0
  ) {
    stop(
      "incomplete_pathways must have field names of 'months_waited_id' and 'incompletes'"
    )
  }

  # checks on target
  if (!grepl("%", target)) {
    stop("target must have a percentage")
  }

  if (length(parse_number(target)) == 0) {
    stop("unable to parse the number from target")
  }

  if (length(parse_number(target)) > 1) {
    stop("multiple numbers parsed from target")
  }

  if (
    !between(parse_number(target), 0, 100) &
      !grepl("~", target)
  ) {
    stop("absolute target must be between 0% and 100%")
  }

  # checks on tolerance
  if (!is.numeric(tolerance)) {
    stop("tolerance must be numeric")
  }

  if (length(tolerance) > 1) {
    stop("tolerance must be length 1")
  }

  # if no t_1 capacity then multiplier will not work
  if (t_1_capacity == 0) {
    change_proportion <- setNames(
      NA,
      nm = "no_starting_capacity"
    )
    return(change_proportion)
  }

  # check inputs to capacity_profile
  capacity_profile <- match.arg(
    capacity_profile,
    c("linear_change", "flat")
  )

  # check incomplete_adjustment_factor
  if (!is.null(incomplete_adjustment_factor)) {
    if (!dplyr::is.tbl(incomplete_adjustment_factor)) {
      stop("incomplete_adjustment_factor must be a tibble")
    }

    # check the names of the provided tibble
    if (
      !all(
        names(incomplete_adjustment_factor) %in%
          c("months_waited_id", "adjustment_factor")
      )
    ) {
      stop(
        "names of incomplete_adjustment_factor must be 'months_waited_id' and 'adjustment_factor'"
      )
    }
  }

  # check that values are provided for all months_waited_id
  if (
    !all(
      incomplete_adjustment_factor[["months_waited_id"]] %in%
        incomplete_pathways[["months_waited_id"]]
    )
  ) {
    stop(
      "months_waited_id values in incomplete_adjustment_factor must match those in incomplete_pathways"
    )
  }

  if (!is.null(incomplete_adjustment_factor)) {
    # deflate_incomplete_pathways to calculate performance
    incomplete_pathways_unadjusted <- incomplete_pathways |>
      dplyr::left_join(
        incomplete_adjustment_factor,
        by = "months_waited_id"
      ) |>
      mutate(
        incompletes = .data$incompletes /
          (1 + .data$adjustment_factor)
      ) |>
      select(!c("adjustment_factor"))
  } else {
    incomplete_pathways_unadjusted <- incomplete_pathways
  }

  # target calculation
  current_val <- incomplete_pathways_unadjusted |>
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

  # if there are no incompletes in final bin (eg, current_val is NaN), need to
  # set current_val to 0
  if (is.nan(current_val)) {
    current_val <- 0
  }

  target_val <- parse_number(target) / 100

  if (grepl("~", target)) {
    # if tilde in target string, then make the target a relative number
    target_val <- current_val + target_val
  }

  # set target_val to 0 if it is negative
  if (target_val < 0) {
    target_val <- 0
  }

  # set target_val to 100% if over 100%
  if (target_val > 1) {
    target_val <- 1
  }

  change_proportion <- 1 # this controls the size of the adjustment for each iteration
  converged <- FALSE
  iteration <- 1
  last_iteration_proportion <- NA

  # adjustment is the amount to adjust the change_proportion when attempting to
  # converge; starts at 1
  adjustment <- 1

  # above_target and below_target both need to become TRUE before the adjustment
  # object starts reducing in magnitude
  above_target <- FALSE
  below_target <- FALSE

  # for gradients that treat the whole waiting list, we want to find the minimum
  # gradient that achieves that, so we set a starting value of NULL
  min_change_proportion <- NULL

  while (converged == FALSE) {
    if (capacity_profile == "linear_change") {
      # build linear model for monthly capacity
      lm_fit <- stats::lm(
        capacity ~ period,
        data = tibble(
          capacity = c(t_1_capacity, t_1_capacity * change_proportion),
          period = c(1, 13)
        )
      )

      capacity_projections <- stats::predict(
        object = lm_fit,
        newdata = tibble(period = 1:length(referrals_projections))
      ) |>
        unname()
    } else if (capacity_profile == "flat") {
      # create a flat profile for capacity with the same length as the referrals
      # profile
      capacity_projections <- rep(
        t_1_capacity * change_proportion,
        length(referrals_projections)
      )
    }

    # floor the data at 0 because negative capacity is not possible
    capacity_projections[capacity_projections < 0] <- 0

    proportion_at_highest_bin <- apply_params_to_projections(
      capacity_projections = capacity_projections,
      referrals_projections = referrals_projections,
      incomplete_pathways = incomplete_pathways,
      renege_capacity_params = renege_capacity_params,
      max_months_waited = max_months_waited,
      surplus_treatment_redistribution_method = surplus_treatment_redistribution_method
    ) |>
      filter(
        period_id == max(period_id)
      )

    if (!is.null(incomplete_adjustment_factor)) {
      # deflate_incomplete_pathways
      proportion_at_highest_bin <- proportion_at_highest_bin |>
        dplyr::left_join(
          incomplete_adjustment_factor,
          by = "months_waited_id"
        ) |>
        mutate(
          incompletes = .data$incompletes /
            (1 + .data$adjustment_factor)
        ) |>
        select(!c("adjustment_factor"))
    }

    proportion_at_highest_bin <- proportion_at_highest_bin |>
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

    wl_size <- sum(proportion_at_highest_bin[["incompletes"]])

    if (wl_size == 0) {
      # this part of statement means the total on the waiting list is zero
      proportion_at_highest_bin <- 0
      min_change_proportion <- min(min_change_proportion, change_proportion)
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
      # browser()
      converged <- TRUE
      change_proportion <- setNames(
        change_proportion,
        nm = "converged"
      )
    } else {
      iteration <- iteration + 1

      if (
        isTRUE(last_iteration_proportion == proportion_at_highest_bin) &
          !(proportion_at_highest_bin %in% c(0, 1))
      ) {
        warning("parameter distribution means optimiser cannot meet target")
        converged <- TRUE
        change_proportion <- Inf
        change_proportion <- setNames(
          change_proportion,
          nm = "not_converged_check"
        )
      }
      last_iteration_proportion <- proportion_at_highest_bin

      if (iteration > max_iterations) {
        warning("optimiser failed to converge before maximum iteration reached")
        converged <- TRUE # set to true so while loop is exited

        if (is.null(min_change_proportion)) {
          change_proportion <- NaN
          change_proportion <- setNames(
            change_proportion,
            nm = "not_converged"
          )
        } else {
          change_proportion <- min(change_proportion)
          change_proportion <- setNames(
            change_proportion,
            nm = "waitlist_cleared"
          )
        }
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

    # FOR DEBUGGING
    # cat(
    #   paste(
    #     paste("Iteration:", iteration - 1),
    #     paste("Performance:", paste0(formatC(100 * proportion_at_highest_bin, format = "f", digits = 1), "%")),
    #     paste("WL size:", round(wl_size, 2)),
    #     paste("Next change proportion:", round(change_proportion, 3)),
    #     "\n"
    #   )
    # )
  }

  return(change_proportion)
}

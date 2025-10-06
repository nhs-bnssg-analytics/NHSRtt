#' Calculate Geometric Survival Probabilities
#'
#' Computes the survival probabilities for a geometric distribution over a
#' specified number of months.
#'
#' @param p1 numeric of length 1; the probability parameter for the geometric distribution
#'   (probability of failure in each period).
#' @param max_num_months integer of length 1; the maximum number of months to calculate
#'   survival probabilities for, between 1 and 24. Default is 24.
#'
#' @return A numeric vector of survival probabilities for each month.
#'
#' @examples
#' \dontrun{
#' # Calculate the geometric survival probabilities for p1 = 0.2, up to 24 months
#' geom_fn_s(p1 = 0.2, max_num_months = 24)
#'
#' # Calculate for a single month
#' geom_fn_s(p1 = 0.7, max_num_months = 1)
#' }
#'
geom_fn_s <- function(p1, max_num_months = 24) {
  if (max_num_months > 24 | max_num_months < 1) {
    stop("max_num_months must be between 1 and 24")
  }

  probs_24_month <- stats::dgeom(0:24, prob = 1 - p1)

  # aggregate top compartment
  probs <- c(
    probs_24_month[seq_len(max_num_months - 1)],
    sum(probs_24_month[max_num_months:25])
  )
  return(probs)
}

#' @title Configure Removals Data Frame
#'
#' @description Constructs a data frame representing the configuration of removals,
#'   including time, renge rates, and potential services.
#'
#' @param renege_params numeric; proportion \ifelse{html}{\out{(0 &le; r<sub>m</sub> &leq; 1)}}{\eqn{(0 \leq r_m \leq 1)}}
#'   of waiting patients reneging in the m-th month of waiting, where \ifelse{html}{\out{r<sub>2</sub>}}{\eqn{r_2}} = 0.1
#'   refers to 10\% of those in the second month of waiting reneging.
#' @param mu numeric of length 1; total capacity to be applied over the geometric
#'   distribution
#' @inheritParams geom_fn_s
#'
#' @return A data frame with columns: \code{months_waited_id}, \code{r}, and
#'   \code{service}. \code{months_waited_id} is the lower bound of the
#'   number of months waited. \code{r} is the renege rate for each month
#'   waited compartment. \code{service} is the initial capacity provided,
#'   applied across the compartments using the geometric distribution
#'   defined by \code{p1}.
#'
#' @seealso \code{\link{geom_fn_s}}
#'
#' @examples
#' \dontrun{
# Example usage:
#' set.seed(123)
#' renege_params <- runif(24)
#' initialise_removals(renege_params, p1 = 0.5, mu = 50)
#' }
initialise_removals <- function(renege_params, p1, mu) {
  capacity <- c(mu, rep(0, length(renege_params) - 1))
  s_distribution <- geom_fn_s(p1, max_num_months = length(renege_params))
  wl_removals <- data.frame(
    months_waited_id = seq_along(renege_params) - 1,
    r = renege_params,
    # service = round(s_distribution * mu)
    service = s_distribution * mu
  )
  return(wl_removals)
}


#' @title Calculate Waiting List Sizes by Time Waited
#'
#' @description This function applies the available capacity, defined by the geometric
#' distribution and the total capacity, and updates a data frame representing
#' the waiting list, by time waited, by calculating the waiting list size
#' (\code{wlsize}) and the number of serviced items (\code{sigma}) for each time
#' compartment waited.
#'
#' @param wl_removals A data frame containing the renege rate and potential number of
#'   services per waiting compartment. It must include columns \code{r} (removal/renege
#'   rate), and \code{service} (number of potential services).
#' @param referrals numeric of length 1; value representing the steady-state number
#'   of referrals.
#'
#' @return A data frame with calculated \code{wlsize} and \code{sigma} columns for each row.
#'
#' @details For each row in \code{wl_removals}, the function computes the new waiting
#'   list size based on the previous size, the removal rate, and the number
#'   serviced. If the computed size is positive, it is assigned to \code{wlsize} and
#'   \code{sigma} is set to the serviced value. If not, \code{wlsize} is set to zero and
#'   \code{sigma} is set to the floored value of the previous size after removal.
#'
#' @examples
#' \dontrun{
#' wl_removals <- data.frame(r = c(0.1, 0.2), service = c(5, 3))
#' calc_wl_sizes(wl_removals, referrals = 50)
#' }
calc_wl_sizes <- function(wl_removals, referrals) {
  # sigma is the number of people actually serviced (treated)
  wl_removals$sigma <- 0
  wl_removals$wlsize <- 0

  for (i in 1:nrow(wl_removals)) {
    prev_wlsize <- if (i == 1) referrals else wl_removals$wlsize[i - 1]
    # val <- floor(prev_wlsize * (1 - wl_removals$r[i]) - wl_removals$service[i])
    val <- prev_wlsize * (1 - wl_removals$r[i]) - wl_removals$service[i]
    if (val > 0) {
      wl_removals$wlsize[i] <- val
      wl_removals$sigma[i] <- wl_removals$service[i]
    } else {
      wl_removals$wlsize[i] <- 0
      # wl_removals$sigma[i] <- floor(prev_wlsize * (1 - wl_removals$r[i]))
      wl_removals$sigma[i] <- prev_wlsize * (1 - wl_removals$r[i])
    }
  }
  return(wl_removals)
}


#' @title Calculate the Time at a Given Percentile of a Weighted Distribution
#'
#' @description Computes the interpolated time at which a specified percentile of the cumulative sum
#' of a weight column (e.g., waiting list size) is reached, based on a sorted data frame.
#'
#' @param wl_structure A data frame containing at least the columns specified by \code{wlsize_col} and \code{time_col}.
#' @param percentile Numeric value between 0 and 1 indicating the desired percentile (default is 0.92).
#' @param wlsize_col Character string specifying the name of the column representing the weights (default is \code{wlsize}).
#' @param time_col Character string specifying the name of the column representing the time variable (default is \code{time}).
#'
#' @return A numeric value representing the interpolated time at which the cumulative sum of the weights reaches the specified
#'   percentile. The value is the number of months waited where the percentile value is met. Despite the requirement that the
#'   \code{time_col} starts at 0, a value of 0.5 means the percentile value is met after waiting 0.5 months
#'
#' @details
#' The function sorts the data frame by the time column, computes the cumulative sum of the weights,
#' and determines the time at which the cumulative sum first meets or exceeds the specified percentile
#' of the total sum. If the percentile falls within a bin, linear interpolation is used to estimate the precise time.
#'
#' @importFrom rlang .data
#' @importFrom dplyr mutate filter slice
#' @examples
#' \dontrun{
#' wl_structure <- data.frame(time = 1:10, wlsize = c(5, 3, 8, 2, 7, 4, 6, 1, 9, 2))
#' hist_percentile_calc(wl_structure, percentile = 0.9)
#' }
#' @importFrom dplyr arrange mutate slice filter
#' @export
hist_percentile_calc <- function(
  wl_structure,
  percentile = 0.92,
  wlsize_col = "wlsize",
  time_col = "months_waited_id"
) {
  # check that the column names exist in wl_structure
  if (!wlsize_col %in% names(wl_structure)) {
    stop("wlsize_col not in column names of wl_structure")
  }

  if (!time_col %in% names(wl_structure)) {
    stop("time_col not in column names of wl_structure")
  }

  wl_structure <- wl_structure |>
    arrange(.data[[time_col]])

  # replace nas in the wlsize_col
  wl_structure[[wlsize_col]][is.na(wl_structure[[wlsize_col]])] <- 0

  # check that time_col starts at 0 to be consistent within the package
  if (wl_structure[[time_col]][1] != 0) {
    stop(
      "time_col must start from 0, and be the lower bound of the unit of time waiting"
    )
  }

  total_wlsize <- sum(wl_structure[[wlsize_col]], na.rm = TRUE)
  p_cut <- percentile * total_wlsize

  wl_structure <- wl_structure |>
    mutate(cum_wlsize = cumsum(.data[[wlsize_col]]))
  row_p <- wl_structure |>
    filter(.data$cum_wlsize >= p_cut) |>
    slice(1)

  needed_in_row <- p_cut - (row_p$cum_wlsize - row_p[[wlsize_col]])
  if (row_p[[wlsize_col]] == 0) {
    prop_in_bin <- 0
  } else {
    prop_in_bin <- needed_in_row / row_p[[wlsize_col]]
  }

  # Interpolate the time within the bin
  if (row_p[[time_col]] > 0) {
    prev_time <- wl_structure[[time_col]][row_p[[time_col]]]
  } else {
    prev_time <- -1
  }
  time_p <- prev_time + prop_in_bin * (row_p[[time_col]] - prev_time)
  # add 1 because months_waited_id is lower bound of the number of months waited
  time_p <- time_p + 1
  return(time_p)
}


#' @title Find the value of p1 that achieves a target waiting time percentile
#'
#' @description Uses a binary search algorithm to find the value of \code{p1} (the proportion
#' parameter) that achieves a specified target waiting time (\code{target_time})
#' at a given percentile (hardcoded as 92nd percentile) of the cumulative
#' waiting list size, given a treatment capacity in the first month
#' (\code{mu_1}) and other parameters.
#'
#' @param target_time numeric of length 1; the target waiting time,
#'    in months, to achieve the specified percentile.
#' @param mu_1 numeric of length 1; treatment capacity in the first month.
#' @param p1_lower numeric of length 1; lower bound for the binary search of \code{p1}.
#'   Must be between 0 and 1 and less than  \code{p1_upper}. Defaults to 0.1.
#' @param p1_upper numeric of length 1; upper bound for the binary search of \code{p1}.
#'   Must be between 0 and 1 and more than  \code{p1_lower}. Defaults to 0.85.
#' @param tolerance numeric of length 1; tolerance for the binary search stopping
#'   criterion. The unit is months.
#' @param max_iterations numeric of length 1; maximum number of iterations to
#'   search for a valid result
#' @inheritParams geom_fn_s
#' @inheritParams calc_wl_sizes
#' @inheritParams hist_percentile_calc
#' @inheritParams initialise_removals
#'
#' @return A list containing: \item{p1}{The value of \code{p1} found to achieve
#'   the target waiting time.} \item{time_p}{The interpolated waiting time at
#'   the specified percentile.} \item{mu}{The total number of treatments required
#'   when the solution is found.} \item{wlsize}{The total size of the waiting list
#'   when the solution is found.} \item{waiting_list}{The data frame with historical data
#'   formatted and waiting list sizes calculated.} \item{niterations}{The number
#'   of iterations performed in the binary search.} \item{status}{Converged indicates a
#'   solution was found, and Not converged indicates no solution was found}
#'
#' @details The function repeatedly uses the removals table (from \code{initialise_removals()})
#'   and calculates waiting list sizes for candidate values of \code{p1}, using binary search to
#'   home in on the value that achieves the target waiting time at the required
#'   percentile. The search stops when the difference between upper and lower
#'   bounds is less than \code{tolerance}, or when the calculated waiting time is
#'   within \code{tolerance} of the target.
#'
#' @seealso \code{\link{initialise_removals}}, \code{\link{calc_wl_sizes}},
#'   \code{\link{hist_percentile_calc}}
#'
#' @examples
#' find_p(
#'   target_time = 4 + (68 / 487),
#'   renege_params = c(0.04, 0.04, 0.03, 0.01, 0.02, 0.02, 0.01),
#'   mu_1 = 2651.227,
#'   p1_lower = 0.1,
#'   p1_upper = 0.85,
#'   tolerance = 0.001,
#'   referrals = 12000,
#'   percentile = 0.92
#' )
#' @export
#'
find_p <- function(
  target_time = 4 + (68 / 487),
  renege_params,
  mu_1,
  p1_lower = 0.1,
  p1_upper = 0.85,
  tolerance = 0.001,
  referrals,
  percentile = 0.92,
  max_iterations = 15
) {
  # check p1_lower less than p1_upper
  if (p1_lower > p1_upper) {
    stop("p1_lower must be less than p1_upper")
  }

  # check p1_lower and p1_upper between 0 and 1
  if (!(all(between(p1_lower, 0, 1), between(p1_upper, 0, 1)))) {
    stop("p1_lower and p1_upper need to be between 0 and 1")
  }

  # check all renege parameters are greater or equal to 0
  if (!all(renege_params >= 0)) {
    stop("All renege_params must be greater or equal to 0")
  }

  max_num_months <- length(renege_params)

  iter_count <- 0
  time_p <- 1e12 # dummy number so the while loop can commence
  # Use binary search to find the value of p1 that achieves the target_time
  # at the desired percentile
  while (
    abs(time_p - target_time) > tolerance &
      iter_count < max_iterations
  ) {
    iter_count <- iter_count + 1

    p1_mid <- (p1_lower + p1_upper) / 2 # Calculate midpoint of current p1
    # Format the historical data with current p1
    removals_table <- initialise_removals(
      renege_params = renege_params,
      p1 = p1_mid,
      mu = mu_1 / (1 - p1_mid)
    )

    # Calculate waiting list sizes for each time period
    waiting_list <- calc_wl_sizes(
      wl_removals = removals_table,
      referrals = referrals
    )

    # Calculate the interpolated time at which the desired percentile of
    # cumulative wlsize is reached
    time_p <- hist_percentile_calc(
      waiting_list,
      percentile = percentile,
      wlsize_col = "wlsize",
      time_col = "months_waited_id"
    )
    # Check if the calculated time is within the specified tolerance of the
    # target
    if (abs(time_p - target_time) < tolerance) {
      # If yes, return the current p1 and time_p
      return(list(
        p1 = p1_mid,
        time_p = time_p,
        mu = sum(waiting_list$sigma),
        wlsize = sum(waiting_list$wlsize),
        waiting_list = waiting_list,
        niterations = iter_count,
        status = "Converged"
      ))
    } else if (time_p > target_time) {
      # If the time is too high, increase p1_lower to search higher p1
      p1_lower <- p1_mid
    } else {
      # If the time is too low, decrease p1_upper to search lower p1
      p1_upper <- p1_mid
    }
  }
  # After exiting the loop, return the closest found p1 and corresponding
  # time_p
  removals_table <- initialise_removals(
    renege_params = renege_params,
    p1 = p1_mid,
    mu = mu_1 / (1 - p1_mid)
  )
  waiting_list <- calc_wl_sizes(removals_table, referrals)

  # Calculate the interpolated time at which the desired percentile of
  # cumulative wlsize is reached
  time_p <- hist_percentile_calc(
    waiting_list,
    percentile = percentile,
    wlsize_col = "wlsize",
    time_col = "months_waited_id"
  )

  return(list(
    p1 = p1_mid,
    time_p = time_p,
    mu = sum(waiting_list$sigma),
    wlsize = sum(waiting_list$wlsize),
    waiting_list = waiting_list,
    niterations = iter_count,
    status = "Not converged"
  ))
}

#' Convert target_time into a vector of 1s and 0s
#' @inheritParams find_p
#' @param n_months integer length 1, number of compartments
#' @return numeric vector (all values between 0 and 1 inclusive) of length equal to n_months
#'   value
#' @noRd
calc_gamma <- function(target_time, n_months) {
  if (target_time >= n_months) {
    stop("target_time must be less than n_months")
  }
  ones <- floor(target_time)
  fraction <- target_time %% 1
  zeros <- n_months - (ones + 1)
  gamma <- c(rep(1, ones), fraction, rep(0, zeros))
  return(gamma)
}


#' Identify the steady-state solution with the closest treatment capacity or renege rate
#'   to a given target
#'
#' @description This function performs either a linear programming or binary search method
#'   to identify a solution that satisies multiple constraints (1, arrivals = departures,
#'   2, percentile on waiting list waiting within given time is equal to given value, 3,
#'   renege rate is equal to given value)
#'
#' @param target Numeric. The desired renege rate to be achieved. The value must be between
#'   0 and 1 and represents the proportion of all departures that are reneges.
#' @param bs_tolerance Numeric. Acceptable deviation from `target` for convergence.
#'   Defaults to 10\% of `target`.
#' @param bs_max_iterations Integer. Maximum number of iterations for the binary search.
#'   Defaults to 10.
#' @param method character length 1; "lp" for linear programming, or "bs" for
#'   binary search
#' @inheritParams initialise_removals
#' @inheritParams calc_wl_sizes
#' @inheritParams find_p
#' @inheritParams optimise_steady_state_lp
#'
#' @importFrom purrr pluck map map_chr map_dbl
#'
#' @return A list containing:
#'   \item{p1}{The value of \code{p1} found to achieve the target waiting time.}
#'   \item{time_p}{The interpolated waiting time at the specified percentile.}
#'   \item{mu}{The total number of treatments required when the solution is found.}
#'   \item{wlsize}{The total size of the waiting list when the solution is found.}
#'   \item{waiting_list}{The data frame with historical data formatted and waiting
#'     list sizes calculated.}
#'   \item{niterations}{The number of iterations performed in the binary search.}
#'   \item{status}{Converged indicates a solution was found, and Not converged
#'     indicates no solution was found.}
#'   \item{method}{The method the solution was identified.}
#'
#' @examples
#' \dontrun{
#' optimise_steady_state(
#'   referrals = 100,
#'   target = 0.1,
#'   renege_params = runif(24),
#'   method = "lp",
#'   s_given = c(0.4, 0.2, 0.1, rep(0.05, 6), rep(0, 15))
#' )
#'
#' optimise_steady_state(
#'   referrals = 100,
#'   bs_target = 0.2,
#'   renege_params = runif(24),
#'   method = "bs"
#' )
#' }
#'
#' @export
optimise_steady_state <- function(
  referrals,
  target,
  renege_params,
  target_time = 4 + (68 / 487),
  percentile = 0.92,
  method = c("lp", "bs"),
  s_given = NULL,
  bs_tolerance = target * 0.05,
  bs_max_iterations = 10
) {
  # check referrals is single length numeric
  if (!is.numeric(referrals)) {
    stop("referrals must be numeric")
  }

  if (length(referrals) != 1) {
    stop("referrals must be length 1")
  }

  # check target is single length numeric
  if (!is.numeric(target)) {
    stop("target must be numeric")
  }

  if (length(target) != 1) {
    stop("target must be length 1")
  }

  # check method input
  if (identical(method, c("lp", "bs"))) {
    method <- "lp"
  }

  method <- match.arg(
    method,
    c("lp", "bs")
  )

  if (method == "lp") {
    gamma <- calc_gamma(target_time, length(renege_params))

    # check s_given inputs
    if (is.null(s_given)) {
      stop("s_given cannot be NULL")
    }

    if (!is.numeric(s_given)) {
      stop("s_given must be numeric")
    }

    if (any(!between(s_given, 0, 1))) {
      stop("all values of s_given must be between 0 and 1")
    }

    if (length(s_given) != length(renege_params)) {
      stop("s_given must be the same length as renege_params")
    }

    lp_solution <- optimise_steady_state_lp(
      renege_params = renege_params,
      referrals = referrals,
      gamma = gamma,
      percentile = percentile,
      theta = target,
      s_given = s_given
    )$solution[seq_len(length(renege_params))]

    lp_out <- generate_lp_outputs(
      renege_params = renege_params,
      lp_solution = lp_solution,
      referrals = referrals,
      percentile = percentile
    )

    percentile_achieved <- round(lp_out$time_p, 5) == round(target_time, 5)
    if (isTRUE(percentile_achieved)) {
      lp_out[["status"]] <- "Converged"
      lp_out[["solution_method"]] <- "Linear programming exact solution"
      return(lp_out)
    } else {
      theta_increment <- 0.005
      target <- target - theta_increment
      while (between(target, 0, 1) & isFALSE(percentile_achieved)) {
        lp_solution <- optimise_steady_state_lp(
          renege_params = renege_params,
          referrals = referrals,
          gamma = gamma,
          percentile = percentile,
          theta = target,
          s_given = s_given
        )$solution[seq_len(length(renege_params))]

        lp_out <- generate_lp_outputs(
          renege_params = renege_params,
          lp_solution = lp_solution,
          referrals = referrals,
          percentile = percentile
        )

        percentile_achieved <- round(lp_out$time_p, 5) == round(target_time, 5)

        target <- target - theta_increment
      }
      if (isTRUE(percentile_achieved)) {
        lp_out[["status"]] <- "Converged"
        lp_out[["solution_method"]] <- "Linear programming nearest solution"
        return(lp_out)
      } else {
        lp_out[["status"]] <- "Not converged"
        lp_out[["solution_method"]] <- NA
        return(lp_out)
      }
    }
  } else if (method == "bs") {
    # check bs_bs_method input

    target_mu <- referrals - (referrals * target)
    mu_high <- target_mu * 0.7
    mu_low <- target_mu * 0.2

    iter <- 0
    tol <- 1e6
    found_steady_state <- FALSE
    mu_1_converged <- NA

    while (tol > bs_tolerance & iter < bs_max_iterations) {
      iter <- iter + 1
      mu_mid <- (mu_high + mu_low) / 2 # Calculate midpoint of current mu1

      solution <- find_p(
        renege_params = renege_params,
        mu_1 = mu_mid,
        referrals = referrals,
        target_time = target_time,
        percentile = percentile,
        max_iterations = 30
      )

      if (solution$status == "Converged") {
        found_steady_state <- TRUE
        mu_1_converged <- mu_mid
      }

      calc_solution <- ((referrals - solution$mu) / referrals)

      tol <- abs(calc_solution - target)
      if (tol < bs_tolerance) {
        break
      } else {
        if (calc_solution < target) {
          # If the renege rate is too low, decrease mu1 to find a higher treatment capacity
          mu_high <- mu_mid
        } else {
          # If the renege rate is too high, increase mu1 to find a lower treatment capacity
          mu_low <- mu_mid
        }
      }
    }

    # here we need to perform and secondary analysis for those who either
    # haven't found the solution that has a similar mu to the target
    # or whose solution isn't in steady state

    if (solution$status == "Converged") {
      return(
        list(
          p1 = solution$p1,
          time_p = solution$time_p,
          mu = solution$mu,
          wlsize = sum(solution$waiting_list$wlsize),
          waiting_list = solution$waiting_list,
          niterations = iter,
          status = solution$status,
          solution_method = "Within tolerance of target mu"
        )
      )
    } else if (isTRUE(found_steady_state)) {
      # where steady state has been found in previous iterations but
      # not the final iteration, search from the final iteration towards
      # the iteration where it was found, and select the first occasion
      # that steady state is identified
      increment <- (mu_1_converged - mu_mid) / 20

      while (solution$status == "Not converged") {
        mu_mid <- mu_mid + increment
        solution <- find_p(
          renege_params = renege_params,
          mu_1 = mu_mid,
          referrals = referrals,
          target_time = target_time,
          percentile = percentile,
          max_iterations = 30
        )
      }
      return(list(
        p1 = solution$p1,
        time_p = solution$time_p,
        mu = solution$mu,
        wlsize = sum(solution$waiting_list$wlsize),
        waiting_list = solution$waiting_list,
        niterations = -1, # eg, exceeded the iterations step
        status = solution$status,
        solution_method = "Between target mu and identified converged value"
      ))
    } else {
      # search along a range of solutions unrelated to the previous searches
      all_solutions <- seq(
        from = referrals * 0.1,
        to = referrals * 0.7,
        length.out = 400
      ) |>
        purrr::map(
          \(x) {
            solution <- find_p(
              renege_params = renege_params,
              mu_1 = x,
              referrals = referrals,
              target_time = target_time,
              percentile = percentile,
              max_iterations = 30
            )
          }
        )

      # find only converged solutions (eg, ones in steady state)
      converged_solutions <- all_solutions |>
        purrr::map_chr(
          ~ pluck(.x, "status")
        ) |>
        (\(x) x == "Converged")()

      # subset all solutions for those that are converged
      converged_solutions <- all_solutions[converged_solutions]

      if (!identical(converged_solutions, list())) {
        # of the converged solutions, identify the ones that have
        # the smalled renege rate
        min_rr_solution <- converged_solutions |>
          purrr::map_dbl(
            ~ pluck(.x, "mu")
          ) |>
          (\(x) {
            ((referrals - x) / referrals) == min((referrals - x) / referrals)
          })()

        # subset the converged solutions for the one with the
        # smallest mu
        final_solution <- converged_solutions[min_rr_solution]

        return(list(
          p1 = final_solution[[1]]$p1,
          time_p = final_solution[[1]]$time_p,
          mu = final_solution[[1]]$mu,
          wlsize = sum(final_solution[[1]]$waiting_list$wlsize),
          waiting_list = final_solution[[1]]$waiting_list,
          niterations = -1, # eg, exceeded the iterations step
          status = final_solution[[1]]$status,
          solution_method = "Solution identified from broader mus"
        ))
      } else {
        return(list(
          p1 = NA,
          time_p = NA,
          mu = NA,
          wlsize = NA,
          waiting_list = NULL,
          niterations = -1, # eg, exceeded the iterations step
          status = NA,
          solution_method = "No solution identified"
        ))
      }
    }
  }
}


# Linear programming method ----------------------------------------------

#######################################################
# 1. FORMULAE FOR COEFFS/CONSTANTS

#' @inheritParams optimise_steady_state_lp
#' @noRd
E <- function(m, renege_params) {
  1 -
    ifelse(
      (length(renege_params) - m) < 1,
      0,
      sum(sapply(1:(length(renege_params) - m), function(i) {
        renege_params[i + m] *
          ifelse(
            (i - 1) < 1,
            1,
            prod(sapply(1:(i - 1), function(j) (1 - renege_params[j + m])))
          )
      }))
    )
}

#' @inheritParams optimise_steady_state_lp
#' @noRd
J <- function(m, renege_params, gamma, percentile) {
  sum(sapply(1:(length(renege_params) - m + 1), function(i) {
    (gamma[i + m - 1] - percentile) *
      ifelse(
        (i - 1) < 1,
        1,
        prod(sapply(1:(i - 1), function(j) (1 - renege_params[j + m])))
      )
  }))
}

#' @inheritParams optimise_steady_state_lp
#' @noRd
G <- function(m, renege_params, theta) {
  theta +
    (1 - theta) *
      ifelse(
        (length(renege_params) - m) < 1,
        0,
        sum(sapply(1:(length(renege_params) - m), function(i) {
          renege_params[i + m] *
            ifelse(
              (i - 1) < 1,
              1,
              prod(sapply(1:(i - 1), function(j) (1 - renege_params[j + m])))
            )
        }))
      )
}

#' @inheritParams optimise_steady_state_lp
#' @noRd
Fx <- function(renege_params, referrals) {
  referrals -
    referrals *
      sum(sapply(1:length(renege_params), function(m) {
        renege_params[m] *
          ifelse(
            (m - 1) < 1,
            1,
            prod(sapply(1:(m - 1), function(i) (1 - renege_params[i])))
          )
      }))
}

#' @inheritParams optimise_steady_state_lp
#' @noRd
I <- function(renege_params, referrals, gamma, percentile) {
  referrals *
    sum(sapply(1:length(renege_params), function(m) {
      (gamma[m] - percentile) *
        prod(sapply(1:m, function(i) (1 - renege_params[i])))
    }))
}

#' @inheritParams optimise_steady_state_lp
#' @noRd
H <- function(renege_params, referrals, theta) {
  (1 - theta) *
    referrals *
    sum(sapply(1:length(renege_params), function(m) {
      renege_params[m] *
        ifelse(
          (m - 1) < 1,
          1,
          prod(sapply(1:(m - 1), function(i) (1 - renege_params[i])))
        )
    }))
}

#' @inheritParams optimise_steady_state_lp
#' @noRd
A <- function(renege_params, referrals, gamma, percentile, theta) {
  t(as.matrix(data.frame(
    E = sapply(1:length(renege_params), function(m) E(m, renege_params)),
    J = sapply(1:length(renege_params), function(m) {
      J(m, renege_params, gamma, percentile)
    }),
    G = sapply(1:length(renege_params), function(m) G(m, renege_params, theta))
  )))
}

#' @inheritParams optimise_steady_state_lp
#' @noRd
b <- function(renege_params, referrals, gamma, percentile, theta) {
  c(
    Fx(renege_params, referrals),
    I(renege_params, referrals, gamma, percentile),
    H(renege_params, referrals, theta)
  )
}

#' Linear programming solution to steady state optimisation
#' @param renege_params numeric; proportion \ifelse{html}{\out{(0 &le; r<sub>m</sub> &leq; 1)}}{\eqn{(0 \leq r_m \leq 1)}}
#'   of waiting patients reneging in the m-th month of waiting, where \ifelse{html}{\out{r<sub>2</sub>}}{\eqn{r_2}} = 0.1
#'   refers to 10\% of those in the second month of waiting reneging.
#' @param referrals numeric of length 1; value representing the steady-state number
#'   of referrals.
#' @param percentile numeric value between 0 and 1 indicating the desired percentile (default is 0.92).
#' @param gamma numeric vector, length equal to \code{length(renege_params)}; must take values between
#'   0 and 1. Vector represents the proportion of the corresponding compartment that the `percentile`
#'   target refers. For example, if the data has five compartments and the target percentile was at
#'   exactly 3.5 months, the corresponding vector would be c(1, 1, 1, 0.5, 0, 0)
#' @param theta numeric value of length 1, must be a value between 0 and 1 which is equal to
#'   the proportion of all departures that are reneges
#' @param s_given numeric vector, length equal to \code{length(renege_params)}; values must be between
#'   0 and 1 and must represent the proportion of total treatments that are applied to each compartment.
#'   This is used as a constraint in the optimisation and the output should follow the same profile
#' @importFrom lpSolve lp
#' @export
optimise_steady_state_lp <- function(
  renege_params,
  referrals,
  gamma,
  percentile,
  theta,
  s_given
) {
  tmp_Tx_given <- s_given * referrals * (1 - theta)
  tmp_A <- A(renege_params, referrals, gamma, percentile, theta)
  tmp_b <- b(renege_params, referrals, gamma, percentile, theta)
  tmp_M <- length(renege_params)
  # objective: minimize sum d_m, so coeffs: 0 for x, 1 for each d
  f.obj <- c(rep(0, tmp_M), rep(1, tmp_M))
  # constraint 1: Ax=b equality constraints
  con_Ax <- cbind(tmp_A, matrix(0, nrow = nrow(tmp_A), ncol = ncol(tmp_A)))
  dir_Ax <- rep("=", nrow(tmp_A))
  rhs_Ax <- tmp_b
  # constraint 2: d_m >= x_m - tmp_Tx_given_m: d_m - x_m >= -tmp_Tx_given_m
  con_d_pos <- matrix(0, nrow = ncol(tmp_A), ncol = 2 * ncol(tmp_A))
  for (m in 1:tmp_M) {
    con_d_pos[m, m] <- -1 # -x_m
    con_d_pos[m, tmp_M + m] <- 1 # d_m
  }
  dir_d_pos <- rep(">=", tmp_M)
  rhs_d_pos <- -tmp_Tx_given
  # constraint 3: d_m >= tmp_Tx_given_m - x_m: d_m + x_m >= tmp_Tx_given_m
  con_d_neg <- matrix(0, nrow = ncol(tmp_A), ncol = 2 * ncol(tmp_A))
  for (m in 1:tmp_M) {
    con_d_neg[m, m] <- 1 # x_m
    con_d_neg[m, tmp_M + m] <- 1 # d_m
  }
  dir_d_neg <- rep(">=", tmp_M)
  rhs_d_neg <- tmp_Tx_given
  # combine
  f.con <- rbind(con_Ax, con_d_pos, con_d_neg)
  f.dir <- c(dir_Ax, dir_d_pos, dir_d_neg)
  f.rhs <- c(rhs_Ax, rhs_d_pos, rhs_d_neg)
  # solution
  solution <- lpSolve::lp("min", f.obj, f.con, f.dir, f.rhs, all.int = FALSE)

  return(solution)
}

generate_lp_outputs <- function(
  renege_params,
  lp_solution,
  referrals,
  percentile
) {
  wl_setup <- data.frame(
    months_waited_id = seq_along(renege_params) - 1,
    r = renege_params,
    service = lp_solution
  )

  waiting_list <- calc_wl_sizes(wl_setup, referrals = referrals)

  solution_time <- hist_percentile_calc(
    waiting_list,
    percentile = percentile,
    wlsize_col = "wlsize",
    time_col = "months_waited_id"
  )

  return(
    list(
      p1 = NA,
      time_p = solution_time,
      mu = sum(lp_solution),
      wlsize = sum(waiting_list$wlsize),
      waiting_list = waiting_list,
      niterations = NA
    )
  )
}

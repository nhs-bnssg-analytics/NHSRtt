#' calculate the capacity and renege parameters over the period of data per
#' month waited
#'
#' @param referrals data frame with two columns; period_id and referrals. This
#'   represents the count of referrals in each period
#' @param incompletes data from with three columns; period_id, months_waited_id,
#'   and incompletes. This represents the count of incomplete pathways by the
#'   number of months waited for each period
#' @param completes  data from with three columns; period_id, months_waited_id,
#'   and treatments. This represents the count of completed pathways by the
#'   number of months waited for each period
#' @param max_months_waited integer; the maximum number of months to group
#'   patients waiting times by for the analysis. Data are published up to 104
#'   weeks, so 24 is likely to be the maximum useful value for this argument.
#' @param redistribute_m0_reneges logical; should negative renege counts in the
#'   zero months waited stock be reassigned to referrals?
#' @param full_breakdown logical; include a full breakdown of monthly
#'   transitions by period. FALSE provides the parameters by months_waited_id
#'   only
#' @importFrom dplyr setdiff across summarise count filter
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' max_months <- 4
#' refs <- create_dummy_data(
#'   type = "referral",
#'   max_months_waited = max_months,
#'   number_periods = 6
#' )
#' incomp <- create_dummy_data(
#'   type = "incomplete",
#'   max_months_waited = max_months,
#'   number_periods = 6
#' )
#'
#' comp <- create_dummy_data(
#'   type = "complete",
#'   max_months_waited = max_months,
#'   number_periods = 6
#' )
#'
#' params <- calibrate_capacity_renege_params(
#'   referrals = refs,
#'   incompletes = incomp,
#'   completes = comp,
#'   max_months_waited = max_months,
#'   redistribute_m0_reneges = TRUE
#' )
calibrate_capacity_renege_params <- function(referrals, incompletes, completes,
                                             max_months_waited = 12,
                                             redistribute_m0_reneges,
                                             full_breakdown = FALSE) {

  # data checks
  # redistribute_m0_reneges
  if (!is.logical(redistribute_m0_reneges))
    stop("adjust_renege_param must be TRUE or FALSE")

  if (is.na(redistribute_m0_reneges))
    stop("adjust_renege_param must be TRUE or FALSE")

  # max_months_waited
  if (!is.numeric(max_months_waited))
    stop("max_months_waited must be numeric")

  if (length(max_months_waited) != 1)
    stop("max_months_waited must be length 1")

  # checking names
  # referrals
  if (length(dplyr::setdiff(names(referrals), c("period_id", "referrals")) != 0))
    stop("the field names for referrals should be period_id and referrals")

  # completes
  if (length(dplyr::setdiff(names(completes), c("period_id", "months_waited_id", "treatments")) != 0))
    stop("the field names for completes should be period_id, months_waited_id and treatments")

  # incompletes
  if (length(dplyr::setdiff(names(incompletes), c("period_id", "months_waited_id", "incompletes")) != 0))
    stop("the field names for incompletes should be period_id, months_waited_id and incompletes")

  # checking dimensions
  if (nrow(referrals |> dplyr::count(period_id) |> dplyr::filter(n > 1)) > 0)
    stop("period_id is repeated in referrals data")

  if (nrow(completes |> dplyr::count(period_id, months_waited_id) |> dplyr::filter(n > 1)) > 0)
    stop("repeated combinations of period_id and months_waited_id in completes data")

  if (nrow(incompletes |> dplyr::count(period_id, months_waited_id) |> dplyr::filter(n > 1)) > 0)
    stop("repeated combinations of period_id and months_waited_id in incompletes data")


  if (nrow(referrals) != length(unique(completes[["period_id"]])))
    stop("referrals and completes should have the same number of period_ids")

  if (!identical(dim(completes), dim(incompletes)))
    stop("completes and incompletes should have the same dimensions")


  # check for missing time periods within data
  expected_period_ids <- seq(
    from = min(
      referrals[["period_id"]],
      completes[["period_id"]],
      incompletes[["period_id"]]
    ),
    to = max(
      referrals[["period_id"]],
      completes[["period_id"]],
      incompletes[["period_id"]]
    ),
    by = 1
  )

  # check all periods
  if (length(dplyr::setdiff(expected_period_ids, unique(referrals[["period_id"]]))) > 0)
    stop("There is a missing period_id from the referrals, completes and incompletes data")

  transitions <- calculate_timestep_transitions(
    referrals = referrals,
    incompletes = incompletes,
    completes = completes,
    max_months_waited = max_months_waited
  )

  # create cross join of period ids and number of months waited
  reneg_cap <- expand.grid(
    period_id = seq(
      from = min(transitions[["period_id"]]),
      to = max(transitions[["period_id"]]),
      by = 1
    ),
    months_waited_id = seq(
      from = min(transitions[["months_waited_id"]], na.rm = TRUE),
      to = max(transitions[["months_waited_id"]], na.rm = TRUE),
      by = 1
    )
  ) |>
  # join onto the reneg_cap tibble
    left_join(
      transitions,
      by = join_by(
        months_waited_id,
        period_id
      )
    ) |>
    mutate(
      reneges = .data$node_inflow -
        .data$treatments - .data$waiting_same_node
    )


  # redistribute negative reneges in month 0 to referrals if required
  if (isTRUE(redistribute_m0_reneges)) {
    reneg_cap <- reneg_cap |>
      mutate(
        node_inflow = case_when(
          .data$months_waited_id == 0 & .data$reneges < 0 ~ .data$node_inflow + abs(.data$reneges),
          .default = .data$node_inflow
        ),
        reneges = case_when(
          .data$months_waited_id == 0 & .data$reneges < 0 ~ 0,
          .default = .data$reneges
        )
      )
  }


  # timestep calcs of reneges and capacity parameters
  reneg_cap <- reneg_cap |>
    mutate(
      renege_param = .data$reneges / .data$node_inflow,
      capacity_param = .data$treatments / .data$node_inflow
    )

  if (!isTRUE(full_breakdown)) {
    reneg_cap <- reneg_cap |>
      summarise(
        across(
          c(renege_param, capacity_param),
          ~ mean(.x, na.rm = TRUE)
        ),
        .by = months_waited_id
      )

    if (any(reneg_cap |> pull(.data$renege_param) < 0))
      warning("negative renege parameters present, investigate raw data")

    if (any(reneg_cap |> pull(.data$capacity_param) < 0))
      warning("negative capacity parameters present, investigate raw data")
  }

  return(reneg_cap)
}

#' Apply the months waited parameters for renege and capacity to projections and
#' capacity and referrals If needed, or if validating your parameters, include
#' projections for incomplete pathways per period
#'
#' @param capacity_projections numeric; vector of projections for capacity for
#'   each time step. This must be the same length as referrals_projections
#' @param referrals_projections numeric; vector of projections for reneges for
#'   each time step. This must be the same length as capacity_projections
#' @param incomplete_pathways tibble; two column data frame or tibble, with
#'   fields called months_waited_id (taking values 0 to the maximum months
#'   waited group of interest), and incompletes (the count of the number of
#'   incomplete pathways) representing the count of incomplete pathways at
#'   timestep 0 (to initialise the model with)
#' @param renege_capacity_params tibble; three column data frame or tibble, with
#'   fields called months_waited_id (taking values 0 to the maximum months
#'   waited group of interest), and fields called capacity_param and
#'   renege_param, which are outputs from the function
#'   \code{calibrate_capacity_renege_params()}
#' @param max_months_waited integer; the maximum number of months to group
#'   patients waiting times by for the analysis. Data are published up to 104
#'   weeks, so 24 is likely to be the maximum useful value for this argument.
#'
#' @importFrom rlang .data
#' @importFrom dplyr distinct bind_rows left_join tibble join_by mutate
#'   case_when summarise select
#' @importFrom tidyr replace_na
#'
#' @return a tibble with fields for period_id, months_waited_id,
#'   calculated_treatments, reneges, incompletes and input_treatments
#' @export
#'
#' @examples
#' max_months <- 4
#' refs <- create_dummy_data(
#'   type = "referral",
#'   max_months_waited = max_months,
#'   number_periods = 6
#' )
#' incomp <- create_dummy_data(
#'   type = "incomplete",
#'   max_months_waited = max_months,
#'   number_periods = 6
#' )
#'
#' comp <- create_dummy_data(
#'   type = "complete",
#'   max_months_waited = max_months,
#'   number_periods = 6
#' )
#'
#' params <- calibrate_capacity_renege_params(
#'   referrals = refs,
#'   incompletes = incomp,
#'   completes = comp,
#'   max_months_waited = max_months,
#'   redistribute_m0_reneges = TRUE
#' )
#'
#' set.seed(3)
#' future_capacity <- sample(300:500, 4, replace = TRUE)
#' future_referrals <- sample(300:500, 4, replace = TRUE)
#' incompletes_t0 <- dplyr::tibble(
#'   months_waited_id = c(0, seq_len(max_months)),
#'   incompletes = sample(
#'     100:200,
#'     length(c(0, seq_len(max_months))),
#'     replace = TRUE
#'    )
#' )
#'
#' projections <- apply_params_to_projections(
#'   capacity_projections = future_capacity,
#'   referrals_projections = future_referrals,
#'   incomplete_pathways = incompletes_t0,
#'   renege_capacity_params = params,
#'   max_months_waited = max_months
#' )
apply_params_to_projections <- function(capacity_projections, referrals_projections,
                                        incomplete_pathways = NULL, renege_capacity_params,
                                        max_months_waited) {

  # check lengths of inputs
  if (length(capacity_projections) != length(referrals_projections))
    stop("capacity_projections and referrals_projections must be the same length")

  # check numeric inputs for max_months_waited
  if (!is.numeric(max_months_waited))
    stop("max_months_waited must be an integer")


  # check field names
  if (length(setdiff(names(renege_capacity_params), c("months_waited_id", "renege_param", "capacity_param"))) > 0)
    stop("renege_capacity_params must have the column names: months_waited_id, renege_param and capacity_param")

  if (!is.null(incomplete_pathways)) {
    # check the number of rows are less than or equal to the number of months
    # waited of interest
    if (nrow(incomplete_pathways) > max_months_waited + 1) {
      stop("incomplete_pathways must have nrow less than or equal to max_months_waited + 1")
    }

    # check the column headers
    if (length(dplyr::setdiff(names(incomplete_pathways), c("months_waited_id", "incompletes"))) > 0) {
      stop("incomplete_pathways must have field names of 'months_waited_id' and 'incompletes'")
    }


    # make sure there is a value for every value of months_waited_id
    all_months_waited <- dplyr::tibble(
      months_waited_id = c(0, seq_len(max_months_waited))
    )

    incomplete_pathways <- dplyr::left_join(
      all_months_waited,
      incomplete_pathways,
      by = join_by(months_waited_id)
    ) |>
      dplyr::mutate(
        incompletes = tidyr::replace_na(
          .data$incompletes,
          0
        )
      )

  }

  projections <- tibble(
    period_id = numeric(0),
    months_waited_id = numeric(0),
    calculated_treatments = numeric(0),
    reneges = numeric(0),
    incompletes = numeric(0),
    input_treatments = numeric(0)
  )

  for (period in seq_len(length(referrals_projections))) {
    transitions <- incomplete_pathways |>
      mutate(
        period_id = period,
        months_waited_id = case_when(
          .data$months_waited_id == max_months_waited ~ max_months_waited, # this prevents new bins appearing at the extent of the waiting period
          .default = .data$months_waited_id + 1
        )
      ) |>
      dplyr::summarise(
        node_inflow = sum(.data$incompletes),
        .by = c(
          period_id,
          months_waited_id
        )
      ) |>
      # add in referrals
      dplyr::bind_rows(
        dplyr::tibble(
          period_id = period,
          months_waited_id = 0,
          node_inflow = referrals_projections[period]
        )
      ) |>
      # add in the projections for  capacity
      mutate(
        capacity = capacity_projections[period]
      ) |>
      # add in the renege and capacity params
      left_join(
        renege_capacity_params,
        by = join_by(
          months_waited_id
        )
      ) |>
      mutate(
        reneges = .data$renege_param * .data$node_inflow,
        input_treatments = .data$capacity,
        capacity_numerator = .data$capacity_param * .data$node_inflow,
        capacity_denominator = sum(.data$capacity_numerator),
        calculated_treatments = .data$input_treatments *
          .data$capacity_numerator / .data$capacity_denominator,
        incompletes = .data$node_inflow -
          .data$calculated_treatments - .data$reneges,
        # redistribute the negative incompletes into the positive incompletes so
        # there are no negative incompletes in a timestep
        incompletes = redistribute_incompletes(.data$incompletes)
      )

    # recreate the incomplete_pathways tibble for the next period
    incomplete_pathways <- transitions |>
      distinct(
        .data$months_waited_id,
        .data$incompletes
      )

    # create output for timestep
    transitions <- transitions |>
      dplyr::select(
        "period_id",
        "months_waited_id",
        "calculated_treatments",
        "reneges",
        "incompletes",
        "input_treatments"
      )

    projections = bind_rows(
      projections,
      transitions
    )
  }

  return(projections)
}

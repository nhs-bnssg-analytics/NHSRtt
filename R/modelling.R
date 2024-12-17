#' calculate the capacity and renege parameters over the period of data per
#' month waited
#'
#' @param data
#' @param max_months_waited integer; the maximum number of months to group
#'   patients waiting times by for the analysis. Data are published up to 104
#'   weeks, so 24 is likely to be the maximum useful value for this argument.
#'
#' @return
#' @export
#'
#' @examples
calibrate_capacity_renege_params <- function(data, max_months_waited = 12) {

  data <- data |>
    select(!c("trust", "specialty")) |>
    mutate(
      months_waited = case_when(
        type == "Referrals" ~ "<1",
        .default = months_waited
      ),
      months_waited_id =  convert_months_waited_to_id(
        months_waited = months_waited,
        max_months_waited = max_months_waited
      ),
      months_waited = case_when(
        months_waited_id >= max_months_waited ~ paste0(">", max_months_waited),
        .default = as.character(months_waited_id)
      )
    ) |>
    summarise(
      value = sum(value),
      .by = c(
        period,
        type,
        months_waited,
        months_waited_id
      )
    ) |>
    mutate(
      period_id = interval(
        min(period),
        period) %/% months(1)
    )


  # check for missing time periods within data
  expected_period_ids <- seq(
    from = min(data[["period_id"]]),
    to = max(data[["period_id"]]),
    by = 1
  )

  processed_period_ids <- data |>
    pull(period_id) |>
    unique() |>
    sort()

  if (length(setdiff(expected_period_ids, processed_period_ids)) > 0)
    stop("At least one month is missing for trust")

  transitions <- calculate_timestep_transitions(
    data = data,
    max_months_waited = max_months_waited
  )

  # create cross join of period ids and number of months waited
  reneg_cap <- tidyr::expand_grid(
    period_id = seq_len(max(data[["period_id"]])),
    months_waited_id = seq(
      from = min(data[["months_waited_id"]], na.rm = TRUE),
      to = max(data[["months_waited_id"]], na.rm = TRUE),
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
    )


  # timestep calcs of reneges and capacity parameters
  reneg_cap <- reneg_cap |>
    mutate(
      Reneges = node_inflow - treatments - waiting_same_node,
      renege_param = Reneges / node_inflow,
      capacity_param = treatments / node_inflow
    ) |>
    select(
      "period_id",
      "months_waited_id",
      # "Reneges",
      "renege_param",
      "capacity_param"
    ) |>
    # take the mean parameter value per months waited, butadjust to zero if it
    # is negative (eg, not possible)
    summarise(
      across(
        c(renege_param, capacity_param),
        ~ ifelse(mean(.x) < 0, 0, mean(.x))
      ),
      .by = months_waited_id
    )

  return(reneg_cap)
}

validate_parameters <- function(data, max_months_waited = 12, capacity_renege_params) {

  transitions <- calculate_timestep_transitions(
    data = data,
    max_months_waited = max_months_waited
  ) |>
    dplyr::mutate(
      Reneges = node_inflow - treatments - waiting_same_node,
      renege_param = Reneges / node_inflow,
      capacity_param = treatments / node_inflow
    ) |>
    dplyr::select(
      "period_id",
      "months_waited_id",
      "renege_param",
      "capacity_param"
    ) |>
    pivot_longer(
      cols = c(renege_param, capacity_param),
      names_to = "type",
      values_to = "value"
    ) |>
    mutate(
      meanval = mean(value),
      value = ifelse(
        value < 0,
        0,
        value),
      .by = c(
        months_waited_id,
        type
      )
    ) |>
    summarise(
      value = ifelse(
        unique(meanval) < 0, 0, mean(value)
      ),
      .by = c(
        months_waited_id,
        type
      )
    )

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
#'   waited group of interest), and fields called capacity_param and renege_param,
#'   which are outputs from the function
#'   \code{calibrate_capacity_renege_params()}
#' @param max_months_waited integer; the maximum number of months to group
#'   patients waiting times by for the analysis. Data are published up to 104
#'   weeks, so 24 is likely to be the maximum useful value for this argument.
#'
#' @return
#' @export
#'
#' @examples
apply_params_to_projections <- function(capacity_projections, referrals_projections,
                                        incomplete_pathways = NULL, renege_capacity_params,
                                        max_months_waited) {

  # check lengths of inputs
  if (length(capacity_projections) != length(referrals_projections))
    stop("capacity_projections and referrals_projections must be the same length")

  # check numeric inputs for max_months_waited
  if (!is.numeric(max_months_waited))
    stop("max_months_waited must be an integer")


  if (!is.null(incomplete_pathways)) {
    # check the number of rows are less than or equal to the number of months
    # waited of interest
    if (nrow(incomplete_pathways) > max_months_waited + 1) {
      stop("incomplete_pathways must have nrow less than or equal to max_months_waited + 1")
    }

    # check the column headers
    if (length(dplyr::setdiff(names(incomplete_pathways), c("months_waited_id", "incompletes"))) > 0) {
      stop("incomplte_pathways must have field names of 'months_waited_id' and 'incompletes'")
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
          incompletes,
          0
        )
      )

  }


  # projected_referrals_capacity <- tibble(
  #   capacity = capacity_projections#,
  #   # referrals = referrals_projections
  # ) |>
  #   mutate(
  #     period_id = seq_len(
  #       length(capacity_projections)
  #     )
  #   )

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
          months_waited_id == max_months_waited ~ max_months_waited, # this prevents new bins appearing at the extent of the waiting period
          .default = months_waited_id + 1
        )
      ) |>
      dplyr::summarise(
        node_inflow = sum(incompletes),
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
        reneges = renege_param * node_inflow,
        input_treatments = capacity,
        capacity_numerator = capacity_param * node_inflow,
        capacity_denominator = sum(capacity_numerator),
        calculated_treatments = input_treatments * capacity_numerator / capacity_denominator,
        incompletes = node_inflow - calculated_treatments - reneges,
        # redistribute the negative incompletes into the positive incompletes so
        # there are no negative incompletes in a timestep
        incompletes = redistribute_incompletes(incompletes)
      )


    # recreate the incomplete_pathways tibble for the next period
    incomplete_pathways <- transitions |>
      distinct(
        months_waited_id,
        incompletes
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

  # # timestep 1 has inflow of initial incompletes plus referrals
  # if (!is.null(incomplete_pathways)) {
  #   incomplete_into_timestep_1 <- incomplete_pathways |>
  #     mutate(
  #       period_id = 1,
  #       months_waited_id = case_when(
  #         months_waited_id == max_months_waited ~ max_months_waited, # this prevents new bins appearing at the extent of the waiting period
  #         .default = months_waited_id + 1
  #       )
  #     ) |>
  #     dplyr::summarise(
  #       node_inflow = sum(value),
  #       .by = c(
  #         period_id,
  #         months_waited_id
  #       )
  #     )
  # } else {
  #   incomplete_into_timestep_1 <- tibble(
  #     period_id = 1,
  #     months_waited_id = seq_len(max_months_waited),
  #     node_inflow = 0
  #   )
  # }
  #
  # referrals <- projected_referrals_capacity |>
  #   dplyr::filter(
  #     period_id == 1
  #   ) |>
  #   dplyr::select(
  #     "period_id",
  #     node_inflow = "referrals"
  #   ) |>
  #   mutate(
  #     months_waited_id = 0
  #   )
  #
  # transitions <- bind_rows(
  #   incomplete_into_timestep_1,
  #   referrals
  # )

  # calculate completes and reneges from the input parameters

  # remaining patients are incompletes again, and are provided to the next timestep along with input referrals
}

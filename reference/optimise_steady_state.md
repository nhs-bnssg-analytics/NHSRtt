# Identify the steady-state solution with the closest treatment capacity or renege rate to a given target

This function performs either a linear programming or binary search
method to identify a solution that satisies multiple constraints (1,
arrivals = departures, 2, percentile on waiting list waiting within
given time is equal to given value, 3, renege rate is equal to given
value)

## Usage

``` r
optimise_steady_state(
  referrals,
  target,
  renege_params,
  target_time = 4 + (68/487),
  percentile = 0.92,
  method = c("lp", "bs"),
  s_given = NULL,
  bs_tolerance = target * 0.05,
  bs_max_iterations = 10
)
```

## Arguments

- referrals:

  numeric of length 1; value representing the steady-state number of
  referrals.

- target:

  Numeric. The desired renege rate to be achieved. The value must be
  between 0 and 1 and represents the proportion of all departures that
  are reneges.

- renege_params:

  numeric; proportion (0 ≤ r_(m) &leq; 1) of waiting patients reneging
  in the m-th month of waiting, where r₂ = 0.1 refers to 10% of those in
  the second month of waiting reneging.

- target_time:

  numeric of length 1; the target waiting time, in months, to achieve
  the specified percentile.

- percentile:

  Numeric value between 0 and 1 indicating the desired percentile
  (default is 0.92).

- method:

  character length 1; "lp" for linear programming, or "bs" for binary
  search

- s_given:

  numeric vector, length equal to `length(renege_params)`; values must
  be between 0 and 1 and must represent the proportion of total
  treatments that are applied to each compartment. This is used as a
  constraint in the optimisation and the output should follow the same
  profile

- bs_tolerance:

  Numeric. Acceptable deviation from \`target\` for convergence.
  Defaults to 10% of \`target\`.

- bs_max_iterations:

  Integer. Maximum number of iterations for the binary search. Defaults
  to 10.

## Value

A list containing:

- p1:

  The value of `p1` found to achieve the target waiting time.

- time_p:

  The interpolated waiting time at the specified percentile.

- mu:

  The total number of treatments required when the solution is found.

- wlsize:

  The total size of the waiting list when the solution is found.

- waiting_list:

  The data frame with historical data formatted and waiting list sizes
  calculated.

- niterations:

  The number of iterations performed in the binary search.

- status:

  Converged indicates a solution was found, and Not converged indicates
  no solution was found.

- method:

  The method the solution was identified.

## Examples

``` r
if (FALSE) { # \dontrun{
optimise_steady_state(
  referrals = 100,
  target = 0.1,
  renege_params = runif(24),
  method = "lp",
  s_given = c(0.4, 0.2, 0.1, rep(0.05, 6), rep(0, 15))
)

optimise_steady_state(
  referrals = 100,
  bs_target = 0.2,
  renege_params = runif(24),
  method = "bs"
)
} # }
```

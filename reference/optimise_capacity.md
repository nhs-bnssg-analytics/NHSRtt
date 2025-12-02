# Optimise the capacity profile for projections

Optimise the capacity profile for projections

## Usage

``` r
optimise_capacity(
  t_1_capacity,
  referrals_projections,
  incomplete_pathways,
  renege_capacity_params,
  target,
  target_bin,
  capacity_profile = "linear_change",
  surplus_treatment_redistribution_method = "evenly",
  tolerance,
  max_iterations = 50
)
```

## Arguments

- t_1_capacity:

  numeric; a single value for the capacity for the first time period of
  the projected time period

- referrals_projections:

  numeric; a vector for the number of referrals for each period in the
  projected time period

- incomplete_pathways:

  tibble; two column data frame or tibble, with fields called
  months_waited_id (taking values 0 to the maximum months waited group
  of interest), and incompletes (the count of the number of incomplete
  pathways) representing the count of incomplete pathways at timestep 0
  (to initialise the model with)

- renege_capacity_params:

  tibble; three column data frame or tibble, with fields called
  months_waited_id (taking values 0 to the maximum months waited group
  of interest), and fields called capacity_param and renege_param, which
  are outputs from the function
  [`calibrate_capacity_renege_params()`](https://nhs-bnssg-analytics.github.io/NHSRtt/reference/calibrate_capacity_renege_params.md)

- target:

  string length 1; can be either a percentage point change, eg, "~-5%"
  or a percent value, eg, "5%". This refers to percentage of patients on
  the waiting list in the `target_bin` or higher waiting times. Note,
  this is the opposite of the NHS RTT targets, which are a proportion of
  patients on the waiting list that are below the `target_bin`

- target_bin:

  numeric length 1; the bin that the target refers to. It must be less
  than or equal to the max_months_waited value

- capacity_profile:

  string, one of "linear_change" or "flat"; determines how the capacity
  counts vary into the future. Linear change means that the first point
  is held stationary at the value of `t_1_capacity` and the end point is
  varied, with a linear interpolation between the two points. Flat means
  that capacity remains constant into the future

- surplus_treatment_redistribution_method:

  string; one of "none", "evenly" or "prioritise_long_waiters"; should
  cases where the counts of reneges and treatments exceed the counts of
  people waiting be redistributed, and if so, which method should be
  used

- tolerance:

  numeric length 1; the tolerance used to compare the absolute error
  with in the max_months_waited bin to determine convergence. The
  absolute error is calculated on the proportion in the
  max_months_waited bin relative to the total waiting (even if a
  non-percentage target is used)

- max_iterations:

  numeric; the maximum number of iterations to test for convergence
  before providing a warning and an invalid number

## Value

A capacity multiplier representing that is applied to the `t_1_capacity`
input to achieve the desired target by the end of the projection period.
Where the `capacity_profile` is 'linear_change', this represent a linear
growth in capacity from t₁ to t₁₃, where t₁ is equal to `t_1_capacity`
and t₁₃ is `t_1_capacity` multiplied by the output of the function.

If `capacity_profile` is 'flat', then the projected capacity is simply
`t_1_capacity` multiplied by the output of the function for the whole of
the projected period.

The name of the returned object provides an indication of whether the
optimiser converged.

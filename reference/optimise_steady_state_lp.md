# Linear programming solution to steady state optimisation

Linear programming solution to steady state optimisation

## Usage

``` r
optimise_steady_state_lp(
  renege_params,
  referrals,
  gamma,
  percentile,
  theta,
  s_given
)
```

## Arguments

- renege_params:

  numeric; proportion (0 ≤ r_(m) &leq; 1) of waiting patients reneging
  in the m-th month of waiting, where r₂ = 0.1 refers to 10% of those in
  the second month of waiting reneging.

- referrals:

  numeric of length 1; value representing the steady-state number of
  referrals.

- gamma:

  numeric vector, length equal to `length(renege_params)`; must take
  values between 0 and 1. Vector represents the proportion of the
  corresponding compartment that the \`percentile\` target refers. For
  example, if the data has five compartments and the target percentile
  was at exactly 3.5 months, the corresponding vector would be c(1, 1,
  1, 0.5, 0, 0)

- percentile:

  numeric value between 0 and 1 indicating the desired percentile
  (default is 0.92).

- theta:

  numeric value of length 1, must be a value between 0 and 1 which is
  equal to the proportion of all departures that are reneges

- s_given:

  numeric vector, length equal to `length(renege_params)`; values must
  be between 0 and 1 and must represent the proportion of total
  treatments that are applied to each compartment. This is used as a
  constraint in the optimisation and the output should follow the same
  profile

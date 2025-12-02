# Configure Removals Data Frame

Constructs a data frame representing the configuration of removals,
including time, renge rates, and potential services.

## Usage

``` r
initialise_removals(renege_params, p1, mu)
```

## Arguments

- renege_params:

  numeric; proportion (0 ≤ r_(m) &leq; 1) of waiting patients reneging
  in the m-th month of waiting, where r₂ = 0.1 refers to 10% of those in
  the second month of waiting reneging.

- p1:

  numeric of length 1; the probability parameter for the geometric
  distribution (probability of failure in each period).

- mu:

  numeric of length 1; total capacity to be applied over the geometric
  distribution

## Value

A data frame with columns: `months_waited_id`, `r`, and `service`.
`months_waited_id` is the lower bound of the number of months waited.
`r` is the renege rate for each month waited compartment. `service` is
the initial capacity provided, applied across the compartments using the
geometric distribution defined by `p1`.

## See also

[`geom_fn_s`](https://nhs-bnssg-analytics.github.io/NHSRtt/reference/geom_fn_s.md)

## Examples

``` r
if (FALSE) { # \dontrun{
set.seed(123)
renege_params <- runif(24)
initialise_removals(renege_params, p1 = 0.5, mu = 50)
} # }
```

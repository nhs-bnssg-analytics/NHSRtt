# Calculate Geometric Survival Probabilities

Computes the survival probabilities for a geometric distribution over a
specified number of months.

## Usage

``` r
geom_fn_s(p1, max_num_months = 24)
```

## Arguments

- p1:

  numeric of length 1; the probability parameter for the geometric
  distribution (probability of failure in each period).

- max_num_months:

  integer of length 1; the maximum number of months to calculate
  survival probabilities for, between 1 and 24. Default is 24.

## Value

A numeric vector of survival probabilities for each month.

## Examples

``` r
if (FALSE) { # \dontrun{
# Calculate the geometric survival probabilities for p1 = 0.2, up to 24 months
geom_fn_s(p1 = 0.2, max_num_months = 24)

# Calculate for a single month
geom_fn_s(p1 = 0.7, max_num_months = 1)
} # }
```

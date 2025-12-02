# Find the value of p1 that achieves a target waiting time percentile

Uses a binary search algorithm to find the value of `p1` (the proportion
parameter) that achieves a specified target waiting time (`target_time`)
at a given percentile (hardcoded as 92nd percentile) of the cumulative
waiting list size, given a treatment capacity in the first month
(`mu_1`) and other parameters.

## Usage

``` r
find_p(
  target_time = 4 + (68/487),
  renege_params,
  mu_1,
  p1_lower = 0.1,
  p1_upper = 0.85,
  tolerance = 0.001,
  referrals,
  percentile = 0.92,
  max_iterations = 15
)
```

## Arguments

- target_time:

  numeric of length 1; the target waiting time, in months, to achieve
  the specified percentile.

- renege_params:

  numeric; proportion (0 ≤ r_(m) &leq; 1) of waiting patients reneging
  in the m-th month of waiting, where r₂ = 0.1 refers to 10% of those in
  the second month of waiting reneging.

- mu_1:

  numeric of length 1; treatment capacity in the first month.

- p1_lower:

  numeric of length 1; lower bound for the binary search of `p1`. Must
  be between 0 and 1 and less than `p1_upper`. Defaults to 0.1.

- p1_upper:

  numeric of length 1; upper bound for the binary search of `p1`. Must
  be between 0 and 1 and more than `p1_lower`. Defaults to 0.85.

- tolerance:

  numeric of length 1; tolerance for the binary search stopping
  criterion. The unit is months.

- referrals:

  numeric of length 1; value representing the steady-state number of
  referrals.

- percentile:

  Numeric value between 0 and 1 indicating the desired percentile
  (default is 0.92).

- max_iterations:

  numeric of length 1; maximum number of iterations to search for a
  valid result

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
  no solution was found

## Details

The function repeatedly uses the removals table (from
[`initialise_removals()`](https://nhs-bnssg-analytics.github.io/NHSRtt/reference/initialise_removals.md))
and calculates waiting list sizes for candidate values of `p1`, using
binary search to home in on the value that achieves the target waiting
time at the required percentile. The search stops when the difference
between upper and lower bounds is less than `tolerance`, or when the
calculated waiting time is within `tolerance` of the target.

## See also

[`initialise_removals`](https://nhs-bnssg-analytics.github.io/NHSRtt/reference/initialise_removals.md),
[`calc_wl_sizes`](https://nhs-bnssg-analytics.github.io/NHSRtt/reference/calc_wl_sizes.md),
[`hist_percentile_calc`](https://nhs-bnssg-analytics.github.io/NHSRtt/reference/hist_percentile_calc.md)

## Examples

``` r
find_p(
  target_time = 4 + (68 / 487),
  renege_params = c(0.04, 0.04, 0.03, 0.01, 0.02, 0.02, 0.01),
  mu_1 = 2651.227,
  p1_lower = 0.1,
  p1_upper = 0.85,
  tolerance = 0.001,
  referrals = 12000,
  percentile = 0.92
)
#> $p1
#> [1] 0.8200623
#> 
#> $time_p
#> [1] 4.139275
#> 
#> $mu
#> [1] 10836.18
#> 
#> $wlsize
#> [1] 24630.12
#> 
#> $waiting_list
#>   months_waited_id    r   service     sigma    wlsize
#> 1                0 0.04 2651.2270 2651.2270 8868.7730
#> 2                1 0.04 2174.1712 2174.1712 6339.8509
#> 3                2 0.03 1782.9557 1782.9557 4366.6996
#> 4                3 0.01 1462.1347 1462.1347 2860.8979
#> 5                4 0.02 1199.0415 1199.0415 1604.6385
#> 6                5 0.02  983.2887  983.2887  589.2571
#> 7                6 0.01 4377.9222  583.3645    0.0000
#> 
#> $niterations
#> [1] 13
#> 
#> $status
#> [1] "Converged"
#> 
```

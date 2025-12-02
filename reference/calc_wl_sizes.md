# Calculate Waiting List Sizes by Time Waited

This function applies the available capacity, defined by the geometric
distribution and the total capacity, and updates a data frame
representing the waiting list, by time waited, by calculating the
waiting list size (`wlsize`) and the number of serviced items (`sigma`)
for each time compartment waited.

## Usage

``` r
calc_wl_sizes(wl_removals, referrals)
```

## Arguments

- wl_removals:

  A data frame containing the renege rate and potential number of
  services per waiting compartment. It must include columns `r`
  (removal/renege rate), and `service` (number of potential services).

- referrals:

  numeric of length 1; value representing the steady-state number of
  referrals.

## Value

A data frame with calculated `wlsize` and `sigma` columns for each row.

## Details

For each row in `wl_removals`, the function computes the new waiting
list size based on the previous size, the removal rate, and the number
serviced. If the computed size is positive, it is assigned to `wlsize`
and `sigma` is set to the serviced value. If not, `wlsize` is set to
zero and `sigma` is set to the floored value of the previous size after
removal.

## Examples

``` r
if (FALSE) { # \dontrun{
wl_removals <- data.frame(r = c(0.1, 0.2), service = c(5, 3))
calc_wl_sizes(wl_removals, referrals = 50)
} # }
```

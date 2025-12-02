# Calculate the Time at a Given Percentile of a Weighted Distribution

Computes the interpolated time at which a specified percentile of the
cumulative sum of a weight column (e.g., waiting list size) is reached,
based on a sorted data frame.

## Usage

``` r
hist_percentile_calc(
  wl_structure,
  percentile = 0.92,
  wlsize_col = "wlsize",
  time_col = "months_waited_id"
)
```

## Arguments

- wl_structure:

  A data frame containing at least the columns specified by `wlsize_col`
  and `time_col`.

- percentile:

  Numeric value between 0 and 1 indicating the desired percentile
  (default is 0.92).

- wlsize_col:

  Character string specifying the name of the column representing the
  weights (default is `wlsize`).

- time_col:

  Character string specifying the name of the column representing the
  time variable (default is `time`).

## Value

A numeric value representing the interpolated time at which the
cumulative sum of the weights reaches the specified percentile. The
value is the number of months waited where the percentile value is met.
Despite the requirement that the `time_col` starts at 0, a value of 0.5
means the percentile value is met after waiting 0.5 months

## Details

The function sorts the data frame by the time column, computes the
cumulative sum of the weights, and determines the time at which the
cumulative sum first meets or exceeds the specified percentile of the
total sum. If the percentile falls within a bin, linear interpolation is
used to estimate the precise time.

## Examples

``` r
if (FALSE) { # \dontrun{
wl_structure <- data.frame(time = 1:10, wlsize = c(5, 3, 8, 2, 7, 4, 6, 1, 9, 2))
hist_percentile_calc(wl_structure, percentile = 0.9)
} # }
```

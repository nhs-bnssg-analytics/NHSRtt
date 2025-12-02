# convert the string version of months waited to the numeric id version

convert the string version of months waited to the numeric id version

## Usage

``` r
convert_months_waited_to_id(months_waited, max_months_waited)
```

## Arguments

- months_waited:

  string; vector with format, by example "2-3"

- max_months_waited:

  integer; the maximum number of months to group patients waiting times
  by for the analysis. Data are published up to 104 weeks, so 24 is
  likely to be the maximum useful value for this argument.

## Examples

``` r
mnths_waited <- c("<1", "1-2", "2-3", "3-4", "4-5", "5+")
convert_months_waited_to_id(
  months_waited = mnths_waited,
  max_months_waited = 3
)
#> [1] 0 1 2 3 3 3
```

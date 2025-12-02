# Returns the date of the latest available data

Returns the date of the latest available data

## Usage

``` r
latest_rtt_date(
  url = "https://www.england.nhs.uk/statistics/statistical-work-areas/rtt-waiting-times/"
)
```

## Arguments

- url:

  string; url of the NHS Referral to Treatment (RTT) Waiting Times

## Value

the latest date of available data

## Examples

``` r
latest_rtt_date()
#> [1] "2025-09-30"
```

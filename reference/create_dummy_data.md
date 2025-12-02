# Create a set of dummy data to put through the functions of the package

Create a set of dummy data to put through the functions of the package

## Usage

``` r
create_dummy_data(
  type,
  max_months_waited,
  number_periods,
  referral_values = 9000:12000,
  max_incompletes = 10000,
  max_treatments = 500,
  seed = 123
)
```

## Arguments

- type:

  string; one of "referral", "incomplete" or "complete"

- max_months_waited:

  integer; the maximum number of months to group patients waiting times
  by for the analysis. Data are published up to 104 weeks, so 24 is
  likely to be the maximum useful value for this argument.

- number_periods:

  integer; the intended number of periods in the dataset

- referral_values:

  integer; vector of values that are sampled from for the count of
  referrals at each time step

- max_incompletes:

  integer; the maximum number of incomplete pathways possible in one
  time step

- max_treatments:

  integer; the maximum number of treatments possible

- seed:

  seed to generate the random data from

## Value

a tibble whose columns depend on the type input. If type is "referral"
then it will have two fields, period_id and referrals. If type is
"complete" or "incomplete", the fields will be period_id,
months_waited_id and treatments/incompletes, depending on the type value

## Examples

``` r
create_dummy_data(
  type = "referral",
  max_months_waited = 4,
  number_period = 6
)
#> # A tibble: 7 × 2
#>   period_id referrals
#>       <dbl>     <int>
#> 1         0     11462
#> 2         1     11510
#> 3         2     11226
#> 4         3      9525
#> 5         4      9194
#> 6         5     11985
#> 7         6     10841
```

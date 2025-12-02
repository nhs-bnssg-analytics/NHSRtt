# Calculates the flow from each stock at each timestep

Calculates the flow from each stock at each timestep

## Usage

``` r
calculate_timestep_transitions(
  referrals,
  incompletes,
  completes,
  max_months_waited
)
```

## Arguments

- referrals:

  data frame with two columns; period_id and referrals. This represents
  the count of referrals in each period

- incompletes:

  data from with three columns; period_id, months_waited_id, and
  incompletes. This represents the count of incomplete pathways by the
  number of months waited for each period

- completes:

  data from with three columns; period_id, months_waited_id, and
  treatments. This represents the count of completed pathways by the
  number of months waited for each period

- max_months_waited:

  integer; the maximum number of months to group patients waiting times
  by for the analysis. Data are published up to 104 weeks, so 24 is
  likely to be the maximum useful value for this argument

## Value

a tibble with fields for months_waited_id, period_id, node_inflow,
waiting_same_node and treatments. These represent all the counts being
moved on a single time step

## Details

for each timestep, the stock is calculated as the current count of
patients waiting at that timestep plus the inflow (via referrals or
incomplete pathways from previous timestep) minus outflow (pathways have
been completed or reneges, which are deduced from the other flows). This
stock is divided into "months waited", and patients with incomplete
pathways at the end of the timestep are incremented up to an additional
month's waiting time

For each of the data frames supplied, the period_id and the
months_waited_id fields should be numeric/integers. period_id is an
integer representing the chronology of the data, and months_waited_id is
a numeric representation of the months waited, where 0 represents 0 to 1
month, and 5 represents 5 to 6 months.

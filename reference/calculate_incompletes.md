# Calculate the number of incomplete pathways in a bin from the inflow, reneges and treatments. Following the calculations, apply an optional redistribution of the occasions where incompletes are negative (eg, more treatments have been performed than there were people).

Calculate the number of incomplete pathways in a bin from the inflow,
reneges and treatments. Following the calculations, apply an optional
redistribution of the occasions where incompletes are negative (eg, more
treatments have been performed than there were people).

## Usage

``` r
calculate_incompletes(inflow, reneges, treatments, redistribution_method)
```

## Arguments

- inflow:

  numeric; vector of count of patients moving into a bin in a period in
  order of lowest to highest bin

- reneges:

  numeric; vector of count of patients reneging from a bin in a period
  in order of lowest to highest bin

- treatments:

  numeric; vector of count of patients being treated from a bin in a
  period in order of lowest to highest bin

- redistribution_method:

  string; one of "none", "evenly" or "prioritise_long_waiters"

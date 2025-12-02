# Redistribute cases where there are more treatments and reneges than people in a particular bin for a particular period. This function chooses the redistribute the surplus treatment to the people waiting the longest first

Redistribute cases where there are more treatments and reneges than
people in a particular bin for a particular period. This function
chooses the redistribute the surplus treatment to the people waiting the
longest first

## Usage

``` r
redistribute_incompletes_optimally(incomplete_counts)
```

## Arguments

- incomplete_counts:

  numeric; vector of count of patients with an incomplete pathway within
  a bin in a period in order of lowest to highest bin

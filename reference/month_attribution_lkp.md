# creates table of weights to apply to dates that represent the end of a week. These weights correspond tot he proportion of that week's counts that fall into the month that the "week end date" occurs, and the proportion of the counts that fall into the previous month

rtt data is published monthly and represents a snapshot of counts at the
end of the given month. The weekly counts are provided within the data.
To convert the weekly counts to monthly counts we must sum the weekly
counts within each month. Some weeks will fall over month ends, so this
function identifies those, and calculates the number of days those weeks
fall within each month. These days are then used to calculate a
weighting, which is the output of the function.

## Usage

``` r
month_attribution_lkp(week_end_dates)
```

## Arguments

- week_end_dates:

  vector of dates that represent the final date of the week in
  consideration (this doesn't have to be a specific day of the week as
  the reference point for this purpose is the end of the month which can
  fall on any day of the week)

## Value

a tibble of week_end, wait_start_month, month_weight where week_end are
the dates provided, wait_start_month is the start of the month that the
waiting period began, and month_weight is a value between 0 and 1 to
apply to the counts within that week

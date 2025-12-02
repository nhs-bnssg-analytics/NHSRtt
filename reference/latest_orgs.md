# Returns a table of organisations and how they map to one another from the latest available RTT file

Returns a table of organisations and how they map to one another from
the latest available RTT file

## Usage

``` r
latest_orgs(
  url = "https://www.england.nhs.uk/statistics/statistical-work-areas/rtt-waiting-times/"
)
```

## Arguments

- url:

  string; url of the NHS Referral to Treatment (RTT) Waiting Times

## Value

a tibble with fields for "NHS Region Code", "NHS Region Name",
"Provider", "Parent Org Code", "Provider Parent Name", "Provider Org
Code", "Provider Org Name", "Commissioner", "Parent Org Code",
"Commissioner Parent Name", "Commissioner Org Code", "Commissioner Org
Name"

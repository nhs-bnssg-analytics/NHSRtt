# Download and tidy the referral to treatment data from the NHS Statistics webpage

Download and tidy the referral to treatment data from the NHS Statistics
webpage

## Usage

``` r
get_rtt_data(
  url = "https://www.england.nhs.uk/statistics/statistical-work-areas/rtt-waiting-times/",
  date_start = as.Date("2019-04-01"),
  date_end = Sys.Date(),
  trust_parent_codes = NULL,
  commissioner_parent_codes = NULL,
  commissioner_org_codes = NULL,
  trust_codes = NULL,
  specialty_codes = NULL,
  show_progress = FALSE
)
```

## Arguments

- url:

  string; url of the NHS Referral to Treatment (RTT) Waiting Times

- date_start:

  date; start date (earliest date is 1st May 2016, but the default is
  1st April 2019)

- date_end:

  date; end date (defaults to "today")

- trust_parent_codes:

  character; vector of trust parent codes to filter for (optional); eg
  "QE1" is NHS Lancashire and South Cumbria Integrated Care Board

- commissioner_parent_codes:

  character; vector of commissioner parent codes to filter for
  (optional); eg "QF7" is NHS South Yorkshire Integrated Care Board

- commissioner_org_codes:

  character; vector of commissioner org codes to filter for (optional);
  eg "15C" is the NHS Bristol, North Somerset and South Gloucestershire
  (sub-ICB location)

- trust_codes:

  character; vector of trust codes to filter for (optional); eg "R1A" is
  Herefordshire and Worcestershire Health and Care NHS Trust

- specialty_codes:

  character; vector of specialty codes to filter for (optional); eg
  "C_999" is "Total" specialties

- show_progress:

  logical; show progress of downloading and processing files. Defaults
  to false

## Value

a tibble with fields for trust_parent_org_code,
commissioner_parent_org_code, commissioner_org_code, trust, specialty,
period, type, months_waited and value

## Examples

``` r
if (FALSE) { # \dontrun{
monthly_rtt <- NHSRtt::get_rtt_data(
  date_start = as.Date("2024-10-01"),
  date_end = as.Date("2024-11-01"),
  show_progress = TRUE
)
} # }
```

# Read in and tidy the csv file

Read in and tidy the csv file

## Usage

``` r
tidy_file(
  csv_filepath,
  trust_parent_codes = NULL,
  commissioner_parent_codes = NULL,
  commissioner_org_codes = NULL,
  trust_codes = NULL,
  specialty_codes = NULL
)
```

## Arguments

- csv_filepath:

  string; file path to the file location

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

## Value

a tidy tibble

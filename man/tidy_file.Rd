% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\name{tidy_file}
\alias{tidy_file}
\title{Read in and tidy the csv file}
\usage{
tidy_file(
  csv_filepath,
  trust_parent_codes = NULL,
  commissioner_parent_codes = NULL,
  commissioner_org_codes = NULL,
  trust_codes = NULL,
  specialty_codes = NULL
)
}
\arguments{
\item{csv_filepath}{string; file path to the file location}

\item{trust_parent_codes}{character; vector of trust parent codes to filter
for (optional); eg "QE1" is NHS Lancashire and South Cumbria Integrated
Care Board}

\item{commissioner_parent_codes}{character; vector of commissioner parent
codes to filter for (optional); eg "QF7" is NHS South Yorkshire Integrated
Care Board}

\item{commissioner_org_codes}{character; vector of commissioner org codes to
filter for (optional); eg "15C" is the NHS Bristol, North Somerset and
South Gloucestershire (sub-ICB location)}

\item{trust_codes}{character; vector of trust codes to filter for (optional);
eg "R1A" is  Herefordshire and Worcestershire Health and Care NHS Trust}

\item{specialty_codes}{character; vector of specialty codes to filter for (optional);
eg "C_999" is "Total" specialties}
}
\value{
a tidy tibble
}
\description{
Read in and tidy the csv file
}

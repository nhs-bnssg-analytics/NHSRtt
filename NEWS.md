# NHSRtt 0.3.0

* provides user control over whether negative parameters are allowed from the `calibrate_capacity_renege_params()` function using the new `allow_negative_params` function

# NHSRtt 0.2.4

* steady state calculation available using the `find_p()` function

# NHSRtt 0.2.3

* includes latest_orgs() function

# NHSRtt 0.2.2

* exports the skew parameter function
* allows specification on where pivoting happens for skew parameter
* skew function provides two different skew methods
* adds `latest_rtt_date()`
* data import works on more months of historic data

# NHSRtt 0.2.1

* Data import specialty code bug fix

# NHSRtt 0.2.0

* data import now includes commissioner information
* `get_rtt_data()` stops using `type` as an input argument and all the data are returned in one call

# NHSRtt 0.1.5

* includes non-admitted data in get_rtt_data which was accidentally being excluded

# NHSRtt 0.1.4

* bug fixes and edge case management
* allows surplus capacity to be allocated to longest waiters rather than spreading it evenly

# NHSRtt 0.1.3

* includes optimiser function

# NHSRtt 0.1.2

* fixes calibration period bug where first period was removed unintentionally


# NHSRtt 0.1.1

* improves vignettes


# NHSRtt 0.1.0

* first release for testing with collaborators


# Package index

## All functions

- [`NHSRtt`](https://nhs-bnssg-analytics.github.io/NHSRtt/reference/NHSRtt.md)
  : NHSRtt: A package for modelling waiting times using a stock and flow
  method
- [`apply_parameter_skew()`](https://nhs-bnssg-analytics.github.io/NHSRtt/reference/apply_parameter_skew.md)
  : Pivot the parameters passed into the function so relationships
  between the parameters remain consistent but giving control to
  providing more or less focus on extreme bins
- [`apply_params_to_projections()`](https://nhs-bnssg-analytics.github.io/NHSRtt/reference/apply_params_to_projections.md)
  : Apply the parameters for renege and capacity (by months waited) to
  projections of capacity and referrals. If needed, or if validating
  your parameters, include the observed incomplete pathways by the
  number of months waited for the period prior to the period being
  projected (eg, a starting position)
- [`calc_wl_sizes()`](https://nhs-bnssg-analytics.github.io/NHSRtt/reference/calc_wl_sizes.md)
  : Calculate Waiting List Sizes by Time Waited
- [`calculate_incompletes()`](https://nhs-bnssg-analytics.github.io/NHSRtt/reference/calculate_incompletes.md)
  : Calculate the number of incomplete pathways in a bin from the
  inflow, reneges and treatments. Following the calculations, apply an
  optional redistribution of the occasions where incompletes are
  negative (eg, more treatments have been performed than there were
  people).
- [`calculate_timestep_transitions()`](https://nhs-bnssg-analytics.github.io/NHSRtt/reference/calculate_timestep_transitions.md)
  : Calculates the flow from each stock at each timestep
- [`calibrate_capacity_renege_params()`](https://nhs-bnssg-analytics.github.io/NHSRtt/reference/calibrate_capacity_renege_params.md)
  : calculate the capacity and renege parameters over the period of data
  per month waited
- [`convert_months_waited_to_id()`](https://nhs-bnssg-analytics.github.io/NHSRtt/reference/convert_months_waited_to_id.md)
  : convert the string version of months waited to the numeric id
  version
- [`create_dummy_data()`](https://nhs-bnssg-analytics.github.io/NHSRtt/reference/create_dummy_data.md)
  : Create a set of dummy data to put through the functions of the
  package
- [`find_p()`](https://nhs-bnssg-analytics.github.io/NHSRtt/reference/find_p.md)
  : Find the value of p1 that achieves a target waiting time percentile
- [`geom_fn_s()`](https://nhs-bnssg-analytics.github.io/NHSRtt/reference/geom_fn_s.md)
  : Calculate Geometric Survival Probabilities
- [`get_rtt_data()`](https://nhs-bnssg-analytics.github.io/NHSRtt/reference/get_rtt_data.md)
  : Download and tidy the referral to treatment data from the NHS
  Statistics webpage
- [`hist_percentile_calc()`](https://nhs-bnssg-analytics.github.io/NHSRtt/reference/hist_percentile_calc.md)
  : Calculate the Time at a Given Percentile of a Weighted Distribution
- [`initialise_removals()`](https://nhs-bnssg-analytics.github.io/NHSRtt/reference/initialise_removals.md)
  : Configure Removals Data Frame
- [`latest_orgs()`](https://nhs-bnssg-analytics.github.io/NHSRtt/reference/latest_orgs.md)
  : Returns a table of organisations and how they map to one another
  from the latest available RTT file
- [`latest_rtt_date()`](https://nhs-bnssg-analytics.github.io/NHSRtt/reference/latest_rtt_date.md)
  : Returns the date of the latest available data
- [`latest_rtt_file()`](https://nhs-bnssg-analytics.github.io/NHSRtt/reference/latest_rtt_file.md)
  : Returns the url for the latest available file
- [`month_attribution_lkp()`](https://nhs-bnssg-analytics.github.io/NHSRtt/reference/month_attribution_lkp.md)
  : creates table of weights to apply to dates that represent the end of
  a week. These weights correspond tot he proportion of that week's
  counts that fall into the month that the "week end date" occurs, and
  the proportion of the counts that fall into the previous month
- [`obtain_links()`](https://nhs-bnssg-analytics.github.io/NHSRtt/reference/obtain_links.md)
  : Identify links within a given url
- [`optimise_capacity()`](https://nhs-bnssg-analytics.github.io/NHSRtt/reference/optimise_capacity.md)
  : Optimise the capacity profile for projections
- [`optimise_steady_state()`](https://nhs-bnssg-analytics.github.io/NHSRtt/reference/optimise_steady_state.md)
  : Identify the steady-state solution with the closest treatment
  capacity or renege rate to a given target
- [`optimise_steady_state_lp()`](https://nhs-bnssg-analytics.github.io/NHSRtt/reference/optimise_steady_state_lp.md)
  : Linear programming solution to steady state optimisation
- [`redistribute_incompletes_evenly()`](https://nhs-bnssg-analytics.github.io/NHSRtt/reference/redistribute_incompletes_evenly.md)
  : Redistribute cases where there are more treatments and reneges than
  people in a particular bin for a particular period. This function
  chooses the redistribute the surplus treatment to the people waiting
  the longest first
- [`redistribute_incompletes_optimally()`](https://nhs-bnssg-analytics.github.io/NHSRtt/reference/redistribute_incompletes_optimally.md)
  : Redistribute cases where there are more treatments and reneges than
  people in a particular bin for a particular period. This function
  chooses the redistribute the surplus treatment to the people waiting
  the longest first
- [`tidy_file()`](https://nhs-bnssg-analytics.github.io/NHSRtt/reference/tidy_file.md)
  : Read in and tidy the csv file

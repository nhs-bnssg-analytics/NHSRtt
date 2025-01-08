# get_rtt_data error checks

    Code
      get_rtt_data(type = "completes", date_start = as.Date("2024-10-01"), date_end = as.Date(
        "2024-11-01"), show_progress = FALSE)
    Condition
      Error in `match.arg()`:
      ! 'arg' should be one of "complete", "incomplete", "referral"

# create_dummy_data functionality

    Code
      create_dummy_data(type = "completes", max_months_waited = 4, number_periods = 10,
        max_treatments = 50)
    Condition
      Error in `match.arg()`:
      ! 'arg' should be one of "referral", "complete", "incomplete"


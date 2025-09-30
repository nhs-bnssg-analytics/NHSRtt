test_that("month_attribution_lkp function tests", {
  week_ends <- seq(
    from = as.Date("2024-11-29"),
    to = as.Date("2024-12-08"),
    by = "days"
  )

  mnth_lkp <- month_attribution_lkp(
    week_end_dates = week_ends
  )

  expect_equal(
    week_ends,
    unique(
      mnth_lkp[["week_end"]]
    ),
    label = "dates are not the same between inputs and outputs"
  )

  expect_identical(
    names(mnth_lkp),
    c("week_end", "wait_start_month", "month_weight"),
    label = "names are expected"
  )

  expect_identical(
    dim(mnth_lkp),
    c(16L, 3L),
    label = "dimensions are expected"
  )
})


test_that("redistribute_incompletes_evenly works", {
  incompletes <- c(1, 2, 3, -1, 5, -4)
  expect_identical(
    sum(redistribute_incompletes_evenly(incompletes)),
    sum(incompletes),
    label = "total number of incompletes remains the same before and after the function is used"
  )

  expect_gte(
    redistribute_incompletes_evenly(incompletes) |>
      min(),
    0,
    label = "there are no negative values returned from the redistribute_incompletes_evenly function"
  )
})

test_that("redistribute_incompletes_optimally works", {
  inflow <- c(11, 20, 20, 12, 16)
  reneges <- c(6, 7, 7, 9, 9)
  treatments <- c(9, 9, 9, 9, 7)

  redistributed_incompletes <- calculate_incompletes(
    inflow = inflow,
    reneges = reneges,
    treatments = treatments,
    redistribution_method = "prioritise_long_waiters"
  )

  expect_identical(
    sum(redistributed_incompletes),
    sum(inflow - reneges - treatments),
    label = "total number of incompletes remains the same before and after the function is used"
  )

  inflow <- c(11, 20, 20, 12, 16)
  reneges <- c(6, 7, 7, 9, 9)
  treatments <- c(9, 9, 9, 2, 7)

  redistributed_incompletes <- calculate_incompletes(
    inflow = inflow,
    reneges = reneges,
    treatments = treatments,
    redistribution_method = "prioritise_long_waiters"
  )

  expect_identical(
    redistributed_incompletes,
    c(0, 4, 1, 0, 0),
    label = "treatments are redistributed consistently"
  )
})

test_that("redistribute_incompletes none works", {
  inflow <- c(11, 20, 20, 12, 16)
  reneges <- c(6, 7, 7, 9, 9)
  treatments <- c(9, 9, 9, 9, 7)

  redistributed_incompletes <- calculate_incompletes(
    inflow = inflow,
    reneges = reneges,
    treatments = treatments,
    redistribution_method = "none"
  )

  expect_identical(
    sum(redistributed_incompletes),
    sum(inflow - reneges - treatments),
    label = "total number of incompletes remains the same before and after the function is used"
  )

  inflow <- c(11, 20, 20, 12, 16)
  reneges <- c(6, 7, 7, 9, 9)
  treatments <- c(9, 9, 9, 2, 7)

  redistributed_incompletes <- calculate_incompletes(
    inflow = inflow,
    reneges = reneges,
    treatments = treatments,
    redistribution_method = "none"
  )

  expect_identical(
    redistributed_incompletes,
    c(-4, 4, 4, 1, 0),
    label = "treatments aren't redistributed"
  )
})

test_that("apply_parameter_skew works", {
  params_before <- c(0.05, 0.02, 0.02, 0.06, 0.08, 0.1)
  skew_factor <- 1.1

  params_after <- apply_parameter_skew(
    params = params_before,
    skew = skew_factor
  )

  expect_equal(
    length(params_after),
    length(params_before),
    info = "length of output identical to length of input"
  )

  expect_equal(
    params_after[1],
    params_before[1],
    info = "first parameter remains unchanged"
  )

  expect_equal(
    tail(params_after, 1),
    tail(params_before, 1) * skew_factor,
    info = "final value is equal to the final value of the input multiplied by the skew factor"
  )

  # testing uniform skew with a pivot_bin

  piv_bin <- 2
  params_after <- apply_parameter_skew(
    params = params_before,
    skew = skew_factor,
    skew_method = "uniform",
    pivot_bin = piv_bin
  )

  expect_equal(
    params_after[1:piv_bin],
    c(params_before[1], params_before[2:piv_bin] * (1 / skew_factor)),
    info = "outputs below the pivot bin are correct"
  )

  expect_equal(
    params_after[(piv_bin + 1):length(params_after)],
    params_before[(piv_bin + 1):length(params_after)] * skew_factor,
    info = "outputs above the pivot bin are correct"
  )

  # test non-integer pivot_bin with "rotate"
  piv_bin <- 4.5
  params_after <- apply_parameter_skew(
    params = params_before,
    skew = skew_factor,
    skew_method = "rotate",
    pivot_bin = piv_bin
  )

  expect_equal(
    params_after[1],
    params_before[1],
    info = "first compartment remains unchanged"
  )

  expect_true(
    all(
      params_after[2:floor(piv_bin + 1)] < params_before[2:floor(piv_bin + 1)]
    ),
    info = "all param vals bar the first one that are below the pivot bin are less than the input params"
  )

  expect_true(
    tail(params_after, 1) > tail(params_before, 1),
    info = "final param value is greater than the final input param value"
  )

  # test non-integer pivot_bin with "rotate"
  piv_bin <- 4.5
  params_after <- apply_parameter_skew(
    params = params_before,
    skew = skew_factor,
    skew_method = "uniform",
    pivot_bin = piv_bin
  )

  expect_equal(
    params_after[1],
    params_before[1],
    info = "first compartment remains unchanged"
  )

  expect_true(
    all(
      params_after[2:floor(piv_bin + 1)] < params_before[2:floor(piv_bin + 1)]
    ),
    info = "all param vals bar the first one that are below the pivot bin are less than the input params"
  )

  expect_true(
    tail(params_after, 1) > tail(params_before, 1),
    info = "final param value is greater than the final input param value"
  )

  params_before <- 0.05
  params_after <- apply_parameter_skew(
    params = params_before,
    skew = 1.5,
    skew_method = "rotate",
    pivot_bin = 2
  )

  expect_equal(
    params_before,
    params_after,
    info = "length 1 params unchanged"
  )
})

test_that("apply_parameter_skew errors", {
  expect_error(
    apply_parameter_skew(
      params = c("0.05", "0.02", "0.02", "0.06", "0.08", "0.1"),
      skew = 1.1
    ),
    "params must be numeric",
    info = "numeric input for params"
  )

  expect_error(
    apply_parameter_skew(
      params = c(0.05, 0.02, 0.02, 0.06, 0.08, 0.1),
      skew = "1.1"
    ),
    "skew must be numeric",
    info = "numeric input for skew"
  )

  expect_error(
    apply_parameter_skew(
      params = c(0.05, 0.02, 0.02, 0.06, 0.08, 0.1),
      skew = rep(1.1, 2)
    ),
    "skew must be length 1",
    info = "skew must be length 1"
  )

  expect_error(
    apply_parameter_skew(
      params = c(0.05, 0.02, 0.02, 0.06, 0.08, 0.1),
      skew = -0.1
    ),
    "skew must be greater than 0",
    info = "skew must be > 0"
  )

  expect_snapshot(
    apply_parameter_skew(
      params = c(0.05, 0.02, 0.02, 0.06, 0.08, 0.1),
      skew = 1.1,
      skew_method = "unknown"
    ),
    error = TRUE
  )
})

test_that("Handles '<1' correctly", {
  expect_equal(convert_months_waited_to_id("<1", 12), 0)
})

test_that("Handles '>x' correctly", {
  expect_equal(convert_months_waited_to_id(">15", 20), 15)
})

test_that("Extracts first numeric value from ranges", {
  expect_equal(convert_months_waited_to_id("10-11", 12), 10)
  expect_equal(convert_months_waited_to_id("3 to 5", 12), 3)
})

test_that("Caps values at max_months_waited", {
  expect_equal(convert_months_waited_to_id("25", 20), 20)
})

test_that("Handles numeric strings and numbers", {
  expect_equal(convert_months_waited_to_id("5", 12), 5)
  expect_equal(convert_months_waited_to_id(7, 12), 7)
})

test_that("Handles mixed input vector", {
  input <- c("<1", "2", "10-11", ">15", "25", "3 to 5")
  expected <- c(0, 2, 10, 15, 20, 3)
  expect_equal(convert_months_waited_to_id(input, 20), expected)
})

test_that("Handles NA and empty strings gracefully", {
  expect_equal(convert_months_waited_to_id(NA, 12), NA_real_)
  expect_equal(convert_months_waited_to_id("", 12), NA_real_)
})


test_that("chop_top_off_data removes top rows and renames columns correctly", {
  # Simulate a data frame with NA rows and header row
  df <- data.frame(
    V1 = c(
      "SHA & Self Trusts - Provider,",
      NA,
      "Year",
      "2016-17",
      "2016-17",
      "2016-17",
      "2016-17",
      "2016-17",
      "2016-17",
      "2016-17",
      "2016-17",
      "2016-17",
      "2016-17"
    ),
    V2 = c(
      rep(NA, 2),
      "Period.Name",
      "MAY",
      "MAY",
      "MAY",
      "MAY",
      "MAY",
      "MAY",
      "MAY",
      "MAY",
      "MAY",
      "MAY"
    ),
    V3 = c(
      rep(NA, 2),
      "Provider.Parent.Org.Code",
      "Q70",
      "Q70",
      "Q70",
      "Q70",
      "Q70",
      "Q70",
      "Q70",
      "Q70",
      "Q70",
      "Q70"
    ),
    V4 = c(
      rep(NA, 2),
      "Provider.Parent.Name",
      "NHS ENGLAND SOUTH (WESSEX)",
      "NHS ENGLAND SOUTH (WESSEX)",
      "NHS ENGLAND SOUTH (WESSEX)",
      "NHS ENGLAND SOUTH (WESSEX)",
      "NHS ENGLAND SOUTH (WESSEX)",
      "NHS ENGLAND SOUTH (WESSEX)",
      "NHS ENGLAND SOUTH (WESSEX)",
      "NHS ENGLAND SOUTH (WESSEX)",
      "NHS ENGLAND SOUTH (WESSEX)",
      "NHS ENGLAND SOUTH (WESSEX)"
    ),
    V5 = c(
      rep(NA, 2),
      "Provider.Org.Code",
      "NT202",
      "NT202",
      "NT202",
      "NT202",
      "NT202",
      "NT202",
      "NT202",
      "NT202",
      "NT202",
      "NT202"
    ),
    V6 = c(
      rep(NA, 2),
      "Provider.Org.Name",
      "NUFFIELD HEALTH, BOURNEMOUTH HOSPITAL",
      "NUFFIELD HEALTH, BOURNEMOUTH HOSPITAL",
      "NUFFIELD HEALTH, BOURNEMOUTH HOSPITAL",
      "NUFFIELD HEALTH, BOURNEMOUTH HOSPITAL",
      "NUFFIELD HEALTH, BOURNEMOUTH HOSPITAL",
      "NUFFIELD HEALTH, BOURNEMOUTH HOSPITAL",
      "NUFFIELD HEALTH, BOURNEMOUTH HOSPITAL",
      "NUFFIELD HEALTH, BOURNEMOUTH HOSPITAL",
      "NUFFIELD HEALTH, BOURNEMOUTH HOSPITAL",
      "NUFFIELD HEALTH, BOURNEMOUTH HOSPITAL"
    ),
    V7 = c(
      rep(NA, 2),
      "Commissioner.Parent.Org.Code",
      "Q70",
      "Q70",
      "Q70",
      "Q70",
      "Q70",
      "Q70",
      "Q70",
      "Q70",
      "Q70",
      "Q70"
    ),
    V8 = c(
      rep(NA, 2),
      "Commissioner.Parent.Name",
      "NHS ENGLAND SOUTH (WESSEX)",
      "NHS ENGLAND SOUTH (WESSEX)",
      "NHS ENGLAND SOUTH (WESSEX)",
      "NHS ENGLAND SOUTH (WESSEX)",
      "NHS ENGLAND SOUTH (WESSEX)",
      "NHS ENGLAND SOUTH (WESSEX)",
      "NHS ENGLAND SOUTH (WESSEX)",
      "NHS ENGLAND SOUTH (WESSEX)",
      "NHS ENGLAND SOUTH (WESSEX)",
      "NHS ENGLAND SOUTH (WESSEX)"
    ),
    V9 = c(
      rep(NA, 2),
      "Commissioner.Org.Code",
      "11A",
      "11A",
      "11A",
      "11A",
      "11A",
      "11A",
      "11A",
      "11A",
      "11A",
      "11A"
    ),
    V10 = c(
      rep(NA, 2),
      "Commissioner.Org.Name",
      "NHS WEST HAMPSHIRE CCG",
      "NHS WEST HAMPSHIRE CCG",
      "NHS WEST HAMPSHIRE CCG",
      "NHS WEST HAMPSHIRE CCG",
      "NHS WEST HAMPSHIRE CCG",
      "NHS WEST HAMPSHIRE CCG",
      "NHS WEST HAMPSHIRE CCG",
      "NHS WEST HAMPSHIRE CCG",
      "NHS WEST HAMPSHIRE CCG",
      "NHS WEST HAMPSHIRE CCG"
    ),
    V11 = c(
      rep(NA, 2),
      "RTT.Part.Name",
      "PART_1A",
      "PART_1A",
      "PART_1A",
      "PART_1A",
      "PART_1A",
      "PART_1B",
      "PART_1B",
      "PART_1B",
      "PART_1B",
      "PART_2"
    ),
    V12 = c(
      rep(NA, 2),
      "RTT.Part.Description",
      "Completed Pathways For Admitted Patients",
      "Completed Pathways For Admitted Patients",
      "Completed Pathways For Admitted Patients",
      "Completed Pathways For Admitted Patients",
      "Completed Pathways For Admitted Patients",
      "Completed Pathways For Non-Admitted Patients",
      "Completed Pathways For Non-Admitted Patients",
      "Completed Pathways For Non-Admitted Patients",
      "Completed Pathways For Non-Admitted Patients",
      "Incomplete Pathways"
    ),
    V13 = c(
      rep(NA, 2),
      "Treatment.Function.Code",
      "C_100",
      "C_110",
      "C_330",
      "C_502",
      "C_999",
      "C_100",
      "C_110",
      "C_502",
      "C_999",
      "C_100"
    ),
    V14 = c(
      rep(NA, 2),
      "Treatment.Function.Name",
      "General Surgery",
      "Trauma & Orthopaedics",
      "Dermatology",
      "Gynaecology",
      "Total",
      "General Surgery",
      "Trauma & Orthopaedics",
      "Gynaecology",
      "Total",
      "General Surgery"
    ),
    V15 = c(
      rep(NA, 2),
      "Gt.00.To.01.Weeks.SUM.1",
      0L,
      0L,
      0L,
      0L,
      0L,
      0L,
      0L,
      0L,
      0L,
      0L
    )
  )
  result <- chop_top_off_data(df)

  expect_equal(
    names(result),
    c(
      "Year",
      "Period.Name",
      "Provider.Parent.Org.Code",
      "Provider.Parent.Name",
      "Provider.Org.Code",
      "Provider.Org.Name",
      "Commissioner.Parent.Org.Code",
      "Commissioner.Parent.Name",
      "Commissioner.Org.Code",
      "Commissioner.Org.Name",
      "RTT.Part.Name",
      "RTT.Part.Description",
      "Treatment.Function.Code",
      "Treatment.Function.Name",
      "Gt.00.To.01.Weeks.SUM.1"
    )
  )
  expect_equal(nrow(result), 10)
})

test_that("chop_top_off_data does not modify data if column names do not all start with V", {
  df <- data.frame(
    A = c(NA, NA, "ID", "1", "2"),
    B = c(NA, NA, "Name", "Alice", "Bob")
  )

  result <- chop_top_off_data(df)

  expect_equal(result, df)
})

test_that("chop_top_off_data handles case with no NA in V1 correctly", {
  df <- data.frame(
    V1 = c("ID", "1", "2"),
    V2 = c("Name", "Alice", "Bob"),
    V3 = c("GtScore", "10", "20")
  )

  result <- chop_top_off_data(df)

  # Since no NA in V1, match(NA, data$V1) returns NA, max(NA) is NA
  expect_equal(names(result), c("ID", "Name", "GtScore"))
  expect_equal(nrow(result), 2)
  expect_equal(result$ID, c("1", "2"))
  expect_equal(result$Name, c("Alice", "Bob"))
  expect_equal(result$GtScore, c(10, 20)) # Should be numeric
  expect_type(result$GtScore, "double")
})

test_that("chop_top_off_data handles empty data frame", {
  df <- data.frame()
  result <- chop_top_off_data(df)
  expect_equal(result, df)
})


test_that("Correct period is computed when Year is present and month is after March", {
  df <- read.csv(test_sheet("20241130-RTT-November-2024-RBD.csv"))
  result <- make_period_field(df)

  expect_equal(unique(result$Period), as.Date("2024-11-01"))
})

test_that("Year is incremented when month is before April", {
  df <- read.csv(test_sheet("20241130-RTT-November-2024-RBD.csv"))
  result <- df |>
    mutate(Period = "February", Year = "2016/17") |>
    make_period_field()

  expect_equal(unique(result$Period), as.Date("2017-02-01"))
})

test_that("Period is parsed correctly when Year is absent", {
  df <- data.frame(
    PeriodName = c("01-January-2023", "01-January-2023")
  )
  names(df)[1] <- "PeriodName"

  result <- make_period_field(df)

  expect_equal(unique(result$PeriodName), as.Date("2023-01-01"))
})

test_that("RTT in period string is replaced and parsed correctly", {
  df <- data.frame(
    PeriodName = c("RTT-January-2023", "RTT-January-2023")
  )
  names(df)[1] <- "PeriodName"

  result <- make_period_field(df)

  expect_equal(unique(result$PeriodName), as.Date("2023-01-01"))
})

test_that("Function handles mixed case month names", {
  df <- data.frame(
    Year = "2023",
    PeriodName = "jAnUaRy"
  )
  names(df)[2] <- "PeriodName"

  result <- make_period_field(df)

  expect_equal(result$PeriodName, as.Date("2024-01-01"))
})

test_that("Function errors gracefully if Period column is missing", {
  df <- data.frame(
    Year = "2023",
    Month = "January"
  )

  expect_error(make_period_field(df))
})


test_that("adjust_treatment_function_field_name changes Name to Code if name isn't present", {
  df <- dplyr::tibble(
    `Treatment Function Name` = c("100", "101")
  )

  result <- adjust_treatment_function_field_name(df)

  expect_equal(names(result), "Treatment Function Code")
})

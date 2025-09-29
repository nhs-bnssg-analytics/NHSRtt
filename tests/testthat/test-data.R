test_that("get_rtt_data error checks", {
  expect_error(
    get_rtt_data(
      date_start = "2024-10-01",
      date_end = as.Date("2024-11-01"),
      show_progress = TRUE
    ),
    "date_start needs to be a date format",
    info = "date_start is a date"
  )

  expect_error(
    get_rtt_data(
      date_start = as.Date("2024-10-01"),
      date_end = "2024-11-01",
      show_progress = TRUE
    ),
    "date_end needs to be a date format",
    info = "date_end is a date"
  )

  expect_error(
    get_rtt_data(
      date_start = as.Date("2024-10-01"),
      date_end = as.Date("2024-11-01"),
      show_progress = c(TRUE, TRUE)
    ),
    "show_progress must be length 1",
    info = "show_progress is length 1"
  )

  expect_error(
    get_rtt_data(
      date_start = as.Date("2024-10-01"),
      date_end = as.Date("2024-11-01"),
      show_progress = "test"
    ),
    "show_progress must be TRUE or FALSE",
    info = "show_progress is logical"
  )

  expect_error(
    get_rtt_data(
      date_start = as.Date("2024-10-01"),
      date_end = as.Date("2024-11-01"),
      show_progress = NA
    ),
    "show_progress must be TRUE or FALSE",
    info = "show_progress is not NA"
  )

  expect_error(
    get_rtt_data(
      date_start = as.Date("2013-10-01"),
      date_end = as.Date("2024-11-01"),
      show_progress = FALSE
    ),
    "The data import function currently isn't set up for data prior to April 2016",
    info = "date_start is too early"
  )
})


test_that("get_rtt_data functionality", {
  df <- get_rtt_data(
    date_start = as.Date("2023-01-01"),
    date_end = as.Date("2023-01-31"),
    trust_codes = "RTE",
    show_progress = FALSE
  )

  expected_names <- c(
    "trust_parent_org_code",
    "commissioner_parent_org_code",
    "commissioner_org_code",
    "trust",
    "specialty",
    "period",
    "months_waited",
    "type",
    "value"
  )

  # completes
  expect_identical(
    names(df),
    expected_names,
    info = "names of get_rtt_data are expected"
  )

  expect_gt(
    nrow(df),
    0
  )

  expect_false(
    any(is.na(df |> dplyr::select(!c("commissioner_parent_org_code")))),
    info = "there are NAs within the completes data"
  )

  expect_equal(
    df |>
      dplyr::filter(type == "Referrals") |>
      dplyr::pull(.data$months_waited) |>
      unique() |>
      as.character(),
    "<1",
    info = "all referrals have months waited '<1'"
  )
})

test_that("tidy_file functionality", {
  tidied_rbd <- tidy_file(
    csv_filepath = test_sheet("20241130-RTT-November-2024-RBD.csv"),
    trust_parent_codes = "QVV",
    commissioner_parent_codes = "QE1",
    commissioner_org_codes = "00X",
    trust_codes = "RBD",
    specialty_codes = "C_999"
  )

  expected_names <- c(
    "trust_parent_org_code",
    "commissioner_parent_org_code",
    "commissioner_org_code",
    "trust",
    "specialty",
    "period",
    "months_waited",
    "type",
    "value"
  )

  expect_equal(
    names(tidied_rbd),
    expected_names,
    info = "names are as expected"
  )

  expect_gt(
    nrow(tidied_rbd),
    0
  )

  expect_false(
    any(is.na(tidied_rbd |> dplyr::select(!c("commissioner_parent_org_code")))),
    info = "there are NAs within the tidied referrals data"
  )
})


test_that("create_dummy_data functionality", {
  expect_snapshot(
    create_dummy_data(
      type = "completes",
      max_months_waited = 4,
      number_periods = 10,
      max_treatments = 50
    ),
    error = TRUE
  )

  mx_treatments <- 50
  mnth_waited_comp <- 24
  pds_comp <- 10
  dummy_complete <- create_dummy_data(
    type = "complete",
    max_months_waited = mnth_waited_comp,
    number_periods = pds_comp,
    max_treatments = mx_treatments
  )

  expect_equal(
    names(dummy_complete),
    c("period_id", "months_waited_id", "treatments"),
    info = "names are as expected"
  )

  expect_equal(
    max(dummy_complete$treatments),
    mx_treatments,
    info = "max number of treatments is expected"
  )

  expect_equal(
    max(dummy_complete$months_waited_id),
    mnth_waited_comp,
    info = "max months waited is expected"
  )

  expect_equal(
    max(dummy_complete$period_id),
    pds_comp,
    info = "max periods is expected"
  )

  expect_gt(
    nrow(dummy_complete),
    0
  )

  expect_false(
    any(is.na(dummy_complete)),
    info = "there are NAs within the dummy_complete data"
  )

  mx_incompletes <- 100
  mnth_waited_inc <- 13
  pds_inc <- 5
  dummy_incomplete <- create_dummy_data(
    type = "incomplete",
    max_months_waited = mnth_waited_inc,
    number_periods = pds_inc,
    max_incompletes = mx_incompletes
  )

  expect_equal(
    names(dummy_incomplete),
    c("period_id", "months_waited_id", "incompletes"),
    info = "names are as expected"
  )

  expect_equal(
    max(dummy_incomplete$incompletes),
    mx_incompletes,
    info = "max number of incompletes is expected"
  )

  expect_equal(
    max(dummy_incomplete$months_waited_id),
    mnth_waited_inc,
    info = "max months waited is expected"
  )

  expect_equal(
    max(dummy_incomplete$period_id),
    pds_inc,
    info = "max periods is expected"
  )

  expect_gt(
    nrow(dummy_incomplete),
    0
  )

  expect_false(
    any(is.na(dummy_incomplete)),
    info = "there are NAs within the dummy_incomplete data"
  )

  referral_vals <- 50:100
  pds_ref <- 2
  dummy_referrals <- create_dummy_data(
    type = "referral",
    number_periods = pds_ref,
    referral_values = referral_vals
  )

  expect_equal(
    names(dummy_referrals),
    c("period_id", "referrals"),
    info = "names are as expected"
  )

  expect_gte(
    min(dummy_referrals$referrals),
    min(referral_vals)
  )

  expect_lte(
    max(dummy_referrals$referrals),
    max(referral_vals)
  )

  expect_equal(
    max(dummy_referrals$period_id),
    pds_ref,
    info = "max periods is expected"
  )

  expect_gt(
    nrow(dummy_referrals),
    0
  )

  expect_false(
    any(is.na(dummy_referrals)),
    info = "there are NAs within the dummy_referrals data"
  )
})


# mocking tests ----------------------------------------------------------

test_that("latest_rtt_file returns expected ZIP file link", {
  # Create a mock version of obtain_links
  mock_obtain_links <- function(url) {
    if (
      url ==
        "https://www.england.nhs.uk/statistics/statistical-work-areas/rtt-waiting-times/"
    ) {
      return(c(
        `Skip to main content` = "#main-content",
        `read more about our cookies` = "https://www.england.nhs.uk/privacy-policy/",
        Home = "https://www.england.nhs.uk/",
        News = "https://www.england.nhs.uk/news/",
        Publications = "https://www.england.nhs.uk/publication/",
        Statistics = "https://www.england.nhs.uk/statistics/",
        Blogs = "https://www.england.nhs.uk/blogs/",
        Events = "https://www.england.nhs.uk/events/",
        `Contact us` = "https://www.england.nhs.uk/contact-us/",
        "https://www.england.nhs.uk/ourwork/",
        `About us` = "https://www.england.nhs.uk/about/",
        `Our work` = "https://www.england.nhs.uk/ourwork",
        Commissioning = "https://www.england.nhs.uk/commissioning/",
        `Get involved` = "https://www.england.nhs.uk/participation/",
        Statistics = "https://www.england.nhs.uk/statistics",
        `Statistical work areas` = "https://www.england.nhs.uk/statistics/statistical-work-areas/",
        `Acute provider table` = "https://www.england.nhs.uk/statistics/statistical-work-areas/acute-provider-table/",
        `2-hour Urgent Community Response` = "https://www.england.nhs.uk/statistics/statistical-work-areas/2-hour-urgent-community-response/",
        `A&E Attendances and Emergency Admissions` = "https://www.england.nhs.uk/statistics/statistical-work-areas/ae-waiting-times-and-activity/",
        `Ambulance Quality Indicators` = "https://www.england.nhs.uk/statistics/statistical-work-areas/ambulance-quality-indicators/",
        `Bed Availability and Occupancy` = "https://www.england.nhs.uk/statistics/statistical-work-areas/bed-availability-and-occupancy/",
        `Cancelled Elective Operations` = "https://www.england.nhs.uk/statistics/statistical-work-areas/cancelled-elective-operations/",
        `National Cancer Patient Experience Survey` = "https://www.england.nhs.uk/statistics/statistical-work-areas/cancer-patient-experience-survey/",
        `Under 16 Cancer Patient Experience Survey` = "https://www.england.nhs.uk/statistics/statistical-work-areas/under-16-cancer-patient-experience-survey/",
        `Cancer Waiting Times` = "https://www.england.nhs.uk/statistics/statistical-work-areas/cancer-waiting-times/",
        `Community health services waiting lists` = "https://www.england.nhs.uk/statistics/statistical-work-areas/community-health-services-waiting-lists/",
        `Continuing Healthcare and NHS-funded Nursing Care` = "https://www.england.nhs.uk/statistics/statistical-work-areas/nhs-chc-fnc/",
        `COVID-19 Hospital Activity` = "https://www.england.nhs.uk/statistics/statistical-work-areas/covid-19-hospital-activity/",
        `Critical Care Bed Capacity and Urgent Operations Cancelled` = "https://www.england.nhs.uk/statistics/statistical-work-areas/critical-care-capacity/",
        `Dental commissioning` = "https://www.england.nhs.uk/statistics/statistical-work-areas/dental-commissioning/",
        `Dental Workforce` = "https://www.england.nhs.uk/statistics/statistical-work-areas/dental-workforce/",
        `Diagnostic Imaging Dataset` = "https://www.england.nhs.uk/statistics/statistical-work-areas/diagnostic-imaging-dataset/",
        `Diagnostics Waiting Times and Activity` = "https://www.england.nhs.uk/statistics/statistical-work-areas/diagnostics-waiting-times-and-activity/",
        `Discharge Delays` = "https://www.england.nhs.uk/statistics/statistical-work-areas/discharge-delays/",
        `Friends and Family Test` = "https://www.england.nhs.uk/statistics/statistical-work-areas/friends-and-family-test/",
        `Genomic Testing activity` = "https://www.england.nhs.uk/statistics/statistical-work-areas/genomic-testing-activity/",
        `GP Patient Survey` = "https://www.england.nhs.uk/statistics/statistical-work-areas/gp-patient-survey/",
        `Health Visitors` = "https://www.england.nhs.uk/statistics/statistical-work-areas/health-visitors/",
        `Integrated Urgent Care` = "https://www.england.nhs.uk/statistics/statistical-work-areas/iucadc-new-from-april-2021/",
        `Intermediate Care` = "https://www.england.nhs.uk/statistics/statistical-work-areas/intermediate-care/",
        `Martha’s Rule` = "https://www.england.nhs.uk/statistics/statistical-work-areas/marthas-rule/",
        `Mental health: children and young people with an eating disorder waiting times` = "https://www.england.nhs.uk/statistics/statistical-work-areas/cyped-waiting-times/",
        `Mental health: physical health checks for people with severe mental illness` = "https://www.england.nhs.uk/statistics/statistical-work-areas/serious-mental-illness-smi/",
        `Mixed-sex accommodation` = "https://www.england.nhs.uk/statistics/statistical-work-areas/mixed-sex-accommodation/",
        `National Diabetes Experience Survey (NDES)` = "https://www.england.nhs.uk/statistics/statistical-work-areas/national-diabetes-experience-survey-ndes/",
        `National Patient and Staff Surveys` = "https://www.england.nhs.uk/statistics/statistical-work-areas/patient-surveys/",
        `NHS Staff Survey in England` = "https://www.england.nhs.uk/statistics/statistical-work-areas/nhs-staff-survey-in-england/",
        `Outpatient Recovery and Transformation` = "https://www.england.nhs.uk/statistics/statistical-work-areas/outpatient-transformation/",
        `Patient Reported Outcome Measures (PROMs)` = "https://www.england.nhs.uk/statistics/statistical-work-areas/proms/",
        `Patient safety data` = "https://www.england.nhs.uk/statistics/statistical-work-areas/patient-safety-data/",
        `Referral to Treatment (RTT) Waiting Times` = "https://www.england.nhs.uk/statistics/statistical-work-areas/rtt-waiting-times/",
        `Waiting List Minimum Data Set (WLMDS) Information` = "https://www.england.nhs.uk/statistics/statistical-work-areas/rtt-waiting-times/wlmds/",
        `Recovery of Elective Activity` = "https://www.england.nhs.uk/statistics/statistical-work-areas/rtt-waiting-times/recovery-of-elective_activity-mi/",
        `Consultant-led Referral to Treatment Waiting Times Data 2025-26` = "https://www.england.nhs.uk/statistics/statistical-work-areas/rtt-waiting-times/rtt-data-2025-26/",
        `Consultant-led Referral to Treatment Waiting Times Rules and Guidance` = "https://www.england.nhs.uk/statistics/statistical-work-areas/rtt-waiting-times/rtt-guidance/",
        `Consultant-led Referral to Treatment Waiting Times Data 2024-25` = "https://www.england.nhs.uk/statistics/statistical-work-areas/rtt-waiting-times/rtt-data-2024-25/",
        `Consultant-led Referral to Treatment Waiting Times Data 2023-24` = "https://www.england.nhs.uk/statistics/statistical-work-areas/rtt-waiting-times/rtt-data-2023-24/",
        `Consultant-led Referral to Treatment Waiting Times Data 2022-23` = "https://www.england.nhs.uk/statistics/statistical-work-areas/rtt-waiting-times/rtt-data-2022-23/",
        `Consultant-led Referral to Treatment Waiting Times Data 2021-22` = "https://www.england.nhs.uk/statistics/statistical-work-areas/rtt-waiting-times/rtt-data-2021-22/",
        `Consultant-led Referral to Treatment Waiting Times Data 2020-21` = "https://www.england.nhs.uk/statistics/statistical-work-areas/rtt-waiting-times/rtt-data-2020-21/",
        `Consultant-led Referral to Treatment Waiting Times Data 2019-20` = "https://www.england.nhs.uk/statistics/statistical-work-areas/rtt-waiting-times/rtt-data-2019-20/",
        `Consultant-led Referral to Treatment Waiting Times Data 2018-19` = "https://www.england.nhs.uk/statistics/statistical-work-areas/rtt-waiting-times/rtt-data-2018-19/",
        `Consultant-led Referral to Treatment Waiting Times Data 2017-18` = "https://www.england.nhs.uk/statistics/statistical-work-areas/rtt-waiting-times/rtt-data-2017-18/",
        `Consultant-led Referral to Treatment Waiting Times Data 2016-17` = "https://www.england.nhs.uk/statistics/statistical-work-areas/rtt-waiting-times/rtt-data-2016-17/",
        `Consultant-led Referral to Treatment Waiting Times Data 2015-16` = "https://www.england.nhs.uk/statistics/statistical-work-areas/rtt-waiting-times/rtt-data-2015-16/",
        `Consultant-led Referral to Treatment Waiting Times Data 2014-15` = "https://www.england.nhs.uk/statistics/statistical-work-areas/rtt-waiting-times/rtt-data-2014-15/",
        `Consultant-led Referral to Treatment Waiting Times Data 2013-14` = "https://www.england.nhs.uk/statistics/statistical-work-areas/rtt-waiting-times/rtt-data-2013-14/",
        `Consultant-led Referral to Treatment Waiting Times Data 2012-13` = "https://www.england.nhs.uk/statistics/statistical-work-areas/rtt-waiting-times/rtt-data-2012-13/",
        `Consultant-led Referral to Treatment Waiting Times Data 2011-12` = "https://www.england.nhs.uk/statistics/statistical-work-areas/rtt-waiting-times/rtt-data-2011-12/",
        Screening = "https://www.england.nhs.uk/statistics/statistical-work-areas/screening/",
        `Urgent and Emergency Care Daily Situation Reports` = "https://www.england.nhs.uk/statistics/statistical-work-areas/uec-sitrep/",
        `Vaccinations: Child Immunisation` = "https://www.england.nhs.uk/statistics/statistical-work-areas/child-immunisation/",
        `Vaccinations: COVID-19` = "https://www.england.nhs.uk/statistics/statistical-work-areas/covid-19-vaccinations/",
        `Vaccinations: Flu` = "https://www.england.nhs.uk/statistics/statistical-work-areas/flu-vaccinations/",
        `Vaccinations: RSV` = "https://www.england.nhs.uk/statistics/statistical-work-areas/vaccinations-rsv/",
        `Vaccinations: Mpox` = "https://www.england.nhs.uk/statistics/statistical-work-areas/vaccinations-for-mpox/",
        `Vaccinations: Polio` = "https://www.england.nhs.uk/statistics/statistical-work-areas/polio-vaccinations-in-london-region/",
        `Venous thromboembolism (VTE) risk assessment` = "https://www.england.nhs.uk/statistics/statistical-work-areas/vte/",
        `Virtual Ward` = "https://www.england.nhs.uk/statistics/statistical-work-areas/virtual-ward/",
        `Wheelchair data collection` = "https://www.england.nhs.uk/statistics/statistical-work-areas/national-wheelchair/",
        `(Discontinued) Combined Performance Summary` = "https://www.england.nhs.uk/statistics/statistical-work-areas/combined-performance-summary/",
        `(Discontinued) COVID-19 Deaths` = "https://www.england.nhs.uk/statistics/statistical-work-areas/covid-19-deaths/",
        `(Discontinued) COVID-19 Post-Covid Assessment Service` = "https://www.england.nhs.uk/statistics/statistical-work-areas/covid-19-post-covid-assessment-service/",
        `(Discontinued) Delayed Transfers of Care` = "https://www.england.nhs.uk/statistics/statistical-work-areas/delayed-transfers-of-care/",
        `(Discontinued) Dementia Assessment and Referral` = "https://www.england.nhs.uk/statistics/statistical-work-areas/dementia/",
        `(Discontinued) Direct Access Audiology` = "https://www.england.nhs.uk/statistics/statistical-work-areas/direct-access-audiology/",
        `(Discontinued) Discharge delays (community)` = "https://www.england.nhs.uk/statistics/statistical-work-areas/discharge-delays-community-data/",
        `(Discontinued) Early Intervention in Psychosis Waiting Times` = "https://www.england.nhs.uk/statistics/statistical-work-areas/eip-waiting-times/",
        `(Discontinued) GP Extended Access` = "https://www.england.nhs.uk/statistics/statistical-work-areas/extended-access-general-practice/",
        `(Discontinued) Hospital Activity` = "https://www.england.nhs.uk/statistics/statistical-work-areas/hospital-activity/",
        `(Discontinued) Integrated Performance Measures Monitoring` = "https://www.england.nhs.uk/statistics/statistical-work-areas/integrated-performance-measures-monitoring/",
        `(Discontinued) Maternity and Breastfeeding` = "https://www.england.nhs.uk/statistics/statistical-work-areas/maternity-and-breastfeeding/",
        `(Discontinued) Mental Health Community Teams Activity` = "https://www.england.nhs.uk/statistics/statistical-work-areas/mental-health-community-teams-activity/",
        `(Discontinued) Patient Experience Scores Overall` = "https://www.england.nhs.uk/statistics/statistical-work-areas/pat-exp/",
        `(Discontinued) Referral for Outpatient` = "https://www.england.nhs.uk/statistics/statistical-work-areas/outpatient-referrals/",
        `(Discontinued) Winter Daily Situation Reports` = "https://www.england.nhs.uk/statistics/statistical-work-areas/winter-daily-sitreps/",
        `Supplementary information` = "https://www.england.nhs.uk/statistics/statistical-work-areas/supplementary-information/",
        `Critical care and General & Acute Beds – Urgent and Emergency Care Daily Situation Reports 2024-25` = "https://www.england.nhs.uk/statistics/critical-care-and-general-acute-beds-urgent-and-emergency-care-daily-situation-reports-2024-25/",
        `GP Patient Survey Dental Statistics, January to March 2025, England` = "https://www.england.nhs.uk/statistics/gp-patient-survey-dental-statistics-january-to-march-2025-england/",
        `12 months statistics calendar` = "https://www.england.nhs.uk/statistics/12-months-statistics-calendar/",
        `Collections timetable` = "https://www.england.nhs.uk/statistics/collections-timetable/",
        `COVID-19 and the production of statistics` = "https://www.england.nhs.uk/statistics/covid-19-and-the-production-of-statistics/",
        `General guidance for NHS England collections for data providers and commissioners` = "https://www.england.nhs.uk/statistics/guidance-prov-comms/",
        `Guidance on non-submission of mandatory returns` = "https://www.england.nhs.uk/statistics/guidance-non-submissions/",
        News = "https://www.england.nhs.uk/statistics/news/",
        `Statistics code of practice compliance` = "https://www.england.nhs.uk/statistics/code-compliance/",
        `Statistics contact us` = "https://www.england.nhs.uk/statistics/statistics-contact-us/",
        Home = "/",
        Statistics = "https://www.england.nhs.uk/statistics",
        `Statistical work areas` = "https://www.england.nhs.uk/statistics/statistical-work-areas/",
        "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2024/06/OfAcLogo-150x150.jpg",
        `2025-26 RTT waiting times data` = "https://www.england.nhs.uk/statistics/statistical-work-areas/rtt-waiting-times/rtt-data-2025-26",
        `2024-25 RTT waiting times data` = "https://www.england.nhs.uk/statistics/statistical-work-areas/rtt-waiting-times/rtt-data-2024-25",
        `2023-24 RTT waiting times data` = "https://www.england.nhs.uk/statistics/statistical-work-areas/rtt-waiting-times/rtt-data-2023-24/",
        `2022-23 RTT waiting times data` = "https://www.england.nhs.uk/statistics/statistical-work-areas/rtt-waiting-times/rtt-data-2022-23/",
        `2021-22 RTT waiting times data` = "https://www.england.nhs.uk/statistics/statistical-work-areas/rtt-waiting-times/rtt-data-2021-22/",
        `2020-21 RTT waiting times data` = "https://www.england.nhs.uk/statistics/statistical-work-areas/rtt-waiting-times/rtt-data-2020-21/",
        `2019-20 RTT waiting times data` = "https://www.england.nhs.uk/statistics/statistical-work-areas/rtt-waiting-times/rtt-data-2019-20/",
        `2018-19 RTT waiting times data` = "https://www.england.nhs.uk/statistics/statistical-work-areas/rtt-waiting-times/rtt-data-2018-19/",
        `2017-18 RTT waiting times data` = "https://www.england.nhs.uk/statistics/statistical-work-areas/rtt-waiting-times/rtt-data-2017-18/",
        `2016-17 RTT waiting times data` = "https://www.england.nhs.uk/statistics/statistical-work-areas/rtt-waiting-times/rtt-data-2016-17/",
        `2015-16 RTT waiting times data` = "https://www.england.nhs.uk/statistics/statistical-work-areas/rtt-waiting-times/rtt-data-2015-16/",
        `2014-15 RTT waiting times data` = "https://www.england.nhs.uk/statistics/rtt-data-2014-15/",
        `2013-14 RTT waiting times data` = "https://www.england.nhs.uk/statistics/rtt-waiting-times/rtt-data-2013-14/",
        `2012-13 RTT waiting times data` = "https://www.england.nhs.uk/statistics/rtt-waiting-times/rtt-data-2012-13/",
        `2011-12 RTT waiting times data` = "https://www.england.nhs.uk/statistics/rtt-waiting-times/rtt-data-2011-12/",
        `Data prior to 2011-12 is currently still found here on the old RTT web page` = "http://webarchive.nationalarchives.gov.uk/20130107105354/http://www.dh.gov.uk/en/Publicationsandstatistics/Statistics/Performancedataandstatistics/ReferraltoTreatmentstatistics/index.htm",
        `Pre-release access list (PDF, 91K)` = "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2025/09/20250911-Pre-release-access-list-PDF-91K.pdf",
        `Download 2019_20 Annual Report (PDF, 734K)` = "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2020/11/RTT-Annual-Report-2019-20-2.pdf",
        `2019-20 Annual Report – spreadsheet (XLS 99K)` = "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2020/11/Annual-Report-2019-20-timeseries-including-missing-data-ests-XLS-62K.xls",
        `Download 2018_19 RTT Annual Report (PDF, 1336K)` = "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2019/06/RTT-Annual-Report-2018-19-v0.5.pdf",
        `2018_19 Annual Report – spreadsheet (XLS, 51K)` = "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2019/06/2018_19-Annual-Report-spreadsheet-XLS-51K.xls",
        `Download 2017_18 RTT Annual Report (PDF, 1295K)` = "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2018/06/RTT-Annual-Report-2017-18-PDF-1295K.pdf",
        `2017_18 Annual Report – spreadsheet (XLS, 51K)` = "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2018/06/2017_18-Annual-Report-spreadsheet-XLS-51K.xls",
        `Download 2016_17 RTT Annual Report (PDF, 1484K)` = "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2017/06/RTT-Annual-Report-2016-17-v0.9-final.pdf",
        `2016_17 Annual Report – spreadsheet (XLS, 48K)` = "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2017/06/2016_17-Annual-Report-spreadsheet-XLS-48K.xls",
        `Download 2015_16 RTT Annual Report v2 (PDF, 564K)` = "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2013/04/RTT-Annual-Report-2015-16-v3_final.pdf",
        `2015_16 Annual Report – spreadsheet (XLS, 46K)` = "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2013/04/2015_16-Annual-Report-spreadsheet-XLS-46K.xls",
        `Download 2014 Annual Report (PDF, 2985K)` = "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2013/04/RTT-Annual-Report-2014.pdf",
        `2014 Annual Report – spreadsheet (XLS, 46K)` = "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2013/04/RTT-Overview-Timeseries-to-Dec-2014-with-estimates-for-missing-data-v2.xls",
        `Download 2013 Annual Report (PDF, 1749K)` = "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2014/02/RTT-Annual-Report-2013-final.pdf",
        `Download 2012 Annual Report (PDF, 152K)` = "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2013/04/RTT-Annual-Report-2012.pdf",
        `Download 2011 Annual Report (PDF, 155K)` = "http://webarchive.nationalarchives.gov.uk/20130107105354/http://www.dh.gov.uk/prod_consum_dh/groups/dh_digitalassets/@dh/@en/@ps/@sta/@perf/documents/digitalasset/dh_132679.pdf",
        `Download 2010 Annual Report (PDF, 362K)` = "http://webarchive.nationalarchives.gov.uk/20130107105354/http://www.dh.gov.uk/prod_consum_dh/groups/dh_digitalassets/@dh/@en/@ps/@sta/@perf/documents/digitalasset/dh_124415.pdf",
        `Download 2009 Annual Report (PDF, 283K)` = "http://webarchive.nationalarchives.gov.uk/20130107105354/http://www.dh.gov.uk/prod_consum_dh/groups/dh_digitalassets/@dh/@en/@ps/@sta/@perf/documents/digitalasset/dh_112663.pdf",
        `Recovery of Elective Activity` = "https://www.england.nhs.uk/statistics/statistical-work-areas/rtt-waiting-times/recovery-of-elective_activity-mi/",
        `RTT WLMDS summary information` = "https://www.england.nhs.uk/statistics/statistical-work-areas/rtt-waiting-times/wlmds/",
        `RTT waiting times clock rules and FAQ` = "https://www.england.nhs.uk/statistics/rtt-waiting-times/rtt-guidance/",
        `Guide to waiting times for patients` = "https://www.nhs.uk/nhs-services/hospitals/guide-to-nhs-waiting-times-in-england/",
        `NHS website` = "http://www.nhs.uk",
        Statistics = "https://www.england.nhs.uk/statistics",
        `Statistical work areas` = "https://www.england.nhs.uk/statistics/statistical-work-areas/",
        `Acute provider table` = "https://www.england.nhs.uk/statistics/statistical-work-areas/acute-provider-table/",
        `2-hour Urgent Community Response` = "https://www.england.nhs.uk/statistics/statistical-work-areas/2-hour-urgent-community-response/",
        `A&E Attendances and Emergency Admissions` = "https://www.england.nhs.uk/statistics/statistical-work-areas/ae-waiting-times-and-activity/",
        `Ambulance Quality Indicators` = "https://www.england.nhs.uk/statistics/statistical-work-areas/ambulance-quality-indicators/",
        `Bed Availability and Occupancy` = "https://www.england.nhs.uk/statistics/statistical-work-areas/bed-availability-and-occupancy/",
        `Cancelled Elective Operations` = "https://www.england.nhs.uk/statistics/statistical-work-areas/cancelled-elective-operations/",
        `National Cancer Patient Experience Survey` = "https://www.england.nhs.uk/statistics/statistical-work-areas/cancer-patient-experience-survey/",
        `Under 16 Cancer Patient Experience Survey` = "https://www.england.nhs.uk/statistics/statistical-work-areas/under-16-cancer-patient-experience-survey/",
        `Cancer Waiting Times` = "https://www.england.nhs.uk/statistics/statistical-work-areas/cancer-waiting-times/",
        `Community health services waiting lists` = "https://www.england.nhs.uk/statistics/statistical-work-areas/community-health-services-waiting-lists/",
        `Continuing Healthcare and NHS-funded Nursing Care` = "https://www.england.nhs.uk/statistics/statistical-work-areas/nhs-chc-fnc/",
        `COVID-19 Hospital Activity` = "https://www.england.nhs.uk/statistics/statistical-work-areas/covid-19-hospital-activity/",
        `Critical Care Bed Capacity and Urgent Operations Cancelled` = "https://www.england.nhs.uk/statistics/statistical-work-areas/critical-care-capacity/",
        `Dental commissioning` = "https://www.england.nhs.uk/statistics/statistical-work-areas/dental-commissioning/",
        `Dental Workforce` = "https://www.england.nhs.uk/statistics/statistical-work-areas/dental-workforce/",
        `Diagnostic Imaging Dataset` = "https://www.england.nhs.uk/statistics/statistical-work-areas/diagnostic-imaging-dataset/",
        `Diagnostics Waiting Times and Activity` = "https://www.england.nhs.uk/statistics/statistical-work-areas/diagnostics-waiting-times-and-activity/",
        `Discharge Delays` = "https://www.england.nhs.uk/statistics/statistical-work-areas/discharge-delays/",
        `Friends and Family Test` = "https://www.england.nhs.uk/statistics/statistical-work-areas/friends-and-family-test/",
        `Genomic Testing activity` = "https://www.england.nhs.uk/statistics/statistical-work-areas/genomic-testing-activity/",
        `GP Patient Survey` = "https://www.england.nhs.uk/statistics/statistical-work-areas/gp-patient-survey/",
        `Health Visitors` = "https://www.england.nhs.uk/statistics/statistical-work-areas/health-visitors/",
        `Integrated Urgent Care` = "https://www.england.nhs.uk/statistics/statistical-work-areas/iucadc-new-from-april-2021/",
        `Intermediate Care` = "https://www.england.nhs.uk/statistics/statistical-work-areas/intermediate-care/",
        `Martha’s Rule` = "https://www.england.nhs.uk/statistics/statistical-work-areas/marthas-rule/",
        `Mental health: children and young people with an eating disorder waiting times` = "https://www.england.nhs.uk/statistics/statistical-work-areas/cyped-waiting-times/",
        `Mental health: physical health checks for people with severe mental illness` = "https://www.england.nhs.uk/statistics/statistical-work-areas/serious-mental-illness-smi/",
        `Mixed-sex accommodation` = "https://www.england.nhs.uk/statistics/statistical-work-areas/mixed-sex-accommodation/",
        `National Diabetes Experience Survey (NDES)` = "https://www.england.nhs.uk/statistics/statistical-work-areas/national-diabetes-experience-survey-ndes/",
        `National Patient and Staff Surveys` = "https://www.england.nhs.uk/statistics/statistical-work-areas/patient-surveys/",
        `NHS Staff Survey in England` = "https://www.england.nhs.uk/statistics/statistical-work-areas/nhs-staff-survey-in-england/",
        `Outpatient Recovery and Transformation` = "https://www.england.nhs.uk/statistics/statistical-work-areas/outpatient-transformation/",
        `Patient Reported Outcome Measures (PROMs)` = "https://www.england.nhs.uk/statistics/statistical-work-areas/proms/",
        `Patient safety data` = "https://www.england.nhs.uk/statistics/statistical-work-areas/patient-safety-data/",
        `Referral to Treatment (RTT) Waiting Times` = "https://www.england.nhs.uk/statistics/statistical-work-areas/rtt-waiting-times/",
        `Waiting List Minimum Data Set (WLMDS) Information` = "https://www.england.nhs.uk/statistics/statistical-work-areas/rtt-waiting-times/wlmds/",
        `Recovery of Elective Activity` = "https://www.england.nhs.uk/statistics/statistical-work-areas/rtt-waiting-times/recovery-of-elective_activity-mi/",
        `Consultant-led Referral to Treatment Waiting Times Data 2025-26` = "https://www.england.nhs.uk/statistics/statistical-work-areas/rtt-waiting-times/rtt-data-2025-26/",
        `Consultant-led Referral to Treatment Waiting Times Rules and Guidance` = "https://www.england.nhs.uk/statistics/statistical-work-areas/rtt-waiting-times/rtt-guidance/",
        `Consultant-led Referral to Treatment Waiting Times Data 2024-25` = "https://www.england.nhs.uk/statistics/statistical-work-areas/rtt-waiting-times/rtt-data-2024-25/",
        `Consultant-led Referral to Treatment Waiting Times Data 2023-24` = "https://www.england.nhs.uk/statistics/statistical-work-areas/rtt-waiting-times/rtt-data-2023-24/",
        `Consultant-led Referral to Treatment Waiting Times Data 2022-23` = "https://www.england.nhs.uk/statistics/statistical-work-areas/rtt-waiting-times/rtt-data-2022-23/",
        `Consultant-led Referral to Treatment Waiting Times Data 2021-22` = "https://www.england.nhs.uk/statistics/statistical-work-areas/rtt-waiting-times/rtt-data-2021-22/",
        `Consultant-led Referral to Treatment Waiting Times Data 2020-21` = "https://www.england.nhs.uk/statistics/statistical-work-areas/rtt-waiting-times/rtt-data-2020-21/",
        `Consultant-led Referral to Treatment Waiting Times Data 2019-20` = "https://www.england.nhs.uk/statistics/statistical-work-areas/rtt-waiting-times/rtt-data-2019-20/",
        `Consultant-led Referral to Treatment Waiting Times Data 2018-19` = "https://www.england.nhs.uk/statistics/statistical-work-areas/rtt-waiting-times/rtt-data-2018-19/",
        `Consultant-led Referral to Treatment Waiting Times Data 2017-18` = "https://www.england.nhs.uk/statistics/statistical-work-areas/rtt-waiting-times/rtt-data-2017-18/",
        `Consultant-led Referral to Treatment Waiting Times Data 2016-17` = "https://www.england.nhs.uk/statistics/statistical-work-areas/rtt-waiting-times/rtt-data-2016-17/",
        `Consultant-led Referral to Treatment Waiting Times Data 2015-16` = "https://www.england.nhs.uk/statistics/statistical-work-areas/rtt-waiting-times/rtt-data-2015-16/",
        `Consultant-led Referral to Treatment Waiting Times Data 2014-15` = "https://www.england.nhs.uk/statistics/statistical-work-areas/rtt-waiting-times/rtt-data-2014-15/",
        `Consultant-led Referral to Treatment Waiting Times Data 2013-14` = "https://www.england.nhs.uk/statistics/statistical-work-areas/rtt-waiting-times/rtt-data-2013-14/",
        `Consultant-led Referral to Treatment Waiting Times Data 2012-13` = "https://www.england.nhs.uk/statistics/statistical-work-areas/rtt-waiting-times/rtt-data-2012-13/",
        `Consultant-led Referral to Treatment Waiting Times Data 2011-12` = "https://www.england.nhs.uk/statistics/statistical-work-areas/rtt-waiting-times/rtt-data-2011-12/",
        Screening = "https://www.england.nhs.uk/statistics/statistical-work-areas/screening/",
        `Urgent and Emergency Care Daily Situation Reports` = "https://www.england.nhs.uk/statistics/statistical-work-areas/uec-sitrep/",
        `Vaccinations: Child Immunisation` = "https://www.england.nhs.uk/statistics/statistical-work-areas/child-immunisation/",
        `Vaccinations: COVID-19` = "https://www.england.nhs.uk/statistics/statistical-work-areas/covid-19-vaccinations/",
        `Vaccinations: Flu` = "https://www.england.nhs.uk/statistics/statistical-work-areas/flu-vaccinations/",
        `Vaccinations: RSV` = "https://www.england.nhs.uk/statistics/statistical-work-areas/vaccinations-rsv/",
        `Vaccinations: Mpox` = "https://www.england.nhs.uk/statistics/statistical-work-areas/vaccinations-for-mpox/",
        `Vaccinations: Polio` = "https://www.england.nhs.uk/statistics/statistical-work-areas/polio-vaccinations-in-london-region/",
        `Venous thromboembolism (VTE) risk assessment` = "https://www.england.nhs.uk/statistics/statistical-work-areas/vte/",
        `Virtual Ward` = "https://www.england.nhs.uk/statistics/statistical-work-areas/virtual-ward/",
        `Wheelchair data collection` = "https://www.england.nhs.uk/statistics/statistical-work-areas/national-wheelchair/",
        `(Discontinued) Combined Performance Summary` = "https://www.england.nhs.uk/statistics/statistical-work-areas/combined-performance-summary/",
        `(Discontinued) COVID-19 Deaths` = "https://www.england.nhs.uk/statistics/statistical-work-areas/covid-19-deaths/",
        `(Discontinued) COVID-19 Post-Covid Assessment Service` = "https://www.england.nhs.uk/statistics/statistical-work-areas/covid-19-post-covid-assessment-service/",
        `(Discontinued) Delayed Transfers of Care` = "https://www.england.nhs.uk/statistics/statistical-work-areas/delayed-transfers-of-care/",
        `(Discontinued) Dementia Assessment and Referral` = "https://www.england.nhs.uk/statistics/statistical-work-areas/dementia/",
        `(Discontinued) Direct Access Audiology` = "https://www.england.nhs.uk/statistics/statistical-work-areas/direct-access-audiology/",
        `(Discontinued) Discharge delays (community)` = "https://www.england.nhs.uk/statistics/statistical-work-areas/discharge-delays-community-data/",
        `(Discontinued) Early Intervention in Psychosis Waiting Times` = "https://www.england.nhs.uk/statistics/statistical-work-areas/eip-waiting-times/",
        `(Discontinued) GP Extended Access` = "https://www.england.nhs.uk/statistics/statistical-work-areas/extended-access-general-practice/",
        `(Discontinued) Hospital Activity` = "https://www.england.nhs.uk/statistics/statistical-work-areas/hospital-activity/",
        `(Discontinued) Integrated Performance Measures Monitoring` = "https://www.england.nhs.uk/statistics/statistical-work-areas/integrated-performance-measures-monitoring/",
        `(Discontinued) Maternity and Breastfeeding` = "https://www.england.nhs.uk/statistics/statistical-work-areas/maternity-and-breastfeeding/",
        `(Discontinued) Mental Health Community Teams Activity` = "https://www.england.nhs.uk/statistics/statistical-work-areas/mental-health-community-teams-activity/",
        `(Discontinued) Patient Experience Scores Overall` = "https://www.england.nhs.uk/statistics/statistical-work-areas/pat-exp/",
        `(Discontinued) Referral for Outpatient` = "https://www.england.nhs.uk/statistics/statistical-work-areas/outpatient-referrals/",
        `(Discontinued) Winter Daily Situation Reports` = "https://www.england.nhs.uk/statistics/statistical-work-areas/winter-daily-sitreps/",
        `Supplementary information` = "https://www.england.nhs.uk/statistics/statistical-work-areas/supplementary-information/",
        `Critical care and General & Acute Beds – Urgent and Emergency Care Daily Situation Reports 2024-25` = "https://www.england.nhs.uk/statistics/critical-care-and-general-acute-beds-urgent-and-emergency-care-daily-situation-reports-2024-25/",
        `GP Patient Survey Dental Statistics, January to March 2025, England` = "https://www.england.nhs.uk/statistics/gp-patient-survey-dental-statistics-january-to-march-2025-england/",
        `12 months statistics calendar` = "https://www.england.nhs.uk/statistics/12-months-statistics-calendar/",
        `Collections timetable` = "https://www.england.nhs.uk/statistics/collections-timetable/",
        `COVID-19 and the production of statistics` = "https://www.england.nhs.uk/statistics/covid-19-and-the-production-of-statistics/",
        `General guidance for NHS England collections for data providers and commissioners` = "https://www.england.nhs.uk/statistics/guidance-prov-comms/",
        `Guidance on non-submission of mandatory returns` = "https://www.england.nhs.uk/statistics/guidance-non-submissions/",
        News = "https://www.england.nhs.uk/statistics/news/",
        `Statistics code of practice compliance` = "https://www.england.nhs.uk/statistics/code-compliance/",
        `Statistics contact us` = "https://www.england.nhs.uk/statistics/statistics-contact-us/",
        `Terms and conditions` = "https://www.england.nhs.uk/terms-and-conditions/",
        `Privacy and cookies` = "https://www.england.nhs.uk/privacy-policy/",
        `Social media and comment moderation` = "https://www.england.nhs.uk/comment-policy/",
        `How could this website work better for you?` = "https://www.england.nhs.uk/contact-us/feedback/",
        `Accessibility statement` = "https://www.england.nhs.uk/accessibility/",
        `Open Government Licence v3.0` = "https://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/",
        `Follow us on Twitter` = "http://www.twitter.com/NHSEngland",
        `Visit us on LinkedIn` = "https://www.linkedin.com/company/nhsengland",
        `Watch videos on YouTube` = "http://www.youtube.com/thenhsengland",
        `View photos on Flickr` = "http://www.flickr.com/photos/nhsengland",
        `All RSS` = "http://feeds.feedburner.com/NHSCBoard",
        "https://www.nhs.uk/"
      ))
    } else if (
      url ==
        "https://www.england.nhs.uk/statistics/statistical-work-areas/rtt-waiting-times/rtt-data-2025-26"
    ) {
      return(c(
        `Skip to main content` = "#main-content",
        `read more about our cookies` = "https://www.england.nhs.uk/privacy-policy/",
        Home = "https://www.england.nhs.uk/",
        News = "https://www.england.nhs.uk/news/",
        Publications = "https://www.england.nhs.uk/publication/",
        Statistics = "https://www.england.nhs.uk/statistics/",
        Blogs = "https://www.england.nhs.uk/blogs/",
        Events = "https://www.england.nhs.uk/events/",
        `Contact us` = "https://www.england.nhs.uk/contact-us/",
        "https://www.england.nhs.uk/ourwork/",
        `About us` = "https://www.england.nhs.uk/about/",
        `Our work` = "https://www.england.nhs.uk/ourwork",
        Commissioning = "https://www.england.nhs.uk/commissioning/",
        `Get involved` = "https://www.england.nhs.uk/participation/",
        Statistics = "https://www.england.nhs.uk/statistics",
        `Statistical work areas` = "https://www.england.nhs.uk/statistics/statistical-work-areas/",
        `Acute provider table` = "https://www.england.nhs.uk/statistics/statistical-work-areas/acute-provider-table/",
        `2-hour Urgent Community Response` = "https://www.england.nhs.uk/statistics/statistical-work-areas/2-hour-urgent-community-response/",
        `A&E Attendances and Emergency Admissions` = "https://www.england.nhs.uk/statistics/statistical-work-areas/ae-waiting-times-and-activity/",
        `Ambulance Quality Indicators` = "https://www.england.nhs.uk/statistics/statistical-work-areas/ambulance-quality-indicators/",
        `Bed Availability and Occupancy` = "https://www.england.nhs.uk/statistics/statistical-work-areas/bed-availability-and-occupancy/",
        `Cancelled Elective Operations` = "https://www.england.nhs.uk/statistics/statistical-work-areas/cancelled-elective-operations/",
        `National Cancer Patient Experience Survey` = "https://www.england.nhs.uk/statistics/statistical-work-areas/cancer-patient-experience-survey/",
        `Under 16 Cancer Patient Experience Survey` = "https://www.england.nhs.uk/statistics/statistical-work-areas/under-16-cancer-patient-experience-survey/",
        `Cancer Waiting Times` = "https://www.england.nhs.uk/statistics/statistical-work-areas/cancer-waiting-times/",
        `Community health services waiting lists` = "https://www.england.nhs.uk/statistics/statistical-work-areas/community-health-services-waiting-lists/",
        `Continuing Healthcare and NHS-funded Nursing Care` = "https://www.england.nhs.uk/statistics/statistical-work-areas/nhs-chc-fnc/",
        `COVID-19 Hospital Activity` = "https://www.england.nhs.uk/statistics/statistical-work-areas/covid-19-hospital-activity/",
        `Critical Care Bed Capacity and Urgent Operations Cancelled` = "https://www.england.nhs.uk/statistics/statistical-work-areas/critical-care-capacity/",
        `Dental commissioning` = "https://www.england.nhs.uk/statistics/statistical-work-areas/dental-commissioning/",
        `Dental Workforce` = "https://www.england.nhs.uk/statistics/statistical-work-areas/dental-workforce/",
        `Diagnostic Imaging Dataset` = "https://www.england.nhs.uk/statistics/statistical-work-areas/diagnostic-imaging-dataset/",
        `Diagnostics Waiting Times and Activity` = "https://www.england.nhs.uk/statistics/statistical-work-areas/diagnostics-waiting-times-and-activity/",
        `Discharge Delays` = "https://www.england.nhs.uk/statistics/statistical-work-areas/discharge-delays/",
        `Friends and Family Test` = "https://www.england.nhs.uk/statistics/statistical-work-areas/friends-and-family-test/",
        `Genomic Testing activity` = "https://www.england.nhs.uk/statistics/statistical-work-areas/genomic-testing-activity/",
        `GP Patient Survey` = "https://www.england.nhs.uk/statistics/statistical-work-areas/gp-patient-survey/",
        `Health Visitors` = "https://www.england.nhs.uk/statistics/statistical-work-areas/health-visitors/",
        `Integrated Urgent Care` = "https://www.england.nhs.uk/statistics/statistical-work-areas/iucadc-new-from-april-2021/",
        `Intermediate Care` = "https://www.england.nhs.uk/statistics/statistical-work-areas/intermediate-care/",
        `Martha’s Rule` = "https://www.england.nhs.uk/statistics/statistical-work-areas/marthas-rule/",
        `Mental health: children and young people with an eating disorder waiting times` = "https://www.england.nhs.uk/statistics/statistical-work-areas/cyped-waiting-times/",
        `Mental health: physical health checks for people with severe mental illness` = "https://www.england.nhs.uk/statistics/statistical-work-areas/serious-mental-illness-smi/",
        `Mixed-sex accommodation` = "https://www.england.nhs.uk/statistics/statistical-work-areas/mixed-sex-accommodation/",
        `National Diabetes Experience Survey (NDES)` = "https://www.england.nhs.uk/statistics/statistical-work-areas/national-diabetes-experience-survey-ndes/",
        `National Patient and Staff Surveys` = "https://www.england.nhs.uk/statistics/statistical-work-areas/patient-surveys/",
        `NHS Staff Survey in England` = "https://www.england.nhs.uk/statistics/statistical-work-areas/nhs-staff-survey-in-england/",
        `Outpatient Recovery and Transformation` = "https://www.england.nhs.uk/statistics/statistical-work-areas/outpatient-transformation/",
        `Patient Reported Outcome Measures (PROMs)` = "https://www.england.nhs.uk/statistics/statistical-work-areas/proms/",
        `Patient safety data` = "https://www.england.nhs.uk/statistics/statistical-work-areas/patient-safety-data/",
        `Referral to Treatment (RTT) Waiting Times` = "https://www.england.nhs.uk/statistics/statistical-work-areas/rtt-waiting-times/",
        `Waiting List Minimum Data Set (WLMDS) Information` = "https://www.england.nhs.uk/statistics/statistical-work-areas/rtt-waiting-times/wlmds/",
        `Recovery of Elective Activity` = "https://www.england.nhs.uk/statistics/statistical-work-areas/rtt-waiting-times/recovery-of-elective_activity-mi/",
        `Consultant-led Referral to Treatment Waiting Times Data 2025-26` = "https://www.england.nhs.uk/statistics/statistical-work-areas/rtt-waiting-times/rtt-data-2025-26/",
        `Consultant-led Referral to Treatment Waiting Times Rules and Guidance` = "https://www.england.nhs.uk/statistics/statistical-work-areas/rtt-waiting-times/rtt-guidance/",
        `Consultant-led Referral to Treatment Waiting Times Data 2024-25` = "https://www.england.nhs.uk/statistics/statistical-work-areas/rtt-waiting-times/rtt-data-2024-25/",
        `Consultant-led Referral to Treatment Waiting Times Data 2023-24` = "https://www.england.nhs.uk/statistics/statistical-work-areas/rtt-waiting-times/rtt-data-2023-24/",
        `Consultant-led Referral to Treatment Waiting Times Data 2022-23` = "https://www.england.nhs.uk/statistics/statistical-work-areas/rtt-waiting-times/rtt-data-2022-23/",
        `Consultant-led Referral to Treatment Waiting Times Data 2021-22` = "https://www.england.nhs.uk/statistics/statistical-work-areas/rtt-waiting-times/rtt-data-2021-22/",
        `Consultant-led Referral to Treatment Waiting Times Data 2020-21` = "https://www.england.nhs.uk/statistics/statistical-work-areas/rtt-waiting-times/rtt-data-2020-21/",
        `Consultant-led Referral to Treatment Waiting Times Data 2019-20` = "https://www.england.nhs.uk/statistics/statistical-work-areas/rtt-waiting-times/rtt-data-2019-20/",
        `Consultant-led Referral to Treatment Waiting Times Data 2018-19` = "https://www.england.nhs.uk/statistics/statistical-work-areas/rtt-waiting-times/rtt-data-2018-19/",
        `Consultant-led Referral to Treatment Waiting Times Data 2017-18` = "https://www.england.nhs.uk/statistics/statistical-work-areas/rtt-waiting-times/rtt-data-2017-18/",
        `Consultant-led Referral to Treatment Waiting Times Data 2016-17` = "https://www.england.nhs.uk/statistics/statistical-work-areas/rtt-waiting-times/rtt-data-2016-17/",
        `Consultant-led Referral to Treatment Waiting Times Data 2015-16` = "https://www.england.nhs.uk/statistics/statistical-work-areas/rtt-waiting-times/rtt-data-2015-16/",
        `Consultant-led Referral to Treatment Waiting Times Data 2014-15` = "https://www.england.nhs.uk/statistics/statistical-work-areas/rtt-waiting-times/rtt-data-2014-15/",
        `Consultant-led Referral to Treatment Waiting Times Data 2013-14` = "https://www.england.nhs.uk/statistics/statistical-work-areas/rtt-waiting-times/rtt-data-2013-14/",
        `Consultant-led Referral to Treatment Waiting Times Data 2012-13` = "https://www.england.nhs.uk/statistics/statistical-work-areas/rtt-waiting-times/rtt-data-2012-13/",
        `Consultant-led Referral to Treatment Waiting Times Data 2011-12` = "https://www.england.nhs.uk/statistics/statistical-work-areas/rtt-waiting-times/rtt-data-2011-12/",
        Screening = "https://www.england.nhs.uk/statistics/statistical-work-areas/screening/",
        `Urgent and Emergency Care Daily Situation Reports` = "https://www.england.nhs.uk/statistics/statistical-work-areas/uec-sitrep/",
        `Vaccinations: Child Immunisation` = "https://www.england.nhs.uk/statistics/statistical-work-areas/child-immunisation/",
        `Vaccinations: COVID-19` = "https://www.england.nhs.uk/statistics/statistical-work-areas/covid-19-vaccinations/",
        `Vaccinations: Flu` = "https://www.england.nhs.uk/statistics/statistical-work-areas/flu-vaccinations/",
        `Vaccinations: RSV` = "https://www.england.nhs.uk/statistics/statistical-work-areas/vaccinations-rsv/",
        `Vaccinations: Mpox` = "https://www.england.nhs.uk/statistics/statistical-work-areas/vaccinations-for-mpox/",
        `Vaccinations: Polio` = "https://www.england.nhs.uk/statistics/statistical-work-areas/polio-vaccinations-in-london-region/",
        `Venous thromboembolism (VTE) risk assessment` = "https://www.england.nhs.uk/statistics/statistical-work-areas/vte/",
        `Virtual Ward` = "https://www.england.nhs.uk/statistics/statistical-work-areas/virtual-ward/",
        `Wheelchair data collection` = "https://www.england.nhs.uk/statistics/statistical-work-areas/national-wheelchair/",
        `(Discontinued) Combined Performance Summary` = "https://www.england.nhs.uk/statistics/statistical-work-areas/combined-performance-summary/",
        `(Discontinued) COVID-19 Deaths` = "https://www.england.nhs.uk/statistics/statistical-work-areas/covid-19-deaths/",
        `(Discontinued) COVID-19 Post-Covid Assessment Service` = "https://www.england.nhs.uk/statistics/statistical-work-areas/covid-19-post-covid-assessment-service/",
        `(Discontinued) Delayed Transfers of Care` = "https://www.england.nhs.uk/statistics/statistical-work-areas/delayed-transfers-of-care/",
        `(Discontinued) Dementia Assessment and Referral` = "https://www.england.nhs.uk/statistics/statistical-work-areas/dementia/",
        `(Discontinued) Direct Access Audiology` = "https://www.england.nhs.uk/statistics/statistical-work-areas/direct-access-audiology/",
        `(Discontinued) Discharge delays (community)` = "https://www.england.nhs.uk/statistics/statistical-work-areas/discharge-delays-community-data/",
        `(Discontinued) Early Intervention in Psychosis Waiting Times` = "https://www.england.nhs.uk/statistics/statistical-work-areas/eip-waiting-times/",
        `(Discontinued) GP Extended Access` = "https://www.england.nhs.uk/statistics/statistical-work-areas/extended-access-general-practice/",
        `(Discontinued) Hospital Activity` = "https://www.england.nhs.uk/statistics/statistical-work-areas/hospital-activity/",
        `(Discontinued) Integrated Performance Measures Monitoring` = "https://www.england.nhs.uk/statistics/statistical-work-areas/integrated-performance-measures-monitoring/",
        `(Discontinued) Maternity and Breastfeeding` = "https://www.england.nhs.uk/statistics/statistical-work-areas/maternity-and-breastfeeding/",
        `(Discontinued) Mental Health Community Teams Activity` = "https://www.england.nhs.uk/statistics/statistical-work-areas/mental-health-community-teams-activity/",
        `(Discontinued) Patient Experience Scores Overall` = "https://www.england.nhs.uk/statistics/statistical-work-areas/pat-exp/",
        `(Discontinued) Referral for Outpatient` = "https://www.england.nhs.uk/statistics/statistical-work-areas/outpatient-referrals/",
        `(Discontinued) Winter Daily Situation Reports` = "https://www.england.nhs.uk/statistics/statistical-work-areas/winter-daily-sitreps/",
        `Supplementary information` = "https://www.england.nhs.uk/statistics/statistical-work-areas/supplementary-information/",
        Home = "/",
        Statistics = "https://www.england.nhs.uk/statistics",
        `Statistical work areas` = "https://www.england.nhs.uk/statistics/statistical-work-areas/",
        `Referral to Treatment (RTT) Waiting Times` = "https://www.england.nhs.uk/statistics/statistical-work-areas/rtt-waiting-times/",
        `Click here to return to main RTT page` = "https://www.england.nhs.uk/statistics/rtt-waiting-times/",
        `revisions policy.` = "https://www.england.nhs.uk/statistics/code-compliance/#Unifypolicy",
        Apr25 = "#Apr25",
        May25 = "#May25",
        Jun25 = "#Jun25",
        Jul25 = "#Jul25",
        `RTT Overview Timeseries Including Estimates for Missing Trusts Jul25 (XLS, 114K)` = "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2025/09/RTT-Overview-Timeseries-Including-Estimates-for-Missing-Trusts-Jul25-XLS-114K-92707.xlsx",
        `Download Waiting Times by Hospital Trust Jul25 (XLS, 10M)` = "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2025/09/Download-Waiting-Times-by-Hospital-Trust-Jul25-XLS-10M-92707.xls",
        `RTT statistical press notice Jul25 (PDF, 466K)` = "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2025/09/Jul25-RTT-SPN-Publication-PDF-466K-92707.pdf",
        `Full CSV data file Jul25 (ZIP, 4M)` = "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2025/09/Full-CSV-data-file-Jul25-ZIP-4M-92707.zip",
        NA,
        `Incomplete Commissioner Jul25 (XLSX, 4M)` = "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2025/09/Incomplete-Commissioner-Jul25-XLSX-4M-92707.xlsx",
        `Incomplete Provider Jul25 (XLSX, 9M)` = "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2025/09/Incomplete-Provider-Jul25-XLSX-9M-92707.xlsx",
        `Admitted Commissioner Jul25 (XLSX, 2M)` = "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2025/09/Admitted-Commissioner-Jul25-XLSX-2M-92707.xlsx",
        `NonAdmitted Commissioner Jul25 (XLSX, 2M)` = "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2025/09/NonAdmitted-Commissioner-Jul25-XLSX-2M-92707.xlsx",
        `New Periods Commissioner Jul25 (XLSX, 157K)` = "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2025/09/New-Periods-Commissioner-Jul25-XLSX-157K-92707.xlsx",
        `Admitted Provider Jul25 (XLSX, 4M)` = "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2025/09/Admitted-Provider-Jul25-XLSX-4M-92707.xlsx",
        `NonAdmitted Provider Jul25 (XLSX, 4M)` = "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2025/09/NonAdmitted-Provider-Jul25-XLSX-4M-92707.xlsx",
        `New Periods Provider Jul25 (XLSX, 352K)` = "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2025/09/New-Periods-Provider-Jul25-XLSX-352K-92707.xlsx",
        `Full CSV data file Jul25 (ZIP, 4M)` = "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2025/09/Full-CSV-data-file-Jul25-ZIP-4M-92707.zip",
        NA,
        `Incomplete Commissioner Jun25 (XLSX, 4M)` = "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2025/08/Incomplete-Commissioner-Jun25-XLSX-4M-94367.xlsx",
        `Incomplete Provider Jun25 (XLSX, 9M)` = "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2025/08/Incomplete-Provider-Jun25-XLSX-9M-94367.xlsx",
        `Admitted Commissioner Jun25 (XLSX, 2M)` = "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2025/08/Admitted-Commissioner-Jun25-XLSX-2M-94367.xlsx",
        `NonAdmitted Commissioner Jun25 (XLSX, 2M)` = "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2025/08/NonAdmitted-Commissioner-Jun25-XLSX-2M-94367.xlsx",
        `New Periods Commissioner Jun25 (XLSX, 157K)` = "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2025/08/New-Periods-Commissioner-Jun25-XLSX-157K-94367.xlsx",
        `Admitted Provider Jun25 (XLSX, 4M)` = "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2025/08/Admitted-Provider-Jun25-XLSX-4M-94367.xlsx",
        `NonAdmitted Provider Jun25 (XLSX, 4M)` = "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2025/08/NonAdmitted-Provider-Jun25-XLSX-4M-94367.xlsx",
        `New Periods Provider Jun25 (XLSX, 351K)` = "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2025/08/New-Periods-Provider-Jun25-XLSX-351K-94367.xlsx",
        `Full CSV data file Jun25 (ZIP, 4M)` = "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2025/08/Full-CSV-data-file-Jun25-ZIP-4M-94367.zip",
        NA,
        `Incomplete Commissioner May25 (XLSX, 4M)` = "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2025/07/Incomplete-Commissioner-May25-XLSX-4M-32711.xlsx",
        `Incomplete Provider May25 (XLSX, 9M)` = "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2025/07/Incomplete-Provider-May25-XLSX-9M-32711.xlsx",
        `Admitted Commissioner May25 (XLSX, 2M)` = "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2025/07/Admitted-Commissioner-May25-XLSX-2M-32711.xlsx",
        `NonAdmitted Commissioner May25 (XLSX, 2M)` = "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2025/07/NonAdmitted-Commissioner-May25-XLSX-2M-32711.xlsx",
        `New Periods Commissioner May25 (XLSX, 157K)` = "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2025/07/New-Periods-Commissioner-May25-XLSX-157K-32711.xlsx",
        `Admitted Provider May25 (XLSX, 4M)` = "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2025/07/Admitted-Provider-May25-XLSX-4M-32711.xlsx",
        `NonAdmitted Provider May25 (XLSX, 4M)` = "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2025/07/NonAdmitted-Provider-May25-XLSX-4M-32711.xlsx",
        `New Periods Provider May25 (XLSX, 349K)` = "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2025/07/New-Periods-Provider-May25-XLSX-349K-32711.xlsx",
        `Full CSV data file May25 (ZIP, 4M)` = "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2025/07/Full-CSV-data-file-May25-ZIP-4M-32711.zip",
        NA,
        `Incomplete Commissioner Apr25 (XLSX, 4M)` = "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2025/06/Incomplete-Commissioner-Apr25-XLSX-4M-77252.xlsx",
        `Incomplete Provider Apr25 (XLSX, 9M)` = "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2025/06/Incomplete-Provider-Apr25-XLSX-9M-77252.xlsx",
        `Admitted Commissioner Apr25 (XLSX, 2M)` = "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2025/06/Admitted-Commissioner-Apr25-XLSX-2M-77252.xlsx",
        `NonAdmitted Commissioner Apr25 (XLSX, 2M)` = "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2025/06/NonAdmitted-Commissioner-Apr25-XLSX-2M-77252.xlsx",
        `New Periods Commissioner Apr25 (XLSX, 157K)` = "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2025/06/New-Periods-Commissioner-Apr25-XLSX-157K-77252.xlsx",
        `Admitted Provider Apr25 (XLSX, 4M)` = "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2025/06/Admitted-Provider-Apr25-XLSX-4M-77252.xlsx",
        `NonAdmitted Provider Apr25 (XLSX, 4M)` = "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2025/06/NonAdmitted-Provider-Apr25-XLSX-4M-77252.xlsx",
        `New Periods Provider Apr25 (XLSX, 349K)` = "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2025/06/New-Periods-Provider-Apr25-XLSX-349K-77252.xlsx",
        `Full CSV data file Apr25 (ZIP, 4M)` = "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2025/06/Full-CSV-data-file-Apr25-ZIP-4M-77252.zip",
        Statistics = "https://www.england.nhs.uk/statistics",
        `Statistical work areas` = "https://www.england.nhs.uk/statistics/statistical-work-areas/",
        `Acute provider table` = "https://www.england.nhs.uk/statistics/statistical-work-areas/acute-provider-table/",
        `2-hour Urgent Community Response` = "https://www.england.nhs.uk/statistics/statistical-work-areas/2-hour-urgent-community-response/",
        `A&E Attendances and Emergency Admissions` = "https://www.england.nhs.uk/statistics/statistical-work-areas/ae-waiting-times-and-activity/",
        `Ambulance Quality Indicators` = "https://www.england.nhs.uk/statistics/statistical-work-areas/ambulance-quality-indicators/",
        `Bed Availability and Occupancy` = "https://www.england.nhs.uk/statistics/statistical-work-areas/bed-availability-and-occupancy/",
        `Cancelled Elective Operations` = "https://www.england.nhs.uk/statistics/statistical-work-areas/cancelled-elective-operations/",
        `National Cancer Patient Experience Survey` = "https://www.england.nhs.uk/statistics/statistical-work-areas/cancer-patient-experience-survey/",
        `Under 16 Cancer Patient Experience Survey` = "https://www.england.nhs.uk/statistics/statistical-work-areas/under-16-cancer-patient-experience-survey/",
        `Cancer Waiting Times` = "https://www.england.nhs.uk/statistics/statistical-work-areas/cancer-waiting-times/",
        `Community health services waiting lists` = "https://www.england.nhs.uk/statistics/statistical-work-areas/community-health-services-waiting-lists/",
        `Continuing Healthcare and NHS-funded Nursing Care` = "https://www.england.nhs.uk/statistics/statistical-work-areas/nhs-chc-fnc/",
        `COVID-19 Hospital Activity` = "https://www.england.nhs.uk/statistics/statistical-work-areas/covid-19-hospital-activity/",
        `Critical Care Bed Capacity and Urgent Operations Cancelled` = "https://www.england.nhs.uk/statistics/statistical-work-areas/critical-care-capacity/",
        `Dental commissioning` = "https://www.england.nhs.uk/statistics/statistical-work-areas/dental-commissioning/",
        `Dental Workforce` = "https://www.england.nhs.uk/statistics/statistical-work-areas/dental-workforce/",
        `Diagnostic Imaging Dataset` = "https://www.england.nhs.uk/statistics/statistical-work-areas/diagnostic-imaging-dataset/",
        `Diagnostics Waiting Times and Activity` = "https://www.england.nhs.uk/statistics/statistical-work-areas/diagnostics-waiting-times-and-activity/",
        `Discharge Delays` = "https://www.england.nhs.uk/statistics/statistical-work-areas/discharge-delays/",
        `Friends and Family Test` = "https://www.england.nhs.uk/statistics/statistical-work-areas/friends-and-family-test/",
        `Genomic Testing activity` = "https://www.england.nhs.uk/statistics/statistical-work-areas/genomic-testing-activity/",
        `GP Patient Survey` = "https://www.england.nhs.uk/statistics/statistical-work-areas/gp-patient-survey/",
        `Health Visitors` = "https://www.england.nhs.uk/statistics/statistical-work-areas/health-visitors/",
        `Integrated Urgent Care` = "https://www.england.nhs.uk/statistics/statistical-work-areas/iucadc-new-from-april-2021/",
        `Intermediate Care` = "https://www.england.nhs.uk/statistics/statistical-work-areas/intermediate-care/",
        `Martha’s Rule` = "https://www.england.nhs.uk/statistics/statistical-work-areas/marthas-rule/",
        `Mental health: children and young people with an eating disorder waiting times` = "https://www.england.nhs.uk/statistics/statistical-work-areas/cyped-waiting-times/",
        `Mental health: physical health checks for people with severe mental illness` = "https://www.england.nhs.uk/statistics/statistical-work-areas/serious-mental-illness-smi/",
        `Mixed-sex accommodation` = "https://www.england.nhs.uk/statistics/statistical-work-areas/mixed-sex-accommodation/",
        `National Diabetes Experience Survey (NDES)` = "https://www.england.nhs.uk/statistics/statistical-work-areas/national-diabetes-experience-survey-ndes/",
        `National Patient and Staff Surveys` = "https://www.england.nhs.uk/statistics/statistical-work-areas/patient-surveys/",
        `NHS Staff Survey in England` = "https://www.england.nhs.uk/statistics/statistical-work-areas/nhs-staff-survey-in-england/",
        `Outpatient Recovery and Transformation` = "https://www.england.nhs.uk/statistics/statistical-work-areas/outpatient-transformation/",
        `Patient Reported Outcome Measures (PROMs)` = "https://www.england.nhs.uk/statistics/statistical-work-areas/proms/",
        `Patient safety data` = "https://www.england.nhs.uk/statistics/statistical-work-areas/patient-safety-data/",
        `Referral to Treatment (RTT) Waiting Times` = "https://www.england.nhs.uk/statistics/statistical-work-areas/rtt-waiting-times/",
        `Waiting List Minimum Data Set (WLMDS) Information` = "https://www.england.nhs.uk/statistics/statistical-work-areas/rtt-waiting-times/wlmds/",
        `Recovery of Elective Activity` = "https://www.england.nhs.uk/statistics/statistical-work-areas/rtt-waiting-times/recovery-of-elective_activity-mi/",
        `Consultant-led Referral to Treatment Waiting Times Data 2025-26` = "https://www.england.nhs.uk/statistics/statistical-work-areas/rtt-waiting-times/rtt-data-2025-26/",
        `Consultant-led Referral to Treatment Waiting Times Rules and Guidance` = "https://www.england.nhs.uk/statistics/statistical-work-areas/rtt-waiting-times/rtt-guidance/",
        `Consultant-led Referral to Treatment Waiting Times Data 2024-25` = "https://www.england.nhs.uk/statistics/statistical-work-areas/rtt-waiting-times/rtt-data-2024-25/",
        `Consultant-led Referral to Treatment Waiting Times Data 2023-24` = "https://www.england.nhs.uk/statistics/statistical-work-areas/rtt-waiting-times/rtt-data-2023-24/",
        `Consultant-led Referral to Treatment Waiting Times Data 2022-23` = "https://www.england.nhs.uk/statistics/statistical-work-areas/rtt-waiting-times/rtt-data-2022-23/",
        `Consultant-led Referral to Treatment Waiting Times Data 2021-22` = "https://www.england.nhs.uk/statistics/statistical-work-areas/rtt-waiting-times/rtt-data-2021-22/",
        `Consultant-led Referral to Treatment Waiting Times Data 2020-21` = "https://www.england.nhs.uk/statistics/statistical-work-areas/rtt-waiting-times/rtt-data-2020-21/",
        `Consultant-led Referral to Treatment Waiting Times Data 2019-20` = "https://www.england.nhs.uk/statistics/statistical-work-areas/rtt-waiting-times/rtt-data-2019-20/",
        `Consultant-led Referral to Treatment Waiting Times Data 2018-19` = "https://www.england.nhs.uk/statistics/statistical-work-areas/rtt-waiting-times/rtt-data-2018-19/",
        `Consultant-led Referral to Treatment Waiting Times Data 2017-18` = "https://www.england.nhs.uk/statistics/statistical-work-areas/rtt-waiting-times/rtt-data-2017-18/",
        `Consultant-led Referral to Treatment Waiting Times Data 2016-17` = "https://www.england.nhs.uk/statistics/statistical-work-areas/rtt-waiting-times/rtt-data-2016-17/",
        `Consultant-led Referral to Treatment Waiting Times Data 2015-16` = "https://www.england.nhs.uk/statistics/statistical-work-areas/rtt-waiting-times/rtt-data-2015-16/",
        `Consultant-led Referral to Treatment Waiting Times Data 2014-15` = "https://www.england.nhs.uk/statistics/statistical-work-areas/rtt-waiting-times/rtt-data-2014-15/",
        `Consultant-led Referral to Treatment Waiting Times Data 2013-14` = "https://www.england.nhs.uk/statistics/statistical-work-areas/rtt-waiting-times/rtt-data-2013-14/",
        `Consultant-led Referral to Treatment Waiting Times Data 2012-13` = "https://www.england.nhs.uk/statistics/statistical-work-areas/rtt-waiting-times/rtt-data-2012-13/",
        `Consultant-led Referral to Treatment Waiting Times Data 2011-12` = "https://www.england.nhs.uk/statistics/statistical-work-areas/rtt-waiting-times/rtt-data-2011-12/",
        Screening = "https://www.england.nhs.uk/statistics/statistical-work-areas/screening/",
        `Urgent and Emergency Care Daily Situation Reports` = "https://www.england.nhs.uk/statistics/statistical-work-areas/uec-sitrep/",
        `Vaccinations: Child Immunisation` = "https://www.england.nhs.uk/statistics/statistical-work-areas/child-immunisation/",
        `Vaccinations: COVID-19` = "https://www.england.nhs.uk/statistics/statistical-work-areas/covid-19-vaccinations/",
        `Vaccinations: Flu` = "https://www.england.nhs.uk/statistics/statistical-work-areas/flu-vaccinations/",
        `Vaccinations: RSV` = "https://www.england.nhs.uk/statistics/statistical-work-areas/vaccinations-rsv/",
        `Vaccinations: Mpox` = "https://www.england.nhs.uk/statistics/statistical-work-areas/vaccinations-for-mpox/",
        `Vaccinations: Polio` = "https://www.england.nhs.uk/statistics/statistical-work-areas/polio-vaccinations-in-london-region/",
        `Venous thromboembolism (VTE) risk assessment` = "https://www.england.nhs.uk/statistics/statistical-work-areas/vte/",
        `Virtual Ward` = "https://www.england.nhs.uk/statistics/statistical-work-areas/virtual-ward/",
        `Wheelchair data collection` = "https://www.england.nhs.uk/statistics/statistical-work-areas/national-wheelchair/",
        `(Discontinued) Combined Performance Summary` = "https://www.england.nhs.uk/statistics/statistical-work-areas/combined-performance-summary/",
        `(Discontinued) COVID-19 Deaths` = "https://www.england.nhs.uk/statistics/statistical-work-areas/covid-19-deaths/",
        `(Discontinued) COVID-19 Post-Covid Assessment Service` = "https://www.england.nhs.uk/statistics/statistical-work-areas/covid-19-post-covid-assessment-service/",
        `(Discontinued) Delayed Transfers of Care` = "https://www.england.nhs.uk/statistics/statistical-work-areas/delayed-transfers-of-care/",
        `(Discontinued) Dementia Assessment and Referral` = "https://www.england.nhs.uk/statistics/statistical-work-areas/dementia/",
        `(Discontinued) Direct Access Audiology` = "https://www.england.nhs.uk/statistics/statistical-work-areas/direct-access-audiology/",
        `(Discontinued) Discharge delays (community)` = "https://www.england.nhs.uk/statistics/statistical-work-areas/discharge-delays-community-data/",
        `(Discontinued) Early Intervention in Psychosis Waiting Times` = "https://www.england.nhs.uk/statistics/statistical-work-areas/eip-waiting-times/",
        `(Discontinued) GP Extended Access` = "https://www.england.nhs.uk/statistics/statistical-work-areas/extended-access-general-practice/",
        `(Discontinued) Hospital Activity` = "https://www.england.nhs.uk/statistics/statistical-work-areas/hospital-activity/",
        `(Discontinued) Integrated Performance Measures Monitoring` = "https://www.england.nhs.uk/statistics/statistical-work-areas/integrated-performance-measures-monitoring/",
        `(Discontinued) Maternity and Breastfeeding` = "https://www.england.nhs.uk/statistics/statistical-work-areas/maternity-and-breastfeeding/",
        `(Discontinued) Mental Health Community Teams Activity` = "https://www.england.nhs.uk/statistics/statistical-work-areas/mental-health-community-teams-activity/",
        `(Discontinued) Patient Experience Scores Overall` = "https://www.england.nhs.uk/statistics/statistical-work-areas/pat-exp/",
        `(Discontinued) Referral for Outpatient` = "https://www.england.nhs.uk/statistics/statistical-work-areas/outpatient-referrals/",
        `(Discontinued) Winter Daily Situation Reports` = "https://www.england.nhs.uk/statistics/statistical-work-areas/winter-daily-sitreps/",
        `Supplementary information` = "https://www.england.nhs.uk/statistics/statistical-work-areas/supplementary-information/",
        `Terms and conditions` = "https://www.england.nhs.uk/terms-and-conditions/",
        `Privacy and cookies` = "https://www.england.nhs.uk/privacy-policy/",
        `Social media and comment moderation` = "https://www.england.nhs.uk/comment-policy/",
        `How could this website work better for you?` = "https://www.england.nhs.uk/contact-us/feedback/",
        `Accessibility statement` = "https://www.england.nhs.uk/accessibility/",
        `Open Government Licence v3.0` = "https://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/",
        `Follow us on Twitter` = "http://www.twitter.com/NHSEngland",
        `Visit us on LinkedIn` = "https://www.linkedin.com/company/nhsengland",
        `Watch videos on YouTube` = "http://www.youtube.com/thenhsengland",
        `View photos on Flickr` = "http://www.flickr.com/photos/nhsengland",
        `All RSS` = "http://feeds.feedburner.com/NHSCBoard",
        "https://www.nhs.uk/"
      ))
    } else {
      return(character(0))
    }
  }

  mock_download_unzip_files <- function(url) {
    return(test_sheet("20241130-RTT-November-2024-RBD.csv"))
  }

  # Use local_mocked_bindings to temporarily override obtain_links
  local_mocked_bindings(
    obtain_links = mock_obtain_links,
    download_unzip_files = mock_download_unzip_files
  )

  result <- latest_rtt_file(
    "https://www.england.nhs.uk/statistics/statistical-work-areas/rtt-waiting-times/"
  )

  expected <- c(
    "Full CSV data file Jul25 (ZIP, 4M)" = "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2025/09/Full-CSV-data-file-Jul25-ZIP-4M-92707.zip"
  )

  expect_equal(result, expected)

  # latest_rtt_date
  latest_date <- latest_rtt_date()
  expect_equal(
    latest_date,
    lubridate::ymd("2025-07-31")
  )

  orgs <- latest_orgs()

  expect_equal(
    names(orgs),
    c(
      "Provider Parent Org Code",
      "Provider Parent Name",
      "Provider Org Code",
      "Provider Org Name",
      "Commissioner Parent Org Code",
      "Commissioner Parent Name",
      "Commissioner Org Code",
      "Commissioner Org Name",
      "NHS Region Code",
      "NHS Region Name"
    ),
    info = "expected names in org lookup"
  )
})

devtools::load_all()
library(dplyr)
library(lubridate)
min_date <- as.Date("2023-03-01")
max_date <- as.Date("2025-03-01")

df <- seq(
  from = min_date,
  to = max_date,
  by = "months"
) |>
  purrr::map(
    \(x) {
      cat(paste(x, "\n\n"))
      get_rtt_data(
        date_start = x,
        date_end = x
      )
    }
  ) |>
  purrr::list_rbind()

# saveRDS(df, "ad_hoc/linear programming solution/junk.rds")
# df <- readRDS("ad_hoc/linear programming solution/junk.rds")

# create geography lkps
org_lkp <- latest_orgs()

trust_name_lkp <- org_lkp |>
  select(
    trust = `Provider Org Code`,
    trust_name = `Provider Org Name`,
    region_name = `NHS Region Name`
  ) |>
  distinct() |>
  filter(grepl("NHS", trust_name)) |>
  dtplyr::lazy_dt()

create_modelling_data <- function(data, max_months_waited = 24) {
  required_fields <- c(
    "trust",
    "specialty",
    "period_id",
    "type",
    "months_waited_id",
    "value"
  )

  if (length(dplyr::setdiff(required_fields, names(data)) > 0)) {
    stop(
      "incorrect fields in data object - requires 'trust', 'specialty', 'period_id', 'type', 'months_waited_id', 'value'"
    )
  }

  periods <- unique(
    sort(
      data$period_id
    )
  )

  months_waited <- unique(
    sort(
      data$months_waited_id
    )
  )

  specialties <- unique(
    sort(
      data$specialty
    )
  )

  referrals <- data |>
    filter(
      .data$type == "Referrals"
    ) |>
    distinct(
      .data$trust,
      .data$specialty,
      .data$period_id,
      .data$value
    ) |>
    rename(
      referrals = "value"
    ) |>
    tidyr::complete(
      period_id = periods,
      specialty = specialties,
      .data$trust,
      fill = list(referrals = 0)
    )

  referrals <- referrals |>
    tidyr::nest(
      referrals_data = c(
        "period_id",
        "referrals"
      )
    )

  completes <- data |>
    filter(
      .data$type == "Complete"
    ) |>
    distinct(
      .data$trust,
      .data$specialty,
      .data$period_id,
      .data$months_waited_id,
      .data$value
    ) |>
    rename(
      treatments = "value"
    ) |>
    tidyr::complete(
      specialty = specialties,
      period_id = periods,
      months_waited_id = months_waited,
      .data$trust,
      fill = list(treatments = 0)
    ) |>
    tidyr::nest(
      completes_data = c(
        "period_id",
        "months_waited_id",
        "treatments"
      )
    )

  incompletes <- data |>
    filter(
      .data$type == "Incomplete"
    ) |>
    distinct(
      .data$trust,
      .data$specialty,
      .data$period_id,
      .data$months_waited_id,
      .data$value
    ) |>
    rename(
      incompletes = "value"
    ) |>
    tidyr::complete(
      specialty = specialties,
      period_id = periods,
      months_waited_id = months_waited,
      .data$trust,
      fill = list(incompletes = 0)
    ) |>
    tidyr::nest(
      incompletes_data = c(
        "period_id",
        "months_waited_id",
        "incompletes"
      )
    )

  all_calibration_data <- completes |>
    left_join(
      referrals,
      by = join_by(
        trust,
        specialty
      )
    ) |>
    left_join(
      incompletes,
      by = join_by(
        trust,
        specialty
      )
    )

  return(all_calibration_data)
}

period_lkp <- dplyr::tibble(
  period = seq(from = min_date, to = max_date, by = "months")
) |>
  mutate(
    period_id = dplyr::row_number()
  ) |>
  dtplyr::lazy_dt()

mnth_wtd_lkp <- df |>
  distinct(months_waited) |>
  arrange(months_waited) |>
  mutate(months_waited_id = dplyr::row_number() - 1) |>
  dtplyr::lazy_dt()

specialty_lkp <- dplyr::tribble(
  ~specialty                  ,
  ~specialty_name             ,
  "C_100"                     ,
  "General Surgery"           ,
  "C_101"                     ,
  "Urology"                   ,
  "C_110"                     ,
  "Trauma and Orthopaedic"    ,
  "C_120"                     ,
  "Ear Nose and Throat"       ,
  "C_130"                     ,
  "Ophthalmology"             ,
  "C_140"                     ,
  "Oral Surgery"              ,
  "C_150"                     ,
  "Neurosurgical"             ,
  "C_160"                     ,
  "Plastic Surgery"           ,
  "C_170"                     ,
  "Cardiothoracic Surgery"    ,
  "C_300"                     ,
  "General Internal Medicine" ,
  "C_301"                     ,
  "Gastroenterology"          ,
  "C_320"                     ,
  "Cardiology"                ,
  "C_330"                     ,
  "Dermatology"               ,
  "C_340"                     ,
  "Respiratory Medicine"      ,
  "C_400"                     ,
  "Neurology"                 ,
  "C_410"                     ,
  "Rheumatology"              ,
  "C_430"                     ,
  "Elderly Medicine"          ,
  "C_502"                     ,
  "Gynaecology"               ,
  "C_999"                     ,
  "Total"                     ,
  "X01"                       ,
  "Other"                     ,
  "X02"                       ,
  "Other"                     ,
  "X03"                       ,
  "Other"                     ,
  "X04"                       ,
  "Other"                     ,
  "X05"                       ,
  "Other"                     ,
  "X06"                       ,
  "Other"
) |>
  dtplyr::lazy_dt()

df <- df |>
  dtplyr::lazy_dt() |>
  left_join(specialty_lkp, by = "specialty") |>
  select(!c("specialty")) |>
  rename(specialty = "specialty_name") |>
  summarise(
    value = sum(value),
    .by = c("trust", "specialty", "period", "months_waited", "type")
  ) |>
  dplyr::inner_join(trust_name_lkp, by = "trust") |>
  select(!c("trust", "region_name")) |>
  dplyr::rename(trust = "trust_name") |>
  dplyr::left_join(
    period_lkp,
    by = "period"
  ) |>
  dplyr::left_join(mnth_wtd_lkp, by = "months_waited") |>
  dplyr::select(!c("period", "months_waited"))


# calculate regional and England data
df_region <- df |>
  left_join(
    trust_name_lkp |> dplyr::select("trust_name", "region_name"),
    by = c("trust" = "trust_name")
  ) |>
  summarise(
    value = sum(value),
    .by = c("region_name", "specialty", "period_id", "months_waited_id", "type")
  ) |>
  rename(trust = "region_name") |>
  collect()

df_eng <- df_region |>
  summarise(
    value = sum(value),
    .by = c("specialty", "period_id", "months_waited_id", "type")
  ) |>
  mutate(trust = "England")


combined <- df |>
  dplyr::collect() |>
  bind_rows(df_region, df_eng) |>
  collect() |>
  create_modelling_data() |>
  mutate(
    full_data = purrr::pmap(
      .l = list(
        .data$referrals_data,
        .data$completes_data,
        .data$incompletes_data
      ),
      .f = \(ref, comp, incomp) {
        NHSRtt::calibrate_capacity_renege_params(
          referrals = ref,
          completes = comp,
          incompletes = incomp,
          max_months_waited = mnth_wtd_lkp |>
            dplyr::collect() |>
            pull(months_waited_id) |>
            max(),
          redistribute_m0_reneges = FALSE,
          full_breakdown = TRUE,
          allow_negative_params = TRUE
        )
      }
    )
  ) |>
  select("trust", "specialty", "full_data") |>
  tidyr::unnest("full_data") |>
  left_join(period_lkp |> collect(), by = "period_id") |>
  left_join(mnth_wtd_lkp |> collect(), by = "months_waited_id") |>
  select(
    "geog" = "trust",
    "specialty",
    "period",
    "months_waited",
    "Inflow" = "node_inflow",
    "Treatments" = "treatments",
    "Incompletes" = "waiting_same_node",
    "Reneges" = "reneges"
  )


# check
exp_periods <- 24 * #months
  (mnth_wtd_lkp |>
    dplyr::collect() |>
    pull(months_waited_id) |>
    (\(x) max(x) + 1)()) #compartments

combined |>
  count(geog, specialty) |>
  filter(n != exp_periods)

colSums(is.na(combined))

saveRDS(combined, "ad_hoc/linear programming solution/ss_paper_data.rds")

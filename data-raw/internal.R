# get the lateset ICB to region lkp file from here:
# https://geoportal.statistics.gov.uk/datasets/ons::sub-icb-locations-to-integrated-care-boards-to-nhser-april-2024-lookup-in-en/about

icb_lkp <- data.table::fread(
  "https://hub.arcgis.com/api/v3/datasets/cdd2e45c39e14e9eb8280789560f83a9_0/downloads/data?format=csv&spatialRefId=4326&where=1%3D1",
  showProgress = FALSE
) |>
  dplyr::select(
    "Provider Parent Org Code" = "ICB24CDH",
    "NHS Region Code" = "NHSER24CDH",
    "NHS Region Name" = "NHSER24NM"
  )

usethis::use_data(icb_lkp, internal = TRUE, overwrite = TRUE)

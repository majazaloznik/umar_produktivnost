################################################################################
#         PRODUCTIVITY REPORT SHIFT SHARE & STRUKTURA
#         revizija MZ Junij 2024, original Katarina, Junij 2019
################################################################################
####   Knjižnjice   ############################################################
library(eurostat)
library(dplyr)
library(tidyr)
####   setup    ################################################################
source("R/00_geo_lookup.R") # geografije
source("R/00_skd_lookup.R", encoding = "UTF-8") # skd klasifikaicje
source("R/helper_functions.R")

################################################################################
#                         Download Podatkov                                    #
################################################################################
VA <- get_eurostat("nama_10_a64",
                   filters = list(
                     geo = geo_subset,
                     sinceTimePeriod = 1995,
                     na_item = "B1G", # Value added, gross
                     unit = c("CP_MEUR", "CLV10_MEUR", "PYP_MEUR")))|> # Current prices, million euro
  select(-freq, -na_item) |>
  filter(!is.na(values))

EMP <- get_eurostat("nama_10_a64_e",
                    filters = list(
                      geo = geo_subset,
                      sinceTimePeriod = 1995,
                      na_item = "EMP_DC", # Total employment domestic concept
                      unit = c("THS_PER", "THS_HW")))|> # Thousand persons, Thousand hours worked
  select(-freq, -na_item) |>
  filter(!is.na(values))

################################################################################
#                 Definiranje dodatnih geografskih agregatov                   #
################################################################################
# agregiranje za VA in preračun CLV-jev
VA_agr_geo <- VA |>
  bind_rows(lapply(names(geo_aggregations), function(label) {
    aggregate_geo_and_calculate_clv_annual(VA, geo_aggregations[[label]], label)}))

# agregiranje za EMP
EMP_agr_geo <- EMP |>
  bind_rows(lapply(names(geo_aggregations), function(label) {
    aggregate_geo(EMP, geo_aggregations[[label]], label)}))

################################################################################
#                         Definiranje dodatnih SKD agregatov                   #
################################################################################
# agregiranje za VA in preračun CLV-jev
VA_agr_geo_skd <- VA_agr_geo |>
  bind_rows(lapply(names(sectors_eurostat_a64), function(label) {
    aggregate_skd_and_calculate_clv_annual(VA_agr_geo, sectors_eurostat_a64[[label]], label)}))

# agregiranje za EMP
EMP_agr_geo_skd <- EMP_agr_geo |>
  bind_rows(lapply(names(sectors_eurostat_a64), function(label) {
    aggregate_skd(EMP_agr_geo, sectors_eurostat_a64[[label]], label)}))

####  Združitev tabel   ########################################################
master_agr <- bind_rows(VA_agr_geo_skd, EMP_agr_geo_skd) |>
  filter(unit != "PYP_MEUR")

################################################################################
#                               PRERAČUNI                                      #
################################################################################
data_shift_share_prod <- master_agr |>
  pivot_wider(names_from = unit, values_from = values) |>
  mutate(time = lubridate::year(time)) |>
  dplyr::group_by(geo, time) |>
  dplyr::mutate(EMP_share = THS_PER / THS_PER[nace_r2=="TOTAL"] * 100) |> # delež zaposlenih v sektorju
  dplyr::mutate(HW_share = THS_HW / THS_HW[nace_r2=="TOTAL"] * 100) |> # delež delovnih ur v sektorju
  dplyr::mutate(VA_share_nom = CP_MEUR / CP_MEUR[nace_r2=="TOTAL"]*100) |> # Delež dodane vrednost v sektorju nominalno
  dplyr::mutate(VA_share_real = CLV10_MEUR / CLV10_MEUR[nace_r2=="TOTAL"]*100) |> # Delež dodane vrednost v sektorju realno
  dplyr::mutate(PROD_real_EMP = CLV10_MEUR / THS_PER) |> # realna produktivnost na zaposlenega
  dplyr::mutate(PROD_real_HW = CLV10_MEUR / THS_HW) |> # realna produktivnost na delovno uro
  dplyr::mutate(PROD_nom_EMP = CP_MEUR / THS_PER) |> # nominalna produktivnost na zaposlenega
  dplyr::mutate(PROD_nom_HW = CP_MEUR / THS_HW) |> # nominalna produktivnost na delovno uro
  dplyr::group_by(geo, nace_r2) |>
  dplyr::arrange(time) |>
  dplyr::mutate(PROD_diff_EMP = (PROD_real_EMP - lag(PROD_real_EMP)), # sprememba realne produktivnosti na zaposlenega
                PROD_yoy_EMP = ((PROD_real_EMP / lag(PROD_real_EMP)) * 100 - 100), # medletna rast realne produktivnosti na zaposlenega
                EMP_share_diff = (EMP_share - lag(EMP_share)), # sprememba deleža zaposlenih v sektorju
                WITHIN_effect_EMP = (PROD_diff_EMP * lag(EMP_share)), # znotrajsektorska rast produktivnosti na zaposlenega
                STATIC_shift_EMP = (EMP_share_diff * lag(PROD_real_EMP)), # statični strukturni učinek rasti produktivnosti na zaposlenega
                DYNAMIC_shift_EMP = (EMP_share_diff * PROD_diff_EMP), # dinamični strukturni učinek rasti produktivnosti na zaposlenega
                STRUCT_shift_EMP = STATIC_shift_EMP + DYNAMIC_shift_EMP, # strukturni učinek rasti produktivnosti na zaposlenega
                PROD_diff_HW = (PROD_real_HW - lag(PROD_real_HW)), # sprememba realne produktivnosti na delovno uro
                PROD_yoy_HW = ((PROD_real_HW / lag(PROD_real_HW)) * 100 - 100),# medletna rast realne produktivnosti na delovno uro
                HW_share_diff = (HW_share - lag(HW_share)), # sprememba deleža delovnih ur v sektorju
                WITHIN_effect_HW = (PROD_diff_HW * lag(HW_share)), # znotrajsektorska rast produktivnosti na delovno uro
                STATIC_shift_HW = (HW_share_diff * lag(PROD_real_HW)), # statični strukturni učinek rasti produktivnosti na delovno uro
                DYNAMIC_shift_HW = (HW_share_diff * PROD_diff_HW), # dinamični strukturni učinek rasti produktivnosti na delovno uro
                STRUCT_shift_HW = STATIC_shift_HW + DYNAMIC_shift_HW)  |> # strukturni učinek rasti produktivnosti na delovno uro
  ungroup () |>
  mutate(geo_agr = ifelse(geo %in% geo_lookup$geo, FALSE, TRUE)) |>
  relocate(c(HW_share, PROD_real_HW), .after = STRUCT_shift_EMP) |>
  left_join(nace_codes) |>
  relocate(c(nace_level, nace_descr), .after = nace_r2) |>
  relocate(geo_agr, .after = geo) |>
  relocate(VA_share_nom, .after = CP_MEUR) |>
  relocate( VA_share_real, .after = CLV10_MEUR) |>
  relocate(PROD_nom_HW, .after = PROD_real_HW) |>
  rename(VA_nom = CP_MEUR, VA_real = CLV10_MEUR)

#### zapis na postgres bazo ####################################################
# Database connection details
con <- DBI::dbConnect(RPostgres::Postgres(),
                      dbname = "produktivnost",
                      host = "localhost",
                      port = 5432,
                      user = "postgres",
                      password = Sys.getenv("PG_PG_PSW"))
DBI::dbExecute(con, "set search_path to produktivnost")

DBI::dbExecute(con, "TRUNCATE TABLE \"produktivnost_shift_share\"")
# Insert data into the PostgreSQL table
DBI::dbWriteTable(con, "produktivnost_shift_share", data_shift_share_prod, append = TRUE, row.names = FALSE)


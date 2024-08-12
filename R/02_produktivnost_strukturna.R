################################################################################
#         PRODUCTIVITY REPORT SHIFT SHARE & STRUKTURA
#         revizija MZ Junij 2024, original Katarina, Junij 2019
################################################################################

####   Knjižnjice   ############################################################
library(eurostat)
library(dplyr)
library(tidyr)
library(data.table)

####   setup    ################################################################
source("R/00_geo_lookup.R") # geografije
source("R/00_skd_lookup.R") # skd klasifikaicje
last_year <- lubridate::year(Sys.Date()) - 1

################################################################################
#                         Download Podatkov                                    #
################################################################################
VA <- get_eurostat("nama_10_a64",
                    filters = list(
                      geo = geo_subset,
                      time = 1995:last_year,
                      na_item = "B1G", # Value added, gross
                      unit = c("CP_MEUR", "CLV10_MEUR")))|> # Current prices, million euro
  select(-freq, -na_item) |>
  filter(!is.na(values))

EMP <- get_eurostat("nama_10_a64_e",
                   filters = list(
                     geo = geo_subset,
                     time = 1995:last_year,
                     na_item = "EMP_DC", # Total employment domestic concept
                     unit = c("THS_PER", "THS_HW")))|> # Thousand persons, Thousand hours worked
  select(-freq, -na_item) |>
  filter(!is.na(values))

################################################################################
#                         Definiranje dodatnih SKD agregatov                   #
################################################################################
# funkcija za agregiranje
aggregate_skd <- function(df, group, group_name) {
  df %>%
    filter(nace_r2 %in% group) %>%
    group_by(geo, time, unit) %>%
    summarise(values = sum(values, na.rm = TRUE), .groups = 'drop') %>%
    mutate(nace_r2 = group_name)
}

va_menj <- aggregate_skd(VA, MENJALNI_sektor, "MENJALNI_sektor")
va_nemenj <- aggregate_skd(VA, NEMENJALNI_sektor, "NEMENJALNI_sektor")
va_posl <- aggregate_skd(VA, POSLOVNI_sektor, "POSLOVNI_sektor")
va_neposl <- aggregate_skd(VA, NEPOSLOVNI_sektor, "NEPOSLOVNI_sektor")
va_htm <- aggregate_skd(VA, HIGH_TECH_MANUF, "HIGH_TECH_MANUF")
va_mhtm <- aggregate_skd(VA, MED_HIGH_TECH_MANUF, "MED_HIGH_TECH_MANUF")
va_mltm <- aggregate_skd(VA, MED_LOW_TECH_MANUF, "MED_LOW_TECH_MANUF")
va_ltm <- aggregate_skd(VA, LOW_TECH_MANUF, "LOW_TECH_MANUF")
va_kmkt <- aggregate_skd(VA, KNOWLEDGE_MKT_SERV, "KNOWLEDGE_MKT_SERV")
va_rmkt <- aggregate_skd(VA, REST_MKT_SERV, "REST_MKT_SERV")
va_trg <- aggregate_skd(VA, TRZNE, "TRŽNE")
va_ost <- aggregate_skd(VA, OSTALE, "OSTALE")

emp_menj <- aggregate_skd(EMP, MENJALNI_sektor, "MENJALNI_sektor")
emp_nemenj <- aggregate_skd(EMP, NEMENJALNI_sektor, "NEMENJALNI_sektor")
emp_posl <- aggregate_skd(EMP, POSLOVNI_sektor, "POSLOVNI_sektor")
emp_neposl <- aggregate_skd(EMP, NEPOSLOVNI_sektor, "NEPOSLOVNI_sektor")
emp_htm <- aggregate_skd(EMP, HIGH_TECH_MANUF, "HIGH_TECH_MANUF")
emp_mhtm <- aggregate_skd(EMP, MED_HIGH_TECH_MANUF, "MED_HIGH_TECH_MANUF")
emp_mltm <- aggregate_skd(EMP, MED_LOW_TECH_MANUF, "MED_LOW_TECH_MANUF")
emp_ltm <- aggregate_skd(EMP, LOW_TECH_MANUF, "LOW_TECH_MANUF")
emp_kmkt <- aggregate_skd(EMP, KNOWLEDGE_MKT_SERV, "KNOWLEDGE_MKT_SERV")
emp_rmkt <- aggregate_skd(EMP, REST_MKT_SERV, "REST_MKT_SERV")
emp_trg <- aggregate_skd(EMP, TRZNE, "TRŽNE")
emp_ost <- aggregate_skd(EMP, OSTALE, "OSTALE")

####  Združitev tabel   ########################################################
VA_agr <- bind_rows(VA, va_menj, va_nemenj, va_posl, va_neposl, va_htm,
                    va_mhtm, va_mltm, va_ltm, va_kmkt, va_rmkt, va_trg, va_ost)
EMP_agr <- bind_rows(EMP, emp_menj, emp_nemenj, emp_posl, emp_neposl, emp_htm,
                    emp_mhtm, emp_mltm, emp_ltm, emp_kmkt, emp_rmkt, emp_trg, emp_ost)

################################################################################
#                 Definiranje dodatnih geografskih agregatov                   #
################################################################################
# funkcija za agregiranje
aggregate <- function(df, group, group_name) {
  df %>%
    filter(geo %in% group) %>%
    group_by(nace_r2, time, unit) %>%
    summarise(values = sum(values, na.rm = TRUE), .groups = 'drop') %>%
    mutate(geo = group_name)
}

# agregiranje za VA
va_eu13 <- aggregate(VA_agr, country_code_EU13, "EU13")
va_eu14 <- aggregate(VA_agr, country_code_EU14, "EU14")
va_EU27 <- aggregate(VA_agr, country_code_EU27, "EU27")
va_EU27noIE <- aggregate(VA_agr, country_code_EU27_noIE, "EU27noIE")
va_EA20 <- aggregate(VA_agr, country_code_EA20, "EA20")
va_EAnoIE <- aggregate(VA_agr, country_code_EA_noIE, "EAnoIE")
va_inovatorke <- aggregate(VA_agr, country_code_inovatorke, "inovatorke")
va_V4 <- aggregate(VA_agr, country_code_V4, "V4")

# agregiranje za EMP
emp_eu13 <- aggregate(EMP_agr, country_code_EU13, "EU13")
emp_eu14 <- aggregate(EMP_agr, country_code_EU14, "EU14")
emp_EU27 <- aggregate(EMP_agr, country_code_EU27, "EU27")
emp_EU27noIE <- aggregate(EMP_agr, country_code_EU27_noIE, "EU27noIE")
emp_EA20 <- aggregate(EMP_agr, country_code_EA20, "EA20")
emp_EAnoIE <- aggregate(EMP_agr, country_code_EA_noIE, "EAnoIE")
emp_inovatorke <- aggregate(EMP_agr, country_code_inovatorke, "inovatorke")
emp_V4 <- aggregate(EMP_agr, country_code_V4, "V4")

####  Združitev tabel   ########################################################
VA_agr2 <- bind_rows(VA_agr, va_eu13, va_eu14, va_EU27, va_EU27noIE,
                     va_EA20, va_EAnoIE, va_inovatorke, va_V4)
EMP_agr2 <- bind_rows(EMP_agr, emp_eu13, emp_eu14, emp_EU27, emp_EU27noIE,
                     emp_EA20, emp_EAnoIE, emp_inovatorke, emp_V4)
master_agr <- bind_rows(VA_agr2, EMP_agr2)

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
                      host = "192.168.38.21",
                      port = 5432,
                      user = "postgres",
                      password = Sys.getenv("PG_PG_PSW"))
DBI::dbExecute(con, "set search_path to produktivnost")

DBI::dbExecute(con, "TRUNCATE TABLE \"produktivnost_shift_share\"")
# Insert data into the PostgreSQL table
DBI::dbWriteTable(con, "produktivnost_shift_share", data_shift_share_prod, append = TRUE, row.names = FALSE)

# Disconnect from the database
DBI::dbDisconnect(con)

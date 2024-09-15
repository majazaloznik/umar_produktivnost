################################################################################
#               LETNI PODATKI, PO SKD                                      #
#         revizija MZ Junij 2024, original Katarina                            #
################################################################################

####   Knjižnjice   ############################################################
library(tidyr)
library(pxR)
library(dplyr)
library(lubridate)
library(zoo)
####   setup    ################################################################
source("R/00_geo_lookup.R") # geografije
source("R/00_skd_lookup.R", encoding = "UTF-8")# skd klasifikaicje
source("R/helper_functions.R")
################################################################################
#                 Download Letnih Podatkov                                     #
################################################################################
url_4 <- "https://pxweb.stat.si/SiStatData/Resources/PX/Databases/Data/0301915S.px"
url_5 <-"https://pxweb.stat.si/SiStatData/Resources/PX/Databases/Data/0301975S.px"
url_6 <-"https://pxweb.stat.si/SiStatData/Resources/PX/Databases/Data/0301930S.px"
url_7 <-"https://pxweb.stat.si/SiStatData/Resources/PX/Databases/Data/0301910S.px"
url_8 <-"https://pxweb.stat.si/SiStatData/Resources/PX/Databases/Data/0301925S.px"

bulk_DV_A <- as.data.frame(read.px(url_4, encoding = "cp1250",
                                   na.strings = c('"."', '".."', '"..."', '"...."')))
bulk_zap_A <- as.data.frame(read.px(url_5, encoding = "cp1250",
                                    na.strings = c('"."', '".."', '"..."', '"...."')))
bulk_stroski_A <- as.data.frame(read.px(url_6, encoding = "cp1250",
                                        na.strings = c('"."', '".."', '"..."', '"...."')))

################################################################################
#       filtriranje, SKD rekodiranja in pivot wider                            #
################################################################################
DV_A <- bulk_DV_A |>
  filter(MERITVE %in% c("Stalne cene, referenčno leto 2010 (mio EUR)",
                        "Stalne cene predhodnega leta (mio EUR)",
                        "Tekoče cene (mio EUR)")&
           TRANSAKCIJE =="Dodana vrednost" &
           !DEJAVNOSTI.TRANSAKCIJE %in% c("Neto davki na proizvode",
                                          "..Davki na proizvode",
                                          "..Minus: subvencije po proizvodih",
                                          "Bruto domači proizvod")) |>
  mutate(across(where(is.factor), as.character)) |>
  dplyr::rename(DEJAVNOST = DEJAVNOSTI.TRANSAKCIJE) |>
  select(-TRANSAKCIJE) |>
  pivot_wider(names_from = MERITVE)|>
  dplyr::rename_with(~ c("CP_MEUR", "PYP_MEUR", "CLV10_MEUR" ),
                     .cols = 3:5) |>
  dplyr::rename(time = LETO) |>
  mutate(aggr =ifelse(grepl("^[A-Z]{1} ", DEJAVNOST), "level_1", "level_2"),
         aggr =ifelse(DEJAVNOST == "Skupaj dejavnosti", "aggr", aggr),
         nace_r2 = ifelse(DEJAVNOST == "Skupaj dejavnosti", "SKUPAJ",
                          substr(DEJAVNOST, 1,1)),
         nace_r2 = ifelse(nace_r2 == ".", substr(DEJAVNOST, 3,4 ), nace_r2 ),
         nace_r2 = ifelse(nace_r2 == "od", "XX", nace_r2)) |>
  relocate(nace_r2, aggr, .after =DEJAVNOST) |>
  pivot_longer(5:7, values_to = "values", names_to = "unit")

zap_A <- bulk_zap_A |>
  filter(!TRANSAKCIJE=="..Samozaposleni" &
           !DEJAVNOSTI.SEKTORJI %in% c("..S.15 NPISG", "..S.14 Gospodinjstva",
                                       "..S.13 Država", "..S.11 Nefinančne družbe",
                                       "..S.12 Finančne družbe")) |>
  mutate(across(where(is.factor), as.character)) |>
  dplyr::rename(DEJAVNOST = DEJAVNOSTI.SEKTORJI) |>
  pivot_wider(names_from = c(MERITVE, TRANSAKCIJE)) |>
  dplyr::rename_with(~ c( "EMP_PER","EMP_HW", "SAL_PER", "SAL_HW" ),
                     .cols = 3:6) |>
  dplyr::rename(time = LETO) |>
  mutate(aggr =ifelse(grepl("^[A-Z]{1} ", DEJAVNOST), "level_1", "level_2"),
         aggr =ifelse(DEJAVNOST == "Skupaj dejavnosti", "aggr", aggr),
         nace_r2 = ifelse(DEJAVNOST == "Skupaj dejavnosti", "SKUPAJ",
                          substr(DEJAVNOST, 1,1)),
         nace_r2 = ifelse(nace_r2 == ".", substr(DEJAVNOST, 3,4 ), nace_r2 ),
         nace_r2 = ifelse(nace_r2 == "od", "XX", nace_r2)) |>
  filter(nace_r2 != "..") |>
  relocate(nace_r2, aggr, .after =DEJAVNOST)|>
  pivot_longer(5:8, values_to = "values", names_to = "unit")

stroski_A <- bulk_stroski_A |>
  filter(TRANSAKCIJE == "Sredstva za zaposlene") |>
  select(-TRANSAKCIJE) |>
  dplyr::rename(DEJAVNOST = DEJAVNOSTI, COMP_nom = value, time = LETO) |>
  mutate(aggr =ifelse(grepl("^[A-Z]{1} ", DEJAVNOST), "level_1", "level_2"),
         aggr =ifelse(DEJAVNOST == "Skupaj dejavnosti", "aggr", aggr),
         nace_r2 = ifelse(DEJAVNOST == "Skupaj dejavnosti", "SKUPAJ",
                          substr(DEJAVNOST, 1,1)),
         nace_r2 = ifelse(nace_r2 == ".", substr(DEJAVNOST, 3,4 ), nace_r2 ),
         nace_r2 = ifelse(nace_r2 == "od", "XX", nace_r2)) |>
  relocate(nace_r2, aggr, .after =DEJAVNOST) |>
  pivot_longer(5, values_to = "values", names_to = "unit")



################################################################################
#                         Definiranje dodatnih SKD agregatov za RULC           #
################################################################################
DV_A_agr <- DV_A |>
  bind_rows(lapply(names(sectors_surs_det_teoretski), function(label) {
    aggregate_skd_and_calculate_clv_annual(DV_A, sectors_surs_det_teoretski[[label]], label)})) |>
  mutate(DEJAVNOST = if_else(is.na(DEJAVNOST),
                             sapply(nace_r2, function(x) paste(sectors_surs_det_teoretski[[x]], collapse = ", ")),
                             DEJAVNOST),
         aggr = if_else(is.na(aggr), "aggr", aggr))

zap_A_agr <- zap_A |>
  bind_rows(lapply(names(sectors_surs_det_teoretski), function(label) {
    aggregate_skd(zap_A, sectors_surs_det_teoretski[[label]], label)})) |>
  mutate(DEJAVNOST = if_else(is.na(DEJAVNOST),
                             sapply(nace_r2, function(x) paste(sectors_surs_det_teoretski[[x]], collapse = ", ")),
                             DEJAVNOST),
         aggr = if_else(is.na(aggr), "aggr", aggr))

stroski_A_agr <- stroski_A |>
  bind_rows(lapply(names(sectors_surs_det_teoretski), function(label) {
    aggregate_skd(stroski_A, sectors_surs_det_teoretski[[label]], label)})) |>
  mutate(DEJAVNOST = if_else(is.na(DEJAVNOST),
                             sapply(nace_r2, function(x) paste(sectors_surs_det_teoretski[[x]], collapse = ", ")),
                             DEJAVNOST),
         aggr = if_else(is.na(aggr), "aggr", aggr))

# združevanje tabel
PROD_A_agr <-  DV_A_agr |>
  bind_rows(zap_A_agr, stroski_A_agr) |>
  pivot_wider(values_from = values, names_from = unit) |>
  rename(VA_nom = CP_MEUR, VA_real = CLV10_MEUR, LETO = time, SKD = nace_r2) |>
  select(-PYP_MEUR)
################################################################################
#                       Preračuni  za shift share                              #
################################################################################
SURS_shift_share_letni_teoretsko_prav <- PROD_A_agr |>
  select(-SAL_PER,-SAL_HW, -COMP_nom) |>
  dplyr::group_by(LETO) |>
  dplyr::mutate(EMP_share = EMP_PER / EMP_PER[SKD == "Skupaj teoretsko prav"] * 100) |>
  dplyr::mutate(HW_share = EMP_HW / EMP_HW[SKD == "Skupaj teoretsko prav"] * 100) |>
  dplyr::mutate(VA_share_nom = VA_nom / VA_nom[SKD == "Skupaj teoretsko prav"] * 100) |>
  dplyr::mutate(VA_share_real = VA_real / VA_real[SKD == "Skupaj teoretsko prav"] * 100) |>
  rowwise () |>
  dplyr::mutate(PROD_real_EMP = VA_real / EMP_PER,
                PROD_real_HW = VA_real / EMP_HW,
                PROD_nom_EMP = VA_nom / EMP_PER,
                PROD_nom_HW = VA_nom / EMP_HW) |>
  group_by(DEJAVNOST) |>
  arrange(LETO) |>
  dplyr::mutate(PROD_yoy_EMP = ((PROD_real_EMP / lag(PROD_real_EMP)) * 100 - 100),
                PROD_diff_EMP = (PROD_real_EMP - lag(PROD_real_EMP)),
                EMP_share_diff = (EMP_share - lag(EMP_share)),
                WITHIN_effect_EMP = (PROD_diff_EMP * lag(EMP_share)),
                STATIC_shift_EMP = (EMP_share_diff * lag(PROD_real_EMP)),
                DYNAMIC_shift_EMP = (EMP_share_diff * PROD_diff_EMP),
                STRUCT_shift_EMP = STATIC_shift_EMP + DYNAMIC_shift_EMP,
                PROD_yoy_HW = ((PROD_real_HW / lag(PROD_real_HW)) * 100 - 100),
                PROD_diff_HW = (PROD_real_HW - lag(PROD_real_HW)),
                HW_share_diff = (HW_share - lag(HW_share)),
                WITHIN_effect_HW = (PROD_diff_HW * lag(HW_share)),
                STATIC_shift_HW = (HW_share_diff * lag(PROD_real_HW)),
                DYNAMIC_shift_HW = (HW_share_diff * PROD_diff_HW),
                STRUCT_shift_HW = STATIC_shift_HW + DYNAMIC_shift_HW)  |>
  relocate(SKD, DEJAVNOST, aggr, .after = LETO) |>
  ungroup()

################################################################################
#            zapis na postgres bazo                                            #
################################################################################
# Database connection details
con <- DBI::dbConnect(RPostgres::Postgres(),
                      dbname = "produktivnost",
                      host = "localhost",
                      port = 5432,
                      user = "postgres",
                      password = Sys.getenv("PG_PG_PSW"))
DBI::dbExecute(con, "set search_path to produktivnost")

DBI::dbExecute(con, "TRUNCATE TABLE \"SURS_shift_share_letni_teoretsko_prav\"")
# Insert data into the PostgreSQL table
DBI::dbWriteTable(con, "SURS_shift_share_letni_teoretsko_prav", SURS_shift_share_letni_teoretsko_prav,
                  append = TRUE, row.names = FALSE)

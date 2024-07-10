################################################################################
#               KVARTALNI PODATKI, PO SKD                                      #
#         revizija MZ Junij 2024, original Katarina                            #
################################################################################

####   Knjižnjice   ############################################################
library(tidyr)
library(pxR)
library(dplyr)
library(lubridate)
library(zoo)
####   setup    ################################################################
source("R/00_skd_lookup.R") # skd klasifikaicje

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
bulk_BDP_A <- as.data.frame(read.px(url_7, encoding = "cp1250",
                                    na.strings = c('"."', '".."', '"..."', '"...."')))
bulk_sredstva_A <- as.data.frame(read.px(url_8, encoding = "cp1250",
                                         na.strings = c('"."', '".."', '"..."', '"...."')))

################################################################################
#       filtriranje, SKD rekodiranja in pivot wider                            #
################################################################################
DV_A <- bulk_DV_A |>
  filter(MERITVE %in% c("Stalne cene, referenčno leto 2010 (mio EUR)",
                          "Tekoče cene (mio EUR)")&
           TRANSAKCIJE =="Dodana vrednost" &
           !DEJAVNOSTI.TRANSAKCIJE %in% c("Neto davki na proizvode",
                                          "..Davki na proizvode",
                                          "..Minus: subvencije po proizvodih")) |>
  mutate(across(where(is.factor), as.character)) |>
  dplyr::rename(DEJAVNOST = DEJAVNOSTI.TRANSAKCIJE) |>
  select(-TRANSAKCIJE) |>
  pivot_wider(names_from = MERITVE)

zap_A <- bulk_zap_A |>
  filter(!TRANSAKCIJE=="..Samozaposleni" &
           !DEJAVNOSTI.SEKTORJI %in% c("..S.15 NPISG", "..S.14 Gospodinjstva",
                                     "..S.13 Država", "..S.11 Nefinančne družbe",
                                     "..S.12 Finančne družbe")) |>
  mutate(across(where(is.factor), as.character)) |>
  dplyr::rename(DEJAVNOST = DEJAVNOSTI.SEKTORJI) |>
  pivot_wider(names_from = c(MERITVE, TRANSAKCIJE))

stroski_A <- bulk_stroski_A |>
  filter(TRANSAKCIJE == "Sredstva za zaposlene") |>
  select(-TRANSAKCIJE) |>
  dplyr::rename(DEJAVNOST = DEJAVNOSTI, COMP_nom = value)

tot_BDP_A <- bulk_BDP_A |>
  filter(MERITVE %in% c("Stalne cene, referenčno leto 2010 (mio EUR)", "Tekoče cene (mio EUR)")) |>
  pivot_wider(id_cols = LETO, names_from = MERITVE)

tot_zap_A <- bulk_zap_A |>
  filter(TRANSAKCIJE %in% c("Zaposlenost", "..Zaposleni") &
           DEJAVNOSTI.SEKTORJI == "Skupaj dejavnosti") |>
  select(-DEJAVNOSTI.SEKTORJI) |>
  pivot_wider(id_cols = LETO, names_from = c(MERITVE, TRANSAKCIJE))

tot_stroski_A <- bulk_sredstva_A |>
  filter(TRANSAKCIJE=="Sredstva za zaposlene" & MERITVE=="Tekoče cene (mio EUR)") |>
  select(-MERITVE,-TRANSAKCIJE)

################################################################################
#        Zdrzžitev baz                                                         #
################################################################################
# Združitev baz za shift share in RULC
PROD_A <-  DV_A|>
  inner_join(zap_A) |>
  dplyr::rename_with(~ c("VA_nom", "VA_real", "EMP_PER","EMP_HW", "SAL_PER", "SAL_HW" ),
              .cols = 3:8) |>
  mutate(aggr =ifelse(grepl("^[A-Z]{1} ", DEJAVNOST), "level_1", "level_2"),
         aggr =ifelse(DEJAVNOST == "Skupaj dejavnosti", "aggr", aggr),
         SKD = ifelse(DEJAVNOST == "Skupaj dejavnosti", "SKUPAJ",
                      substr(DEJAVNOST, 1,1)),
         SKD = ifelse(SKD == ".", substr(DEJAVNOST, 3,4 ), SKD )) |>
  relocate(aggr, .after =DEJAVNOST) |>
  inner_join(stroski_A)



# Združitev baz za BDP total RULC
# združitev baz total
tot_RULC_A <-tot_BDP_A |>
  left_join(tot_zap_A) |>
  left_join(tot_stroski_A) |>
  dplyr::rename_with(~ c("GDP_nom", "GDP_real", "EMP_PER","EMP_HW", "SAL_PER", "SAL_HW", "COMP_nom" ),
              .cols = 2:8)



################################################################################
#                         Definiranje dodatnih SKD agregatov za RULC           #
################################################################################
# funkcija za agregiranje
aggregate_skd <- function(df, group, group_name) {
  x <-  df  |>
    filter(SKD %in% group) |>
    group_by(LETO) |>
    summarise(across(where(is.numeric), \(x) sum(x, na.rm = TRUE)),
              SKD = group_name, DEJAVNOST = paste(group, collapse = ", "),
              aggr = "aggr") |>
    bind_rows(df)
}

PROD_A_agr <- aggregate_skd(PROD_A, POSLOVNI_sektor_surs_det, "Poslovni")
PROD_A_agr <- aggregate_skd(PROD_A_agr, NEPOSLOVNI_sektor_surs_det, "Neposlovni")
PROD_A_agr <- aggregate_skd(PROD_A_agr, MENJALNI_sektor_surs_det, "Menjalni")
PROD_A_agr <- aggregate_skd(PROD_A_agr, NEMENJALNI_sektor_surs_det, "Nemenjalni")
PROD_A_agr <- aggregate_skd(PROD_A_agr, TRZNE_surs_det, "Tržne")
PROD_A_agr <- aggregate_skd(PROD_A_agr, OSTALE_surs_det, "Ostale")|>
  relocate(SKD, DEJAVNOST, aggr, .after =LETO)

################################################################################
#                       Preračuni  za shift share                              #
################################################################################
SURS_shift_share_letni <- PROD_A_agr |>
  select(-SAL_PER,-SAL_HW, -COMP_nom) |>
  dplyr::group_by(LETO) |>
  dplyr::mutate(EMP_share = EMP_PER / EMP_PER[DEJAVNOST == "Skupaj dejavnosti"] * 100) |>
  dplyr::mutate(HW_share = EMP_HW / EMP_HW[DEJAVNOST == "Skupaj dejavnosti"] * 100) |>
  dplyr::mutate(VA_share_nom = VA_nom / VA_nom[DEJAVNOST == "Skupaj dejavnosti"] * 100) |>
  dplyr::mutate(VA_share_real = VA_real / VA_real[DEJAVNOST == "Skupaj dejavnosti"] * 100) |>
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

DBI::dbExecute(con, "TRUNCATE TABLE \"SURS_shift_share_letni\"")
# Insert data into the PostgreSQL table
DBI::dbWriteTable(con, "SURS_shift_share_letni", SURS_shift_share_letni,
                  append = TRUE, row.names = FALSE)


################################################################################
#                       Preračuni  za RULC                                     #
################################################################################
SURS_RULC_VA_SKD_letni <- PROD_A_agr |>
  mutate(deflator_VA = VA_nom / VA_real,
         COMP_nom_EMP = COMP_nom / SAL_PER,
         COMP_real_EMP = (COMP_nom / deflator_VA) / SAL_PER,
         COMP_nom_HW = COMP_nom / SAL_HW,
         COMP_real_HW = (COMP_nom / deflator_VA) / SAL_HW,
         PROD_nom_EMP = VA_nom / EMP_PER,
         PROD_nom_HW = VA_nom / EMP_HW,
         PROD_real_EMP = VA_real / EMP_PER,
         PROD_real_HW = VA_real / EMP_HW,
         RULC = COMP_nom_EMP / PROD_nom_EMP,
         NULC = COMP_nom_EMP / PROD_real_EMP) |>
  ungroup() |>
  pivot_longer(cols = 5:22, names_to = "Indicator", values_to = "Value") |>
  arrange(LETO) |>
  group_by(Indicator, SKD) |>
  mutate(YOY = ((Value / lag(Value, 1)) * 100 - 100),
         Indeks2005 = Value /(mean(Value[LETO == 2005])) * 100,
         Indeks2007 = Value /(mean(Value[LETO == 2007])) * 100,
         Indeks2008 = Value /(mean(Value[LETO == 2008])) * 100,
         Indeks2019 = Value /(mean(Value[LETO == 2019])) * 100,
         Descriptor = indicator_descriptions[Indicator]) |>
  relocate(Descriptor, .after = Indicator) |>
  ungroup()

################################################################################
#            zapis na postgres bazo                                            #
################################################################################
DBI::dbExecute(con, "TRUNCATE TABLE \"SURS_RULC_VA_SKD_letni\"")
# Insert data into the PostgreSQL table
DBI::dbWriteTable(con, "SURS_RULC_VA_SKD_letni", SURS_RULC_VA_SKD_letni,
                  append = TRUE, row.names = FALSE)

###################################################################################
#               TOTAL preračuni za RULC                                           #
###################################################################################
SURS_RULC_BDP_total_letni <- tot_RULC_A |>
  dplyr::group_by(LETO) |>
  dplyr::mutate(deflator_GDP = GDP_nom / GDP_real,
                COMP_nom_EMP = COMP_nom / SAL_PER,
                COMP_real_EMP = (COMP_nom / deflator_GDP) / SAL_PER,
                COMP_nom_HW = COMP_nom / SAL_HW,
                COMP_real_HW = (COMP_nom / deflator_GDP) / SAL_HW,
                PROD_nom_EMP = GDP_nom / EMP_PER,
                PROD_nom_HW = GDP_nom / EMP_HW,
                PROD_real_EMP = GDP_real / EMP_PER,
                PROD_real_HW = GDP_real / EMP_HW,
                RULC = COMP_nom_EMP / PROD_nom_EMP,
                NULC = COMP_nom_EMP / PROD_real_EMP) |>
  pivot_longer(cols = 2:19, names_to = "Indicator", values_to = "Value") |>
  arrange(LETO) |>
  group_by(Indicator) |>
  mutate(YOY = ((Value / lag(Value, 1)) * 100 - 100),
         Indeks2005 = Value /(mean(Value[LETO == 2005])) * 100,
         Indeks2007 = Value /(mean(Value[LETO == 2007])) * 100,
         Indeks2008 = Value /(mean(Value[LETO == 2008])) * 100,
         Indeks2019 = Value /(mean(Value[LETO == 2019])) * 100,
         Descriptor = indicator_descriptions[Indicator]) |>
  relocate(Descriptor, .after = Indicator) |>
  ungroup()

################################################################################
#            zapis na postgres bazo                                            #
################################################################################
DBI::dbExecute(con, "TRUNCATE TABLE \"SURS_RULC_BDP_total_letni\"")
# Insert data into the PostgreSQL table
DBI::dbWriteTable(con, "SURS_RULC_BDP_total_letni", SURS_RULC_BDP_total_letni,
                  append = TRUE, row.names = FALSE)




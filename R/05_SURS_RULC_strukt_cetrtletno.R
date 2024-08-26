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
source("R/00_geo_lookup.R") # geografije
source("R/00_skd_lookup.R", encoding = "UTF-8")



################################################################################
#                 Download Četrtletnih Podatkov                                #
################################################################################
url_1 <- "https://pxweb.stat.si/SiStatData/Resources/PX/Databases/Data/0300220S.px"
url_2 <-"https://pxweb.stat.si/SiStatData/Resources/PX/Databases/Data/0300260S.px"
url_3 <-"https://pxweb.stat.si/SiStatData/Resources/PX/Databases/Data/0300240S.px"

bulk_DV <- as.data.frame(read.px(url_1, encoding = "cp1250",
                   na.strings = c('"."', '".."', '"..."', '"...."')))
bulk_zap <- as.data.frame(read.px(url_2, encoding = "cp1250",
                    na.strings = c('"."', '".."', '"..."', '"...."')))
bulk_stroski <- as.data.frame(read.px(url_3, encoding = "cp1250",
                        na.strings = c('"."', '".."', '"..."', '"...."')))

################################################################################
#       filtriranje, SKD rekodiranja in pivot wider                            #
################################################################################
DV <- bulk_DV |>
  filter(VRSTA.PODATKA == "Originalni podatki" &
           MERITVE %in% c("Stalne cene, referenčno leto 2010 (mio EUR)", "Tekoče cene (mio EUR)")&
           !TRANSAKCIJE %in% c("Neto davki na proizvode", "Bruto domači proizvod")) |>
  mutate(across(where(is.factor), as.character)) |>
  select(-VRSTA.PODATKA) |>
  rename(DEJAVNOST = TRANSAKCIJE) |>
  mutate(SKD = recode(DEJAVNOST, !!!dejanvost_lookup, .default = NA_character_),
         DEJAVNOST = ifelse(SKD == "SKUPAJ", "Dejavnost - SKUPAJ", DEJAVNOST)) |>
  pivot_wider(names_from = MERITVE)

zap <- bulk_zap |>
  filter(VRSTA.PODATKA=="Originalni podatki" &
           VRSTA.ZAPOSLENOSTI %in% c("Zaposlenost (domači koncept)", "..Zaposleni") &
           !DEJAVNOST.SEKTOR %in% c("..S.15 NPISG","..S.14 Gospodinjstva", "..S.13 Država","..S.11 Nefinančne družbe","..S.12 Finančne družbe")) |>
  mutate(across(where(is.factor), as.character)) |>
  select(-VRSTA.PODATKA) |>
  rename(DEJAVNOST = DEJAVNOST.SEKTOR) |>
  mutate(SKD = recode(DEJAVNOST, !!!dejanvost_lookup, .default = NA_character_)) |>
  pivot_wider(names_from = c(MERITVE, VRSTA.ZAPOSLENOSTI))

stroski <- bulk_stroski |>
  filter(VRSTA.PODATKA == "Originalni podatki") |>
  filter(str_detect(TRANSAKCIJE, regex("Sredstva za zaposlene", ignore_case = TRUE))) |>
  rename(DEJAVNOST = TRANSAKCIJE) |>
  select(-VRSTA.PODATKA) |>
  mutate(across(where(is.factor), as.character)) |>
  mutate(SKD = recode(DEJAVNOST, !!!transakcije_lookup, .default = NA_character_)) |>
  mutate(DEJAVNOST = recode( SKD, !!!setNames(names(dejanvost_lookup), dejanvost_lookup),
         .default = NA_character_))

tot_BDP <- bulk_DV |>
  mutate(across(where(is.factor), as.character)) |>
  filter(VRSTA.PODATKA %in% c("Originalni podatki",
                              "Podatki z izločenimi vplivi sezone in koledarja") &
           MERITVE %in% c("Stalne cene, referenčno leto 2010 (mio EUR)",
                          "Tekoče cene (mio EUR)") &
           TRANSAKCIJE == "Bruto domači proizvod") |>
  select(-TRANSAKCIJE) |>
  pivot_wider(id_cols = c(ČETRTLETJE, VRSTA.PODATKA), names_from = MERITVE) |>
  rename_with(~ c("season_adj", "GDP_nom", "GDP_real"), .cols = 2:4)

tot_zap <- bulk_zap |>
  mutate(across(where(is.factor), as.character)) |>
  filter(VRSTA.PODATKA %in% c("Originalni podatki",
                              "Podatki z izločenimi vplivi sezone in koledarja") &
           VRSTA.ZAPOSLENOSTI %in% c("Zaposlenost (domači koncept)",
                                     "..Zaposleni") &
           DEJAVNOST.SEKTOR == "Dejavnost - SKUPAJ") |>
  select(-DEJAVNOST.SEKTOR) |>
  pivot_wider(id_cols = c(ČETRTLETJE,VRSTA.PODATKA), names_from = c(MERITVE, VRSTA.ZAPOSLENOSTI)) |>
  rename_with(~ c("season_adj", "EMP_PER", "EMP_HW", "SAL_PER", "SAL_HW"), .cols = 2:6)

tot_stroski <- bulk_stroski |>
  filter(VRSTA.PODATKA%in%c("Originalni podatki",
                            "Podatki z izločenimi vplivi sezone in koledarja") &
           TRANSAKCIJE=="Sredstva za zaposlene") |>
  mutate(across(where(is.factor), as.character)) |>
  select(-TRANSAKCIJE) |>
  rename_with(~ c("season_adj", "COMP_nom"), .cols = 2:3)

################################################################################
#                         Definiranje dodatnih SKD agregatov                   #
################################################################################
# funkcija za agregiranje
aggregate_skd <- function(df, group, group_name) {
 x <-  df  |>
    filter(SKD %in% group) |>
    group_by(ČETRTLETJE) |>
    summarise(across(where(is.numeric), \(x) sum(x, na.rm = TRUE)),
              SKD = group_name, DEJAVNOST = paste(group, collapse = ", ")) |>
    bind_rows(df)
}
# funkcija za BDE
calculate_c <- function(df) {
  x <- df  |>
    filter(SKD %in% c("BCDE", "C")) |>
    select(-DEJAVNOST) |>
    group_by(ČETRTLETJE) |>
    pivot_wider(names_from = SKD, values_from = where(is.numeric)) |>
    mutate(across(ends_with("BCDE"),
                  .fns = ~ . - get(gsub("BCDE$", "C", cur_column())),
                  .names = "BDE_{.col}")) |>
    ungroup() |>
    select(ČETRTLETJE, starts_with("BDE_")) |>
    rename_with(~ gsub("BDE_", "", .), starts_with("BDE_")) |>
    rename_with(~ gsub("_BCDE", "", .), ends_with("_BCDE")) |>
    rename_with(~ if_else(. %in% "BCDE", "value", .), .cols = everything()) |>
    mutate(SKD = "BDE",
      DEJAVNOST = "Rudarstvo, oskrba z elektriko in vodo, ravnanje z odplakami, saniranje okolja") |>
    bind_rows(df) |>
    mutate(time = as.Date(as.yearqtr(ČETRTLETJE, format = "%YQ%q")),.keep = "unused") |>
    relocate(time, SKD, DEJAVNOST)
}

DV_agr <- aggregate_skd(DV, POSLOVNI_sektor_surs, "Poslovni")
DV_agr <- aggregate_skd(DV_agr, NEPOSLOVNI_sektor_surs, "Neposlovni")
DV_agr <- aggregate_skd(DV_agr, MENJALNI_sektor_surs, "Menjalni")
DV_agr <- aggregate_skd(DV_agr, NEMENJALNI_sektor_surs, "Nemenjalni")
DV_agr <- aggregate_skd(DV_agr, TRZNE_surs, "Tržne")
DV_agr <- aggregate_skd(DV_agr, OSTALE_surs, "Ostale")
DV_agr <- calculate_c(DV_agr)


zap_agr <- aggregate_skd(zap, POSLOVNI_sektor_surs, "Poslovni")
zap_agr <- aggregate_skd(zap_agr, NEPOSLOVNI_sektor_surs, "Neposlovni")
zap_agr <- aggregate_skd(zap_agr, MENJALNI_sektor_surs, "Menjalni")
zap_agr <- aggregate_skd(zap_agr, NEMENJALNI_sektor_surs, "Nemenjalni")
zap_agr <- aggregate_skd(zap_agr, TRZNE_surs, "Tržne")
zap_agr <- aggregate_skd(zap_agr, OSTALE_surs, "Ostale")
zap_agr <- calculate_c(zap_agr)

stroski_agr <- aggregate_skd(stroski, POSLOVNI_sektor_surs, "Poslovni")
stroski_agr <- aggregate_skd(stroski_agr, NEPOSLOVNI_sektor_surs, "Neposlovni")
stroski_agr <- aggregate_skd(stroski_agr, MENJALNI_sektor_surs, "Menjalni")
stroski_agr <- aggregate_skd(stroski_agr, NEMENJALNI_sektor_surs, "Nemenjalni")
stroski_agr <- aggregate_skd(stroski_agr, TRZNE_surs, "Tržne")
stroski_agr <- aggregate_skd(stroski_agr, OSTALE_surs, "Ostale")
stroski_agr <- calculate_c(stroski_agr) |> rename(COMP_nom = value)

# Združitev baz
PROD_q <- DV_agr |>
  left_join(zap_agr) |>
  rename_with(~ c("VA_nom", "VA_real", "EMP_PER","EMP_HW", "SAL_PER", "SAL_HW" ),
              .cols = 4:9)

RULC_q <- PROD_q |>
  left_join(stroski_agr)

# združitev baz total
tot_RULC_q <- tot_BDP |>
  left_join(tot_zap) |>
  left_join(tot_stroski) |>
  mutate(time = as.Date(as.yearqtr(ČETRTLETJE, format = "%YQ%q")),.keep = "unused") |>
  relocate(time)


################################################################################
#                       Preračuni  za shift share                              #
################################################################################
SURS_shift_share_cetrtletni <- PROD_q |>
  select(-SAL_PER, -SAL_HW) |>
  dplyr::group_by(time) |>
  dplyr::mutate(EMP_share = EMP_PER / EMP_PER[DEJAVNOST == "Dejavnost - SKUPAJ"] * 100) |>
  dplyr::mutate(HW_share = EMP_HW / EMP_HW[DEJAVNOST == "Dejavnost - SKUPAJ"] * 100) |>
  dplyr::mutate(VA_share_nom = VA_nom / VA_nom[DEJAVNOST == "Dejavnost - SKUPAJ"] * 100) |>
  dplyr::mutate(VA_share_real = VA_real / VA_real[DEJAVNOST == "Dejavnost - SKUPAJ"] * 100) |>
  rowwise () |>
  dplyr::mutate(PROD_real_EMP = VA_real / EMP_PER,
                PROD_real_HW = VA_real / EMP_HW,
                PROD_nom_EMP = VA_nom / EMP_PER,
                PROD_nom_HW = VA_nom / EMP_HW) |>
  group_by(DEJAVNOST) |>
  arrange(time) |>
  dplyr::mutate(PROD_yoy_EMP = ((PROD_real_EMP / lag(PROD_real_EMP, 4)) * 100 - 100),
                PROD_diff_EMP = (PROD_real_EMP - lag(PROD_real_EMP, 4)),
                EMP_share_diff = (EMP_share - lag(EMP_share, 4)),
                WITHIN_effect_EMP = (PROD_diff_EMP * lag(EMP_share, 4)),
                STATIC_shift_EMP = (EMP_share_diff * lag(PROD_real_EMP, 4)),
                DYNAMIC_shift_EMP = (EMP_share_diff * PROD_diff_EMP),
                STRUCT_shift_EMP = STATIC_shift_EMP + DYNAMIC_shift_EMP,
                PROD_yoy_HW = ((PROD_real_HW / lag(PROD_real_HW, 4)) * 100 - 100),
                PROD_diff_HW = (PROD_real_HW - lag(PROD_real_HW, 4)),
                HW_share_diff = (HW_share - lag(HW_share, 4)),
                WITHIN_effect_HW = (PROD_diff_HW * lag(HW_share, 4)),
                STATIC_shift_HW = (HW_share_diff * lag(PROD_real_HW, 4)),
                DYNAMIC_shift_HW = (HW_share_diff * PROD_diff_HW),
                STRUCT_shift_HW = STATIC_shift_HW + DYNAMIC_shift_HW,
                LETO = year(time),
                Q = paste0("Q", quarter(time)))  |>
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

DBI::dbExecute(con, "TRUNCATE TABLE \"SURS_shift_share_cetrtletni\"")
# Insert data into the PostgreSQL table
DBI::dbWriteTable(con, "SURS_shift_share_cetrtletni", SURS_shift_share_cetrtletni,
                  append = TRUE, row.names = FALSE)

################################################################################
#                       Preračuni  za RULC                                     #
################################################################################
SURS_RULC_VA_SKD_cetrtletni <- RULC_q |>
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
  pivot_longer(cols = 4:21, names_to = "Indicator", values_to = "Value") |>
  arrange(time) |>
  group_by(Indicator, SKD) |>
  mutate(LETO = year(time),
         Q = paste0("Q", quarter(time)),
         YOY = ((Value / lag(Value, 4)) * 100 - 100),
         Indeks2005 = Value /(mean(Value[LETO == 2005])) * 100,
         Indeks2005_4cds = rollmean(Indeks2005, 4, fill = NA, align = "right"),
         Indeks2007 = Value /(mean(Value[LETO == 2007])) * 100,
         Indeks2007_4cds = rollmean(Indeks2007, 4, fill = NA, align = "right"),
         Indeks2008 = Value /(mean(Value[LETO == 2008])) * 100,
         Indeks2008_4cds = rollmean(Indeks2008, 4, fill = NA, align = "right"),
         Indeks2019 = Value /(mean(Value[LETO == 2019])) * 100,
         Indeks2019_4cds = rollmean(Indeks2019, 4, fill = NA, align = "right"),
         Descriptor = indicator_descriptions[Indicator]) |>
  relocate(Descriptor, .after = Indicator) |>
  relocate("LETO", "Q", .after = Indeks2019_4cds) |>
  ungroup()

################################################################################
#            zapis na postgres bazo                                            #
################################################################################
DBI::dbExecute(con, "TRUNCATE TABLE \"SURS_RULC_VA_SKD_cetrtletni\"")
# Insert data into the PostgreSQL table
DBI::dbWriteTable(con, "SURS_RULC_VA_SKD_cetrtletni", SURS_RULC_VA_SKD_cetrtletni,
                  append = TRUE, row.names = FALSE)

###################################################################################
#               TOTAL preračuni za RULC                                           #
###################################################################################
SURS_RULC_BDP_total_cetrtletni <- tot_RULC_q |>
  dplyr::group_by(time ,season_adj) |>
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
  pivot_longer(cols = 3:20, names_to = "Indicator", values_to = "Value") |>
  arrange(time) |>
  group_by(Indicator, season_adj) |>
  mutate(LETO = year(time),
         Q = paste0("Q", quarter(time)),
         YOY = ((Value / lag(Value, 4)) * 100 - 100),
         Indeks2005 = Value /(mean(Value[LETO == 2005])) * 100,
         Indeks2005_4cds = rollmean(Indeks2005, 4, fill = NA, align = "right"),
         Indeks2007 = Value /(mean(Value[LETO == 2007])) * 100,
         Indeks2007_4cds = rollmean(Indeks2007, 4, fill = NA, align = "right"),
         Indeks2008 = Value /(mean(Value[LETO == 2008])) * 100,
         Indeks2008_4cds = rollmean(Indeks2008, 4, fill = NA, align = "right"),
         Indeks2019 = Value /(mean(Value[LETO == 2019])) * 100,
         Indeks2019_4cds = rollmean(Indeks2019, 4, fill = NA, align = "right"),
         Descriptor = indicator_descriptions[Indicator]) |>
  relocate(Descriptor, .after = Indicator) |>
  relocate("LETO", "Q", .after = Indeks2019_4cds) |>
  ungroup()

################################################################################
#            zapis na postgres bazo                                            #
################################################################################
DBI::dbExecute(con, "TRUNCATE TABLE \"SURS_RULC_BDP_total_cetrtletni\"")
# Insert data into the PostgreSQL table
DBI::dbWriteTable(con, "SURS_RULC_BDP_total_cetrtletni", SURS_RULC_BDP_total_cetrtletni,
                  append = TRUE, row.names = FALSE)

# Disconnect from the database
DBI::dbDisconnect(con)


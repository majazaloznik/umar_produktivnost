################################################################################
#                       Izracun ČETRTLETNIH RULC in NULC                       #
#         revizija MZ Junij 2024, original Katarina, Oktober 2020              #
################################################################################
####   Knjižnjice   ############################################################
library(eurostat)
library(dplyr)
library(tidyr)
library(zoo)
library(lubridate)
####   setup    ################################################################
source("R/00_geo_lookup.R") # geografije
source("R/00_skd_lookup.R", encoding = "UTF-8") # skd klasifikaicje
source("R/helper_functions.R")

################################################################################
#                        RULC_BDP_totals_Q                                     #
################################################################################
################################################################################
#                         Download Podatkov                                    #
################################################################################
GDP <- get_eurostat("namq_10_gdp",
                    filters = list(
                      geo = geo_subset,
                      sinceTimePeriod = 1995,
                      s_adj = c("NSA", "SCA"),
                      na_item = c("B1GQ" , "D1"), # Gross domestic product at market prices, compensation of employees
                      unit = c("CP_MEUR" , "CLV10_MEUR", "PYP_MEUR")))|> #Current prices million euro, + chain linked volumes 2010, million eur + previous year's prices
  select(-freq)

EMP <- get_eurostat("namq_10_a10_e",
                    filters = list(
                      geo = geo_subset,
                      sinceTimePeriod = 1995,
                      s_adj = c("NSA", "SCA"),
                      na_item = c("EMP_DC" , "SAL_DC"), # Total employment domestic concept, employees dc,
                      unit = c("THS_PER", "THS_HW"), # Thousand persons, Thousand hours worked
                      nace_r2 = "TOTAL"))|>
  select(-freq, -nace_r2) |>
  filter(!is.na(values))

################################################################################
#                 Definiranje dodatnih geografskih agregatov                   #
################################################################################
# agregiranje za BDP in preračun CLV-jev
GDP_agr_geo <- GDP |>
  bind_rows(lapply(names(geo_aggregations), function(label) {
    aggregate_geo_and_calculate_clv_quarterly(GDP, geo_aggregations[[label]], label)})) |>
  filter(!is.na(values) & unit != "PYP_MEUR")

# agregiranje za EMP
EMP_agr_geo <- EMP |>
  bind_rows(lapply(names(geo_aggregations), function(label) {
    aggregate_geo(EMP, geo_aggregations[[label]], label)})) |>
  filter(!is.na(values))

# združitev baz
master_agr <- bind_rows(GDP_agr_geo, EMP_agr_geo)

################################################################################
#                               PRERAČUNI                                      #
################################################################################
RULC_BDP_totals_Q <-  master_agr |>
  pivot_wider(names_from = c(unit, na_item), values_from = values) |>
  group_by(geo, s_adj) |>
  mutate(deflator_GDP = CP_MEUR_B1GQ / CLV10_MEUR_B1GQ,
         COMP_nom_EMP = CP_MEUR_D1 / THS_PER_SAL_DC,
         COMP_real_EMP = CP_MEUR_D1 / deflator_GDP / THS_PER_SAL_DC,
         COMP_nom_HW = CP_MEUR_D1 / THS_HW_SAL_DC,
         COMP_real_HW = (CP_MEUR_D1 / deflator_GDP) / THS_HW_SAL_DC,
         PROD_nom_EMP = CP_MEUR_B1GQ / THS_PER_EMP_DC,
         PROD_real_EMP = CLV10_MEUR_B1GQ / THS_PER_EMP_DC,
         PROD_nom_HW = CP_MEUR_B1GQ / THS_HW_EMP_DC,
         PROD_real_HW = CLV10_MEUR_B1GQ / THS_HW_EMP_DC,
         RULC = COMP_nom_EMP / PROD_nom_EMP,
         NULC = COMP_nom_EMP / PROD_real_EMP) |>
  rename(GDP_nom = CP_MEUR_B1GQ,
         GDP_real = CLV10_MEUR_B1GQ,
         COMP_nom = CP_MEUR_D1) |>
  pivot_longer(!c(time,geo, s_adj), names_to = "Indicator", values_to = "Value") |>
  arrange(time) |>
  group_by(geo, Indicator, s_adj) |>
  mutate(YOY = ((Value / lag(Value,4))*100-100),
         Indeks2005 = Value /(mean(Value[time >= "2005-01-01"& time <= "2005-10-01"]))*100,
         Indeks2005_4cds = rollmean(Indeks2005,4, fill=NA, align = "right"),
         Indeks2007 = Value /(mean(Value[time >= "2007-01-01"& time <= "2007-10-01"]))*100,
         Indeks2007_4cds = rollmean(Indeks2007,4, fill=NA, align = "right"),
         Indeks2008 = Value /(mean(Value[time >= "2008-01-01"& time <= "2008-10-01"]))*100,
         Indeks2008_4cds = rollmean(Indeks2008,4, fill=NA, align = "right"),
         Indeks2019 = Value /(mean(Value[time >= "2019-01-01"& time <= "2019-10-01"]))*100,
         Indeks2019_4cds = rollmean(Indeks2019,4, fill=NA, align = "right"),
         year = year(time),
         quarter = paste0("Q", quarter(time)),
         Descriptor = indicator_descriptions[Indicator]) |>
  relocate(Descriptor, .after = Indicator) |>
  ungroup()

################################################################################
#            zapis na postgres bazo                                            #
################################################################################
# Database connection details
con <- DBI::dbConnect(RPostgres::Postgres(),
                      dbname = "produktivnost",
                      host = "localhost", # "192.168.38.21"
                      port = 5432,
                      user = "postgres",
                      password = Sys.getenv("PG_PG_PSW"))
DBI::dbExecute(con, "set search_path to produktivnost")

DBI::dbExecute(con, "TRUNCATE TABLE \"RULC_BDP_total_cetrtletni\"")
# Insert data into the PostgreSQL table
DBI::dbWriteTable(con, "RULC_BDP_total_cetrtletni", RULC_BDP_totals_Q, append = TRUE, row.names = FALSE)

################################################################################
#                        RULC_VA_NACE_Q                                        #
################################################################################
################################################################################
#                         Download Podatkov                                    #
################################################################################
VA <- get_eurostat("namq_10_a10",
                   filters = list(
                     geo = geo_subset,
                     sinceTimePeriod = 1995,
                     s_adj = "NSA",
                     na_item = c("B1G" , "D1"), # Value added, gross, compensation of employees,
                     unit = c("CP_MEUR", "CLV10_MEUR", "PYP_MEUR", "CP_MNAC","PYP_MNAC")))|> # Current prices, million euro
  select(-freq, -s_adj) |>
  filter(!(na_item == "D1" & unit %in% c("CP_MNAC","PYP_MNAC", "CLV10_MEUR", "PYP_MEUR")))

EMP <- get_eurostat("namq_10_a10_e",
                    filters = list(
                      geo = geo_subset,
                      sinceTimePeriod = 1995,
                      s_adj = "NSA",
                      na_item = c("EMP_DC" , "SAL_DC"), # Total employment domestic concept, employees dc
                      unit = c("THS_PER", "THS_HW")))|> # Thousand persons, Thousand hours worked
  select(-freq, -s_adj) |>
  filter(!is.na(values))

################################################################################
#                         Definiranje dodatnih SKD agregatov                   #
################################################################################
# disagregiranje BCDE v BDE
VA_w_BDE <- VA |>
  bind_rows(disagregate_bde_and_calculate_clv_eurostat_quarterly(VA, "nace_r2", "B-E"))
EMP_w_BDE <- EMP |>
  bind_rows(disagregate_bde_annual(EMP, "nace_r2", "B-E"))


# agregiranje za VA in preračun CLV-jev
VA_agr_skd <- VA_w_BDE |>
  bind_rows(lapply(names(sectors_eurostat_a10), function(label) {
    aggregate_skd_and_calculate_clv_quarterly_MNAC(VA_w_BDE, sectors_eurostat_a10[[label]], label)})) |>
  filter(!unit %in% c("CP_MNAC", "PYP_MNAC"))

# agregiranje za EMP
EMP_agr_skd <- EMP_w_BDE |>
  bind_rows(lapply(names(sectors_eurostat_a10), function(label) {
    aggregate_skd(EMP_w_BDE, sectors_eurostat_a10[[label]], label)}))

################################################################################
#                 Definiranje dodatnih geografskih agregatov                   #
################################################################################
# agregiranje za VA in preračun CLV-jev
VA_agr_geo_skd <- VA_agr_skd |>
  bind_rows(lapply(names(geo_aggregations), function(label) {
    aggregate_geo_and_calculate_clv_quarterly(VA_agr_skd, geo_aggregations[[label]], label)}))

# agregiranje za EMP
EMP_agr_geo_skd <- EMP_agr_skd |>
  bind_rows(lapply(names(geo_aggregations), function(label) {
    aggregate_geo(EMP_agr_skd, geo_aggregations[[label]], label)}))

####  Združitev tabel   ########################################################
master_agr <- bind_rows(VA_agr_geo_skd, EMP_agr_geo_skd) |>
  filter(unit != "PYP_MEUR")

################################################################################
#                               PRERAČUNI                                      #
################################################################################
data_nace <- master_agr |>
  pivot_wider(names_from = c(unit, na_item),
              values_from = values) |>
  group_by(geo, nace_r2) |>
  mutate(deflator_VA = CP_MEUR_B1G / CLV10_MEUR_B1G,
         COMP_nom_EMP = CP_MEUR_D1 / THS_PER_SAL_DC,
         COMP_real_EMP = CP_MEUR_D1 / deflator_VA / THS_PER_SAL_DC,
         COMP_nom_HW = CP_MEUR_D1 / THS_HW_SAL_DC,
         COMP_real_HW = (CP_MEUR_D1 / deflator_VA) / THS_HW_SAL_DC,
         PROD_nom_EMP = CP_MEUR_B1G / THS_PER_EMP_DC,
         PROD_real_EMP = CLV10_MEUR_B1G / THS_PER_EMP_DC,
         RULC = COMP_nom_EMP / PROD_nom_EMP,
         PROD_real_HW = CLV10_MEUR_B1G / THS_HW_EMP_DC,
         PROD_nom_HW = CP_MEUR_D1 / THS_HW_EMP_DC,
         NULC = COMP_nom_EMP / PROD_real_EMP) |>
  rename(VA_nom = CP_MEUR_B1G,
         VA_real = CLV10_MEUR_B1G,
         COMP_nom = CP_MEUR_D1) |>
  ungroup() |>
  pivot_longer(cols = 4:21, names_to = "Indicator", values_to = "Value") |>
  arrange(time) |>
  group_by(geo, Indicator, nace_r2) |>
  mutate(YOY = ((Value / lag(Value,4))*100-100),
         Indeks2005 = Value /(mean(Value[time >= "2005-01-01"& time <= "2005-10-01"]))*100,
         Indeks2005_4cds = rollmean(Indeks2005,4, fill=NA, align = "right"),
         Indeks2007 = Value /(mean(Value[time >= "2007-01-01"& time <= "2007-10-01"]))*100,
         Indeks2007_4cds = rollmean(Indeks2007,4, fill=NA, align = "right"),
         Indeks2008 = Value /(mean(Value[time >= "2008-01-01"& time <= "2008-10-01"]))*100,
         Indeks2008_4cds = rollmean(Indeks2008,4, fill=NA, align = "right"),
         Indeks2019 = Value /(mean(Value[time >= "2019-01-01"& time <= "2019-10-01"]))*100,
         Indeks2019_4cds = rollmean(Indeks2019,4, fill=NA, align = "right"),
         year = year(time),
         quarter = paste0("Q", quarter(time)),
         Descriptor = indicator_descriptions[Indicator]) |>
  relocate(Descriptor, .after = Indicator) |>
  ungroup()

################################################################################
#            zapis na postgres bazo                                            #
################################################################################
DBI::dbExecute(con, "TRUNCATE TABLE \"RULC_VA_NACE_cetrtletni\"")
# Insert data into the PostgreSQL table
DBI::dbWriteTable(con, "RULC_VA_NACE_cetrtletni", data_nace, append = TRUE, row.names = FALSE)

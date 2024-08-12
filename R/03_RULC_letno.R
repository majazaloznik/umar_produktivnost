################################################################################
#                           Izračun LETNIH RULC in NULC                        #
#         revizija MZ Junij 2024, original Katarina, September 2021            #
################################################################################
####   Knjižnjice   ############################################################
library(eurostat)
library(dplyr)
library(tidyr)

####   setup    ################################################################
source("R/00_geo_lookup.R") # geografije
source("R/00_skd_lookup.R") # skd klasifikaicje
source("R/helper_functions.R")

################################################################################
#                        RULC_BDP_totals                                       #
################################################################################
################################################################################
#                         Download Podatkov                                    #
################################################################################
GDP <- get_eurostat("nama_10_gdp",
                    filters = list(
                      geo = geo_subset,
                      sinceTimePeriod = 1995,
                      na_item = c("B1GQ" , "D1", "D11", "D12"), # Gross domestic product at market prices, compensation of employees, wages and salaries,employers social contributions
                      unit = c("CP_MEUR" , "CLV10_MEUR", "PYP_MEUR")))|> #Current prices million euro, + chain linked volumes 2010, million eur + previous year's prices for CLV calc
  select(-freq) |>
  filter(!is.na(values))

EMP <- get_eurostat("nama_10_a10_e",
                    filters = list(
                      geo = geo_subset,
                      sinceTimePeriod = 1995,
                      na_item = c("EMP_DC" , "SAL_DC", "SELF_DC"), # Total employment domestic concept, employees dc, self employed dc.
                      unit = c("THS_PER", "THS_HW"), # Thousand persons, Thousand hours worked
                      nace_r2 = "TOTAL"))|>
  select(-freq, -nace_r2) |>
  filter(!is.na(values))

################################################################################
#                 Definiranje dodatnih geografskih agregatov                   #
################################################################################

# agregiranje za GDP in preračun CLV-jev
GDP_agr <- GDP |>
  bind_rows(lapply(names(geo_aggregations), function(label) {
    aggregate_geo_and_calculate_clv_annual(GDP, geo_aggregations[[label]], label)}))

# agregiranje za EMP
EMP_agr <- EMP |>
  bind_rows(lapply(names(geo_aggregations), function(label) {
    aggregate_geo_annual(EMP, geo_aggregations[[label]], label)}))

master_agr <- bind_rows(GDP_agr, EMP_agr) |>
  filter(unit != "PYP_MEUR")

################################################################################
#                               PRERAČUNI                                      #
################################################################################
RULC_BDP_totals <-  master_agr |>
  pivot_wider(names_from = c(unit, na_item), values_from = values) |>
  group_by(geo) |>
  mutate(time = lubridate::year(time),
         deflator_GDP = CP_MEUR_B1GQ / CLV10_MEUR_B1GQ,
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
         COMP_nom = CP_MEUR_D1,
         COMP_SAL_nom = CP_MEUR_D11,
         COMP_CONTR_nom = CP_MEUR_D12) |>
  pivot_longer(!c(time,geo), names_to = "Indicator", values_to = "Value") |>
  arrange(time) |>
  group_by(geo, Indicator)%>%
  mutate(YOY = ((Value /lag(Value,1))*100-100),
         Indeks2005 = (Value/(Value[time=="2005"]))*100,
         Indeks2007 = (Value/(Value[time=="2007"]))*100,
         Indeks2008 = (Value/(Value[time=="2008"]))*100,
         Indeks2019 = (Value/(Value[time=="2019"]))*100,
         Descriptor = indicator_descriptions[Indicator]) |>
  relocate(Descriptor, .after = Indicator) |>
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

DBI::dbExecute(con, "TRUNCATE TABLE \"RULC_BDP_total_letni\"")
# Insert data into the PostgreSQL table
DBI::dbWriteTable(con, "RULC_BDP_total_letni", RULC_BDP_totals, append = TRUE, row.names = FALSE)

################################################################################
#                        RULC_VA_NACE                                          #
################################################################################
################################################################################
#                         Download Podatkov                                    #
################################################################################
VA <- get_eurostat("nama_10_a10",
                   filters = list(
                     geo = geo_subset,
                     sinceTimePeriod = 1995,
                     na_item = c("B1G" , "D1"), # Value added, gross, compensation of employees,
                     unit = c("CP_MEUR", "CLV10_MEUR", "PYP_MEUR")))|> # Current prices, million euro
  select(-freq) |>
  filter(!is.na(values))

EMP <- get_eurostat("nama_10_a10_e",
                    filters = list(
                      geo = geo_subset,
                      sinceTimePeriod = 1995,
                      na_item = c("EMP_DC" , "SAL_DC"), # Total employment domestic concept, employees dc
                      unit = c("THS_PER", "THS_HW")))|> # Thousand persons, Thousand hours worked
  select(-freq) |>
  filter(!is.na(values))

################################################################################
#                 Definiranje dodatnih geografskih agregatov                   #
################################################################################

# agregiranje za GDP in preračun CLV-jev
VA_agr_geo <- VA |>
  bind_rows(lapply(names(geo_aggregations), function(label) {
    aggregate_geo_and_calculate_clv_annual(VA, geo_aggregations[[label]], label)}))

# agregiranje za EMP
EMP_agr_geo <- EMP |>
  bind_rows(lapply(names(geo_aggregations), function(label) {
    aggregate_geo_annual(EMP, geo_aggregations[[label]], label)}))

################################################################################
#                         Definiranje dodatnih SKD agregatov                   #
################################################################################
# disagregiranje BCDE v BDE
VA_agr_geo <- VA_agr_geo |>
  bind_rows(disagregate_bde_and_calculate_clv_annual(VA_agr_geo, "nace_r2", "B-E"))
EMP_agr_geo <- EMP_agr_geo |>
  bind_rows(disagregate_bde_annual(EMP_agr_geo, "nace_r2", "B-E"))

# agregiranje za VA in preračun CLV-jev
VA_agr_geo_skd <- VA_agr_geo |>
  bind_rows(lapply(names(sectors_eurostat_a10), function(label) {
    aggregate_skd_and_calculate_clv_annual(VA_agr_geo, sectors_eurostat_a10[[label]], label)}))

# agregiranje za EMP
EMP_agr_geo_skd <- EMP_agr_geo |>
  bind_rows(lapply(names(sectors_eurostat_a10), function(label) {
    aggregate_skd_annual(EMP_agr_geo, sectors_eurostat_a10[[label]], label)}))

####  Združitev tabel   ########################################################
master_agr <- bind_rows(VA_agr_geo_skd, EMP_agr_geo_skd) |>
  filter(unit != "PYP_MEUR")


################################################################################
#                               PRERAČUNI                                      #
################################################################################
RULC_VA_NACE_letni <- master_agr |>
  pivot_wider(names_from = c(unit, na_item),
              values_from = values) |>
  mutate(time = lubridate::year(time)) |>
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
  mutate(YOY = ((Value /lag(Value,1))*100-100),
         Indeks2005 = (Value/(Value[time == 2005])) * 100,
         Indeks2007 = (Value/(Value[time == 2007])) * 100,
         Indeks2008 = (Value/(Value[time == 2008])) * 100,
         Indeks2019 = (Value/(Value[time == 2019])) * 100,
         Descriptor = indicator_descriptions[Indicator]) |>
  relocate(Descriptor, .after = Indicator) |>
  ungroup()

################################################################################
#            zapis na postgres bazo                                            #
################################################################################
DBI::dbExecute(con, "TRUNCATE TABLE \"RULC_VA_NACE_letni\"")
# Insert data into the PostgreSQL table
DBI::dbWriteTable(con, "RULC_VA_NACE_letni", RULC_VA_NACE_letni, append = TRUE, row.names = FALSE)



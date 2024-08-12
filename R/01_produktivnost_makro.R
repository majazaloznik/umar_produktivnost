################################################################################
#         PRODUKTIVNOST IN BDP v PPS
#         revizija MZ Maj 2024, original Katarina, Avgust 2020
################################################################################
####   Knjižnjice   ############################################################
library(eurostat)
library(dplyr)
library(tidyr)
####   setup    ################################################################
source("R/00_geo_lookup.R") # geografije
source("R/helper_functions.R")

################################################################################
#                         Download Podatkov                                    #
################################################################################
GDP <- get_eurostat("nama_10_gdp",
                    filters = list(
                      geo = geo_subset,
                      sinceTimePeriod = 1995,
                      na_item = "B1GQ", # Gross domestic product at market prices
                      unit = c("CP_MPPS_EU27_2020", "CP_MEUR", "CLV10_MEUR", "PYP_MEUR")))|> #Current prices, million purchasing power standards (PPS, EU27 from 2020) + chain linked volumes 2010, million eur
  select(-freq) |>
  filter(!is.na(values))

EMP <- get_eurostat("nama_10_a10_e",
                    filters = list(
                      geo = geo_subset,
                      sinceTimePeriod = 1995,
                      na_item = "EMP_DC", # Total employment domestic concept
                      unit = c("THS_PER" , "THS_HW"), # thousand persons + thousand hours worked
                      nace_r2 ="TOTAL")) |>
  select(-freq, - nace_r2) |>
  filter(!is.na(values))

POP <- get_eurostat("nama_10_pe",
                    filters = list(
                      geo = geo_subset,
                      sinceTimePeriod = 1995,
                      na_item = "POP_NC", # Total population national concept
                      unit = c("THS_PER" ))) |>  # thousand persons
  select(-freq) |>
  filter(!is.na(values))

POP2 <- get_eurostat("demo_pjan",
                     filters = list(
                       geo = geo_subset,
                       sinceTimePeriod = 1995,
                       sex = "T", # Total
                       age = c("TOTAL", paste0("Y", 20:64)))) |>  #age groups
  select(-freq, -sex) |>
  rename(na_item =age) |>
  filter(!is.na(values)) |>
  arrange(na_item, geo, time)  |>
  group_by(na_item, geo)  |>
  mutate(Next_Year_Value = lead(values))  |>
  mutate(Midyear_Pop = (values + Next_Year_Value) / 2)  |>
  filter(!is.na(Midyear_Pop))  |>
  select(unit, na_item, geo, time, Midyear_Pop) |>
  rename(values = Midyear_Pop)

################################################################################
#                 Definiranje dodatnih geografskih agregatov                   #
################################################################################
# agregiranje za GDP in preračun CLV-jev
GDP_agr <- GDP |>
  bind_rows(lapply(names(geo_aggregations), function(label) {
    aggregate_geo_and_calculate_clv_annual(GDP, geo_aggregations[[label]], label)})) |>
  filter(!unit %in% c("CP_MEUR", "PYP_MEUR"))

# agregiranje za EMP
EMP_agr <- EMP |>
  bind_rows(lapply(names(geo_aggregations), function(label) {
    aggregate_geo_annual(EMP, geo_aggregations[[label]], label)}))

# agregiranje za POP
POP_agr <- POP |>
  bind_rows(lapply(names(geo_aggregations), function(label) {
    aggregate_geo_annual(POP, geo_aggregations[[label]], label)}))

# agregiranje za POP2
POP2_agr <- POP2 |>
  bind_rows(lapply(names(geo_aggregations), function(label) {
    aggregate_geo_annual(POP2, geo_aggregations[[label]], label)}))

####  Združitev tabel   ########################################################
master_agr <- bind_rows(GDP_agr, EMP_agr, POP_agr, POP2_agr)

################################################################################
#                               PRERAČUNI                                      #
################################################################################
data_macro <- master_agr %>%
  reshape2::dcast(geo + time ~ unit + na_item, value.var ="values") |>
  rowwise() |>
  mutate(NR_20_64 = sum(c_across(starts_with("NR_Y")))/1000, #običajni prebivalci 20-64
         NR_TOTAL = NR_TOTAL/1000, .keep = "unused") |> # običajni prebivalci skupaj
  mutate(GDP_PC_PPS = CP_MPPS_EU27_2020_B1GQ / THS_PER_POP_NC, # gdp per capita PPS
         GDP_PC_PPS_pjan = CP_MPPS_EU27_2020_B1GQ / NR_TOTAL, # gdp per capita PPS
         PROD_PPS = CP_MPPS_EU27_2020_B1GQ/THS_PER_EMP_DC, # produktivnost PPS
         PROD_PPS_HW = CP_MPPS_EU27_2020_B1GQ/THS_HW_EMP_DC, # produktivnost hours worked
         PROD_real = CLV10_MEUR_B1GQ/THS_PER_EMP_DC, # produktivnost realna
         PROD_real_HW = CLV10_MEUR_B1GQ/THS_HW_EMP_DC, # produktivnost realna hours worked
         EMP_RATE = THS_PER_EMP_DC/THS_PER_POP_NC, # stopnja zaposlenosti
         HW_EMP = THS_HW_EMP_DC/THS_PER_EMP_DC, # število ur na zaposlenega
         EMP_W_AGE_PROP = THS_PER_EMP_DC / NR_20_64, # delež zaposlenih na 20-64
         W_AGE_PROP = NR_20_64 /THS_PER_POP_NC, # delež 20-64 v prebivalstvu
         W_AGE_PROP_pjan = NR_20_64 /NR_TOTAL) |>  # delež 20-64 v prebivalstvu
  ungroup() |>
  group_by(time) |>
  mutate(GDP_PC_PPS_EU27_100 = GDP_PC_PPS / GDP_PC_PPS[geo=="EU27_2020"] * 100,
         GDP_PC_PPS_pjan_EU27_100 = GDP_PC_PPS_pjan / GDP_PC_PPS_pjan[geo=="EU27_2020"] * 100,
         PROD_PPS_EU27_100  = PROD_PPS / PROD_PPS[geo=="EU27_2020"] * 100,
         PROD_PPS_HW_EU27_100 = PROD_PPS_HW / PROD_PPS_HW[geo=="EU27_2020"] * 100,
         EMP_RATE_EU27_100 = EMP_RATE / EMP_RATE[geo=="EU27_2020"] * 100,
         HW_EMP_EU27_100 = HW_EMP / HW_EMP[geo=="EU27_2020"] * 100,
         EMP_W_AGE_PROP_EU27_100 = EMP_W_AGE_PROP / EMP_W_AGE_PROP[geo=="EU27_2020"] * 100,
         W_AGE_PROP_EU27_100 = W_AGE_PROP / W_AGE_PROP[geo=="EU27_2020"] * 100,
         W_AGE_PROP_pjan_EU27_100 = W_AGE_PROP_pjan / W_AGE_PROP_pjan[geo=="EU27_2020"] * 100) |>
  ungroup() |>
  mutate(time = lubridate::year(time),
         agr = ifelse(geo %in% geo_lookup$geo, FALSE, TRUE)) |>
  select(geo, agr, time, CP_MPPS_EU27_2020_B1GQ,
         CLV10_MEUR_B1GQ, THS_PER_EMP_DC, THS_HW_EMP_DC, THS_PER_POP_NC,
         NR_20_64, NR_TOTAL, GDP_PC_PPS, GDP_PC_PPS_pjan, PROD_PPS,
         PROD_PPS_HW, PROD_real, PROD_real_HW, EMP_RATE, HW_EMP,
         EMP_W_AGE_PROP, W_AGE_PROP, W_AGE_PROP_pjan, GDP_PC_PPS_EU27_100,
         GDP_PC_PPS_pjan_EU27_100, PROD_PPS_EU27_100, PROD_PPS_HW_EU27_100,
         EMP_RATE_EU27_100, HW_EMP_EU27_100, EMP_W_AGE_PROP_EU27_100,
         W_AGE_PROP_EU27_100, W_AGE_PROP_pjan_EU27_100)

#### zapis na postgres bazo ####################################################
# Database connection details
con <- DBI::dbConnect(RPostgres::Postgres(),
                      dbname = "produktivnost",
                      host = "localhost",
                      port = 5432,
                      user = "postgres",
                      password = Sys.getenv("PG_PG_PSW"))
DBI::dbExecute(con, "set search_path to produktivnost")
DBI::dbExecute(con, "TRUNCATE TABLE \"produktivnost_makro\"")
# Insert data into the PostgreSQL table
DBI::dbWriteTable(con, "produktivnost_makro", data_macro, append = TRUE, row.names = FALSE)


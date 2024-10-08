################################################################################
#                           Izračun LETNIH RULC in NULC                        #
#         revizija MZ Junij 2024, original Katarina, September 2021            #
################################################################################

####   Knjižnjice   ############################################################
library(eurostat)
library(dplyr)
library(tidyr)
library(data.table)

####   setup    ################################################################
source("R/00_geo_lookup.R") # geografije
source("R/00_skd_lookup.R") # skd klasifikaicje

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
                      unit = c("CP_MEUR" , "CLV10_MEUR")))|> #Current prices million euro, + chain linked volumes 2010, million eur
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
# funkcija za agregiranje
aggregate <- function(df, group, group_name) {
  df %>%
    filter(geo %in% group) %>%
    group_by(na_item, time, unit) %>%
    summarise(values = sum(values, na.rm = TRUE), .groups = 'drop') %>%
    mutate(geo = group_name)
}

# agregiranje za GDP
gdp_eu13 <- aggregate(GDP,country_code_EU13, "EU13")
gdp_eu14 <- aggregate(GDP,country_code_EU14, "EU14")
gdp_EU27 <- aggregate(GDP,country_code_EU27, "EU27")
gdp_EU27noIE <- aggregate(GDP,country_code_EU27_noIE, "EU27noIE")
gdp_EA20 <- aggregate(GDP,country_code_EA20, "EA20")
gdp_EAnoIE <- aggregate(GDP,country_code_EA_noIE, "EAnoIE")
gdp_inovatorke <- aggregate(GDP,country_code_inovatorke, "inovatorke")
gdp_V4 <- aggregate(GDP,country_code_V4, "V4")

# agregiranje za EMP
emp_eu13 <- aggregate(EMP,country_code_EU13, "EU13")
emp_eu14 <- aggregate(EMP,country_code_EU14, "EU14")
emp_EU27 <- aggregate(EMP,country_code_EU27, "EU27")
emp_EU27noIE <- aggregate(EMP,country_code_EU27_noIE, "EU27noIE")
emp_EA20 <- aggregate(EMP,country_code_EA20, "EA20")
emp_EAnoIE <- aggregate(EMP,country_code_EA_noIE, "EAnoIE")
emp_inovatorke <- aggregate(EMP,country_code_inovatorke, "inovatorke")
emp_V4 <- aggregate(EMP,country_code_V4, "V4")

# Združitev baz
GDP_agr <- bind_rows(GDP, gdp_eu13, gdp_eu14, gdp_EU27, gdp_EU27noIE,
                     gdp_EA20, gdp_EAnoIE, gdp_inovatorke, gdp_V4)
EMP_agr <- bind_rows(EMP, emp_eu13, emp_eu14, emp_EU27, emp_EU27noIE,
                     emp_EA20, emp_EAnoIE, emp_inovatorke, emp_V4)
master_agr <- bind_rows(GDP_agr, EMP_agr)

################################################################################
#                               PRERAČUNI                                      #
################################################################################


RULC_BDP_totals <-  master_agr |>
  pivot_wider(names_from = c(unit, na_item), values_from = values) |>
  group_by(geo) |>
  mutate(time = year(time),
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
                      host = "192.168.38.21",
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
                     unit = c("CP_MEUR", "CLV10_MEUR")))|> # Current prices, million euro
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
#                         Definiranje dodatnih SKD agregatov                   #
################################################################################
# funkcija za agregiranje
aggregate_skd <- function(df, group, group_name) {
  df %>%
    filter(nace_r2 %in% group) %>%
    group_by(na_item,geo, time, unit) %>%
    summarise(values = sum(values, na.rm = TRUE), .groups = 'drop') %>%
    mutate(nace_r2 = group_name)
}

va_menj <- aggregate_skd(VA, MENJALNI_sektor, "MENJALNI_sektor")
va_nemenj <- aggregate_skd(VA, NEMENJALNI_sektor, "NEMENJALNI_sektor")
va_posl <- aggregate_skd(VA, POSLOVNI_sektor, "POSLOVNI_sektor")
va_neposl <- aggregate_skd(VA, NEPOSLOVNI_sektor, "NEPOSLOVNI_sektor")
va_trg <-  aggregate_skd(VA, TRZNE, "TRŽNE")
va_ost <- aggregate_skd(VA, OSTALE, "OSTALE")

emp_menj <- aggregate_skd(EMP, MENJALNI_sektor, "MENJALNI_sektor")
emp_nemenj <- aggregate_skd(EMP, NEMENJALNI_sektor, "NEMENJALNI_sektor")
emp_posl <- aggregate_skd(EMP, POSLOVNI_sektor, "POSLOVNI_sektor")
emp_neposl <- aggregate_skd(EMP, NEPOSLOVNI_sektor, "NEPOSLOVNI_sektor")
emp_trg <- aggregate_skd(EMP, TRZNE, "TRŽNE")
emp_ost <- aggregate_skd(EMP, OSTALE, "OSTALE")

####  Združitev tabel   ########################################################
VA_agr <- bind_rows(VA, va_menj, va_nemenj, va_posl, va_neposl, va_trg, va_ost)
EMP_agr <- bind_rows(EMP, emp_menj, emp_nemenj, emp_posl, emp_neposl, emp_trg, emp_ost)

################################################################################
#                 Definiranje dodatnih geografskih agregatov                   #
################################################################################
# funkcija za agregiranje
aggregate <- function(df, group, group_name) {
  df %>%
    filter(geo %in% group) %>%
    group_by(na_item, time, nace_r2, unit) %>%
    summarise(values = sum(values, na.rm = TRUE), .groups = 'drop') %>%
    mutate(geo = group_name)
}

# agregiranje za VA
va_eu13 <- aggregate(VA_agr,country_code_EU13, "EU13")
va_eu14 <- aggregate(VA_agr,country_code_EU14, "EU14")
va_EU27 <- aggregate(VA_agr,country_code_EU27, "EU27")
va_EU27noIE <- aggregate(VA_agr,country_code_EU27_noIE, "EU27noIE")
va_EA20 <- aggregate(VA_agr,country_code_EA20, "EA20")
va_EAnoIE <- aggregate(VA_agr,country_code_EA_noIE, "EAnoIE")
va_inovatorke <- aggregate(VA_agr,country_code_inovatorke, "inovatorke")
va_V4 <- aggregate(VA_agr,country_code_V4, "V4")

# agregiranje za EMP
emp_eu13 <- aggregate(EMP_agr,country_code_EU13, "EU13")
emp_eu14 <- aggregate(EMP_agr,country_code_EU14, "EU14")
emp_EU27 <- aggregate(EMP_agr,country_code_EU27, "EU27")
emp_EU27noIE <- aggregate(EMP_agr,country_code_EU27_noIE, "EU27noIE")
emp_EA20 <- aggregate(EMP_agr,country_code_EA20, "EA20")
emp_EAnoIE <- aggregate(EMP_agr,country_code_EA_noIE, "EAnoIE")
emp_inovatorke <- aggregate(EMP_agr,country_code_inovatorke, "inovatorke")
emp_V4 <- aggregate(EMP_agr,country_code_V4, "V4")

# Združitev baz
VA_agr <- bind_rows(VA_agr, va_eu13, va_eu14, va_EU27, va_EU27noIE,
                     va_EA20, va_EAnoIE, va_inovatorke, va_V4)
EMP_agr <- bind_rows(EMP_agr, emp_eu13, emp_eu14, emp_EU27, emp_EU27noIE,
                     emp_EA20, emp_EAnoIE, emp_inovatorke, emp_V4)
master_agr <- bind_rows(VA_agr, EMP_agr)

################################################################################
#                               PRERAČUNI                                      #
################################################################################
RULC_VA_NACE_letni <- master_agr |>
  pivot_wider(names_from = c(unit, na_item),
              values_from = values) |>
  mutate(time = year(time)) |>
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

# Disconnect from the database
DBI::dbDisconnect(con)



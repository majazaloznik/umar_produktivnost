################################################################################
#         PRODUKTIVNOST IN BDP v PPS
#         revizija MZ Maj 2024, original Katarina, Avgust 2020
################################################################################

####   Knjižnjice   ############################################################
library(eurostat)
library(dplyr)
library(data.table)

####   setup    ################################################################
geo_subset <- c("EU28" , "EU15" , "EA19" , "EU27_2020",
                "BE" , "BG" , "CZ" , "DK" , "DE" , "EE" , "IE" ,"EL" , "ES" ,
                "FR" , "HR" , "IT" , "CY" , "LV" , "LT" ,
                "LU" , "HU" , "MT" ,"NL" , "AT" , "FR" ,
                "HR" , "IT" , "CY" , "LV" , "LT" , "LU" ,
                "HU" , "MT" ,"NL" , "AT" ,"PL" , "PT" ,
                "RO" , "SI" , "SK" , "FI" , "SE", "UK" )

country_code_EU13 <- c("BG", "CZ", "EE", "HR", "CY",
                       "LV", "LT", "HU", "MT", "PL",
                       "RO", "SI", "SK")

country_code_EU14 <- c("BE", "DK", "DE", "IE", "EL",
                       "ES", "FR", "IT", "LU", "NL",
                       "AT", "PT", "FI", "SE" )

country_code_EU27_noIE <- c("BE", "BG", "CZ", "DK", "DE",
                            "EE", "EL", "ES", "FR", "HR",
                            "IT", "CY", "LV", "LT", "LU",
                            "HU", "MT", "NL", "AT", "PL",
                            "PT", "RO", "SI", "SK", "FI",
                            "SE" )

country_code_EA_noIE <- c("BE", "DE", "EE", "HR", "EL",
                          "ES", "FR", "IT", "CY", "LV",
                          "LT", "LU", "MT", "NL", "AT",
                          "PT", "SI", "SK", "FI")

country_code_inovatorke <- c("BE", "DK", "SE", "FI", "NL")

country_code_V4 <- c("CZ", "HU", "SK", "PL")

last_year <- lubridate::year(Sys.Date()) - 1

####  Download Podatkov ########################################################
GDP <- get_eurostat("nama_10_gdp",
                    filters = list(
                      geo = geo_subset,
                      time = 1995:last_year,
                      na_item = "B1GQ", # Gross domestic product at market prices
                      unit = c("CP_MPPS_EU27_2020" , "CLV10_MEUR")))|> #Current prices, million purchasing power standards (PPS, EU27 from 2020) + chain linked volumes 2010, million eur
  select(-freq) |>
  filter(!is.na(values))

EMP <- get_eurostat("nama_10_a10_e",
                    filters = list(
                      geo = geo_subset,
                      time = 1995:last_year,
                      na_item = "EMP_DC", # Total employment domestic concept
                      unit = c("THS_PER" , "THS_HW"), # thousand persons + thousand hours worked
                      nace_r2 ="TOTAL")) |>
  select(-freq, - nace_r2) |>
  filter(!is.na(values))

POP <- get_eurostat("nama_10_pe",
                    filters = list(
                      geo = geo_subset,
                      time = 1995:last_year,
                      na_item = "POP_NC", # Total population national concept
                      unit = c("THS_PER" ))) |>  # thousand persons
  select(-freq) |>
  filter(!is.na(values))

####  Definiranje dodatnih EU agregatov ########################################
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
gdp_EU27noIE <- aggregate(GDP,country_code_EU27_noIE, "EU27noIE")
gdp_EAnoIE <- aggregate(GDP,country_code_EA_noIE, "EAnoIE")
gdp_inovatorke <- aggregate(GDP,country_code_inovatorke, "inovatorke")
gdp_V4 <- aggregate(GDP,country_code_V4, "V4")

# agregiranje za EMP
emp_eu13 <- aggregate(EMP,country_code_EU13, "EU13")
emp_eu14 <- aggregate(EMP,country_code_EU14, "EU14")
emp_EU27noIE <- aggregate(EMP,country_code_EU27_noIE, "EU27noIE")
emp_EAnoIE <- aggregate(EMP,country_code_EA_noIE, "EAnoIE")
emp_inovatorke <- aggregate(EMP,country_code_inovatorke, "inovatorke")
emp_V4 <- aggregate(EMP,country_code_V4, "V4")

# agregiranje za POP
pop_eu13 <- aggregate(POP,country_code_EU13, "EU13")
pop_eu14 <- aggregate(POP,country_code_EU14, "EU14")
pop_EU27noIE <- aggregate(POP,country_code_EU27_noIE, "EU27noIE")
pop_EAnoIE <- aggregate(POP,country_code_EA_noIE, "EAnoIE")
pop_inovatorke <- aggregate(POP,country_code_inovatorke, "inovatorke")
pop_V4 <- aggregate(POP,country_code_V4, "V4")

####  Združitev tabel   ########################################################
GDP <- bind_rows(GDP, gdp_eu13, gdp_eu14, gdp_EU27noIE,
                 gdp_EAnoIE, gdp_inovatorke, gdp_V4)
EMP <- bind_rows(EMP, emp_eu13, emp_eu14, emp_EU27noIE,
                 emp_EAnoIE, emp_inovatorke, emp_V4)
POP <- bind_rows(POP, pop_eu13, pop_eu14, pop_EU27noIE,
                 pop_EAnoIE, pop_inovatorke, pop_V4)
master <- bind_rows(GDP, EMP, POP)

####  Preračuni ################################################################
data_macro <- master %>%
  reshape2::dcast(geo + time ~ unit + na_item, value.var ="values") |>
  dplyr::mutate(GDP_PC_PPS = CP_MPPS_EU27_2020_B1GQ / THS_PER_POP_NC) |> # gdp per capita PPS
  dplyr::mutate(PROD_PPS = CP_MPPS_EU27_2020_B1GQ/THS_PER_EMP_DC) |> # produktivnost PPS
  dplyr::mutate(PROD_PPS_HW = CP_MPPS_EU27_2020_B1GQ/THS_HW_EMP_DC) |> # produktivnost hours worked
  dplyr::mutate(PROD_real = CLV10_MEUR_B1GQ/THS_PER_EMP_DC) |> # produktivnost realna
  dplyr::mutate(PROD_real_HW = CLV10_MEUR_B1GQ/THS_HW_EMP_DC) |> # produktivnost realna hours worked
  dplyr::mutate(EMP_RATE = THS_PER_EMP_DC/THS_PER_POP_NC) |> # stopnja zaposlenosti
  dplyr::mutate(HW_EMP = THS_HW_EMP_DC/THS_PER_EMP_DC) |> # število ur na zaposlenega
  ungroup() |>
  dplyr::group_by(time) |>
  dplyr::mutate(GDP_PC_PPS_EU27_100 = GDP_PC_PPS / GDP_PC_PPS[geo=="EU27_2020"] * 100) |>
  dplyr::mutate(PROD_PPS_EU27_100  = PROD_PPS / PROD_PPS[geo=="EU27_2020"] * 100) |>
  dplyr::mutate(PROD_PPS_EU27_100_HW = PROD_PPS_HW / PROD_PPS_HW[geo=="EU27_2020"] * 100) |>
  dplyr::mutate(EMP_RATE_EU27_100 = EMP_RATE / EMP_RATE[geo=="EU27_2020"] * 100) |>
  dplyr::mutate(GDP_PC_PPS_EU27_100 = GDP_PC_PPS / GDP_PC_PPS[geo=="EU27_2020"] * 100) |>
  ungroup()


# #####  Access   #####


# Database connection details
con <- DBI::dbConnect(RPostgres::Postgres(),
                      dbname = "prod-test",
                      host = "localhost",
                      port = 5432,
                      user = "postgres",
                      password = Sys.getenv("PG_local_16_PG_PSW"))
DBI::dbExecute(con, "set search_path to produktivnost")


# Insert data into the PostgreSQL table
dbWriteTable(con, "produktivnost_makro", data_macro, append = TRUE, row.names = FALSE)

# Disconnect from the database
dbDisconnect(con)

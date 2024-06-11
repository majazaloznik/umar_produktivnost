################################################################################
#         PRODUKTIVNOST IN BDP v PPS
#         revizija MZ Maj 2024, original Katarina, Avgust 2020
################################################################################

####   Knjižnjice   ############################################################
library(eurostat)
library(dplyr)
library(data.table)

####   setup    ################################################################
source("R/00_geo_lookup.R") # geografije

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

POP2 <- get_eurostat("demo_pjan",
                    filters = list(
                      geo = geo_subset,
                      time = 1995:last_year,
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
gdp_EU27 <- aggregate(GDP,country_code_EU27, "EU27")
gdp_EU27noIE <- aggregate(GDP,country_code_EU27_noIE, "EU27noIE")
gdp_EA20 <- aggregate(GDP,country_code_EU27, "EA20")
gdp_EAnoIE <- aggregate(GDP,country_code_EA_noIE, "EAnoIE")
gdp_inovatorke <- aggregate(GDP,country_code_inovatorke, "inovatorke")
gdp_V4 <- aggregate(GDP,country_code_V4, "V4")

# agregiranje za EMP
emp_eu13 <- aggregate(EMP,country_code_EU13, "EU13")
emp_eu14 <- aggregate(EMP,country_code_EU14, "EU14")
emp_EU27 <- aggregate(EMP,country_code_EU27, "EU27")
emp_EU27noIE <- aggregate(EMP,country_code_EU27_noIE, "EU27noIE")
emp_EA20 <- aggregate(EMP,country_code_EA_noIE, "EA20")
emp_EAnoIE <- aggregate(EMP,country_code_EA_noIE, "EAnoIE")
emp_inovatorke <- aggregate(EMP,country_code_inovatorke, "inovatorke")
emp_V4 <- aggregate(EMP,country_code_V4, "V4")

# agregiranje za POP
pop_eu13 <- aggregate(POP,country_code_EU13, "EU13")
pop_eu14 <- aggregate(POP,country_code_EU14, "EU14")
pop_EU27 <- aggregate(POP,country_code_EU27, "EU27")
pop_EU27noIE <- aggregate(POP,country_code_EU27_noIE, "EU27noIE")
pop_EA20 <- aggregate(POP,country_code_EA_noIE, "EA20")
pop_EAnoIE <- aggregate(POP,country_code_EA_noIE, "EAnoIE")
pop_inovatorke <- aggregate(POP,country_code_inovatorke, "inovatorke")
pop_V4 <- aggregate(POP,country_code_V4, "V4")

# agregiranje za POP2
pop2_eu13 <- aggregate(POP2,country_code_EU13, "EU13")
pop2_eu14 <- aggregate(POP2,country_code_EU14, "EU14")
pop2_EU27 <- aggregate(POP2,country_code_EU27, "EU27")
pop2_EU27noIE <- aggregate(POP2,country_code_EU27_noIE, "EU27noIE")
pop2_EA20 <- aggregate(POP2,country_code_EA_noIE, "EA20")
pop2_EAnoIE <- aggregate(POP2,country_code_EA_noIE, "EAnoIE")
pop2_inovatorke <- aggregate(POP2,country_code_inovatorke, "inovatorke")
pop2_V4 <- aggregate(POP2,country_code_V4, "V4")


####  Združitev tabel   ########################################################
GDP_agr <- bind_rows(GDP, gdp_eu13, gdp_eu14, gdp_EU27, gdp_EU27noIE,
                 gdp_EA20, gdp_EAnoIE, gdp_inovatorke, gdp_V4)
EMP_agr <- bind_rows(EMP, emp_eu13, emp_eu14, emp_EU27, emp_EU27noIE,
                 emp_EA20, emp_EAnoIE, emp_inovatorke, emp_V4)
POP_agr <- bind_rows(POP, pop_eu13, pop_eu14, pop_EU27, pop_EU27noIE,
                 pop_EA20, pop_EAnoIE, pop_inovatorke, pop_V4)
POP2_agr <- bind_rows(POP2, pop2_eu13, pop2_eu14, pop2_EU27, pop2_EU27noIE,
                  pop2_EA20, pop2_EAnoIE, pop2_inovatorke, pop2_V4)
master_agr <- bind_rows(GDP_agr, EMP_agr, POP_agr, POP2_agr)

# saveRDS(master, "data/master01.rds")
####  Preračuni ################################################################
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
  mutate(GDP_PC_PPS_EU27_100 = GDP_PC_PPS / GDP_PC_PPS[geo=="EU27"] * 100,
         GDP_PC_PPS_pjan_EU27_100 = GDP_PC_PPS_pjan / GDP_PC_PPS_pjan[geo=="EU27"] * 100,
         PROD_PPS_EU27_100  = PROD_PPS / PROD_PPS[geo=="EU27"] * 100,
         PROD_PPS_HW_EU27_100 = PROD_PPS_HW / PROD_PPS_HW[geo=="EU27"] * 100,
         EMP_RATE_EU27_100 = EMP_RATE / EMP_RATE[geo=="EU27"] * 100,
         HW_EMP_EU27_100 = HW_EMP / HW_EMP[geo=="EU27"] * 100,
         EMP_W_AGE_PROP_EU27_100 = EMP_W_AGE_PROP / EMP_W_AGE_PROP[geo=="EU27"] * 100,
         W_AGE_PROP_EU27_100 = W_AGE_PROP / W_AGE_PROP[geo=="EU27"] * 100,
         W_AGE_PROP_pjan_EU27_100 = W_AGE_PROP_pjan / W_AGE_PROP_pjan[geo=="EU27"] * 100) |>
  ungroup() |>
mutate(time = lubridate::year(time)) |>
  select(geo, time, CP_MPPS_EU27_2020_B1GQ,
         CLV10_MEUR_B1GQ, THS_PER_EMP_DC, THS_HW_EMP_DC, THS_PER_POP_NC,
         NR_20_64, NR_TOTAL, GDP_PC_PPS, GDP_PC_PPS_pjan, PROD_PPS,
         PROD_PPS_HW, PROD_real, PROD_real_HW, EMP_RATE, HW_EMP,
         EMP_W_AGE_PROP, W_AGE_PROP, W_AGE_PROP_pjan, GDP_PC_PPS_EU27_100,
         GDP_PC_PPS_pjan_EU27_100, PROD_PPS_EU27_100, PROD_PPS_HW_EU27_100,
         EMP_RATE_EU27_100, HW_EMP_EU27_100, EMP_W_AGE_PROP_EU27_100,
         W_AGE_PROP_EU27_100, W_AGE_PROP_pjan_EU27_100)


# saveRDS(data_macro, "data/data_macro01.rds")


#### zapis na postgres bazo ####################################################
# Database connection details
con <- DBI::dbConnect(RPostgres::Postgres(),
                      dbname = "prod-test",
                      host = "localhost",
                      port = 5432,
                      user = "postgres",
                      password = Sys.getenv("PG_local_16_PG_PSW"))
DBI::dbExecute(con, "set search_path to produktivnost")

# Insert data into the PostgreSQL table
dbWriteTable(con, "produktivnost_makro", data_macro, overwrite = TRUE, row.names = FALSE)

# Disconnect from the database
dbDisconnect(con)

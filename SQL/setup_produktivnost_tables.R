# Load necessary libraries
library(DBI)
library(RPostgres)
library(dplyr)

# Assuming `data_macro` is already created as per the given code

# Database connection details
con <- DBI::dbConnect(RPostgres::Postgres(),
                      dbname = "prod-test",
                      host = "localhost",
                      port = 5432,
                      user = "postgres",
                      password = Sys.getenv("PG_local_16_PG_PSW"))
DBI::dbExecute(con, "set search_path to produktivnost")

# table deletion command
drop_table_sql <- "DROP TABLE IF EXISTS produktivnost_makro;"
dbExecute(con, drop_table_sql)

# table creation
create_table_sql <- '
CREATE TABLE "produktivnost_makro" (
    "geo" VARCHAR,
    "time" INTEGER,
    "CP_MPPS_EU27_2020_B1GQ" DOUBLE PRECISION,
    "CLV10_MEUR_B1GQ" DOUBLE PRECISION,
    "THS_PER_EMP_DC" DOUBLE PRECISION,
    "THS_HW_EMP_DC" DOUBLE PRECISION,
    "THS_PER_POP_NC" DOUBLE PRECISION,
    "NR_20_64" DOUBLE PRECISION,
    "NR_TOTAL" DOUBLE PRECISION,
    "GDP_PC_PPS" DOUBLE PRECISION,
    "GDP_PC_PPS_pjan" DOUBLE PRECISION,
    "PROD_PPS" DOUBLE PRECISION,
    "PROD_PPS_HW" DOUBLE PRECISION,
    "PROD_real" DOUBLE PRECISION,
    "PROD_real_HW" DOUBLE PRECISION,
    "EMP_RATE" DOUBLE PRECISION,
    "HW_EMP" DOUBLE PRECISION,
    "EMP_W_AGE_PROP" DOUBLE PRECISION,
    "W_AGE_PROP" DOUBLE PRECISION,
    "W_AGE_PROP_pjan" DOUBLE PRECISION,
    "GDP_PC_PPS_EU27_100" DOUBLE PRECISION,
    "GDP_PC_PPS_pjan_EU27_100" DOUBLE PRECISION,
    "PROD_PPS_EU27_100" DOUBLE PRECISION,
    "PROD_PPS_HW_EU27_100" DOUBLE PRECISION,
    "EMP_RATE_EU27_100" DOUBLE PRECISION,
    "HW_EMP_EU27_100" DOUBLE PRECISION,
    "EMP_W_AGE_PROP_EU27_100" DOUBLE PRECISION,
    "W_AGE_PROP_EU27_100" DOUBLE PRECISION,
    "W_AGE_PROP_pjan_EU27_100" DOUBLE PRECISION,
     CONSTRAINT unique_geo_time UNIQUE (geo, time)
);
'

# Execute the table creation command
dbExecute(con, create_table_sql)

# Disconnect from the database
dbDisconnect(con)


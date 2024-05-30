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
                      password = "Uc1JLH3wisMpxEo")
DBI::dbExecute(con, "set search_path to produktivnost")


create_table_sql <- "
CREATE TABLE produktivnost_makro (
    geo VARCHAR NOT NULL,
    time INTEGER NOT NULL,
    CLV10_MEUR_B1GQ DOUBLE PRECISION,
    CP_MPPS_EU27_2020_B1GQ DOUBLE PRECISION,
    THS_PER_EMP_DC DOUBLE PRECISION,
    THS_HW_EMP_DC DOUBLE PRECISION,
    THS_PER_POP_NC DOUBLE PRECISION,
    CP_MPPS_EU27_2020_B1GQ DOUBLE PRECISION,
    THS_PER_POP_NC DOUBLE PRECISION,
    THS_PER_EMP_DC DOUBLE PRECISION,
    THS_HW_EMP_DC DOUBLE PRECISION,
    CLV10_MEUR_B1GQ DOUBLE PRECISION,
    GDP_PC_PPS DOUBLE PRECISION,
    PROD_PPS DOUBLE PRECISION,
    PROD_PPS_HW DOUBLE PRECISION,
    PROD_real DOUBLE PRECISION,
    PROD_real_HW DOUBLE PRECISION,
    EMP_RATE DOUBLE PRECISION,
    HW_EMP DOUBLE PRECISION,
    GDP_PC_PPS_EU27_100 DOUBLE PRECISION,
    PROD_PPS_EU27_100 DOUBLE PRECISION,
    PROD_PPS_EU27_100_HW DOUBLE PRECISION,
    EMP_RATE_EU27_100 DOUBLE PRECISION,
    PRIMARY KEY (geo, time)
);
"

# Execute the table creation command
dbExecute(con, create_table_sql)


# Disconnect from the database
dbDisconnect(con)
# Load necessary libraries
library(DBI)
library(RPostgres)
library(dplyr)

# Assuming `data_macro` is already created as per the given code

# Database connection details
con <- DBI::dbConnect(RPostgres::Postgres(),
                      dbname = "produktivnost",
                      host = "192.168.38.21",
                      port = 5432,
                      user = "postgres",
                      password = Sys.getenv("PG_PG_PSW"))
DBI::dbExecute(con, "set search_path to produktivnost")

#### geo_lookup ################################################################

# table deletion command
drop_table_sql <- "DROP TABLE IF EXISTS geo_lookup;"
dbExecute(con, drop_table_sql)

# table creation
create_table_sql <- '
CREATE TABLE "geo_lookup" (
    "geo" VARCHAR,
    "EU28" boolean,
    "EU15" boolean,
    "EA20"  boolean,
    "EA19" boolean,
    "EU27" boolean,
    "EU13" boolean,
    "EU14" boolean,
    "EU27_noIE" boolean,
    "EA_noIE" boolean,
    "inovatorke" boolean,
    "V4" boolean,
     UNIQUE (geo));'
# Execute the table creation command
dbExecute(con, create_table_sql)

#### produktivnost_makro########################################################

# table deletion command
drop_table_sql <- "DROP TABLE IF EXISTS produktivnost_makro;"
dbExecute(con, drop_table_sql)

# table creation
create_table_sql <- '
CREATE TABLE "produktivnost_makro" (
    "geo" VARCHAR,
    "agr" BOOLEAN,
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
     UNIQUE (geo, time));'
# Execute the table creation command
dbExecute(con, create_table_sql)

#### produktivnost_shift_share  ################################################
# table deletion command
drop_table_sql <- "DROP TABLE IF EXISTS produktivnost_shift_share;"
dbExecute(con, drop_table_sql)

# table creation
create_table_sql <- '
CREATE TABLE "produktivnost_shift_share" (
  nace_r2 VARCHAR,
  nace_level VARCHAR,
  nace_descr VARCHAR,
  geo VARCHAR,
  geo_agr VARCHAR,
  time INTEGER,
  "VA_nom" DOUBLE PRECISION,
  "VA_share_nom" DOUBLE PRECISION,
  "VA_real" DOUBLE PRECISION,
  "VA_share_real" DOUBLE PRECISION,
  "THS_HW" DOUBLE PRECISION,
  "THS_PER" DOUBLE PRECISION,
  "EMP_share" DOUBLE PRECISION,
  "PROD_real_EMP" DOUBLE PRECISION,
  "PROD_nom_EMP" DOUBLE PRECISION,
  "PROD_diff_EMP" DOUBLE PRECISION,
  "PROD_yoy_EMP" DOUBLE PRECISION,
  "EMP_share_diff" DOUBLE PRECISION,
  "WITHIN_effect_EMP" DOUBLE PRECISION,
  "STATIC_shift_EMP" DOUBLE PRECISION,
  "DYNAMIC_shift_EMP" DOUBLE PRECISION,
  "STRUCT_shift_EMP" DOUBLE PRECISION,
  "HW_share" DOUBLE PRECISION,
  "PROD_real_HW" DOUBLE PRECISION,
  "PROD_nom_HW" DOUBLE PRECISION,
  "PROD_diff_HW" DOUBLE PRECISION,
  "PROD_yoy_HW" DOUBLE PRECISION,
  "HW_share_diff" DOUBLE PRECISION,
  "WITHIN_effect_HW" DOUBLE PRECISION,
  "STATIC_shift_HW" DOUBLE PRECISION,
  "DYNAMIC_shift_HW" DOUBLE PRECISION,
  "STRUCT_shift_HW" DOUBLE PRECISION,
  UNIQUE (nace_r2, geo, time));'

# Execute the table creation command
dbExecute(con, create_table_sql)


#### RULC_letni_total  ################################################
# table deletion command
drop_table_sql <- "DROP TABLE IF EXISTS \"RULC_BDP_total_letni\";"
dbExecute(con, drop_table_sql)

# table creation
create_table_sql <- '
CREATE TABLE "RULC_BDP_total_letni" (
  geo VARCHAR,
  time INTEGER,
  "Indicator" VARCHAR,
  "Descriptor" VARCHAR,
  "Value" DOUBLE PRECISION,
  "YOY" DOUBLE PRECISION,
  "Indeks2005" DOUBLE PRECISION,
  "Indeks2007" DOUBLE PRECISION,
  "Indeks2008" DOUBLE PRECISION,
  "Indeks2019" DOUBLE PRECISION,
  UNIQUE (geo, time, \"Indicator\"));'

# Execute the table creation command
dbExecute(con, create_table_sql)

#### RULC_letni_NACE  ################################################
# table deletion command
drop_table_sql <- "DROP TABLE IF EXISTS \"RULC_VA_NACE_letni\";"
dbExecute(con, drop_table_sql)

# table creation
create_table_sql <- '
CREATE TABLE "RULC_VA_NACE_letni" (
  nace_r2 VARCHAR,
  geo VARCHAR,
  time INTEGER,
  "Indicator" VARCHAR,
  "Descriptor" VARCHAR,
  "Value" DOUBLE PRECISION,
  "YOY" DOUBLE PRECISION,
  "Indeks2005" DOUBLE PRECISION,
  "Indeks2007" DOUBLE PRECISION,
  "Indeks2008" DOUBLE PRECISION,
  "Indeks2019" DOUBLE PRECISION,
  UNIQUE (nace_r2, geo, time, \"Indicator\"));'

# Execute the table creation command
dbExecute(con, create_table_sql)


#### RULC_cetrtletni_total  ################################################
# table deletion command
drop_table_sql <- "DROP TABLE IF EXISTS \"RULC_BDP_total_cetrtletni\";"
dbExecute(con, drop_table_sql)

# table creation
create_table_sql <- '
CREATE TABLE "RULC_BDP_total_cetrtletni" (
  geo VARCHAR,
  time DATE,
  s_adj VARCHAR,
  "Indicator" VARCHAR,
  "Descriptor" VARCHAR,
  "Value" DOUBLE PRECISION,
  "YOY" DOUBLE PRECISION,
  "Indeks2005" DOUBLE PRECISION,
  "Indeks2005_4cds" DOUBLE PRECISION,
  "Indeks2007" DOUBLE PRECISION,
  "Indeks2007_4cds" DOUBLE PRECISION,
  "Indeks2008" DOUBLE PRECISION,
  "Indeks2008_4cds" DOUBLE PRECISION,
  "Indeks2019" DOUBLE PRECISION,
  "Indeks2019_4cds" DOUBLE PRECISION,
  year INTEGER,
  quarter VARCHAR,
  UNIQUE (geo, time, s_adj, \"Indicator\"));'

# Execute the table creation command
dbExecute(con, create_table_sql)

#### RULC_cetrtletni_NACE  ################################################
# table deletion command
drop_table_sql <- "DROP TABLE IF EXISTS \"RULC_VA_NACE_cetrtletni\";"
dbExecute(con, drop_table_sql)

# table creation
create_table_sql <- '
CREATE TABLE "RULC_VA_NACE_cetrtletni" (
  nace_r2 VARCHAR,
  geo VARCHAR,
  time DATE,
  "Indicator" VARCHAR,
  "Descriptor" VARCHAR,
  "Value" DOUBLE PRECISION,
  "YOY" DOUBLE PRECISION,
  "Indeks2005" DOUBLE PRECISION,
  "Indeks2005_4cds" DOUBLE PRECISION,
  "Indeks2007" DOUBLE PRECISION,
  "Indeks2007_4cds" DOUBLE PRECISION,
  "Indeks2008" DOUBLE PRECISION,
  "Indeks2008_4cds" DOUBLE PRECISION,
  "Indeks2019" DOUBLE PRECISION,
  "Indeks2019_4cds" DOUBLE PRECISION,
  year INTEGER,
  quarter VARCHAR,
  UNIQUE (nace_r2, geo, time, \"Indicator\"));'

# Execute the table creation command
dbExecute(con, create_table_sql)

#### SURS četrtletna produktivnost_shift_share  ################################
# table deletion command
drop_table_sql <- "DROP TABLE IF EXISTS \"SURS_shift_share_cetrtletni\";"
dbExecute(con, drop_table_sql)

# table creation
create_table_sql <- '
CREATE TABLE "SURS_shift_share_cetrtletni" (
  "time" DATE,
  "SKD" VARCHAR,
  "DEJAVNOST" VARCHAR,
  "VA_nom" DOUBLE PRECISION,
  "VA_real" DOUBLE PRECISION,
  "EMP_HW" DOUBLE PRECISION,
  "EMP_PER" DOUBLE PRECISION,
  "EMP_share" DOUBLE PRECISION,
  "HW_share" DOUBLE PRECISION,
  "VA_share_nom" DOUBLE PRECISION,
  "VA_share_real" DOUBLE PRECISION,
  "PROD_real_EMP" DOUBLE PRECISION,
  "PROD_real_HW" DOUBLE PRECISION,
  "PROD_nom_EMP" DOUBLE PRECISION,
  "PROD_nom_HW" DOUBLE PRECISION,
  "PROD_yoy_EMP" DOUBLE PRECISION,
  "PROD_diff_EMP" DOUBLE PRECISION,
  "EMP_share_diff" DOUBLE PRECISION,
  "WITHIN_effect_EMP" DOUBLE PRECISION,
  "STATIC_shift_EMP" DOUBLE PRECISION,
  "DYNAMIC_shift_EMP" DOUBLE PRECISION,
  "STRUCT_shift_EMP" DOUBLE PRECISION,
  "PROD_yoy_HW" DOUBLE PRECISION,
  "PROD_diff_HW" DOUBLE PRECISION,
  "HW_share_diff" DOUBLE PRECISION,
  "WITHIN_effect_HW" DOUBLE PRECISION,
  "STATIC_shift_HW" DOUBLE PRECISION,
  "DYNAMIC_shift_HW" DOUBLE PRECISION,
  "STRUCT_shift_HW" DOUBLE PRECISION,
  "LETO" INTEGER,
  "Q" VARCHAR,
  UNIQUE (time, "SKD"));'

# Execute the table creation command
dbExecute(con, create_table_sql)



#### SURS RULC_cetrtletni_skd   ################################################
# table deletion command
drop_table_sql <- "DROP TABLE IF EXISTS \"SURS_RULC_VA_SKD_cetrtletni\";"
dbExecute(con, drop_table_sql)

# table creation
create_table_sql <- '
CREATE TABLE "SURS_RULC_VA_SKD_cetrtletni" (
  time DATE,
  "SKD" VARCHAR,
  "DEJAVNOST" VARCHAR,
  "Indicator" VARCHAR,
  "Descriptor" VARCHAR,
  "Value" DOUBLE PRECISION,
  "YOY" DOUBLE PRECISION,
  "Indeks2005" DOUBLE PRECISION,
  "Indeks2005_4cds" DOUBLE PRECISION,
  "Indeks2007" DOUBLE PRECISION,
  "Indeks2007_4cds" DOUBLE PRECISION,
  "Indeks2008" DOUBLE PRECISION,
  "Indeks2008_4cds" DOUBLE PRECISION,
  "Indeks2019" DOUBLE PRECISION,
  "Indeks2019_4cds" DOUBLE PRECISION,
  "LETO" INTEGER,
  "Q" VARCHAR,
  UNIQUE (time, "SKD", "Indicator"));'

# Execute the table creation command
dbExecute(con, create_table_sql)



#### SURS RULC_cetrtletni_TOTAL ################################################
# table deletion command
drop_table_sql <- "DROP TABLE IF EXISTS \"SURS_RULC_BDP_total_cetrtletni\";"
dbExecute(con, drop_table_sql)

# table creation
create_table_sql <- '
CREATE TABLE "SURS_RULC_BDP_total_cetrtletni" (
  time DATE,
  "season_adj" VARCHAR,
  "Indicator" VARCHAR,
  "Descriptor" VARCHAR,
  "Value" DOUBLE PRECISION,
  "YOY" DOUBLE PRECISION,
  "Indeks2005" DOUBLE PRECISION,
  "Indeks2005_4cds" DOUBLE PRECISION,
  "Indeks2007" DOUBLE PRECISION,
  "Indeks2007_4cds" DOUBLE PRECISION,
  "Indeks2008" DOUBLE PRECISION,
  "Indeks2008_4cds" DOUBLE PRECISION,
  "Indeks2019" DOUBLE PRECISION,
  "Indeks2019_4cds" DOUBLE PRECISION,
  "LETO" INTEGER,
  "Q" VARCHAR,
  UNIQUE (time, "season_adj", "Indicator"));'

# Execute the table creation command
dbExecute(con, create_table_sql)


#### SURS letna produktivnost_shift_share  ################################
# table deletion command
drop_table_sql <- "DROP TABLE IF EXISTS \"SURS_shift_share_letni\";"
dbExecute(con, drop_table_sql)

# table creation
create_table_sql <- '
CREATE TABLE "SURS_shift_share_letni" (
  "LETO" INTEGER,
  "SKD" VARCHAR,
  "DEJAVNOST" VARCHAR,
  aggr VARCHAR,
  "VA_nom" DOUBLE PRECISION,
  "VA_real" DOUBLE PRECISION,
  "EMP_HW" DOUBLE PRECISION,
  "EMP_PER" DOUBLE PRECISION,
  "EMP_share" DOUBLE PRECISION,
  "HW_share" DOUBLE PRECISION,
  "VA_share_nom" DOUBLE PRECISION,
  "VA_share_real" DOUBLE PRECISION,
  "PROD_real_EMP" DOUBLE PRECISION,
  "PROD_real_HW" DOUBLE PRECISION,
  "PROD_nom_EMP" DOUBLE PRECISION,
  "PROD_nom_HW" DOUBLE PRECISION,
  "PROD_yoy_EMP" DOUBLE PRECISION,
  "PROD_diff_EMP" DOUBLE PRECISION,
  "EMP_share_diff" DOUBLE PRECISION,
  "WITHIN_effect_EMP" DOUBLE PRECISION,
  "STATIC_shift_EMP" DOUBLE PRECISION,
  "DYNAMIC_shift_EMP" DOUBLE PRECISION,
  "STRUCT_shift_EMP" DOUBLE PRECISION,
  "PROD_yoy_HW" DOUBLE PRECISION,
  "PROD_diff_HW" DOUBLE PRECISION,
  "HW_share_diff" DOUBLE PRECISION,
  "WITHIN_effect_HW" DOUBLE PRECISION,
  "STATIC_shift_HW" DOUBLE PRECISION,
  "DYNAMIC_shift_HW" DOUBLE PRECISION,
  "STRUCT_shift_HW" DOUBLE PRECISION,
  UNIQUE ("LETO", "DEJAVNOST"));'

# Execute the table creation command
dbExecute(con, create_table_sql)


#### SURS_RULC_VA_SKD_letni  ################################################
# table deletion command
drop_table_sql <- "DROP TABLE IF EXISTS \"SURS_RULC_VA_SKD_letni\";"
dbExecute(con, drop_table_sql)

# table creation
create_table_sql <- '
CREATE TABLE "SURS_RULC_VA_SKD_letni" (
  "LETO" INTEGER,
  "SKD" VARCHAR,
  "DEJAVNOST" VARCHAR,
  aggr VARCHAR,
  "Indicator" VARCHAR,
  "Descriptor" VARCHAR,
  "Value" DOUBLE PRECISION,
  "YOY" DOUBLE PRECISION,
  "Indeks2005" DOUBLE PRECISION,
  "Indeks2007" DOUBLE PRECISION,
  "Indeks2008" DOUBLE PRECISION,
  "Indeks2019" DOUBLE PRECISION,
  UNIQUE ("LETO", "SKD", \"Indicator\"));'

# Execute the table creation command
dbExecute(con, create_table_sql)



#### SURS RULC_letni_TOTAL ################################################
# table deletion command
drop_table_sql <- "DROP TABLE IF EXISTS \"SURS_RULC_BDP_total_letni\";"
dbExecute(con, drop_table_sql)

# table creation
create_table_sql <- '
CREATE TABLE "SURS_RULC_BDP_total_letni" (
  "LETO" INTEGER,
  "Indicator" VARCHAR,
  "Descriptor" VARCHAR,
  "Value" DOUBLE PRECISION,
  "YOY" DOUBLE PRECISION,
  "Indeks2005" DOUBLE PRECISION,
  "Indeks2007" DOUBLE PRECISION,
  "Indeks2008" DOUBLE PRECISION,
  "Indeks2019" DOUBLE PRECISION,
  UNIQUE ("LETO", "Indicator"));'

# Execute the table creation command
dbExecute(con, create_table_sql)



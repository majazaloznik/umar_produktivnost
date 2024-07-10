################################################################################
#               Pomožne skripte, ki se kličejo iz drugih skript,               #
#                  tega ni treba nikoli samega zalaufat                        #
#            Razen, če se spremeni članstvo držav v kateri od skupin           #
#       Potem se zalaufa vse skupaj zključno z zadnjim zakomentiranim delom    #
################################################################################

geo_subset <- c("EU28", "EU15", "EA19", "EU27_2020", "BE",
                "BG", "CZ", "DK", "DE", "EE",
                "IE", "EL", "ES", "FR", "HR",
                "IT", "CY", "LV", "LT", "LU",
                "HU", "MT", "NL", "AT", "PL",
                "PT", "RO", "SI", "SK", "FI",
                "SE", "UK" )

geo_countries <- c("BE", "BG", "CZ", "DK", "DE",
                   "EE", "IE", "EL", "ES", "FR",
                   "HR", "IT", "CY", "LV", "LT",
                   "LU", "HU", "MT", "NL", "AT",
                   "PL", "PT", "RO", "SI", "SK",
                   "FI", "SE", "UK" )

country_code_EU13 <- c("BG", "CZ", "EE", "HR", "CY",
                       "LV", "LT", "HU", "MT", "PL",
                       "RO", "SI", "SK")

country_code_EU14 <- c("BE", "DK", "DE", "IE", "EL",
                       "ES", "FR", "IT", "LU", "NL",
                       "AT", "PT", "FI", "SE" )

country_code_EU15 <- c("BE", "DK", "DE", "IE", "EL",
                       "ES", "FR", "IT", "LU", "NL",
                       "AT", "PT", "FI", "SE", "UK" )

country_code_EU27_noIE <- c("BE", "BG", "CZ", "DK", "DE",
                            "EE", "EL", "ES", "FR", "HR",
                            "IT", "CY", "LV", "LT", "LU",
                            "HU", "MT", "NL", "AT", "PL",
                            "PT", "RO", "SI", "SK", "FI",
                            "SE" )

country_code_EU27 <- c("BE", "BG", "CZ", "DK", "DE",
                       "EE", "EL", "ES", "FR", "HR",
                       "IT", "CY", "LV", "LT", "LU",
                       "HU", "MT", "NL", "AT", "PL",
                       "PT", "RO", "SI", "SK", "FI",
                       "SE", "IE")

country_code_EA_noIE <- c("BE", "DE", "EE", "HR", "EL",
                          "ES", "FR", "IT", "CY", "LV",
                          "LT", "LU", "MT", "NL", "AT",
                          "PT", "SI", "SK", "FI")

country_code_EA20 <- c("BE", "DE", "EE", "HR", "EL",
                          "ES", "FR", "IT", "CY", "LV",
                          "LT", "LU", "MT", "NL", "AT",
                          "PT", "SI", "SK", "FI", "IE")

country_code_EA19 <- c("BE", "DE", "EE",  "EL",
                       "ES", "FR", "IT", "CY", "LV",
                       "LT", "LU", "MT", "NL", "AT",
                       "PT", "SI", "SK", "FI", "IE")

country_code_inovatorke <- c("DK", "SE", "FI", "NL")

country_code_V4 <- c("CZ", "HU", "SK", "PL")


# create lookup table
geo_lookup <- data.frame(geo = geo_countries)

# Add columns for each group with TRUE/FALSE values
geo_lookup$EU28 <- geo_lookup$geo %in% geo_countries
geo_lookup$EU15 <- geo_lookup$geo %in% country_code_EU15
geo_lookup$EA20 <- geo_lookup$geo %in% country_code_EA20
geo_lookup$EA19 <- geo_lookup$geo %in% country_code_EA19
geo_lookup$EU27 <- geo_lookup$geo %in% country_code_EU27
geo_lookup$EU27_2020 <- geo_lookup$geo %in% country_code_EU27
geo_lookup$EU13 <- geo_lookup$geo %in% country_code_EU13
geo_lookup$EU14 <- geo_lookup$geo %in% country_code_EU14
geo_lookup$EU27_noIE <- geo_lookup$geo %in% country_code_EU27_noIE
geo_lookup$EA_noIE <- geo_lookup$geo %in% country_code_EA_noIE
geo_lookup$inovatorke <- geo_lookup$geo %in% country_code_inovatorke
geo_lookup$V4 <- geo_lookup$geo %in% country_code_V4



# #### zapis na postgres bazo ####################################################
# # Database connection details
# con <- DBI::dbConnect(RPostgres::Postgres(),
#                       dbname = "produktivnost",
#                       host = "192.168.38.21",
#                       port = 5432,
#                       user = "postgres",
#                       password = Sys.getenv("PG_PG_PSW"))
# DBI::dbExecute(con, "set search_path to produktivnost")
# geo_lookup$EU27_2020 <- NULL
# dbExecute(con, "TRUNCATE TABLE \"geo_lookup\"")
# # Insert data into the PostgreSQL table
# dbWriteTable(con, "geo_lookup", geo_lookup, append = TRUE, row.names = FALSE)
#
# # Disconnect from the database
# dbDisconnect(con)

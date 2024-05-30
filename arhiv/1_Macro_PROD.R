###################################################
#         PRODUKTIVNOST IN BDP v PPS              #
#             Katarina, Avgust 2020               #
###################################################

rm(list=ls())
gc (verbose = T)


setwd("M:/Konkurencnost/R")


#####   Knjižnice   #####

library(eurostat)
library(dplyr)
library(data.table)




##### Download Podatkov #####

GDP_bulk <- get_eurostat("nama_10_gdp")
GDP_bulk <- GDP_bulk%>%rename(time = TIME_PERIOD) #preimenovanje oznake v "time" zaradi spremembe pri Eurostat poročanju

GDP <- droplevels(subset(GDP_bulk, geo %in% c("EU27_2020" , "EU15" , "EA19" , "EU27_2020","BE" , "BG" , "CZ" , "DK" , "DE" , "EE" , "IE" ,"EL" , "ES" ,
                                           "FR" , "HR" , "IT" , "CY" , "LV" , "LT" ,
                                           "LU" , "HU" , "MT" ,"NL" , "AT" , "FR" ,
                                           "HR" , "IT" , "CY" , "LV" , "LT" , "LU" ,
                                           "HU" , "MT" ,"NL" , "AT" ,"PL" , "PT" ,
                                           "RO" , "SI" , "SK" , "FI" , "SE" ) & time >= as.Date("1995-01-01") & time >= as.Date("1995-01-01") &  time <= as.Date("2023-01-01")&na_item == "B1GQ" & unit %in% c("CP_MPPS_EU27_2020" , "CLV10_MEUR")))%>%select(-freq)

EMP_bulk <- get_eurostat("nama_10_a10_e")
EMP_bulk <- EMP_bulk%>%rename(time = TIME_PERIOD)
EMP <- droplevels(subset(EMP_bulk, geo %in% c("EU28" , "EU15" , "EA19" , "EU27_2020","BE" , "BG" , "CZ" , "DK" , "DE" , "EE" , "IE" ,"EL" , "ES" ,
                                   "FR" , "HR" , "IT" , "CY" , "LV" , "LT" ,
                                   "LU" , "HU" , "MT" ,"NL" , "AT" , "FR" ,
                                   "HR" , "IT" , "CY" , "LV" , "LT" , "LU" ,
                                   "HU" , "MT" ,"NL" , "AT" ,"PL" , "PT" ,
                                   "RO" , "SI" , "SK" , "FI" , "SE" , "UK")   & time >= as.Date("1995-01-01")& time <= as.Date("2022-01-01") & na_item == "EMP_DC" & unit %in% c("THS_PER" , "THS_HW") & nace_r2 =="TOTAL"))%>%select(-freq)

EMP$nace_r2 <- NULL

POP_bulk <- get_eurostat("nama_10_pe")
POP_bulk <- POP_bulk%>%rename(time = TIME_PERIOD)

POP <- droplevels(subset(POP_bulk, geo %in% c("EU28" , "EU15" , "EA19" , "EU27_2020","BE" , "BG" , "CZ" , "DK" , "DE" , "EE" , "IE" ,"EL" , "ES" ,
                                   "FR" , "HR" , "IT" , "CY" , "LV" , "LT" ,
                                   "LU" , "HU" , "MT" ,"NL" , "AT" , "FR" ,
                                   "HR" , "IT" , "CY" , "LV" , "LT" , "LU" ,
                                   "HU" , "MT" ,"NL" , "AT" ,"PL" , "PT" , "RO" , "SI" , "SK" , "FI" , "SE" , "UK")   & time >= as.Date("1995-01-01") & time <= as.Date("2022-01-01") &na_item == "POP_NC" & unit == "THS_PER"))%>%select(-freq)



##### Definiranje dodatnih EU agregatov #####



#Country codes#

  country_code_EU13 <- c("BG" , "CZ" ,  "EE" , "HR" , "CY" , "LV" , "LT" ,"HU" , "MT" ,
                         "PL" , "RO" , "SI" , "SK")

  country_code_EU14 <- c("BE", "DK" , "DE" , "IE" ,"EL" , "ES" ,
                       "FR" , "IT" , "LU" ,"NL" , "AT" ,
                        "PT" , "FI" , "SE" )

  country_code_EU27_noIE <- c("BE","BG" , "CZ" , "DK" , "DE" , "EE" ,"EL" , "ES" ,
                              "FR" , "HR" , "IT" , "CY" , "LV" , "LT" , "LU" , "HU" , "MT" ,"NL" , "AT" ,
                              "PL" , "PT" , "RO" , "SI" , "SK" , "FI" , "SE" )

  country_code_EA_noIE <- c("BE", "DE" , "EE" , "IE" ,"EL" , "ES" ,
                            "FR" , "IT" , "CY" , "LV" , "LT" , "LU" , "MT" ,"NL" , "AT" ,
                            "PT" , "SI" , "SK" , "FI")
  country_code_inovatorke <- c( "BE" , "DK" , "SE" , "FI", "NL")

  country_code_CEEC <- c( "CZ" , "HU" , "SK" , "PL")

  df <- GDP # za BDP

  tmp_eu13 <- df%>%
    dplyr::group_by(na_item, time, unit)%>%
    dplyr::summarize(values = sum(values[geo%in%country_code_EU13]))%>%
    dplyr::mutate(geo = "EU13")

   tmp_eu14 <- df%>%
    dplyr::group_by(na_item, time, unit)%>%
    dplyr::summarize(values = sum(values[geo%in%country_code_EU14]))%>%
    dplyr::mutate(geo = "EU14")

  tmp_EU27noIE <- df%>%
    dplyr::group_by(na_item, time, unit)%>%
    dplyr::summarize(values = sum(values[geo%in%country_code_EU27_noIE]))%>%
    dplyr:: mutate(geo = "EU27noIE")

  tmp_EAnoIE <- df%>%
    dplyr::group_by(na_item, time, unit)%>%
    dplyr::summarize(values = sum(values[geo%in%country_code_EA_noIE]))%>%
    dplyr::mutate(geo = "EAnoIE")

  tmp_inovatorke <- df%>%
    dplyr::group_by(na_item, time, unit)%>%
    dplyr::summarize(values = sum(values[geo%in%country_code_inovatorke]))%>%
    dplyr::mutate(geo = "inovatorke")

  tmp_CEEC <- df%>%
    dplyr::group_by(na_item, time, unit)%>%
    dplyr::summarize(values = sum(values[geo%in%country_code_CEEC]))%>%
    dplyr::mutate(geo = "CEEC")

  tmp <- rbindlist(list(tmp_eu13, tmp_EU27noIE,tmp_EAnoIE, tmp_inovatorke, tmp_eu14, tmp_CEEC))
  data_tmp <<- rbind (df, tmp) #output je data_tmp
  GDP <- data_tmp

  df <- EMP # isto za zaposlenost

  tmp_eu13 <- df%>%
    dplyr::group_by(na_item, time, unit)%>%
    dplyr::summarize(values = sum(values[geo%in%country_code_EU13]))%>%
    dplyr::mutate(geo = "EU13")

  tmp_eu14 <- df%>%
    dplyr::group_by(na_item, time, unit)%>%
    dplyr::summarize(values = sum(values[geo%in%country_code_EU14]))%>%
    dplyr::mutate(geo = "EU14")

  tmp_EU27noIE <- df%>%
    dplyr::group_by(na_item, time, unit)%>%
    dplyr::summarize(values = sum(values[geo%in%country_code_EU27_noIE]))%>%
    dplyr:: mutate(geo = "EU27noIE")

  tmp_EAnoIE <- df%>%
    dplyr::group_by(na_item, time, unit)%>%
    dplyr::summarize(values = sum(values[geo%in%country_code_EA_noIE]))%>%
    dplyr::mutate(geo = "EAnoIE")

  tmp_inovatorke <- df%>%
    dplyr::group_by(na_item, time, unit)%>%
    dplyr::summarize(values = sum(values[geo%in%country_code_inovatorke]))%>%
    dplyr::mutate(geo = "inovatorke")

  tmp_CEEC <- df%>%
    dplyr::group_by(na_item, time, unit)%>%
    dplyr::summarize(values = sum(values[geo%in%country_code_CEEC]))%>%
    dplyr::mutate(geo = "CEEC")

  tmp <- rbindlist(list(tmp_eu13, tmp_eu14, tmp_EU27noIE,tmp_EAnoIE, tmp_inovatorke, tmp_CEEC))
  data_tmp <<- rbind (df, tmp) #output je data_tmp
  EMP <- data_tmp


  df <- POP # isto za prebivalstvo

  tmp_eu13 <- df%>%
    dplyr::group_by(na_item, time, unit)%>%
    dplyr::summarize(values = sum(values[geo%in%country_code_EU13]))%>%
    dplyr::mutate(geo = "EU13")
  tmp_eu14 <- df%>%
    dplyr::group_by(na_item, time, unit)%>%
    dplyr::summarize(values = sum(values[geo%in%country_code_EU14]))%>%
    dplyr::mutate(geo = "EU14")


  #tmp_eu15 <- df%>% group_by(na_item, time)%>% summarize(values = sum(values[geo%in%country_code_EU15]))%>%  mutate(geo = "EU15")
  tmp_EU27noIE <- df%>%
    dplyr::group_by(na_item, time, unit)%>%
    dplyr::summarize(values = sum(values[geo%in%country_code_EU27_noIE]))%>%
    dplyr:: mutate(geo = "EU27noIE")
  tmp_EAnoIE <- df%>%
    dplyr::group_by(na_item, time, unit)%>%
    dplyr::summarize(values = sum(values[geo%in%country_code_EA_noIE]))%>%
    dplyr::mutate(geo = "EAnoIE")
  tmp_inovatorke <- df%>%
    dplyr::group_by(na_item, time, unit)%>%
    dplyr::summarize(values = sum(values[geo%in%country_code_inovatorke]))%>%
    dplyr::mutate(geo = "inovatorke")
  tmp_CEEC <- df%>%
    dplyr::group_by(na_item, time, unit)%>%
    dplyr::summarize(values = sum(values[geo%in%country_code_CEEC]))%>%
    dplyr::mutate(geo = "CEEC")


  tmp <- rbindlist(list(tmp_eu13, tmp_eu14, tmp_EU27noIE,tmp_EAnoIE, tmp_inovatorke, tmp_CEEC))
  data_tmp <<- rbind (df, tmp) #output je data_tmp
  POP <- data_tmp

#####################################################


##### Združitev baz #####

tmp1 <- merge(GDP, EMP, all=TRUE)
tmp2 <- merge(tmp1, POP, all=TRUE)


##### Preračuni #####
tmp2 <- tmp2%>%filter(time<"2023-01-01") # filtrirala, ker so nazadnje nagajali ti nepopolni podatki?? Spremeniti s popolnejšo objavo leta 2023

data_macro <- tmp2 %>%
  reshape2::dcast(geo + time ~ unit + na_item, value.var ="values")%>%
  dplyr::group_by(time, geo)%>%
  dplyr::mutate(GDP_PC_PPS = CP_MPPS_EU27_2020_B1GQ / THS_PER_POP_NC)%>%
  dplyr::mutate(PROD_PPS = CP_MPPS_EU27_2020_B1GQ/THS_PER_EMP_DC)%>%
  dplyr::mutate(PROD_PPS_HW = CP_MPPS_EU27_2020_B1GQ/THS_HW_EMP_DC)%>%
  dplyr::mutate(PROD_real = CLV10_MEUR_B1GQ/THS_PER_EMP_DC)%>%
  dplyr::mutate(PROD_real_HW = CLV10_MEUR_B1GQ/THS_HW_EMP_DC)%>%
  dplyr::mutate(EMP_RATE = THS_PER_EMP_DC/THS_PER_POP_NC)%>%
  dplyr::mutate(HW_EMP = THS_HW_EMP_DC/THS_PER_EMP_DC)%>%
  ungroup()%>%
  dplyr::group_by(time)%>%
  dplyr::mutate(GDP_PC_PPS_EU27_100 = GDP_PC_PPS / GDP_PC_PPS[geo=="EU27_2020"]*100)%>%
  dplyr::mutate(PROD_EU27_100 = PROD_PPS / PROD_PPS[geo=="EU27_2020"]*100)%>%
  dplyr::mutate(PROD_EU27_100_HW = PROD_PPS_HW / PROD_PPS_HW[geo=="EU27_2020"]*100)%>%
  dplyr::mutate(EMP_RATE_EU27_100 = EMP_RATE / EMP_RATE[geo=="EU27_2020"]*100)%>%
  dplyr::mutate(GDP_PC_PPS_EU27_100 = GDP_PC_PPS / GDP_PC_PPS[geo=="EU27_2020"]*100)


rm(GDP, EMP, POP, tmp1, tmp2)

data_macro <- as.data.frame(data_macro)

#####  Access   #####
require(RODBC)
conn <- odbcConnectAccess2007(path.expand("O:/Users/KIvas/R/podatki_PROD_makro.accdb"))
try(sqlDrop(conn, "Produktivnost_makro", errors = FALSE), silent = TRUE)
sqlSave(conn,data_macro, tablename= "Produktivnost_makro", varTypes = c(time="datetime"))
close(conn)

save(data_macro, file = "Produktivnost_makro.Rdata")

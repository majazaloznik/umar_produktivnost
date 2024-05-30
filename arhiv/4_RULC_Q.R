#####################################################################################################
#                           Izracun ČETRTLETNIH RULC in NULC                                        #
#                               Zadnja sprememba: Oktober 2020                                         #
#####################################################################################################


##### Knjižnice #####

library(eurostat)
library(stringr)
library(data.table)
library(lubridate)
library(tidyr)
library(dplyr)
library(rvest)
library(httr)
library(xml2)
library(zoo)
library(RODBC)


##### 	Priprava delovnega okolja #####

rm(list=ls())
ls()

gc()
gc (verbose = T)

setwd("M:/Konkurencnost/R")



#################################################################################################
#                               RULC in NULC Q TOTALI                                           #
#################################################################################################


##### Download Podatkov #####

q_gdp_bulk <- get_eurostat("namq_10_gdp")
q_emp_bulk <- get_eurostat("namq_10_a10_e")


q_gdp_bulk <- q_gdp_bulk%>%rename(time = TIME_PERIOD) #zaradi Eurostat spremembe v oznaki
q_emp_bulk <- q_emp_bulk%>%rename(time = TIME_PERIOD)


# Filtriranje
q_gdp <- droplevels(subset(q_gdp_bulk, geo %in% c("EU27_2020" , "EU15" , "EA20" , "BE" , "BG" , "CZ" , "DK" , "DE" , "EE" , "IE" ,"EL" , "ES" ,
                                       "FR" , "HR" , "IT" , "CY" , "LV" , "LT" , "LU" , "HU" , "MT" ,"NL" , "AT" , "FR" ,
                                       "HR" , "IT" , "CY" , "LV" , "LT" , "LU" , "HU" , "MT" ,"NL" , "AT" ,"PL" , "PT" ,
                                       "RO" , "SI" , "SK" , "FI" , "SE" )
                & time >= as.Date("1995-01-01") & na_item %in% c("B1GQ" , "D1") &
                  unit %in% c("CP_MEUR" , "CLV10_MEUR") & s_adj %in% c("NSA", "SCA")))%>%select(-freq)


q_emp <- droplevels(subset(q_emp_bulk, geo %in% c("EU27_2020" , "EU15" , "EA20" , "BE" , "BG" , "CZ" , "DK" , "DE" , "EE" , "IE" ,"EL" , "ES" ,
                                       "FR" , "HR" , "IT" , "CY" , "LV" , "LT" , "LU" , "HU" , "MT" ,"NL" , "AT" , "FR" ,
                                       "HR" , "IT" , "CY" , "LV" , "LT" , "LU" , "HU" , "MT" ,"NL" , "AT" ,"PL" , "PT" ,
                                       "RO" , "SI" , "SK" , "FI" , "SE" )
                & time >= as.Date("1995-01-01") & na_item %in% c("EMP_DC" , "SAL_DC") &
                  unit %in% c("THS_PER" , "THS_HW") & s_adj %in% c("NSA", "SCA") & nace_r2 == "TOTAL"))%>%select(-freq)


q_emp$nace_r2 <- NULL



#####   Generiranje novih EU agregatov #####

country_code_EU13 <- c("BG" , "CZ" ,  "EE" , "HR" , "CY" , "LV" , "LT" ,"HU" , "MT" ,
                       "PL" , "RO" , "SI" , "SK")


country_code_EU27_noIE <- c("BE","BG" , "CZ" , "DK" , "DE" , "EE" ,"EL" , "ES" ,
                            "FR" , "HR" , "IT" , "CY" , "LV" , "LT" , "LU" , "HU" , "MT" ,"NL" , "AT" ,
                            "PL" , "PT" , "RO" , "SI" , "SK" , "FI" , "SE" )

country_code_EA_noIE <- c("BE", "DE" , "EE" , "IE" ,"EL" , "ES" ,
                          "FR" , "IT" , "CY" , "LV" , "LT" , "LU" , "MT" ,"NL" , "AT" ,
                          "PT" , "SI" , "SK" , "FI", "HR")

# Za GDP

df <- q_gdp


tmp_eu13 <- df%>%
  dplyr:: group_by(na_item, time, s_adj, unit)%>%
  dplyr::summarize(values = sum(values[geo%in%country_code_EU13]))%>%
  dplyr::mutate(geo = "EU13")

tmp_EU27noIE <- df%>%
  dplyr::group_by(na_item, time, s_adj, unit)%>%
  dplyr::summarize(values = sum(values[geo%in%country_code_EU27_noIE]))%>%
  dplyr::mutate(geo = "EU27noIE")
tmp_EAnoIE <- df%>%
  dplyr::group_by(na_item, time, s_adj, unit)%>%
  dplyr::summarize(values = sum(values[geo%in%country_code_EA_noIE]))%>%
  dplyr::mutate(geo = "EAnoIE")

tmp <- rbindlist(list(tmp_eu13, tmp_EU27noIE,tmp_EAnoIE))
data_q_gdp <<- rbind (tmp, q_gdp) #output je data_tmp


# Za EMP

df <- q_emp


tmp_eu13 <- df%>%
  dplyr::group_by(na_item, time, s_adj, unit)%>%
  dplyr::summarize(values = sum(values[geo%in%country_code_EU13]))%>%
  dplyr::mutate(geo = "EU13")

tmp_EU27noIE <- df%>%
  dplyr::group_by(na_item, time, s_adj, unit)%>%
  dplyr::summarize(values = sum(values[geo%in%country_code_EU27_noIE]))%>%
  dplyr::mutate(geo = "EU27noIE")

tmp_EAnoIE <- df%>%
  dplyr::group_by(na_item, time, s_adj, unit)%>%
  dplyr::summarize(values = sum(values[geo%in%country_code_EA_noIE]))%>%
  dplyr::mutate(geo = "EAnoIE")

tmp <- rbindlist(list(tmp_eu13, tmp_EU27noIE,tmp_EAnoIE))
data_q_emp <<- rbind (tmp, q_emp)


# Združitev baz
tmp_all <- merge(data_q_gdp, data_q_emp, all=TRUE)




##### Preračuni #####

q_data <-
  tmp_all %>%
  dcast(s_adj + geo + time ~ unit + na_item, value.var ="values")%>%
  dplyr::group_by(geo, s_adj)%>%
  dplyr::mutate(deflatorBDP = CP_MEUR_B1GQ / CLV10_MEUR_B1GQ)%>%
  dplyr::mutate(sredstva_zap_nom = CP_MEUR_D1 / THS_PER_SAL_DC)%>%
  dplyr::mutate(sredstva_zap_real = (CP_MEUR_D1 / deflatorBDP) / THS_PER_SAL_DC)%>%
  dplyr::mutate(prod_nom = CP_MEUR_B1GQ / THS_PER_EMP_DC)%>%
  dplyr::mutate(prod_real = CLV10_MEUR_B1GQ / THS_PER_EMP_DC)%>%
  dplyr::mutate(RULC = sredstva_zap_nom / prod_nom)%>%
  dplyr::mutate(prod_real_wh = CLV10_MEUR_B1GQ / THS_HW_EMP_DC)%>%
  dplyr::mutate(NULC = sredstva_zap_nom / prod_real)%>%
  gather("Indicator", "Value", 4:18)%>%
  arrange(time)%>%
  group_by(geo, Indicator, s_adj)%>%
  dplyr::mutate(YOY = ((Value /lag(Value,4))*100-100))%>%
  dplyr::mutate(Indeks2005=Value/(mean(Value[time >= "2005-01-01"& time <= "2005-10-01"]))*100)%>%
  dplyr:: mutate(Indeks2005_4cds=rollmean(Indeks2005,4, fill=NA, align = "right"))%>%
  dplyr::mutate(Indeks2007=Value/(mean(Value[time >= "2007-01-01"& time <= "2007-10-01"]))*100)%>%
  dplyr:: mutate(Indeks2007_4cds=rollmean(Indeks2007,4, fill=NA, align = "right"))%>%
  dplyr::mutate(Indeks2008=Value/(mean(Value[time >= "2008-01-01"& time <= "2008-10-01"]))*100)%>%
  dplyr:: mutate(Indeks2008_4cds=rollmean(Indeks2008,4, fill=NA, align = "right"))





##### Urejanje  #####

rm(data_q_emp, data_q_gdp, df, q_emp, q_gdp, tmp_all, tmp)

#dodati leto in mesec
q_data$year <-substr(q_data$time, 1, 4)
q_data$quarter <-substr(q_data$time, 6, 7)

#sprememba stevilk cetrtletij v drugo poimenovanje
q_data$quarter [q_data$quarter =="01" ] <- "Q1"
q_data$quarter [q_data$quarter =="04" ] <- "Q2"
q_data$quarter [q_data$quarter =="07" ] <- "Q3"
q_data$quarter [q_data$quarter =="10" ] <- "Q4"

q_data$Indicator <- replace(q_data$Indicator, q_data$Indicator == "CLV10_MEUR_B1GQ", "DV_sc_2010")
q_data$Indicator <- replace(q_data$Indicator, q_data$Indicator == "CP_MEUR_B1GQ", "DV_tc")
q_data$Indicator <- replace(q_data$Indicator, q_data$Indicator == "CP_MEUR_D1", "sredstva")
q_data$Indicator <- replace(q_data$Indicator, q_data$Indicator == "THS_HW_EMP_DC", "zaposlenost_hw")
q_data$Indicator <- replace(q_data$Indicator, q_data$Indicator== "THS_PER_EMP_DC", "zaposlenost")
q_data$Indicator <- replace(q_data$Indicator, q_data$Indicator== "THS_PER_SAL_DC", "zaposleni")


##### Zapis v Access  #####

q_data$time <- as.character(q_data$time)
q_data <- as.data.frame(q_data)

require(RODBC)
conn <- RODBC::odbcConnectAccess2007(path.expand("O:/Users/KIvas/R/RULC_q.accdb"))
try(RODBC::sqlDrop(conn, "Q_total", errors = FALSE), silent = TRUE)
sqlSave(conn,q_data, tablename= "Q_total")
close(conn)

#################################################################################################
#                                  RULC in NULC Q SKD                                           #
#################################################################################################


##### Download Podatkov #####

q_va_bulk <- get_eurostat("namq_10_a10")
q_emp_bulk <- get_eurostat("namq_10_a10_e")

q_va_bulk <- q_va_bulk%>%rename(time = TIME_PERIOD) #zaradi Eurostat spremembe v oznaki
q_emp_bulk <- q_emp_bulk%>%rename(time = TIME_PERIOD)

# Filtriranje
q_va <- droplevels(subset(q_va_bulk, geo %in% c("EU27_2020" , "EU15" , "EA20" , "BE" , "BG" , "CZ" , "DK" , "DE" , "EE" , "IE" ,"EL" , "ES" ,
                                       "FR" , "HR" , "IT" , "CY" , "LV" , "LT" , "LU" , "HU" , "MT" ,"NL" , "AT" , "FR" ,
                                       "HR" , "IT" , "CY" , "LV" , "LT" , "LU" , "HU" , "MT" ,"NL" , "AT" ,"PL" , "PT" ,
                                       "RO" , "SI" , "SK" , "FI" , "SE" )
                & time >= as.Date("1995-01-01") & na_item %in% c("B1G" , "D1") &
                  unit %in% c("CP_MEUR" , "CLV10_MEUR") & s_adj == "NSA"))%>%select(-freq)


q_emp <- droplevels(subset(q_emp_bulk, geo %in% c("EU27_2020" , "EU15" , "EA20" , "BE" , "BG" , "CZ" , "DK" , "DE" , "EE" , "IE" ,"EL" , "ES" ,
                                       "FR" , "HR" , "IT" , "CY" , "LV" , "LT" , "LU" , "HU" , "MT" ,"NL" , "AT" , "FR" ,
                                       "HR" , "IT" , "CY" , "LV" , "LT" , "LU" , "HU" , "MT" ,"NL" , "AT" ,"PL" , "PT" ,
                                       "RO" , "SI" , "SK" , "FI" , "SE" )
                & time >= as.Date("1995-01-01") & na_item %in% c("EMP_DC" , "SAL_DC") &
                  unit %in% c("THS_PER" , "THS_HW") & s_adj == "NSA"))%>%select(-freq)

q_va$s_adj <- NULL
q_emp$s_adj <- NULL

q_va$geo <- as.character(q_va$geo)
q_emp$geo <- as.character(q_emp$geo)


#####   Generiranje novih EU agregatov #####

# Za Value added

df <- q_va


tmp_eu13 <- df%>%
  dplyr::group_by(na_item, time, nace_r2, unit)%>%
  dplyr::summarize(values = sum(values[geo%in%country_code_EU13]))%>%
  dplyr::mutate(geo = "EU13")


tmp_EU27noIE <- df%>%
  dplyr::group_by(na_item, time, nace_r2, unit)%>%
  dplyr::summarize(values = sum(values[geo%in%country_code_EU27_noIE]))%>%
  dplyr::mutate(geo = "EU27noIE")
tmp_EAnoIE <- df%>%
  dplyr::group_by(na_item, time, nace_r2, unit)%>%
  dplyr::summarize(values = sum(values[geo%in%country_code_EA_noIE]))%>%
  dplyr::mutate(geo = "EAnoIE")

tmp <- rbindlist(list(tmp_eu13, tmp_EU27noIE,tmp_EAnoIE))
data_q_va <<- rbind (tmp, q_va)


# Za EMP

df <- q_emp


tmp_eu13 <- df%>%
  dplyr::group_by(na_item, time, nace_r2, unit)%>%
  dplyr::summarize(values = sum(values[geo%in%country_code_EU13]))%>%
  dplyr::mutate(geo = "EU13")

tmp_EU27noIE <- df%>%
  dplyr::group_by(na_item, time, nace_r2, unit)%>%
  dplyr::summarize(values = sum(values[geo%in%country_code_EU27_noIE]))%>%
  dplyr::mutate(geo = "EU27noIE")
tmp_EAnoIE <- df%>%
  dplyr::group_by(na_item, time, nace_r2, unit)%>%
  dplyr::summarize(values = sum(values[geo%in%country_code_EA_noIE]))%>%
  dplyr::mutate(geo = "EAnoIE")

tmp <- rbindlist(list(tmp_eu13, tmp_EU27noIE,tmp_EAnoIE))
data_q_emp <<- rbind (tmp, q_emp)

# Združitev baz
tmp_all <- merge(data_q_va, data_q_emp, all=TRUE)

#####   Generiranje novih SEKTOR agregatov #####

MENJALNI_sektor <- c("A" , "B-E" ,  "G-I" , "J" )
NEMENJALNI_sektor <- c("F" , "K" ,  "L" , "M_N", "O-Q", "R-U" )
POSLOVNI_sektor <- c("B-E" , "F" , "G-I", "J" , "K", "M_N" )
NEPOSLOVNI_sektor <- c("A" , "L" , "O-Q", "R-U" )



df <- tmp_all

tmp_MENJALNI_sektor <- df%>%
  dplyr::group_by(na_item, time, geo, unit)%>%
  dplyr::summarize(values = sum(values[nace_r2%in%MENJALNI_sektor]))%>%
  dplyr::mutate(nace_r2 = "MENJALNI")
tmp_NEMENJALNI_sektor <- df%>%
  dplyr::group_by(na_item, time, geo, unit)%>%
  dplyr::summarize(values = sum(values[nace_r2%in%NEMENJALNI_sektor]))%>%
  dplyr::mutate(nace_r2 = "NEMENJALNI")
tmp_POSLOVNI_sektor <- df%>%
  dplyr::group_by(na_item, time, geo, unit)%>%
  dplyr::summarize(values = sum(values[nace_r2%in%POSLOVNI_sektor]))%>%
  dplyr::mutate(nace_r2 = "POSLOVNI")
tmp_NEPOSLOVNI_sektor <- df%>%
  dplyr::group_by(na_item, time, geo, unit)%>%
  dplyr::summarize(values = sum(values[nace_r2%in%NEPOSLOVNI_sektor]))%>%
  dplyr::mutate(nace_r2 = "NEPOSLOVNI")

tmp2 <- rbindlist(list(tmp_MENJALNI_sektor, tmp_NEMENJALNI_sektor, tmp_POSLOVNI_sektor, tmp_NEPOSLOVNI_sektor))
data_q_va2 <<- rbind (tmp2, tmp_all)




##### Preračuni #####

q_data_nace <-
  data_q_va2 %>%
  dcast(nace_r2 + geo + time ~ unit + na_item, value.var ="values")%>%
  dplyr::group_by(geo, nace_r2)%>%
  dplyr::mutate(deflatorVA = CP_MEUR_B1G / CLV10_MEUR_B1G)%>%
  dplyr::mutate(sredstva_zap_nom = CP_MEUR_D1 / THS_PER_SAL_DC)%>%
  dplyr::mutate(sredstva_zap_real = (CP_MEUR_D1 / deflatorVA) / THS_PER_SAL_DC)%>%
  dplyr::mutate(prod_nom = CP_MEUR_B1G / THS_PER_EMP_DC)%>%
  dplyr::mutate(prod_real = CLV10_MEUR_B1G / THS_PER_EMP_DC)%>%
  dplyr::mutate(RULC = sredstva_zap_nom / prod_nom)%>%
  dplyr::mutate(prod_real_wh = CLV10_MEUR_B1G / THS_HW_EMP_DC)%>%
  dplyr::mutate(NULC = sredstva_zap_nom / prod_real)%>%
  ungroup()%>%
  gather("Indicator", "Value", 4:18)%>%
  arrange(time)%>%
  group_by(geo, Indicator, nace_r2)%>%
  dplyr::mutate(YOY = ((Value /lag(Value,4))*100-100))%>%
  dplyr::mutate(Indeks2005=Value/(mean(Value[time >= "2005-01-01"& time <= "2005-10-01"]))*100)%>%
  dplyr:: mutate(Indeks2005_4cds=rollmean(Indeks2005,4, fill=NA, align = "right"))%>%
  dplyr::mutate(Indeks2007=Value/(mean(Value[time >= "2007-01-01"& time <= "2007-10-01"]))*100)%>%
  dplyr:: mutate(Indeks2007_4cds=rollmean(Indeks2007,4, fill=NA, align = "right"))%>%
  dplyr::mutate(Indeks2008=Value/(mean(Value[time >= "2008-01-01"& time <= "2008-10-01"]))*100)%>%
  dplyr:: mutate(Indeks2008_4cds=rollmean(Indeks2008,4, fill=NA, align = "right"))




##### Urejanje  #####

rm(data_q_emp, data_q_va, df, q_emp, q_va, tmp_all, tmp)

#dodati leto in mesec
q_data_nace$year <-substr(q_data_nace$time, 1, 4)
q_data_nace$quarter <-substr(q_data_nace$time, 6, 7)

#sprememba stevilk cetrtletij v drugo poimenovanje
q_data_nace$quarter [q_data_nace$quarter =="01" ] <- "Q1"
q_data_nace$quarter [q_data_nace$quarter =="04" ] <- "Q2"
q_data_nace$quarter [q_data_nace$quarter =="07" ] <- "Q3"
q_data_nace$quarter [q_data_nace$quarter =="10" ] <- "Q4"



q_data_nace$Indicator <- replace(q_data_nace$Indicator, q_data_nace$Indicator == "CLV10_MEUR_B1GQ", "DV_sc_2010")
q_data_nace$Indicator <- replace(q_data_nace$Indicator, q_data_nace$Indicator== "CP_MEUR_B1GQ", "DV_tc")
q_data_nace$Indicator <- replace(q_data_nace$Indicator, q_data_nace$Indicator == "CP_MEUR_D1", "sredstva")
q_data_nace$Indicator <- replace(q_data_nace$Indicator, q_data_nace$Indicator == "THS_HW_EMP_DC", "zaposlenost_hw")
q_data_nace$Indicator <- replace(q_data_nace$Indicator, q_data_nace$Indicator== "THS_PER_EMP_DC", "zaposlenost")
q_data_nace$Indicator <- replace(q_data_nace$Indicator, q_data_nace$Indicator== "THS_PER_SAL_DC", "zaposleni")

##### Zapis v RData  #####
save(q_data, file = "RULC_Total_Q.Rdata")
save(q_data_nace, file = "RULC_NACE_Q.Rdata")


##### Zapis v Access  #####

q_data_nace$time <- as.character(q_data_nace$time)
q_data_nace <- as.data.frame(q_data_nace)

require(RODBC)
conn <- odbcConnectAccess2007("O:/Users/KIvas/R/RULC_q.accdb")
try(sqlDrop(conn, "Q_NACE", errors = FALSE), silent = TRUE)
sqlSave(conn,q_data_nace, tablename= "Q_NACE")
close(conn)


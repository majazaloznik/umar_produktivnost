###################################################
#  PRODUCTIVITY REPORT SHIFT SHARE & STRUKTURA    #
#             Katarina, Junij 2019                #
###################################################

rm(list=ls())
gc (verbose = T)


setwd("M:/Konkurencnost/R")


#####   Knjižnice   #####

library(eurostat)
library(tidyr)
library(dplyr)
library(RODBC)



########################################################################################
#                         Varianta z ZAPOSLENIMI                                       #
########################################################################################


#####    Download podatkov  #####


va_nace_det_bulk <- get_eurostat("nama_10_a64") 
va_nace_det_bulk <- va_nace_det_bulk%>%rename(time = TIME_PERIOD) #zaradi Eurostat spremembe v oznaki

va_nace_det <- droplevels(subset(va_nace_det_bulk, geo %in% c("EU27_2020" , "EU15" , "EA19" , "BE" , "BG" , "CZ" , "DK" , "DE" , "EE" , "IE" ,"EL" , "ES" ,
                                                   "FR" , "HR" , "IT" , "CY" , "LV" , "LT" , 
                                                   "LU" , "HU" , "MT" ,"NL" , "AT" , "FR" , 
                                                   "HR" , "IT" , "CY" , "LV" , "LT" , "LU" , 
                                                   "HU" , "MT" ,"NL" , "AT" ,"PL" , "PT" , 
                                                   "RO" , "SI" , "SK" , "FI" , "SE" ) & na_item == "B1G" & unit == "CLV10_MEUR" & time >= as.Date("1995-01-01")))%>%select(-freq) 

EMP_nace_det_bulk <- get_eurostat("nama_10_a64_e")
EMP_nace_det_bulk <- EMP_nace_det_bulk%>%rename(time = TIME_PERIOD)
EMP_nace_det <- droplevels(subset(EMP_nace_det_bulk, geo %in% c("EU27_2020" , "EU15" , "EA19" ,  "BE" , "BG" , "CZ" , "DK" , "DE" , "EE" , "IE" ,"EL" , "ES" ,
                                                     "FR" , "HR" , "IT" , "CY" , "LV" , "LT" , 
                                                     "LU" , "HU" , "MT" ,"NL" , "AT" , "FR" , 
                                                     "HR" , "IT" , "CY" , "LV" , "LT" , "LU" , 
                                                     "HU" , "MT" ,"NL" , "AT" ,"PL" , "PT",
                                                     "RO" , "SI" , "SK" , "FI" , "SE" ) & time >= as.Date("1995-01-01") & na_item == "EMP_DC" & unit == "THS_PER"))%>%select(-freq) 




#####   Priprava in združitev baz   ##### 
df_shift_share_prod <- merge(EMP_nace_det, va_nace_det, all=TRUE)
df_shift_share_prod$na_item <- NULL 





#####   Preračuni   ######

data_shift_share_prod <- df_shift_share_prod%>%
  pivot_wider(names_from=unit, values_from=values)%>%
  dplyr::group_by(geo, time)%>%
  dplyr::mutate(EMP_share = THS_PER / THS_PER[nace_r2=="TOTAL"]*100)%>%
  ungroup ()%>%
  dplyr::group_by(nace_r2,geo, time)%>%
  dplyr::mutate(PROD_real = CLV10_MEUR/THS_PER)%>% ungroup ()%>%
  dplyr::arrange(time)%>%
  dplyr::group_by(geo, nace_r2)%>%
  dplyr::mutate(PROD_diff = (PROD_real - lag(PROD_real))) %>%
  dplyr::mutate(EMP_share_diff = (EMP_share - lag(EMP_share))) %>%
  dplyr::mutate(WITHIN_effect = (PROD_diff*lag(EMP_share))) %>%
  dplyr::mutate(STATIC_shift = (EMP_share_diff*lag(PROD_real))) %>%
  dplyr::mutate(DYNAMIC_SHIFT = (EMP_share_diff*PROD_diff)) %>%
  ungroup ()



rm(df_shift_share_prod)



########################################################################################
#                         Varianta z DELOVNIMI URAMI                                   #
########################################################################################

HOURS_nace_det <- droplevels(subset(EMP_nace_det_bulk, geo %in% c("EU27_2020" , "EU15" , "EA19" ,  "BE" , "BG" , "CZ" , "DK" , "DE" , "EE" , "IE" ,"EL" , "ES" ,
                                                       "FR" , "HR" , "IT" , "CY" , "LV" , "LT" , 
                                                       "LU" , "HU" , "MT" ,"NL" , "AT" , "FR" , 
                                                       "HR" , "IT" , "CY" , "LV" , "LT" , "LU" , 
                                                       "HU" , "MT" ,"NL" , "AT" ,"PL" , "PT" , 
                                                       "RO" , "SI" , "SK" , "FI" , "SE" ) & time >= as.Date("1995-01-01") & na_item == "EMP_DC" & unit == "THS_HW"))%>%select(-freq) 

#####   Priprava in zdru?itev baz   ##### 
df_shift_share_prod_hw <- merge(HOURS_nace_det, va_nace_det, all=TRUE)
df_shift_share_prod_hw$na_item <- NULL 







#####   Preračuni   ######

HW_data_shift_share_prod <- df_shift_share_prod_hw%>%
  pivot_wider(names_from=unit, values_from=values)%>%
  dplyr::group_by(geo, time)%>%
  dplyr::mutate(HW_share = THS_HW/THS_HW[nace_r2=="TOTAL"]*100)%>%
  ungroup ()%>%
  dplyr::group_by(nace_r2,geo)%>%
  dplyr::mutate(PROD_real_HW = CLV10_MEUR/THS_HW)%>%
  dplyr::mutate(PROD_diff_HW = (PROD_real_HW - lag(PROD_real_HW)))%>%
  dplyr::mutate(HW_share_diff = (HW_share - lag(HW_share))) %>%
  dplyr::mutate(WITHIN_effect = (PROD_diff_HW*lag(HW_share))) %>%
  dplyr::mutate(STATIC_shift = (HW_share_diff*lag(PROD_real_HW))) %>%
  dplyr::mutate(DYNAMIC_SHIFT = (HW_share_diff*PROD_diff_HW)) %>%
  ungroup ()



rm(df_shift_share_prod_hw)




########################################################################################
#                         Varianta z AGREGATI                                          #
########################################################################################

#####   Generiranje novih EU agregatov #####


country_code_EU13 <- c("BG" , "CZ" ,  "EE" , "HR" , "CY" , "LV" , "LT" ,"HU" , "MT" ,
                       "PL" , "RO" , "SI" , "SK")


country_code_EU27_noIE <- c("BE","BG" , "CZ" , "DK" , "DE" , "EE" ,"EL" , "ES" ,
                            "FR" , "HR" , "IT" , "CY" , "LV" , "LT" , "LU" , "HU" , "MT" ,"NL" , "AT" ,
                            "PL" , "PT" , "RO" , "SI" , "SK" , "FI" , "SE" )

country_code_EA_noIE <- c("BE", "DE" , "EE" , "IE" ,"EL" , "ES" ,
                          "FR" , "IT" , "CY" , "LV" , "LT" , "LU" , "MT" ,"NL" , "AT" ,
                          "PT" , "SI" , "SK" , "FI") 
country_code_CEE4 <- c("CZ" , "HU" , "PL" , "SK")

country_code_inovatorke <- c( "LU" , "NL" , "DK" , "SE" , "FI") 



# Za Value added
nva_nace_det <- subset(va_nace_det_bulk, geo %in% c("EU27_2020" , "EU15" , "EA19" , "BE" , "BG" , "CZ" , "DK" , "DE" , "EE" , "IE" ,"EL" , "ES" ,
                                                    "FR" , "HR" , "IT" , "CY" , "LV" , "LT" , 
                                                    "LU" , "HU" , "MT" ,"NL" , "AT" , "FR" , 
                                                    "HR" , "IT" , "CY" , "LV" , "LT" , "LU" , 
                                                    "HU" , "MT" ,"NL" , "AT" ,"PL" , "PT" , 
                                                    "RO" , "SI" , "SK" , "FI" , "SE" ) & na_item == "B1G" & unit %in% c("CP_MEUR", "CLV10_MEUR")  & time >= as.Date("1995-01-01"))%>%droplevels()%>%select(-freq) 

nEMP_nace_det <- subset(EMP_nace_det_bulk, geo %in% c("EU27_2020" , "EU15" , "EA19" ,  "BE" , "BG" , "CZ" , "DK" , "DE" , "EE" , "IE" ,"EL" , "ES" ,
                                                     "FR" , "HR" , "IT" , "CY" , "LV" , "LT" , 
                                                     "LU" , "HU" , "MT" ,"NL" , "AT" , "FR" , 
                                                     "HR" , "IT" , "CY" , "LV" , "LT" , "LU" , 
                                                     "HU" , "MT" ,"NL" , "AT" ,"PL" , "PT" , 
                                                     "RO" , "SI" , "SK" , "FI" , "SE" ) & time >= as.Date("1995-01-01") & na_item == "EMP_DC" & unit %in% c("THS_PER", "THS_HW"))%>%droplevels()%>%select(-freq) 


#####   Priprava in združitev baz   ##### 
df <- merge(nEMP_nace_det, nva_nace_det, all=TRUE)
df$na_item <- NULL 

tmp_eu13 <- df%>% 
  dplyr::group_by(time, nace_r2, unit)%>% 
  dplyr::summarize(values = sum(values[geo%in%country_code_EU13]))%>%  mutate(geo = "EU13")

tmp_EU27noIE <- df%>% 
  dplyr::group_by(time, nace_r2, unit)%>% 
  dplyr::summarize(values = sum(values[geo%in%country_code_EU27_noIE]))%>%  mutate(geo = "EU28noIE")

tmp_EAnoIE <- df%>% 
  dplyr::group_by(time, nace_r2, unit)%>% 
  dplyr::summarize(values = sum(values[geo%in%country_code_EA_noIE]))%>%  mutate(geo = "EAnoIE")

tmp_CEE4 <- df%>% 
  dplyr::group_by(time, nace_r2, unit)%>% 
  dplyr::summarize(values = sum(values[geo%in%country_code_CEE4]))%>%  mutate(geo = "CEE4")

tmp_inovatorke <- df%>% 
  dplyr::group_by(time, nace_r2, unit)%>% 
  dplyr::summarize(values = sum(values[geo%in%country_code_inovatorke]))%>%  
  dplyr::mutate(geo = "inovatorke")

tmp <- bind_rows(list(tmp_eu13, tmp_EU27noIE,tmp_EAnoIE, tmp_CEE4, tmp_inovatorke))
data_c_agg <<- bind_rows (tmp, df) 


#####   Generiranje novih SEKTOR agregatov #####

MENJALNI_sektor <- c("A" , "B-E" ,  "G-I" , "J" )
NEMENJALNI_sektor <- c("F" , "K" ,  "L" , "M_N", "O-Q", "R-U" )
POSLOVNI_sektor <- c("B-E" , "F" , "G-I", "J" , "K", "M_N" )
NEPOSLOVNI_sektor <- c("A" , "L" , "O-Q", "R-U" )
HIGH_TECH_MANUF <- c("C21" , "C26")
MED_HIGH_TECH_MANUF <- c("C20" , "C27", "C28", "C29_C30")
MED_LOW_TECH_MANUF <- c("C19", "C22_C23", "C24_C25")
LOW_TECH_MANUF <- c("C10-C12",  "C13-C15", "C16-C18", "C31-C32")
KNOWLEDGE_MKT_SERV <- c("J",  "M")
REST_MKT_SERV <-c("G", "H",  "I", "K", "N")

df <- data_c_agg

tmp_MENJALNI_sektor <- df%>% 
  dplyr::group_by(time, geo, unit)%>% 
  dplyr::summarize(values = sum(values[nace_r2%in%MENJALNI_sektor]))%>%  mutate(nace_r2 = "MENJALNI")
tmp_NEMENJALNI_sektor <- df%>% 
  dplyr::group_by(time, geo, unit)%>% 
  dplyr::summarize(values = sum(values[nace_r2%in%NEMENJALNI_sektor]))%>%  mutate(nace_r2 = "NEMENJALNI")
tmp_POSLOVNI_sektor <- df%>% 
  dplyr::group_by( time, geo, unit)%>% 
  dplyr::summarize(values = sum(values[nace_r2%in%POSLOVNI_sektor]))%>%  mutate(nace_r2 = "POSLOVNI")
tmp_NEPOSLOVNI_sektor <- df%>% 
  dplyr::group_by(time, geo, unit)%>% 
  dplyr::summarize(values = sum(values[nace_r2%in%NEPOSLOVNI_sektor]))%>%  mutate(nace_r2 = "NEPOSLOVNI")

tmp_HIGH_TECH_MANUF <- df%>% 
  dplyr::group_by( time, geo, unit)%>% 
  dplyr::summarize(values = sum(values[nace_r2%in%HIGH_TECH_MANUF]))%>%  mutate(nace_r2 = "HIGH_TECH")

tmp_MED_HIGH_TECH_MANUF <- df%>% 
  dplyr::group_by( time, geo, unit)%>% 
  dplyr::summarize(values = sum(values[nace_r2%in%MED_HIGH_TECH_MANUF]))%>%  mutate(nace_r2 = "MED_HIGH_TECH")

tmp_MED_LOW_TECH_MANUF <- df%>% 
  dplyr::group_by( time, geo, unit)%>% 
  dplyr::summarize(values = sum(values[nace_r2%in%MED_LOW_TECH_MANUF]))%>%  mutate(nace_r2 = "MED_LOW_TECH")

tmp_LOW_TECH_MANUF <- df%>% 
  dplyr::group_by( time, geo, unit)%>% 
  dplyr::summarize(values = sum(values[nace_r2%in%LOW_TECH_MANUF]))%>%  mutate(nace_r2 = "LOW_TECH")

tmp_KNOWLEDGE_MKT_SERV <- df%>% 
  dplyr::group_by(time, geo, unit)%>% 
  dplyr::summarize(values = sum(values[nace_r2%in%KNOWLEDGE_MKT_SERV]))%>%  mutate(nace_r2 = "KNOWLEDGE_MKT_S")

tmp_REST_MKT_SERV <- df%>% 
  dplyr::group_by(time, geo, unit)%>% 
  dplyr::summarize(values = sum(values[nace_r2%in%REST_MKT_SERV]))%>%  mutate(nace_r2 = "REST_MKT_S")

tmp2 <- bind_rows(tmp_MENJALNI_sektor, tmp_NEMENJALNI_sektor, tmp_POSLOVNI_sektor, tmp_NEPOSLOVNI_sektor, tmp_HIGH_TECH_MANUF, tmp_MED_HIGH_TECH_MANUF, tmp_MED_LOW_TECH_MANUF, tmp_LOW_TECH_MANUF, tmp_KNOWLEDGE_MKT_SERV, tmp_REST_MKT_SERV)
data_all <<- bind_rows (tmp2, data_c_agg) 


#####   Preračuni   ######

data_aggregates <- data_all%>%
  pivot_wider(names_from=unit, values_from=values)%>%
  dplyr::group_by(geo, time)%>%
  mutate(EMP_share = THS_PER / THS_PER[nace_r2=="TOTAL"]*100)%>%
  mutate(HW_share = THS_HW / THS_HW[nace_r2=="TOTAL"]*100)%>%
  mutate(VA_share = CP_MEUR / CP_MEUR[nace_r2=="TOTAL"]*100)%>%
  ungroup ()%>%
  dplyr::group_by(nace_r2,geo, time)%>%
  mutate(PROD_real = CLV10_MEUR/THS_PER)%>%
  mutate(PROD_nom = (CP_MEUR/THS_PER)) %>%
  mutate(PROD_real_HW = CLV10_MEUR/THS_HW)%>%
  mutate(PROD_nom_HW = (CP_MEUR/THS_HW)) %>%
  ungroup ()

save(data_shift_share_prod, file = "data_shift_share_prod.Rdata")
save(HW_data_shift_share_prod, file = "HW_data_shift_share_prod.Rdata")
save(data_aggregates, file = "data_aggregates.Rdata")


#####  Zapis v Access   #####
data_shift_share_prod$time <- format(as.Date(data_shift_share_prod$time, format="%Y-%m-%d"), "%Y")
HW_data_shift_share_prod$time <- format(as.Date(HW_data_shift_share_prod$time, format="%Y-%m-%d"), "%Y")
data_aggregates$time <- format(as.Date(data_aggregates$time, format="%Y-%m-%d"), "%Y")


require(RODBC)
conn <- odbcConnectAccess2007(path.expand("O:/Users/KIvas/R/Shift_share(PROD).accdb")) 
try(sqlDrop(conn, "Shift_share", errors = FALSE), silent = TRUE)
sqlSave(conn,data_shift_share_prod, tablename= "Shift_share")
try(sqlDrop(conn, "Shift_share_HW", errors = FALSE), silent = TRUE)
sqlSave(conn,HW_data_shift_share_prod, tablename= "Shift_share_HW")
try(sqlDrop(conn, "Aggregates", errors = FALSE), silent = TRUE)
sqlSave(conn,data_aggregates, tablename= "Aggregates")
close(conn) 


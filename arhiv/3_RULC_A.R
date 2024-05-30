#####################################################################################################
#                           Izračun LETNIH RULC in NULC                                             #
#                          Zadnja sprememba: September 2021                                         #
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

setwd("O:/Users/KIvas/R")

# set_config(use_proxy(url = "http://proxy.gov.si", port = 80)) 

#################################################################################################
#                               RULC in NULC TOTALI                                           #
#################################################################################################


##### Download Podatkov #####

gdp_bulk <- get_eurostat("nama_10_gdp") 
emp_bulk <- get_eurostat("nama_10_a10_e") 
gdp_bulk <- gdp_bulk%>%rename(time = TIME_PERIOD) #zaradi Eurostat spremembe v oznaki
emp_bulk <- emp_bulk%>%rename(time = TIME_PERIOD)

# Filtriranje
gdp <- droplevels(subset(gdp_bulk, geo %in% c("EU27_2020" , "EU15" , "EA19" , "BE" , "BG" , "CZ" , "DK" , "DE" , "EE" , "IE" ,"EL" , "ES" ,
                                       "FR" , "HR" , "IT" , "CY" , "LV" , "LT" , "LU" , "HU" , "MT" ,"NL" , "AT" , "FR" , 
                                       "HR" , "IT" , "CY" , "LV" , "LT" , "LU" , "HU" , "MT" ,"NL" , "AT" ,"PL" , "PT" , 
                                       "RO" , "SI" , "SK" , "FI" , "SE" ) 
                  & time >= as.Date("1995-01-01")& na_item %in% c("B1GQ" , "D1", "D11", "D12") & unit%in% c("CP_MEUR" , "CLV10_MEUR")))%>%select(-freq)
                   
emp <- droplevels(subset(emp_bulk, geo %in% c("EU27_2020" , "EU15" , "EA19" , "BE" , "BG" , "CZ" , "DK" , "DE" , "EE" , "IE" ,"EL" , "ES" ,
                                       "FR" , "HR" , "IT" , "CY" , "LV" , "LT" , "LU" , "HU" , "MT" ,"NL" , "AT" , "FR" , 
                                       "HR" , "IT" , "CY" , "LV" , "LT" , "LU" , "HU" , "MT" ,"NL" , "AT" ,"PL" , "PT" , 
                                       "RO" , "SI" , "SK" , "FI" , "SE" ) 
                   & time >= as.Date("1995-01-01") & na_item %in% c("EMP_DC" , "SAL_DC", "SELF_DC") & 
                  unit %in% c("THS_PER" , "THS_HW") & nace_r2 == "TOTAL"))%>%select(-freq)


emp$nace_r2 <- NULL



#####   Generiranje novih EU agregatov #####

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

# Za GDP

df <- gdp 



tmp_eu13 <- df%>% 
  dplyr:: group_by(na_item, time, unit)%>% 
  dplyr::summarize(values = sum(values[geo%in%country_code_EU13]))%>%  
  dplyr::mutate(geo = "EU13")

tmp_eu14 <- df%>% 
  dplyr:: group_by(na_item, time, unit)%>% 
  dplyr::summarize(values = sum(values[geo%in%country_code_EU14]))%>%  
  dplyr::mutate(geo = "EU14")

tmp_EU27noIE <- df%>% 
  dplyr::group_by(na_item, time, unit)%>% 
  dplyr::summarize(values = sum(values[geo%in%country_code_EU27_noIE]))%>%  
  dplyr::mutate(geo = "EU27noIE")

tmp_EAnoIE <- df%>% 
  dplyr::group_by(na_item, time, unit)%>% 
  dplyr::summarize(values = sum(values[geo%in%country_code_EA_noIE]))%>% 
  dplyr::mutate(geo = "EAnoIE")

tmp_inovatorke <- df%>% 
  dplyr::group_by(na_item, time, unit)%>% 
  dplyr::summarize(values = sum(values[geo%in%country_code_inovatorke]))%>% 
  dplyr::mutate(geo = "Inovatorke")

tmp_CEEC <- df%>% 
  dplyr::group_by(na_item, time, unit)%>% 
  dplyr::summarize(values = sum(values[geo%in%country_code_CEEC]))%>% 
  dplyr::mutate(geo = "CEEC")


data_gdp <- bind_rows(tmp_eu13, tmp_EU27noIE,tmp_EAnoIE, tmp_eu14, tmp_inovatorke, tmp_CEEC, gdp)

# Za EMP

df <- emp 


tmp_eu13 <- df%>% 
  dplyr::group_by(na_item, time, unit)%>% 
  dplyr::summarize(values = sum(values[geo%in%country_code_EU13]))%>%  
  dplyr::mutate(geo = "EU13")

tmp_eu14 <- df%>% 
  dplyr:: group_by(na_item, time, unit)%>% 
  dplyr::summarize(values = sum(values[geo%in%country_code_EU14]))%>%  
  dplyr::mutate(geo = "EU14")

tmp_EU27noIE <- df%>% 
  dplyr::group_by(na_item, time, unit)%>% 
  dplyr::summarize(values = sum(values[geo%in%country_code_EU27_noIE]))%>% 
  dplyr::mutate(geo = "EU27noIE")

tmp_EAnoIE <- df%>% 
  dplyr::group_by(na_item, time, unit)%>% 
  dplyr::summarize(values = sum(values[geo%in%country_code_EA_noIE]))%>%  
  dplyr::mutate(geo = "EAnoIE")

tmp_inovatorke <- df%>% 
  dplyr::group_by(na_item, time, unit)%>% 
  dplyr::summarize(values = sum(values[geo%in%country_code_inovatorke]))%>% 
  dplyr::mutate(geo = "Inovatorke")

tmp_CEEC <- df%>% 
  dplyr::group_by(na_item, time, unit)%>% 
  dplyr::summarize(values = sum(values[geo%in%country_code_CEEC]))%>% 
  dplyr::mutate(geo = "CEEC")


data_emp <- bind_rows(tmp_eu13, tmp_EU27noIE,tmp_EAnoIE, tmp_eu14, tmp_inovatorke, tmp_CEEC, emp)


# Združitev baz
tmp_all <- merge(data_gdp, data_emp, all=TRUE)




##### Preračuni #####

data <- 
  tmp_all%>% pivot_wider(names_from = c(unit, na_item), values_from = values)%>%
  dplyr:: group_by(geo) %>% 
  dplyr::mutate(deflatorBDP = CP_MEUR_B1GQ / CLV10_MEUR_B1GQ)%>%
  dplyr::mutate(sredstva_zap_nom = CP_MEUR_D1 / THS_PER_SAL_DC)%>% 
  dplyr::mutate(sredstva_zap_real = (CP_MEUR_D1 / deflatorBDP) / THS_PER_SAL_DC)%>% 
  dplyr::mutate(prod_nom = CP_MEUR_B1GQ / THS_PER_EMP_DC)%>% 
  dplyr::mutate(prod_real = CLV10_MEUR_B1GQ / THS_PER_EMP_DC)%>% 
  dplyr::mutate(RULC = sredstva_zap_nom / prod_nom)%>% 
  dplyr::mutate(prod_real_hw = CLV10_MEUR_B1GQ / THS_HW_EMP_DC)%>% 
  dplyr::mutate(NULC = sredstva_zap_nom / prod_real)%>%
  pivot_longer(!c(time,geo), names_to = "Indicator", values_to = "Value")%>%
  arrange(time)%>%
  group_by(geo, Indicator)%>%
  dplyr::mutate(YOY = ((Value /lag(Value,1))*100-100))%>%
  dplyr::mutate(Indeks2005 = (Value/(Value[time=="2005-01-01"]))*100)%>% 
  dplyr::mutate(Indeks2007 = (Value/(Value[time=="2007-01-01"]))*100)%>%
  dplyr::mutate(Indeks2008 = (Value/(Value[time=="2008-01-01"]))*100)

 
##### Urejanje  #####

rm(data_emp, data_gdp, df, emp, gdp, tmp_all)


data$Indicator <- replace(data$Indicator, data$Indicator == "CLV10_MEUR_B1GQ", "DV_sc_2010")
data$Indicator <- replace(data$Indicator, data$Indicator == "CP_MEUR_B1GQ", "DV_tc")
data$Indicator <- replace(data$Indicator, data$Indicator == "CP_MEUR_D1", "sredstva")
data$Indicator <- replace(data$Indicator, data$Indicator == "THS_HW_EMP_DC", "zaposlenost_hw")
data$Indicator <- replace(data$Indicator, data$Indicator == "THS_HW_SAL_DC", "zaposleni_hw")
data$Indicator <- replace(data$Indicator, data$Indicator == "THS_PER_EMP_DC", "zaposlenost")
data$Indicator <- replace(data$Indicator, data$Indicator == "THS_PER_SAL_DC", "zaposleni")




##### Zapis v Access  #####


data <- as.data.frame(data)

library(RODBC)
require(RODBC)
conn <- odbcConnectAccess2007(path.expand("O:/Users/KIvas/R/RULC_letni.accdb")) 
try(sqlDrop(conn, "Total", errors = FALSE), silent = TRUE)
sqlSave(conn,data, tablename= "Total", varTypes = c(time="datetime"))
close(conn)



#################################################################################################
#                                  RULC in NULC SKD                                           #
#################################################################################################


##### Download Podatkov #####

va_bulk <- get_eurostat("nama_10_a10") 

va_bulk <- va_bulk%>%rename(time=TIME_PERIOD)


# Filtriranje
va <- droplevels(subset(va_bulk, geo %in% c("EU27_2020" , "EU15" , "EA19" , "BE" , "BG" , "CZ" , "DK" , "DE" , "EE" , "IE" ,"EL" , "ES" ,
                                     "FR" , "HR" , "IT" , "CY" , "LV" , "LT" , "LU" , "HU" , "MT" ,"NL" , "AT" , "FR" , 
                                     "HR" , "IT" , "CY" , "LV" , "LT" , "LU" , "HU" , "MT" ,"NL" , "AT" ,"PL" , "PT" , 
                                     "RO" , "SI" , "SK" , "FI" , "SE" , "UK") 
               & time >= as.Date("1995-01-01") & na_item %in% c("B1G" , "D1") & 
                 unit %in% c("CP_MEUR" , "CLV10_MEUR")))%>%select(-freq)


emp <- droplevels(subset(emp_bulk, geo %in% c("EU27_2020" , "EU15" , "EA19" , "BE" , "BG" , "CZ" , "DK" , "DE" , "EE" , "IE" ,"EL" , "ES" ,
                                       "FR" , "HR" , "IT" , "CY" , "LV" , "LT" , "LU" , "HU" , "MT" ,"NL" , "AT" , "FR" , 
                                       "HR" , "IT" , "CY" , "LV" , "LT" , "LU" , "HU" , "MT" ,"NL" , "AT" ,"PL" , "PT" , 
                                       "RO" , "SI" , "SK" , "FI" , "SE" , "UK") 
                & time >= as.Date("1995-01-01") & na_item %in% c("EMP_DC" , "SAL_DC") & 
                  unit %in% c("THS_PER" , "THS_HW")))%>%select(-freq)



#####   Generiranje novih EU agregatov #####

# Za Value added

df <- va 


tmp_eu13 <- df%>% 
  dplyr:: group_by(na_item, time, nace_r2, unit)%>% 
  dplyr:: summarize(values = sum(values[geo%in%country_code_EU13]))%>% 
  dplyr:: mutate(geo = "EU13")

tmp_eu14 <- df%>% 
  dplyr:: group_by(na_item, time, nace_r2, unit)%>% 
  dplyr:: summarize(values = sum(values[geo%in%country_code_EU14]))%>% 
  dplyr:: mutate(geo = "EU14")

tmp_EU27noIE <- df%>% 
  dplyr:: group_by(na_item, time, nace_r2, unit)%>% 
  dplyr:: summarize(values = sum(values[geo%in%country_code_EU27_noIE]))%>% 
  dplyr:: mutate(geo = "EU27noIE")
tmp_EAnoIE <- df%>% 
  dplyr:: group_by(na_item, time, nace_r2, unit)%>% 
  dplyr::summarize(values = sum(values[geo%in%country_code_EA_noIE]))%>%  
  dplyr::mutate(geo = "EAnoIE")

tmp_inovatorke <- df%>% 
  dplyr:: group_by(na_item, time, nace_r2, unit)%>% 
  dplyr::summarize(values = sum(values[geo%in%country_code_inovatorke]))%>%  
  dplyr::mutate(geo = "Inovatorke")

tmp_CEEC <- df%>% 
  dplyr:: group_by(na_item, time, nace_r2, unit)%>% 
  dplyr::summarize(values = sum(values[geo%in%country_code_CEEC]))%>%  
  dplyr::mutate(geo = "CEEC")

tmp <- rbindlist(list(tmp_eu13, tmp_EU27noIE,tmp_EAnoIE, tmp_eu14, tmp_inovatorke, tmp_CEEC))
data_va <<- rbind (tmp, va) 


# Za EMP

df <- emp 


tmp_eu13 <- df%>% 
  dplyr:: group_by(na_item, time, nace_r2, unit)%>% 
  dplyr::summarize(values = sum(values[geo%in%country_code_EU13]))%>%  
  dplyr::mutate(geo = "EU13")
tmp_eu14 <- df%>% 
  dplyr:: group_by(na_item, time, nace_r2, unit)%>% 
  dplyr::summarize(values = sum(values[geo%in%country_code_EU14]))%>%  
  dplyr::mutate(geo = "EU14")

tmp_EU28noIE <- df%>% 
  dplyr::group_by(na_item, time, nace_r2, unit)%>% 
  dplyr::summarize(values = sum(values[geo%in%country_code_EU27_noIE]))%>% 
  dplyr::mutate(geo = "EU27noIE")
tmp_EAnoIE <- df%>% 
  dplyr::group_by(na_item, time, nace_r2, unit)%>% 
  dplyr::summarize(values = sum(values[geo%in%country_code_EA_noIE]))%>%  
  dplyr::mutate(geo = "EAnoIE")

tmp_inovatorke <- df%>% 
  dplyr::group_by(na_item, time, nace_r2, unit)%>% 
  dplyr::summarize(values = sum(values[geo%in%country_code_inovatorke]))%>%  
  dplyr::mutate(geo = "Inovatorke")
tmp_CEEC <- df%>% 
  dplyr::group_by(na_item, time, nace_r2, unit)%>% 
  dplyr::summarize(values = sum(values[geo%in%country_code_CEEC]))%>%  
  dplyr::mutate(geo = "CEEC")

tmp <- rbindlist(list(tmp_eu13, tmp_EU27noIE,tmp_EAnoIE, tmp_eu14, tmp_inovatorke, tmp_CEEC))
data_emp <<- rbind (tmp, emp) 

# Združitev baz
tmp_all <- merge(data_va, data_emp, all=TRUE)

#####   Generiranje novih SEKTOR agregatov #####

MENJALNI_sektor <- c("A" , "B-E" ,  "G-I" , "J" )
NEMENJALNI_sektor <- c("F" , "K" ,  "L" , "M_N", "O-Q", "R-U" )
POSLOVNI_sektor <- c("B-E" , "F" , "G-I", "J" , "K", "M_N" )
NEPOSLOVNI_sektor <- c("A" , "L" , "O-Q", "R-U" )



df <- tmp_all 

tmp_MENJALNI_sektor <- df%>% 
  dplyr:: group_by(na_item, time, geo, unit)%>% 
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
data_va2 <<- rbind (tmp2, tmp_all) 




##### Preračuni #####

data_nace <- 
  data_va2 %>% 
  dcast(nace_r2 + geo + time ~ unit + na_item, value.var ="values")%>% 
  dplyr:: group_by(geo, nace_r2)%>% 
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
  dplyr::mutate(YOY = ((Value /lag(Value,1))*100-100))%>%
  dplyr::mutate(Indeks2005 = (Value/(Value[time=="2005-01-01"]))*100)%>% 
  dplyr::mutate(Indeks2007 = (Value/(Value[time=="2007-01-01"]))*100)%>%
  dplyr::mutate(Indeks2008 = (Value/(Value[time=="2008-01-01"]))*100)

##### Urejanje  #####

rm(data_emp, data_va, df, emp, va, tmp_all, tmp)



data_nace$Indicator <- replace(data_nace$Indicator, data_nace$Indicator == "CLV10_MEUR_B1G", "DV_sc_2010")
data_nace$Indicator <- replace(data_nace$Indicator, data_nace$Indicator == "CP_MEUR_B1G", "DV_tc")
data_nace$Indicator <- replace(data_nace$Indicator, data_nace$Indicator == "CP_MEUR_D1", "sredstva")
data_nace$Indicator <- replace(data_nace$Indicator, data_nace$Indicator == "THS_HW_EMP_DC", "zaposlenost_hw")
data_nace$Indicator <- replace(data_nace$Indicator, data_nace$Indicator == "THS_HW_SAL_DC", "zaposleni_hw")
data_nace$Indicator <- replace(data_nace$Indicator, data_nace$Indicator == "THS_PER_EMP_DC", "zaposlenost")
data_nace$Indicator <- replace(data_nace$Indicator, data_nace$Indicator == "THS_PER_SAL_DC", "zaposleni")

##### Zapis v Access  #####

data_nace <- as.data.frame(data_nace)

require(RODBC)
conn <- odbcConnectAccess2007(path.expand("O:/Users/KIvas/R/RULC_letni.accdb")) 
try(sqlDrop(conn, "RULC_NACE", errors = FALSE), silent = TRUE)
sqlSave(conn,data_nace, tablename= "RULC_NACE", varTypes = c(time="datetime"))
close(conn)

save(data, file = "M:/Konkurencnost/R/RULC_Total_A.Rdata")
save(data_nace, file = "M:/Konkurencnost/R/RULC_NACE_A.Rdata")






##### Knjiznice in delovno okolje #####
rm(list = ls())

#library(plyr)
suppressPackageStartupMessages(library(tidyr))
suppressPackageStartupMessages(library(pxR))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(rjson))
suppressPackageStartupMessages(library(readxl))
suppressPackageStartupMessages(library(writexl))
suppressPackageStartupMessages(library(data.table))
suppressPackageStartupMessages(library(lubridate))
suppressPackageStartupMessages(library(httr))
suppressPackageStartupMessages(library(stringr))

suppressPackageStartupMessages(library(ggplot2))

suppressPackageStartupMessages(library(zoo))
library(RODBC)
library(extrafont)
library(gridExtra)


setwd("M:/Konkurencnost/R")

###################################################################################
#               KVARTALNI PODATKI, PO SKD                                         #
###################################################################################


##### Prenos Q podatkov #####

url_1 <- "https://pxweb.stat.si/SiStatData/Resources/PX/Databases/Data/0300220S.px"
url_2 <-"https://pxweb.stat.si/SiStatData/Resources/PX/Databases/Data/0300260S.px"
url_3 <-"https://pxweb.stat.si/SiStatData/Resources/PX/Databases/Data/0300240S.px"


bulk_DV <- read.px(url_1, encoding = "cp1250",
                     na.strings = c('"."', '".."', '"..."', '"...."'))
bulk_zap <- read.px(url_2, encoding = "cp1250",
                     na.strings = c('"."', '".."', '"..."', '"...."'))
bulk_stroski <- read.px(url_3, encoding = "cp1250",
                    na.strings = c('"."', '".."', '"..."', '"...."'))


bulk_DV <- as.data.frame(bulk_DV)
bulk_zap <- as.data.frame(bulk_zap)
bulk_stroski <- as.data.frame(bulk_stroski)

#filtriranje (verzija brez desezoniranih podatkov)
DV_q <- bulk_DV%>%filter(VRSTA.PODATKA=="Originalni podatki"& MERITVE%in%c("Stalne cene, referenčno leto 2010 (mio EUR)", "Tekoče cene (mio EUR)")&!TRANSAKCIJE=="Neto davki na proizvode")%>%droplevels()

zap_q <- bulk_zap%>%filter(VRSTA.PODATKA=="Originalni podatki"& VRSTA.ZAPOSLENOSTI%in%c("Zaposlenost (domači koncept)", "..Zaposleni")&!DEJAVNOST.SEKTOR%in%c("..S.15 NPISG","..S.14 Gospodinjstva", "..S.13 Dr?ava","..S.11 Nefinančne družbe","..S.12 Finančne družbe"))%>%
         droplevels()%>%select(-VRSTA.PODATKA)

stroski_q <- bulk_stroski%>%filter(VRSTA.PODATKA=="Originalni podatki")%>%filter(str_detect(TRANSAKCIJE, regex("Sredstva za zaposlene", ignore_case = TRUE)))%>%droplevels()

##### Zdruzitev baz #####

#priprava na zdruzitev
DV_q$ČETRTLETJE <- as.yearqtr(DV_q$ČETRTLETJE, format = "%YQ%q")
zap_q$ČETRTLETJE <- as.yearqtr(zap_q$ČETRTLETJE, format = "%YQ%q")
stroski_q$ČETRTLETJE <- as.yearqtr(stroski_q$ČETRTLETJE, format = "%YQ%q")

DV_q <- spread(DV_q, MERITVE, value)%>%select(-VRSTA.PODATKA)%>%
  dplyr::rename(DEJAVNOST=TRANSAKCIJE)
levels(DV_q$DEJAVNOST)[levels(DV_q$DEJAVNOST)=="Dodana vrednost, skupaj"] <- "Dejavnost - SKUPAJ"

zap_q<-zap_q %>% 
  pivot_wider(id_cols = c(ČETRTLETJE, DEJAVNOST.SEKTOR), names_from = c(MERITVE, VRSTA.ZAPOSLENOSTI))%>% 
  dplyr::rename(DEJAVNOST=DEJAVNOST.SEKTOR)

prod_q <-merge(DV_q,zap_q,by=c("ČETRTLETJE","DEJAVNOST"))
colnames(prod_q) <- c("ČETRTLETJE","DEJAVNOST", "DV_tc", "DV_sc", "zaposlenost_osebe","zaposlenost_ure", "zaposleni_osebe","zaposleni_ure" )

prod_q$SKD = ifelse(prod_q$DEJAVNOST == "A Kmetijstvo, lov, gozdarstvo, ribištvo", "A",
                     ifelse(prod_q$DEJAVNOST == "BCDE Rudarstvo, predelovalne dejavnosti, oskrba z elektriko in vodo, ravnanje z odplakami, saniranje okolja", "BCDE",
                                      ifelse(prod_q$DEJAVNOST == "..od tega: C Predelovalne dejavnosti", "C",
                                                   ifelse(prod_q$DEJAVNOST == "F Gradbeništvo", "F",
                                                          ifelse(prod_q$DEJAVNOST == "GHI Trgovina in popravila vozil, promet in skladiščenje, gostinstvo", "GHI",
                                                                 ifelse(prod_q$DEJAVNOST == "J Informacijske in komunikacijske dejavnosti", "J",
                                                                        ifelse(prod_q$DEJAVNOST == "K Finančne in zavarovalniške dejavnosti", "K",
                                                                                             ifelse(prod_q$DEJAVNOST == "L Poslovanje z nepremičninami", "L",
                                                                                                    ifelse(prod_q$DEJAVNOST == "MN Strokovne, znanstvene, tehnične dejavnosti in druge raznovrstne poslovne dejavnosti", "MN",
                                                                                                           ifelse(prod_q$DEJAVNOST == "OPQ Uprava in obramba, izobraževanje, zdravstvo in socialno varstvo", "OPQ",
                                                                                                                  ifelse(prod_q$DEJAVNOST == "RST Druge storitvene dejavnosti", "RST",
                                                                                                                        ifelse(prod_q$DEJAVNOST == "Dejavnost - SKUPAJ", "SKUPAJ",
                                                                                                                               NA))))))))))))



stroski_q$SKD = ifelse(stroski_q$TRANSAKCIJE == "..Sredstva za zaposlene A Kmetijstvo, lov, gozdarstvo, ribištvo" , "A",
                    ifelse(stroski_q$TRANSAKCIJE == "..Sredstva za zaposlene BCDE Rudarstvo, predelovalne dejavnosti, oskrba z elektriko in vodo, ravnanje z odplakami, saniranje okolja", "BCDE",
                           ifelse(stroski_q$TRANSAKCIJE == "....od tega: sredstva za zaposlene C Predelovalne dejavnosti", "C",
                                  ifelse(stroski_q$TRANSAKCIJE == "..Sredstva za zaposlene F Gradbeništvo", "F",
                                         ifelse(stroski_q$TRANSAKCIJE == "..Sredstva za zaposlene GHI Trgovina in popravila vozil, promet in skladiščenje, gostinstvo", "GHI",
                                                ifelse(stroski_q$TRANSAKCIJE == "..Sredstva za zaposlene J Informacijske in komunikacijske dejavnosti", "J",
                                                       ifelse(stroski_q$TRANSAKCIJE == "..Sredstva za zaposlene K Finančne in zavarovalniške dejavnosti", "K",
                                                              ifelse(stroski_q$TRANSAKCIJE == "..Sredstva za zaposlene L Poslovanje z nepremičninami", "L",
                                                                     ifelse(stroski_q$TRANSAKCIJE == "..Sredstva za zaposlene MN Strokovne, znanstvene, tehnične dejavnosti in druge raznovrstne poslovne dejavnosti", "MN",
                                                                            ifelse(stroski_q$TRANSAKCIJE == "..Sredstva za zaposlene OPQ Uprava in obramba, izobraževanje, zdravstvo in socialno varstvo", "OPQ",
                                                                                   ifelse(stroski_q$TRANSAKCIJE == "..Sredstva za zaposlene RST Druge storitvene dejavnosti", "RST",
                                                                                          ifelse(stroski_q$TRANSAKCIJE == "Sredstva za zaposlene", "SKUPAJ",
                                                                                                 NA))))))))))))


prod_q <- prod_q[c("ČETRTLETJE", "DEJAVNOST", "SKD", "DV_tc", "DV_sc", "zaposlenost_osebe","zaposlenost_ure", "zaposleni_osebe","zaposleni_ure")] 

RULC_q <-merge(prod_q,stroski_q,by=c("ČETRTLETJE","SKD"))
RULC_q<-RULC_q%>%select(-VRSTA.PODATKA, -TRANSAKCIJE)
colnames(RULC_q) <- c("ČETRTLETJE","SKD","DEJAVNOST", "DV_tc", "DV_sc", "zaposlenost_osebe","zaposlenost_ure", "zaposleni_osebe","zaposleni_ure","stroski_dela" )

# novi sektor agregati

prod_q<-prod_q%>% filter(SKD%in%c("BCDE", "F", "GHI", "J", "K", "MN"))%>%
  dplyr::group_by(ČETRTLETJE)%>%
  dplyr::summarise(DEJAVNOST="Poslovni", SKD="Poslovni",DV_tc=sum(DV_tc, na.rm=TRUE), DV_sc=sum(DV_sc, na.rm=TRUE), zaposlenost_osebe=sum(zaposlenost_osebe, na.rm=TRUE), zaposlenost_ure=sum(zaposlenost_ure, na.rm=TRUE), zaposleni_osebe=sum(zaposleni_osebe, na.rm=TRUE), zaposleni_ure=sum(zaposleni_ure, na.rm=TRUE))%>%
  bind_rows(prod_q, .)

prod_q<-prod_q%>% filter(SKD%in%c("A", "L", "OPQ", "RST"))%>%
  dplyr::group_by(ČETRTLETJE)%>%
  dplyr::summarise(DEJAVNOST="Neposlovni", SKD="Neposlovni",DV_tc=sum(DV_tc, na.rm=TRUE), DV_sc=sum(DV_sc, na.rm=TRUE), zaposlenost_osebe=sum(zaposlenost_osebe, na.rm=TRUE), zaposlenost_ure=sum(zaposlenost_ure, na.rm=TRUE), zaposleni_osebe=sum(zaposleni_osebe, na.rm=TRUE), zaposleni_ure=sum(zaposleni_ure, na.rm=TRUE))%>%
  bind_rows(prod_q, .)

prod_q<-prod_q%>% filter(SKD%in%c("A", "BCDE", "GHI", "J"))%>%
  dplyr::group_by(ČETRTLETJE)%>%
  dplyr::summarise(DEJAVNOST="Menjalni", SKD="Menjalni",DV_tc=sum(DV_tc, na.rm=TRUE), DV_sc=sum(DV_sc, na.rm=TRUE), zaposlenost_osebe=sum(zaposlenost_osebe, na.rm=TRUE), zaposlenost_ure=sum(zaposlenost_ure, na.rm=TRUE), zaposleni_osebe=sum(zaposleni_osebe, na.rm=TRUE), zaposleni_ure=sum(zaposleni_ure, na.rm=TRUE))%>%
  bind_rows(prod_q, .)

prod_q<-prod_q%>% filter(SKD%in%c("F", "K", "L", "MN", "OPQ", "RST"))%>%
  dplyr::group_by(ČETRTLETJE)%>%
  dplyr::summarise(DEJAVNOST="Nemenjalni", SKD="Nemenjalni",DV_tc=sum(DV_tc, na.rm=TRUE), DV_sc=sum(DV_sc, na.rm=TRUE), zaposlenost_osebe=sum(zaposlenost_osebe, na.rm=TRUE), zaposlenost_ure=sum(zaposlenost_ure, na.rm=TRUE), zaposleni_osebe=sum(zaposleni_osebe, na.rm=TRUE), zaposleni_ure=sum(zaposleni_ure, na.rm=TRUE))%>%
  bind_rows(prod_q, .)


prod_q$ČETRTLETJE <- as.yearqtr(prod_q$ČETRTLETJE, format = "%YQ%q")


#####   Preračuni  za Produktivnost ######


prod_q <- prod_q%>% select(-zaposleni_osebe, -zaposleni_ure)%>%
  dplyr::group_by(ČETRTLETJE)%>%
  dplyr::mutate(delez_zap = zaposlenost_osebe / zaposlenost_osebe[DEJAVNOST=="Dejavnost - SKUPAJ"]*100)%>%
  dplyr::mutate(delez_ure = zaposlenost_ure / zaposlenost_ure[DEJAVNOST=="Dejavnost - SKUPAJ"]*100)%>%
  dplyr::mutate(delez_dv = DV_tc / DV_tc[DEJAVNOST=="Dejavnost - SKUPAJ"]*100)%>%
  ungroup ()%>%
  dplyr::group_by(ČETRTLETJE,DEJAVNOST)%>%
  dplyr::mutate(PROD_real = DV_sc/zaposlenost_osebe)%>% ungroup()%>%
  dplyr::mutate(PROD_real_wh = DV_sc/zaposlenost_ure)%>% 
  dplyr::mutate(PROD_nom = DV_tc/zaposlenost_osebe)%>% 
  dplyr::mutate(PROD_nom_wh = DV_tc/zaposlenost_ure)%>% 
  arrange(ČETRTLETJE)%>%
  group_by(DEJAVNOST)%>%
  dplyr::mutate(PROD_yoy = ((PROD_real /lag(PROD_real,4))*100-100)) %>%
  dplyr::mutate(PROD_yoy_wh = ((PROD_real_wh /lag(PROD_real_wh,4))*100-100))%>%ungroup()%>%
  arrange(ČETRTLETJE)%>%
  group_by(DEJAVNOST)%>%
  dplyr::mutate(WITHIN_effect = ((PROD_real -lag(PROD_real,4))*lag(delez_zap,4))) %>%
  dplyr::mutate(STATIC_shift = (delez_zap -lag(delez_zap,4))*lag(PROD_real,4))%>%
  dplyr::mutate(DYNAMIC_SHIFT = (delez_zap -lag(delez_zap,4))*(PROD_real -lag(PROD_real,4)))%>%
  dplyr::mutate(WITHIN_effect_wh = ((PROD_real_wh -lag(PROD_real_wh,4))*lag(delez_ure,4))) %>%
  dplyr::mutate(STATIC_shift_wh = (delez_ure -lag(delez_ure,4))*lag(PROD_real_wh,4))%>%
  dplyr::mutate(DYNAMIC_SHIFT_wh = (delez_ure -lag(delez_ure,4))*(PROD_real_wh -lag(PROD_real_wh,4)))%>%
  ungroup()

# novi sektor agregati

RULC_q<-RULC_q%>% filter(SKD%in%c("BCDE", "F", "GHI", "J", "K", "MN"))%>%
  dplyr::group_by(ČETRTLETJE)%>%
  dplyr::summarise(DEJAVNOST="Poslovni", SKD="Poslovni",DV_tc=sum(DV_tc, na.rm=TRUE), DV_sc=sum(DV_sc, na.rm=TRUE), zaposlenost_osebe=sum(zaposlenost_osebe, na.rm=TRUE), zaposlenost_ure=sum(zaposlenost_ure, na.rm=TRUE), zaposleni_osebe=sum(zaposleni_osebe, na.rm=TRUE), zaposleni_ure=sum(zaposleni_ure, na.rm=TRUE), stroski_dela=sum(stroski_dela, na.rm=TRUE))%>%
  bind_rows(RULC_q, .)

RULC_q<-RULC_q%>% filter(SKD%in%c("A", "L", "OPQ", "RST"))%>%
  dplyr::group_by(ČETRTLETJE)%>%
  dplyr::summarise(DEJAVNOST="Neposlovni", SKD="Neposlovni",DV_tc=sum(DV_tc, na.rm=TRUE), DV_sc=sum(DV_sc, na.rm=TRUE), zaposlenost_osebe=sum(zaposlenost_osebe, na.rm=TRUE), zaposlenost_ure=sum(zaposlenost_ure, na.rm=TRUE), zaposleni_osebe=sum(zaposleni_osebe, na.rm=TRUE), zaposleni_ure=sum(zaposleni_ure, na.rm=TRUE), stroski_dela=sum(stroski_dela, na.rm=TRUE))%>%
  bind_rows(RULC_q, .)

RULC_q<-RULC_q%>% filter(SKD%in%c("A", "BCDE", "GHI", "J"))%>%
  dplyr::group_by(ČETRTLETJE)%>%
  dplyr::summarise(DEJAVNOST="Menjalni", SKD="Menjalni",DV_tc=sum(DV_tc, na.rm=TRUE), DV_sc=sum(DV_sc, na.rm=TRUE), zaposlenost_osebe=sum(zaposlenost_osebe, na.rm=TRUE), zaposlenost_ure=sum(zaposlenost_ure, na.rm=TRUE), zaposleni_osebe=sum(zaposleni_osebe, na.rm=TRUE), zaposleni_ure=sum(zaposleni_ure, na.rm=TRUE), stroski_dela=sum(stroski_dela, na.rm=TRUE))%>%
  bind_rows(RULC_q, .)

RULC_q<-RULC_q%>% filter(SKD%in%c("F", "K", "L", "MN", "OPQ", "RST"))%>%
  dplyr::group_by(ČETRTLETJE)%>%
  dplyr::summarise(DEJAVNOST="Nemenjalni", SKD="Nemenjalni",DV_tc=sum(DV_tc, na.rm=TRUE), DV_sc=sum(DV_sc, na.rm=TRUE), zaposlenost_osebe=sum(zaposlenost_osebe, na.rm=TRUE), zaposlenost_ure=sum(zaposlenost_ure, na.rm=TRUE), zaposleni_osebe=sum(zaposleni_osebe, na.rm=TRUE), zaposleni_ure=sum(zaposleni_ure, na.rm=TRUE), stroski_dela=sum(stroski_dela, na.rm=TRUE))%>%
  bind_rows(RULC_q, .)

RULC_q<-RULC_q%>% filter(SKD%in%c("BCDE", "C"))%>%select(-DEJAVNOST)%>%
                      gather("kazalnik", "vrednost", 3:9)%>%
                      spread("SKD", "vrednost")%>%
                      mutate(BDE=BCDE-C)%>%select(-BCDE,-C)%>%
                      spread("kazalnik", "BDE")%>%
                      mutate(SKD="BDE", DEJAVNOST="Rudarstvo, oskrba z elektriko in vodo, ravnanje z odplakami, saniranje okolja")%>%
                      bind_rows(RULC_q, .)

RULC_q$ČETRTLETJE <- as.yearqtr(RULC_q$ČETRTLETJE, format = "%YQ%q")
RULC_A_q <- RULC_q %>%
  separate(ČETRTLETJE, c("LETO", "Q"), " ")


#####   Preračuni  za RULC ######

RULC_q <- RULC_q%>%
  dplyr::group_by(ČETRTLETJE,DEJAVNOST)%>%
  dplyr::mutate(PROD_real = DV_sc/zaposlenost_osebe)%>% ungroup()%>%
  dplyr::mutate(PROD_real_wh = DV_sc/zaposlenost_ure)%>% 
  dplyr::mutate(PROD_nom = DV_tc/zaposlenost_osebe)%>% 
  dplyr::mutate(PROD_nom_wh = DV_tc/zaposlenost_ure)%>% 
  dplyr::mutate(stroski_dela_zap = stroski_dela/zaposleni_osebe)%>% 
  dplyr::mutate(stroski_dela_wh = stroski_dela/zaposleni_ure)%>% 
  dplyr::mutate(RULC= stroski_dela_zap/PROD_nom)%>%
  dplyr::mutate(NULC= stroski_dela_zap/PROD_real)%>%
  gather("Kazalnik", "Vrednost", 4:18)%>%
  arrange(ČETRTLETJE)%>%
  group_by(DEJAVNOST, Kazalnik)%>%
  dplyr::mutate(YOY = ((Vrednost /lag(Vrednost,4))*100-100))%>%
  dplyr::mutate(Indeks2005 = (Vrednost/(mean(Vrednost[ČETRTLETJE >= "2005 Q1"& ČETRTLETJE <= "2005 Q4"]))*100))%>% #Poskusi spravit v datum, da ne bo kaj narobe
  dplyr:: mutate(Indeks2005_4cds=rollmean(Indeks2005,4, fill=NA, align = "right"))%>%
  dplyr::mutate(Indeks2007 = (Vrednost/(mean(Vrednost[ČETRTLETJE >= "2007 Q1"& ČETRTLETJE <= "2007 Q4"]))*100))%>% #Poskusi spravit v datum, da ne bo kaj narobe
  dplyr:: mutate(Indeks2007_4cds=rollmean(Indeks2007,4, fill=NA, align = "right"))%>%
  dplyr::mutate(Indeks2008 = (Vrednost/(mean(Vrednost[ČETRTLETJE >= "2008 Q1"& ČETRTLETJE <= "2008 Q4"]))*100))%>% #Poskusi spravit v datum, da ne bo kaj narobe
  dplyr:: mutate(Indeks2008_4cds=rollmean(Indeks2008,4, fill=NA, align = "right"))


###################################################################################
#               LETNI PODATKI, PO SKD                                             #
###################################################################################

##### Prenos A podatkov #####

url_4 <- "https://pxweb.stat.si/SiStatData/Resources/PX/Databases/Data/0301915S.px"
url_5 <-"https://pxweb.stat.si/SiStatData/Resources/PX/Databases/Data/0301975S.px"
url_6 <-"https://pxweb.stat.si/SiStatData/Resources/PX/Databases/Data/0301930S.px"


bulk_DV_A <- read.px(url_4, encoding = "cp1250",
                   na.strings = c('"."', '".."', '"..."', '"...."'))
bulk_zap_A <- read.px(url_5, encoding = "cp1250",
                    na.strings = c('"."', '".."', '"..."', '"...."'))
bulk_stroski_A <- read.px(url_6, encoding = "cp1250",
                        na.strings = c('"."', '".."', '"..."', '"...."'))


bulk_DV_A <- as.data.frame(bulk_DV_A)
bulk_zap_A <- as.data.frame(bulk_zap_A)
bulk_stroski_A <- as.data.frame(bulk_stroski_A)

#filtriranje

DV_A <- bulk_DV_A%>%filter(MERITVE%in%c("Stalne cene, referenčno leto 2010 (mio EUR)", "Tekoče cene (mio EUR)")&TRANSAKCIJE=="Dodana vrednost"&!DEJAVNOSTI.TRANSAKCIJE%in%c("Neto davki na proizvode","..Davki na proizvode","..Minus: subvencije po proizvodih"))%>%droplevels()

zap_A <- bulk_zap_A%>%filter(!TRANSAKCIJE=="..Samozaposleni"&!DEJAVNOSTI.SEKTORJI%in%c("..S.15 NPISG","..S.14 Gospodinjstva", "..S.13 Država","..S.11 Nefinančne družbe","..S.12 Finančne družbe"))%>%droplevels() #%>%select(-VRSTA.ZAPOSLENOSTI)

stroski_A <- bulk_stroski_A%>%filter(str_detect(TRANSAKCIJE, regex("Sredstva za zaposlene", ignore_case = TRUE)))%>%droplevels()

##### Zdruzitev baz - 2 varianti#####



#priprava na zdruzitev

DV_A <- spread(DV_A, MERITVE, value)%>%select(-TRANSAKCIJE)%>%
  dplyr::rename(DEJAVNOST=DEJAVNOSTI.TRANSAKCIJE)

zap_A<-zap_A %>% 
         pivot_wider(id_cols = c(LETO, DEJAVNOSTI.SEKTORJI), names_from = c(MERITVE, TRANSAKCIJE))%>% 
  dplyr::rename(DEJAVNOST=DEJAVNOSTI.SEKTORJI)

prod_A <-merge(DV_A,zap_A,by=c("LETO","DEJAVNOST"))

colnames(prod_A) <- c("LETO","DEJAVNOST", "DV_tc", "DV_sc", "zaposlenost_osebe","zaposlenost_ure", "zaposleni_osebe","zaposleni_ure" )

## 1. podrobna varianta za shift-share 
prod_A_det <-prod_A%>%
  filter(!grepl("^(A|C|E|G|H|J|K|M|N|O|Q|R|S|U|..12|..od)",DEJAVNOST))%>%droplevels()

## 2. varianta na ravni ČRK za RULC
prod_A_SKD <-prod_A%>%
  filter(grepl("^(A|B|C|D|E|F|G|H|I|J|K|L|M|N|O|P|Q|R|S|T)",DEJAVNOST))%>%droplevels()

#####   PREARČUNI SHIFT-SHARE ######

prod_A_det <- prod_A_det%>%select(-zaposleni_osebe, -zaposleni_ure)
prod_A_det <- prod_A_det%>%
  dplyr::group_by(LETO)%>%
  dplyr::summarise(DEJAVNOST="SKUPAJ",DV_tc=sum(DV_tc, na.rm=TRUE), DV_sc=sum(DV_sc, na.rm=TRUE), zaposlenost_osebe=sum(zaposlenost_osebe, na.rm=TRUE), zaposlenost_ure=sum(zaposlenost_ure, na.rm=TRUE))%>%
  bind_rows(prod_A_det, .)

prod_A_det <- prod_A_det%>%
  dplyr::group_by(LETO)%>%
  dplyr::mutate(delez_zap = zaposlenost_osebe / zaposlenost_osebe[DEJAVNOST=="SKUPAJ"]*100)%>%
  dplyr::mutate(delez_ure = zaposlenost_ure / zaposlenost_ure[DEJAVNOST=="SKUPAJ"]*100)%>%
  dplyr::mutate(delez_dv = DV_tc / DV_tc[DEJAVNOST=="SKUPAJ"]*100)%>%
  ungroup ()%>%
  dplyr::group_by(LETO,DEJAVNOST)%>%
  dplyr::mutate(PROD_real = DV_sc/zaposlenost_osebe)%>% ungroup()%>%
  dplyr::mutate(PROD_real_wh = DV_sc/zaposlenost_ure)%>% 
  dplyr::mutate(PROD_nom = DV_tc/zaposlenost_osebe)%>% 
  dplyr::mutate(PROD_nom_wh = DV_tc/zaposlenost_ure)%>% 
  arrange(LETO)%>%
  group_by(DEJAVNOST)%>%
  dplyr::mutate(PROD_yoy = ((PROD_real /lag(PROD_real,1))*100-100)) %>%
  dplyr::mutate(PROD_yoy_wh = ((PROD_real_wh /lag(PROD_real_wh,1))*100-100))%>%ungroup()%>%
  arrange(LETO)%>%
  group_by(DEJAVNOST)%>%
  dplyr::mutate(WITHIN_effect = ((PROD_real -lag(PROD_real,1))*lag(delez_zap,1))) %>%
  dplyr::mutate(STATIC_shift = (delez_zap -lag(delez_zap,1))*lag(PROD_real,1))%>%
  dplyr::mutate(DYNAMIC_SHIFT = (delez_zap -lag(delez_zap,1))*(PROD_real -lag(PROD_real,1)))%>%
  dplyr::mutate(WITHIN_effect_wh = ((PROD_real_wh -lag(PROD_real_wh,1))*lag(delez_ure,1))) %>%
  dplyr::mutate(STATIC_shift_wh = (delez_ure -lag(delez_ure,1))*lag(PROD_real_wh,1))%>%
  dplyr::mutate(DYNAMIC_SHIFT_wh = (delez_ure -lag(delez_ure,1))*(PROD_real_wh -lag(PROD_real_wh,1)))%>%
  ungroup()

#####   PREARČUNI RULC ######

# zdruzitev z bazo za sredstva

stroski_A <- bulk_stroski_A%>%filter(TRANSAKCIJE=="Sredstva za zaposlene")%>%
  select(-TRANSAKCIJE)%>%
  dplyr::rename(DEJAVNOST=DEJAVNOSTI, sredstva=value)%>%
  filter(grepl("^(A|B|C|D|E|F|G|H|I|J|K|L|M|N|O|P|Q|R|S|T)",DEJAVNOST))%>%droplevels()

RULC_A <-merge(prod_A_SKD,stroski_A,by=c("LETO","DEJAVNOST"))

# Simple SKD oznake 

RULC_A$SKD <- substr(RULC_A$DEJAVNOST, 1,1)
RULC_A$SKD[RULC_A$DEJAVNOST=="Skupaj dejavnosti"] <- "SKUPAJ"
RULC_A <- RULC_A[c("LETO", "DEJAVNOST", "SKD", "DV_tc", "DV_sc", "zaposlenost_osebe","zaposlenost_ure", "zaposleni_osebe","zaposleni_ure", "sredstva")]  #reorder

# novi sektor agregati

RULC_A<-RULC_A%>% filter(SKD%in%c("B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "M", "N"))%>%
  dplyr::group_by(LETO)%>%
  dplyr::summarise(DEJAVNOST="Poslovni", SKD="Poslovni",DV_tc=sum(DV_tc, na.rm=TRUE), DV_sc=sum(DV_sc, na.rm=TRUE), zaposlenost_osebe=sum(zaposlenost_osebe, na.rm=TRUE), zaposlenost_ure=sum(zaposlenost_ure, na.rm=TRUE), zaposleni_osebe=sum(zaposleni_osebe, na.rm=TRUE), zaposleni_ure=sum(zaposleni_ure, na.rm=TRUE),sredstva=sum(sredstva, na.rm=TRUE))%>%
  bind_rows(RULC_A, .)

RULC_A<-RULC_A%>% filter(SKD%in%c("A", "L", "O", "P", "Q", "R", "S", "T"))%>%
  dplyr::group_by(LETO)%>%
  dplyr::summarise(DEJAVNOST="Neposlovni", SKD="Neposlovni",DV_tc=sum(DV_tc, na.rm=TRUE), DV_sc=sum(DV_sc, na.rm=TRUE), zaposlenost_osebe=sum(zaposlenost_osebe, na.rm=TRUE), zaposlenost_ure=sum(zaposlenost_ure, na.rm=TRUE), zaposleni_osebe=sum(zaposleni_osebe, na.rm=TRUE), zaposleni_ure=sum(zaposleni_ure, na.rm=TRUE), sredstva=sum(sredstva, na.rm=TRUE))%>%
  bind_rows(RULC_A, .)

RULC_A<-RULC_A%>% filter(SKD%in%c("A","B", "C", "D", "E", "G", "H", "I", "J"))%>%
  dplyr::group_by(LETO)%>%
  dplyr::summarise(DEJAVNOST="Menjalni", SKD="Menjalni",DV_tc=sum(DV_tc, na.rm=TRUE), DV_sc=sum(DV_sc, na.rm=TRUE), zaposlenost_osebe=sum(zaposlenost_osebe, na.rm=TRUE), zaposlenost_ure=sum(zaposlenost_ure, na.rm=TRUE), zaposleni_osebe=sum(zaposleni_osebe, na.rm=TRUE), zaposleni_ure=sum(zaposleni_ure, na.rm=TRUE), sredstva=sum(sredstva, na.rm=TRUE))%>%
  bind_rows(RULC_A, .)

RULC_A<-RULC_A%>% filter(SKD%in%c("F","K", "L", "M", "N" ,"O", "P", "Q", "R", "S", "T"))%>%
  dplyr::group_by(LETO)%>%
  dplyr::summarise(DEJAVNOST="Nemenjalni", SKD="Nemenjalni",DV_tc=sum(DV_tc, na.rm=TRUE), DV_sc=sum(DV_sc, na.rm=TRUE), zaposlenost_osebe=sum(zaposlenost_osebe, na.rm=TRUE), zaposlenost_ure=sum(zaposlenost_ure, na.rm=TRUE), zaposleni_osebe=sum(zaposleni_osebe, na.rm=TRUE), zaposleni_ure=sum(zaposleni_ure, na.rm=TRUE), sredstva=sum(sredstva, na.rm=TRUE))%>%
  bind_rows(RULC_A, .)

#####   Preračuni  ######

RULC_A <- RULC_A%>%
  dplyr::group_by(LETO,DEJAVNOST, SKD)%>%
  dplyr::mutate(PROD_real = DV_sc/zaposlenost_osebe)%>% ungroup()%>%
  dplyr::mutate(PROD_real_wh = DV_sc/zaposlenost_ure)%>% 
  dplyr::mutate(PROD_nom = DV_tc/zaposlenost_osebe)%>% 
  dplyr::mutate(PROD_nom_wh = DV_tc/zaposlenost_ure)%>% 
  dplyr::mutate(stroski_dela_zap = sredstva/zaposleni_osebe)%>% 
  dplyr::mutate(stroski_dela_wh = sredstva/zaposleni_ure)%>% 
  dplyr::mutate(RULC= stroski_dela_zap/PROD_nom)%>%
  dplyr::mutate(NULC= stroski_dela_zap/PROD_real)%>%
  gather("Kazalnik", "Vrednost", 4:18)%>%  
  arrange(LETO)%>%
  group_by(DEJAVNOST, Kazalnik)%>%
  dplyr::mutate(YOY = ((Vrednost /lag(Vrednost,1))*100-100))%>%
  dplyr::mutate(Indeks2000 = (Vrednost/(Vrednost[LETO == "2000"])*100))%>% 
  dplyr::mutate(Indeks2005 = (Vrednost/(Vrednost[LETO == "2005"])*100))%>%
  ungroup()

###################################################################################
#                       LETNI IZ Q PODATKOV, PO SKD                               #
###################################################################################



# 
# library(dplyr)
# RULC_A_q <- RULC_A_q%>%
#   dplyr::group_by(LETO, SKD, DEJAVNOST)%>%
#   summarise(DV_tc = sum(DV_tc), DV_sc = sum(DV_sc), zaposlenost_ure = sum(zaposlenost_ure), zaposleni_ure = sum(zaposleni_ure), stroski_dela = sum(stroski_dela), zaposlenost_osebe=mean(zaposlenost_osebe), zaposleni_osebe=mean(zaposleni_osebe))
# 
# RULC_A_q <- RULC_A_q%>%
#   dplyr::group_by(LETO,DEJAVNOST, SKD)%>%
#   dplyr::mutate(PROD_real = DV_sc/zaposlenost_osebe)%>% ungroup()%>%
#   dplyr::mutate(PROD_real_wh = DV_sc/zaposlenost_ure)%>% 
#   dplyr::mutate(PROD_nom = DV_tc/zaposlenost_osebe)%>% 
#   dplyr::mutate(PROD_nom_wh = DV_tc/zaposlenost_ure)%>% 
#   dplyr::mutate(stroski_dela_zap = stroski_dela/zaposleni_osebe)%>% 
#   dplyr::mutate(stroski_dela_wh = stroski_dela/zaposleni_ure)%>% 
#   dplyr::mutate(RULC= stroski_dela_zap/PROD_nom)%>%
#   dplyr::mutate(NULC= stroski_dela_zap/PROD_real)%>%
#   gather("Kazalnik", "Vrednost", 4:18)%>%  
#   arrange(LETO)%>%
#   group_by(DEJAVNOST, Kazalnik)%>%
#   dplyr::mutate(YOY = ((Vrednost /lag(Vrednost,1))*100-100))%>%
#   dplyr::mutate(Indeks2000 = (Vrednost/(Vrednost[LETO == "2000"])*100))%>% 
#   dplyr::mutate(Indeks2005 = (Vrednost/(Vrednost[LETO == "2005"])*100))%>%
#   ungroup()


###################################################################################
#               TOTAL GOSPODARSTVO Q                                              #
###################################################################################

# filtriranje in oblikovanje
BDP_q <- bulk_DV%>%filter(VRSTA.PODATKA%in%c("Originalni podatki","Podatki z izločenimi vplivi sezone in koledarja")& MERITVE%in%c("Stalne cene, referenčno leto 2010 (mio EUR)", "Tekoče cene (mio EUR)")&TRANSAKCIJE=="Bruto domači proizvod")%>%
         droplevels()%>%select(-TRANSAKCIJE)%>%
         pivot_wider(id_cols = c(ČETRTLETJE,VRSTA.PODATKA), names_from = MERITVE)
colnames(BDP_q) <- c("ČETRTLETJE","season_adj","BDP_tc", "BDP_sc_2010" )


tot_zap_q <- bulk_zap%>%filter(VRSTA.PODATKA%in%c("Originalni podatki", "Podatki z izločenimi vplivi sezone in koledarja")& VRSTA.ZAPOSLENOSTI%in%c("Zaposlenost (domači koncept)", "..Zaposleni")&DEJAVNOST.SEKTOR=="Dejavnost - SKUPAJ")%>%
             droplevels()%>%select(-DEJAVNOST.SEKTOR)%>%
             pivot_wider(id_cols = c(ČETRTLETJE,VRSTA.PODATKA), names_from = c(MERITVE, VRSTA.ZAPOSLENOSTI))
colnames(tot_zap_q) <- c("ČETRTLETJE","season_adj","zaposlenost_osebe", "zaposlenost_ure", "zaposleni_osebe","zaposleni_ure" )


tot_stroski_q <- bulk_stroski%>%filter(VRSTA.PODATKA%in%c("Originalni podatki", "Podatki z izločenimi vplivi sezone in koledarja")&TRANSAKCIJE=="Sredstva za zaposlene")%>%
                 droplevels()%>%select(-TRANSAKCIJE)
colnames(tot_stroski_q) <- c("ČETRTLETJE","season_adj","sredstva" )            

# združitev baz
TOT_PROD_q <-merge(BDP_q,tot_zap_q, by=c("ČETRTLETJE","season_adj"))
TOT_RULC_q <-merge(TOT_PROD_q,tot_stroski_q, by=c("ČETRTLETJE","season_adj"))



TOT_RULC_q$ČETRTLETJE <- as.yearqtr(TOT_RULC_q$ČETRTLETJE, format = "%YQ%q")


#####   Preračuni  za RULC ######

TOT_RULC_q <- TOT_RULC_q%>%
  dplyr::group_by(ČETRTLETJE,season_adj)%>%
  dplyr::mutate(PROD_real = BDP_sc_2010/zaposlenost_osebe)%>% ungroup()%>%
  dplyr::mutate(PROD_real_wh = BDP_sc_2010/zaposlenost_ure)%>% 
  dplyr::mutate(PROD_nom = BDP_tc/zaposlenost_osebe)%>% 
  dplyr::mutate(PROD_nom_wh = BDP_tc/zaposlenost_ure)%>% 
  dplyr::mutate(stroski_dela_zap = sredstva/zaposleni_osebe)%>% 
  dplyr::mutate(stroski_dela_wh = sredstva/zaposleni_ure)%>% 
  dplyr::mutate(RULC= stroski_dela_zap/PROD_nom)%>%
  dplyr::mutate(NULC= stroski_dela_zap/PROD_real)%>%
  gather("Kazalnik", "Vrednost", 3:17)%>%
  arrange(ČETRTLETJE)%>%
  group_by(season_adj, Kazalnik)%>%
  dplyr::mutate(YOY = ((Vrednost /lag(Vrednost,4))*100-100))%>%
  dplyr::mutate(Indeks2005 = (Vrednost/(mean(Vrednost[ČETRTLETJE >= "2005 Q1"& ČETRTLETJE <= "2005 Q4"]))*100))%>% #Poskusi spravit v datum, da ne bo kaj narobe
  dplyr:: mutate(Indeks2005_4cds=rollmean(Indeks2005,4, fill=NA, align = "right"))%>%
  dplyr::mutate(Indeks2007 = (Vrednost/(mean(Vrednost[ČETRTLETJE >= "2007 Q1"& ČETRTLETJE <= "2007 Q4"]))*100))%>% #Poskusi spravit v datum, da ne bo kaj narobe
  dplyr:: mutate(Indeks2007_4cds=rollmean(Indeks2007,4, fill=NA, align = "right"))%>%
  dplyr::mutate(Indeks2008 = (Vrednost/(mean(Vrednost[ČETRTLETJE >= "2008 Q1"& ČETRTLETJE <= "2008 Q4"]))*100))%>% #Poskusi spravit v datum, da ne bo kaj narobe
  dplyr:: mutate(Indeks2008_4cds=rollmean(Indeks2008,4, fill=NA, align = "right"))

###################################################################################
#               TOTAL GOSPODARSTVO A                                              #
###################################################################################

url_7 <-"https://pxweb.stat.si/SiStatData/Resources/PX/Databases/Data/0301910S.px"
url_8 <-"https://pxweb.stat.si/SiStatData/Resources/PX/Databases/Data/0301925S.px"

bulk_BDP_A <- read.px(url_7, encoding = "cp1250",
                          na.strings = c('"."', '".."', '"..."', '"...."'))
bulk_sredstva_A <- read.px(url_8, encoding = "cp1250",
                      na.strings = c('"."', '".."', '"..."', '"...."'))

bulk_BDP_A <- as.data.frame(bulk_BDP_A)
bulk_sredstva_A <- as.data.frame(bulk_sredstva_A)


# filtriranje in oblikovanje
BDP_A <- bulk_BDP_A%>%filter(MERITVE%in%c("Stalne cene, referenčno leto 2010 (mio EUR)", "Tekoče cene (mio EUR)"))%>%droplevels()%>%
         pivot_wider(id_cols = LETO, names_from = MERITVE)
colnames(BDP_A) <- c("LETO","BDP_tc", "BDP_sc_2010" )
 

tot_zap_A <- bulk_zap_A%>%filter(TRANSAKCIJE%in%c("Zaposlenost", "..Zaposleni")&DEJAVNOSTI.SEKTORJI=="Skupaj dejavnosti")%>%
  droplevels()%>%select(-DEJAVNOSTI.SEKTORJI)%>%
  pivot_wider(id_cols = LETO, names_from = c(MERITVE, TRANSAKCIJE))
colnames(tot_zap_A) <- c("LETO","zaposlenost_osebe", "zaposlenost_ure", "zaposleni_osebe","zaposleni_ure" )


tot_stroski_A <- bulk_sredstva_A%>%filter(TRANSAKCIJE=="Sredstva za zaposlene"&MERITVE=="Tekoče cene (mio EUR)")%>%
  droplevels()%>%select(-MERITVE,-TRANSAKCIJE)
colnames(tot_stroski_A) <- c("LETO","sredstva" )            


# združitev baz
TOT_PROD_A <-merge(BDP_A,tot_zap_A, by=c("LETO"))
TOT_RULC_A <-merge(TOT_PROD_A,tot_stroski_A, by=c("LETO"))

#####   Preračuni  ######

TOT_RULC_A <- TOT_RULC_A%>%
  dplyr::group_by(LETO)%>%
  dplyr::mutate(PROD_real = BDP_sc_2010/zaposlenost_osebe)%>% ungroup()%>%
  dplyr::mutate(PROD_real_wh = BDP_sc_2010/zaposlenost_ure)%>% 
  dplyr::mutate(PROD_nom = BDP_tc/zaposlenost_osebe)%>% 
  dplyr::mutate(PROD_nom_wh = BDP_tc/zaposlenost_ure)%>% 
  dplyr::mutate(stroski_dela_zap = sredstva/zaposleni_osebe)%>% 
  dplyr::mutate(stroski_dela_wh = sredstva/zaposleni_ure)%>% 
  dplyr::mutate(RULC= stroski_dela_zap/PROD_nom)%>%
  dplyr::mutate(NULC= stroski_dela_zap/PROD_real)%>%
  gather("Kazalnik", "Vrednost", 2:16)%>%  
  arrange(LETO)%>%
  group_by(Kazalnik)%>%
  dplyr::mutate(YOY = ((Vrednost /lag(Vrednost,1))*100-100))%>%
  dplyr::mutate(Indeks2000 = (Vrednost/(Vrednost[LETO == "2000"])*100))%>% 
  dplyr::mutate(Indeks2005 = (Vrednost/(Vrednost[LETO == "2005"])*100))%>%
  ungroup()



###################################################################################
#                     ZAPIS V ACCESS in RDA                                             #
###################################################################################

##### Zapis v RDA  #####

save(TOT_RULC_A, file = "SURS_TOT_RULC_A.Rdata")
save(TOT_RULC_q, file = "SURS_TOT_RULC_q.Rdata")
save(RULC_A, file = "SURS_RULC_A.Rdata")
save(RULC_q, file = "SURS_RULC_q.Rdata")
save(RULC_A_q, file = "SURS_RULC_A_q.Rdata")
save(prod_q, file = "SURS_prod_q.Rdata")
save(prod_A_det, file = "SURS_prod_A_det.Rdata")



##### Zapis v Access  #####

TOT_RULC_A$LETO <- as.character(TOT_RULC_A$LETO)
TOT_RULC_q$ČETRTLETJE <- as.character(TOT_RULC_q$ČETRTLETJE)
TOT_RULC_q$season_adj <- as.character(TOT_RULC_q$season_adj)
RULC_q$ČETRTLETJE <- as.character(RULC_q$ČETRTLETJE)
RULC_A_q$LETO <- as.character(RULC_A_q$LETO)
RULC_A$LETO <- as.character(RULC_A$LETO)
prod_q$ČETRTLETJE <- as.character(prod_q$ČETRTLETJE)
prod_A_det$LETO <- as.character(prod_A_det$LETO)


TOT_RULC_q <- as.data.frame(TOT_RULC_q)
RULC_q <- as.data.frame(RULC_q)
RULC_A_q <- as.data.frame(RULC_A_q)
TOT_PROD_q <- as.data.frame(TOT_PROD_q)



# access
require(RODBC)
conn <- odbcConnectAccess2007(path.expand("O:/Users/KIvas/R/SURS_PROD_RULC.accdb")) 
try(sqlDrop(conn, "TOT_RULC_A", errors = TRUE), silent = FALSE)
sqlSave(conn,TOT_RULC_A, tablename= "TOT_RULC_A")
try(sqlDrop(conn, "TOT_RULC_q", errors = TRUE), silent = FALSE)
sqlSave(conn,TOT_RULC_q, tablename= "TOT_RULC_q")
try(sqlDrop(conn, "RULC_q", errors = TRUE), silent = FALSE)
sqlSave(conn,RULC_q, tablename= "RULC_q")
try(sqlDrop(conn, "RULC_A_q", errors = TRUE), silent = FALSE)
sqlSave(conn,RULC_q, tablename= "RULC_A_q")
try(sqlDrop(conn, "RULC_A", errors = TRUE), silent = FALSE)
sqlSave(conn,RULC_A, tablename= "RULC_A")
try(sqlDrop(conn, "prod_q", errors = TRUE), silent = FALSE)
sqlSave(conn,prod_q, tablename= "prod_q")
try(sqlDrop(conn, "prod_A_det", errors = TRUE), silent = FALSE)
sqlSave(conn,prod_A_det, tablename= "prod_A_det")
try(sqlDrop(conn, "TOT_PROD_q", errors = TRUE), silent = FALSE)
sqlSave(conn,TOT_PROD_q, tablename= "TOT_PROD_q")
close(conn)





head(TOT_RULC_q)

TOT_RULC_A$LETO <- as.numeric(as.character(TOT_RULC_A$LETO))
A<-TOT_RULC_A%>%filter(LETO >= "1995"& Kazalnik %in% c("BDP_tc", "BDP_sc_2010","zaposlenost_osebe", "zaposleni_osebe", "sredstva"))%>%select(LETO, Kazalnik, Vrednost)%>%
  pivot_wider( names_from = Kazalnik, values_from = Vrednost)%>%select(LETO,sredstva,BDP_sc_2010, BDP_tc, zaposlenost_osebe, zaposleni_osebe)

A2<-TOT_RULC_A%>%filter(LETO >= "1996"& Kazalnik %in% c("BDP_tc", "BDP_sc_2010","zaposlenost_osebe", "zaposleni_osebe", "sredstva", "PROD_real", "PROD_nom", "RULC", "NULC"))%>%select(LETO, Kazalnik, YOY)%>%
  pivot_wider( names_from = Kazalnik, values_from = YOY)%>%select(LETO,sredstva,BDP_sc_2010, BDP_tc, zaposlenost_osebe, zaposleni_osebe, PROD_real, PROD_nom, RULC, NULC)


last_q <- max(TOT_RULC_q$?ETRTLETJE)

B<-TOT_RULC_q%>%filter(?ETRTLETJE == last_q & Kazalnik %in% c("BDP_tc", "BDP_sc_2010","zaposlenost_osebe", "zaposleni_osebe", "sredstva", "PROD_real", "PROD_nom", "RULC", "NULC") & season_adj=="Originalni podatki")%>%select(?ETRTLETJE,Kazalnik, YOY)%>%
  pivot_wider(-season_adj, names_from = Kazalnik, values_from = YOY)%>%select("LETO"=?ETRTLETJE,sredstva,BDP_sc_2010, BDP_tc, zaposlenost_osebe, zaposleni_osebe, PROD_real, PROD_nom, RULC, NULC)
B2 <- bind_rows(A2, B)
tibble::as_tibble(B)
library("xlsx")
write.xlsx(A, file = "M:/Konkurencnost/Napoved/Tabela.xlsx")





################################ SLIKE  #############################

last_q <- max(RULC_q$?ETRTLETJE)
#RULC_q$CETRTLETJE<-format(as.POSIXct(RULC_q$CETRTLETJE, format="%m/%d/%Y %H:%M:%S"),format="%Y-%m-%d")format='%m/%d/%Y'
RULC_q$?ETRTLETJE <- as.yearqtr(RULC_q$?ETRTLETJE, format = "%YQ%q")


# podatki_SKD_q <- RULC_q
# save(podatki_SKD_q, file = "RULC_SURS_SKD_Q.Rdata")


agregati<-c("Neposlovni", "Poslovni", "Menjalni", "Nemenjalni")

####### SLIKE
loadfonts(device="win",quiet = T) #za Arial pisavo
#font_import()
fonts()

# rdeca	#9E001A ; svetlo roza   #D99694; svetlo siva	#949494;  temno siva	#535353;
# nafta #3F8B94; svetlo zelena  #9FCDAB; bela	#FFFFFF; crna	#000000
colourUMAR <- c("#000000", "#9E001A", "#949494", "#DBDBDB", "#44546A", "#A6A6A6", "#54A4A3", "#BBD2B0", "#6F3B66")   #barvna paleta

theme_UMAR <- function(){ 
  
  theme_bw () + theme(    #replace elements you want to change
    text=element_text(family="Arial", size=10),
    plot.title = element_text(face = "bold",hjust = 0,vjust = 2, size=10, family="Arial"), 
    plot.caption = element_text(hjust = 0, size=9, family="Arial"),
    axis.text.x = element_text(angle=90, hjust=1, vjust=0.5, color="black",size=10, family="Arial"),
    axis.text.y = element_text(hjust=1, vjust=0.5, color="black",size=10,family="Arial"),
    axis.title.x  = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "top",
    legend.justification="left",
    legend.title=element_blank(), # Legend title
    legend.text=element_text(size=10),
    legend.key.size = unit(0.3, "cm"),
    legend.key.width = unit(0.3,"cm"),
    #panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.border = element_rect(colour = "grey", fill=NA, size=0.5))
  
}
      

# RULC Zadnji podatek YOY
p1<- RULC_q %>% filter(?ETRTLETJE == last_q&!SKD%in%agregati&!SKD=="BCDE"&Kazalnik=="RULC")%>%
   ggplot(aes(x=SKD, y=YOY, fill=factor(ifelse(SKD == "SKUPAJ", "Highlighted", "Normal"))))+
  geom_col()+
  scale_fill_manual(values=c("#9E001A", "#535353"))+
  labs(title= paste(" Realni stro?ki dela na enoto proizvoda (RULC),", last_q),
       caption="Vir: SURS; preracuni Umar.")+
  ylab("Medletna sprememba, v %")+
  theme_UMAR() +
  theme(legend.position = "none")
  
p1

# RULC Zadnji podatek YOY
p2<- RULC_q %>% filter(?ETRTLETJE == last_q&!SKD%in%agregati&!SKD=="BCDE"&Kazalnik%in%c("PROD_real"))%>%
  ggplot(aes(x=SKD, y=YOY, fill=factor(ifelse(SKD == "SKUPAJ", "Highlighted", "Normal"))))+
  geom_col()+
  scale_fill_manual(values=c("#9E001A", "#535353"))+
  labs(title= paste(" Produktivnost dela (realno),", last_q),
       caption="Vir: SURS; preracuni Umar.")+
  ylab("Medletna sprememba, v %")+
  theme_UMAR() +
  theme(legend.position = "none")

p2

# RULC Zadnji podatek YOY
p3<- RULC_q %>% filter(?ETRTLETJE == last_q&!SKD%in%agregati&!SKD=="BCDE"&Kazalnik%in%c("stroski_dela_zap"))%>%
  ggplot(aes(x=SKD, y=YOY, fill=factor(ifelse(SKD == "SKUPAJ", "Highlighted", "Normal"))))+
  geom_col()+
  scale_fill_manual(values=c("#9E001A", "#535353"))+
  labs(title= paste(" Stro?ki dela na zaposlenega (nominalno),", last_q),
       caption="Vir: SURS; preracuni Umar.")+
  ylab("Medletna sprememba, v %")+
  theme_UMAR() +
  theme(legend.position = "none")

p3

# RULC, indeks 2005, 4-?etrtletne drse?e sredine
p4a<- RULC_q %>% filter((?ETRTLETJE >= "2008 Q1")&(SKD%in% c("SKUPAJ", "Nemenjalni","Menjalni"))&Kazalnik=="RULC")%>%select("Indeks2005_4cds", "SKD","Kazalnik","?ETRTLETJE")%>%
                ggplot(aes(x=?ETRTLETJE, y=Indeks2005_4cds, color=SKD, group=SKD))+
  geom_line(size=0.7)+
  scale_color_manual(values=c("#535353","#D99694","#9E001A"))+
  labs(title= " Realni stro?ki dela na enoto proizvoda (RULC)",
       caption="Vir: SURS; preracuni Umar.")+
  ylab("Indeks 2005=100, 4-?etrtletne drse?e sredine")+
  theme_UMAR()
 
p4a

# RULC, indeks 2005, 4-?etrtletne drse?e sredine
p4<- RULC_q %>% filter((?ETRTLETJE >= 2015)&(SKD%in% c("SKUPAJ", "Nemenjalni","Menjalni"))&Kazalnik=="RULC")%>%select("YOY", "SKD","Kazalnik","?ETRTLETJE")%>%
  ggplot(aes(x=?ETRTLETJE, y=YOY, color=SKD, group=SKD))+
  geom_line(size=0.7)+
  scale_color_manual(values=c("#535353","#D99694","#9E001A"))+
  scale_y_continuous(expand = c(0,0), limits=range(-15,20))+
  labs(title= " Realni stro?ki dela na enoto proizvoda (RULC)",
       caption="Vir: SURS; preracuni Umar.")+
  ylab("Medletna sprememba, v %")+
  theme_UMAR()

p4


# Produktivnost dela, realno, YOY
p5<- RULC_q %>% filter((?ETRTLETJE >= 2015)&(SKD%in% c("SKUPAJ", "Nemenjalni","Menjalni"))&Kazalnik=="PROD_real")%>%select("YOY", "SKD","Kazalnik","?ETRTLETJE")%>%
  ggplot(aes(x=?ETRTLETJE, y=YOY, color=SKD, group=SKD))+
  geom_line()+
  scale_color_manual(values=c("#535353","#D99694","#9E001A"))+
  scale_y_continuous(expand = c(0,0), limits=range(-15,25))+
  labs(title= " Produktivnost dela (realno)",
       caption="Vir: SURS; preracuni Umar.")+
  ylab("Medletna sprememba, v %")+
  theme_UMAR()+
  theme(legend.position = "top")

p5

# Produktivnost dela, realno, YOY
p6<- RULC_q %>% filter((?ETRTLETJE >= 2015)&(SKD%in% c("SKUPAJ", "Nemenjalni","Menjalni"))&Kazalnik=="stroski_dela_zap")%>%select("YOY", "SKD","Kazalnik","?ETRTLETJE")%>%
  ggplot(aes(x=?ETRTLETJE, y=YOY, color=SKD, group=SKD))+
  geom_line()+
  scale_color_manual(values=c("#535353","#D99694","#9E001A"))+
   scale_y_continuous(expand = c(0,0), limits=range(-10,15))+
  labs(title= " Stro?ki dela na zaposlenega (nominalno)",
       caption="Vir: SURS; preracuni Umar.")+
  ylab("Medletna sprememba, v %")+
  theme_UMAR()+
  theme(legend.position = "top")

p6

grid.arrange(p1, p4, p2, p5, p3, p6, ncol=2)


# Produktivnost dela, realno, YOY
p7<- TOT_RULC_q %>% filter((?ETRTLETJE >= 2019)&(season_adj!="Originalni podatki")&Kazalnik %in% c("stroski_dela_wh","PROD_nom_wh", "PROD_real_wh" ))%>%select("Indeks2005", "Kazalnik","?ETRTLETJE")%>%
  ggplot(aes(x=?ETRTLETJE, y=Indeks2005, color=Kazalnik, group=Kazalnik))+
  geom_line()+
  scale_color_manual(values=c("#535353","#D99694","#9E001A"))+
  scale_y_continuous(expand = c(0,0), limits=range(100,220))+
  labs(title= " Stro?ki dela na zaposlenega (nominalno)",
       caption="Vir: SURS; preracuni Umar.")+
  ylab("Medletna sprememba, v %")+
  theme_UMAR()+
  theme(legend.position = "top")

p7

grid.arrange(p1, p4, p2, p5, p3, p6, ncol=2)

#########################################################################################


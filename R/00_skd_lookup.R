################################################################################
#               Pomožne skripte, ki se kličejo iz drugih skript,               #
#                  tega ni treba nikoli samega zalaufat                        #
################################################################################
library(dplyr)
################################################################################
#                Šifrant NACE za eurostatove  + malo naših - skripta 02        #
################################################################################
nace_codes <- eurostat::get_eurostat("nama_10_a64",
                                     cache = FALSE,
                           filters = list(
                             geo = "SI",
                             time = 2020,
                             na_item = "B1G", # Value added, gross
                             unit = "CLV10_MEUR"),
                           type = "both")|> # Current prices, million euro
  select(nace_r2) |>
  dplyr::rename(nace_descr = nace_r2) |>
  mutate(nace_r2 =  names(nace_descr)) |>
  mutate(nace_level = ifelse(grepl("^[A-Z]{1}$", nace_r2), "level_1",
                             ifelse(grepl("^[A-Z]{1}[0-9]{2}$", nace_r2), "level_2", "aggr"))) |>
  relocate(nace_r2, nace_level, nace_descr)

# sektorski agregati za eurostat A64
sectors_eurostat_a64 <- list(
  Menjalni = c("A" , "B-E",  "G-I", "J"),
  Nemenjalni = c("F" , "K" ,  "L" , "M_N", "O-Q", "R-U"),
  Poslovni = c("B-E", "F" ,"G-I", "J", "K", "M_N"),
  Neposlovni = c("A" , "L" , "O-Q", "R-U"),
  `High tech manuf` =  c("C21" , "C26"),
  `Med-high tech manuf` = c("C20" , "C27", "C28", "C29_C30"),
  `Med-low tech manuf` = c("C19", "C22_C23", "C24_C25"),
  `Low tech manuf` = c("C10-C12",  "C13-C15", "C16-C18", "C31-C32"),
  `Knowledge market services` = c("J",  "M"),
  `Rest market services` = c("G-I", "K", "N"),
  `Tržni` = c("G-I", "J", "K", "L", "M_N"),
  `Ostali` = c( "A", "B", "D", "E", "O-Q", "R-U"),
  `Energetsko intenzivni` = c("C17", "C20", "C23", "C24"),
  `Predelovaln neenergetski` = c("C10-C12", "C13-C15", "C16", "C18",
                           "C19", "C21", "C22", "C25", "C26",
                           "C27", "C28", "C29-C30", "C31-C32", "C33"))
# sektorski agregati za eurostat A10
sectors_eurostat_a10 <- list(
  Menjalni = c("A" , "B-E",  "G-I", "J"),
  Nemenjalni = c("F" , "K" ,  "L" , "M_N", "O-Q", "R-U"),
  Poslovni = c("B-E", "F" ,"G-I", "J", "K", "M_N"),
  Neposlovni = c("A" , "L" , "O-Q", "R-U"),
  Tržni = c("G-I", "J", "K", "L", "M_N"),
  Ostali =  c( "A", "BDE", "O-Q", "R-U"))

# sektorski agregati za SURS - detaljni
sectors_surs_det <- list(
  Menjalni = c("A", "B", "C", "D", "E", "G", "H", "I" , "J" ),
  Nemenjalni = c("F" , "K" ,  "L" , "M", "N", "O", "P", "Q", "R", "S", "T"),
  Poslovni = c( "B", "C", "D", "E" , "F" ,  "G", "H", "I", "J" , "K", "M", "N" ),
  Neposlovni = c("A" , "L" , "O", "P", "Q", "R", "S", "T"),
  `High tech manuf` = c("21" , "26"),
  `Med-high tech manuf` = c("20" , "27", "28", "29", "30"),
  `Med-low tech manuf` = c("19", "22", "23", "24", "25"),
  `Low tech manuf` = c("10", "11", "12",  "13","14",  "15",
                     "16", "17", "18", "31", "32"),
  `Knowledge market services` = c("J",  "M"),
  `Rest market services` = c("G", "H", "I", "K", "N"),
  `Tržni` = c("G", "H", "I", "J", "K", "L", "M", "N"),
  `Ostali` = c( "A", "B", "D", "E", "O", "P", "Q", "R", "S", "T"),
  `Energetsko intenzivni` = c("17", "20", "23", "24"),
  `Predelovalni neenergetski` = c("10", "11", "12", "13", "14", "15", "16", "18",
                           "19", "21", "22", "25", "26",
                           "27", "28", "29", "30", "31", "32", "33"))

new_aggregations <- data.frame(
  nace_r2 = names(sectors_eurostat_a64),
  nace_level = "aggr",
  nace_descr = sapply(sectors_eurostat_a64, function(vec) paste(vec, collapse = ", ")),
  stringsAsFactors = FALSE, row.names = NULL
)

nace_codes <- nace_codes |>
  bind_rows(new_aggregations)

# saveRDS(nace_codes, "data/nace_codes.rds")

nace_codes10 <- eurostat::get_eurostat("nama_10_a10",
                                       cache = FALSE,
                                     filters = list(
                                       geo = "SI",
                                       time = 2020,
                                       na_item = "B1G", # Value added, gross
                                       unit = "CLV10_MEUR"),
                                     type = "both")|> # Current prices, million euro
  select(nace_r2) |>
  dplyr::rename(nace_descr = nace_r2) |>
  mutate(nace_r2 =  names(nace_descr)) |>
  relocate(nace_r2, nace_descr) |>
  bind_rows(new_aggregations[c(1:4, 11, 12),c(1,3)])

# saveRDS(nace_codes10, "data/nace_codes10.rds")


################################################################################
#                Šifrant SKD za SURS  - skripta 03                             #
################################################################################
dejanvost_lookup <- c(
  "A Kmetijstvo, lov, gozdarstvo, ribištvo" = "A",
  "BCDE Rudarstvo, predelovalne dejavnosti, oskrba z elektriko in vodo, ravnanje z odplakami, saniranje okolja" = "BCDE",
  "..od tega: C Predelovalne dejavnosti" = "C",
  "F Gradbeništvo" = "F",
  "GHI Trgovina in popravila vozil, promet in skladiščenje, gostinstvo" = "GHI",
  "J Informacijske in komunikacijske dejavnosti" = "J",
  "K Finančne in zavarovalniške dejavnosti" = "K",
  "L Poslovanje z nepremičninami" = "L",
  "MN Strokovne, znanstvene, tehnične dejavnosti in druge raznovrstne poslovne dejavnosti" = "MN",
  "OPQ Uprava in obramba, izobraževanje, zdravstvo in socialno varstvo" = "OPQ",
  "RST Druge storitvene dejavnosti" = "RST",
  "Dejavnost - SKUPAJ" = "SKUPAJ",
  "Dodana vrednost, skupaj" = "SKUPAJ"
)

transakcije_lookup <- c(
  "..Sredstva za zaposlene A Kmetijstvo, lov, gozdarstvo, ribištvo" = "A",
  "..Sredstva za zaposlene BCDE Rudarstvo, predelovalne dejavnosti, oskrba z elektriko in vodo, ravnanje z odplakami, saniranje okolja" = "BCDE",
  "....od tega: sredstva za zaposlene C Predelovalne dejavnosti" = "C",
  "..Sredstva za zaposlene F Gradbeništvo" = "F",
  "..Sredstva za zaposlene GHI Trgovina in popravila vozil, promet in skladiščenje, gostinstvo" = "GHI",
  "..Sredstva za zaposlene J Informacijske in komunikacijske dejavnosti" = "J",
  "..Sredstva za zaposlene K Finančne in zavarovalniške dejavnosti" = "K",
  "..Sredstva za zaposlene L Poslovanje z nepremičninami" = "L",
  "..Sredstva za zaposlene MN Strokovne, znanstvene, tehnične dejavnosti in druge raznovrstne poslovne dejavnosti" = "MN",
  "..Sredstva za zaposlene OPQ Uprava in obramba, izobraževanje, zdravstvo in socialno varstvo" = "OPQ",
  "..Sredstva za zaposlene RST Druge storitvene dejavnosti" = "RST",
  "Sredstva za zaposlene" = "SKUPAJ"
)

################################################################################
#                Šifrant za imena spremenljivk v dolgih tabelah..              #
################################################################################

indicator_descriptions <- c(
  "CP_MEUR_B1GQ" = "BDP tekoče cene",
  "CLV10_MEUR_B1GQ" = "BDP stalne cene 2010",
  "CP_MEUR_D1" = "sredstva za zaposlene",
  "CP_MEUR_D11" = "plače",
  "CP_MEUR_D12" = "socialni prispevki",
  "THS_HW_EMP_DC" = "zaposlenost - delovne ure",
  "EMP_HW" = "zaposlenost - delovne ure",
  "THS_HW_SAL_DC" = "zaposleni - delovne ure",
  "SAL_HW" = "zaposleni - delovne ure",
  "THS_HW_SELF_DC" = "samozaposleni - delovne ure",
  "THS_PER_EMP_DC" = "zaposlenost",
  "EMP_PER" = "zaposlenost",
  "THS_PER_SAL_DC" = "zaposleni",
  "SAL_PER" = "zaposleni",
  "THS_PER_SELF_DC" = "samozaposleni",
  "deflator_BDP" = "deflator BDP",
  "COMP_nom" = "sredstva za zaposlene - nom.",
  "COMP_SAL_nom" = "plače - nom.",
  "COMP_CONTR_nom" = "socialni prispevki - nom.",
  "COMP_real" = "sredstva za zaposlene - real.",
  "COMP_nom_EMP" = "sredstva na zaposlenega - nom.",
  "COMP_nom_HW" = "sredstva na delovno uro - nom.",
  "COMP_real_EMP" = "sredstva na zaposlenega - real",
  "COMP_real_HW" = "sredstva na delovno uro - real.",
  "PROD_nom" = "produktivnost",
  "PROD_nom_EMP" = "produktivnost",
  "PROD_real" = "realna produktivnost",
  "PROD_real_EMP" = "realna produktivnost",
  "PROD_real_hw" = "realna produktivnost na delovno uro",
  "PROD_real_HW" = "realna produktivnost na delovno uro",
  "PROD_nom_HW" = "nominalna produktivnost na delovno uro",
  "RULC" = "Real unit labour cost",
  "NULC" = "Nominal unit labour cost",
  "CP_MEUR_B1G" = "DV tekoče cene",
  "VA_nom" = "DV tekoče cene",
  "GDP_real" = "BDP stalne cene 2010",
  "GDP_nom" = "BDP tekoče cene",
  "VA_real" = "DV stalne cene 2010",
  "CLV10_MEUR_B1G" = "DV stalne cene 2010",
  "deflator_VA" = "deflator DV",
  "deflator_GDP" = "deflator BDP")

MENJALNI_sektor <- c("A" , "B-E",  "G-I", "J")
NEMENJALNI_sektor <- c("F" , "K" ,  "L" , "M_N", "O-Q", "R-U")
POSLOVNI_sektor <- c("B-E", "F" ,"G-I", "J", "K", "M_N")
NEPOSLOVNI_sektor <- c("A" , "L" , "O-Q", "R-U")
HIGH_TECH_MANUF <- c("C21" , "C26")
MED_HIGH_TECH_MANUF <- c("C20" , "C27", "C28", "C29_C30")
MED_LOW_TECH_MANUF <- c("C19", "C22_C23", "C24_C25")
LOW_TECH_MANUF <- c("C10-C12",  "C13-C15", "C16-C18", "C31-C32")
KNOWLEDGE_MKT_SERV <- c("J",  "M")
REST_MKT_SERV <- c("G-I", "K", "N")
TRZNE <- c("G-I", "J", "K", "L", "M_N")
OSTALE <- c( "A", "B", "D", "E", "O-Q", "R-U")
MENJALNI_sektor_surs <- c("A", "BCDE", "GHI" , "J" )
NEMENJALNI_sektor_surs <- c("F" , "K" ,  "L" , "MN", "OPQ", "RST")
POSLOVNI_sektor_surs <- c( "BCDE" , "F" ,  "GHI", "J" , "K", "MN" )
NEPOSLOVNI_sektor_surs <- c("A" , "L" , "OPQ", "RST")
TRZNE_surs <- c("GHI", "J", "K", "L", "MN")
OSTALE_surs <- c( "A", "BDE", "OPQ", "RST")
sectors <- list(
  MENJALNI_sektor = MENJALNI_sektor,
  NEMENJALNI_sektor = NEMENJALNI_sektor,
  POSLOVNI_sektor = POSLOVNI_sektor,
  NEPOSLOVNI_sektor = NEPOSLOVNI_sektor,
  HIGH_TECH_MANUF = HIGH_TECH_MANUF,
  MED_HIGH_TECH_MANUF = MED_HIGH_TECH_MANUF,
  MED_LOW_TECH_MANUF = MED_LOW_TECH_MANUF,
  LOW_TECH_MANUF = LOW_TECH_MANUF,
  KNOWLEDGE_MKT_SERV = KNOWLEDGE_MKT_SERV,
  REST_MKT_SERV = REST_MKT_SERV,
  TRZNE = TRZNE,
  OSTALE = OSTALE
)
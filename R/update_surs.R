###############################################################################
## produktivnost update tables
###############################################################################

###############################################################################
## Preliminaries
###############################################################################
home <- "\\\\192.168.38.7\\public$\\Avtomatizacija\\umar_produktivnost\\"
setwd(home)
library(gmailr)
library(kableExtra)
options(gargle_oauth_email = TRUE)
gm_auth_configure(path ="data/credentials.json")
gm_auth(email = TRUE, cache = ".secret")

email_list <- c("maja.zaloznik@gmail.com",
                "maja.zaloznik@gov.si",
                "Rotija.Kmet-Zupancic@gov.si",
                "urska.cede@gov.si",
                "peter_wostner@gov.si")
source("R/helper_functions.R")

###############################################################################
## preveri velikosti tabel
###############################################################################
# Database connection details
con <- DBI::dbConnect(RPostgres::Postgres(),
                      dbname = "produktivnost",
                      host = "localhost",
                      port = 5432,
                      user = "postgres",
                      password = Sys.getenv("PG_PG_PSW"))
DBI::dbExecute(con, "set search_path to produktivnost")

initial_counts <- get_produktivnost_table_row_counts(con)

###############################################################################
## poženi skripte za eurostat tabele
###############################################################################
getwd()
source("R\\05_SURS_RULC_strukt_cetrtletno.R", encoding = 'UTF-8')
source("R\\06_SURS_RULC_strukt_letno.R", encoding = 'UTF-8')

###############################################################################
## preveri novo velikosti tabel
###############################################################################
new_counts <- get_produktivnost_table_row_counts(con)

diff <- initial_counts  |>
  full_join(new_counts, by = "tabela", suffix = c("_prej", "_zdej")) |>
  filter(vrstice_prej != vrstice_zdej) |>
  mutate(sprememba = vrstice_zdej - vrstice_prej) |>
  select(tabela, vrstice_prej, vrstice_zdej, sprememba)


# prepare email body
body <- email_produktivnost_update_body(diff)

# email changes to list of recipients
email_produktivnost_changes(body, recipient = email_list)

###############################################################################
## Wrap up
###############################################################################
DBI::dbDisconnect(con)
cat("script completed")
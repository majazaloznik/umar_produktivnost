# function to get current row numbers
get_produktivnost_table_row_counts <- function(con) {
  tables <- DBI::dbListTables(con)
  row_counts <- purrr::map_dfr(tables, ~ {
    table <- dplyr::tbl(con,  .x)
    table  |>
      dplyr::summarize(tabela = .x, vrstice = n()) |>
      dplyr::collect()}) |>
    dplyr::arrange(tabela)
}

# sestavi telo emaila
email_produktivnost_update_body <- function (diff) {
  if (nrow(diff) > 0) {
    html_table <- kable(diff, "html") |>
      kable_styling(
        bootstrap_options = c("striped", "hover", "condensed"),
        full_width = FALSE,
        font_size = 12)

    email_body <- "To je avtomatsko generirano sporočilo o posodobitvah na bazi produktivost.<br><br>"
    email_body <- paste0(email_body, "<b>Naslednje tabele imajo novo število vrstic:</b><br><br>",
                           html_table, "<br><br>")
    email_body <- paste0(email_body, "<br>Tvoj Umar Data Bot &#129302;")
    email_body} else {
      NULL}
}

# pošlje email
email_produktivnost_changes <- function (body, recipient) {
  if (!is.null(body)) {
    text_msg <- gmailr::gm_mime() %>% gmailr::gm_bcc(recipient) %>%
      gmailr::gm_subject("Spremembe na bazi produktivnost") %>%
      gmailr::gm_html_body(body)
    gmailr::gm_send_message(text_msg)
  }
}



################################################################################
#                        ANNUAL aggregation and chainlinking                   #
################################################################################

#' Chain link annual volume data with 2010 reference year
#'
#' This functions is used to calculate the chain linked volumes with 2010 as the
#' selected reference year from current price and previous year's price
#'
#' @param df dataframe with the quarters in column Q, current prices in column
#' CP_MEUR and previous year's prices in a column named PYP_MEUR.
#'
#' @return dataframe with extra column for CLV-s called CLV10_MEUR
chain_linking_annual_2010 <- function(df) {
  df |>
    mutate(growth_factor = PYP_MEUR/lag(CP_MEUR)) |>
    mutate(
      year = if(inherits(time, "Date")) {lubridate::year(time)} else { time},
      growth_factor = PYP_MEUR / lag(CP_MEUR),
      growth_factor = ifelse(is.na(growth_factor) | growth_factor == 0, 1, growth_factor),
      # Forward chaining (for years > ref_year)
      chain_forward = cumprod(growth_factor),
      # Backward chaining (for years < ref_year)
      chain_backward = rev(cumprod(rev(1/growth_factor))),
      # Combine forward and backward chains
      has_2010 = any(year == 2010),
      chained_index = if_else(
        has_2010,
        case_when(
          year == 2010 ~ 1,
          year > 2010 ~ chain_forward / chain_forward[year == 2010][1],
          year < 2010 ~ chain_backward[row_number() + 1] / chain_backward[year == 2010 + 1][1]
        ),
        NA_real_  # If 2010 doesn't exist, all values are NA
      ),
      CLV10_MEUR = if_else(
        has_2010 & CP_MEUR != 0,
        CP_MEUR[year == 2010][1] * chained_index,
        NA_real_
      )) |>
    select(-year, -chain_forward, -chain_backward, -growth_factor,
           -chained_index, -has_2010)
}


#' Aggregate long table over geography
#'
#' Takes a long dataframe and a vector of geo names which need to be added
#' up. Should technically summarise along all relevant groups, but this has
#' not been tested to necessarily work with anything you throw at it, so be
#' careful. Works for appropriately named data frames where values are in a
#' values column and geo in a geo column. If data for a country is missing
#' the whole aggregation will be missing.
#'
#' @param df long df with geo an values columns at a minimum
#' @param group vector of geo names e.g. "AT", "BE"..
#' @param group_name name of the group to use in the aggregated column's geo table
#'
#' @return dataframe with single geo and same number of other relevant groups,
#' making them now rows. This is generally then row_binded to the original df
aggregate_geo <- function(df, group, group_name) {
  df  |>
    filter(geo %in% group) |>
    group_by(across(-c(geo, values))) |>
    summarise(values = sum(values), .groups = 'drop') |>
    mutate(geo = group_name)
}

#' Aggregate long table over nace groupings
#'
#' Takes a long dataframe and a vector of nace names which need to be added
#' up. Should technically summarise along all relevant groups, but this has
#' not been tested to necessarily work with anything you throw at it, so be
#' careful. Works for appropriately named data frames where values are in a
#' values column and geo in a geo column. Tested on annual data so far
#' we'll see how it goes with quarterly.
#'
#' @param df long df with nace_r2 an values columns at a minimum
#' @param group vector of nace_r2 names e.g. "A".."C12" depending on the schema used
#' @param group_name name of the group to use in the aggregated column's nace_r2 table
#'
#' @return dataframe with single nace_r2 and same number of other relevant groups,
#' making them now rows. This is generally then row_binded to the original df
aggregate_skd <- function(df, group, group_name) {
  df %>%
    filter(nace_r2 %in% group) %>%
    group_by(across(-any_of(c("nace_r2", "values", "DEJAVNOST", "aggr")))) %>%
    summarise(values = sum(values), .groups = 'drop') %>%
    mutate(nace_r2 = group_name)
}

#' Calulate annual CLV with 2010
#'
#' takes a long dataframe which must have CP_MEUR and PYP_MEUR values in the unit
#' column. groups by nace_r2 and/or geo, so is agnostic.
#'
#' Usually the input table has CLV10_MEUR data alreay, which this funciton
#' overwrites. This is correct, because the input to this function is
#' normally the output of aggregation - and CLVs should not be aggregated. But
#' just be aware.
#'
#' If any group has not PYP prices then the CLV-s are set to NA. Which are then
#' deleted as well.
#'
#' NB: this only works on units used so far which must be listed in the funciton
#' below. This means that if you want to have other indicators in the table
#' in addition to CP and CP_MPPS, you need to add them below, because otherwise
#' this funciton will remove them.
#'
#' @param df long table with unit and values columns. unit column must have
#' CP_MEUR and PYP_MEUR values for the CLV to be calculated
#'
#' @return returns a long table with CLV10_MEUR. you probably want to then
#' remove PYP unless you wanted to keep it for something else
#'
calculate_clv_annual  <- function(df){
  df |>
    pivot_wider(names_from = unit, values_from = values) |>
    group_by(across(any_of(c("nace_r2", "geo", "na_item")))) |>
    chain_linking_annual_2010() |>
    mutate(CLV10_MEUR = if_else(rep(all(is.na(PYP_MEUR)), n()), NA,CLV10_MEUR)) |>
    pivot_longer(any_of(c("CP_MPPS_EU27_2020", "CP_MEUR", "PYP_MEUR", "CLV10_MEUR")), names_to = "unit", values_to = "values") |>
    filter(!is.na(values))
}


#' Aggregate by geo and calculate CLV for 2010 annual data
#'
#' uses aggregate_geo and calculate_clv_annual, so check what all
#' applies there
#'
#' @param data long df with geo an values columns at a minimum
#' @param group vector of geo names e.g. "AT", "BE"..
#' @param group_name name of the group to use in the aggregated column's geo table
#'
#' @return returns a long table with CLV10_MEUR. you probably want to then
#' remove PYP unless you wanted to keep it for something else
aggregate_geo_and_calculate_clv_annual <- function(data, group, group_name) {
  aggregated_data <- aggregate_geo(data, group, group_name)
  calculate_clv_annual(aggregated_data)
}

#' Aggregate by nace_r2 and calculate CLV for 2010 annual data
#'
#' uses aggregate_skd and calculate_clv_annual, so check what all
#' applies there
#'
#' @param data long df with nace_r2 an values columns at a minimum
#' @param group vector of nace_r2 names e.g. "A".."C12" depending on the schema used
#' @param group_name name of the group to use in the aggregated column's nace_r2 table
#'
#' @return returns a long table with CLV10_MEUR. you probably want to then
#' remove PYP unless you wanted to keep it for something else
aggregate_skd_and_calculate_clv_annual <- function(data, group, group_name) {
  aggregated_data <- aggregate_skd(data, group, group_name)
  calculate_clv_annual(aggregated_data)
}


#' Subtract C from BCDE
#'
#' Analogously with the aggregate skd functions, sometimes you need to
#' disaggregate C from the BCDE group. requires that the nace_r2 column
#' inlcudes both a C value and a B-E one or sth similar, which you enter
#' as the third argument to the function
#'
#' @param data long df with nace_r2 an values columns at a minimum
#' @param colname name of column where nace_r2 labels are
#' @param label name of aggregate group so either BCDE or B-E or sth
#'
#' @return returns a long table with CLV10_MEUR. you probably want to then
#' remove PYP unless you wanted to keep it for something else
disagregate_bde_annual <-  function(df, colname, label) {
  df  |>
    filter(!!sym(colname)  %in% c(label, "C")) |>
    group_by(across(any_of(c("unit", "na_item", "geo", "time")))) |>
    pivot_wider(names_from = !!sym(colname), values_from = values) |>
    mutate(BDE = !!sym(label) - C, .keep = "unused") |>
    filter(unit != "CLV10_MEUR") |>
    pivot_longer(BDE, values_to = "values", names_to = "nace_r2")
}

#' SUbrtract C from whatever it needs to and calculate annual CLVs
#'
#' uses disagregate_bde_annual and calculate_clv_annual, so check what all
#' applies there
#'
#' @param data long df with nace_r2 an values columns at a minimum
#' @param colname name of column where nace_r2 labels are
#' @param label name of aggregate group so either BCDE or B-E or sth
#'
#' @return returns a long table with CLV10_MEUR. you probably want to then
#' remove PYP unless you wanted to keep it for something else
disagregate_bde_and_calculate_clv_annual <- function(data, colname, label) {
  disaggregated_data <- disagregate_bde_annual(data, colname, label)
  calculate_clv_annual(disaggregated_data)
}


################################################################################
#                     QUARTERLY aggregation and chainlinking                   #
################################################################################

#' Chain link quarterly volume data using annual overlap to 2010
#'
#' This functions is used to calculate the chain linked volumes to a
#' selected reference year from current price and previous year's price
#' data using the annual overlap method, which both SURS and Eurostat
#' use. The reference year is 2010. Should also work if you pass a
#' dataframe grouped by geo.
#'
#'
#' @param df dataframe with the quarters in column Q, current prices in column
#' CP and previous year's prices in a column named PYP.
#'
#' @return dataframe with extra column for CLV-s
chain_linking_quarterly_2010 <- function(df) {
  groupz <- groups(df)
  links <- df |>
    arrange(time) |>
    mutate(year = substr(time, 1,4)) |>
    group_by(year, .add = TRUE) |>
    mutate(CP_annual_ave = mean(CP_MEUR),
           PYP_annual_ave = mean(PYP_MEUR)) |>
    group_by(!!!groupz) |>
    mutate(PYP_annual_ave = ifelse(is.na(PYP_annual_ave) , CP_annual_ave,
                                   PYP_annual_ave),
           link = ifelse(year == min(year),
                         ifelse(is.na(PYP_MEUR), CP_MEUR/PYP_annual_ave,
                                PYP_MEUR/PYP_annual_ave),
                         PYP_MEUR/lag(CP_annual_ave, 4)))
 x <-  links |>
    group_by(year, .add = TRUE) |>
    summarise(annual_average = mean(link),
              year = first(year)) |>
    ungroup(year) |>
    mutate(cumprod_annual_average = cumprod(annual_average)) |>
    right_join(links) |>
    relocate(annual_average, cumprod_annual_average, .after = link) |>
    mutate(chained_1995 = ifelse(year == min(year),
                                 link,
                                 link * lag(cumprod_annual_average, 4))) |>
    mutate(chained_2010 = chained_1995 / mean(chained_1995[year == 2010])) |>
    mutate(CLV10_MEUR = chained_2010 * mean(CP_annual_ave[year == 2010])) |>
    select(time, !!!groupz, any_of(c("CP_MEUR", "PYP_MEUR", "CLV10_MEUR")))
}



chain_linking_quarterly_2010_MNAC <- function(df) {
  groupz <- groups(df)
  links <- df |>
    arrange(time) |>
    mutate(year = substr(time, 1,4)) |>
    group_by(year, .add = TRUE) |>
    mutate(CP_annual_ave = mean(CP_MNAC),
           PYP_annual_ave = mean(PYP_MNAC)) |>
    group_by(!!!groupz) |>
    mutate(PYP_annual_ave = ifelse(is.na(PYP_annual_ave) , CP_annual_ave,
                                   PYP_annual_ave),
           link = ifelse(year == min(year),
                         ifelse(is.na(PYP_MNAC), CP_MNAC/PYP_annual_ave,
                                PYP_MNAC/PYP_annual_ave),
                         PYP_MNAC/lag(CP_annual_ave, 4)))
  links |>
    group_by(year, .add = TRUE) |>
    summarise(annual_average = mean(link),
              year = first(year)) |>
    ungroup(year) |>
    mutate(cumprod_annual_average = cumprod(annual_average)) |>
    right_join(links) |>
    relocate(annual_average, cumprod_annual_average, .after = link) |>
    mutate(chained_1995 = ifelse(year == min(year),
                                 link,
                                 link * lag(cumprod_annual_average, 4))) |>
    mutate(chained_2010 = chained_1995 / mean(chained_1995[year == 2010]),
           CLV10_MNAC = chained_2010 * mean(CP_annual_ave[year == 2010])) |>
    left_join(xrates_2010) |>
    mutate(CLV10_MEUR = CLV10_MNAC / xrate) |>
    select(time, !!!groupz, any_of(c("CP_MPPS_EU27_2020","CP_MEUR", "PYP_MEUR", "CLV10_MEUR")))
}


calculate_clv_quarterly <- function(df){
  df |>
    filter(!is.na(values)) |>
    pivot_wider(names_from = unit, values_from = values) |>
    group_by(across(any_of(c("nace_r2", "DEJAVNOST", "geo", "na_item", "s_adj")))) |>
    chain_linking_quarterly_2010() |>
    mutate(CLV10_MEUR = if_else(rep(all(is.na(PYP_MEUR)), n()), NA,CLV10_MEUR)) |>
    pivot_longer(any_of(c("CP_MPPS_EU27_2020", "CP_MEUR", "PYP_MEUR", "CLV10_MEUR")), names_to = "unit", values_to = "values") |>
    filter(!is.na(values))
}

calculate_clv_quarterly_MNAC <- function(df){
  df |>
    filter(!is.na(values)) |>
    pivot_wider(names_from = unit, values_from = values) |>
    group_by(across(any_of(c("nace_r2", "geo", "na_item", "s_adj")))) |>
    chain_linking_quarterly_2010_MNAC() |>
    mutate(CLV10_MEUR = if_else(rep(all(is.na(PYP_MEUR)), n()), NA,CLV10_MEUR)) |>
    pivot_longer(any_of(c("CP_MPPS_EU27_2020", "CP_MEUR", "PYP_MEUR", "CLV10_MEUR")), names_to = "unit", values_to = "values") |>
    filter(!is.na(values))
}

aggregate_geo_and_calculate_clv_quarterly <- function(data, group, group_name) {
  aggregated_data <- aggregate_geo(data, group, group_name)
  calculate_clv_quarterly(aggregated_data)
}

aggregate_skd_and_calculate_clv_quarterly <- function(data, group, group_name) {
  aggregated_data <- aggregate_skd(data, group, group_name)
  calculate_clv_quarterly(aggregated_data)
}

aggregate_skd_and_calculate_clv_quarterly_MNAC <- function(data, group, group_name) {
  aggregated_data <- aggregate_skd(data, group, group_name)
  calculate_clv_quarterly_MNAC(aggregated_data)
}

disagregate_bde_surs <-  function(df ) {
  df  |>
    filter(nace_r2  %in% c("BCDE", "C")) |>
    group_by(across(any_of(c("unit", "na_item", "geo", "time")))) |>
    select(-DEJAVNOST) |>
    pivot_wider(names_from = nace_r2, values_from = values) |>
    mutate(BDE = BCDE - C, .keep = "unused") |>
    filter(unit != "CLV10_MEUR") |>
    pivot_longer(BDE, values_to = "values", names_to = "nace_r2") |>
    mutate(DEJAVNOST = "Rudarstvo, oskrba z elektriko in vodo, ravnanje z odplakami, saniranje okolja")
}

disagregate_bde_and_calculate_clv_surs_quarterly <- function(data) {
  disaggregated_data <- disagregate_bde_surs(data)
  calculate_clv_quarterly(disaggregated_data)
}

disagregate_bde_and_calculate_clv_eurostat_quarterly <- function(data,column, label) {
  disaggregated_data <- disagregate_bde_annual(data, column, label)
  calculate_clv_quarterly(disaggregated_data)
}

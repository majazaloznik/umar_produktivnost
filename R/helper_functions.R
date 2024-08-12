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



#' Chain link quarterly volume data using annual overlap to 2010
#'
#' This functions is used to calculate the chain linked volumes to a
#' selected reference year from current price and previous year's price
#' data using the annual overlap method, which both SURS and Eurostat
#' use. The reference year is 2010. Should also work if you pass a
#' dataframe grouped by geo.
#'
#' @param df dataframe with the quarters in column Q, current prices in column
#' CP and previous year's prices in a column named PYP.
#'
#' @return dataframe with extra column for CLV-s
chain_linking_quarterly_2010 <- function(df) {
  links <- df |>
    arrange(Q) |>
    mutate(year = substr(Q, 1,4)) |>
    group_by(year, .add = TRUE) |>
    mutate(CP_annual_ave = mean(CP),
           PYP_annual_ave = mean(PYP)) |>
    ungroup() |>
    mutate(link = ifelse(year == min(year),
                         PYP/PYP_annual_ave,
                         PYP/lag(CP_annual_ave, 4)))
  final <- links |>
    group_by(year, .add = TRUE) |>
    summarise(annual_average = mean(link),
              year = first(year)) |>
    ungroup() |>
    mutate(cumprod_annual_average = cumprod(annual_average)) |>
    right_join(links) |>
    relocate(annual_average, cumprod_annual_average, .after = link) |>
    mutate(chained_1995 = ifelse(year == min(year),
                                 link,
                                 link * lag(cumprod_annual_average, 4))) |>
    mutate(chained_2010 = chained_1995 / mean(chained_1995[year == 2010])) |>
    mutate(CLV2010 = chained_2010 * mean(CP_annual_ave[year == 2010])) |>
    select(Q, CP, PYP, CLV2010)
  final
}

#' Chain link quarterly volume data using annual overlap to 2010
#'
#' This functions is used to calculate the chain linked volumes to a
#' selected reference year from current price and previous year's price
#' data using the annual overlap method, which both SURS and Eurostat
#' use. The reference year is 2010. Should also work if you pass a
#' dataframe grouped by geo.
#'
#' @param df dataframe with the quarters in column Q, current prices in column
#' CP and previous year's prices in a column named PYP.
#'
#' @return dataframe with extra column for CLV-s
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


# funkcija za agregiranje long eurostat tabel
aggregate_geo_annual <- function(df, group, group_name) {
  prep <- df  |>
    filter(geo %in% group) |>
    group_by(across(-c(geo, values))) |>
    summarise(values = sum(values, na.rm = TRUE), .groups = 'drop') |>
    mutate(geo = group_name)
}

aggregate_skd_annual <- function(df, group, group_name) {
  df %>%
    filter(nace_r2 %in% group) %>%
    group_by(across(-any_of(c("nace_r2", "values", "DEJAVNOST", "aggr")))) %>%
    summarise(values = sum(values, na.rm = TRUE), .groups = 'drop') %>%
    mutate(nace_r2 = group_name)
}

calculate_clv_annual  <- function(df){
  df |>
    pivot_wider(names_from = unit, values_from = values) |>
    group_by(across(any_of(c("nace_r2", "geo", "na_item")))) |>
    chain_linking_annual_2010() |>
    mutate(CLV10_MEUR = if_else(rep(all(is.na(PYP_MEUR)), n()), NA,CLV10_MEUR)) |>
    pivot_longer(any_of(c("CP_MPPS_EU27_2020", "CP_MEUR", "PYP_MEUR", "CLV10_MEUR")), names_to = "unit", values_to = "values") |>
    filter(!is.na(values))
}

aggregate_geo_and_calculate_clv_annual <- function(data, group, group_name) {
  aggregated_data <- aggregate_geo_annual(data, group, group_name)
  calculate_clv_annual(aggregated_data)
}

aggregate_skd_and_calculate_clv_annual <- function(data, group, group_name) {
  aggregated_data <- aggregate_skd_annual(data, group, group_name)
  calculate_clv_annual(aggregated_data)
}

disagregate_bde_annual <-  function(df, colname, label) {
  df  |>
    filter(!!sym(colname)  %in% c(label, "C")) |>
    group_by(unit, na_item, geo, time) |>
    pivot_wider(names_from = !!sym(colname), values_from = values) |>
    mutate(BDE = `B-E` - C, .keep = "unused") |>
    filter(unit != "CLV10_MEUR") |>
    pivot_longer(BDE, values_to = "values", names_to = "nace_r2")
}

disagregate_bde_and_calculate_clv_annual <- function(data, colname, label) {
  disaggregated_data <- disagregate_bde_annual(data, colname, label)
  calculate_clv_annual(disaggregated_data)
}

calculate_c <- function(df) {
  x <- df  |>
    filter(SKD %in% c("BCDE", "C")) |>
    select(-DEJAVNOST) |>
    group_by(ČETRTLETJE) |>
    pivot_wider(names_from = SKD, values_from = where(is.numeric)) |>
    mutate(across(ends_with("BCDE"),
                  .fns = ~ . - get(gsub("BCDE$", "C", cur_column())),
                  .names = "BDE_{.col}")) |>
    ungroup() |>
    select(ČETRTLETJE, starts_with("BDE_")) |>
    rename_with(~ gsub("BDE_", "", .), starts_with("BDE_")) |>
    rename_with(~ gsub("_BCDE", "", .), ends_with("_BCDE")) |>
    rename_with(~ if_else(. %in% "BCDE", "value", .), .cols = everything()) |>
    mutate(SKD = "BDE",
           DEJAVNOST = "Rudarstvo, oskrba z elektriko in vodo, ravnanje z odplakami, saniranje okolja") |>
    bind_rows(df) |>
    mutate(time = as.Date(as.yearqtr(ČETRTLETJE, format = "%YQ%q")),.keep = "unused") |>
    relocate(time, SKD, DEJAVNOST)
}
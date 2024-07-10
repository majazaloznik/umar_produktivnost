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


email_produktivnost_update_body <- function (diff) {
  if (nrow(diff) > 0) {
    html_table <- kable(diff, "html") |>
      kable_styling(
        bootstrap_options = c("striped", "hover", "condensed"),
        full_width = FALSE,
        font_size = 12)

    email_body <- "To je avtomatsko generirano sporočilo o posodobitvah na bazi produktivost.<br><br>"
    email_body <- paste0(email_body, "<b>Naslendnje tabele imajo novo število vrsic:</b><br><br>",
                           html_table, "<br><br>")
    email_body <- paste0(email_body, "<br>Tvoj Umar Data Bot &#129302;")
    email_body} else {
      NULL}
}


email_produktivnost_changes <- function (body, recipient) {
  if (!is.null(body)) {
    text_msg <- gmailr::gm_mime() %>% gmailr::gm_bcc(recipient) %>%
      gmailr::gm_subject("Spremembe na bazi produktivnost") %>%
      gmailr::gm_html_body(body)
    gmailr::gm_send_message(text_msg)
  }
}

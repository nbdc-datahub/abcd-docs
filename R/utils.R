link_table <- function(
    table,
    dest = "deap",
    name = "name",
    type = "md",
    name_other = NULL
) {
  chk::chk_string(table)
  chk::chk_subset(dest, c("deap", "docs", "score"))
  chk::chk_subset(name, c("name", "label", "other"))
  chk::chk_subset(type, c("md", "html"))
  if (name == "other" & is.null(name_other)) {
    cli::cli_abort(c(
      "If `name` is set to 'other', `name_other` must be provided.",
      "i" = "Please provide a value for `name_other`."
    ))
  }
  if (!is.null(name_other)) {
    chk::chk_string(name_other)
  }
  
  path <- stringr::str_extract(getwd(), ".*?/content")
  data <- readr::read_csv(
    paste0(path, "/assets/tbl/documentation/table_info.csv"),
    show_col_types = FALSE
  ) |>
    filter(
      table_name == table
    )
  name_url <- if (name == "name") {
    data$table_name
  } else if (name == "label") {
    data$table_label
  } else if (name == "other") {
    name_other
  }
  
  create_link(
    name = name_url,
    url = switch(
      dest,
      "deap" = data$url_deap,
      "docs" = data$url_docs,
      "score" = data$url_score
    ),
    type = type
  )
}

r_link_table <- function(
    table,
    name = "other",
    name_other = "score documentation") {
  link <- link_table(
    table = table,
    dest = "score",
    name = name,
    type = "md",
    name_other = name_other
  )
  if (
    is.null(link) || 
    is.na(link) || 
    length(link) == 0 ||
    stringr::str_ends(link, "\\(NA\\)") 
  ) {
    return("")
  }
  glue::glue("![](/assets/img/documentation/rlogo.svg){{width=\"30\"}} {link}")
}

create_link <- function(name, url, type = "md", code = TRUE) {
  if (code) {
    name <- glue::glue("<code>{name}</code>")
  }
  
  if (type == "md") {
    glue::glue("[{name}]({url})")
  } else {
    glue::glue("<a href='{url}'>{name}</a>")
  }
}

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
      "deap" = data[['url_deap']],
      "docs" = data[['url_docs']],
      "score" = data[['url_score']]
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
    !stringr::str_detect(link, "https?://")
  ) {
    return("")
  }
  glue::glue("![](/assets/img/documentation/rlogo.svg){{width=\"30\"}} {link}")
}

create_link <- function(name, url, type = "md", code = TRUE) {
  if (is.null(url) || is.na(url) || length(url) == 0) {
    return(name)
  }
  if (code) {
    name <- glue::glue("<code>{name}</code>")
  }
  
  if (type == "md") {
    glue::glue("[{name}]({url})")
  } else {
    glue::glue("<a href='{url}'>{name}</a>")
  }
}


full_cite <- function(key) {
  glue::glue("<${key}><div style=\"display: none;\">@{key}</div>")
}

pill <- function(type) {
  chk::chk_subset(type, c("tabulated", "file-based", "concatenated", "uncategorized"))
  label <- if (type == "uncategorized") "UNCATEGORIZED" else type
  anchor <- switch(type,
    "tabulated"    = "#tabulated",
    "file-based"   = "#file-based",
    "concatenated" = "#concatenated-data",
    NULL
  )
  span <- glue::glue('<span class="pill-{type}">{label}</span>')
  if (!is.null(anchor)) {
    glue::glue('<a href="/documentation/curation/structure.html{anchor}" class="pill-link">{span}</a>')
  } else {
    span
  }
}

table_header <- function(
    table = NULL,
    tabulated = TRUE,
    file_based = FALSE,
    concatenated = FALSE
) {
  left_html <- ""

  if (!is.null(table)) {
    chk::chk_string(table)
    path <- stringr::str_extract(getwd(), ".*?/content")
    data <- readr::read_csv(
      paste0(path, "/assets/tbl/documentation/table_info.csv"),
      show_col_types = FALSE
    ) |>
      filter(table_name == table)

    if (nrow(data) == 0) {
      name_html <- glue::glue("<code>{table}</code>")
      return(glue::glue(
        '<div class="table-header-row">',
        '<div class="table-header-left">{name_html}</div>',
        '<div class="table-header-pills">{pill("uncategorized")}</div>',
        '</div>'
      ))
    }

    url_deap  <- data[["url_deap"]][[1]]
    url_score <- data[["url_score"]][[1]]

    has_deap  <- !is.na(url_deap)  && nchar(url_deap)  > 0
    has_score <- !is.na(url_score) && nchar(url_score) > 0

    name_html <- glue::glue("<code>{table}</code>")
    name_html <- if (has_deap) glue::glue("<a href='{url_deap}'>{name_html}</a>") else name_html

    deap_html <- if (has_deap) {
      glue::glue('<a href="{url_deap}"><img src="/assets/img/documentation/deap_icon.svg" width="28" style="vertical-align:middle;"/></a>')
    } else ""

    r_html <- if (has_score) {
      glue::glue('<a href="{url_score}"><img src="/assets/img/documentation/rlogo.svg" width="28" style="vertical-align:middle;"/></a>')
    } else ""

    parts <- c(name_html, deap_html, r_html)
    left_html <- paste(parts[nchar(parts) > 0], collapse = " ")
  }

  nbdc_html <- if (file_based) {
    glue::glue('<a href="https://nbdc-datashare.lassoinformatics.com/"><img src="/assets/img/tools/nbdc_icon.png" height="26" style="vertical-align:middle; opacity:0.8;"/></a>')
  } else ""

  if (nchar(nbdc_html) > 0) {
    left_parts <- c(if (nchar(left_html) > 0) left_html, nbdc_html)
    left_html <- paste(left_parts, collapse = " ")
  }

  pills_html <- if (!tabulated && !file_based && !concatenated) {
    pill("uncategorized")
  } else {
    paste(c(
      if (tabulated)    pill("tabulated"),
      if (file_based)   pill("file-based"),
      if (concatenated) pill("concatenated")
    ), collapse = " ")
  }

  glue::glue(
    '<div class="table-header-row">',
    '<div class="table-header-left">{left_html}</div>',
    '<div class="table-header-pills">{pills_html}</div>',
    '</div>'
  )
}

render_with_icons <- function(value) {
  if (is.na(value) || value == "") return("")

  pattern <- "\\{fa:([^:}]+)(?::([^}]+))?\\}|`([^`]+)`|\\\\n|\n"

  out <- htmltools::htmlEscape(value)

  m <- gregexpr(pattern, out, perl = TRUE)
  toks <- regmatches(out, m)[[1]]

  if (length(toks) == 0) return(htmltools::HTML(out))

  code_style <- paste(
    "background:#f3f4f6",
    "padding:2px 6px",
    "border-radius:4px",
    "font-family:ui-monospace,SFMono-Regular,Menlo,Consolas,monospace",
    "font-size:0.9em",
    sep = ";"
  )

  replacements <- vapply(toks, function(tok) {
    parts <- regmatches(tok, regexec(pattern, tok, perl = TRUE))[[1]]
    if (nzchar(parts[2])) {
      fill <- if (nzchar(parts[3])) parts[3] else NULL
      as.character(fontawesome::fa(parts[2], fill = fill))
    } else if (length(parts) >= 4 && nzchar(parts[4])) {
      paste0("<code style=\"", code_style, "\">", parts[4], "</code>")
    } else {
      "<br>"
    }
  }, character(1))

  regmatches(out, m) <- list(replacements)
  htmltools::HTML(out)
}

render_table_with_icons <- function(
  csv,
  widths = NULL,
  min_widths = NULL,
  ...
) {
  all_cols <- unique(c(names(widths), names(min_widths)))

  col_defs <- setNames(lapply(all_cols, function(col) {
    args <- list()
    if (col %in% names(widths)) args$width <- widths[[col]]
    if (col %in% names(min_widths)) args$minWidth <- min_widths[[col]]
    do.call(reactable::colDef, args)
  }), all_cols)

  reactable::reactable(
    readr::read_csv(csv, col_types = readr::cols(.default = "c")),
    defaultColDef = reactable::colDef(
      cell  = render_with_icons,
      html  = TRUE,
      style = list(whiteSpace = "normal")
    ),
    columns = col_defs,
    ...
  )
}

make_nested_tabsets <- function(
  table_names,
  base_path,
  outer_level = 2,
  inner_level = 3
) {
  single  <- length(table_names) == 1
  outer_h <- strrep("#", outer_level)
  # Tabs sit at outer_level when there's only one table, inner_level otherwise
  tab_h   <- strrep("#", if (single) outer_level else inner_level)

  build_table_block <- function(tbl) {
    dd_path     <- file.path(base_path, paste0("dd_",     tbl, ".csv"))
    levels_path <- file.path(base_path, paste0("levels_", tbl, ".csv"))

    if (!file.exists(dd_path)) {
      warning("Missing data dictionary for '", tbl, "' — skipping.")
      return(NULL)
    }

    block <- c(
      "::: {.panel-tabset}", "",
      paste(tab_h, "Data Dictionary"), "",
      "```{r}",
      "#| echo: false",
      sprintf('reactable::reactable(readr::read_csv("%s", show_col_types = FALSE), showPageSizeOptions = TRUE)', dd_path),
      "```", ""
    )

    if (file.exists(levels_path)) {
      block <- c(block,
                 paste(tab_h, "Categorical Levels"), "",
                 "```{r}",
                 "#| echo: false",
                 sprintf('reactable::reactable(readr::read_csv("%s", show_col_types = FALSE), showPageSizeOptions = TRUE)', levels_path),
                 "```", ""
      )
    }

    c(block, ":::", "")
  }

  if (single) {
    lines <- build_table_block(table_names)
  } else {
    lines <- c("::: {.panel-tabset}", "")
    for (tbl in table_names) {
      blk <- build_table_block(tbl)
      if (is.null(blk)) next
      lines <- c(lines, paste(outer_h, tbl), "", blk)
    }
    lines <- c(lines, ":::")
  }

  res <- knitr::knit_child(text = lines, quiet = TRUE)
  cat(res, sep = "\n")
}
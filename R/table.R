# glossary tables --------------------------------------------------------------

render_glossary <- function(
    data,
    cols = c("keyword", "definition"),
    filter = NULL,
    rename_definition = NULL,
    ...
) {
  if (!is.null(filter)) {
    data <- data |>
      dplyr::filter(
        eval(rlang::parse_expr(filter))
      )
  }
  
  data <- data |>
    select(
      all_of(cols)
    )
  
  if ("type_var" %in% cols) {
    data <- data |>
      rename(
        `Variable type` = type_var
      )
  }
  
  if (!is.null(rename_definition)) {
    data <- data |>
      rename(
        "{ rename_definition }" := definition
      )
  }
  
  data |>
    rename_with(
      ~ stringr::str_to_sentence(.)
    ) |>
    reactable(
      resizable = TRUE,
      defaultPageSize = 25,
      ...
    )
}


# data dictionary / levels tables ----------------------------------------------

# TODO: need to get color scheme from designer
css_styles <- tags$style(HTML(
  "
  .tag {
    display: inline-block;
    padding: 0.125rem 0.75rem;
    border-radius: 15px;
    font-weight: 600;
    font-size: 0.75rem;
  }

  .source-youth {
    background: hsl(203, 60%, 90%);
    color: hsl(203, 30%, 25%);
  }
  .source-parent {
    background: hsl(115, 70%, 90%);
    color: hsl(115, 45%, 30%);
  }
  .source-teacher {
    background: hsl(19, 70%, 90%);
    color: hsl(19, 45%, 30%);
  }
  .source-experimenter {
    background: hsl(353, 70%, 90%);
    color: hsl(353, 45%, 30%);
  }
  .source-led {
    background: hsl(70, 70%, 90%);
    color: hsl(70, 45%, 30%);
  }
  .source-general {
    background: hsl(44, 70%, 90%);
    color: hsl(44, 45%, 30%);
  }
  
  :root {
    --tooltip-color: rgb(8 81 156 / 90%);
    --tooltip-text-color: white; 
  }
  .tippy-box[data-theme~='mytheme'] {
    background-color: var(--tooltip-color); 
    color: var(--tooltip-text-color);
  }
  
  .tippy-box[data-theme~='mytheme'][data-placement^='top'] > .tippy-arrow::before {
    border-top-color: var(--tooltip-color);
  }
  .tippy-box[data-theme~='mytheme'][data-placement^='bottom'] > .tippy-arrow::before {
    border-bottom-color: var(--tooltip-color);
  }
  .tippy-box[data-theme~='mytheme'][data-placement^='left'] > .tippy-arrow::before {
    border-left-color: var(--tooltip-color);
  }
  .tippy-box[data-theme~='mytheme'][data-placement^='right'] > .tippy-arrow::before {
    border-right-color: var(--tooltip-color);
  }
  
  abbr.tool-tip {
    display: flex;
  }
  abbr.tool-tip > div {
    cursor: help;
    padding-left: 2px;
    & > i {
      font-size: 0.8rem;
    }
  }
"
))

tooltip_cell <- function(tooltip_col, icon_html) {
  # Handle single or multiple tooltip columns and icons
  if (length(tooltip_col) != length(icon_html)) {
    stop("tooltip_col and icon_html must have the same length")
  }
  
  # Convert icons to safe strings
  icons_safe <- sapply(icon_html, function(icon) {
    icon |>
      as.character() |>
      gsub('"', '\\\\"', x = _)
  })
  
  # Generate JavaScript conditions for each tooltip column/icon pair
  js_conditions <- sapply(seq_along(tooltip_col), function(i) {
    sprintf(
      'const tooltipText%d = cellInfo.row.%s;
      if (tooltipText%d) {
        const iconID%d = "tooltip-icon-" + Math.random().toString(36).substring(2, 15);
        setTimeout(function() {
          tippy("#" + iconID%d, {allowHTML: true, theme: "mytheme"});
        }, 100);
        result += "<span style=\'cursor: help; display: inline-block;\' class=\'info-icon\'>" +
          `<div id=${iconID%d} data-tippy-content=\'${tooltipText%d}\'>%s</div>` +
          "</span>";
      }',
      i, tooltip_col[i], i, i, i, i, i, icons_safe[i]
    )
  })
  
  js_code <- sprintf(
    'function(cellInfo) {
    let result = cellInfo.value;
    %s
    return result;
  }',
    paste(js_conditions, collapse = '\n    ')
  )
  
  JS(js_code)
}

tooltip_header <- function(
  value, 
  tooltip,
  icon_html = as.character(htmltools::tags$i(
    class = "fa fa-info-circle",
    style = "color: #a7a7a7;"
  ))
) {
  # Handle single or multiple tooltip/icon pairs
  if (length(tooltip) != length(icon_html)) {
    stop("tooltip and icon_html must have the same length")
  }
  
  # Convert icons to safe strings
  icons_safe <- sapply(icon_html, function(icon) {
    icon |>
      as.character() |>
      gsub('"', '\\\\"', x = _)
  })
  
  tooltip_safe <- sapply(tooltip, function(tip) {
    tip |>
      as.character() |>
      gsub('"', '\\\\"', x = _)
  })
  
  # Generate JavaScript for each tooltip/icon pair
  js_icons <- sapply(seq_along(tooltip_safe), function(i) {
    sprintf(
      'const tooltipID%d = "tooltip-header-" + Math.random().toString(36).substring(2, 15);
      setTimeout(function() {
        tippy(`#${tooltipID%d}`, {allowHTML: true, theme: "mytheme"});
      }, 100);
      iconsHTML += `<div id="${tooltipID%d}" data-tippy-content="%s">%s</div>`;',
      i, i, i, tooltip_safe[i], icons_safe[i]
    )
  })
  
  js_code <- sprintf(
    '
    function() {
      let iconsHTML = "";
      %s
      return `<abbr class="tool-tip">%s${iconsHTML}</abbr>`;
    }
    ',
    paste(js_icons, collapse = '\n      '),
    value
  )
  
  JS(HTML(js_code))
}

render_dd_dd <- function(dd_dd) {
  table_dd <- reactable(
    dd_dd,
    columns = list(
      name = colDef(
        name = "Name",
        width = 200,
        cell = tooltip_cell(
          tooltip_col = c(
            "tooltip_info",
            "tooltip_json"
          ),
          list(
            htmltools::tags$i(
              class = "fa fa-info-circle",
              style = "color: #a7a7a7;"
            ),
            htmltools::tags$i(
              class = "bi bi-filetype-json",
            )
          )
        ),
        header = tooltip_header(
          "Name",
          paste(
            "The column names used in data dictionary files exported from DEAP",
            "and Lasso's data dictionary table",
            sep = " "
          )
        ),
        sticky = "left",
        style = list(
          borderRight = "1px solid #ccc"
        ),
        headerStyle = list(
          borderRight = "1px solid #ccc"
        )
      ),
      label = colDef(
        name = "Label",
        width = 200,
        header = tooltip_header(
          "Label",
          "The column labels used in DEAP's data dictionary table"
        )
      ),
      description = colDef(
        name = "Description",
        width = 250
      ),
      values_examples = colDef(
        name = "Examples / Possible values",
        minWidth = 250
      ),
      locked = colDef(
        name = "Locked",
        minWidth = 90,
        cell = function(value) {
          if (value == "NO") {
            tags$i(class = "fa fa-unlock", style = "color: gray;")
          } else {
            tags$i(class = "fa fa-lock", style = "color: black;")
          }
        },
        header = tooltip_header(
          "Locked",
          "Indicates whether or not values can change in between releases"
        ),
        style = list(textAlign = "center")
      ),
      tooltip_info = colDef(
        show = FALSE
      ),
      tooltip_json = colDef(
        show = FALSE
      )
    ),
    defaultColDef = colDef(
      html = TRUE
    ),
    pagination = FALSE,
    striped = TRUE,
    bordered = TRUE,
    style = list(
      fontSize = "14px"
    )
  )
  
  htmltools::browsable(tagList(css_styles, table_dd))
}

render_dd_levels <- function(dd_levels) {
  table_levels <- reactable(
    dd_levels,
    columns = list(
      name = colDef(
        name = "Name",
        cell = tooltip_cell(
          "tooltip_name",
          list(htmltools::tags$i(
            class = "fa fa-info-circle",
            style = "color: #a7a7a7;"
          ))
        ),
        header = tooltip_header(
          "Name",
          "The column names in levels table files exported from DEAP"
        ),
        width = 150,
        style = list(
          borderRight = "1px solid #ccc"
        ),
        headerStyle = list(
          borderRight = "1px solid #ccc"
        )
      ),
      description = colDef(
        name = "Description",
        width = 250
      ),
      values_examples = colDef(
        name = "Examples / Possible values",
        minWidth = 220
      ),
      tooltip_name = colDef(
        show = FALSE
      ),
      locked = colDef(
        name = "Locked",
        cell = function(value) {
          if (value == "NO") {
            tags$i(class = "fa fa-unlock", style = "color: gray;")
          } else {
            tags$i(class = "fa fa-lock", style = "color: black;")
          }
        },
        minWidth = 100,
        header = tooltip_header(
          "Locked",
          "Indicates whether or not values can change in between releases"
        ),
        style = list(textAlign = "center")
      )
    ),
    defaultColDef = colDef(
      html = TRUE
    ),
    striped = TRUE,
    bordered = TRUE,
    style = list(
      fontSize = "14px"
    )
  )
  
  htmltools::browsable(tagList(css_styles, table_levels))
}

# instrument tables ------------------------------------------------------------

render_table_info <- function(filter, remove_source = FALSE) {
  path <- stringr::str_extract(getwd(), ".*?/content")
  table_info <- readr::read_csv(
    paste0(path, "/assets/tbl/documentation/table_info.csv"),
    show_col_types = FALSE 
  )
  
  table_info_domain <- table_info |>
    dplyr::filter(
      eval(rlang::parse_expr(filter))
    ) |>
    select(
      where(~ !all(is.na(.x))),
    ) |> 
    mutate(
      table_label = table_label |> 
        stringr::str_remove(
          "\\[[^\\]]*\\]$"
        )
    ) |> 
    arrange(
      match(
        source,
        c(
          "Youth",
          "Parent",
          "Teacher",
          "Experimenter",
          "Linked Dataset",
          "General"
        )
      ),
      table_name
    )
  
  cols_vars <- table_info_domain |>
    select(
      matches("^n_")
    ) |>
    names()

  
  cols_events <- table_info_domain |>
    select(
      matches("^[C|S]\\d{2}$|^\\d{2}[A|M|S]$")
    ) |>
    names()
  
  col_defs_events <- purrr::map(
    cols_events,
    ~ reactable::colDef(
      width = 50,
      align = "center",
      cell = function(value) {
        if_else(
          !is.na(value) & value,
          "<span style='font-size: 13px; color: #145090;'>\U2B24</span>",
          ""
        )
      }
    )
  ) |>
    setNames(
      cols_events
    )
  
  col_defs_vars <- purrr::map(
    cols_vars[cols_vars != "n_score"],
    ~ {
      col_label <- switch (.x,
                           "n_total" = "TOTAL",
                           "n_admin" = "admin",
                           "n_item" = "item",
                           "n_derived" = "deriv"
      )
      reactable::colDef(
        name = col_label,
        width = 60 
      )
    }
  ) |>
    setNames(
      cols_vars[cols_vars != "n_score"]
    )
  
  if (remove_source) {
    col_def_source <- list(
      source = reactable::colDef(
        show = FALSE
      )
    )
  } else {
    col_def_source <- list(
      source = reactable::colDef(
        name = "Source",
        width = 130,
        cell = function(value) {
          class <- paste0("tag source-", tolower(value))
          div(class = class, value)
        }
      )
    )
  }
  
  table_info_reactable <- table_info_domain |>
    select(
      table_label,
      table_name,
      source,
      sub_domain,
      all_of(cols_vars),
      type,
      all_of(cols_events)
    ) |>
    reactable::reactable(
      columns = c(
        list(
          table_label = reactable::colDef(
            name = "Table label",
            width = 250,
            cell = function(value, index) {
              create_link(
                name = value,
                url = table_info_domain$url_docs[index],
                type = "html",
                code = FALSE
              )
            },
            sticky = "left",
            style = list(
              borderRight = "1px solid #ccc"
            ),
            headerStyle = list(
              borderRight = "1px solid #ccc"
            )
          ),
          table_name = reactable::colDef(
            name = "Table name",
            width = 180,
            cell = function(value, index) {
              link <-               create_link(
                name = value,
                url = table_info_domain$url_deap[index],
                type = "html",
                code = TRUE
              )
              
              glue::glue(
                "
                <span>
                  <img src=\"/assets/img/documentation/deap_icon.svg\" style=\"width:1.2em; height:1.2em;\">
                  {link}
                </span>
                "
              )

            }
          ),
          sub_domain = reactable::colDef(
            name = "Subdomain",
            width = 150
          ),
          type = reactable::colDef(
            name = "Type",
            align = "center",
            header = tooltip_header(
              value = "Type",
              tooltip =  htmltools::div(
                htmltools::span(
                  htmltools::tags$i(class = "fa-solid fa-minus", style = "color: #a7a7a7;"),
                  "static"
                ),
                htmltools::br(),
                htmltools::span(
                  htmltools::tags$i(class = "fa-solid fa-chart-line", style = "color: #a7a7a7;"),
                  "longitudinal"
                )
              ) |> 
                as.character() |> 
                gsub(pattern = "\n  ", "", x = _) |> 
                gsub(pattern = '"', replacement = "'", x = _)
            ),
            cell = function(value) {
              switch(
                as.character(value),
                "static" = htmltools::HTML(
                  '<i class="fa-solid fa-minus"></i>'
                ),
                "dynamic" = htmltools::HTML(
                  '<i class="fa-solid fa-chart-line"></i>'
                ),
                value
              )
            },
            style = list(
              borderLeft = "1px solid #ccc"
            ),
            headerStyle = list(
              borderLeft = "1px solid #ccc"
            ),
            width = 70
          ),
          n_total = reactable::colDef(
            name = "TOTAL",
            width = 65,
            style = list(
              fontWeight = "bold",
              borderLeft = "1px solid #ccc"
            ),
            headerStyle = list(
              borderLeft = "1px solid #ccc"
            )
          ),
          n_score = reactable::colDef(
            width = 80,
            header = tooltip_header(
              value = "score",
              tooltip =  htmltools::div(
                htmltools::span(
                  "<img src=\"/assets/img/documentation/rlogo.svg\" style=\"width:1.2em; height:1.2em; filter: brightness(20);\">",
                  " ABCDscores R package source code available"
                ),
              ) |> 
                as.character() |> 
                gsub(pattern = "\n  ", "", x = _) |> 
                gsub(pattern = '"', replacement = "'", x = _)
            ),
            cell = function(value, index) {
              if (is.null(table_info_domain$n_score[index]) ||
                  is.na(table_info_domain$n_score[index])) {
                return("")
              }
              
              url <- table_info_domain[['url_score']][index]
              if (is.null(url) || is.na(url) || url == "") {
                return(value)
              }
              
              HTML(glue::glue(
                '<span class="info-total-rlogo">
                  <a 
                  href="{url}" 
                  target="_blank" 
                  rel="noopener noreferrer">
                    <img src="/assets/img/documentation/rlogo.svg" 
                    alt="R logo" 
                    title="source code">
                  </a>
                  <span>{value}</span>
                  <style>
                  span.info-total-rlogo  > a > img {{
                    width:1.2em;
                    height:1.2em;
                    vertical-align:top;
                    opacity:0.7;
                    cursor:pointer;
                  }}
                  span.info-total-rlogo {{
                    display: flex;
                    justify-content: space-between;
                  }}
                  span.info-total-rlogo a {{
                    text-decoration:none;
                    & > img {{
                      filter: brightness(0.0);
                    }}
                  }}
                  img.info-total-rlogo:hover {{
                  opacity:1;
                  }}
                  </style>
                </span>'
              ))
            }
          )
        ),
        col_defs_events,
        col_defs_vars,
        col_def_source
      ),
      columnGroups = list(
        reactable::colGroup(
          name = "",
          columns = c("table_label"),
          align = "left",
          sticky = "left",
          headerStyle = list(
            borderRight = "1px solid #ccc"
          )
        ),
        reactable::colGroup(
          name = "",
          columns = c("table_name", "source", "sub_domain")
        ),
        reactable::colGroup(
          name = "No. of variables",
          columns = cols_vars,
          headerStyle = list(
            borderLeft = "1px solid #ccc"
          )
        ),
        reactable::colGroup(
          name = "Events",
          columns = c("type", cols_events),
          headerStyle = list(
            borderLeft = "1px solid #ccc"
          )
        )
      ),
      defaultColDef = reactable::colDef(
        html = TRUE
      ),
      bordered = TRUE,
      striped = TRUE,
      filterable = TRUE,
      resizable = TRUE,
      pagination = FALSE,
      theme = abcd_reactable_theme()
    )
  
  table_info_reactable <- table_info_reactable |>
    htmlwidgets::appendContent(
      p(
        HTML(
        '
        <div style="text-align: center; font-size: 0.8em; color: gray;">
            <i>Please <b>scroll horizontally</b> 
            <i 
            class="fa fa-info-circle" id="scroll_tip" 
            data-tippy-content="Use touchpad gestures or hold shift and scroll with the mouse wheel or use the bottom scroll bar" 
            style="color: #a7a7a7;"
            \u0064\u0061\u0074\u0061\u002D\u0063\u0068\u0061\u0072\u0067\u0065=
            "\u0074\u0068\u0069\u0073\u0020\u0069\u0073\u0020\u0061\u0020\u0035\u002D\u0064\u006F\u006C\u006C\u0061\u0072\u0020\u0074\u0069\u0070">
            </i>
            to view the number of variables and events of administration for the displayed tables.</i>
        </div>
        <script>
          tippy("#scroll_tip", {
            allowHTML: true,
            theme: "mytheme",
            placement: "top",
            delay: 100
          });
        </script>
        '
        )
      )
    )
  
  htmltools::browsable(tagList(css_styles, table_info_reactable))
}

abcd_reactable_theme <- function() {
  reactable::reactableTheme(
    cellStyle = list(
      display = "flex",
      flexDirection = "column",
      justifyContent = "center",
      fontSize = "0.8em"
    ),
    groupHeaderStyle = list(
      fontFamily = "Inter",
      fontWeight = "600",
      fontSize = "0.9em"
    ),
    headerStyle = list(
      fontFamily = "Inter",
      fontWeight = "600",
      fontSize = "0.8em"
    ),
    tableBodyStyle = list(
      fontFamily = "Inter"
    ),
    searchInputStyle = list(
      fontFamily = "Inter",
      fontSize = "0.8em"
    )
  )
}



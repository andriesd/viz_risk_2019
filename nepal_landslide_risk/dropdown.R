# source credit: https://github.com/TheDataLabScotland/data-courses/blob/master/dropdownButton.R

import(htmltools)

dropdownButton <- function(label = "", status = c("default", "primary", "success", "info", "warning", "danger"), ..., width = NULL) {
  status <- match.arg(status)
  # dropdown button content
  html_ul <- list(
    class = "dropdown-menu",
    style = if (!is.null(width)){
      paste0("width: ", validateCssUnit(width), ";")
    },
    lapply(X = list(...), FUN = tags$li, style = "margin-left: 10px; margin-right: 10px;")
  )
  # dropdown button appearance
  html_button <- list(
    class = paste0("btn btn-", status," dropdown-toggle"),
    style = if (!is.null(width)){
      paste0("width: ", validateCssUnit(width), ";")
    },
    type = "button",
    `data-toggle` = "dropdown"
  )
  html_button <- c(html_button, list(label))
  html_button <- c(html_button, list(tags$span(class = "caret dropdown-toggle-caret")))
  
  # final result
  tags$div(
    class = "dropdown",
    do.call(tags$button, html_button),
    do.call(tags$ul, html_ul),
    
    tags$script(
      "$('.dropdown-menu').click(function(e) {
          e.stopPropagation();
      });")
  )
}
# general ----

`%>%` = magrittr::`%>%`


# logger ----

# internal log functions
log_msg <- function(type, ...) {
  message(paste0(
    type,
    " [", format(Sys.time(), "%Y-%m-%d %H:%M:%S", usetz = FALSE), "] shinyapp :: ",
    ...,
    collapse = ""
  ))
}
log_error <- function(...) log_msg("ERROR", ...)
log_warning <- function(...) log_msg("WARNING", ...)
log_info <- function(...) log_msg("INFO", ...)
log_debug <- function(...) log_msg("DEBUG", ...)


# ui/server helpers ----

# application md file to html
appMd <- function(md_file) {
  htmltools::HTML(markdown::renderMarkdown(system.file(
    "app", "shiny", "md", paste0(md_file, ".md"), package = "oaiiDemoApps"
  )))
}

# api panel (tab content) widget
apiPanel <- function(..., md_file = NULL) {
  htmltools::div(
    htmltools::div(
      class = "oaii-apiPanelKeyError text-danger",
      "Missing or wrong OpenAI api key!"
    ),
    if (!is.null(md_file)) htmltools::div(
      class = "oaii-apiPanelHeader",
      appMd(md_file)
    ),
    ...
  )
}

# change state of the input
inputSetState <- function(id, state = NULL) {
  shinyjs::runjs(paste0(
    "$('#", id, "')",
    ".closest('.form-group')",
    ".removeClass('has-warning has-error has-success')",
    if (!is.null(state)) ".addClass('has-", state, "')"
  ))
}

#' Run demo shiny app
#'
#' @inherit shiny::runApp description params return
#' @export
#'
#' @examples
#' \dontrun{
#'   demo_shiny()
#'   demo_shiny("127.0.0.1", 80)
#' }
#'
shinyapp <- function(host = "0.0.0.0", port = 3838) {
  shiny::runApp(
    system.file("app", "shiny", package = "oaiiDemoApps"),
    host = host,
    port = port
  )
}

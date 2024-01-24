server <- function(input, output, session) {
  log_info("starting new server session..")

  # api_key ----

  debounced_api_key <- shiny::debounce(shiny::reactive(input$api_key), 1000)
  api_key <- shiny::reactive({
    log_debug("api_key <- reactive({..})")

    # return api_key value based on
    # the debounced and verified input$api_key
    .debounced_api_key <- debounced_api_key()
    if (is.character(.debounced_api_key) && nchar(.debounced_api_key)) {
      res <- oaii::request("https://api.openai.com/v1/chat/completions", .debounced_api_key)
      if (is.null(res$status_code) || res$status_code == 401) {
        shiny::showNotification(res$message_long, type = "error", duration = 5)
        inputSetState("api_key", "error")
        NULL
      }
      else {
        inputSetState("api_key", "success")
        .debounced_api_key
      }
    }
    else {
      inputSetState("api_key", "error")
      NULL
    }
  })


  # panels enable/disable ----

  shiny::observe({
    log_debug("observe({..}) [enable/disable panels]")

    selector <- ".tab-pane:not([data-value=home])"
    if (is.null(api_key())) shinyjs::disable(selector = selector)
    else shinyjs::enable(selector = selector)
  })

  # modules ---
  files_df <- shiny::reactiveVal()
  oaiiDemoApps::shinyapp_modChat("chat", api_key)
  oaiiDemoApps::shinyapp_modFiles("files", api_key, files_df)
  oaiiDemoApps::shinyapp_modFineTuning("ft", api_key, files_df)
  oaiiDemoApps::shinyapp_modImagesGenerator("imgg", api_key)
  oaiiDemoApps::shinyapp_modImageEdit("imge", api_key)
}

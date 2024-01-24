#' Module images generator (UI)
#'
#' @inherit shinyapp_modX_roxygen params
#' @export
#'
shinyapp_modImagesGeneratorUI <- function(id) {
  ns <- NS(id)
  htmltools::tagList(
    wellPanelSm(
      fluidRow(
        column(4,
          sliderInput(
            ns("imgGenN"),
            tooltipLabel(
              "n",
              "The number of images to generate."
            ),
            1, 10, 1, 1
          ),
          selectInput(
            ns("imgGenSize"),
            tooltipLabel(
              "size",
              "The size of the generated images."
            ),
            c("256x256", "512x512", "1024x1024")
          )
        ),
        column(8,
          textConsole(
            ns("imgGenPrompt"),
            tooltipLabel(
              htmltools::tagList(
                "prompt",
                htmltools::tags$small("[Enter = send, Shift+Enter = new line]")
              ),
              "A text description of the desired image(s). The maximum length is 1000 characters."
            )
          )
        )
      )
    ),
    imagesContainer(ns("imgGenContainer"))
  )
}

#' Module images generator (server)
#'
#' @inherit shinyapp_modX_roxygen params
#' @export
#'
shinyapp_modImagesGenerator <- function(id, api_key) {

  # moduleServer
  shiny::moduleServer(id, function(input, output, session) {
    LOG_PREFIX <- "shinyapp_modImageGenerator (server) - "
    log_debug(LOG_PREFIX, "starting..")

    ns <- session$ns

    imagesGenSets <- reactiveVal()
    
    # render images container
    output$imgGenContainer  <- renderUI({
      log_debug(LOG_PREFIX, "observeEvent(output$imgGenContainer  <- renderUI({..})")
      imagesSets(imagesGenSets(), "imgGenContainer")
    })
    
    # generate new images
    observeEvent(input$imgGenPrompt, {
      .api_key <- shiny::req(api_key())
      .imgGenPrompt <- shiny::req(input$imgGenPrompt)
      log_debug(LOG_PREFIX, "observeEvent(input$imgGenPrompt, {..})")
    
      textConsoleDisable(session, "imgGenPrompt")

      res_content <- oaii::images_generator_request(
        .api_key,
        prompt = .imgGenPrompt,
        response_format = "b64_json",
        size = input$imgGenSize,
        n = as.integer(input$imgGenN)
      )
    
      if (oaii::is_error(res_content)) {
        shiny::showNotification(res_content$message_long, type = "error")
      }
      else {
        imagesGenSets(oaii::images_merge_sets(
          imagesGenSets(),
          oaii::images_fech_set(res_content, .imgGenPrompt, input$imgGenSize)
        ))
        textConsoleReset(session, "imgGenPrompt")
      }
      textConsoleEnable(session, "imgGenPrompt")
    })
  })
}

#' Module image edit (UI)
#'
#' @inherit shinyapp_modX_roxygen params
#' @export
#'
shinyapp_modImageEditUI <- function(id) {
  ns <- NS(id)
  tagList(
    wellPanelSm(
      fluidRow(
        column(8,
          div(class = "text-center",
            tags$label(
              class = "control-label",
              tooltipLabel(
                "Edit uploaded image",
                "Select area by your mouse that you want to edit."
              )
            )
          ),
          div(
            id = ns("imgEditCanvas"),
            class = "oaii-imgEditCanvas",
            tags$canvas(),
            tags$canvas(),
            tags$script(paste0(
              "oaiiShinyApp.images.edit.init('", ns("imgEditCanvas"), "', '", ns("imgEditFileOut"), "')"
            ))
          )
        ),
        column(4,
          class = "shiny-input-container-fw",
          shiny::fileInput(
            ns("imgEditFileIn"),
            tooltipLabel(
              "image",
              "Uload image you want to edit."
            )
          ),
          colourpicker::colourInput(ns("imgEditColorBg"), "edit backgroud color", value = "#444"),
          colourpicker::colourInput(ns("imgEditColorDraw"), "edit draw color", value = "#777"),
          sliderInput(
            ns("imgEditN"),
            tooltipLabel(
              "n",
              "The number of images to generate."
            ),
            1, 10, 1, 1
          ),
          selectInput(
            ns("imgEditSize"),
            tooltipLabel(
              "size",
              "The size of the generated images."
            ),
            c("256x256", "512x512", "1024x1024")
          ),
          textConsole(
            ns("imgEditPrompt"),
            tooltipLabel(
              tagList(
                "edit description",
                tags$small("[Enter = send, Shift+Enter = new line]")
              ),
              "A text description of the desired image(s). The maximum length is 1000 characters."
            )
          )
        )
      )
    ),
    imagesContainer(ns("imgEditContainer"))
  )
}

#' Module image edit (server)
#'
#' @inherit shinyapp_modX_roxygen params
#' @export
#'
shinyapp_modImageEdit <- function(id, api_key) {

  # moduleServer
  moduleServer(id, function(input, output, session) {
    LOG_PREFIX <- "shinyapp_modImageGenerator (server) - "
    log_debug(LOG_PREFIX, "starting..")

    ns <- session$ns

    # self-destroy observer to set the initial image edit "input" size
    menuEvent <- observeEvent(input$menu, {
      .menu <- req(input$menu)
      log_debug(LOG_PREFIX, "observeEvent(menuEvent <- observeEvent(input$menu, {..})")

      if (.menu == "image_edit") {
        session$sendCustomMessage(
          "oaiiShinyApp.images.edit",
          list(
            cmd = "resize",
            data = NULL
          )
        )
        menuEvent$destroy()
      }
    })

    # forward loaded image (js -> R -> js)
    observeEvent(input$imgEditFileIn, {
      log_debug(LOG_PREFIX, "observeEvent(input$imgEditFileIn, {..})")

      session$sendCustomMessage(
        "oaiiShinyApp.images.edit",
        list(
          cmd = "file",
          data = base64enc::base64encode(input$imgEditFileIn$datapath)
        )
      )
    })

    # forward edit bg color (js -> R -> js)
    observeEvent(input$imgEditColorBg, {
      log_debug(LOG_PREFIX, "observeEvent(input$imgEditColorBg, {..})")

      session$sendCustomMessage(
        "oaiiShinyApp.images.edit",
        list(
          cmd = "colorBg",
          data = input$imgEditColorBg
        )
      )
    })

    # forward edit draw color (js -> R -> js)
    observeEvent(input$imgEditColorDraw, {
      log_debug(LOG_PREFIX, "observeEvent(input$imgEditColorDraw, {..})")

      session$sendCustomMessage(
        "oaiiShinyApp.images.edit",
        list(
          cmd = "colorDraw",
          data = input$imgEditColorDraw
        )
      )
    })

    imgEditSets <- reactiveVal()

    # render images container
    output$imgEditContainer  <- renderUI({
      log_debug(LOG_PREFIX, "output$imgEditContainer  <- renderUI({..})")

      imagesSets(imgEditSets(), "imgEditContainer")
    })

    # send a request for new (edited) images
    observeEvent(input$imgEditPrompt, {
      .api_key <- req(api_key())
      .imgEditPrompt <- req(input$imgEditPrompt)
      .imgEditFileOut <- req(input$imgEditFileOut)
      log_debug(LOG_PREFIX, "observeEvent(input$imgEditPrompt, {..})")

      textConsoleDisable(session, "imgEditPrompt")

      res_content <- oaii::images_edit_request(
        api_key = .api_key,
        image = base64enc::base64decode(
          sub("data:image/png;base64,", "", .imgEditFileOut)
        ),
        prompt = .imgEditPrompt,
        size = input$imgEditSize,
        response_format = "b64_json",
        n = as.integer(input$imgEditN)
      )

      if (oaii::is_error(res_content)) {
        showNotification(res_content$message_long, type = "error")
      }
      else {
        imgEditSets(oaii::images_merge_sets(
          imgEditSets(),
          oaii::images_fech_set(res_content, .imgEditPrompt, input$imgEditSize)
        ))
        textConsoleReset(session, "imgEditPrompt")
      }
      textConsoleEnable(session, "imgEditPrompt")
    })
  })
}

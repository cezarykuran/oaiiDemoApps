#' Module chat (UI)
#'
#' @inherit shinyapp_modX_roxygen params
#' @export
#'
shinyapp_modChatUI <- function(id) {
  ns <- NS(id)
  fluidRow(
    column(4, wellPanelSm(
      selectInput(
        ns("chatModel"),
        tooltipLabel(
          "model",
          "ID of the model to use. See the model endpoint compatibility table for details on which models work with the Chat API."
        ),
        c("gpt-4", "gpt-4-1106-preview", "gpt-4-vision-preview", "gpt-4-32k", "gpt-3.5-turbo", "gpt-3.5-turbo-16k"),
        selected = "gpt-4"
      ),
      sliderInput(
        ns("chatN"),
        tooltipLabel(
          "n",
          "How many chat completion choices to generate for each input message."
        ),
        1, 50, 1, 1
      ),
      sliderInput(
        ns("chatMaxTokens"),
        tooltipLabel(
          "max_tokens",
          "The maximum number of tokens to generate in the chat completion."
        ),
        1, 1000, 200, 1
      ),
      sliderInput(
        ns("chatTemperature"),
        tooltipLabel(
          "temperature",
          "What sampling temperature to use, between 0 and 2. Higher values like 0.8 will make the output more random, while lower values like 0.2 will make it more focused and deterministic."
        ),
        0, 2, 0.7, 0.1
      ),
      sliderInput(
        ns("chatPresencePenalty"),
        tooltipLabel(
          "presence_penalty",
          "Number between -2.0 and 2.0. Positive values penalize new tokens based on whether they appear in the text so far, increasing the model's likelihood to talk about new topics."
        ),
        -2, 2, 0, 0.1
      ),
      sliderInput(
        ns("chatFrequencyPenalty"),
        tooltipLabel(
          "frequency_penalty",
          "Number between -2.0 and 2.0. Positive values penalize new tokens based on their existing frequency in the text so far, decreasing the model's likelihood to repeat the same line verbatim."
        ),
        -2, 2, 0, 0.1
      )
    )),
    column(8,
      dialogContainer(ns("chatDialogContainer"), TRUE, TRUE),
      wellPanelSm(
        textConsole(
          ns("chatQ"),
          tagList(
            "message content",
            tags$small("[Enter = send, Shift+Enter = new line]")
          )
        )
      )
    )
  )
}

#' Module chat (server)
#'
#' @inherit shinyapp_modX_roxygen params
#' @export
#'
shinyapp_modChat <- function(id, api_key) {
  # moduleServer
  shiny::moduleServer(id, function(input, output, session) {
    LOG_PREFIX <- "shinyapp_modChat (server) - "
    log_debug(LOG_PREFIX, "starting..")

    chatMessages <- reactiveVal()

    # render chat dialog container
    output$chatDialogContainer <- renderUI({
      log_debug(LOG_PREFIX, "output$chatDialogContainer <- renderUI({..})")
      dialogMessages(chatMessages(), session$ns("chatDialogContainer"))
    })
    
    # send message(s)
    observeEvent(input$chatQ, {
      .api_key <- req(api_key())
      .chatQ <- req(input$chatQ)
      log_debug(LOG_PREFIX, "observeEvent(input$chatQ, {..})")
      
      textConsoleDisable(session, "chatQ")
      
      q <- oaii::dialog_df(.chatQ)
      res_content <- oaii::chat_request(
        .api_key,
        oaii::merge_dialog_df(chatMessages(), q),
        model = input$chatModel,
        temperature = as.double(input$chatTemperature),
        n = as.integer(input$chatN),
        max_tokens = as.integer(input$chatMaxTokens),
        presence_penalty = as.double(input$chatPresencePenalty),
        frequency_penalty = as.double(input$chatFrequencyPenalty)
      )
      if (oaii::is_error(res_content)) {
        showNotification(res_content$message_long, type = "error")
      }
      else {
        a <- oaii::chat_fetch_messages(res_content)
        chatMessages(oaii::merge_dialog_df(chatMessages(), q, a))
        textConsoleReset(session, "chatQ")
      }
      textConsoleEnable(session, "chatQ")
    })
    
    # export chat to csv
    output$chatDialogContainerDownload <- shiny::downloadHandler(
      function() {
        log_debug(LOG_PREFIX, "output$chatDialogContainerDownload <- shiny::downloadHandler(..) [filename]")
        paste0("chat ", format(Sys.time(), "%Y.%m.%d %H.%M"), ".csv")
      },
      function(file) {
        log_debug(LOG_PREFIX, "output$chatDialogContainerDownload <- shiny::downloadHandler(..) [content]")
        
        res <- oaii::dialog_df_to_csv(chatMessages(), file)
        if (oaii::is_error(res)) {
          showNotification(
            paste0("An error occurred while preparing csv file, error message '", res$message, "'"),
            type = "error"
          )
        }
      }
    )
    
    # import csv file
    observeEvent(input$chatDialogContainerUpload, {
      log_debug(LOG_PREFIX, "observeEvent(input$chatDialogContainerUpload, {..})")
      
      datapath <- input$chatDialogContainerUpload$datapath
      dialog_df <- oaii::csv_to_dialog_df(datapath)
      if (oaii::is_error(dialog_df)) {
        showNotification(
          paste0("An error occurred while loading the file, error message '", dialog_df$message, "'"),
          type = "error"
        )
      }
      else {
        chatMessages(dialog_df)
        showNotification(
          paste0("File '", datapath ,"' uploaded successfully!"),
          type = "message"
        )
      }
      shinyjs::reset("chatDialogContainerUpload")
    })
  })
}

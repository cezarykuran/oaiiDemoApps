#' Module fine-tunes (UI)
#'
#' @inherit shinyapp_modX_roxygen params
#' @export
#'
shinyapp_modFineTuningUI <- function(id) {
  ns <- NS(id)
  tagList(
    wellPanelSm(
      fluidRow(
        column(2, selectInput(
          ns("fineTuningModel"),
          "model",
          c("gpt-3.5-turbo-1106", "gpt-3.5-turbo-0613", "babbage-002", "davinci-002", "gpt-4-0613")
        )),
        column(4, selectInput(
          ns("fineTuningTrainingFile"),
          "training_file",
          choices = c()
        )),
        column(2, numericInput(
          ns("fineTuningNEpoch"),
          "n_epochs",
          value = 4, min = 1, max = 10, step = 1
        )),
        column(2, textInput(
          ns("fineTuningBatchSize"),
          "n_epochs",
          value = "auto"
        )),
        column(2, numericInput(
          ns("fineTuningLearningRateMultiplier"),
          "learning_rate_multiplier",
          value = 0.1, min = 0.02, max = 0.2, step = 0.01
        ))
      ),
      actionButton(ns("fineTuningCreate"), "Create", class = "btn-primary pull-right"),
      div(class = "clearfix")
    ),
    tableContainer(
      shiny::dataTableOutput(ns("fineTuningTable"))
    )
  )
}

#' Module files (server)
#'
#' @inherit shinyapp_modX_roxygen params
#' @param files_df files data frame (reactive value)
#' @export
#'
shinyapp_modFineTuning <- function(id, api_key, files_df) {

  # moduleServer
  moduleServer(id, function(input, output, session) {
    LOG_PREFIX <- "shinyapp_modFineTuning (server) - "
    log_debug(LOG_PREFIX, "starting..")

    ns <- session$ns

    fine_tuning_update <- reactiveVal(TRUE)
    trigger_fine_tuning_update <- function() {
      fine_tuning_update(!fine_tuning_update())
    }
  
    fine_tuning_df <- reactive({
      .api_key <- req(api_key())
      log_debug(LOG_PREFIX, "fine_tuning_df <- reactive({..})")
  
      fine_tuning_update()

      res_content <- oaii::fine_tuning_jobs_list_request(.api_key)
      if (oaii::is_error(res_content)) {
        showNotification(res_content$message_long, type = "error")
        data.frame()
      }
      else {
        oaii::fine_tuning_fetch_jobs_list(res_content)
      }
    })
  
    # df_col_obj_implode html table version (helper)
    fine_tuning_df_implode_table <- function(df, col, obj_prop = NULL, nested = TRUE) {
      oaii::df_col_obj_implode(
        df, col, obj_prop, nested,
        objs_glue = "",
        cell_header = "<table class='tdTable'>",
        cell_footer = "</table>",
        prop_fmt = "<tr><td>%s</td><td>%s</td></tr>"
      )
    }

    # update files selectize input
    observeEvent(files_df(), {
      log_debug(LOG_PREFIX, "observeEvent(files_df(), {..}) - update input$fineTuningTrainingFile")
  
      # default (empty)
      choices <- c()
  
      # update by files data.frame
      .files_df <- files_df()
      df_choices_cols <- c("id", "filename")
      if (all(df_choices_cols %in% colnames(.files_df))) {
        choices_df <- .files_df[.files_df$purpose == "fine-tune", df_choices_cols]
        if (NROW(choices_df)) {
          choices  <- unlist(choices_df$id)
          names(choices) <- unlist(choices_df$filename)
        }
      }
      shiny::updateSelectInput(session, "fineTuningTrainingFile", choices = choices)
    }, ignoreNULL = TRUE)
  
    # send new fine-tunes request
    observeEvent(input$fineTuningCreate, {
      .api_key <- req(api_key())
      .fineTuningTrainingFile <- req(input$fineTuningTrainingFile)
      .fineTuningModel <- req(input$fineTuningModel)
      .fineTuningNEpoch <- req(input$fineTuningNEpoch)
      .fineTuningBatchSize <- req(input$fineTuningBatchSize)
      .fineTuningLearningRateMultiplier <- req(input$fineTuningLearningRateMultiplier)

      res_content <- oaii::fine_tuning_create_job_request(
        .api_key,
        training_file = .fineTuningTrainingFile,
        model = .fineTuningModel,
        hyperparameters = list(
          n_epochs = as.integer(.fineTuningNEpoch),
          batch_size = .fineTuningBatchSize,
          learning_rate_multiplier = as.double(.fineTuningLearningRateMultiplier)
        )
      )
      if (oaii::is_error(res_content)) {
        showNotification(res_content$message_long, type = "error")
      }
      else {
        showNotification("Completion created successfully!", type = "message")
      }
      trigger_fine_tuning_update()
    })
  
    # render fine-tunes table
    output$fineTuningTable <- shiny::renderDataTable(
      expr = {
        log_debug(LOG_PREFIX, "output$fineTuning_table <- shiny::renderDataTable({..})")
        .fine_tuning_df <- fine_tuning_df()
        if (NROW(.fine_tuning_df)) {
          .fine_tuning_df %>%
            oaii::df_exclude_col(c("object", "organization_id")) %>%
            oaii::df_order_by_col("created_at", decreasing = TRUE) %>%
            oaii::df_col_dt_format(
              c("created_at", "finished_at")
            ) %>%
            fine_tuning_df_implode_table(
              c("training_file", "result_files")
            ) %>%
            fine_tuning_df_implode_table(
              "hyperparameters",
              nested = FALSE
            )
        }
        else data.frame()
      },
      options = list(
        searching = FALSE,
        columnDefs = list(),
        pageLength = 5
      ),
      escape = FALSE
    )
  })
}

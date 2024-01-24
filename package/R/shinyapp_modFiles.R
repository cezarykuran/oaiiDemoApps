#' Module files (UI)
#'
#' @inherit shinyapp_modX_roxygen params
#' @export
#'
shinyapp_modFilesUI <- function(id) {
  ns <- NS(id)
  tagList(
    wellPanelSm(
      fluidRow(
        column(8, fileInput(ns("filesUpload"), "file")),
        column(4, buttonContainer(
          actionButton(ns("filesUploadExecute"), "Create", class = "btn-primary")
        ))
      )
    ),
    tableContainer(
      shiny::dataTableOutput(ns("filesTable"))
    )
  )
}

#' Module files (server)
#'
#' @inherit shinyapp_modX_roxygen params
#' @param files_df files data frame (reactive value)
#' @export
#'
shinyapp_modFiles <- function(id, api_key, files_df) {

  # moduleServer
  moduleServer(id, function(input, output, session) {
    LOG_PREFIX <- "shinyapp_modFiles (server) - "
    log_debug(LOG_PREFIX, "starting..")

    ns <- session$ns

    files_df_update <- reactiveVal(TRUE)
    trigger_files_df_update <- function() {
      files_df_update(!files_df_update())
    }

    observe({
      .api_key <- req(api_key())
      log_debug(LOG_PREFIX, "observe({..}) - update files_df")
  
      files_df_update()

      res_content <- oaii::files_list_request(.api_key)
      files_df(
        if (oaii::is_error(res_content)) {
          showNotification(res_content$message_long, type = "error")
          data.frame()
        }
        else {
          oaii::files_fetch_list(res_content)
        }
      )
    })
  
    # append manage column (helper function)
    files_df_col_manage <- function(df, column, id) {
      col_manage <- lapply(seq_len(NROW(df)), function(row) {
        value <- df[row, "id"]
        as.list(paste0(
          tags$button(
            class = "btn btn-default btn-sm btn-xs",
            onclick = paste0("oaiiShinyApp.tableBtn('", ns("filesTableDownload"),"','", value, "')"),
            fontawesome::fa("download")
          ),
          tags$button(
            class = "btn btn-danger btn-sm btn-xs",
            onclick = paste0("oaiiShinyApp.tableBtn('", ns("filesTableRm"), "','", value, "')"),
            fontawesome::fa("trash")
          )
        ))
      })
      df$manage <- col_manage
      df
    }
  
    # send upload file request
    observeEvent(input$filesUploadExecute, {
      .api_key <- req(api_key())
      .filesUpload <- req(input$filesUpload)
      req(file.exists(.filesUpload$datapath))
      log_debug(LOG_PREFIX, "observeEvent(input$filesUploadExecute, {..})")
  
      file_uploaded <- file.path(
        dirname(.filesUpload$datapath),
        gsub("[^a-zA-Z0-9\\.]", "_", .filesUpload$name, perl = TRUE)
      )
      file.rename(.filesUpload$datapath, file_uploaded)
      res_content <- oaii::files_upload_request(
        .api_key,
        file_uploaded,
        "fine-tune"
      )
      if (oaii::is_error(res_content)) {
        showNotification(res_content$message_long, type = "error")
      }
      else {
        showNotification(
          paste0("File '", .filesUpload$name ,"' uploaded successfully!"),
          type = "message"
        )
      }
      unlink(file_uploaded)
      shinyjs::reset("filesUpload")
      trigger_files_df_update()
    })
  
    # render files table
    output$filesTable <- shiny::renderDataTable(
      expr = {
        log_debug(LOG_PREFIX, "output$filesTable <- shiny::renderDataTable({..})")
  
        .files_df <- files_df()
        if (NROW(.files_df)) {
          .files_df %>%
            oaii::df_exclude_col("object") %>%
            oaii::df_null_replace() %>%
            oaii::df_order_by_col("created_at", decreasing = TRUE) %>%
            oaii::df_col_dt_format("created_at") %>%
            files_df_col_manage("id", "filesTableRm")
        }
        else data.frame()
      },
      options = list(
        searching = FALSE,
        columnDefs = list()
      ),
      escape = FALSE
    )
  
    # send delete file request
    observeEvent(input$filesTableRm, {
      .api_key <- req(api_key())
      log_debug(LOG_PREFIX, "observeEvent(input$filesTableRm, {..})")
  
      res_content <- oaii::files_delete_request(
        .api_key,
        input$filesTableRm
      )
      if (oaii::is_error(res_content)) {
        showNotification(res_content$message_long, type = "error")
      }
      trigger_files_df_update()
    })

    # download file
    observeEvent(input$filesTableDownload, {
      .api_key <- req(api_key())
      .filesTableDownload <- req(input$filesTableDownload)
      .files_df <- req(files_df())
      log_debug(LOG_PREFIX, "observeEvent(input$filesTableDownload, {..})")
  
      res_content <-
        oaii::files_retrieve_content_request(.api_key, .filesTableDownload)
  
      if (oaii::is_error(res_content)) {
        showNotification(res_content$message_long, type = "error")
      }
      else {
        filename <- .files_df[.files_df$id == .filesTableDownload, "filename"]
        session$sendCustomMessage(
          "oaiiShinyApp.rDownload",
          list(
            filename = paste0(.filesTableDownload, "_", filename),
            content = res_content
          )
        )
      }
    })
  })
}

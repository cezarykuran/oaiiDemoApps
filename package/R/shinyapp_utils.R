#' shinyapp_modX roxygen helper
#'
#' @param id module id
#' @param api_key OpenAI api key
#'
shinyapp_modX_roxygen <- function(id, api_key) {
  NULL
}


## textConsole ----

textConsoleInputId <- function(id) paste0(id, "Input")

# create text console UI widget
textConsole <- function(id, label = NULL) {
  inputId <- textConsoleInputId(id)
  htmltools::tagList(
    shiny::textAreaInput(
      inputId,
      label,
      width = "100%",
      resize = "none",
    ),
    htmltools::tags$script(
      type = "text/javascript",
      paste0("oaiiShinyApp.textConsole.attachEvent('", id ,"', '", inputId ,"')")
    )
  )
}

# send command to console (helper)
textConsoleCommand <- function(session, id, command) {
  session$sendCustomMessage(
    "oaiiShinyApp.textConsole",
    list(inputId = session$ns(textConsoleInputId(id)), command = command)
  )
}
# disable console
textConsoleDisable <- function (session, id) {
  textConsoleCommand(session, id, "disable")
}
# enable console
textConsoleEnable <- function (session, id) {
  textConsoleCommand(session, id, "enable")
}
# reset console
textConsoleReset <- function (session, id) {
  textConsoleCommand(session, id, "reset")
}


## dialog ----

# create dialog UI widget
dialogContainer <- function(id, btnUpload = FALSE, btnDownload = FALSE) {
  htmltools::div(
    class = "oaii-chatDialogContainer",
    if (any(btnUpload, btnDownload))
      htmltools::div(
        class = "oaii-chatDialogContainerToolbar",
        if (btnDownload) shiny::downloadButton(paste0(id, "Download"), label = NULL),
        if (btnUpload) shiny::fileInput(paste0(id, "Upload"), label = NULL)
      ),
    shiny::uiOutput(id, class = "oaii-chatDialogContainerDialog"),
  )
}

# create content for the dialog widget from the messages
dialogMessages <- function(messages, idDialogContainer = NULL) {
  htmltools::tagList(
    htmltools::tags$table(
      class = "table",
      lapply(seq_len(NROW(messages)), function(row) {
        htmltools::tags$tr(
          htmltools::tags$td(messages[row, "role"]),
          htmltools::tags$td(htmltools::HTML(gsub("\n", "<br>", messages[row, "content"])))
        )
      })
    ),
    if (!is.null(idDialogContainer))
      htmltools::tags$script(paste0("oaiiShinyApp.scrollDown('", idDialogContainer  , "')"))
  )
}


## images ----

# create image UI widget (container)
imagesContainer <- function(id) {
  shiny::uiOutput(id, class = "oaii-imagesContainer")
}

# create image set content (helper)
imagesSet <- function(set) {
  download_filename <- gsub("[^a-zA-Z0-9-\\.]", "_", set$prompt, perl = TRUE)

  shiny::fluidRow(
    lapply(set$data, function(png64) {
      shiny::column(
        6, class = "col-md-4 col-lg-3",
        htmltools::div(
          class = "oaii-imagesSetImageContainer",
          onclick = "oaiiShinyApp.images.container.fsInOut(this)",
          htmltools::div(
            class = "oaii-imagesSetImage",
            htmltools::tags$img(
              src = paste0("data:image/png;base64,", png64)
            ),
            htmltools::tags$button(
              onclick = paste0(
                "oaiiShinyApp.images.container.download(this, event, '", download_filename, "')"
              ),
              shiny::icon("download")
            )
          )
        )
      )
    })
  )
}

# create content for the image widget from images sets object
imagesSets <- function(images, idContainer) {
  htmltools::tagList(
    lapply(rev(images), function(set) {
      htmltools::div(
        class = "oaii-imagesSetContainer",
        htmltools::div(
          class = "oaii-imagesSetPrompt",
          set$prompt
        ),
        imagesSet(set)
      )
    })
  )
}


## other ----

# container for table
tableContainer <- function(...) {
  htmltools::div(
    class = "oaii-tableContainer",
    div(
      class = "oaii-tableContainerContent",
      ...
    )
  )
}

# container for button
buttonContainer <- function(..., label = NULL) {
  htmltools::div(
    class = "form-group",
    htmltools::tags$label(
      class = "form-label",
      htmltools::HTML(paste0(label, "&nbsp;"))
    ),
    div(
      class = "oaii-tableContainerContent",
      ...
    )
  )
}

# create label with (?) icon (tooltip)
tooltipLabel <- function(label, tooltipContent, tooltipIcon = "question-circle") {
  bslib::tooltip(
    tagList(label, shiny::icon(tooltipIcon, class = "text-primary text-success")),
    tooltipContent
  )
}

# wellPanel with small inner margins
wellPanelSm <- function(...) {
  shiny::wellPanel(class = "well-sm", ...)
}

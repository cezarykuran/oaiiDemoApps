ui <- fluidPage(
  # resources ----
  shinyjs::useShinyjs(),
  includeScript(file.path("www", "widgets.js")),
  includeCSS(file.path("www", "widgets.css")),
  includeCSS(file.path("www", "app.css")),

  # tab content ----
  shiny::tabsetPanel(
    id = "menu",

    ## home ----
    shiny::tabPanel(
      shiny::icon("home"),
      value = "home",
      shiny::wellPanel(class = "well-sm",
        shiny::passwordInput(
          "api_key",
          "OpenAI API key",
          value = Sys.getenv("API_KEY", unset = "")
        ),
        htmltools::tagList(
          "The OpenAI API uses API keys for authentication. Visit your",
          htmltools::a(
            href = "https://platform.openai.com/account/api-keys",
            target = "_blank",
            "API keys page"
          ),
          "to retrieve the API key you'll use in your requests."
        )
      ),
      appMd("home")
    ),

    ## chat ----
    shiny::tabPanel(
      "Chat",
      apiPanel(
        md_file = "chat",
        oaiiDemoApps::shinyapp_modChatUI("chat")
      )
    ),

    ## files ----
    shiny::tabPanel(
      "Files",
      apiPanel(
        md_file = "files",
        oaiiDemoApps::shinyapp_modFilesUI("files")
      )
    ),

    ## fine-tunes ----
    shiny::tabPanel(
      "Fine-tuning",
      apiPanel(
        md_file = "fine-tunes",
        oaiiDemoApps::shinyapp_modFineTuningUI("ft")
      )
    ),

    ## images generator ----
    shiny::tabPanel(
      "Image generator",
      apiPanel(
        md_file = "image-generator",
        oaiiDemoApps::shinyapp_modImagesGeneratorUI("imgg")
      )
    ),

    ## image edit ----
    shiny::tabPanel(
      "Image edit",
      value = "image_edit",
      apiPanel(
        md_file = "image-edit",
        oaiiDemoApps::shinyapp_modImageEditUI("imge")
      )
    )
  )
)

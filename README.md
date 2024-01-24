# Demo applications for [oaii](https://github.com/cezarykuran/oaii.git) (OpenAI API R Interface)

- An interactive Shiny demo application, which can be accessed at https://r.cezarykuran.it/shiny/oaii/ (feel free to explore its functionality!).
- Several demonstration terminal applications written as R scripts. These applications serve as practical examples, demonstrating how to effectively leverage the package's functions for API communication.

## The ready-to-use applications

**Install package:**  
`remotes::install_github("https://github.com/cezarykuran/oaiiDemoApps.git", subdir = "package")`  
or  
`devtools::install_github("https://github.com/cezarykuran/oaiiDemoApps.git", subdir = "package")`

**Run applications:**  
`oaiiDemoApps::shinyapp()`  
`oaiiDemoApps::cli_chat("my-api-key")`  
`oaiiDemoApps::cli_completions_data_transmission("my-api-key")`

## Code review / learning
`git clone https://github.com/cezarykuran/oaiiDemoApps.git`

This package contains 3 separate applications:

- `R/cli_chat.R` - chat - standalone CLI application
- `R/cli_completions.R` - two example applications demonstrating the use of AI for problem solving
- `R/shinyapp*` - a collection of files constituting the Shiny application, where `shinyapp_mod*` files are directly related to specific endpoints.  

Take a look at these files and see how to use the [oaii](https://github.com/cezarykuran/oaii.git) in practice.

## Other

**[OpenAI](https://openai.com/) useful links:**

- [API reference](https://platform.openai.com/docs/api-reference/)
- [API help pages](https://help.openai.com/en/collections/3675931-openai-api)
- [docs](https://platform.openai.com/docs/introduction)
- [openai-cookbook](https://github.com/openai/openai-cookbook/)

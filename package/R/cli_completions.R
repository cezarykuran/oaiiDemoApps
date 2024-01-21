#' CLI application utilizing the OpenAI completions endpoint with simple data
#'
#' This function sends four simple queries (simultaneously)
#' and then displays the answers provided by AI.
#'
#' @inheritParams oaii::completions_request
#' @export
#'
#' @examples
#' \dontrun{
#'   cli_completions_simle_data("super-secret-key")
#' }
#' 
cli_completions_simle_data <- function(api_key) {
  # prompt
  now <- format(
    as.POSIXct(as.integer(Sys.time()), origin="1970-01-01"),
    "%Y/%m/%d %H:%M:%S",
    usetz = FALSE
  )
  x2 <- as.integer(stats::runif(1, 0, 100))
  x <- as.integer(stats::runif(1, -10, 10))
  y <- as.integer(stats::runif(1, -10, 10))
  prompt <- c(
    paste0(
      "Problem: x^2 = ", x2, ", solve for x.\nAnswer: x="),
    paste0(
      "Problem x=", x, ", y=", y,", z=x+y, solve for z.\nAnswer: z="),
    paste0(
      "Date time format YYYY/MM/DD hh:mm:ss. Now is ", now, ".\n",
      "Display date and time based on string '2 days, 3 hours, 53 minutes and 43 seconds ago'"
    )
  )
  cli::cat_line("Prompt:", col = "green")
  cli::cli_ul(prompt)
  cli::cat_line()
  
  # send request
  log <- utils::capture.output({
    res <- oaii::completions_request(
      api_key,
      "gpt-3.5-turbo-instruct",
      prompt,
      temperature = 0
    )
  }, type = "message")
  
  # process response
  if (oaii::is_error(res)) {
    # display all logs
    for (n in seq_along(log)) {
      cli::cat_line(cli::col_red(log[n]))
    }
  } else {
    # fetch answer from response as data.frame
    text <- oaii::completions_fetch_text(res)
    
    # dump answer
    cli::cat_line("Answer from AI:", col = "green")
    cli::cli_ul(text$content)
  }
}

#' CLI application utilizing the OpenAI completions endpoint with data transmission
#'
# Function that takes a data frame as input and allows AI to perform
# computations on floating-point numbers. The resulting data is supplemented
# with differences relative to the expected values and displayed.
#'
#' @inheritParams oaii::completions_request
#' @export
#'
#' @examples
#' \dontrun{
#'   cli_completions_data_transmission("super-secret-key")
#' }
#' 
cli_completions_data_transmission <- function(api_key) {
  
  # input data
  df <- data.frame(
    a = 101:110 + stats::runif(10, -20, 20),
    b = 201:210 + stats::runif(10, -20, 20)
  )
  cli::cat_line("Input data.frame", col = "green")
  print(df)
  cli::cat_line()
  
  # prompt
  json_df <- jsonlite::toJSON(df, auto_unbox = TRUE, digits = 6)
  prompt <- paste0(
    "input data in JSON format: ", json_df, "\n",
    "treat columns a and b as the lengths of the legs of a right triangle ",
    "and insert column c with the lengths of the hypotenuses of that triangle ",
    "calculated using the Pythagorean theorem\n",
    "insert column `sum` as the sum of columns a and b\n",
    "display results in json format"
  )
  cli::cat_line("Prompt:", col = "green")
  cli::cat_line(prompt, "\n")
  
  # send request
  log <- utils::capture.output({
    res <- oaii::completions_request(
      api_key,
      "gpt-3.5-turbo-instruct",
      prompt,
      max_tokens = 1000,
      temperature = 0
    )
  }, type = "message")
  
  # process response
  if (oaii::is_error(res)) {
    # display all logs
    for (n in seq_along(log)) {
      cli::cat_line(cli::col_red(log[n]))
    }
  } else {
    # fetch answer(s) from response as data.frame
    text <- oaii::completions_fetch_text(res, ltrim = TRUE)
    df_out <- jsonlite::fromJSON(text$content)
    
    # add *_diff columns
    df_out$sum_diff <- df_out$sum - (df$a + df$b)
    df_out$c_diff <- df_out$c - sqrt(df$a*df$a + df$b*df$b)
    
    # dump data
    cli::cat_line(
      "data.frame from AI ",
      "(`*_diff` columns contain differences between values returned by AI and expected)",
      col = "green"
    )
    print(df_out)
  }
}

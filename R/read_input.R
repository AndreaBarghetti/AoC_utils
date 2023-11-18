#' read_aoc_input_with
#' read AoC input with a given function
#' @param year
#' year
#' @param day
#' day
#' @param .fun
#' function to read input with
#' @param ...
#' extra args to pass to .fun
#' @return
#' depends on .fun
#' @export
#'
read_aoc_input_with <- function(year, day, .fun, ...) {

  xargs = list(...)

  input_url <- paste0("https://adventofcode.com/",year,"/day/",day,"/input")
  cookie <- paste0("session=", Sys.getenv("AOC_SESSION_COOKIE"))

  if (cookie=="session=") {stop("env var AOC_SESSION_COOKIE not found")}

  response <- httr::GET(input_url, httr::add_headers(Cookie = cookie))

  # Check if the request was successful (HTTP status code 200)
  if (response$status_code == 200) {
    # Read the input data
    input_data <- httr::content(response, "text", encoding = 'UTF-8')

  } else {
    stop("Failed to retrieve input data.")
  }

  res <- do.call(.fun, args = c(list(input_data), xargs))

  return(res)
}

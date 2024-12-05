# url processing ----------------------------------------------------------


#' Identify links within a given url
#'
#' @param url string; url of interest to identify the urls within
#'
#' @return named vector with urls linked to within the provided url
#' @importFrom xml2 read_html
#' @importFrom rvest html_nodes html_attr html_text
#'

obtain_links <- function(url) {
  url_html <- xml2::read_html(url)

  links <- url_html |>
    rvest::html_nodes("a") |>
    rvest::html_attr("href")

  link_text <- url_html |>
    rvest::html_nodes("a") |>
    rvest::html_text() |>
    trimws()

  links <- setNames(
    links,
    nm = link_text
  )

  return(links)
}


#' Subset string from the right
#'
#' @param x the string
#' @param n integer; number of letters to subset from on the right
#'
#' @return subsetted string
#'
substr_right <- function(x, n){
  substr(x, nchar(x) - n + 1, nchar(x))
}


#' Download a file from a url to a temporary file
#'
#' @param excel_url sting; url of the file. Must have an xlsx or xls extension
#' @importFrom tools file_ext
#' @return filepath to where the temporary file is stored
download_temp_file <- function(excel_url) {
  temporary_directory <- tempdir()
  temp_file <- paste0(
    temporary_directory,
    "/",
    names(excel_url),
    ".",
    tools::file_ext(excel_url)
  )
  # tmp_file <- tempfile(
  #   pattern = names(excel_url),
  #   fileext = tools::file_ext(excel_url)
  # )

  download.file(
    excel_url,
    temp_file,
    quiet = TRUE,
    mode = "wb"
  )

  return(temp_file)
}



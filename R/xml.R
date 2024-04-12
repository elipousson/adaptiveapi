#' Convert data to an `xml_document` object
#'
#' Convert data from a bare list, character vector, or connection to an xml
#' document. using `xml2::as_xml_document` or
#' `xml2::read_xml`
#'
#' @param data A list passed to [xml2::as_xml_document()] or a character vector
#'   or connection passed to [xml2::read_xml()].
#' @inheritParams xml2::read_xml
#' @inheritParams xml2::write_xml
#' @param ... Additional parameters passed to [xml2::as_xml_document()] or
#'   [xml2::read_xml()]
#' @noRd
data_as_xml_document <- function(
    data,
    file = NULL,
    ...,
    encoding = "",
    verbose = FALSE) {
  if (is_bare_list(data)) {
    return(xml2::as_xml_document(data, ...))
  }

  xml2::read_xml(
    data,
    ...,
    encoding = encoding,
    verbose = verbose
  )
}

#' Write XML data to file
#'
#' @noRd
write_xml_data <- function(
    data,
    ...,
    file = NULL,
    overwrite = TRUE,
    options = "format") {
  if (!inherits(data, "xml_document")) {
    data <- data_as_xml_document(data, ...)
  }

  file <- file %||% fs::file_temp(ext = "xml")

  if (overwrite && fs::file_exists(file)) {
    fs::file_delete(file)
  }

  xml2::write_xml(
    data,
    file = file,
    options = options,
    encoding = "UTF-8"
  )

  file
}

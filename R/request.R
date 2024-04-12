#' Create a Workday Adaptive Planning API request
#'
#' @param base_url Base URL for the Workday Adaptive Planning API call. Defaults
#'   to `"https://api.adaptiveplanning.com/api/"`
#' @param version Workday Adaptive Planning API version. Defaults to
#'   `getOption("adaptiveapi.version", 39)`
#' @seealso [httr2::request()]
#' @export
adaptive_request <- function(
    base_url = "https://api.adaptiveplanning.com/api/",
    version = getOption("adaptiveapi.version", 39)) {
  httr2::request(
    paste0(base_url, "v", version)
  )
}

#' Create a Workday Adaptive Planning API request with a specified method
#'
#' Use [adaptive_request()]
#'
#' @inheritParams adaptive_request
#' @inheritParams adaptive_password_decrypt
#' @inheritParams adaptive_method_template
#' @param ... Additional named parameters passed to [adaptive_method_template()]
#' @export
adaptive_req_method <- function(
    req = NULL,
    method,
    caller_name = getOption("adaptiveapi.caller_name", "adaptiveapi"),
    username = Sys.getenv("ADAPTIVE_USERNAME"),
    password = Sys.getenv("ADAPTIVE_PASSWORD"),
    key = "ADAPTIVE_KEY",
    ...,
    version = getOption("adaptiveapi.version", 39)) {
  req <- req %||% adaptive_request(version = version)

  password <- adaptive_password_decrypt(password, key)

  xml_string <- adaptive_method_template(
    method = method,
    username = username,
    password = password,
    caller_name = caller_name,
    ...
  )

  data <- data_as_xml_document(xml_string)

  req_body_xml(req, data = data)
}


#' Add an XML file to a request body
#'
#' @param data An `xml_document`, a list coercible to an `xml_document`, or a
#'   character string or other object readable with [xml2::read_xml()]. Optional
#'   if `path` is supplied.
#' @param path  Optional if data is supplied.
#' @seealso [httr2::req_body_file()]
req_body_xml <- function(req,
                         data = NULL,
                         ...,
                         file = NULL,
                         overwrite = TRUE,
                         type = NULL) {
  if (is.null(file)) {
    file <- write_xml_data(
      data,
      ...,
      overwrite = overwrite
    )
  }

  httr2::req_body_file(
    req,
    path = file,
    type = type
  )
}

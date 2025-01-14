% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{adaptive_password_decrypt}
\alias{adaptive_password_decrypt}
\title{Use \code{\link[httr2:secrets]{httr2::secret_decrypt()}} to retrieve an encrypted password and decrypt
using a specified key}
\usage{
adaptive_password_decrypt(
  password = Sys.getenv("ADAPTIVE_PASSWORD"),
  key = "ADAPTIVE_KEY"
)
}
\description{
Both the encrypted password and the secret key are stored as environmental
variables. Add these by manually editing your R environment with
\code{\link[usethis:edit]{usethis::edit_r_environ()}}. Use \code{\link[httr2:secrets]{httr2::secret_make_key()}} to generate a key
and \code{\link[httr2:secrets]{httr2::secret_encrypt()}} to encrypt the password.
}

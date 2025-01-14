
<!-- README.md is generated from README.Rmd. Please edit that file -->

# adaptiveapi

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![License:
MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Project Status: Concept – Minimal or no implementation has been done
yet, or the repository is only intended to be a limited example, demo,
or
proof-of-concept.](https://www.repostatus.org/badges/latest/concept.svg)](https://www.repostatus.org/#concept)
<!-- badges: end -->

The goal of adaptiveapi is to support access to [Workday Adaptive
Planning’s
API](https://doc.workday.com/adaptive-planning/en-us/integration/managing-data-integration/api-documentation/cbi1623709771566.html?toc=1.3.0)
using the `{httr2}` and `{xml2}` packages.

## Installation

You can install the development version of adaptiveapi like so:

``` r
pak::pkg_install("elipousson/adaptiveapi")
```

## Usage

The [Adaptive Planning REST
API](https://doc.workday.com/adaptive-planning/en-us/integration/managing-data-integration/api-documentation/understanding-the-adaptive-planning-rest-api/tgm1623708513156.html)
uses XML files as the body of HTTP POST requests.

This package has a helper function `req_body_xml()` that takes an
`xml_document` object, writes the document to disk, and includes the
file in the POST request. The package also includes helper functions for
creating the request, validating [the API
method](https://doc.workday.com/adaptive-planning/en-us/integration/managing-data-integration/api-documentation/understanding-the-adaptive-planning-rest-api/api-methods/brk1623709249507.html),
and including the username and password in the XML to authenticate the
request.

However, as of April 2024, this package is a bare-bones experiment that
only includes a template for the exportCustomerLogo API method.

``` r
library(adaptiveapi)

req <- adaptive_request() |> 
  adaptive_req_method(
  method = "exportCustomerLogo",
  username = Sys.getenv("ADAPTIVE_USERNAME"),
  password = Sys.getenv("ADAPTIVE_PASSWORD")
)

resp <- httr2::req_perform(req)

resp |> 
  httr2::resp_body_xml() |> 
  xml2::as_list()
```

The username and password should be stored as environmental variables as
illustrated in the example below. The password can be encrypted by using
`httr2::secret_make_key()` to generate a key and
`httr2::secret_encrypt()` to encrypt the password. The key, username,
and password can be stored as environmental variables with
`usethis::edit_r_environ()`.

At present, this package does not include the Adaptive Planning JSON API
or any support for [using the API with Workday
Credentials](https://doc.workday.com/adaptive-planning/en-us/integration/managing-data-integration/api-documentation/understanding-the-adaptive-planning-rest-api/bit1623710301264.html?toc=1.3.1.4)
although the latter feature may be added in the future.

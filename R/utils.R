#' Use [httr2::secret_decrypt()] to retrieve an encrypted password and decrypt
#' using a specified key
#'
#' Both the encrypted password and the secret key are stored as environmental
#' variables. Add these by manually editing your R environment with
#' [usethis::edit_r_environ()]. Use [httr2::secret_make_key()] to generate a key
#' and [httr2::secret_encrypt()] to encrypt the password.
#'
#' @export
#' @importFrom httr2 secret_decrypt
adaptive_password_decrypt <- function(
    password = Sys.getenv("ADAPTIVE_PASSWORD"),
    key = "ADAPTIVE_KEY") {
  httr2::secret_decrypt(password, key)
}

#' Match a specified
#' @param method Name of a Workday Adaptive Planning API Method.
#' @keywords internal
adaptive_method_match <- function(
    method,
    ...,
    error_call = caller_env()) {
  arg_match(
    method,
    c(
      "createAccount", "createDimension", "createDimensionValue",
      "createLevel", "createUser", "customReportValues", "exportAccounts",
      "exportActiveCurrencies", "exportAttributes", "exportCalendar",
      "exportConfigurableModelData", "exportCustomerLogo", "exportData",
      "exportDimensionFamilies", "exportDimensions", "exportDimensionMapping",
      "exportGroups", "exportInstances", "exportLevels", "exportLocales",
      "exportModeledSheet", "exportRoles", "exportPermissionSets",
      "exportSecurityAudit", "exportSheetDefinition", "exportSheets",
      "exportTime", "exportTransactionDefinition", "exportUsers",
      "exportVersions", "importDimensionMapping", "importGroups",
      "importModeledSheet", "publishChanges", "unpublishedChangesStatus",
      "updateAccessRules", "updateAccount", "updateAssociations",
      "updateDimension", "updateDimensionValue", "updateLevel", "updateUser",
      "updateAccounts", "updateAttributes", "updateDimensions", "updateLevels",
      "importConfigurableModelData", "importCubeData", "importStandardData",
      "importTransactions", "eraseActuals", "eraseData", "recalculateSheet"
    )
  )
}

#' Get a Workday Adapting Planning API template for a method
#'
#' Use [glue::glue_data()] to fill a template for a specified method with the
#' supplied parameters.
#'
#' @inheritParams adaptive_method_match
#' @keywords internal
adaptive_method_template <- function(
    method,
    ...) {
  templates <- list(
    "exportCustomerLogo" = '<call method="{method}" callerName="{caller_name}">'
  )

  method <- adaptive_method_match(method, error_call = error_call)

  glue::glue_data(
    .x = list2(
      method = method,
      ...
    ),
    '<?xml version="1.0" encoding="UTF-8"?>',
    templates[[method]],
    '<credentials login="{username}" password="{password}"/>',
    "</call>"
  )
}

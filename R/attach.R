# https://github.com/tidyverse/tidyverse/blob/master/R/attach.R
package_version2 <- function(x) {
  version <- as.character(unclass(utils::packageVersion(x))[[1]])

  if (length(version) > 3) {
    version[4:length(version)] <- crayon::red(as.character(version[4:length(version)]))
  }
  paste0(version, collapse = ".")
}

#https://github.com/tidyverse/tidyverse/blob/master/R/utils.R

text_col <- function(x) {
  # If RStudio not available, messages already printed in black
  if (!rstudioapi::isAvailable()) {
    return(x)
  }

  if (!rstudioapi::hasFun("getThemeInfo")) {
    return(x)
  }

  theme <- rstudioapi::getThemeInfo()

  if (isTRUE(theme$dark)) crayon::white(x) else crayon::black(x)

}


msg <- function(..., startup = FALSE) {
  if (startup) {
      packageStartupMessage(text_col(...))
  } else {
    message(text_col(...))
  }
}



attach_me <- function(){

  not_loaded <- to_load[!paste0("package:",to_load) %in% search()]

  if (length(not_loaded) == 0)
    return(invisible())

  msg(
    cli::rule(
      left = crayon::bold("Attaching packages"),
      right = paste0("gcloudR ", package_version2("gcloudR"))
    ),
    startup = TRUE
  )

  versions <- vapply(to_load, package_version2, character(1))
  packages <- paste0(
    crayon::green(cli::symbol$tick), " ", crayon::blue(format(to_load)), " ",
    crayon::col_align(versions, max(crayon::col_nchar(versions)))
  )

  col1 <- 1:floor(length(packages)/2)
  info <- paste0(packages[col1], "     ", packages[-col1])

  msg(paste(info, collapse = "\n"), startup = TRUE)

  options(googleAuthR.httr_oauth_cache = "gcs.oauth")

  suppressPackageStartupMessages(
    lapply(to_load, library, character.only = TRUE, warn.conflicts = FALSE)
  )

  msg(
    cli::rule(
      left = crayon::bold("Authentication"),
      right = paste0("gcloudR ", package_version2("gcloudR"))
    ),
    startup = TRUE
  )
  tryCatch({
    project_id <- googleAuthR::gar_set_client(scopes = "https://www.googleapis.com/auth/cloud-platform")

    msg(paste(crayon::green(cli::symbol$tick),
              "Google client ID set for project",
              crayon::blue(project_id))
        )
    suppressMessages(googleComputeEngineR::gce_global_project(project_id))
    suppressMessages(bigQueryR::bqr_global_project(project_id))
  },

    error = function(ex){
      msg(paste(crayon::red(cli::symbol$cross),
                "Google client ID not set, see ?gar_set_client")
      )
    })

  if(getOption("googleAuthR.scopes.selected") != ""){
    msg(paste(crayon::green(cli::symbol$tick),
              "Scopes set as",
              crayon::blue(getOption("googleAuthR.scopes.selected")))
    )
  } else {
    msg(paste(crayon::red(cli::symbol$cross),
              "No scopes are set via options(googleAuthR.scopes.selected)")
    )
  }

  if(isTRUE(inherits(googleAuthR::Authentication$public_fields$token, "Token2.0"))){
    msg(paste(crayon::green(cli::symbol$tick),
              "Authenticated via",
              crayon::blue(Authentication$public_fields$method))
    )
  } else {
    msg(paste(crayon::red(cli::symbol$cross),
              "Not authenticated")
    )
    googleAuthR::gar_token_info(1)
  }

  invisible()

}

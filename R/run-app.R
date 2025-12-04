#'
#' 
#' Helper function to locate app directory
#' 
#' @keywords internal
#' 
get_app_dir <- function(package = "mwanaApp") {
  app_dir <- system.file("app", package = package)
  if (app_dir == "") {
    stop("Could not find app directory. Try re-installing `mwanaApp`.", call. = FALSE)
  }
  app_dir
}

#'
#' 
#' 
#' Initialise built-in Shiny application
#' 
#' @param package package name (`mwanaApp`).
#' 
#' @return NULL
#' 
#' @examples
#' if (interactive()) run_mwana_app()
#' 
#' @export
#' 
#' 
# nocov start
run_mwana_app <- function(package = "mwanaApp") {
  shiny::runApp(appDir = get_app_dir(package), display.mode = "normal")
}
# nocov end
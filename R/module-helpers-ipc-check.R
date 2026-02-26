# ==============================================================================
#                         IPC Acute Malnutrition Checker
# ==============================================================================



#'
#'
#'
#' Display input variables dynamically, according to UI for screening
#' 
#' @param vars An object holding the data variable names. This is used to display
#' all the variables in the input variable of the UI. 
#' 
#' @param source User-selected source of data. As in the underlying method 
#' used to collect the data. Choices are survey, screening and sentinel.
#'
#' @param ns A placeholder for Shiny module namespace.
#' 
#' @returns A set of input variables specific for the user-selected source of 
#' data.
#' 
#' @keywords internal
#'
#'
mod_ipccheck_display_input_variables <- function(vars, source, ns) {
  ## Base list of variables ----
  base_list <- list(
    shiny::selectInput(ns("area1"),
      label = shiny::tagList(
        htmltools::tags$span("Area 1",
          style = "font-size: 14px; font-weight: bold;"
        ),
        htmltools::tags$span("*", style = "color: red;"),
        htmltools::tags$div(
          style = "font-size: 0.85em; color: #6c7574;", "(Primary area)"
        )
      ),
      choices = c("", vars)
    ),

    ##### Secondary grouping area: optional ----
    shiny::selectInput(ns("area2"),
      label = shiny::tagList(
        htmltools::tags$span("Area 2",
          style = "font-size: 14px; font-weight: bold;"
        ),
        htmltools::tags$div(
          style = "font-size: 0.85em; color: #6c7574;", "(Sub-area)"
        )
      ),
      choices = c("", vars)
    )
  )

  ## Conditional inputs depending on source ----
  ### Survey data ----
  if (source == "survey") {
    input_vars <- c(base_list, list(
      ##### Survey clusters: mandatory ----
      shiny::selectInput(
        inputId = ns("psu"),
        label = shiny::tagList(
          htmltools::tags$span("Survey clusters",
            style = "font-size: 14px; font-weight: bold;"
          ),
          htmltools::tags$span("*", style = "color: red;"),
        ),
        choices = c("", vars)
      )
    ))
  }

  ### Screening data ----
  if (source == "screening") {
    input_vars <- c(base_list, list(
      shiny::selectInput(
        inputId = ns("sites"),
        label = shiny::tagList(
          htmltools::tags$span("Screening sites",
            style = "font-size: 14px; font-weight: bold;"
          ),
          htmltools::tags$span("*", style = "color: red;"),
        ),
        choices = c("", vars)
      )
    ))
  }

  ### Sentinel sites data ----
  if (source == "sentinel") {
    input_vars <- c(base_list, list(
      shiny::selectInput(
        inputId = ns("ssites"),
        label = shiny::tagList(
          htmltools::tags$span("Sentinel sites",
            style = "font-size: 14px; font-weight: bold;"
          ),
          htmltools::tags$span("*", style = "color: red;"),
        ),
        choices = c("", vars)
      )
    ))
  }

  input_vars
}



#'
#'
#' Invoke mwana's IPC Acute Malnutrition minimum sample size requirement checker
#' from within the module server
#'
#' @param df,cluster,source,area1,area2 Input variables collected from the UI
#' and required to pass to mwana::mw_check_ipcamn_ssreq() function.
#' 
#' @returns A summary tibble containing check results for:
#' + n_clusters - the total number of unique clusters or screening or site identifiers;
#' + n_obs - the corresponding total number of children in the dataset; and,
#' + meet_ipc - whether the IPC AMN requirements were met.
#' 
#' @keywords internal
#'
#'
mod_ipccheck_call_checker <- function(
    df, cluster, source = character(),
    area1, area2) {
  ## Build group variables dynamically ----
  dots <- list()
  if (!is.null(area1) && nzchar(area1)) dots <- c(dots, list(rlang::sym(area1)))
  if (!is.null(area2) && nzchar(area2)) dots <- c(dots, list(rlang::sym(area2)))

  mwana::mw_check_ipcamn_ssreq(
    df = df,
    cluster = !!rlang::sym(cluster),
    .source = source,
    !!!dots
  )
}

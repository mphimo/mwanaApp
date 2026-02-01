# ==============================================================================
#                              Plausibility Checker
# ==============================================================================


#'
#' Display input variables dynamically, according to UI for screening
#'
#' @keywords internal
#'
mod_plausibility_display_input_variables <- function(vars, method, ns) {
  ### Base inputs always shown
  inputs <- list(
    shiny::selectInput(
      inputId = ns("area1"),
      label = shiny::tagList(
        htmltools::tags$span("Area 1", style = "font-size: 14px; font-weight: bold;"),
        htmltools::tags$div(style = "font-size: 0.85em; color: #6c7574;", "(Primary area)")
      ),
      choices = c("", vars)
    ),
    shiny::selectInput(
      inputId = ns("area2"),
      label = shiny::tagList(
        htmltools::tags$span("Area 2", style = "font-size: 14px; font-weight: bold;"),
        htmltools::tags$div(style = "font-size: 0.85em; color: #6c7574;", "(Sub-area)")
      ),
      choices = c("", vars)
    ),
    shiny::selectInput(
      inputId = ns("area3"),
      label = shiny::tagList(
        htmltools::tags$span("Area 3", style = "font-size: 14px; font-weight: bold;"),
        htmltools::tags$div(style = "font-size: 0.85em; color: #6c7574;", "(Sub-area)")
      ),
      choices = c("", vars)
    ),
    shiny::selectInput(
      inputId = ns("sex"),
      label = shiny::tagList(
        htmltools::tags$span("Sex", style = "font-size: 14px; font-weight: bold;"),
        htmltools::tags$span("*", style = "color: red;")
      ),
      choices = c("", vars)
    )
  )

  ### Conditional inputs depending on method
  if (method == "wfhz") {
    inputs <- c(inputs, list(
      shiny::selectInput(
        inputId = ns("age"),
        label = shiny::tagList(
          htmltools::tags$span("Age (months)",
            style = "font-size: 14px; font-weight: bold;"
          ),
          htmltools::tags$span("*", style = "color: red;")
        ),
        choices = c("", vars)
      ),
      shiny::selectInput(
        inputId = ns("weight"),
        label = shiny::tagList(
          htmltools::tags$span("Weight (kg)",
            style = "font-size: 14px; font-weight: bold;"
          ),
          htmltools::tags$span("*", style = "color: red;")
        ),
        choices = c("", vars)
      ),
      shiny::selectInput(
        inputId = ns("height"),
        label = shiny::tagList(
          htmltools::tags$span("Height (cm)",
            style = "font-size: 14px; font-weight: bold;"
          ),
          htmltools::tags$span("*", style = "color: red;")
        ),
        choices = c("", vars)
      )
    ))
  } else if (method == "mfaz") {
    inputs <- c(inputs, list(
      shiny::selectInput(
        inputId = ns("age"),
        label = shiny::tagList(
          htmltools::tags$span("Age (months)",
            style = "font-size: 14px; font-weight: bold;"
          ),
          htmltools::tags$span("*", style = "color: red;")
        ),
        choices = c("", vars)
      ),
      shiny::selectInput(
        inputId = ns("muac"),
        label = shiny::tagList(
          htmltools::tags$span("MUAC (cm)",
            style = "font-size: 14px; font-weight: bold;"
          ),
          htmltools::tags$span("*", style = "color: red;")
        ),
        choices = c("", vars)
      )
    ))
  } else {
    inputs <- c(inputs, list(
      shiny::selectInput(
        inputId = ns("muac"),
        label = shiny::tagList(
          htmltools::tags$span("MUAC (cm)",
            style = "font-size: 14px; font-weight: bold;"
          ),
          htmltools::tags$span("*", style = "color: red;")
        ),
        choices = c("", vars)
      )
    ))
  }

  # Always add flags at the end
  inputs_vars <- c(inputs, list(
    shiny::selectInput(
      inputId = ns("flags"),
      label = shiny::tagList(
        htmltools::tags$span("Flags", style = "font-size: 14px; font-weight: bold;"),
        htmltools::tags$span("*", style = "color: red;")
      ),
      choices = c("", vars)
    )
  ))

  inputs_vars
}

#'
#'
#' Invoke mwana's plausibility checkers dynamically from within module server,
#' according to user specifications in the UI
#'
#'
#' @keywords internal
#'
#'
#'
mod_plausibility_call_checker <- function(
    df, age = NULL, sex, muac = NULL, weight = NULL,
    height = NULL, flags, area1, area2, area3, .for = c("wfhz", "muac", "mfaz")) {
  ## Match options in `.for` ----
  .for <- match.arg(.for)

  ## Build grouping variables dynamically ----
  dots <- list()
  if (!is.null(area1) && nzchar(area1)) dots <- c(dots, list(rlang::sym(area1)))
  if (!is.null(area2) && nzchar(area2)) dots <- c(dots, list(rlang::sym(area2)))
  if (!is.null(area3) && nzchar(area3)) dots <- c(dots, list(rlang::sym(area3)))


  if (.for == "wfhz") {
    results <- mwana::mw_neat_output_wfhz(
      mwana::mw_plausibility_check_wfhz(
        df = df,
        sex = !!rlang::sym(sex),
        age = !!rlang::sym(age),
        weight = !!rlang::sym(weight),
        height = !!rlang::sym(height),
        flags = !!rlang::sym(flags),
        !!!dots
      )
    )
  } else if (.for == "mfaz") {
    results <- mwana::mw_neat_output_mfaz(
      mwana::mw_plausibility_check_mfaz(
        df = df,
        sex = !!rlang::sym(sex),
        muac = !!rlang::sym(muac),
        age = !!rlang::sym(age),
        flags = !!rlang::sym(flags),
        !!!dots
      )
    )
  } else {
    results <- mwana::mw_neat_output_muac(
      mwana::mw_plausibility_check_muac(
        df = df,
        sex = !!rlang::sym(sex),
        muac = !!rlang::sym(muac),
        flags = !!rlang::sym(flags),
        !!!dots
      )
    )
  }
}

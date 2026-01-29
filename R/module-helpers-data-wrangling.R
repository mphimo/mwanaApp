# ==============================================================================
#                                  Data Wrangling
# ==============================================================================

#'
#'
#' Display input variables dynamically, according to UI for screening
#'
#'
#' @keywords internal
#'
#'
mod_data_wrangling_display_input_variables <- function(vars, method, ns) {
  ### Base inputs always shown ----
  base_list <- list(
    #### Date of data collection: optional ----
    shiny::selectInput(
      inputId = ns("dos"),
      label = htmltools::tags$span("Date of data collection",
        style = "font-size: 14px; font-weight: bold;"
      ),
      choices = c("", vars)
    ),

    #### Date of birth: optional ----
    shiny::selectInput(
      inputId = ns("dob"),
      label = htmltools::tags$span("Date of birth",
        style = "font-size: 14px; font-weight: bold;"
      ),
      choices = c("", vars)
    )
  )

  ### Conditional inputs depending on method ----
  #### WFHZ ----
  if (method == "wfhz") {
    input_vars <- c(base_list, list(

      #### Age: optional ----
      shiny::selectInput(
        inputId = ns("age"),
        label = shiny::tagList(
          htmltools::tags$span("Age (months)",
            style = "font-size: 14px; font-weight: bold;"
          )
        ),
        choices = c("", vars)
      ),

      #### Sex: mandatory ----
      shiny::selectInput(
        inputId = ns("sex"),
        label = shiny::tagList(
          htmltools::tags$span("Sex",
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
  }

  #### MFAZ ----
  if (method == "mfaz") {
    input_vars <- c(base_list, list(
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

      #### Sex: mandatory ----
      shiny::selectInput(
        inputId = ns("sex"),
        label = shiny::tagList(
          htmltools::tags$span("Sex",
            style = "font-size: 14px; font-weight: bold;"
          ),
          htmltools::tags$span("*", style = "color: red;")
        ),
        choices = c("", vars)
      ),
      shiny::selectInput(
        inputId = ns("muac"),
        label = shiny::tagList(
          htmltools::tags$span("MUAC (mm)",
            style = "font-size: 14px; font-weight: bold;"
          ),
          htmltools::tags$span("*", style = "color: red;")
        ),
        choices = c("", vars)
      )
    ))
  }

  #### MUAC ----
  if (method == "muac") {
    input_vars <- list(
      shiny::selectInput(
        inputId = ns("sex"),
        label = shiny::tagList(
          htmltools::tags$span("Sex",
            style = "font-size: 14px; font-weight: bold;"
          ),
          htmltools::tags$span("*", style = "color: red;")
        ),
        choices = c("", vars)
      ),
      shiny::selectInput(
        inputId = ns("muac"),
        label = shiny::tagList(
          htmltools::tags$span("MUAC (mm)",
            style = "font-size: 14px; font-weight: bold;"
          ),
          htmltools::tags$span("*", style = "color: red;")
        ),
        choices = c("", vars)
      )
    )
  }

  #### Combined ----
  if (method == "combined") {
    input_vars <- c(base_list, list(
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

      #### Sex: mandatory ----
      shiny::selectInput(
        inputId = ns("sex"),
        label = shiny::tagList(
          htmltools::tags$span("Sex",
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

      ###### Height: mandatory ----
      shiny::selectInput(
        inputId = ns("height"),
        label = shiny::tagList(
          htmltools::tags$span("Height (cm)",
            style = "font-size: 14px; font-weight: bold;"
          ),
          htmltools::tags$span("*", style = "color: red;")
        ),
        choices = c("", vars)
      ),
      shiny::selectInput(
        inputId = ns("muac"),
        label = shiny::tagList(
          htmltools::tags$span("MUAC (mm)",
            style = "font-size: 14px; font-weight: bold;"
          ),
          htmltools::tags$span("*", style = "color: red;")
        ),
        choices = c("", vars)
      )
    ))
  }

  input_vars
}
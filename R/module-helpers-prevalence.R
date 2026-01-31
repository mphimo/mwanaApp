# ==============================================================================
#                              Prevalence Estimators
# ==============================================================================

#'
#'
#' Display input variables dynamically, according to UI for screening
#'
#' @keywords internal
#'
#'
mod_prevalence_display_input_variables <- function(
    vars, source, indicator_surv, has_age, ns) {
  ### Base list input vars ----
  inputs <- list(
    shiny::selectInput(
      inputId = ns("area1"),
      label = shiny::tagList(
        htmltools::tags$span("Area 1",
          style = "font-size: 14px; font-weight: bold;"
        ),
        htmltools::tags$div(
          style = "font-size: 0.85em; color: #6c7574;", "(Primary area)"
        )
      ),
      choices = c("", vars)
    ),
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
    ),
    shiny::selectInput(
      inputId = ns("area3"),
      label = shiny::tagList(
        htmltools::tags$span("Area 3",
          style = "font-size: 14px; font-weight: bold;"
        ),
        htmltools::tags$div(
          style = "font-size: 0.85em; color: #6c7574;", "Sub-area)"
        )
      ),
      choices = c("", vars)
    )
  )

  #### Conditional inputs depending on source of data ----
  if (source == "survey") {
    inputs <- c(inputs, list(
      if (isTRUE(indicator_surv == "muac")) {
        #### Display age ----
        shiny::tagList(
          shiny::selectInput(
            inputId = ns("muac"),
            label = shiny::tagList(
              htmltools::tags$span("MUAC",
                style = "font-size: 14px; font-weight: bold;"
              ),
              htmltools::tags$span("*", style = "color: red;")
            ),
            choices = c("", vars)
          ),
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
        inputId = ns("wts"),
        label = shiny::tagList(
          htmltools::tags$span("Survey weights",
            style = "font-size: 14px; font-weight: bold;"
          ),
          htmltools::tags$div(
            style = "font-size: 0.85em; color: #6c7574;",
            "Final survey weights for weighted analysis"
          )
        ),
        choices = c("", vars)
      )
        )
      }
    ))
  }

  if (source == "screening") {
    inputs <- c(inputs, list(
      shiny::selectInput(
        inputId = ns("muac"),
        label = shiny::tagList(
          htmltools::tags$span("MUAC",
            style = "font-size: 14px; font-weight: bold;"
          ),
          htmltools::tags$span("*", style = "color: red;")
        ),
        choices = c("", vars)
      ),
      if (isTRUE(has_age == "yes")) {
        shiny::selectInput(
          inputId = ns("age"),
          label = shiny::tagList(
            htmltools::tags$span("Age (months)",
              style = "font-size: 14px; font-weight: bold;"
            ),
            htmltools::tags$span("*", style = "color: red;")
          ),
          choices = c("", vars)
        )
      } else {
        shiny::selectInput(
          inputId = ns("age_cat"),
          label = shiny::tagList(
            htmltools::tags$span("Age categories (6-23 and 24-59)",
              style = "font-size: 14px; font-weight: bold;"
            ),
            htmltools::tags$span("*", style = "color: red;")
          ),
          choices = c("", vars)
        )
      }
    ))
  }

  # Always add oedema at the end
  inputs_vars <- c(inputs, list(
    shiny::selectInput(
      inputId = ns("oedema"),
      label = shiny::tagList(
        htmltools::tags$span("Oedema",
          style = "font-size: 14px; font-weight: bold;"
        )
      ),
      choices = c("", vars)
    )
  ))

  inputs_vars
}



#'
#'
#' Invoke mwana's prevalence functions from within module server according to
#' user specifications in the UI
#'
#'
#' @keywords internal
#'
#'
mod_prevalence_call_wfhz_prev_estimator <- function(
    df, wts = NULL, oedema = NULL,
    area1, area2, area3) {
  if (all(nzchar(c(area1, area2, area3)))) {
    if ((nzchar(wts) && nzchar(oedema))) {
      mwana::mw_estimate_prevalence_wfhz(
        df = df,
        wt = !!rlang::sym(wts),
        oedema = !!rlang::sym(oedema),
        !!rlang::sym(area1), !!rlang::sym(area2), !!rlang::sym(area3)
      )
    } else if (!nzchar(wts) && nzchar(oedema)) {
      mwana::mw_estimate_prevalence_wfhz(
        df = df,
        wt = NULL,
        oedema = !!rlang::sym(oedema),
        !!rlang::sym(area1), !!rlang::sym(area2), !!rlang::sym(area3)
      )
    } else if (nzchar(wts) && !nzchar(oedema)) {
      mwana::mw_estimate_prevalence_wfhz(
        df = df,
        wt = !!rlang::sym(wts),
        oedema = NULL,
        !!rlang::sym(area1), !!rlang::sym(area2), !!rlang::sym(area3)
      )
    } else {
      mwana::mw_estimate_prevalence_wfhz(
        df = df,
        wt = NULL,
        oedema = NULL,
        !!rlang::sym(area1), !!rlang::sym(area2), !!rlang::sym(area3)
      )
    }
  } else if (nzchar(area2) && !nzchar(area3)) {
    if (all(nzchar(c(wts, oedema)))) {
      mwana::mw_estimate_prevalence_wfhz(
        df = df,
        wt = !!rlang::sym(wts),
        oedema = !!rlang::sym(oedema),
        !!rlang::sym(area1), !!rlang::sym(area2)
      )
    } else if (!nzchar(wts) && nzchar(oedema)) {
      mwana::mw_estimate_prevalence_wfhz(
        df = df,
        wt = NULL,
        oedema = !!rlang::sym(oedema),
        !!rlang::sym(area1), !!rlang::sym(area2)
      )
    } else if (nzchar(wts) && !nzchar(oedema)) {
      mwana::mw_estimate_prevalence_wfhz(
        df = df,
        wt = !!rlang::sym(wts),
        oedema = NULL,
        !!rlang::sym(area1), !!rlang::sym(area2)
      )
    } else {
      mwana::mw_estimate_prevalence_wfhz(
        df = df,
        wt = NULL,
        oedema = NULL,
        !!rlang::sym(area1), !!rlang::sym(area2)
      )
    }
  } else {
    if (all(nzchar(c(wts, oedema)))) {
      mwana::mw_estimate_prevalence_wfhz(
        df = df,
        wt = !!rlang::sym(wts),
        oedema = !!rlang::sym(oedema),
        !!rlang::sym(area1)
      )
    } else if (!nzchar(wts) && nzchar(oedema)) {
      mwana::mw_estimate_prevalence_wfhz(
        df = df,
        wt = NULL,
        oedema = !!rlang::sym(oedema),
        !!rlang::sym(area1)
      )
    } else if (nzchar(wts) && !nzchar(oedema)) {
      mwana::mw_estimate_prevalence_wfhz(
        df = df,
        wt = !!rlang::sym(wts),
        oedema = NULL,
        !!rlang::sym(area1)
      )
    } else {
      mwana::mw_estimate_prevalence_wfhz(
        df = df,
        wt = NULL,
        oedema = NULL,
        !!rlang::sym(area1)
      )
    }
  }
}


#'
#'
#'
#' Invoke mwana's prevalence functions from within module server according to
#' user specifications in the UI
#'
#' @keywords internal
#'
#'
mod_prevalence_call_muac_prev_estimator <- function(
    df, age, muac, wts = NULL, oedema = NULL,
    area1, area2, area3) {
  if (all(nzchar(c(area1, area2, area3)))) {
    if ((nzchar(wts) && nzchar(oedema))) {
      mwana::mw_estimate_prevalence_muac(
        df = df,
        age = !!rlang::sym(age),
        muac = !!rlang::sym(muac),
        wt = !!rlang::sym(wts),
        oedema = !!rlang::sym(oedema),
        !!rlang::sym(area1), !!rlang::sym(area2), !!rlang::sym(area3)
      )
    } else if (!nzchar(wts) && nzchar(oedema)) {
      mwana::mw_estimate_prevalence_muac(
        df = df,
                age = !!rlang::sym(age),
        muac = !!rlang::sym(muac),
        wt = NULL,
        oedema = !!rlang::sym(oedema),
        !!rlang::sym(area1), !!rlang::sym(area2), !!rlang::sym(area3)
      )
    } else if (nzchar(wts) && !nzchar(oedema)) {
      mwana::mw_estimate_prevalence_muac(
        df = df,
                age = !!rlang::sym(age),
        muac = !!rlang::sym(muac),
        wt = !!rlang::sym(wts),
        oedema = NULL,
        !!rlang::sym(area1), !!rlang::sym(area2), !!rlang::sym(area3)
      )
    } else {
      mwana::mw_estimate_prevalence_muac(
        df = df,
                age = !!rlang::sym(age),
        muac = !!rlang::sym(muac),
        wt = NULL,
        oedema = NULL,
        !!rlang::sym(area1), !!rlang::sym(area2), !!rlang::sym(area3)
      )
    }
  } else if (nzchar(area2) && !nzchar(area3)) {
    if (all(nzchar(c(wts, oedema)))) {
      mwana::mw_estimate_prevalence_muac(
        df = df,
                age = !!rlang::sym(age),
        muac = !!rlang::sym(muac),
        wt = !!rlang::sym(wts),
        oedema = !!rlang::sym(oedema),
        !!rlang::sym(area1), !!rlang::sym(area2)
      )
    } else if (!nzchar(wts) && nzchar(oedema)) {
      mwana::mw_estimate_prevalence_muac(
        df = df,
                age = !!rlang::sym(age),
        muac = !!rlang::sym(muac),
        wt = NULL,
        oedema = !!rlang::sym(oedema),
        !!rlang::sym(area1), !!rlang::sym(area2)
      )
    } else if (nzchar(wts) && !nzchar(oedema)) {
      mwana::mw_estimate_prevalence_muac(
        df = df,
                age = !!rlang::sym(age),
        muac = !!rlang::sym(muac),
        wt = !!rlang::sym(wts),
        oedema = NULL,
        !!rlang::sym(area1), !!rlang::sym(area2)
      )
    } else {
      mwana::mw_estimate_prevalence_muac(
        df = df,
                age = !!rlang::sym(age),
        muac = !!rlang::sym(muac),
        wt = NULL,
        oedema = NULL,
        !!rlang::sym(area1), !!rlang::sym(area2)
      )
    }
  } else {
    if (all(nzchar(c(wts, oedema)))) {
      mwana::mw_estimate_prevalence_muac(
        df = df,
                age = !!rlang::sym(age),
        muac = !!rlang::sym(muac),
        wt = !!rlang::sym(wts),
        oedema = !!rlang::sym(oedema),
        !!rlang::sym(area1)
      )
    } else if (!nzchar(wts) && nzchar(oedema)) {
      mwana::mw_estimate_prevalence_muac(
        df = df,
                age = !!rlang::sym(age),
        muac = !!rlang::sym(muac),
        wt = NULL,
        oedema = !!rlang::sym(oedema),
        !!rlang::sym(area1)
      )
    } else if (nzchar(wts) && !nzchar(oedema)) {
      mwana::mw_estimate_prevalence_muac(
        df = df,
                age = !!rlang::sym(age),
        muac = !!rlang::sym(muac),
        wt = !!rlang::sym(wts),
        oedema = NULL,
        !!rlang::sym(area1)
      )
    } else {
      mwana::mw_estimate_prevalence_muac(
        df = df,
                age = !!rlang::sym(age),
        muac = !!rlang::sym(muac),
        wt = NULL,
        oedema = NULL,
        !!rlang::sym(area1)
      )
    }
  }
}


#'
#'
#'
#' Invoke mwana's prevalence functions from within module server according to
#' user specifications in the UI
#'
#' @keywords internal
#'
#'
mod_prevalence_call_combined_prev_estimator <- function(
    df, wts = NULL, oedema = NULL,
    area1, area2, area3) {
  if (all(nzchar(c(area1, area2, area3)))) {
    if ((nzchar(wts) && nzchar(oedema))) {
      mwana::mw_estimate_prevalence_combined(
        df = df,
        wt = !!rlang::sym(wts),
        oedema = !!rlang::sym(oedema),
        !!rlang::sym(area1), !!rlang::sym(area2), !!rlang::sym(area3)
      )
    } else if (!nzchar(wts) && nzchar(oedema)) {
      mwana::mw_estimate_prevalence_combined(
        df = df,
        wt = NULL,
        oedema = !!rlang::sym(oedema),
        !!rlang::sym(area1), !!rlang::sym(area2), !!rlang::sym(area3)
      )
    } else if (nzchar(wts) && !nzchar(oedema)) {
      mwana::mw_estimate_prevalence_combined(
        df = df,
        wt = !!rlang::sym(wts),
        oedema = NULL,
        !!rlang::sym(area1), !!rlang::sym(area2), !!rlang::sym(area3)
      )
    } else {
      mwana::mw_estimate_prevalence_combined(
        df = df,
        wt = NULL,
        oedema = NULL,
        !!rlang::sym(area1), !!rlang::sym(area2), !!rlang::sym(area3)
      )
    }
  } else if (nzchar(area2) && !nzchar(area3)) {
    if (all(nzchar(c(wts, oedema)))) {
      mwana::mw_estimate_prevalence_combined(
        df = df,
        wt = !!rlang::sym(wts),
        oedema = !!rlang::sym(oedema),
        !!rlang::sym(area1), !!rlang::sym(area2)
      )
    } else if (!nzchar(wts) && nzchar(oedema)) {
      mwana::mw_estimate_prevalence_combined(
        df = df,
        wt = NULL,
        oedema = !!rlang::sym(oedema),
        !!rlang::sym(area1), !!rlang::sym(area2)
      )
    } else if (nzchar(wts) && !nzchar(oedema)) {
      mwana::mw_estimate_prevalence_combined(
        df = df,
        wt = !!rlang::sym(wts),
        oedema = NULL,
        !!rlang::sym(area1), !!rlang::sym(area2)
      )
    } else {
      mwana::mw_estimate_prevalence_combined(
        df = df,
        wt = NULL,
        oedema = NULL,
        !!rlang::sym(area1), !!rlang::sym(area2)
      )
    }
  } else {
    if (all(nzchar(c(wts, oedema)))) {
      mwana::mw_estimate_prevalence_combined(
        df = df,
        wt = !!rlang::sym(wts),
        oedema = !!rlang::sym(oedema),
        !!rlang::sym(area1)
      )
    } else if (!nzchar(wts) && nzchar(oedema)) {
      mwana::mw_estimate_prevalence_combined(
        df = df,
        wt = NULL,
        oedema = !!rlang::sym(oedema),
        !!rlang::sym(area1)
      )
    } else if (nzchar(wts) && !nzchar(oedema)) {
      mwana::mw_estimate_prevalence_combined(
        df = df,
        wt = !!rlang::sym(wts),
        oedema = NULL,
        !!rlang::sym(area1)
      )
    } else {
      mwana::mw_estimate_prevalence_combined(
        df = df,
        wt = NULL,
        oedema = NULL,
        !!rlang::sym(area1)
      )
    }
  }
}


#'
#'
#'
#' Invoke mwana's prevalence functions from within module server according to
#' user specifications in the UI
#'
#' @keywords internal
#'
#'
#'
mod_prevalence_call_prev_estimator_screening <- function(
    df, age, muac, oedema = NULL,
    area1, area2, area3) {

    dots <- list(rlang::sym(area1))
  if (nzchar(area2)) dots <- c(dots, list(rlang::sym(area2)))
  if (nzchar(area3)) dots <- c(dots, list(rlang::sym(area3)))
  
  df <- dplyr::mutate(df, muac = !!rlang::sym(muac) * 10)

  # Create the call - pass oedema as NULL or as a symbol
    if (nzchar(oedema)) {
      result <- mwana::mw_estimate_prevalence_screening(
        df = df,
        age = !!rlang::sym(age),
        muac = df$muac,
        oedema = !!rlang::sym(oedema), 
        !!!dots
      )
    } else {
      result <- mwana::mw_estimate_prevalence_screening(
        df = df,
        age = !!rlang::sym(age),
        muac = df$muac,
        oedema = NULL,
        !!!dots
      )
    }
  result
}


#'
#'
#'
#' Invoke mwana's prevalence functions from within module server according to
#' user specifications in the UI
#'
#'
#' @keywords internal
#'
#'
mmod_prevalence_call_prev_estimator_screening2 <- function(
    df, age_cat, muac, oedema = NULL,
    area1, area2, area3) {

    dots <- list(rlang::sym(area1))
  if (nzchar(area2)) dots <- c(dots, list(rlang::sym(area2)))
  if (nzchar(area3)) dots <- c(dots, list(rlang::sym(area3)))
  
  df <- dplyr::mutate(df, muac = !!rlang::sym(muac) * 10)

  # Create the call - pass oedema as NULL or as a symbol
    if (nzchar(oedema)) {
      result <- mwana::mw_estimate_prevalence_screening2(
        df = df,
        age_cat = !!rlang::sym(age_cat),
        muac = df$muac,
        oedema = !!rlang::sym(oedema), 
        !!!dots
      )
    } else {
      result <- mwana::mw_estimate_prevalence_screening2(
        df = df,
        age_cat = !!rlang::sym(age_cat),
        muac = df$muac,
        oedema = NULL,
        !!!dots
      )
    }
  result
}


#'
#'
#' @keywords internal
#'
#'
mod_prevalence_neat_output_survey <- function(
    df,
    .type = c("wfhz", "muac", "combined")) {
  df <- dplyr::mutate(
    .data = df,
    dplyr::across(
      .cols = dplyr::ends_with(c("am_p", "am_p_low", "am_p_upp")),
      .fns = scales::label_percent(
        accuracy = 0.1, suffix = "%", decimal.mark = "."
      )
    )
  )

  if (.type %in% c("wfhz", "muac")) {
    df |>
      dplyr::relocate(.data$wt_pop, .before = .data$gam_n) |>
      dplyr::rename(
        "children (N)" = .data$N,
        "gam #" = .data$gam_n,
        "gam %" = .data$gam_p,
        "gam lcl" = .data$gam_p_low,
        "gam ucl" = .data$gam_p_upp,
        "gam deff" = .data$gam_p_deff,
        "sam #" = .data$sam_n,
        "sam %" = .data$sam_p,
        "sam lcl" = .data$sam_p_low,
        "sam ucl" = .data$sam_p_upp,
        "sam deff" = .data$sam_p_deff,
        "mam #" = .data$mam_n,
        "mam %" = .data$mam_p,
        "mam lcl" = .data$mam_p_low,
        "mam ucl" = .data$mam_p_upp,
        "mam deff" = .data$mam_p_deff
      )
  } else {
    df |>
      dplyr::relocate(.data$wt_pop, .before = .data$cgam_n) |>
      dplyr::rename(
        "children (N)" = .data$N,
        "cgam #" = .data$cgam_n,
        "cgam %" = .data$cgam_p,
        "cgam lcl" = .data$cgam_p_low,
        "cgam ucl" = .data$cgam_p_upp,
        "cgam deff" = .data$cgam_p_deff,
        "csam #" = .data$csam_n,
        "csam %" = .data$csam_p,
        "csam lcl" = .data$csam_p_low,
        "csam ucl" = .data$csam_p_upp,
        "csam deff" = .data$csam_p_deff,
        "cmam #" = .data$cmam_n,
        "cmam %" = .data$cmam_p,
        "cmam lcl" = .data$cmam_p_low,
        "cmam ucl" = .data$cmam_p_upp,
        "cmam deff" = .data$cmam_p_deff
      )
  }
}


#'
#'
#' @keywords internal
#'
#'
mod_prevalence_neat_output_screening <- function(df) {
  ## Get variable names ----
  names <- base::names(df)

  if ("gam_n" %in% names) {
     df <- dplyr::mutate(
      .data = df,
      dplyr::across(
        .cols = dplyr::contains("am_p"),
        .fns = scales::label_percent(
          accuracy = 0.1,
          suffix = "%",
          decimal.mark = "."
        )
      )
    ) |>
      dplyr::rename(
        "children (N)" = .data$N,
        "gam #" = .data$gam_n,
        "gam %" = .data$gam_p,
        "sam #" = .data$sam_n,
        "sam %" = .data$sam_p,
        "mam #" = .data$mam_n,
        "mam %" = .data$mam_p
      )
    
  } else {
df <- df |>
      dplyr::mutate(
        dplyr::across(
          .cols = dplyr::contains("am_p"),
          .fns = scales::label_percent(
            accuracy = 0.1,
            suffix = "%",
            decimal.mark = "."
          )
        )
      ) |>
      dplyr::rename(
        "children (N)" = .data$N,
        "gam %" = .data$gam_p,
        "sam %" = .data$sam_p,
        "mam %" = .data$mam_p
      )
    }
  df
}
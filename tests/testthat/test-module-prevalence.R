# ==============================================================================
#  Test Suite: Module Prevalence
# ==============================================================================


## ---- Survey data ------------------------------------------------------------


### WFHZ Prevalence ----

#### Skip test on windows ----
# if (identical(Sys.getenv("CI"), "true") && Sys.info()[["sysname"]] == "Windows") {
#   skip("Skipping shinytest2 integration tests on Windows CI to reduce runtime")
# }

testthat::test_that(
  desc = "Module works well to estimate prevalence of AMN by WFHZ from survey",
  code = {
    ### Initialise mwana app ----
    app <- shinytest2::AppDriver$new(
      app_dir = testthat::test_path("fixtures"),
      timeout = 120000,
      wait = TRUE
    )

    ### Wait the app to idle ----
    app$wait_for_idle(timeout = 40000)

    ### Click in the Data Upload tab ----
    app$click(selector = "a[data-value='Data Upload']")
    app$wait_for_idle(timeout = 40000)

    #### Read data ----
    data <- read.csv(
      file = testthat::test_path("fixtures", "anthro-01.csv"),
      check.names = FALSE
    )
    data["sex"] <- ifelse(data$sex == 1, "m", "f")
    tempfile <- tempfile(fileext = ".csv")
    write.csv(data, tempfile, row.names = FALSE)

    #### Upload onto the app ----
    app$upload_file(`upload_data-upload` = tempfile, wait_ = TRUE)

    ### Click on the data wrangling tab ----
    app$click(selector = "a[data-value='Data Wrangling']")
    app$wait_for_idle(timeout = 40000)

    ## App defaults to WFHZ ----
    ### Input variables ----
    app$set_inputs(`wrangle_data-dos` = "", wait_ = FALSE)
    app$set_inputs(`wrangle_data-dob` = "", wait_ = FALSE)
    app$set_inputs(`wrangle_data-sex` = "sex", wait_ = FALSE)
    app$set_inputs(`wrangle_data-weight` = "weight", wait_ = FALSE)
    app$set_inputs(`wrangle_data-height` = "height", wait_ = FALSE)

    ### Click wrangle button and wait the app to idle ----
    app$click(input = "wrangle_data-apply_wrangle")
    Sys.sleep(3)

    ### Click on the Prevalence tab and wait the app to idle ----
    app$click(selector = "a[data-value='Prevalence Analysis']")
    app$wait_for_idle(timeout = 40000)

    ### Select source of data ----
    app$set_inputs(`prevalence-source` = "survey", wait_ = FALSE)

    ### Select the method ----
    app$set_inputs(`prevalence-amn_method_survey` = "wfhz", wait_ = FALSE)
    app$set_inputs(`prevalence-area1` = "province", wait_ = FALSE)
    app$set_inputs(`prevalence-area2` = "strata", wait_ = FALSE) ## Assume sex as grouping var
    app$set_inputs(`prevalence-area3` = "sex", wait_ = FALSE)
    app$set_inputs(`prevalence-wts` = "wtfactor", wait_ = FALSE)
    app$set_inputs(`prevalence-oedema` = "oedema", wait_ = FALSE)

    ### Click on Estime Prevalence button ----
    app$click(input = "prevalence-estimate")
    app$wait_for_value(output = "prevalence-results", timeout = 40000)

    ### Capture JavaScript expressions to return results's cols and values ----
    js_cols <- "$('#prevalence-results thead th').map(function()
    {return $(this).text();}).get();"

    js_values <- "$('#prevalence-results tbody tr').map(function()
    {return $(this).text();}).get();"

    ### Capture prevalence results of Nampula Province, Rural Strata ----
    glued_results <- app$get_js(js_values)[[3]]
    weighted_pop <- sub("1", "", stringr::str_extract(glued_results, "\\d{7}(?:)"))
    gam_prev <- stringr::str_extract(glued_results, "\\d\\.\\d")

    ### Test check ----
    testthat::expect_equal(length(app$get_js(js_cols)[1:19]), 19)
    testthat::expect_equal(as.numeric(weighted_pop), 292611)
    testthat::expect_equal(as.numeric(gam_prev), 6.1)

    ### Stop the app ----
    app$stop()
  }
)

### MUAC prevalence ----

#### Skip test on windows ----
# if (identical(Sys.getenv("CI"), "true") && Sys.info()[["sysname"]] == "Windows") {
#   skip("Skipping shinytest2 integration tests on Windows CI to reduce runtime")
# }

testthat::test_that(
  desc = "Module works well to estimate prevalence of AMN by MUAC from survey",
  code = {
    ### Initialise mwana app ----
    app <- shinytest2::AppDriver$new(
      app_dir = testthat::test_path("fixtures"),
      timeout = 120000,
      wait = TRUE
    )

    ### Wait the app to idle ----
    app$wait_for_idle(timeout = 40000)

    ### Click in the Data Upload tab ----
    app$click(selector = "a[data-value='Data Upload']")
    app$wait_for_idle(timeout = 40000)

    #### Read data ----
    data <- read.csv(
      file = testthat::test_path("fixtures", "anthro-01.csv"),
      check.names = FALSE
    )
    data["sex"] <- ifelse(data$sex == 1, "m", "f")
    tempfile <- tempfile(fileext = ".csv")
    write.csv(data, tempfile, row.names = FALSE)

    #### Upload onto the app ----
    app$upload_file(`upload_data-upload` = tempfile, wait_ = TRUE)

    ### Click on the data wrangling tab ----
    app$click(selector = "a[data-value='Data Wrangling']")
    app$wait_for_idle(timeout = 40000)

    ### Select data wrangling method and wait the app till idles ----
    app$set_inputs(`wrangle_data-wrangle` = "mfaz", wait_ = TRUE)
    app$wait_for_idle(timeout = 40000)

    ### Input variables ----
    app$set_inputs(`wrangle_data-dos` = "", wait_ = FALSE)
    app$set_inputs(`wrangle_data-dob` = "", wait_ = FALSE)
    app$set_inputs(`wrangle_data-age` = "age", wait_ = FALSE)
    app$set_inputs(`wrangle_data-sex` = "sex", wait_ = FALSE)
    app$set_inputs(`wrangle_data-muac` = "muac", wait_ = FALSE)

    ### Click wrangle button and wait the app to idle ----
    app$click(input = "wrangle_data-apply_wrangle")
    app$wait_for_idle(timeout = 40000)

    ### Click on the Prevalence tab and wait the app to idle ----
    app$click(selector = "a[data-value='Prevalence Analysis']")
    app$wait_for_idle(timeout = 40000)

    ### Select source of data ----
    app$set_inputs(`prevalence-source` = "survey", wait_ = FALSE)

    ### Select the method ----
    app$set_inputs(`prevalence-amn_method_survey` = "muac", wait_ = TRUE)
    app$set_inputs(`prevalence-area1` = "province", wait_ = FALSE)
    app$set_inputs(`prevalence-area2` = "strata", wait_ = FALSE) ## Assume sex as grouping var
    app$set_inputs(`prevalence-area3` = "sex", wait_ = FALSE)
    app$set_inputs(`prevalence-muac` = "muac", wait_ = FALSE)
    app$set_inputs(`prevalence-age` = "age", wait_ = FALSE)
    app$set_inputs(`prevalence-wts` = "wtfactor", wait_ = FALSE)
    app$set_inputs(`prevalence-oedema` = "oedema", wait_ = FALSE)

    ### Click on Estime Prevalence button ----
    app$click(input = "prevalence-estimate")
    app$wait_for_value(output = "prevalence-results", timeout = 40000)

    ### Capture JavaScript expressions to return results's cols and values ----
    js_cols <- "$('#prevalence-results thead th').map(function()
    {return $(this).text();}).get();"

    js_values <- "$('#prevalence-results tbody tr').map(function()
    {return $(this).text();}).get();"

    ### Get a JS expression ----
    js_result <- app$get_js(js_values)[[3]]

    ### Wait/validate that we have at least 3 values
    if (length(js_result) < 4) {
      #### Add a small delay and retry
      Sys.sleep(3)
      js_result <- app$get_js(js_values)[[3]]
    }
    ### Capture prevalence results of Zambezia Province, Rural Strata ----
    prev <- stringr::str_extract_all(js_result, "\\d\\.\\d")[[1]]
    weighted_pop <- sub("1", "", stringr::str_extract(js_result, "\\d{7}(?:)"))

    ### Test check ----
    testthat::expect_equal(length(app$get_js(js_cols)[1:19]), 19)
    testthat::expect_equal(as.numeric(prev[2]), 7.7) # GAM
    testthat::expect_equal(as.numeric(weighted_pop), 307395)

    ### Stop the app ----
    app$stop()
  }
)

### Combined prevalence ----

#### Skip test on windows ----
# if (identical(Sys.getenv("CI"), "true") && Sys.info()[["sysname"]] == "Windows") {
#   skip("Skipping shinytest2 integration tests on Windows CI to reduce runtime")
# }

testthat::test_that(
  desc = "Module works well to estimate prevalence of combined AMN from survey",
  code = {
    ### Initialise mwana app ----
    app <- shinytest2::AppDriver$new(
      app_dir = testthat::test_path("fixtures"),
      timeout = 120000,
      wait = TRUE
    )

    ### Wait the app to idle ----
    app$wait_for_idle(timeout = 40000)

    ### Click in the Data Upload tab ----
    app$click(selector = "a[data-value='Data Upload']")
    app$wait_for_idle(timeout = 40000)

    #### Read data ----
    data <- read.csv(
      file = testthat::test_path("fixtures", "anthro-01.csv"),
      check.names = FALSE
    )
    data["sex"] <- ifelse(data$sex == 1, "m", "f")
    tempfile <- tempfile(fileext = ".csv")
    write.csv(data, tempfile, row.names = FALSE)

    #### Upload onto the app ----
    app$upload_file(`upload_data-upload` = tempfile, wait_ = TRUE)

    ### Click on the data wrangling tab ----
    app$click(selector = "a[data-value='Data Wrangling']")
    app$wait_for_idle(timeout = 40000)

    ### Select data wrangling method and wait the app till idles ----
    app$set_inputs(`wrangle_data-wrangle` = "combined", wait_ = TRUE)
    app$wait_for_idle(timeout = 40000)

    ### Input variables ----
    app$set_inputs(`wrangle_data-dos` = "", wait_ = FALSE)
    app$set_inputs(`wrangle_data-dob` = "", wait_ = FALSE)
    app$set_inputs(`wrangle_data-age` = "age", wait_ = FALSE)
    app$set_inputs(`wrangle_data-sex` = "sex", wait_ = FALSE)
    app$set_inputs(`wrangle_data-weight` = "weight", wait_ = FALSE)
    app$set_inputs(`wrangle_data-height` = "height", wait_ = FALSE)
    app$set_inputs(`wrangle_data-muac` = "muac", wait_ = FALSE)

    ### Click wrangle button and wait the app to idle ----
    app$click(input = "wrangle_data-apply_wrangle")
    app$wait_for_idle(timeout = 40000)

    ### Click on the Prevalence tab and wait the app to idle ----
    app$click(selector = "a[data-value='Prevalence Analysis']")
    app$wait_for_idle(timeout = 40000)

    ### Select source of data ----
    app$set_inputs(`prevalence-source` = "survey", wait_ = FALSE)

    ### Select the method ----
    app$set_inputs(`prevalence-amn_method_survey` = "combined", wait_ = TRUE)
    app$set_inputs(`prevalence-area1` = "province", wait_ = FALSE)
    app$set_inputs(`prevalence-area2` = "strata", wait_ = FALSE) ## Assume sex as grouping var
    app$set_inputs(`prevalence-area3` = "sex", wait_ = FALSE)
    app$set_inputs(`prevalence-wts` = "wtfactor", wait_ = FALSE)
    app$set_inputs(`prevalence-oedema` = "oedema", wait_ = FALSE)

    ### Click on Estime Prevalence button ----
    app$click(input = "prevalence-estimate")
    app$wait_for_value(output = "prevalence-results", timeout = 40000)

    ### Capture JavaScript expressions to return results's cols and values ----
    js_cols <- "$('#prevalence-results thead th').map(function()
    {return $(this).text();}).get();"

    js_values <- "$('#prevalence-results tbody tr').map(function()
    {return $(this).text();}).get();"

    ### Capture prevalence results of Nampula Province, Urban Strata ----
    glued_results <- app$get_js(js_values)[[3]]
    prev <- stringr::str_extract_all(glued_results, "\\d{2}\\.\\d")[[1]]
    weighted_pop <- sub("1", "", stringr::str_extract(glued_results, "\\d{7}(?:)"))

    ### Test check ----
    testthat::expect_equal(length(app$get_js(js_cols)[1:19]), 19)
    testthat::expect_equal(as.numeric(prev[2]), 10.8) # GAM
    testthat::expect_equal(as.numeric(weighted_pop), 288534)

    ### Stop the app ----
    app$stop()
  }
)


## ---- Screening data ---------------------------------------------------------


### When age is available ----

### Skip test on windows ----
# if (identical(Sys.getenv("CI"), "true") && Sys.info()[["sysname"]] == "Windows") {
#   skip("Skipping shinytest2 integration tests on Windows CI to reduce runtime")
# }

testthat::test_that(
  desc = "Module works well to estimate prevalence from screening",
  code = {
    ### Initialise mwana app ----
    app <- shinytest2::AppDriver$new(
      app_dir = testthat::test_path("fixtures"),
      timeout = 120000,
      wait = TRUE
    )

    ### Wait the app to idle ----
    app$wait_for_idle(timeout = 40000)

    ### Click in the Data Upload tab ----
    app$click(selector = "a[data-value='Data Upload']")
    app$wait_for_idle(timeout = 40000)

    #### Read data ----
    data <- read.csv(
      file = testthat::test_path("fixtures", "anthro-02.csv"),
      check.names = FALSE
    )
    ### Make age categories ----
    data <- data |>
      transform(oedema = dplyr::recode_values(oedema, "n " ~ "n"))

    tempfile <- tempfile(fileext = ".csv")
    write.csv(data, tempfile, row.names = FALSE)

    #### Upload onto the app ----
    app$upload_file(`upload_data-upload` = tempfile, wait_ = TRUE)

    ### Click on the data wrangling tab ----
    app$click(selector = "a[data-value='Data Wrangling']")
    app$wait_for_idle(timeout = 40000)

    ### Select data wrangling method and wait the app till idles ----
    app$set_inputs(`wrangle_data-wrangle` = "mfaz", wait_ = TRUE)
    app$wait_for_idle(timeout = 40000)

    ### Input variables ----
    app$set_inputs(`wrangle_data-dos` = "", wait_ = FALSE)
    app$set_inputs(`wrangle_data-dob` = "", wait_ = FALSE)
    app$set_inputs(`wrangle_data-age` = "age", wait_ = FALSE)
    app$set_inputs(`wrangle_data-sex` = "sex", wait_ = FALSE)
    app$set_inputs(`wrangle_data-muac` = "muac", wait_ = FALSE)

    ### Click wrangle button and wait the app to idle ----
    app$click(input = "wrangle_data-apply_wrangle")
    app$wait_for_idle(timeout = 40000)

    ### Click on the Prevalence tab and wait the app to idle ----
    app$click(selector = "a[data-value='Prevalence Analysis']")
    app$wait_for_idle(timeout = 40000)

    ### Select source of data ----
    app$set_inputs(`prevalence-source` = "screening", wait_ = TRUE)

    ### Select the method ----
    app$set_inputs(`prevalence-has_age` = "yes", wait_ = TRUE)
    app$set_inputs(`prevalence-area1` = "analysis_unit", wait_ = FALSE)
    app$set_inputs(`prevalence-area2` = "sex", wait_ = FALSE) ## Assume sex as grouping var
    app$set_inputs(`prevalence-area3` = "", wait_ = FALSE)
    app$set_inputs(`prevalence-muac` = "muac", wait_ = FALSE)
    app$set_inputs(`prevalence-age` = "age", wait = FALSE)
    app$set_inputs(`prevalence-oedema` = "oedema", wait_ = FALSE)

    ### Click on Estime Prevalence button ----
    app$click(input = "prevalence-estimate")
    app$wait_for_value(output = "prevalence-results", timeout = 40000)

    ### Capture JavaScript expressions to return results's cols and values ----
    js_cols <- "$('#prevalence-results thead th').map(function()
    {return $(this).text();}).get();"

    js_values <- "$('#prevalence-results tbody tr').map(function()
    {return $(this).text();}).get();"

    ### Capture results ----
    glued_results_unit_a <- app$get_js(js_values)[[1]]
    glued_results_unit_b <- app$get_js(js_values)[[2]]

    N_unit_a <- stringr::str_extract_all(glued_results_unit_a, "\\d{3}$")[[1]]
    N_unit_b <- stringr::str_extract_all(glued_results_unit_b, "\\d{4}$")[[1]]

    #### Prevalences ----
    prev_unit_a <- stringr::str_extract_all(glued_results_unit_a, "\\d\\.\\d")[[1]]
    prev_unit_b <- stringr::str_extract(glued_results_unit_b, "\\d{2}\\.\\d")[[1]]


    ### Test check ----
    testthat::expect_equal(length(app$get_js(js_cols)[1:9]), 9)
    testthat::expect_equal(as.numeric(N_unit_a), 608)
    testthat::expect_equal(as.numeric(N_unit_b), 1359)
    testthat::expect_equal(as.numeric(prev_unit_a)[1], 6.4) # GAM
    testthat::expect_equal(as.numeric(prev_unit_a)[2], 1.2) # SAM
    testthat::expect_equal(as.numeric(prev_unit_a)[3], 5.3) # MAM
    testthat::expect_equal(as.numeric(prev_unit_b), 12.4) # Age-weighted GAM

    ### Stop the app ----
    app$stop()
  }
)

### When age is given in categories ----

#### Skip test on windows ----
# if (identical(Sys.getenv("CI"), "true") && Sys.info()[["sysname"]] == "Windows") {
#   skip("Skipping shinytest2 integration tests on Windows CI to reduce runtime")
# }

testthat::test_that(
  desc = "Prevalence tab works as expected when age is given in categories",
  code = {
    #### Initialise app ----
    app <- shinytest2::AppDriver$new(
      app_dir = testthat::test_path("fixtures"),
      timeout = 120000,
      wait = TRUE
    )

    #### Wait app to idle ----
    app$wait_for_idle(timeout = 40000)

    #### Click on the Data Upload tab ----
    app$click(selector = "a[data-value='Data Upload']")

    app$wait_for_idle(timeout = 40000)

    #### Read data ----
    data <- read.csv(
      file = testthat::test_path("fixtures", "anthro-02.csv"),
      check.names = FALSE
    )

    ### Make age categories ----
    data <- data |>
      transform(
        age_cat = ifelse(age < 24, "6-23", "24-59"),
        oedema = dplyr::recode_values(oedema, "n " ~ "n")
      )

    tempfile <- tempfile(fileext = ".csv")
    write.csv(data, tempfile, row.names = FALSE)

    #### Upload onto the app ----
    app$upload_file(`upload_data-upload` = tempfile, wait_ = TRUE)

    ### Click on Data Wrangling tab ----
    app$click(selector = "a[data-value='Data Wrangling'")
    app$wait_for_idle(timeout = 40000)

    #### Select data wrangling method ----
    app$set_inputs("wrangle_data-wrangle" = "muac")
    app$wait_for_idle(timeout = 40000)

    #### Select variables ----
    app$set_inputs("wrangle_data-sex" = "sex", wait_ = FALSE)
    app$set_inputs("wrangle_data-muac" = "muac", wait_ = FALSE)

    #### Click on wrangle button ----
    app$click(input = "wrangle_data-apply_wrangle")
    app$wait_for_idle(timeout = 40000)

    #### Click on Prevalence tab ----
    app$click(selector = "a[data-value='Prevalence Analysis']")
    app$wait_for_idle(timeout = 40000)

    #### Select source of data ----
    app$set_inputs("prevalence-source" = "screening", wait_ = FALSE)
    app$wait_for_idle(timeout = 40000)

    #### Select if age is available ----
    app$set_inputs("prevalence-has_age" = "no", wait_ = TRUE)

    #### Select variables ----
    app$set_inputs("prevalence-area1" = "analysis_unit", wait_ = FALSE)
    app$set_inputs("prevalence-area2" = "sex", wait_ = FALSE)
    app$set_inputs("prevalence-area3" = "", wait_ = FALSE)
    app$set_inputs("prevalence-muac" = "muac", wait_ = FALSE)
    app$set_inputs("prevalence-age_cat" = "age_cat", wait_ = FALSE)
    app$set_inputs("prevalence-oedema" = "oedema", wait_ = FALSE)

    #### Click on Estimate Prevalence button ----
    app$click(input = "prevalence-estimate")
    #### Wait until output has been rendered ----
    app$wait_for_value(output = "prevalence-results", timeout = 40000)

    ### Capture JavaScript expressions to return results's cols and values ----
    js_cols <- "$('#prevalence-results thead th').map(function()
    {return $(this).text();}).get();"

    js_values <- "$('#prevalence-results tbody tr').map(function()
    {return $(this).text();}).get();"

    ### Capture prevalence ----
    glued_results_unit_a <- app$get_js(js_values)[[1]]
    glued_results_unit_b <- app$get_js(js_values)[[2]]

    N_unit_a <- stringr::str_extract_all(glued_results_unit_a, "\\d{3}$")[[1]]
    N_unit_b <- stringr::str_extract_all(glued_results_unit_b, "\\d{4}$")[[1]]

    #### Prevalences ----
    prev_unit_a <- stringr::str_extract_all(glued_results_unit_a, "\\d\\.\\d")[[1]]
    prev_unit_b <- stringr::str_extract(glued_results_unit_b, "\\d{2}\\.\\d")[[1]]


    ### Test check ----
    testthat::expect_equal(length(app$get_js(js_cols)[1:9]), 9)
    testthat::expect_equal(as.numeric(N_unit_a), 612)
    testthat::expect_equal(as.numeric(N_unit_b), 1365)
    testthat::expect_equal(as.numeric(prev_unit_a)[1], 6.4) # GAM
    testthat::expect_equal(as.numeric(prev_unit_a)[2], 1.1) # SAM
    testthat::expect_equal(as.numeric(prev_unit_a)[3], 5.2) # MAM
    testthat::expect_equal(as.numeric(prev_unit_b), 12.6) # Age-weighted GAM

    ### Stop the app ----
    app$stop()
  }
)

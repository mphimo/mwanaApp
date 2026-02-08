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

    ### Capture prevalence results ----
    prev <- "NampulaRural2430163.7%2.0%5.4%51.2%0.0%2.4%112.6%1.2%3.9%"

    ### Test check ----
    testthat::expect_equal(length(app$get_js(js_cols)[1:19]), 19)
    testthat::expect_equal(app$get_js(js_values)[[1]], prev)
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

    ### Capture prevalence results ----
    prev <- 
      "ZambeziaRural2125803.92485165.3%2.9%7.7%0.988318405269110441.3%-0.2%2.7%1.43002767958192124.0%1.6%6.4%1.295294076766007"

    ### Test check ----
    testthat::expect_equal(length(app$get_js(js_cols)[1:19]), 19)
    testthat::expect_equal(app$get_js(js_values)[[3]], prev)

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

    ### Capture prevalence results ----
    prev <- "NampulaUrban25425510.1%7.0%13.3%112.0%0.7%3.4%458.3%5.5%11.1%"

    ### Test check ----
    testthat::expect_equal(length(app$get_js(js_cols)[1:19]), 19)
    testthat::expect_equal(app$get_js(js_values)[[2]], prev)

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

    ### Capture prevalence results ----
    prev_unit_a <- "Unit A2396.4%71.2%325.3%608"
    prev_unit_b <- "Unit B212.4%3.2%9.2%1359"

    ### Test check ----
    testthat::expect_equal(length(app$get_js(js_cols)[1:9]), 9)
    testthat::expect_equal(app$get_js(js_values)[[1]], prev_unit_a)
    testthat::expect_equal(app$get_js(js_values)[[2]], prev_unit_b)

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

    ### Capture prevalence results ----
    prev_unit_a <- "Unit A2396.4%71.1%325.2%612"
    prev_unit_b <- "Unit B212.6%3.4%9.1%1365"
    ### Test check ----
    testthat::expect_equal(length(app$get_js(js_cols)[1:9]), 9)
    testthat::expect_equal(app$get_js(js_values)[[1]], prev_unit_a)
    testthat::expect_equal(app$get_js(js_values)[[2]], prev_unit_b)

    ### Stop the app ----
    app$stop()
  }
)

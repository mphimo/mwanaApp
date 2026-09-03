# ==============================================================================
#  Test Suite: Module Plausibility Check
# ==============================================================================

## ---- Plausibility Check on WFHZ data ----------------------------------------

testthat::test_that(desc = "Plausibility check module works well for WFHZ data", code = {
  ### Skip test on CRAN ----
  testthat::skip_on_cran()

  ### Initialise mwana app ----
  app <- shinytest2::AppDriver$new(
    app_dir = testthat::test_path("fixtures"),
    load_timeout = 120000,
    wait = TRUE
  )

  ### Let the app load ----
  app$wait_for_idle(timeout = 40000)

  ### Click on data upload tab ----
  app$click(selector = "a[data-value='Data Upload']")
  app$wait_for_idle(timeout = 40000)

  #### Find the data to upload ----
  data <- read.csv(
    file = testthat::test_path("fixtures", "anthro-01.csv"),
    check.names = FALSE
  )

  tempfile <- tempfile(fileext = ".csv")
  write.csv(data, tempfile, row.names = FALSE)

  #### Upload ----
  app$upload_file(`upload_data-upload` = tempfile, wait_ = TRUE)

  ### Click on the data wrangling tab ----
  app$click(selector = "a[data-value='Data Wrangling']")
  app$wait_for_idle(timeout = 40000)

  ### Select data wrangling method ----
  app$set_inputs(`wrangle_data-wrangle` = "wfhz", wait_ = FALSE)

  ### Select input variables ----
  app$set_inputs(`wrangle_data-dos` = "", wait_ = FALSE)
  app$set_inputs(`wrangle_data-dob` = "", wait_ = FALSE)
  app$set_inputs(`wrangle_data-age` = "", wait_ = FALSE)
  app$set_inputs(`wrangle_data-sex` = "sex", wait_ = FALSE)
  app$set_inputs(`wrangle_data-weight` = "weight", wait_ = FALSE)
  app$set_inputs(`wrangle_data-height` = "height", wait_ = FALSE)

  ### Click wrangle button ----
  app$click(input = "wrangle_data-apply_wrangle")
  app$wait_for_idle(timeout = 40000)

  ### Click on the Plausibility Check tab ----
  app$click(selector = "a[data-value='Plausibility Check']")
  app$wait_for_idle(timeout = 40000)

  ### Select method for plausibility check ----
  app$set_inputs(`plausible-method` = "wfhz", wait_ = FALSE)

  ### Select input variables ----
  app$set_inputs(`plausible-area1` = "province", wait_ = FALSE)
  app$set_inputs(`plausible-area2` = "strata", wait_ = FALSE)
  app$set_inputs(`plausible-area3` = "sex", wait_ = FALSE)
  app$set_inputs(`plausible-sex` = "sex", wait_ = FALSE)
  app$set_inputs(`plausible-age` = "age", wait_ = FALSE)
  app$set_inputs(`plausible-weight` = "weight", wait_ = FALSE)
  app$set_inputs(`plausible-height` = "height", wait_ = FALSE)
  app$set_inputs(`plausible-flags` = "flag_wfhz", wait_ = FALSE)

  ### Click on check plausibility button ----
  app$click(input = "plausible-check")
  app$wait_for_value(output = "plausible-checked", timeout = 40000)

  ### Capture JavaScript expressions to return results's cols and values ----
  js_cols <- "$('#plausible-checked thead th').map(function() {
      return $(this).text();}).get();"

  js_values <- "$('#plausible-checked tbody tr').map(function() 
    {return $(this).text();}).get();"

  ### Capture Zambezia-urban plausibility check results ----
  plausibility_results <-
    "ZambeziaRural23681.4%Excellent<0.001Problematic0.882Excellent9Good7Excellent0.92Excellent0.1Excellent0.2Excellent12Good"

  ### Test check -----
  testthat::expect_equal(
    as.character(app$get_js(js_cols)[1:22]),
    expected = c(
      "Province",
      "Strata",
      "Sex",
      "Total children",
      "Flagged data (%)",
      "Class. of flagged data",
      "Sex ratio (p)",
      "Class. of sex ratio",
      "Age ratio (p)",
      "Class. of age ratio",
      "DPS weight (#)",
      "Class. DPS weight",
      "DPS height (#)",
      "Class. DPS height",
      "Standard Dev* (#)",
      "Class. of standard dev",
      "Skewness* (#)",
      "Class. of skewness",
      "Kurtosis* (#)",
      "Class. of kurtosis",
      "Overall score",
      "Overall quality"
    )
  )
  testthat::expect_equal(app$get_js(js_values)[[3]], plausibility_results)
  ### Stop the app ----
  app$stop()
})


## ---- Plausibility Check on MFAZ data ----------------------------------------

testthat::test_that(desc = "Plausibility check module works well for MFAZ data", code = {
  ### Skip test on CRAN ----
  testthat::skip_on_cran()

  # Initialise app ----
  app <- shinytest2::AppDriver$new(
    app_dir = testthat::test_path("fixtures"),
    load_timeout = 120000,
    wait = TRUE
  )

  ### Let the app load ----
  app$wait_for_idle(timeout = 40000)

  ### Click on the Data uploading navbar ----
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

  ### Select data wrangling method ----
  app$set_inputs(
    `wrangle_data-wrangle` = "mfaz",
    wait_ = TRUE,
    timeout_ = 15000
  )
  app$wait_for_idle(timeout = 40000)

  app$set_inputs(`wrangle_data-dos` = "", wait_ = FALSE)
  app$set_inputs(`wrangle_data-dob` = "", wait_ = FALSE)
  app$set_inputs(`wrangle_data-age` = "age", wait_ = FALSE)
  app$set_inputs(`wrangle_data-sex` = "sex", wait_ = FALSE)
  app$set_inputs(`wrangle_data-muac` = "muac", wait_ = FALSE)

  ### Click wrangle button ----
  app$click(input = "wrangle_data-apply_wrangle")
  app$wait_for_idle(timeout = 40000)

  ### Click on the Plausibility Check tab ----
  app$click(selector = "a[data-value='Plausibility Check']")
  app$wait_for_idle(timeout = 40000)

  ### Select method for plausibility check ----
  app$set_inputs(`plausible-method` = "mfaz", wait_ = TRUE, timeout_ = 15000)

  ### Select input variables ----
  app$set_inputs(`plausible-area1` = "province", wait_ = FALSE)
  app$set_inputs(`plausible-area2` = "strata", wait_ = FALSE)
  app$set_inputs(`plausible-area3` = "sex", wait_ = FALSE)
  app$set_inputs(`plausible-sex` = "sex", wait_ = FALSE)
  app$set_inputs(`plausible-age` = "age", wait_ = FALSE)
  app$set_inputs(`plausible-muac` = "muac", wait_ = FALSE)
  app$set_inputs(`plausible-flags` = "flag_mfaz", wait_ = FALSE)

  ### Click on check plausibility button ----
  app$click(input = "plausible-check")
  app$wait_for_value(output = "plausible-checked", timeout = 40000)

  ### Capture JavaScript expressions to return results's cols and values ----
  js_cols <- "$('#plausible-checked thead th').map(function() {
      return $(this).text();}).get();"
  js_values <- "$('#plausible-checked tbody tr').map(function() 
    {return $(this).text();}).get();"

  ### Capture Nampula-urban plausibility check results ----
  plausibility_results <-
    "NampulaUrban26141.3%Good<0.001Problematic0.241Excellent10Good0.96Excellent-0.36Excellent0.27Good18Acceptable"

  ### Test check ----
  testthat::expect_equal(
    as.character(app$get_js(js_cols)[1:20]),
    expected = c(
      "Province",
      "Strata",
      "Sex",
      "Total children",
      "Flagged data (%)",
      "Class. of flagged data",
      "Sex ratio (p)",
      "Class. of sex ratio",
      "Age ratio (p)",
      "Class. of age ratio",
      "DPS (#)",
      "Class. of DPS",
      "Standard Dev* (#)",
      "Class. of standard dev",
      "Skewness* (#)",
      "Class. of skewness",
      "Kurtosis* (#)",
      "Class. of kurtosis",
      "Overall score",
      "Overall quality"
    )
  )
  testthat::expect_equal(app$get_js(js_values)[[2]], plausibility_results)
  ### Stop the app ----
  app$stop()
})


## ---- Plausibility Check on raw MUAC data ------------------------------------

testthat::test_that(desc = "Plausibility check module works well for MUAC data", code = {
  ### Skip test on CRAN ----
  testthat::skip_on_cran()

  ### Initialise mwana app ----
  app <- shinytest2::AppDriver$new(
    app_dir = testthat::test_path("fixtures"),
    load_timeout = 120000,
    wait = TRUE
  )

  ### Let the app load ----
  app$wait_for_idle(timeout = 40000)

  ### Click on data upload tab ----
  app$click(selector = "a[data-value='Data Upload']")
  app$wait_for_idle(timeout = 40000)

  #### Find the data to upload ----
  data <- read.csv(
    file = testthat::test_path("fixtures", "anthro-01.csv"),
    check.names = FALSE
  )

  tempfile <- tempfile(fileext = ".csv")
  write.csv(data, tempfile, row.names = FALSE)

  #### Upload ----
  app$upload_file(`upload_data-upload` = tempfile, wait_ = TRUE)

  ### Click on the data wrangling tab ----
  app$click(selector = "a[data-value='Data Wrangling']")
  app$wait_for_idle(timeout = 40000)

  ### Select data wrangling method ----
  app$set_inputs(
    `wrangle_data-wrangle` = "muac",
    wait_ = TRUE,
    timeout_ = 15000
  )
  app$wait_for_idle(timeout = 40000)

  ### Select input variables ----
  app$set_inputs(`wrangle_data-sex` = "sex", wait_ = FALSE)
  app$set_inputs(`wrangle_data-muac` = "muac", wait_ = FALSE)

  ### Click wrangle button ----
  app$click(input = "wrangle_data-apply_wrangle")
  app$wait_for_idle(timeout = 40000)

  ### Click on the Plausibility Check tab ----
  app$click(selector = "a[data-value='Plausibility Check']")
  app$wait_for_idle(timeout = 40000)

  ### Select method for plausibility check ----
  app$set_inputs(`plausible-method` = "muac", wait_ = TRUE, timeout_ = 15000)
  app$wait_for_idle(timeout = 40000)

  ### Select input variables ----
  app$set_inputs(`plausible-area1` = "province", wait_ = FALSE)
  app$set_inputs(`plausible-area2` = "strata", wait_ = FALSE)
  app$set_inputs(`plausible-area3` = "sex", wait_ = FALSE)
  app$set_inputs(`plausible-sex` = "sex", wait_ = FALSE)
  app$set_inputs(`plausible-muac` = "muac", wait_ = FALSE)
  app$set_inputs(`plausible-flags` = "flag_muac", wait_ = FALSE)

  ### Click on check plausibility button
  app$click(input = "plausible-check")
  app$wait_for_value(output = "plausible-checked", timeout = 40000)

  ### Capture JavaScript expressions to return results's cols and values ----
  js_cols <- "$('#plausible-checked thead th').map(function() {
      return $(this).text();}).get();"
  js_values <- "$('#plausible-checked tbody tr').map(function() 
    {return $(this).text();}).get();"

  ### Capture Zambezia-urban plausibility check results ----
  plausibility_results <-
    "ZambeziaUrban28130.1%Excellent<0.001Problematic5Excellent13.48Acceptable"

  ### Test check -----
  testthat::expect_equal(
    as.character(app$get_js(js_cols)[1:12]),
    expected = c(
      "Province",
      "Strata",
      "Sex",
      "Total children",
      "Flagged data (%)",
      "Class. of flagged data",
      "Sex ratio (p)",
      "Class. of sex ratio",
      "DPS(#)",
      "Class. of DPS",
      "Standard Dev* (#)",
      "Class. of standard dev"
    )
  )

  testthat::expect_equal(app$get_js(js_values)[[4]], plausibility_results)

  ### Stop the app ----
  app$stop()
})

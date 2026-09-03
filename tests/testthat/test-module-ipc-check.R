# ==============================================================================
#  Test Suite: Module IPC Check
# ==============================================================================

## ---- IPC check on survey data -----------------------------------------------

testthat::test_that("IPC check's server module behaves as expected on survey data", {
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

  ### Click on the Data uploading navbar ----
  app$click(selector = "a[data-value='Data Upload']")
  app$wait_for_idle(timeout = 40000)

  ### Upload data ----
  #### Read data ----
  data <- read.csv(
    file = testthat::test_path("fixtures", "anthro-01.csv"),
    check.names = FALSE
  )
  tempfile <- tempfile(fileext = ".csv")
  write.csv(data, tempfile, row.names = FALSE)

  #### Upload onto the app ----
  app$upload_file(`upload_data-upload` = tempfile, wait_ = TRUE)

  ### Click on the Data uploading navbar ----
  app$click(selector = "a[data-value='IPC Check']")
  app$wait_for_idle(timeout = 40000)

  #### Set IPC Check for survey data ----
  app$set_inputs(`ipc_check-ipccheck` = "survey", wait_ = FALSE)
  app$wait_for_idle(timeout = 40000)

  #### Now set parameters for survey ----
  app$set_inputs(`ipc_check-area1` = "province", wait_ = FALSE)
  app$set_inputs(`ipc_check-area2` = "strata", wait_ = FALSE)
  app$set_inputs(`ipc_check-psu` = "cluster", wait_ = FALSE)

  #### Run check ----
  app$click(input = "ipc_check-apply_check")
  app$wait_for_value(output = "ipc_check-checked", timeout = 40000)

  ### Capture JavaScript expressions to return results's cols and values ----
  js_cols <- "$('#ipc_check-checked thead th').map(function() {
      return $(this).text();}).get();"
  js_values <- "$('#ipc_check-checked tbody tr').map(function() 
    {return $(this).text();}).get();"

  ### Test check ----
  testthat::expect_true(app$get_js("$('#ipc_check-checked').length > 0"))
  testthat::expect_equal(
    as.character(app$get_js(js_cols)[1:5]),
    c("province", "strata", "n_clusters", "n_obs", "meet_ipc")
  )
  testthat::expect_equal(app$get_js(js_values)[[1]], "NampulaRural60472yes")
  testthat::expect_equal(app$get_js(js_values)[[3]], "ZambeziaRural51368yes")
})


## ---- IPC Check on screening data --------------------------------------------

testthat::test_that("IPC check's server module behaves as expected on screening data", {
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

  ### Click on the Data uploading navbar ----
  app$click(selector = "a[data-value='Data Upload']")
  app$wait_for_idle(timeout = 40000)

  ### Upload data ----
  #### Read data ----
  data <- read.csv(
    file = testthat::test_path("fixtures", "anthro-01.csv"),
    check.names = FALSE
  )
  tempfile <- tempfile(fileext = ".csv")
  write.csv(data, tempfile, row.names = FALSE)

  #### Upload onto the app ----
  app$upload_file(`upload_data-upload` = tempfile, wait_ = TRUE)

  #### Set IPC Check for screening data ----
  app$set_inputs(
    `ipc_check-ipccheck` = "screening",
    wait_ = TRUE,
    timeout_ = 10000
  )
  app$wait_for_idle(timeout = 40000)

  ### Click on the Data uploading navbar ----
  app$click(selector = "a[data-value='IPC Check']")
  app$wait_for_idle(timeout = 40000)

  #### Now set parameters for survey ----
  app$set_inputs(`ipc_check-area1` = "province", wait_ = FALSE)
  app$set_inputs(`ipc_check-area2` = "strata", wait_ = FALSE)
  app$set_inputs(`ipc_check-sites` = "cluster", wait_ = FALSE)

  #### Run check ----
  app$click(input = "ipc_check-apply_check")
  app$wait_for_value(output = "ipc_check-checked", timeout = 40000)

  ### Capture JavaScript expressions to return results's cols and values ----
  js_cols <- "$('#ipc_check-checked thead th').map(function() {
      return $(this).text();}).get();"
  js_values <- "$('#ipc_check-checked tbody tr').map(function() 
    {return $(this).text();}).get();"

  ### Test ----
  testthat::expect_true(app$get_js("$('#ipc_check-checked').length > 0"))
  testthat::expect_equal(
    as.character(app$get_js(js_cols)[1:5]),
    c("province", "strata", "n_clusters", "n_obs", "meet_ipc")
  )
  testthat::expect_equal(app$get_js(js_values)[[1]], "NampulaRural60472no")
  testthat::expect_equal(app$get_js(js_values)[[3]], "ZambeziaRural51368no")
})


## ---- IPC Check on sentinel site data ----------------------------------------

testthat::test_that("IPC check's server module behaves as expected on sentinel site data", {
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

  ### Click on the Data uploading navbar ----
  app$click(selector = "a[data-value='Data Upload']")
  app$wait_for_idle(timeout = 40000)

  ### Upload data ----
  #### Read data ----
  data <- read.csv(
    file = testthat::test_path("fixtures", "anthro-01.csv"),
    check.names = FALSE
  )
  tempfile <- tempfile(fileext = ".csv")
  write.csv(data, tempfile, row.names = FALSE)

  #### Upload onto the app ----
  app$upload_file(`upload_data-upload` = tempfile, wait_ = TRUE)

  ### Click on the IPC Check nav bar ----
  app$click(selector = "a[data-value='IPC Check']")

  #### Set IPC Check for screening data ----
  app$set_inputs(
    `ipc_check-ipccheck` = "sentinel",
    wait_ = TRUE,
    timeout_ = 10000
  )
  app$wait_for_idle(timeout = 40000)

  #### Now set parameters for survey ----
  app$set_inputs(`ipc_check-area1` = "province", wait_ = FALSE)
  app$set_inputs(`ipc_check-area2` = "strata", wait_ = FALSE)
  app$set_inputs(`ipc_check-ssites` = "cluster", wait_ = FALSE)

  #### Run check ----
  app$click(input = "ipc_check-apply_check")
  app$wait_for_value(output = "ipc_check-checked", timeout = 40000)

  ### Capture JavaScript expressions to return results's cols and values ----
  js_cols <- "$('#ipc_check-checked thead th').map(function() {
      return $(this).text();}).get();"
  js_values <- "$('#ipc_check-checked tbody tr').map(function() 
    {return $(this).text();}).get();"

  ### Test ----
  testthat::expect_true(app$get_js("$('#ipc_check-checked').length > 0"))
  testthat::expect_equal(
    as.character(app$get_js(js_cols)[1:5]),
    c("province", "strata", "n_clusters", "n_obs", "meet_ipc")
  )
  testthat::expect_equal(app$get_js(js_values)[[1]], "NampulaRural60472yes")
  testthat::expect_equal(app$get_js(js_values)[[3]], "ZambeziaRural51368yes")
})

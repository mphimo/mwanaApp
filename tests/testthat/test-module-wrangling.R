# ==============================================================================
#  Test Suite: Module Data Wrangling
# ==============================================================================


## ---- Data Wrangling: WFHZ ---------------------------------------------------


testthat::test_that(desc = "Server data wrangling works as expected for WFHZ", {
  ## Initialise app ----
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

  ### Click on the data wrangling tab ----
  app$click(selector = "a[data-value='Data Wrangling']")
  app$wait_for_idle(timeout = 40000)

  ### Select input variables ----
  app$set_inputs(`wrangle_data-wrangle` = "wfhz", wait_ = FALSE)
  app$set_inputs(`wrangle_data-dos` = "", wait_ = FALSE)
  app$set_inputs(`wrangle_data-dob` = "", wait_ = FALSE)
  app$set_inputs(`wrangle_data-age` = "", wait_ = FALSE)
  app$set_inputs(`wrangle_data-sex` = "sex", wait_ = FALSE)
  app$set_inputs(`wrangle_data-weight` = "weight", wait_ = FALSE)
  app$set_inputs(`wrangle_data-height` = "height", wait_ = FALSE)

  ### Click wrangle button ----
  app$click(input = "wrangle_data-apply_wrangle")
  app$wait_for_value(output = "wrangle_data-wrangled", timeout = 40000)

  ### Get wrangled values ----
  column_names <- app$get_js("
    $('#wrangle_data-wrangled thead th').map(function() {
      return $(this).text();
    }).get();
  ") |> as.character()

  testthat::expect_true(all(c("wfhz", "flag_wfhz") %in% column_names))
  testthat::expect_equal(length(column_names), 12)

  #### Stop the app ----
  app$stop()
})


### When user supplies dos and dob for age wrangling ----

testthat::test_that(desc = "Server data wrangling wrangles age correctly in WFHZ", {
  ## Initialise app ----
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

  #### Create a toy dataset ----
  data <- data.frame(
    dos = c("26/02/2026", "26/02/2026", "26/02/2026", "26/02/2026", "26/02/2026"),
    dob = c("12/05/2024", "14/07/2025", "22/03/2024", "20/07/2024", "20/09/2021"),
    age = c(NA, NA, NA, NA, 28),
    sex = c("m", "f", "m", "f", "f"),
    muac = c(121, 124, 132, 117, 100),
    weight = c(12.3, 13.3, 14.5, 15.5, 10.0),
    height = c(110.3, 117.2, 119.2, 80.6, 98.9)
  )
  tempfile <- tempfile(fileext = ".csv")
  write.csv(data, tempfile, row.names = FALSE)

  #### Upload onto the app ----
  app$upload_file(`upload_data-upload` = tempfile, wait_ = TRUE)

  ### Click on the data wrangling tab ----
  app$click(selector = "a[data-value='Data Wrangling']")
  app$wait_for_idle(timeout = 40000)

  ### Select input variables ----
  app$set_inputs(`wrangle_data-wrangle` = "wfhz", wait_ = FALSE)
  app$set_inputs(`wrangle_data-dos` = "dos", wait_ = FALSE)
  app$set_inputs(`wrangle_data-dob` = "dob", wait_ = FALSE)
  app$set_inputs(`wrangle_data-age` = "age", wait_ = FALSE)
  app$set_inputs(`wrangle_data-sex` = "sex", wait_ = FALSE)
  app$set_inputs(`wrangle_data-weight` = "weight", wait_ = FALSE)
  app$set_inputs(`wrangle_data-height` = "height", wait_ = FALSE)

  ### Click wrangle button ----
  app$click(input = "wrangle_data-apply_wrangle")
  app$wait_for_value(output = "wrangle_data-wrangled", timeout = 40000)

  ### Get wrangled values ----
  column_names <- app$get_js("
    $('#wrangle_data-wrangled thead th').map(function() {
      return $(this).text();
    }).get();
  ") |> as.character()

  ### Capture JavaScript expressions to return results ----
  js_results <- app$get_js("$('#wrangle_data-wrangled tbody tr').map(function()
  {return $(this).text();}).get();")

  age_mo <- stringr::str_extract(js_results[[1]], "\\d{2}\\.\\d{2}")
  age_28 <- stringr::str_extract(js_results[[5]], stringr::fixed("28"))

  testthat::expect_true("age_days" %in% column_names)
  testthat::expect_equal(as.numeric(age_mo), 21.51)
  testthat::expect_equal(as.numeric(age_28), 28)

  #### Stop the app ----
  app$stop()
})


## ---- Data Wrangling: MFAZ ---------------------------------------------------


testthat::test_that(desc = "Server data wrangling works as expected for MFAZ", {
  ## Initialise app ----
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

  ### Click on the data wrangling tab ----
  app$click(selector = "a[data-value='Data Wrangling']")
  app$wait_for_idle(timeout = 40000)

  ### Set the wrangling method to MFAZ ----
  app$set_inputs(`wrangle_data-wrangle` = "mfaz", wait_ = TRUE)
  ### Select variables ----
  app$set_inputs(`wrangle_data-dos` = "", wait_ = FALSE)
  app$set_inputs(`wrangle_data-dob` = "", wait_ = FALSE)
  app$set_inputs(`wrangle_data-age` = "age", wait_ = FALSE)
  app$set_inputs(`wrangle_data-sex` = "sex", wait_ = FALSE)
  app$set_inputs(`wrangle_data-muac` = "muac", wait_ = FALSE)

  ### Click wrangle button ----
  app$click(input = "wrangle_data-apply_wrangle", wait_ = TRUE, timeout_ = 15000)
  app$wait_for_value(output = "wrangle_data-wrangled", timeout = 40000)

  ### Get wrangled values ----
  column_names <- app$get_js("
    $('#wrangle_data-wrangled thead th').map(function() {
      return $(this).text();
    }).get();
  ") |> as.character()

  ### Test check ----
  testthat::expect_true(all(c("age_days", "mfaz", "flag_mfaz") %in% column_names))
  testthat::expect_true(app$get_js("$('#wrangle_data-wrangled').length > 0"))
  testthat::expect_equal(length(column_names), 13)

  #### Stop the app ----
  app$stop()
})


### When user supplies dos and dob for age wrangling ----
testthat::test_that(desc = "Server data wrangling wrangles age correctly in MFAZ", {
  ## Initialise app ----
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

  #### Create a toy dataset ----
  data <- data.frame(
    dos = c("26/02/2026", "26/02/2026", "26/02/2026", "26/02/2026", "26/02/2026"),
    dob = c("12/05/2024", "14/07/2025", "22/03/2024", "20/07/2024", "20/09/2021"),
    age = c(NA, NA, NA, NA, 28),
    sex = c("m", "f", "m", "f", "f"),
    muac = c(121, 124, 132, 117, 100)
  )
  tempfile <- tempfile(fileext = ".csv")
  write.csv(data, tempfile, row.names = FALSE)

  #### Upload onto the app ----
  app$upload_file(`upload_data-upload` = tempfile, wait_ = TRUE)

  ### Click on the data wrangling tab ----
  app$click(selector = "a[data-value='Data Wrangling']")
  app$wait_for_idle(timeout = 40000)

  ### Set the wrangling method to MFAZ ----
  app$set_inputs(`wrangle_data-wrangle` = "mfaz", wait_ = TRUE)
  ### Select variables ----
  app$set_inputs(`wrangle_data-dos` = "dos", wait_ = FALSE)
  app$set_inputs(`wrangle_data-dob` = "dob", wait_ = FALSE)
  app$set_inputs(`wrangle_data-age` = "age", wait_ = FALSE)
  app$set_inputs(`wrangle_data-sex` = "sex", wait_ = FALSE)
  app$set_inputs(`wrangle_data-muac` = "muac", wait_ = FALSE)

  ### Click wrangle button ----
  app$click(input = "wrangle_data-apply_wrangle", wait_ = TRUE, timeout_ = 15000)
  app$wait_for_value(output = "wrangle_data-wrangled", timeout = 40000)

  ### Get wrangled values ----
  column_names <- app$get_js("
    $('#wrangle_data-wrangled thead th').map(function() {
      return $(this).text();
    }).get();
  ") |> as.character()

  ### Capture JavaScript expressions to return results ----
  js_results <- app$get_js("$('#wrangle_data-wrangled tbody tr').map(function()
  {return $(this).text();}).get();")

  age_mo <- stringr::str_extract(js_results[[1]], "\\d{2}\\.\\d{2}")
  age_28 <- stringr::str_extract(js_results[[5]], stringr::fixed("28"))

  testthat::expect_true("age_days" %in% column_names)
  testthat::expect_equal(as.numeric(age_mo), 21.51)
  testthat::expect_equal(as.numeric(age_28), 28)

  #### Stop the app ----
  app$stop()
})


## ---- Data Wrangling: MUAC ---------------------------------------------------


### When age is given in categories ----
testthat::test_that(
  desc = "Server data wrangling works as expected for raw MUAC values",
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
      file = testthat::test_path("fixtures", "anthro-01.csv"),
      check.names = FALSE
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

    ### Get wrangled values ----
    column_names <- app$get_js("
    $('#wrangle_data-wrangled thead th').map(function() {
      return $(this).text();
    }).get();
  ") |> as.character()

    ### Test check ----
    testthat::expect_true("flag_muac" %in% column_names)
    testthat::expect_true(app$get_js("$('#wrangle_data-wrangled').length > 0"))
    testthat::expect_equal(length(column_names), 11)

    ### Stop the app ----
    app$stop()
  }
)


## ---- Data Wrangling: WFHZ and MFAZ ------------------------------------------


testthat::test_that(
  desc = "Server data wrangling works as expected for combined wrangling",
  {
    ## Initialise app ----
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

    ### Click on the data wrangling tab ----
    app$click(selector = "a[data-value='Data Wrangling']")
    app$wait_for_idle(timeout = 40000)

    ### Select input variables ----
    app$set_inputs(`wrangle_data-wrangle` = "combined", wait_ = TRUE)
    app$set_inputs(`wrangle_data-dos` = "", wait_ = FALSE)
    app$set_inputs(`wrangle_data-dob` = "", wait_ = FALSE)
    app$set_inputs(`wrangle_data-age` = "age", wait_ = FALSE)
    app$set_inputs(`wrangle_data-sex` = "sex", wait_ = FALSE)
    app$set_inputs(`wrangle_data-weight` = "weight", wait_ = FALSE)
    app$set_inputs(`wrangle_data-height` = "height", wait_ = FALSE)
    app$set_inputs(`wrangle_data-muac` = "muac", wait_ = FALSE)

    ### Click wrangle button ----
    app$click(input = "wrangle_data-apply_wrangle")
    app$wait_for_value(output = "wrangle_data-wrangled", timeout = 40000)

    app$wait_for_idle(timeout = 15000)

    testthat::expect_true(app$get_js("$('#wrangle_data-wrangled').length > 0"))

    #### Stop the app ----
    app$stop()
  }
)


### When user supplies dos and dob for age wrangling ----
testthat::test_that(desc = "Server data wrangling wrangles age correctly in
combined data wrangling", {
  ## Initialise app ----
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

  #### Create a toy dataset ----
  data <- data.frame(
    dos = c("26/02/2026", "26/02/2026", "26/02/2026", "26/02/2026", "26/02/2026"),
    dob = c("12/05/2024", "14/07/2025", "22/03/2024", "20/07/2024", "20/09/2021"),
    age = c(NA, NA, NA, NA, 28),
    sex = c("m", "f", "m", "f", "f"),
    muac = c(121, 124, 132, 117, 100),
    weight = c(12.3, 13.3, 14.5, 15.5, 10.0),
    height = c(110.3, 117.2, 119.2, 80.6, 98.9)
  )
  tempfile <- tempfile(fileext = ".csv")
  write.csv(data, tempfile, row.names = FALSE)

  #### Upload onto the app ----
  app$upload_file(`upload_data-upload` = tempfile, wait_ = TRUE)

  ### Click on the data wrangling tab ----
  app$click(selector = "a[data-value='Data Wrangling']")
  app$wait_for_idle(timeout = 40000)

  ### Select input variables ----
  app$set_inputs(`wrangle_data-wrangle` = "combined", wait_ = TRUE)
  app$set_inputs(`wrangle_data-dos` = "dos", wait_ = FALSE)
  app$set_inputs(`wrangle_data-dob` = "dob", wait_ = FALSE)
  app$set_inputs(`wrangle_data-age` = "age", wait_ = FALSE)
  app$set_inputs(`wrangle_data-sex` = "sex", wait_ = FALSE)
  app$set_inputs(`wrangle_data-weight` = "weight", wait_ = FALSE)
  app$set_inputs(`wrangle_data-height` = "height", wait_ = FALSE)
  app$set_inputs(`wrangle_data-muac` = "muac", wait_ = FALSE)

  ### Click wrangle button ----
  app$click(input = "wrangle_data-apply_wrangle")
  app$wait_for_value(output = "wrangle_data-wrangled", timeout = 40000)

  app$wait_for_idle(timeout = 15000)

  ## Get wrangled values ----
  column_names <- app$get_js("
    $('#wrangle_data-wrangled thead th').map(function() {
      return $(this).text();
    }).get();
  ") |> as.character()

  ### Capture JavaScript expressions to return results ----
  js_results <- app$get_js("$('#wrangle_data-wrangled tbody tr').map(function()
  {return $(this).text();}).get();")

  age_mo <- stringr::str_extract(js_results[[1]], "\\d{2}\\.\\d{2}")
  age_28 <- stringr::str_extract(js_results[[5]], stringr::fixed("28"))

  testthat::expect_true("age_days" %in% column_names)
  testthat::expect_equal(as.numeric(age_mo), 21.51)
  testthat::expect_equal(as.numeric(age_28), 28)

  #### Stop the app ----
  app$stop()
})

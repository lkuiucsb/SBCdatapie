context('data_package_wrapper()')

test_that("an incorrect doi to return an error", {
  expect_error(data_package_wrapper("bgiugrqeigreqlio74981"))
})

test_that("a correct doi will be properly attributed", {
  #Make sure the temp folder is clear
  unlink(paste0(tempdir(), "/data_package"), recursive = TRUE, force = TRUE)
  
  doi_string <- "doi:10.18739/A2DP3X"
  test_data <- suppressMessages(
    suppressWarnings(
      data_package_wrapper("doi:10.18739/A2DP3X")))
  
  expect_equal(attributes(test_data)$doi, doi_string)
  
  suppressMessages(data_package_remove())
})

test_that("a downloaded file generates a temporary directory", {
  #Make sure the temp folder is clear
  unlink(paste0(tempdir(), "/data_package"), recursive = TRUE, force = TRUE)
  
  doi_string <- "doi:10.18739/A2DP3X"
  test_data <- suppressMessages(
    suppressWarnings(
      data_package_wrapper("doi:10.18739/A2DP3X")))
  
  expect_true(dir.exists(attributes(test_data)$folder))
  
  suppressMessages(data_package_remove())
})

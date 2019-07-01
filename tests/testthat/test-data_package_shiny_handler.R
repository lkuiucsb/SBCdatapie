context('data_package_shiny_handler()')

test_that("no doi and no existing data returns the example data", {
  data_import <- data_package_shiny_handler()
  expect_equal(data_import, data_example)
})

test_that("an empty doi and no existing data returns the example data", {
  data_import <- data_package_shiny_handler("")
  expect_equal(data_import, data_example)
})

test_that("no doi and with existing data returns the existing data", {
  doi_string <- "doi:10.6073/pasta/dd7955138eb963a847b861242390a48c"
  data_initial <- suppressWarnings(
    data_package_shiny_handler(data.pkg.doi = doi_string))
  data_incorrect <- data_package_shiny_handler(current.data = data_initial)
  expect_equal(data_incorrect, data_initial)
})

test_that("a working doi doesn't generate an error", {
  doi_string <- "doi:10.6073/pasta/dd7955138eb963a847b861242390a48c"
  expect_message(
    suppressWarnings(
    data_package_shiny_handler(data.pkg.doi = doi_string)))
})
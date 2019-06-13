context('remove_data_package_directory()')

# Return messages -------------------------------------------------------------

test_that("Return messages", {
  
  # Create temporary directory used by read_data_archived()
  dir.create(paste0(tempdir(), '/data_package'))
  
  # Remove existing directory
  expect_message(remove_data_package_directory())
  
  # Try removing non-existanat directory
  expect_message(remove_data_package_directory())
  
})

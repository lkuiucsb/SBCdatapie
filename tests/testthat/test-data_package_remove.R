context('data_package_remove()')

test_that("Validate process", {
  
  # Download data package to temporary directory

  suppressWarnings(
    data_package_download(
      data.pkg.doi = 'doi:10.18739/A2DP3X'
    )
  )

  # Directory exists
  
  expect_true(
    dir.exists(paste0(tempdir(), '/data_package'))
  )
  
  # Remove directory
  
  suppressMessages(data_package_remove())
  
  # Directory doesn't exist
  
  expect_true(
    !dir.exists(paste0(tempdir(), '/data_package'))
  )
  
})

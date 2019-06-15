context('data_package_read()')

test_that("Validate output structure", {
  
  # Ensure the temporary directory has been removed
  
  suppressMessages(data_package_remove())
  
  # Download data package to temporary directory and read
  
  suppressMessages(
    data_package_download(data.pkg.doi = 'doi:10.18739/A2DP3X')
  )
  
  
  output <- suppressMessages(
    suppressWarnings(
      data_package_read()
    )
  )
  
  # Output is a list
  
  expect_true(is.list(output))
  
  # First level has names
  
  expect_true(
    all(
      names(output) %in% c('Alaska_Schools_Rentention2009_15.xlsx',
                           'Alaska_Schools_Rentention2009_15.csv',
                           'Alaska_Schools_Rentention_Definitions.csv')
    )
  )
  
  # Second level has names
  
  expect_true(
    all(
      names(unlist(unname(output), recursive = FALSE)) %in% c('summary_metadata',
                                                              'data',
                                                              'factor_metadata',
                                                              'attribute_metadata')
    )
  )
  
  # Second level has classes
  
  expect_true(
    all(
      unname(
        unlist(
          lapply(
            unlist(output, recursive = FALSE), class
          )
        )
      ) %in% c('tbl_df', 'tbl', 'data.frame')
    )
  )
  
  # Clean up
  
  suppressMessages(data_package_remove())
  
})

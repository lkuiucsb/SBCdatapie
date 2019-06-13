context('read_data_archived()')

# Return messages -------------------------------------------------------------

test_that("Return messages", {
  
  # Download data package to temporary directory and read
  output <- suppressMessages(
    suppressWarnings(
      read_data_archived(data.pkg.doi = 'doi:10.18739/A2DP3X')
    )
  )
   
  # Output is a list
  expect_true(is.list(output))
  
  # First level has names
  expect_true(
    all(
      names(output) %in% c('Alaska_Schools_Rentention2009_15.xlsx',
                           'Alaska_Schools_Rentention2009_15.csv',
                           'Alaska_Schools_Rentention_Definitions.csv',
                           'data_package_path')
    )
  )
  
  # Second level has names
  expect_true(
    all(
      names(unlist(unname(output), recursive = FALSE)) %in% c('summary_metadata',
                                                              'data',
                                                              'factor_metadata',
                                                              'attribute_metadata',
                                                              '')
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
      ) %in% c('tbl_df', 'tbl', 'data.frame', 'character')
    )
  )
  
  # Clean up
  suppressMessages(remove_data_package_directory())
  
})

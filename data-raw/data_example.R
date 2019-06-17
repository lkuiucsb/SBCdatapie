##This was run to initialize this file.
#usethis::use_data_raw("data_example")

## code to prepare `DATASET` dataset goes here
doi_string <- 'doi:10.6073/pasta/d3c106dfafbc14ae46e55dbd084a7c68'

#Unlink any data
unlink(
  paste0(tempdir(), "/data_package"),
  recursive = TRUE, 
  force = TRUE)

#Read in the data
data_example <- read_data_archived(doi_string)

#Change the name of the dataset to something to more obviously indicate it is an
# example dataset.
names(data_example)[1] <- "Example_dataset.csv"

#Give data a doi attribute
attributes(temp_data)$doi <- doi_string

#Save the data
usethis::use_data(data_example, overwrite = TRUE)

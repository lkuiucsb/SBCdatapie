##Code below was used at the console to initialize this file.
#usethis::use_data_raw("data_example")

## code to prepare `DATASET` dataset goes here
doi_string <- 'doi:10.6073/pasta/d3c106dfafbc14ae46e55dbd084a7c68'

#Unlink any data, if necessary.
# unlink(
#   paste0(tempdir(), "/data_package"),
#   recursive = TRUE, 
#   force = TRUE)

#Download and read data
data_example <- data_package_wrapper(doi_string)

#Change the name of the dataset to something to more obviously indicate it is an
# example dataset.
names(data_example)[1] <- "Example_dataset.csv"

#Reassign the folder location to something more generic
attributes(data_example)$folder <- NA

#Save the data
usethis::use_data(data_example, overwrite = TRUE)

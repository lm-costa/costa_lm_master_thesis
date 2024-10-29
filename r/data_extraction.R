# my functions
source('r/function.R')

####
files_names <- list.files("data-raw/",
                          pattern = "nc",
                          full.names = TRUE)

#### Extracting

xco2 <- purrr::map_df(files_names, my_ncdf4_extractor) |>
  dplyr::mutate(
    date = as.Date.POSIXct(time)
  )
dplyr::glimpse(xco2)

readr::write_rds(xco2,'data/xco2_full.rds')

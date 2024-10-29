source('r/function.r')

url_filename <- list.files("url/",
                            pattern = ".txt",
                            full.names = TRUE)

urls <- read.table(url_filename) |>
  dplyr::filter(!stringr::str_detect(V1,".pdf"))
n_urls <- nrow(urls)

###
tictoc::tic()
furrr::future_pmap(list(urls[,1],"input your user","your password"),my_ncdf4_download)
tictoc::toc()

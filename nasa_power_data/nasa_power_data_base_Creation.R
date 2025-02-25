library(tidyverse)
library(geobr)
library(nasapower)
source('r/nasa_power_download.R')
source('r/def_pol.R')


# Expand grid for brazil

#
dist <- 0.5
grid_br <- expand.grid(lon=seq(-74,
                               -27,dist),
                       lat=seq(-34,
                               6,
                               dist))
plot(grid_br)


br <- geobr::read_country(showProgress = FALSE)
region <- geobr::read_region(showProgress = FALSE)

pol_br <- br$geom |> purrr::pluck(1) |> as.matrix()
pol_north <- region$geom |> purrr::pluck(1) |> as.matrix()
pol_northeast <- region$geom |> purrr::pluck(2) |> as.matrix()
pol_southeast <- region$geom |> purrr::pluck(3) |> as.matrix()
pol_south <- region$geom |> purrr::pluck(4) |> as.matrix()
pol_midwest<- region$geom |> purrr::pluck(5) |> as.matrix()

# correcting poligions

pol_br <- pol_br[pol_br[,1]<=-34,]
pol_br <- pol_br[!((pol_br[,1]>=-38.8 & pol_br[,1]<=-38.6) &
                     (pol_br[,2]>= -19 & pol_br[,2]<= -16)),]

pol_northeast <- pol_northeast[pol_northeast[,1]<=-34,]
pol_northeast <- pol_northeast[!((pol_northeast[,1]>=-38.7 &
                                    pol_northeast[,1]<=-38.6) &
                                   pol_northeast[,2]<= -15),]

pol_southeast <- pol_southeast[pol_southeast[,1]<=-30,]


### filtering expanded grid to the brazil boundries

grid_br_cut <- grid_br |>
  dplyr::mutate(
    flag_br = def_pol(lon,lat,pol_br),
    flag_north = def_pol(lon,lat,pol_north),
    flag_northeast = def_pol(lon,lat,pol_northeast),
    flag_midwest= def_pol(lon,lat,pol_midwest),
    flag_southeast = def_pol(lon,lat,pol_southeast),
    flag_south = def_pol(lon,lat,pol_south)
  ) |>
  tidyr::pivot_longer(
    tidyr::starts_with('flag'),
    names_to = 'region',
    values_to = 'flag'
  ) |>
  dplyr::filter(flag) |>
  dplyr::select(lon,lat) |>
  dplyr::group_by(lon,lat) |>
  dplyr::summarise(
    n_obs = dplyr::n()
  )

plot(grid_br_cut$lon,grid_br_cut$lat)

df <- grid_br_cut |> select(lon,lat) |> as.data.frame()

# download dados precipitação

for (i in 1:nrow(df)){
  repeat{
    dw <- try(
      power_data_download(df[i,1],df[i,2],
                          startdate='2015-01-01',
                          enddate = '2023-01-01')
    )
    if (!(inherits(dw,"try-error")))
      break
  }
}


### criação base de dados

files_names <- list.files('nasa_power_data/data-raw/',full.names = T)
for (i in 1:length(files_names)){
  if(i ==1){
    df <- read.csv(files_names[i])
  }else{
    df_a <- read.csv(files_names[i])
    df <- rbind(df,df_a)
  }
}


readr::write_rds(df,'nasa_power_data/data/nasa_power_data.rds')

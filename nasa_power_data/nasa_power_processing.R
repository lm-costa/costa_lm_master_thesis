library(tidyverse)
library(readr)
library(writexl)

path_files <- 'nasa_power_data/data/'

file_nase_power <- list.files(path_files,full.names = T)

df <- read_rds(file_nase_power)


meteo_grided <- df |>
  mutate(
    date = make_date(YEAR,MM,DD)
  ) |>
  group_by(LON,LAT,YEAR, MM) |>
  summarise(
    prec = sum(PRECTOTCORR),
    qg = mean(ALLSKY_SFC_SW_DWN),
    rh = mean(RH2M),
    temp = mean(T2M),
    ws = mean(WS2M)
  )

write_rds(meteo_grided,paste0(path_files,'meteo_grided.rds'))


meteo_avg <- meteo_grided |>
  ungroup() |>
  select(-c("LON","LAT")) |>
  group_by(YEAR,MM) |>
  summarise_all(mean) |>
  mutate(
    date=make_date(YEAR,MM,'15')
  )
write_rds(meteo_avg,paste0(path_files,'meteo_avg.rds'))


meteo_avg |>
  pivot_longer(cols = c('prec':'ws')) |>
  ggplot(aes(x=date,y=value,col=name))+
  geom_point()+
  geom_line()+
  facet_wrap(~name, scales = 'free_y')



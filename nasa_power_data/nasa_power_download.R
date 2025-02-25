power_data_download <- function(lon,lat, startdate, enddate){
  df <- nasapower::get_power(
    community = 'ag',
    lonlat = c(lon,lat),
    pars = c('ALLSKY_SFC_SW_DWN','RH2M','T2M','PRECTOTCORR','WS2M','WD2M'),
    dates = c(startdate,enddate),
    temporal_api = 'daily'
  )
  write.csv(df,paste0('nasa_power_data/data-raw/',lon,'_',lat,'.csv'))
}

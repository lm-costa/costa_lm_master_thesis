filename <- dir('url/',pattern='.txt')
urls <- read.table(paste0('url/',filename))
n_urls <- nrow(urls)
n_split <- length(stringr::str_split(urls[1,1],"/",simplify = TRUE))

files_nc <- stringr::str_split_fixed(urls[,1],"/",n=Inf)[,n_split]


for (i in 1:n_urls){
  repeat{
    dw <- try(download.file(urls[i,1],
                            paste0('data-raw/',files_nc[i]),
                            method = 'wget',
                            extra = c('--user=lm.costa --password Karatedo2007')
    )
    )
    if (!(inherits(dw,"try-error")))
      break
  }
}


### data extraction

files_nc <- list.files('data-raw/', pattern = 'nc')

for(i in 1:length(files_nc)){
  if(i==1){
    df <- ncdf4::nc_open(paste0('data-raw/',files_nc[i]))
    if (df$ndims == 0){

    }else{
      xco2 <- data.frame(
        'lon' = ncdf4::ncvar_get(df,varid='longitude'),
        'lat' = ncdf4::ncvar_get(df,varid='latitude'),
        'time' = ncdf4::ncvar_get(df,varid='time'),
        'xco2' = ncdf4::ncvar_get(df,varid='xco2'),
        'uncertanty' = ncdf4::ncvar_get(df,varid='xco2_uncertainty'),
        'quality_flag' = ncdf4::ncvar_get(df,varid='xco2_quality_flag')
      ) |>
        dplyr::filter(lon < -35 & lon >-75 & lat < 5 & lat >-35) |> # brazil region
        dplyr::filter(quality_flag == 0)
    }
    ncdf4::nc_close(df)
  }else{
    df_a <- ncdf4::nc_open(paste0('data-raw/',files_nc[i]))
    if (df_a$ndims == 0){
    }else{
      xco2_a <- data.frame(
        'lon' = ncdf4::ncvar_get(df_a,varid='longitude'),
        'lat' = ncdf4::ncvar_get(df_a,varid='latitude'),
        'time' = ncdf4::ncvar_get(df_a,varid='time'),
        'xco2' = ncdf4::ncvar_get(df_a,varid='xco2'),
        'uncertanty' = ncdf4::ncvar_get(df_a,varid='xco2_uncertainty'),
        'quality_flag' = ncdf4::ncvar_get(df_a,varid='xco2_quality_flag')
      )|>
        dplyr::filter(lon < -35 & lon >-75 & lat < 5 & lat >-35)|> # brazil region
        dplyr::filter(quality_flag ==0)}
    ncdf4::nc_close(df_a)
    xco2 <- rbind(xco2,xco2_a)
  }
}


xco2 <- xco2 |>
  dplyr::mutate(
    date = as.Date.POSIXct(time))


xco2 |>
  dplyr::filter(lubridate::year(date)==2015) |>
  ggplot2::ggplot(ggplot2::aes(x=lon,y=lat))+
  ggplot2::geom_point()


rm(df,df_a,xco2_a,urls,dw,filename,files_nc,i,n_split,n_urls)

readr::write_rds(xco2,'data/xco2_full.rds') # data frame without any filter




#### creating a spatial grid for brazil
dist <- 0.25
grid_br <- expand.grid(lon=seq(-74,
                             -27,dist),
                       lat=seq(-34,
                             6,
                             dist))
plot(grid_br)

source('r/def_pol.R')
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


#### aggregation

xco2_full_trend <- xco2 |> dplyr::mutate(
  year =lubridate::year(date),
  month = lubridate::month(date)
)

max(xco2_full_trend$year)


for(i in 2015:2023){
  aux_xco2 <- xco2_full_trend |>
    dplyr::filter(year==i)
  vct_xco2 <- vector();dist_xco2 <- vector();
  lon_grid <- vector();lat_grid <- vector();
  for(k in 1:nrow(aux_xco2)){
    d <- sqrt((aux_xco2$lon[k]-grid_br_cut$lon)^2+
                (aux_xco2$lat[k]-grid_br_cut$lat)^2
              )
    min_index <- order(d)[1]
    vct_xco2[k] <- aux_xco2$xco2[min_index]
    dist_xco2[k] <- d[order(d)[1]]
    lon_grid[k] <- grid_br_cut$lon[min_index]
    lat_grid[k] <- grid_br_cut$lat[min_index]
    }
  aux_xco2$dist_xco2 <- dist_xco2
  aux_xco2$xco2_new <- vct_xco2
  aux_xco2$lon_grid <- lon_grid
  aux_xco2$lat_grid <- lat_grid
  if(i == 2015){
    xco2_full_trend_cut <- aux_xco2
  }else{
    xco2_full_trend_cut <- rbind(xco2_full_trend_cut,aux_xco2)
  }
}


xco2_full_trend_cut|>
  dplyr::mutate(
    dist_conf = sqrt((lon - lon_grid)^2 + (lat - lat_grid)^2)
  ) |>
  dplyr::glimpse()

nrow(xco2_agg_novo |>
       dplyr::mutate(
         dist_conf = sqrt((lon - lon_grid)^2 + (lat - lat_grid)^2),
         dist_bol = dist_xco2 - dist_conf
       ) |>
       dplyr::filter(dist_bol ==0)) == nrow(xco2_agg_novo) # cheking if the distances is equal

readr::write_rds(xco2_full_trend_cut,'data/xco2_025_full_trend.rds') # data frame without filter and with the grid coordinates

## plotting

xco2_full_trend_cut |>
  dplyr::filter(dist_xco2<0.15) |>
  dplyr::group_by(lon_grid,lat_grid,year,month) |>
  dplyr::summarise(
    xco2_mean = mean(xco2),
    uncertanty_mean = mean(uncertanty),
    xco2_sd = sd(xco2),
    nobs = dplyr::n(),
    xco2_se = xco2_sd/sqrt(nobs)
  ) |>
  dplyr::filter(year==2015) |>
  ggplot2::ggplot(ggplot2::aes(x=lon_grid,y=lat_grid,col=nobs))+
  ggplot2::geom_point()



xco2_full_trend_cut_ <- xco2_full_trend_cut |>
  dplyr::filter(dist_xco2<0.15) |>
  dplyr::group_by(lon_grid,lat_grid,year,month) |>
  dplyr::summarise(
    xco2 = mean(xco2),
    uncertanty_mean = mean(uncertanty),
    xco2_sd = sd(xco2),
    nobs = dplyr::n(),
    xco2_se = xco2_sd/sqrt(nobs)
  )

readr::write_rds(xco2_full_trend_cut_,'data/xco2_025_trend.rds')


######

xco2df <- readr::read_rds('data/xco2_025_full_trend.rds')

## spatial distribuition of observation
xco2df |>
  dplyr::filter(dist_xco2<0.15) |>
  dplyr::group_by(lon_grid,lat_grid,year,month) |>
  dplyr::summarise(
    xco2_mean = mean(xco2),
    uncertanty_mean = mean(uncertanty),
    xco2_sd = sd(xco2),
    nobs = dplyr::n(),
    xco2_se = xco2_sd/sqrt(nobs)
  ) |>
  ggplot2::ggplot(ggplot2::aes(x=lon_grid,y=lat_grid,col=nobs))+
  ggplot2::geom_point()+
  ggplot2::facet_wrap(~year)

# temporal plot

xco2df |>
  dplyr::filter(dist_xco2<0.15) |>
  dplyr::group_by(date) |>
  dplyr::summarise(xco2_mean=mean(xco2)) |>
  ggplot2::ggplot(ggplot2::aes(x=date,y=xco2_mean )) +
  ggplot2::geom_point(shape=21,color="black",fill="gray") +
  #ggplot2::geom_line(color="red") +
  ggplot2::geom_smooth(method = "lm") +
  ggpubr::stat_regline_equation(ggplot2::aes(
    label =  paste(..eq.label.., ..rr.label.., sep = "*plain(\",\")~~"))) +
  ggplot2::theme_bw()


## analisys with trend

xco2aggtrend <- xco2df |>
  dplyr::filter(dist_xco2<0.15) |>
  dplyr::mutate(
    lon = lon_grid,
    lat = lat_grid,
  ) |>
  dplyr::select(-c(lon_grid,lat_grid)) |>
  dplyr::group_by(lon,lat,year,month) |>
  dplyr::summarise(
    xco2_mean= mean(xco2,na.rm=TRUE),
    xco2_sd = sd(xco2,na.rm=TRUE),
    xco2_uncertanty = mean(uncertanty),
    nobs = dplyr::n(),
    xco2_ep = xco2_sd/sqrt(nobs),
    cv = 100*xco2_sd/xco2_mean
  ) |>
  dplyr::mutate(
    date = lubridate::make_date(year,month,'15')
  )

xco2aggtrend |>
  dplyr::group_by(date) |>
  dplyr::summarise(xco2_mean=mean(xco2_mean)) |>
  ggplot2::ggplot(ggplot2::aes(x=date,y=xco2_mean )) +
  ggplot2::geom_point(shape=21,color="black",fill="gray") +
  ggplot2::geom_line(color="red") +
  ggplot2::geom_smooth(method = "lm") +
  ggpubr::stat_regline_equation(ggplot2::aes(
    label =  paste(..eq.label.., ..rr.label.., sep = "*plain(\",\")~~"))) +
  ggplot2::theme_bw()


xco2_aux_trend <- xco2aggtrend |>
  dplyr::ungroup() |>
  dplyr::group_by(date) |>
  dplyr::summarise(
    xco2 = mean(xco2_mean)
  )

## linear model
mod_trend <- lm(xco2 ~date,
                data = xco2_aux_trend )
beta_r <-mod_trend$coefficients[2] # regional beta
ep <- summary(mod_trend)$coefficients[2,2] # regional standard error

# inferior/superior limit
ilbr <- beta_r-ep
slbr <- beta_r+ep


## model diagnostic
broom::augment(mod_trend, interval="confidence")

plot(mod_trend)
shapiro.test(mod_trend$residuals)
summary(mod_trend)
cooks.distance(mod_trend)


### creating table to the analisys
xco2_nest <- xco2aggtrend |>
  tibble::as_tibble() |>
  dplyr::mutate(year =lubridate::year(date),
                quarter = lubridate::quarter(date),
                quarter_year = lubridate::make_date(year, quarter, 1)) |>
  dplyr::group_by(lon, lat,date) |>
  dplyr::summarise(xco2 = mean(xco2_mean, na.rm=TRUE)) |>
  dplyr::mutate(
    id_time = date
  ) |>
  dplyr::group_by(lon,lat) |>
  tidyr::nest()

## function to create a linear model for each pixel
linear_reg <- function(df,output="beta1"){
  # model for each grid cell
  modelo <- lm(xco2 ~ date, data=df)
  beta_1 <- c(summary(modelo)$coefficients[2])

  # beta
  if(output=="beta1"){
    return(beta_1)
  }

  # p value
  if(output=="p_value"){
    if(is.nan(beta_1)){
      beta_1 <- 0
      p <- 1
    }else{
      p <- summary(modelo)$coefficients[2,4]
      if(is.nan(p)) p <- 1
    }
    return(p)
  }
  #
  if(output == "partial"){
    partial <- df |>
      dplyr::summarise(xco2 = mean(xco2), na.mr=TRUE) |>
      dplyr::pull(xco2)
    return(partial)
  }

  if(output == "n"){
    return(nrow(df))
  }
  }


### calculating for each pixel
xco2_nest <- xco2_nest |>
  dplyr::mutate(
    beta_line = purrr::map(data,linear_reg, output="beta1"),
    p_value = purrr::map(data,linear_reg, output="p_value"),
    partial = purrr::map(data,linear_reg, output="partial"),
    n_obs = purrr::map(data,linear_reg, output="n")
  )

xco2_nest |>
  dplyr::filter(n_obs > 7) |>
  dplyr::select(lon, lat, n_obs) |>
  ggplot2::ggplot(ggplot2::aes(x=lon, y=lat, color = n_obs)) +
  ggplot2::geom_point()


xco2_nest |>
  dplyr::filter(n_obs > 5) |>
  dplyr::select(lon, lat, n_obs) |>
  ggplot2::ggplot(ggplot2::aes(x=lon, y=lat, color = n_obs)) +
  ggplot2::geom_point()

readr::write_rds(xco2_nest,'beta_xco2_trend.rds')

### creating a table

xco2_aux_trend_new <- xco2_nest |>
  dplyr::filter(n_obs > 7) |>
  tidyr::unnest(cols = c(beta_line, partial)) |>
  dplyr::ungroup() |>
  dplyr::select(lon, lat, beta_line, partial)

q3_xco2 <- xco2_aux_trend_new |> dplyr::pull(beta_line) |> quantile(.75)

xco2_aux_trend_new <- xco2_aux_trend_new  |>
  dplyr::mutate(
    anomaly =  partial - xco2_aux_trend_new  |>
      dplyr::pull(partial) |>
      mean(),
    Dbeta = beta_line -xco2_aux_trend_new  |>
      dplyr::pull(beta_line) |> mean(na.rm=TRUE)
  )

q3_anom <- xco2_aux_trend_new  |> dplyr::pull(anomaly) |> quantile(.75)

xco2_aux_trend_new  <- xco2_aux_trend_new  |>
  dplyr::mutate(
    beta_index =  ifelse(beta_line <=q3_xco2, 0, 1)
  )
xco2_aux_trend_new |>
  ggplot2::ggplot(ggplot2::aes(x=beta_line)) +
  ggplot2::geom_histogram(bins=30,
                          fill="orange",
                          color="black") +
  ggplot2::labs(x="βpixel",y="Count") +
  ggplot2::geom_vline(xintercept = q3_xco2,
                      color = "red",
                      lty=2) +
  gghighlight::gghighlight(beta_line > q3_xco2,
                           unhighlighted_params = list(
                             color = "darkgray",
                             fill = "lightgray")) +
  ggplot2::theme_minimal()


xco2_aux_trend_new |>
  ggplot2::ggplot(ggplot2::aes(x=anomaly)) +
  ggplot2::geom_histogram(bins=30,
                          fill="lightblue",
                          color="black") +
  ggplot2::labs(x="Anomaly",y="Count") +
  ggplot2::geom_vline(xintercept = q3_anom,
                      color = "red",
                      lty=2) +
  gghighlight::gghighlight(anomaly > q3_anom,
                           unhighlighted_params = list(
                             color = "darkgray",
                             fill = "lightgray")) +
  ggplot2::theme_minimal()


xco2_aux_trend_new |>
  ggplot2::ggplot(ggplot2::aes(x=lon,y=lat), color = 'black')+
  ggplot2::geom_tile(ggplot2::aes(fill=beta_line))+
  ggplot2::scale_fill_gradient(low='yellow',high='blue')+
  ggplot2::coord_equal()+
  ggplot2::labs(fill='βpixel')+
  ggplot2::theme_bw()

xco2_aux_trend_new |>
  dplyr::mutate(
    bt = dplyr::case_when(
      beta_line - beta_r >0 ~'βpixel > βregional',
      beta_line - beta_r <0 ~ 'βpixel < βregional',
      beta_line - beta_r ==0~ 'βpixel = βregional'
    )
  ) |>
  ggplot2::ggplot(ggplot2::aes(x=lon,y=lat), color = 'black')+
  ggplot2::geom_tile(ggplot2::aes(fill=Dbeta))+
  ggplot2::scale_fill_gradient(low='yellow',high='blue')+
  ggplot2::coord_equal()+
  ggplot2::labs(fill='Dif βpixel')+
  ggplot2::theme_bw()

## analisys without trend

xco2aggdetrend <- xco2df |>
  dplyr::filter(dist_xco2<0.15) |>
  dplyr::mutate(
    lon = lon_grid,
    lat = lat_grid,
  ) |>
  dplyr::select(-c(lon_grid,lat_grid)) |>
  dplyr::group_by(lon,lat,year,month) |>
  dplyr::summarise(
    xco2_mean= mean(xco2,na.rm=TRUE),
    xco2_sd = sd(xco2,na.rm=TRUE),
    xco2_uncertanty = mean(uncertanty),
    nobs = dplyr::n(),
    xco2_ep = xco2_sd/sqrt(nobs),
    cv = 100*xco2_sd/xco2_mean
  ) |>
  dplyr::mutate(
    date = lubridate::make_date(year,month,'15')
  )

xco2aggdetrend |>
  dplyr::group_by(date) |>
  dplyr::summarise(xco2_mean=mean(xco2_mean)) |>
  ggplot2::ggplot(ggplot2::aes(x=date,y=xco2_mean )) +
  ggplot2::geom_point(shape=21,color="black",fill="gray") +
  ggplot2::geom_line(color="red") +
  ggplot2::geom_smooth(method = "lm") +
  ggpubr::stat_regline_equation(ggplot2::aes(
    label =  paste(..eq.label.., ..rr.label.., sep = "*plain(\",\")~~"))) +
  ggplot2::theme_bw()

summary(lm(xco2_mean~x,data =xco2aggdetrend |>
             dplyr::mutate(
               x = 1:dplyr::n()
             )))

xco2aggdetrend |>
  dplyr::mutate(
    x=1:dplyr::n(),
    xco2_est = 408.01915 + 0.04824*x,
    delta=xco2_est - xco2_mean,
    xco2r = (408.01915-delta)-(mean(xco2_mean)-408.01915)
  ) |>
  dplyr::group_by(date) |>
  dplyr::summarise(xco2_mean=mean(xco2r)) |>
  ggplot2::ggplot(ggplot2::aes(x=date,y=xco2_mean )) +
  ggplot2::geom_point(shape=21,color="black",fill="gray") +
  ggplot2::geom_line(color="red") +
  ggplot2::geom_smooth(method = "lm") +
  ggpubr::stat_regline_equation(ggplot2::aes(
    label =  paste(..eq.label.., ..rr.label.., sep = "*plain(\",\")~~"))) +
  ggplot2::theme_bw()

xco2aggdetrend <- xco2aggdetrend |>
  dplyr::mutate(
    x=1:dplyr::n(),
    xco2_est = 408.01915 + 0.04824*x,
    delta=xco2_est - xco2_mean,
    xco2r = (408.01915-delta)-(mean(xco2_mean)-408.01915)
  )


xco2_aux_detrend <- xco2aggdetrend |>
  dplyr::ungroup() |>
  dplyr::group_by(date) |>
  dplyr::summarise(
    xco2 = mean(xco2r)
  )

## linear model
mod_detrend <- lm(xco2 ~date,
                data = xco2_aux_detrend )
beta_r <-mod_detrend$coefficients[2] # regional beta
ep <- summary(mod_detrend)$coefficients[2,2] # regional standard error

# inferior/superior limit
ilbr <- beta_r-ep
slbr <- beta_r+ep


## model diagnostic
broom::augment(mod_detrend, interval="confidence")

plot(mod_detrend)
cooks.distance(mod_detrend)
shapiro.test(mod_detrend$residuals)
hist(xco2aggdetrend$xco2r)
ks.test(xco2aggdetrend$xco2r, 'pnorm')

### creating table to the analisys
xco2_nest <- xco2aggdetrend |>
  tibble::as_tibble() |>
  dplyr::mutate(year =lubridate::year(date),
                quarter = lubridate::quarter(date),
                quarter_year = lubridate::make_date(year, quarter, 1)) |>
  dplyr::group_by(lon, lat,date) |>
  dplyr::summarise(xco2 = mean(xco2r, na.rm=TRUE)) |>
  dplyr::mutate(
    id_time = date
  ) |>
  dplyr::group_by(lon,lat) |>
  tidyr::nest()

## function to create a linear model for each pixel
linear_reg <- function(df,output="beta1"){
  # model for each grid cell
  modelo <- lm(xco2 ~ date, data=df)
  beta_1 <- c(summary(modelo)$coefficients[2])

  # beta
  if(output=="beta1"){
    return(beta_1)
  }

  # p value
  if(output=="p_value"){
    if(is.nan(beta_1)){
      beta_1 <- 0
      p <- 1
    }else{
      p <- summary(modelo)$coefficients[2,4]
      if(is.nan(p)) p <- 1
    }
    return(p)
  }
  #
  if(output == "partial"){
    partial <- df |>
      dplyr::summarise(xco2 = mean(xco2), na.mr=TRUE) |>
      dplyr::pull(xco2)
    return(partial)
  }

  if(output == "n"){
    return(nrow(df))
  }
}


### calculating for each pixel
xco2_nest <- xco2_nest |>
  dplyr::mutate(
    beta_line = purrr::map(data,linear_reg, output="beta1"),
    p_value = purrr::map(data,linear_reg, output="p_value"),
    partial = purrr::map(data,linear_reg, output="partial"),
    n_obs = purrr::map(data,linear_reg, output="n")
  )

xco2_nest |>
  dplyr::filter(n_obs > 7) |>
  dplyr::select(lon, lat, n_obs) |>
  ggplot2::ggplot(ggplot2::aes(x=lon, y=lat, color = n_obs)) +
  ggplot2::geom_point()


xco2_nest |>
  dplyr::filter(n_obs > 5) |>
  dplyr::select(lon, lat, n_obs) |>
  ggplot2::ggplot(ggplot2::aes(x=lon, y=lat, color = n_obs)) +
  ggplot2::geom_point()

readr::write_rds(xco2_nest,'beta_xco2_detrend.rds')

### creating a table

xco2_aux_trend_new <- xco2_nest |>
  dplyr::filter(n_obs > 7) |>
  tidyr::unnest(cols = c(beta_line, partial)) |>
  dplyr::ungroup() |>
  dplyr::select(lon, lat, beta_line, partial)

q3_xco2 <- xco2_aux_trend_new |> dplyr::pull(beta_line) |> quantile(.75)
q1_xco2 <- xco2_aux_trend_new |> dplyr::pull(beta_line) |> quantile(.25)

xco2_aux_trend_new <- xco2_aux_trend_new  |>
  dplyr::mutate(
    anomaly =  partial - xco2_aux_trend_new  |>
      dplyr::pull(partial) |>
      mean(),
    Dbeta = beta_line -xco2_aux_trend_new  |>
      dplyr::pull(beta_line) |> mean(na.rm=TRUE)
  )

q3_anom <- xco2_aux_trend_new  |> dplyr::pull(anomaly) |> quantile(.75)

xco2_aux_trend_new  <- xco2_aux_trend_new  |>
  dplyr::mutate(
    beta_index =  ifelse(beta_line >=q1_xco2, 0, 1)
  )
xco2_aux_trend_new |>
  ggplot2::ggplot(ggplot2::aes(x=beta_line)) +
  ggplot2::geom_histogram(bins=30,
                          fill="orange",
                          color="black") +
  ggplot2::labs(x="βpixel",y="Count") +
  ggplot2::geom_vline(xintercept = q1_xco2,
                      color = "red",
                      lty=2) +
  gghighlight::gghighlight(beta_line < q1_xco2,
                           unhighlighted_params = list(
                             color = "darkgray",
                             fill = "lightgray")) +
  ggplot2::theme_minimal()


xco2_aux_trend_new |>
  ggplot2::ggplot(ggplot2::aes(x=anomaly)) +
  ggplot2::geom_histogram(bins=30,
                          fill="lightblue",
                          color="black") +
  ggplot2::labs(x="Anomaly",y="Count") +
  ggplot2::geom_vline(xintercept = q3_anom,
                      color = "red",
                      lty=2) +
  gghighlight::gghighlight(anomaly > q3_anom,
                           unhighlighted_params = list(
                             color = "darkgray",
                             fill = "lightgray")) +
  ggplot2::theme_minimal()


xco2_aux_trend_new |>
  ggplot2::ggplot(ggplot2::aes(x=lon,y=lat), color = 'black')+
  ggplot2::geom_tile(ggplot2::aes(fill=beta_line))+
  ggplot2::scale_fill_gradient(low='yellow',high='blue')+
  ggplot2::coord_equal()+
  ggplot2::labs(fill='βpixel')+
  ggplot2::theme_bw()

xco2_aux_trend_new |>
  ggplot2::ggplot(ggplot2::aes(x=lon,y=lat), color = 'black')+
  ggplot2::geom_tile(ggplot2::aes(fill=Dbeta))+
  ggplot2::scale_fill_gradient(low='yellow',high='blue')+
  ggplot2::coord_equal()+
  ggplot2::labs(fill='dif βpixel')+
  ggplot2::theme_bw()


xco2_aux_trend_new |>
  dplyr::mutate(
    xco2 = dplyr::case_when(
      beta_line > q3_xco2 ~ 'Source',
      beta_line < q1_xco2 ~'Sink',
      .default = 'Non Significant'
    )
  ) |>
  ggplot2::ggplot(ggplot2::aes(x=lon,y=lat), color = 'black')+
  ggplot2::geom_tile(ggplot2::aes(fill=xco2))+
  #ggplot2::scale_fill_gradient(low='yellow',high='blue')+
  ggplot2::coord_equal()+
  ggplot2::labs(fill='XCO2')+
  ggplot2::theme_bw()

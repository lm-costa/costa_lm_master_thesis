source('r/function.R')


br <- geobr::read_country(showProgress = FALSE)
south_file <- list.files('South_America/',pattern = 'shp',full.names = T)
south_america <- sf::read_sf(south_file[1])
biomes <- geobr::read_biomes(showProgress = FALSE) |>
  dplyr::filter(name_biome!='Sistema Costeiro')#brazilian biomes shp

xco2df <- readr::read_rds('data/xco2_0.5deg_full_trend.rds')


xco2df |>
  dplyr::filter(dist_xco2<0.25) |>
  dplyr::group_by(lon_grid,lat_grid,year,month) |>
  dplyr::summarise(
    xco2_mean = mean(xco2_new),
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
  dplyr::filter(dist_xco2<0.25) |>
  dplyr::group_by(date) |>
  dplyr::summarise(xco2_mean=mean(xco2)) |>
  ggplot2::ggplot(ggplot2::aes(x=date,y=xco2_mean)) +
  ggplot2::geom_point(shape=21,color="black",fill="gray") +
  #ggplot2::geom_line(color="red") +
  ggplot2::geom_smooth(method = "lm") +
  ggpubr::stat_regline_equation(ggplot2::aes(
    label =  paste(..eq.label.., ..rr.label.., sep = "*plain(\",\")~~"))) +
  ggplot2::theme_bw()+
  ggplot2::labs(
    x='',
    y=expression('Xco'[2]~' (ppm)')
  )

ggplot2::ggsave('img/xco2_temporal.png',units="in", width=8, height=6,
                dpi=300)

xco2df |>
  dplyr::filter(dist_xco2<0.25) |>
  dplyr::filter(year %in% 2015:2022) |>
  dplyr::group_by(year,date) |>
  dplyr::summarise(xco2_mean=mean(xco2)) |>
  ggplot2::ggplot(ggplot2::aes(x=date,y=xco2_mean))+
  ggplot2::geom_point(shape=21,color="black",fill="gray") +
  ggplot2::geom_line(color="red")+
  ggplot2::geom_smooth(method = "lm") +
  ggplot2::ylim(390,420)+
  ggpubr::stat_regline_equation(ggplot2::aes(
                                  label =  paste(..eq.label.., ..rr.label..,
                                                 sep = "*plain(\",\")~~")),
                                label.y = 420) +
  ggplot2::facet_wrap(~year,scales ='free')+
  ggplot2::theme_bw()+
  ggplot2::labs(x='',y=expression('Xco'[2]~' (ppm)'),fill='' )
  #ggplot2::scale_fill_viridis_d()


ggplot2::ggsave('img/xco2_temporal_ano.png',units="in", width=10, height=7,
                dpi=300)

###
xco2df |>
  dplyr::filter(dist_xco2<0.25) |>
  dplyr::mutate(
    date = lubridate::make_date(year,month,'15')
  ) |>
  dplyr::group_by(date) |>
  dplyr::summarise(xco2 = mean(xco2)) |>
  dplyr::ungroup() |>
  dplyr::select(xco2) |>
  apply(2,est_desc)

xco2df |>
  dplyr::filter(dist_xco2<0.25) |>
  dplyr::mutate(
    date = lubridate::make_date(year,month,'15')
  ) |>
  dplyr::group_by(date) |>
  dplyr::summarise(xco2 = mean(xco2)) |>
  dplyr::ungroup() |>
  dplyr::select(xco2) |>
  apply(2,trend::mk.test)

### Geral
xco2df_filter <- xco2df |>
  dplyr::filter(dist_xco2<0.25) |>
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

mod <- lm(xco2_mean~x,data =xco2df_filter |>
            dplyr::mutate(
              x = 1:dplyr::n()
            ))


xco2df_filter |>
  dplyr::mutate(
    x=1:dplyr::n(),
    xco2_est = mod$coefficients[1] + mod$coefficients[2]*x,
    delta=xco2_est - xco2_mean,
    xco2r = (mod$coefficients[1]-delta)-(mean(xco2_mean)-mod$coefficients[1])
  ) |>
  dplyr::group_by(date) |>
  dplyr::summarise(xco2_mean=mean(xco2r)) |>
  ggplot2::ggplot(ggplot2::aes(x=date,y=xco2_mean )) +
  ggplot2::geom_point(shape=21,color="black",fill="gray") +
  ggplot2::geom_line(color="red") +
  ggplot2::geom_smooth(method = "lm") +
  ggpubr::stat_regline_equation(ggplot2::aes(
    label =  paste(..eq.label.., ..rr.label.., sep = "*plain(\",\")~~"))) +
  ggplot2::theme_bw()+
  ggplot2::xlab('Date')+
  ggplot2::ylab(expression(
    'Xco'[2][R]~' (ppm)'
  ))


xco2detrend <- xco2df_filter |>
  dplyr::mutate(
    x=1:dplyr::n(),
    xco2_est = mod$coefficients[1] + mod$coefficients[2]*x,
    delta=xco2_est - xco2_mean,
    xco2r = (mod$coefficients[1]-delta)-(mean(xco2_mean)-mod$coefficients[1])
  )
xco2_aux_detrend <- xco2detrend |>
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
hist(xco2detrend$xco2r)

xco2_nest <- xco2detrend |>
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

### calculating for each pixel
xco2_nest_detrend <- xco2_nest|>
  dplyr::mutate(
    beta_line = purrr::map(data,linear_reg, output="beta1"),
    p_value = purrr::map(data,linear_reg, output="p_value"),
    partial = purrr::map(data,linear_reg, output="partial"),
    n_obs = purrr::map(data,linear_reg, output="n"),
    beta_error=purrr::map(data,linear_reg,output='betaerror'),
    model_error=purrr::map(data,linear_reg,output='modelerror')
  )

xco2_nest_detrend |>dplyr::filter(n_obs > 12) |>
  tidyr::unnest(cols = c(beta_error)) |>
  dplyr::select(lon, lat, beta_error) |>
  ggplot2::ggplot(ggplot2::aes(x=lon, y=lat, color = beta_error)) +
  ggplot2::geom_point()

xco2_nest_detrend |>
  dplyr::filter(n_obs > 12) |>
  tidyr::unnest(cols = c(beta_error,model_error)) |>
  dplyr::ungroup() |>
  dplyr::summarise(b=mean(beta_error),
                   m=mean(model_error))

xco2_nest_detrend |>dplyr::filter(n_obs > 7) |>
  tidyr::unnest(cols = c(beta_error)) |>
  dplyr::select(lon, lat, beta_error) |>
  ggplot2::ggplot(ggplot2::aes(x=lon, y=lat, color = beta_error)) +
  ggplot2::geom_point()

xco2_nest_detrend |>
  dplyr::filter(n_obs > 7) |>
  tidyr::unnest(cols = c(beta_error,model_error)) |>
  dplyr::ungroup() |>
  dplyr::summarise(b=mean(beta_error),
                   m=mean(model_error))


xco2_nest_detrend |>dplyr::filter(n_obs > 5) |>
  tidyr::unnest(cols = c(beta_error)) |>
  dplyr::select(lon, lat, beta_error) |>
  ggplot2::ggplot(ggplot2::aes(x=lon, y=lat, color = beta_error)) +
  ggplot2::geom_point()

xco2_nest_detrend |>
  dplyr::filter(n_obs > 5) |>
  tidyr::unnest(cols = c(beta_error,model_error)) |>
  dplyr::ungroup() |>
  dplyr::summarise(b=mean(beta_error),
                   m=mean(model_error))

xco2_nest_detrend |>dplyr::filter(n_obs > 4) |>
  tidyr::unnest(cols = c(beta_error)) |>
  dplyr::select(lon, lat, beta_error) |>
  ggplot2::ggplot(ggplot2::aes(x=lon, y=lat, color = beta_error)) +
  ggplot2::geom_point()

xco2_nest_detrend |>
  dplyr::filter(n_obs > 4) |>
  tidyr::unnest(cols = c(beta_error,model_error)) |>
  dplyr::ungroup() |>
  dplyr::summarise(b=mean(beta_error),
                   m=mean(model_error))

xco2_nest_detrend |>dplyr::filter(n_obs > 3) |>
  tidyr::unnest(cols = c(beta_error)) |>
  dplyr::select(lon, lat, beta_error) |>
  ggplot2::ggplot(ggplot2::aes(x=lon, y=lat, color = beta_error)) +
  ggplot2::geom_point()

xco2_nest_detrend |>
  dplyr::filter(n_obs > 3) |>
  tidyr::unnest(cols = c(beta_error,model_error)) |>
  dplyr::ungroup() |>
  dplyr::summarise(b=mean(beta_error),
                   m=mean(model_error))

### creating a table

xco2_aux_detrend_new <- xco2_nest_detrend |>
  dplyr::filter(n_obs > 5) |>
  tidyr::unnest(cols = c(beta_line, partial,beta_error,model_error)) |>
  dplyr::ungroup() |>
  dplyr::select(lon, lat, beta_line, partial,beta_error,model_error)

q3_xco2 <- xco2_aux_detrend_new |> dplyr::pull(beta_line) |> quantile(.75)
q1_xco2 <- xco2_aux_detrend_new |> dplyr::pull(beta_line) |> quantile(.25)

xco2_aux_detrend_new <- xco2_aux_detrend_new  |>
  dplyr::mutate(
    anomaly = partial - xco2_aux_detrend_new  |>
      dplyr::pull(partial) |>
      median(),
    Dbeta = beta_line -xco2_aux_detrend_new  |>
      dplyr::pull(beta_line) |> mean(na.rm=TRUE)
  )

q3_anom <- xco2_aux_detrend_new  |> dplyr::pull(anomaly) |> quantile(.75)
q1_anom <- xco2_aux_detrend_new  |> dplyr::pull(anomaly) |> quantile(.25)

xco2_aux_detrend_new  <- xco2_aux_detrend_new  |>
  dplyr::mutate(
    beta_index =  ifelse(beta_line >=q1_xco2, 0, 1)
  )
xco2_aux_detrend_new |>
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

### Source and Sink Indentification

br |>
  ggplot2::ggplot()+
  ggplot2::geom_sf(fill="white", color="black",
                   size=.15, show.legend = FALSE)+
  ggplot2::geom_point(data=xco2_aux_detrend_new |>
                        dplyr::mutate(
                          xco2 = dplyr::case_when(
                            beta_line > q3_xco2 ~ 'Source',
                            beta_line < q1_xco2 ~'Sink',
                            .default = 'Non Significant'
                          )
                        ) |>
                        dplyr::filter(xco2!='Non Significant'),
                      ggplot2::aes(x=lon,y=lat,color=xco2),
  )+
  tema_mapa()+
  ggplot2::scale_color_manual(values = c('darkblue','darkred'))+
  ggplot2::labs(x='Longitude',y='Latitude',col=expression('Xco'[2]))

ggplot2::ggsave('img/xco2_identification_geral.png',units="in", width=8, height=6,
                dpi=300)

### 2015

####detrending

xco2df_filter <- xco2df |>
  dplyr::filter(year == 2015) |>
  dplyr::filter(dist_xco2<0.25) |>
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


xco2df_filter |>
  dplyr::group_by(date) |>
  dplyr::summarise(xco2_mean=mean(xco2_mean)) |>
  ggplot2::ggplot(ggplot2::aes(x=date,y=xco2_mean )) +
  ggplot2::geom_point(shape=21,color="black",fill="gray") +
  ggplot2::geom_line(color="red") +
  ggplot2::geom_smooth(method = "lm") +
  ggpubr::stat_regline_equation(ggplot2::aes(
    label =  paste(..eq.label.., ..rr.label.., sep = "*plain(\",\")~~"))) +
  ggplot2::theme_bw()+
  ggplot2::xlab('Date')+
  ggplot2::ylab(expression(
    'Xco'[2]~' (ppm)'
  ))

ggplot2::ggsave('img/xco2_temporal_2015.png',units="in", width=8, height=6,
                dpi=300)

mod <- lm(xco2_mean~x,data =xco2df_filter |>
             dplyr::mutate(
               x = 1:dplyr::n()
             ))


xco2df_filter |>
  dplyr::mutate(
    x=1:dplyr::n(),
    xco2_est = mod$coefficients[1] + mod$coefficients[2]*x,
    delta=xco2_est - xco2_mean,
    xco2r = (mod$coefficients[1]-delta)-(mean(xco2_mean)-mod$coefficients[1])
  ) |>
  dplyr::group_by(date) |>
  dplyr::summarise(xco2_mean=mean(xco2r)) |>
  ggplot2::ggplot(ggplot2::aes(x=date,y=xco2_mean )) +
  ggplot2::geom_point(shape=21,color="black",fill="gray") +
  ggplot2::geom_line(color="red") +
  ggplot2::geom_smooth(method = "lm") +
  ggpubr::stat_regline_equation(ggplot2::aes(
    label =  paste(..eq.label.., ..rr.label.., sep = "*plain(\",\")~~"))) +
  ggplot2::theme_bw()+
  ggplot2::xlab('Date')+
  ggplot2::ylab(expression(
    'Xco'[2][R]~' (ppm)'
  ))

ggplot2::ggsave('img/xco2_temporal_2015_detrend.png',units="in", width=8, height=6,
                dpi=300)

xco2detrend <- xco2df_filter |>
  dplyr::mutate(
    x=1:dplyr::n(),
    xco2_est = mod$coefficients[1] + mod$coefficients[2]*x,
    delta=xco2_est - xco2_mean,
    xco2r = (mod$coefficients[1]-delta)-(mean(xco2_mean)-mod$coefficients[1])
  )
xco2_aux_detrend <- xco2detrend |>
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
hist(xco2detrend$xco2r)

xco2_nest <- xco2detrend |>
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

### calculating for each pixel
xco2_nest_detrend <- xco2_nest|>
  dplyr::mutate(
    beta_line = purrr::map(data,linear_reg, output="beta1"),
    p_value = purrr::map(data,linear_reg, output="p_value"),
    partial = purrr::map(data,linear_reg, output="partial"),
    n_obs = purrr::map(data,linear_reg, output="n"),
    beta_error=purrr::map(data,linear_reg,output='betaerror'),
    model_error=purrr::map(data,linear_reg,output='modelerror')
  )



xco2_nest_detrend |>dplyr::filter(n_obs > 7) |>
  tidyr::unnest(cols = c(beta_error)) |>
  dplyr::select(lon, lat, beta_error) |>
  ggplot2::ggplot(ggplot2::aes(x=lon, y=lat, color = beta_error)) +
  ggplot2::geom_point()

xco2_nest_detrend |>
  dplyr::filter(n_obs > 7) |>
  tidyr::unnest(cols = c(beta_error,model_error)) |>
  dplyr::ungroup() |>
  dplyr::summarise(b=mean(beta_error),
                   m=mean(model_error))


xco2_nest_detrend |>dplyr::filter(n_obs > 5) |>
  tidyr::unnest(cols = c(beta_error)) |>
  dplyr::select(lon, lat, beta_error) |>
  ggplot2::ggplot(ggplot2::aes(x=lon, y=lat, color = beta_error)) +
  ggplot2::geom_point()

xco2_nest_detrend |>
  dplyr::filter(n_obs > 5) |>
  tidyr::unnest(cols = c(beta_error,model_error)) |>
  dplyr::ungroup() |>
  dplyr::summarise(b=mean(beta_error),
                   m=mean(model_error))

xco2_nest_detrend |>dplyr::filter(n_obs > 4) |>
  tidyr::unnest(cols = c(beta_error)) |>
  dplyr::select(lon, lat, beta_error) |>
  ggplot2::ggplot(ggplot2::aes(x=lon, y=lat, color = beta_error)) +
  ggplot2::geom_point()

xco2_nest_detrend |>
  dplyr::filter(n_obs > 4) |>
  tidyr::unnest(cols = c(beta_error,model_error)) |>
  dplyr::ungroup() |>
  dplyr::summarise(b=mean(beta_error),
                   m=mean(model_error))

xco2_nest_detrend |>dplyr::filter(n_obs > 3) |>
  tidyr::unnest(cols = c(beta_error)) |>
  dplyr::select(lon, lat, beta_error) |>
  ggplot2::ggplot(ggplot2::aes(x=lon, y=lat, color = beta_error)) +
  ggplot2::geom_point()

xco2_nest_detrend |>
  dplyr::filter(n_obs > 3) |>
  tidyr::unnest(cols = c(beta_error,model_error)) |>
  dplyr::ungroup() |>
  dplyr::summarise(b=mean(beta_error),
                   m=mean(model_error))

### creating a table

xco2_aux_detrend_new <- xco2_nest_detrend |>
  dplyr::filter(n_obs > 4) |>
  tidyr::unnest(cols = c(beta_line, partial,beta_error,model_error)) |>
  dplyr::ungroup() |>
  dplyr::select(lon, lat, beta_line, partial,beta_error,model_error)

q3_xco2 <- xco2_aux_detrend_new |> dplyr::pull(beta_line) |> quantile(.75)
q1_xco2 <- xco2_aux_detrend_new |> dplyr::pull(beta_line) |> quantile(.25)

xco2_aux_detrend_new <- xco2_aux_detrend_new  |>
  dplyr::mutate(
    anomaly = partial - xco2_aux_detrend_new  |>
      dplyr::pull(partial) |>
      median(),
    Dbeta = beta_line -xco2_aux_detrend_new  |>
      dplyr::pull(beta_line) |> mean(na.rm=TRUE)
  )

q3_anom <- xco2_aux_detrend_new  |> dplyr::pull(anomaly) |> quantile(.75)
q1_anom <- xco2_aux_detrend_new  |> dplyr::pull(anomaly) |> quantile(.25)

xco2_aux_detrend_new  <- xco2_aux_detrend_new  |>
  dplyr::mutate(
    beta_index =  ifelse(beta_line >=q1_xco2, 0, 1)
  )
xco2_aux_detrend_new |>
  ggplot2::ggplot(ggplot2::aes(x=beta_line)) +
  ggplot2::geom_histogram(bins=30,
                          fill="orange",
                          color="black") +
  ggplot2::labs(x="βpixel",y="Count") +
  ggplot2::geom_vline(xintercept = q1_xco2,
                      color = "red",
                      lty=2) +
  ggplot2::geom_vline(xintercept = q3_xco2,
                      color = "red",
                      lty=2)+
  gghighlight::gghighlight(beta_line < q1_xco2 | beta_line>q3_xco2,
                           unhighlighted_params = list(
                             color = "darkgray",
                             fill = "lightgray")) +
  ggplot2::theme_minimal()



### Source and Sink Indentification

south_america |>
  ggplot2::ggplot()+
  ggspatial::annotation_map_tile(type = 'cartolight')+
  ggplot2::geom_sf(col='grey',fill='white')+
  ggplot2::geom_sf(data=biomes,col='black',fill='NA',linetype='dashed')+
  ggplot2::ylim(-40,6)+
  ggplot2::xlim(-80,-35)+
  ggplot2::geom_tile(data=xco2_aux_detrend_new |>
                        dplyr::mutate(
                          xco2 = dplyr::case_when(
                            beta_line > q3_xco2 ~ 'Source',
                            beta_line < q1_xco2 ~'Sink',
                            .default = 'Non Significant'
                          )
                        ) |>
                        dplyr::filter(xco2!='Non Significant'),
                      ggplot2::aes(x=lon,y=lat,color=xco2,fill=xco2),
  )+
  tema_mapa2()+
  ggplot2::scale_color_manual(values = c('darkgreen','darkred'))+
  ggplot2::scale_fill_manual(values = c('darkgreen','darkred'))+
  ggplot2::labs(x='Longitude',y='Latitude',
                col=expression('Xco'[2]),fill=expression('Xco'[2]))

ggplot2::ggsave('img/xco2_identification_new_2015.png',units="in", width=8, height=6,
                dpi=300)

br |>
  ggplot2::ggplot()+
  ggplot2::geom_sf(fill="white", color="black",
                   size=.15, show.legend = FALSE)+
  ggplot2::geom_point(data=xco2_aux_detrend_new |>
                        dplyr::mutate(
                          xco2 = dplyr::case_when(
                            beta_line > q3_xco2 ~ 'Source',
                            beta_line < q1_xco2 ~'Sink',
                            .default = 'Non Significant'
                          )
                        ) |>
                        dplyr::filter(xco2!='Non Significant'),
                      ggplot2::aes(x=lon,y=lat,color=xco2),
  )+
  tema_mapa()+
  ggplot2::scale_color_manual(values = c('darkblue','darkred'))+
  ggplot2::labs(x='Longitude',y='Latitude',col=expression('Xco'[2]))

ggplot2::ggsave('img/xco2_identification_2015.png',units="in", width=8, height=6,
                dpi=1000)

xco2_aux_detrend_new |>
  dplyr::filter(beta_line<0) |>
  dplyr::summarise(
    nobs=dplyr::n()
  )


### Beta
south_america |>
  ggplot2::ggplot()+
  ggspatial::annotation_map_tile(type = 'cartolight')+
  ggplot2::geom_sf(col='grey',fill='white')+
  ggplot2::geom_sf(data=biomes,col='black',fill='NA',linetype='dashed')+
  ggplot2::ylim(-40,6)+
  ggplot2::xlim(-80,-35)+
  ggplot2::geom_tile(data=xco2_aux_detrend_new |>
                        dplyr::mutate(
                          xco2 = dplyr::case_when(
                            beta_line > q3_xco2 ~ 'Source',
                            beta_line < q1_xco2 ~'Sink',
                            .default = 'Non Significant'
                          )
                        ) |>
                        dplyr::filter(xco2!='Non Significant'),
                      ggplot2::aes(x=lon,y=lat,
                                   col=beta_line*30,
                                   fill=beta_line*30),
  )+
  ggplot2::scale_color_gradient(high='yellow',low='blue')+
  ggplot2::scale_fill_gradient(high='yellow',low='blue')+
  tema_mapa2()+
  ggplot2::labs(x='Longitude',y='Latitude',
                col=expression('β (ppm '~month^-1~')'),
                fill=expression('β (ppm '~month^-1~')')
                )
ggplot2::ggsave('img/xco2_beta_new_2015.png',units="in", width=8, height=6,
                dpi=300)


br |>
  ggplot2::ggplot()+
  ggplot2::geom_sf(fill="white", color="black",
                   size=.15, show.legend = FALSE)+
  ggplot2::geom_point(data=xco2_aux_detrend_new |>
                        dplyr::mutate(
                          xco2 = dplyr::case_when(
                            beta_line > q3_xco2 ~ 'Source',
                            beta_line < q1_xco2 ~'Sink',
                            .default = 'Non Significant'
                          )
                        ) |>
                        dplyr::filter(xco2!='Non Significant'),
                      ggplot2::aes(x=lon,y=lat,col=beta_line*30),
  )+
  ggplot2::scale_color_gradient(high='yellow',low='blue')+
  tema_mapa()+
  ggplot2::labs(x='Longitude',y='Latitude',col=expression('β (ppm '~month^-1~')'))

ggplot2::ggsave('img/xco2_beta_2015.png',units="in", width=8, height=6,
                dpi=1000)


### uncertanty
south_america |>
  ggplot2::ggplot()+
  ggspatial::annotation_map_tile(type = 'cartolight')+
  ggplot2::geom_sf(col='grey',fill='white')+
  ggplot2::geom_sf(data=biomes,col='black',fill='NA',linetype='dashed')+
  ggplot2::ylim(-40,6)+
  ggplot2::xlim(-80,-35)+
  ggplot2::geom_tile(data=xco2_aux_detrend_new |>
                       dplyr::mutate(
                         xco2 = dplyr::case_when(
                           beta_line > q3_xco2 ~ 'Source',
                           beta_line < q1_xco2 ~'Sink',
                           .default = 'Non Significant'
                         )
                       ) |>
                       dplyr::filter(xco2!='Non Significant'),
                     ggplot2::aes(x=lon,y=lat,
                                  col=beta_error*30,
                                  fill=beta_error*30),
  )+
  ggplot2::scale_color_gradient(high='yellow',low='blue')+
  ggplot2::scale_fill_gradient(high='yellow',low='blue')+
  tema_mapa2()+
  ggplot2::labs(x='Longitude',y='Latitude',
                col=expression('β error (ppm '~month^-1~')'),
                fill=expression('β error (ppm '~month^-1~')')
  )
ggplot2::ggsave('img/xco2_beta_error_new_2015.png',units="in", width=8, height=6,
                dpi=300)

br |>
  ggplot2::ggplot()+
  ggplot2::geom_sf(fill="white", color="black",
                   size=.15, show.legend = FALSE)+
  ggplot2::geom_point(data=xco2_aux_detrend_new |>
                        dplyr::mutate(
                          xco2 = dplyr::case_when(
                            beta_line > q3_xco2 ~ 'Source',
                            beta_line < q1_xco2 ~'Sink',
                            .default = 'Non Significant'
                          )
                        ) |>
                        dplyr::filter(xco2!='Non Significant'),
                      ggplot2::aes(x=lon,y=lat,col=beta_error*30),
  )+
  ggplot2::scale_color_gradient(high='yellow',low='blue')+
  tema_mapa()+
  ggplot2::labs(x='Longitude',y='Latitude',col=expression('β error (ppm '~month^-1~')'))

ggplot2::ggsave('img/xco2_betaerror_2015.png',units="in", width=8, height=6,
                dpi=1000)

br |>
  ggplot2::ggplot()+
  ggplot2::geom_sf(fill="white", color="black",
                   size=.15, show.legend = FALSE)+
  ggplot2::geom_point(data=xco2_aux_detrend_new |>
                        dplyr::mutate(
                          xco2 = dplyr::case_when(
                            beta_line > q3_xco2 ~ 'Source',
                            beta_line < q1_xco2 ~'Sink',
                            .default = 'Non Significant'
                          )
                        ) |>
                        dplyr::filter(xco2!='Non Significant'),
                      ggplot2::aes(x=lon,y=lat,col=model_error),
  )+
  ggplot2::scale_color_gradient(high='yellow',low='blue')+
  tema_mapa()+
  ggplot2::labs(x='Longitude',y='Latitude',col=expression('Model error (ppm '~month^-1~')'))

ggplot2::ggsave('img/xco2_model_2015.png',units="in", width=8, height=6,
                dpi=1000)


#### CO2 emission and assimilation
south_america |>
  ggplot2::ggplot()+
  ggspatial::annotation_map_tile(type = 'cartolight')+
  ggplot2::geom_sf(col='grey',fill='white')+
  ggplot2::geom_sf(data=biomes,col='black',fill='NA',linetype='dashed')+
  ggplot2::ylim(-40,6)+
  ggplot2::xlim(-80,-35)+
  ggplot2::geom_tile(data=xco2_aux_detrend_new |>
                       dplyr::mutate(
                         xco2 = dplyr::case_when(
                           beta_line > q3_xco2 ~ 'Source',
                           beta_line < q1_xco2 ~'Sink',
                           .default = 'Non Significant'
                         )
                       ) |>
                       dplyr::filter(xco2!='Non Significant') |>
                       dplyr::mutate(
                         beta_molm=(beta_line*10000)*44/24.45,
                         fco2 = beta_molm*30/1000,
                       ),
                     ggplot2::aes(x=lon,y=lat,color=fco2,fill=fco2)
  )+
  ggplot2::scale_color_gradient(high='yellow',low='blue')+
  ggplot2::scale_fill_gradient(high='yellow',low='blue')+
  tema_mapa2()+
  ggplot2::labs(x='Longitude',y='Latitude',
                col=expression('FCO'[2]~'(g'~m^-2*month^-1~')'),
                fill=expression('FCO'[2]~'(g'~m^-2*month^-1~')')
  )

ggplot2::ggsave('img/xco2_emission_nwe_2015.png',units="in", width=8, height=6,
                dpi=300)

br |>
  ggplot2::ggplot()+
  ggplot2::geom_sf(fill="white", color="black",
                   size=.15, show.legend = FALSE)+
  ggplot2::geom_point(data=xco2_aux_detrend_new |>
                        dplyr::mutate(
                          xco2 = dplyr::case_when(
                            beta_line > q3_xco2 ~ 'Source',
                            beta_line < q1_xco2 ~'Sink',
                            .default = 'Non Significant'
                          )
                        ) |>
                        dplyr::filter(xco2!='Non Significant') |>
                        dplyr::mutate(
                          beta_molm=(beta_line*10000)*44/24.45,
                          fco2 = beta_molm*30/1000,
                        ),
                      ggplot2::aes(x=lon,y=lat,color=fco2),
  )+
  ggplot2::scale_color_gradient(high='yellow',low='blue')+
  tema_mapa()+
  ggplot2::labs(x='Longitude',y='Latitude',col=expression('FCO'[2]~'(g'~m^-2*month^-1~')'))

ggplot2::ggsave('img/xco2_emission_2015.png',units="in", width=8, height=6,
                dpi=1000)

south_america |>
  ggplot2::ggplot()+
  ggspatial::annotation_map_tile(type = 'cartolight')+
  ggplot2::geom_sf(col='grey',fill='white')+
  ggplot2::geom_sf(data=biomes,col='black',fill='NA',linetype='dashed')+
  ggplot2::ylim(-35,6)+
  ggplot2::xlim(-75,-35)+
  ggplot2::geom_tile(data=xco2_aux_detrend_new |>
                       dplyr::mutate(
                         xco2 = dplyr::case_when(
                           beta_line > q3_xco2 ~ 'Source',
                           beta_line < q1_xco2 ~'Sink',
                           .default = 'Non Significant'
                         )
                       ) |>
                       dplyr::filter(xco2!='Non Significant')|>
                       dplyr::mutate(
                         beta_molm=(beta_error*10000)*44/24.45,
                         fco2_error = beta_molm*30/1000),
                     ggplot2::aes(x=lon,y=lat,color=fco2_error,fill=fco2_error)
  )+
  ggplot2::scale_color_gradient(high='yellow',low='blue')+
  ggplot2::scale_fill_gradient(high='yellow',low='blue')+
  tema_mapa2()+
  ggplot2::labs(x='Longitude',y='Latitude',
                col=expression('FCO'[2]~'error (g'~m^-2*month^-1~')'),
                fill=expression('FCO'[2]~'error (g'~m^-2*month^-1~')')
  )

ggplot2::ggsave('img/xco2_emissionerror_new_2015.png',units="in", width=8, height=6,
                dpi=300)

br |>
  ggplot2::ggplot()+
  ggplot2::geom_sf(fill="white", color="black",
                   size=.15, show.legend = FALSE)+
  ggplot2::geom_point(data=xco2_aux_detrend_new |>
                        dplyr::mutate(
                          xco2 = dplyr::case_when(
                            beta_line > q3_xco2 ~ 'Source',
                            beta_line < q1_xco2 ~'Sink',
                            .default = 'Non Significant'
                          )
                        ) |>
                        dplyr::filter(xco2!='Non Significant')|>
                        dplyr::mutate(
                          beta_molm=(beta_error*10000)*44/24.45,
                          fco2_error = beta_molm*30/1000,
                        ),
                      ggplot2::aes(x=lon,y=lat,color=fco2_error),
  )+
  ggplot2::scale_color_gradient(high='yellow',low='blue')+
  tema_mapa()+
  ggplot2::labs(x='Longitude',y='Latitude',col=expression('FCO'[2]~' error (g'~m^-2*month^-1~')'))

ggplot2::ggsave('img/xco2_emissionerror_2015.png',units="in", width=8, height=6,
                dpi=1000)

br |>
  ggplot2::ggplot()+
  ggplot2::geom_sf(fill="white", color="black",
                   size=.15, show.legend = FALSE)+
  ggplot2::geom_point(data=xco2_aux_detrend_new |>
                        dplyr::mutate(
                          xco2 = dplyr::case_when(
                            beta_line > q3_xco2 ~ 'Source',
                            beta_line < q1_xco2 ~'Sink',
                            .default = 'Non Significant'
                          )
                        ) |>
                        dplyr::filter(xco2!='Non Significant') |>
                        dplyr::mutate(
                          beta_molm=(10000*model_error)*44/24.45,
                          fco2_error = beta_molm*12.01/1000,
                        ),
                      ggplot2::aes(x=lon,y=lat,color=fco2_error),
  )+
  ggplot2::scale_color_gradient(high='yellow',low='blue')+
  tema_mapa()


####Balance

xco2_aux_detrend_new |>
  dplyr::mutate(
    xco2 = dplyr::case_when(
      beta_line > q3_xco2 ~ 'Source',
      beta_line < q1_xco2 ~'Sink',
      .default = 'Non Significant'
    )
  ) |>
  dplyr::filter(xco2!='Non Significant')|>
  dplyr::mutate(
    beta_molm=(10000*beta_line)*44/24.45,
    fco2 = beta_molm*30/1000,
    fco2=(fco2*3025000000)/1e+15
  ) |>
  dplyr::ungroup() |>
  dplyr::summarise(
    fco2=sum(fco2)
  )


xco2_aux_detrend_new |>
  dplyr::mutate(
    xco2 = dplyr::case_when(
      beta_line > q3_xco2 ~ 'Source',
      beta_line < q1_xco2 ~'Sink',
      .default = 'Non Significant'
    )
  ) |>
  dplyr::filter(xco2!='Non Significant') |>
  dplyr::mutate(
    beta_molm=beta_line*44/24.45,
    fco2 = beta_molm*30/1000)|>
  dplyr::ungroup() |>
  dplyr::summarise(
    fco2m=mean(fco2),
    fco2sd=sd(fco2)
  )

df2015 <- xco2_aux_detrend_new |> dplyr::mutate(
  xco2 = dplyr::case_when(
    beta_line > q3_xco2 ~ 'Source',
    beta_line < q1_xco2 ~'Sink',
    .default = 'Non Significant'
  )
) |>
  dplyr::filter(xco2!='Non Significant')

df2015all <- xco2_aux_detrend_new |> dplyr::mutate(
  xco2 = dplyr::case_when(
    beta_line > q3_xco2 ~ 'Source',
    beta_line < q1_xco2 ~'Sink',
    .default = 'Non Significant'
  )
)

#### 2016

xco2df_filter <- xco2df |>
  dplyr::filter(year == 2016) |>
  dplyr::filter(dist_xco2<0.25) |>
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

xco2df_filter |>
  dplyr::group_by(date) |>
  dplyr::summarise(xco2_mean=mean(xco2_mean)) |>
  ggplot2::ggplot(ggplot2::aes(x=date,y=xco2_mean )) +
  ggplot2::geom_point(shape=21,color="black",fill="gray") +
  ggplot2::geom_line(color="red") +
  ggplot2::geom_smooth(method = "lm") +
  ggpubr::stat_regline_equation(ggplot2::aes(
    label =  paste(..eq.label.., ..rr.label.., sep = "*plain(\",\")~~"))) +
  ggplot2::xlab('Date')+
  ggplot2::ylab(expression(
    'Xco'[2]~' (ppm)'
  ))+
  ggplot2::theme_bw()

ggplot2::ggsave('img/xco2_temporal_2016.png',units="in", width=8, height=6,
                dpi=300)

mod <- lm(xco2_mean~x,data =xco2df_filter |>
            dplyr::mutate(
              x = 1:dplyr::n()
            ))


xco2df_filter |>
  dplyr::mutate(
    x=1:dplyr::n(),
    xco2_est = mod$coefficients[1] + mod$coefficients[2]*x,
    delta=xco2_est - xco2_mean,
    xco2r = (mod$coefficients[1]-delta)-(mean(xco2_mean)-mod$coefficients[1])
  ) |>
  dplyr::group_by(date) |>
  dplyr::summarise(xco2_mean=mean(xco2r)) |>
  ggplot2::ggplot(ggplot2::aes(x=date,y=xco2_mean )) +
  ggplot2::geom_point(shape=21,color="black",fill="gray") +
  ggplot2::geom_line(color="red") +
  ggplot2::geom_smooth(method = "lm") +
  ggpubr::stat_regline_equation(ggplot2::aes(
    label =  paste(..eq.label.., ..rr.label.., sep = "*plain(\",\")~~"))) +
  ggplot2::xlab('Date')+
  ggplot2::ylab(expression(
    'Xco'[2][R]~' (ppm)'
  ))+
  ggplot2::theme_bw()

ggplot2::ggsave('img/xco2_detrendtemporal_2016.png',units="in", width=8, height=6,
                dpi=300)

xco2detrend <- xco2df_filter |>
  dplyr::mutate(
    x=1:dplyr::n(),
    xco2_est = mod$coefficients[1] + mod$coefficients[2]*x,
    delta=xco2_est - xco2_mean,
    xco2r = (mod$coefficients[1]-delta)-(mean(xco2_mean)-mod$coefficients[1])
  )
xco2_aux_detrend <- xco2detrend |>
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
hist(xco2detrend$xco2r)

xco2_nest <- xco2detrend |>
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

### calculating for each pixel
xco2_nest_detrend <- xco2_nest|>
  dplyr::mutate(
    beta_line = purrr::map(data,linear_reg, output="beta1"),
    p_value = purrr::map(data,linear_reg, output="p_value"),
    partial = purrr::map(data,linear_reg, output="partial"),
    n_obs = purrr::map(data,linear_reg, output="n"),
    beta_error=purrr::map(data,linear_reg,output='betaerror'),
    model_error=purrr::map(data,linear_reg,output='modelerror')
  )



xco2_nest_detrend |>dplyr::filter(n_obs > 7) |>
  tidyr::unnest(cols = c(beta_error)) |>
  dplyr::select(lon, lat, beta_error) |>
  ggplot2::ggplot(ggplot2::aes(x=lon, y=lat, color = beta_error)) +
  ggplot2::geom_point()

xco2_nest_detrend |>
  dplyr::filter(n_obs > 7) |>
  tidyr::unnest(cols = c(beta_error,model_error)) |>
  dplyr::ungroup() |>
  dplyr::summarise(b=mean(beta_error),
                   m=mean(model_error))


xco2_nest_detrend |>dplyr::filter(n_obs > 5) |>
  tidyr::unnest(cols = c(beta_error)) |>
  dplyr::select(lon, lat, beta_error) |>
  ggplot2::ggplot(ggplot2::aes(x=lon, y=lat, color = beta_error)) +
  ggplot2::geom_point()

xco2_nest_detrend |>
  dplyr::filter(n_obs > 5) |>
  tidyr::unnest(cols = c(beta_error,model_error)) |>
  dplyr::ungroup() |>
  dplyr::summarise(b=mean(beta_error),
                   m=mean(model_error))

xco2_nest_detrend |>dplyr::filter(n_obs > 4) |>
  tidyr::unnest(cols = c(beta_error)) |>
  dplyr::select(lon, lat, beta_error) |>
  ggplot2::ggplot(ggplot2::aes(x=lon, y=lat, color = beta_error)) +
  ggplot2::geom_point()

xco2_nest_detrend |>
  dplyr::filter(n_obs > 4) |>
  tidyr::unnest(cols = c(beta_error,model_error)) |>
  dplyr::ungroup() |>
  dplyr::summarise(b=mean(beta_error),
                   m=mean(model_error))

xco2_nest_detrend |>dplyr::filter(n_obs > 3) |>
  tidyr::unnest(cols = c(beta_error)) |>
  dplyr::select(lon, lat, beta_error) |>
  ggplot2::ggplot(ggplot2::aes(x=lon, y=lat, color = beta_error)) +
  ggplot2::geom_point()

xco2_nest_detrend |>
  dplyr::filter(n_obs > 3) |>
  tidyr::unnest(cols = c(beta_error,model_error)) |>
  dplyr::ungroup() |>
  dplyr::summarise(b=mean(beta_error),
                   m=mean(model_error))

### creating a table

xco2_aux_detrend_new <- xco2_nest_detrend |>
  dplyr::filter(n_obs > 4) |>
  tidyr::unnest(cols = c(beta_line, partial,beta_error,model_error)) |>
  dplyr::ungroup() |>
  dplyr::select(lon, lat, beta_line, partial,beta_error,model_error)

q3_xco2 <- xco2_aux_detrend_new |> dplyr::pull(beta_line) |> quantile(.75)
q1_xco2 <- xco2_aux_detrend_new |> dplyr::pull(beta_line) |> quantile(.25)

xco2_aux_detrend_new <- xco2_aux_detrend_new  |>
  dplyr::mutate(
    anomaly = partial - xco2_aux_detrend_new  |>
      dplyr::pull(partial) |>
      median(),
    Dbeta = beta_line -xco2_aux_detrend_new  |>
      dplyr::pull(beta_line) |> mean(na.rm=TRUE)
  )

q3_anom <- xco2_aux_detrend_new  |> dplyr::pull(anomaly) |> quantile(.75)
q1_anom <- xco2_aux_detrend_new  |> dplyr::pull(anomaly) |> quantile(.25)

xco2_aux_detrend_new  <- xco2_aux_detrend_new  |>
  dplyr::mutate(
    beta_index =  ifelse(beta_line >=q1_xco2, 0, 1)
  )
xco2_aux_detrend_new |>
  ggplot2::ggplot(ggplot2::aes(x=beta_line)) +
  ggplot2::geom_histogram(bins=30,
                          fill="orange",
                          color="black") +
  ggplot2::labs(x="βpixel",y="Count") +
  ggplot2::geom_vline(xintercept = q1_xco2,
                      color = "red",
                      lty=2) +
  ggplot2::geom_vline(xintercept = q3_xco2,
                      color = "red",
                      lty=2)+
  gghighlight::gghighlight(beta_line < q1_xco2 | beta_line>q3_xco2,
                           unhighlighted_params = list(
                             color = "darkgray",
                             fill = "lightgray")) +
  ggplot2::theme_minimal()


xco2_aux_detrend_new |>
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


### Source and Sink Indentification

south_america |>
  ggplot2::ggplot()+
  ggspatial::annotation_map_tile(type = 'cartolight')+
  ggplot2::geom_sf(col='grey',fill='white')+
  ggplot2::geom_sf(data=biomes,col='black',fill='NA',linetype='dashed')+
  ggplot2::ylim(-35,6)+
  ggplot2::xlim(-75,-35)+
  ggplot2::geom_tile(data=xco2_aux_detrend_new |>
                       dplyr::mutate(
                         xco2 = dplyr::case_when(
                           beta_line > q3_xco2 ~ 'Source',
                           beta_line < q1_xco2 ~'Sink',
                           .default = 'Non Significant'
                         )
                       ) |>
                       dplyr::filter(xco2!='Non Significant'),
                     ggplot2::aes(x=lon,y=lat,color=xco2,fill=xco2),
  )+
  tema_mapa2()+
  ggplot2::scale_color_manual(values = c('darkgreen','darkred'))+
  ggplot2::scale_fill_manual(values = c('darkgreen','darkred'))+
  ggplot2::labs(x='Longitude',y='Latitude',
                col=expression('Xco'[2]),fill=expression('Xco'[2]))

ggplot2::ggsave('img/xco2_identification_new_2016.png',units="in", width=8, height=6,
                dpi=300)

br |>
  ggplot2::ggplot()+
  ggplot2::geom_sf(fill="white", color="black",
                   size=.15, show.legend = FALSE)+
  ggplot2::geom_point(data=xco2_aux_detrend_new |>
                        dplyr::mutate(
                          xco2 = dplyr::case_when(
                            beta_line > q3_xco2 ~ 'Source',
                            beta_line < q1_xco2 ~'Sink',
                            .default = 'Non Significant'
                          )
                        ) |>
                        dplyr::filter(xco2!='Non Significant'),
                      ggplot2::aes(x=lon,y=lat,color=xco2),
  )+
  tema_mapa()+
  ggplot2::scale_color_manual(values = c('darkblue','darkred'))+
  ggplot2::labs(x='Longitude',y='Latitude',col=expression('Xco'[2]))

ggplot2::ggsave('img/xco2_identification_2016.png',units="in", width=8, height=6,
                dpi=1000)

xco2_aux_detrend_new |>
  dplyr::filter(beta_line<0) |>
  dplyr::summarise(
    nobs=dplyr::n()
  )


### Beta
south_america |>
  ggplot2::ggplot()+
  ggspatial::annotation_map_tile(type = 'cartolight')+
  ggplot2::geom_sf(col='grey',fill='white')+
  ggplot2::geom_sf(data=biomes,col='black',fill='NA',linetype='dashed')+
  ggplot2::ylim(-40,6)+
  ggplot2::xlim(-80,-35)+
  ggplot2::geom_tile(data=xco2_aux_detrend_new |>
                       dplyr::mutate(
                         xco2 = dplyr::case_when(
                           beta_line > q3_xco2 ~ 'Source',
                           beta_line < q1_xco2 ~'Sink',
                           .default = 'Non Significant'
                         )
                       ) |>
                       dplyr::filter(xco2!='Non Significant'),
                     ggplot2::aes(x=lon,y=lat,
                                  col=beta_line*30,
                                  fill=beta_line*30),
  )+
  ggplot2::scale_color_gradient(high='yellow',low='blue')+
  ggplot2::scale_fill_gradient(high='yellow',low='blue')+
  tema_mapa2()+
  ggplot2::labs(x='Longitude',y='Latitude',
                col=expression('β (ppm '~month^-1~')'),
                fill=expression('β (ppm '~month^-1~')')
  )
ggplot2::ggsave('img/xco2_beta_new_2016.png',units="in", width=8, height=6,
                dpi=300)


br |>
  ggplot2::ggplot()+
  ggplot2::geom_sf(fill="white", color="black",
                   size=.15, show.legend = FALSE)+
  ggplot2::geom_point(data=xco2_aux_detrend_new |>
                        dplyr::mutate(
                          xco2 = dplyr::case_when(
                            beta_line > q3_xco2 ~ 'Source',
                            beta_line < q1_xco2 ~'Sink',
                            .default = 'Non Significant'
                          )
                        ) |>
                        dplyr::filter(xco2!='Non Significant'),
                      ggplot2::aes(x=lon,y=lat,col=beta_line*30),
  )+
  ggplot2::scale_color_gradient(high='yellow',low='blue')+
  tema_mapa()+
  ggplot2::labs(x='Longitude',y='Latitude',col=expression('β (ppm '~month^-1~')'))

ggplot2::ggsave('img/xco2_beta_2016.png',units="in", width=8, height=6,
                dpi=1000)


### uncertanty
south_america |>
  ggplot2::ggplot()+
  ggspatial::annotation_map_tile(type = 'cartolight')+
  ggplot2::geom_sf(col='grey',fill='white')+
  ggplot2::geom_sf(data=biomes,col='black',fill='NA',linetype='dashed')+
  ggplot2::ylim(-40,6)+
  ggplot2::xlim(-80,-35)+
  ggplot2::geom_tile(data=xco2_aux_detrend_new |>
                       dplyr::mutate(
                         xco2 = dplyr::case_when(
                           beta_line > q3_xco2 ~ 'Source',
                           beta_line < q1_xco2 ~'Sink',
                           .default = 'Non Significant'
                         )
                       ) |>
                       dplyr::filter(xco2!='Non Significant'),
                     ggplot2::aes(x=lon,y=lat,
                                  col=beta_error*30,
                                  fill=beta_error*30),
  )+
  ggplot2::scale_color_gradient(high='yellow',low='blue')+
  ggplot2::scale_fill_gradient(high='yellow',low='blue')+
  tema_mapa2()+
  ggplot2::labs(x='Longitude',y='Latitude',
                col=expression('β error (ppm '~month^-1~')'),
                fill=expression('β error (ppm '~month^-1~')')
  )
ggplot2::ggsave('img/xco2_beta_error_new_2016.png',units="in", width=8, height=6,
                dpi=300)

br |>
  ggplot2::ggplot()+
  ggplot2::geom_sf(fill="white", color="black",
                   size=.15, show.legend = FALSE)+
  ggplot2::geom_point(data=xco2_aux_detrend_new |>
                        dplyr::mutate(
                          xco2 = dplyr::case_when(
                            beta_line > q3_xco2 ~ 'Source',
                            beta_line < q1_xco2 ~'Sink',
                            .default = 'Non Significant'
                          )
                        ) |>
                        dplyr::filter(xco2!='Non Significant'),
                      ggplot2::aes(x=lon,y=lat,col=beta_error*30),
  )+
  ggplot2::scale_color_gradient(high='yellow',low='blue')+
  tema_mapa()+
  ggplot2::labs(x='Longitude',y='Latitude',col=expression('β error (ppm '~month^-1~')'))

ggplot2::ggsave('img/xco2_betaerror_2016.png',units="in", width=8, height=6,
                dpi=1000)

br |>
  ggplot2::ggplot()+
  ggplot2::geom_sf(fill="white", color="black",
                   size=.15, show.legend = FALSE)+
  ggplot2::geom_point(data=xco2_aux_detrend_new |>
                        dplyr::mutate(
                          xco2 = dplyr::case_when(
                            beta_line > q3_xco2 ~ 'Source',
                            beta_line < q1_xco2 ~'Sink',
                            .default = 'Non Significant'
                          )
                        ) |>
                        dplyr::filter(xco2!='Non Significant'),
                      ggplot2::aes(x=lon,y=lat,col=model_error),
  )+
  ggplot2::scale_color_gradient(high='yellow',low='blue')+
  tema_mapa()+
  ggplot2::labs(x='Longitude',y='Latitude',col=expression('Model error (ppm '~month^-1~')'))

ggplot2::ggsave('img/xco2_model_2016.png',units="in", width=8, height=6,
                dpi=1000)


#### CO2 emission and assimilation
south_america |>
  ggplot2::ggplot()+
  ggspatial::annotation_map_tile(type = 'cartolight')+
  ggplot2::geom_sf(col='grey',fill='white')+
  ggplot2::geom_sf(data=biomes,col='black',fill='NA',linetype='dashed')+
  ggplot2::ylim(-40,6)+
  ggplot2::xlim(-80,-35)+
  ggplot2::geom_tile(data=xco2_aux_detrend_new |>
                       dplyr::mutate(
                         xco2 = dplyr::case_when(
                           beta_line > q3_xco2 ~ 'Source',
                           beta_line < q1_xco2 ~'Sink',
                           .default = 'Non Significant'
                         )
                       ) |>
                       dplyr::filter(xco2!='Non Significant') |>
                       dplyr::mutate(
                         beta_molm=(beta_line*10000)*44/24.45,
                         fco2 = beta_molm*30/1000,
                       ),
                     ggplot2::aes(x=lon,y=lat,color=fco2,fill=fco2)
  )+
  ggplot2::scale_color_gradient(high='yellow',low='blue')+
  ggplot2::scale_fill_gradient(high='yellow',low='blue')+
  tema_mapa2()+
  ggplot2::labs(x='Longitude',y='Latitude',
                col=expression('FCO'[2]~'(g'~m^-2*month^-1~')'),
                fill=expression('FCO'[2]~'(g'~m^-2*month^-1~')')
  )

ggplot2::ggsave('img/xco2_emission_nwe_2016.png',units="in", width=8, height=6,
                dpi=300)

br |>
  ggplot2::ggplot()+
  ggplot2::geom_sf(fill="white", color="black",
                   size=.15, show.legend = FALSE)+
  ggplot2::geom_point(data=xco2_aux_detrend_new |>
                        dplyr::mutate(
                          xco2 = dplyr::case_when(
                            beta_line > q3_xco2 ~ 'Source',
                            beta_line < q1_xco2 ~'Sink',
                            .default = 'Non Significant'
                          )
                        ) |>
                        dplyr::filter(xco2!='Non Significant') |>
                        dplyr::mutate(
                          beta_molm=(beta_line*10000)*44/24.45,
                          fco2 = beta_molm*30/1000,
                        ),
                      ggplot2::aes(x=lon,y=lat,color=fco2),
  )+
  ggplot2::scale_color_gradient(high='yellow',low='blue')+
  tema_mapa()+
  ggplot2::labs(x='Longitude',y='Latitude',col=expression('FCO'[2]~'(g'~m^-2*month^-1~')'))

ggplot2::ggsave('img/xco2_emission_2016.png',units="in", width=8, height=6,
                dpi=1000)

south_america |>
  ggplot2::ggplot()+
  ggspatial::annotation_map_tile(type = 'cartolight')+
  ggplot2::geom_sf(col='grey',fill='white')+
  ggplot2::geom_sf(data=biomes,col='black',fill='NA',linetype='dashed')+
  ggplot2::ylim(-40,6)+
  ggplot2::xlim(-80,-35)+
  ggplot2::geom_tile(data=xco2_aux_detrend_new |>
                       dplyr::mutate(
                         xco2 = dplyr::case_when(
                           beta_line > q3_xco2 ~ 'Source',
                           beta_line < q1_xco2 ~'Sink',
                           .default = 'Non Significant'
                         )
                       ) |>
                       dplyr::filter(xco2!='Non Significant')|>
                       dplyr::mutate(
                         beta_molm=(beta_error*10000)*44/24.45,
                         fco2_error = beta_molm*30/1000),
                     ggplot2::aes(x=lon,y=lat,color=fco2_error,fill=fco2_error)
  )+
  ggplot2::scale_color_gradient(high='yellow',low='blue')+
  ggplot2::scale_fill_gradient(high='yellow',low='blue')+
  tema_mapa2()+
  ggplot2::labs(x='Longitude',y='Latitude',
                col=expression('FCO'[2]~'error (g'~m^-2*month^-1~')'),
                fill=expression('FCO'[2]~'error (g'~m^-2*month^-1~')')
  )

ggplot2::ggsave('img/xco2_emissionerror_new_2016.png',units="in", width=8, height=6,
                dpi=300)

br |>
  ggplot2::ggplot()+
  ggplot2::geom_sf(fill="white", color="black",
                   size=.15, show.legend = FALSE)+
  ggplot2::geom_point(data=xco2_aux_detrend_new |>
                        dplyr::mutate(
                          xco2 = dplyr::case_when(
                            beta_line > q3_xco2 ~ 'Source',
                            beta_line < q1_xco2 ~'Sink',
                            .default = 'Non Significant'
                          )
                        ) |>
                        dplyr::filter(xco2!='Non Significant')|>
                        dplyr::mutate(
                          beta_molm=(beta_error*10000)*44/24.45,
                          fco2_error = beta_molm*30/1000,
                        ),
                      ggplot2::aes(x=lon,y=lat,color=fco2_error),
  )+
  ggplot2::scale_color_gradient(high='yellow',low='blue')+
  tema_mapa()+
  ggplot2::labs(x='Longitude',y='Latitude',col=expression('FCO'[2]~' error (g'~m^-2*month^-1~')'))

ggplot2::ggsave('img/xco2_emissionerror_2016.png',units="in", width=8, height=6,
                dpi=1000)

br |>
  ggplot2::ggplot()+
  ggplot2::geom_sf(fill="white", color="black",
                   size=.15, show.legend = FALSE)+
  ggplot2::geom_point(data=xco2_aux_detrend_new |>
                        dplyr::mutate(
                          xco2 = dplyr::case_when(
                            beta_line > q3_xco2 ~ 'Source',
                            beta_line < q1_xco2 ~'Sink',
                            .default = 'Non Significant'
                          )
                        ) |>
                        dplyr::filter(xco2!='Non Significant') |>
                        dplyr::mutate(
                          beta_molm=(10000*model_error)*44/24.45,
                          fco2_error = beta_molm*12.01/1000,
                        ),
                      ggplot2::aes(x=lon,y=lat,color=fco2_error),
  )+
  ggplot2::scale_color_gradient(high='yellow',low='blue')+
  tema_mapa()


####Balance

xco2_aux_detrend_new |>
  dplyr::mutate(
    xco2 = dplyr::case_when(
      beta_line > q3_xco2 ~ 'Source',
      beta_line < q1_xco2 ~'Sink',
      .default = 'Non Significant'
    )
  ) |>
  dplyr::filter(xco2!='Non Significant')|>
  dplyr::mutate(
    beta_molm=(10000*beta_line)*44/24.45,
    fco2 = beta_molm*30/1000,
    fco2=(fco2*3025000000)/1e+15
  ) |>
  dplyr::ungroup() |>
  dplyr::summarise(
    fco2=sum(fco2)
  )


xco2_aux_detrend_new |>
  dplyr::mutate(
    xco2 = dplyr::case_when(
      beta_line > q3_xco2 ~ 'Source',
      beta_line < q1_xco2 ~'Sink',
      .default = 'Non Significant'
    )
  ) |>
  dplyr::filter(xco2!='Non Significant') |>
  dplyr::mutate(
    beta_molm=beta_line*44/24.45,
    fco2 = beta_molm*30/1000)|>
  dplyr::ungroup() |>
  dplyr::summarise(
    fco2m=mean(fco2),
    fco2sd=sd(fco2)
  )

df2016 <- xco2_aux_detrend_new |> dplyr::mutate(
  xco2 = dplyr::case_when(
    beta_line > q3_xco2 ~ 'Source',
    beta_line < q1_xco2 ~'Sink',
    .default = 'Non Significant'
  )
) |>
  dplyr::filter(xco2!='Non Significant')

df2016all <- xco2_aux_detrend_new |> dplyr::mutate(
  xco2 = dplyr::case_when(
    beta_line > q3_xco2 ~ 'Source',
    beta_line < q1_xco2 ~'Sink',
    .default = 'Non Significant'
  )
)
#####
### 2017

xco2df_filter <- xco2df |>
  dplyr::filter(year == 2017) |>
  dplyr::filter(dist_xco2<0.25) |>
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


xco2df_filter |>
  dplyr::group_by(date) |>
  dplyr::summarise(xco2_mean=mean(xco2_mean)) |>
  ggplot2::ggplot(ggplot2::aes(x=date,y=xco2_mean )) +
  ggplot2::geom_point(shape=21,color="black",fill="gray") +
  ggplot2::geom_line(color="red") +
  ggplot2::geom_smooth(method = "lm") +
  ggpubr::stat_regline_equation(ggplot2::aes(
    label =  paste(..eq.label.., ..rr.label.., sep = "*plain(\",\")~~"))) +
  ggplot2::xlab('Date')+
  ggplot2::ylab(expression(
    'Xco'[2]~' (ppm)'
  ))+
  ggplot2::theme_bw()

ggplot2::ggsave('img/xco2_temporal_2017.png',units="in", width=8, height=6,
                dpi=300)



mod <- lm(xco2_mean~x,data =xco2df_filter |>
            dplyr::mutate(
              x = 1:dplyr::n()
            ))


xco2df_filter |>
  dplyr::mutate(
    x=1:dplyr::n(),
    xco2_est = mod$coefficients[1] + mod$coefficients[2]*x,
    delta=xco2_est - xco2_mean,
    xco2r = (mod$coefficients[1]-delta)-(mean(xco2_mean)-mod$coefficients[1])
  ) |>
  dplyr::group_by(date) |>
  dplyr::summarise(xco2_mean=mean(xco2r)) |>
  ggplot2::ggplot(ggplot2::aes(x=date,y=xco2_mean )) +
  ggplot2::geom_point(shape=21,color="black",fill="gray") +
  ggplot2::geom_line(color="red") +
  ggplot2::geom_smooth(method = "lm") +
  ggpubr::stat_regline_equation(ggplot2::aes(
    label =  paste(..eq.label.., ..rr.label.., sep = "*plain(\",\")~~"))) +
  ggplot2::xlab('Date')+
  ggplot2::ylab(expression(
    'Xco'[2][R]~' (ppm)'
  ))+
  ggplot2::theme_bw()
ggplot2::ggsave('img/xco2_detrendtemporal_2017.png',units="in", width=8, height=6,
                dpi=300)

xco2detrend <- xco2df_filter |>
  dplyr::mutate(
    x=1:dplyr::n(),
    xco2_est = mod$coefficients[1] + mod$coefficients[2]*x,
    delta=xco2_est - xco2_mean,
    xco2r = (mod$coefficients[1]-delta)-(mean(xco2_mean)-mod$coefficients[1])
  )
xco2_aux_detrend <- xco2detrend |>
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
((10000*beta_r)*44/24.45)/1000

(0.7*(8510000*1e6))/1e15

# inferior/superior limit
ilbr <- beta_r-ep
slbr <- beta_r+ep


## model diagnostic
broom::augment(mod_detrend, interval="confidence")
plot(mod_detrend)
cooks.distance(mod_detrend)
shapiro.test(mod_detrend$residuals)
hist(xco2detrend$xco2r)

xco2_nest <- xco2detrend |>
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

### calculating for each pixel
xco2_nest_detrend <- xco2_nest|>
  dplyr::mutate(
    beta_line = purrr::map(data,linear_reg, output="beta1"),
    p_value = purrr::map(data,linear_reg, output="p_value"),
    partial = purrr::map(data,linear_reg, output="partial"),
    n_obs = purrr::map(data,linear_reg, output="n"),
    beta_error=purrr::map(data,linear_reg,output='betaerror'),
    model_error=purrr::map(data,linear_reg,output='modelerror')
  )



xco2_nest_detrend |>dplyr::filter(n_obs > 7) |>
  tidyr::unnest(cols = c(beta_error)) |>
  dplyr::select(lon, lat, beta_error) |>
  ggplot2::ggplot(ggplot2::aes(x=lon, y=lat, color = beta_error)) +
  ggplot2::geom_point()

xco2_nest_detrend |>
  dplyr::filter(n_obs > 7) |>
  tidyr::unnest(cols = c(beta_error,model_error)) |>
  dplyr::ungroup() |>
  dplyr::summarise(b=mean(beta_error),
                   m=mean(model_error))


xco2_nest_detrend |>dplyr::filter(n_obs > 5) |>
  tidyr::unnest(cols = c(beta_error)) |>
  dplyr::select(lon, lat, beta_error) |>
  ggplot2::ggplot(ggplot2::aes(x=lon, y=lat, color = beta_error)) +
  ggplot2::geom_point()

xco2_nest_detrend |>
  dplyr::filter(n_obs > 5) |>
  tidyr::unnest(cols = c(beta_error,model_error)) |>
  dplyr::ungroup() |>
  dplyr::summarise(b=mean(beta_error),
                   m=mean(model_error))

xco2_nest_detrend |>dplyr::filter(n_obs > 4) |>
  tidyr::unnest(cols = c(beta_error)) |>
  dplyr::select(lon, lat, beta_error) |>
  ggplot2::ggplot(ggplot2::aes(x=lon, y=lat, color = beta_error)) +
  ggplot2::geom_point()

xco2_nest_detrend |>
  dplyr::filter(n_obs > 4) |>
  tidyr::unnest(cols = c(beta_error,model_error)) |>
  dplyr::ungroup() |>
  dplyr::summarise(b=mean(beta_error),
                   m=mean(model_error))

xco2_nest_detrend |>dplyr::filter(n_obs > 3) |>
  tidyr::unnest(cols = c(beta_error)) |>
  dplyr::select(lon, lat, beta_error) |>
  ggplot2::ggplot(ggplot2::aes(x=lon, y=lat, color = beta_error)) +
  ggplot2::geom_point()

xco2_nest_detrend |>
  dplyr::filter(n_obs > 3) |>
  tidyr::unnest(cols = c(beta_error,model_error)) |>
  dplyr::ungroup() |>
  dplyr::summarise(b=mean(beta_error),
                   m=mean(model_error))

### creating a table

xco2_aux_detrend_new <- xco2_nest_detrend |>
  dplyr::filter(n_obs > 4) |>
  tidyr::unnest(cols = c(beta_line, partial,beta_error,model_error)) |>
  dplyr::ungroup() |>
  dplyr::select(lon, lat, beta_line, partial,beta_error,model_error)

q3_xco2 <- xco2_aux_detrend_new |> dplyr::pull(beta_line) |> quantile(.75)
q1_xco2 <- xco2_aux_detrend_new |> dplyr::pull(beta_line) |> quantile(.25)

xco2_aux_detrend_new <- xco2_aux_detrend_new  |>
  dplyr::mutate(
    anomaly = partial - xco2_aux_detrend_new  |>
      dplyr::pull(partial) |>
      median(),
    Dbeta = beta_line -xco2_aux_detrend_new  |>
      dplyr::pull(beta_line) |> mean(na.rm=TRUE)
  )

q3_anom <- xco2_aux_detrend_new  |> dplyr::pull(anomaly) |> quantile(.75)
q1_anom <- xco2_aux_detrend_new  |> dplyr::pull(anomaly) |> quantile(.25)

xco2_aux_detrend_new  <- xco2_aux_detrend_new  |>
  dplyr::mutate(
    beta_index =  ifelse(beta_line >=q1_xco2, 0, 1)
  )
xco2_aux_detrend_new |>
  ggplot2::ggplot(ggplot2::aes(x=beta_line)) +
  ggplot2::geom_histogram(bins=30,
                          fill="orange",
                          color="black") +
  ggplot2::labs(x="βpixel",y="Count") +
  ggplot2::geom_vline(xintercept = q1_xco2,
                      color = "red",
                      lty=2) +
  ggplot2::geom_vline(xintercept = q3_xco2,
                      color = "red",
                      lty=2)+
  gghighlight::gghighlight(beta_line < q1_xco2 | beta_line>q3_xco2,
                           unhighlighted_params = list(
                             color = "darkgray",
                             fill = "lightgray")) +
  ggplot2::theme_minimal()


xco2_aux_detrend_new |>
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


### Source and Sink Indentification

south_america |>
  ggplot2::ggplot()+
  ggspatial::annotation_map_tile(type = 'cartolight')+
  ggplot2::geom_sf(col='grey',fill='white')+
  ggplot2::geom_sf(data=biomes,col='black',fill='NA',linetype='dashed')+
  ggplot2::ylim(-40,6)+
  ggplot2::xlim(-80,-35)+
  ggplot2::geom_tile(data=xco2_aux_detrend_new |>
                       dplyr::mutate(
                         xco2 = dplyr::case_when(
                           beta_line > q3_xco2 ~ 'Source',
                           beta_line < q1_xco2 ~'Sink',
                           .default = 'Non Significant'
                         )
                       ) |>
                       dplyr::filter(xco2!='Non Significant'),
                     ggplot2::aes(x=lon,y=lat,color=xco2,fill=xco2),
  )+
  tema_mapa2()+
  ggplot2::scale_color_manual(values = c('darkgreen','darkred'))+
  ggplot2::scale_fill_manual(values = c('darkgreen','darkred'))+
  ggplot2::labs(x='Longitude',y='Latitude',
                col=expression('Xco'[2]),fill=expression('Xco'[2]))

ggplot2::ggsave('img/xco2_identification_new_2017.png',units="in", width=8, height=6,
                dpi=300)

br |>
  ggplot2::ggplot()+
  ggplot2::geom_sf(fill="white", color="black",
                   size=.15, show.legend = FALSE)+
  ggplot2::geom_point(data=xco2_aux_detrend_new |>
                        dplyr::mutate(
                          xco2 = dplyr::case_when(
                            beta_line > q3_xco2 ~ 'Source',
                            beta_line < q1_xco2 ~'Sink',
                            .default = 'Non Significant'
                          )
                        ) |>
                        dplyr::filter(xco2!='Non Significant'),
                      ggplot2::aes(x=lon,y=lat,color=xco2),
  )+
  tema_mapa()+
  ggplot2::scale_color_manual(values = c('darkblue','darkred'))+
  ggplot2::labs(x='Longitude',y='Latitude',col=expression('Xco'[2]))

# ggplot2::ggsave('img/xco2_identification_2017.png',units="in", width=8, height=6,
#                 dpi=1000)

xco2_aux_detrend_new |>
  dplyr::filter(beta_line<0) |>
  dplyr::summarise(
    nobs=dplyr::n()
  )


### Beta
south_america |>
  ggplot2::ggplot()+
  ggspatial::annotation_map_tile(type = 'cartolight')+
  ggplot2::geom_sf(col='grey',fill='white')+
  ggplot2::geom_sf(data=biomes,col='black',fill='NA',linetype='dashed')+
  ggplot2::ylim(-40,6)+
  ggplot2::xlim(-80,-35)+
  ggplot2::geom_tile(data=xco2_aux_detrend_new |>
                       dplyr::mutate(
                         xco2 = dplyr::case_when(
                           beta_line > q3_xco2 ~ 'Source',
                           beta_line < q1_xco2 ~'Sink',
                           .default = 'Non Significant'
                         )
                       ) |>
                       dplyr::filter(xco2!='Non Significant'),
                     ggplot2::aes(x=lon,y=lat,
                                  col=beta_line*30,
                                  fill=beta_line*30),
  )+
  ggplot2::scale_color_gradient(high='yellow',low='blue')+
  ggplot2::scale_fill_gradient(high='yellow',low='blue')+
  tema_mapa2()+
  ggplot2::labs(x='Longitude',y='Latitude',
                col=expression('β (ppm '~month^-1~')'),
                fill=expression('β (ppm '~month^-1~')')
  )
ggplot2::ggsave('img/xco2_beta_new_2017.png',units="in", width=8, height=6,
                dpi=300)


# br |>
#   ggplot2::ggplot()+
#   ggplot2::geom_sf(fill="white", color="black",
#                    size=.15, show.legend = FALSE)+
#   ggplot2::geom_point(data=xco2_aux_detrend_new |>
#                         dplyr::mutate(
#                           xco2 = dplyr::case_when(
#                             beta_line > q3_xco2 ~ 'Source',
#                             beta_line < q1_xco2 ~'Sink',
#                             .default = 'Non Significant'
#                           )
#                         ) |>
#                         dplyr::filter(xco2!='Non Significant'),
#                       ggplot2::aes(x=lon,y=lat,col=beta_line*30),
#   )+
#   ggplot2::scale_color_gradient(high='yellow',low='blue')+
#   tema_mapa()+
#   ggplot2::labs(x='Longitude',y='Latitude',col=expression('β (ppm '~month^-1~')'))
#
# ggplot2::ggsave('img/xco2_beta_2017.png',units="in", width=8, height=6,
#                 dpi=1000)


### uncertanty
south_america |>
  ggplot2::ggplot()+
  ggspatial::annotation_map_tile(type = 'cartolight')+
  ggplot2::geom_sf(col='grey',fill='white')+
  ggplot2::geom_sf(data=biomes,col='black',fill='NA',linetype='dashed')+
  ggplot2::ylim(-40,6)+
  ggplot2::xlim(-80,-35)+
  ggplot2::geom_tile(data=xco2_aux_detrend_new |>
                       dplyr::mutate(
                         xco2 = dplyr::case_when(
                           beta_line > q3_xco2 ~ 'Source',
                           beta_line < q1_xco2 ~'Sink',
                           .default = 'Non Significant'
                         )
                       ) |>
                       dplyr::filter(xco2!='Non Significant'),
                     ggplot2::aes(x=lon,y=lat,
                                  col=beta_error*30,
                                  fill=beta_error*30),
  )+
  ggplot2::scale_color_gradient(high='yellow',low='blue')+
  ggplot2::scale_fill_gradient(high='yellow',low='blue')+
  tema_mapa2()+
  ggplot2::labs(x='Longitude',y='Latitude',
                col=expression('β error (ppm '~month^-1~')'),
                fill=expression('β error (ppm '~month^-1~')')
  )
ggplot2::ggsave('img/xco2_beta_error_new_2017.png',units="in", width=8, height=6,
                dpi=300)

# br |>
#   ggplot2::ggplot()+
#   ggplot2::geom_sf(fill="white", color="black",
#                    size=.15, show.legend = FALSE)+
#   ggplot2::geom_point(data=xco2_aux_detrend_new |>
#                         dplyr::mutate(
#                           xco2 = dplyr::case_when(
#                             beta_line > q3_xco2 ~ 'Source',
#                             beta_line < q1_xco2 ~'Sink',
#                             .default = 'Non Significant'
#                           )
#                         ) |>
#                         dplyr::filter(xco2!='Non Significant'),
#                       ggplot2::aes(x=lon,y=lat,col=beta_error*30),
#   )+
#   ggplot2::scale_color_gradient(high='yellow',low='blue')+
#   tema_mapa()+
#   ggplot2::labs(x='Longitude',y='Latitude',col=expression('β error (ppm '~month^-1~')'))
#
# ggplot2::ggsave('img/xco2_betaerror_2017.png',units="in", width=8, height=6,
#                 dpi=1000)

# br |>
#   ggplot2::ggplot()+
#   ggplot2::geom_sf(fill="white", color="black",
#                    size=.15, show.legend = FALSE)+
#   ggplot2::geom_point(data=xco2_aux_detrend_new |>
#                         dplyr::mutate(
#                           xco2 = dplyr::case_when(
#                             beta_line > q3_xco2 ~ 'Source',
#                             beta_line < q1_xco2 ~'Sink',
#                             .default = 'Non Significant'
#                           )
#                         ) |>
#                         dplyr::filter(xco2!='Non Significant'),
#                       ggplot2::aes(x=lon,y=lat,col=model_error),
#   )+
#   ggplot2::scale_color_gradient(high='yellow',low='blue')+
#   tema_mapa()+
#   ggplot2::labs(x='Longitude',y='Latitude',col=expression('Model error (ppm '~month^-1~')'))
#
# ggplot2::ggsave('img/xco2_model_2017.png',units="in", width=8, height=6,
#                 dpi=1000)


#### CO2 emission and assimilation
south_america |>
  ggplot2::ggplot()+
  ggspatial::annotation_map_tile(type = 'cartolight')+
  ggplot2::geom_sf(col='grey',fill='white')+
  ggplot2::geom_sf(data=biomes,col='black',fill='NA',linetype='dashed')+
  ggplot2::ylim(-40,6)+
  ggplot2::xlim(-80,-35)+
  ggplot2::geom_tile(data=xco2_aux_detrend_new |>
                       dplyr::mutate(
                         xco2 = dplyr::case_when(
                           beta_line > q3_xco2 ~ 'Source',
                           beta_line < q1_xco2 ~'Sink',
                           .default = 'Non Significant'
                         )
                       ) |>
                       dplyr::filter(xco2!='Non Significant') |>
                       dplyr::mutate(
                         beta_molm=(beta_line*10000)*44/24.45,
                         fco2 = beta_molm*30/1000,
                       ),
                     ggplot2::aes(x=lon,y=lat,color=fco2,fill=fco2)
  )+
  ggplot2::scale_color_gradient(high='yellow',low='blue')+
  ggplot2::scale_fill_gradient(high='yellow',low='blue')+
  tema_mapa2()+
  ggplot2::labs(x='Longitude',y='Latitude',
                col=expression('FCO'[2]~'(g'~m^-2*month^-1~')'),
                fill=expression('FCO'[2]~'(g'~m^-2*month^-1~')')
  )

ggplot2::ggsave('img/xco2_emission_nwe_2017.png',units="in", width=8, height=6,
                dpi=300)

# br |>
#   ggplot2::ggplot()+
#   ggplot2::geom_sf(fill="white", color="black",
#                    size=.15, show.legend = FALSE)+
#   ggplot2::geom_point(data=xco2_aux_detrend_new |>
#                         dplyr::mutate(
#                           xco2 = dplyr::case_when(
#                             beta_line > q3_xco2 ~ 'Source',
#                             beta_line < q1_xco2 ~'Sink',
#                             .default = 'Non Significant'
#                           )
#                         ) |>
#                         dplyr::filter(xco2!='Non Significant') |>
#                         dplyr::mutate(
#                           beta_molm=(beta_line*10000)*44/24.45,
#                           fco2 = beta_molm*30/1000,
#                         ),
#                       ggplot2::aes(x=lon,y=lat,color=fco2),
#   )+
#   ggplot2::scale_color_gradient(high='yellow',low='blue')+
#   tema_mapa()+
#   ggplot2::labs(x='Longitude',y='Latitude',col=expression('FCO'[2]~'(g'~m^-2*month^-1~')'))
#
# ggplot2::ggsave('img/xco2_emission_2017.png',units="in", width=8, height=6,
#                 dpi=1000)

south_america |>
  ggplot2::ggplot()+
  ggspatial::annotation_map_tile(type = 'cartolight')+
  ggplot2::geom_sf(col='grey',fill='white')+
  ggplot2::geom_sf(data=biomes,col='black',fill='NA',linetype='dashed')+
  ggplot2::ylim(-40,6)+
  ggplot2::xlim(-80,-35)+
  ggplot2::geom_tile(data=xco2_aux_detrend_new |>
                       dplyr::mutate(
                         xco2 = dplyr::case_when(
                           beta_line > q3_xco2 ~ 'Source',
                           beta_line < q1_xco2 ~'Sink',
                           .default = 'Non Significant'
                         )
                       ) |>
                       dplyr::filter(xco2!='Non Significant')|>
                       dplyr::mutate(
                         beta_molm=(beta_error*10000)*44/24.45,
                         fco2_error = beta_molm*30/1000),
                     ggplot2::aes(x=lon,y=lat,color=fco2_error,fill=fco2_error)
  )+
  ggplot2::scale_color_gradient(high='yellow',low='blue')+
  ggplot2::scale_fill_gradient(high='yellow',low='blue')+
  tema_mapa2()+
  ggplot2::labs(x='Longitude',y='Latitude',
                col=expression('FCO'[2]~'error (g'~m^-2*month^-1~')'),
                fill=expression('FCO'[2]~'error (g'~m^-2*month^-1~')')
  )

ggplot2::ggsave('img/xco2_emissionerror_new_2017.png',units="in", width=8, height=6,
                dpi=300)

# br |>
#   ggplot2::ggplot()+
#   ggplot2::geom_sf(fill="white", color="black",
#                    size=.15, show.legend = FALSE)+
#   ggplot2::geom_point(data=xco2_aux_detrend_new |>
#                         dplyr::mutate(
#                           xco2 = dplyr::case_when(
#                             beta_line > q3_xco2 ~ 'Source',
#                             beta_line < q1_xco2 ~'Sink',
#                             .default = 'Non Significant'
#                           )
#                         ) |>
#                         dplyr::filter(xco2!='Non Significant')|>
#                         dplyr::mutate(
#                           beta_molm=(beta_error*10000)*44/24.45,
#                           fco2_error = beta_molm*30/1000,
#                         ),
#                       ggplot2::aes(x=lon,y=lat,color=fco2_error),
#   )+
#   ggplot2::scale_color_gradient(high='yellow',low='blue')+
#   tema_mapa()+
#   ggplot2::labs(x='Longitude',y='Latitude',col=expression('FCO'[2]~' error (g'~m^-2*month^-1~')'))
#
# ggplot2::ggsave('img/xco2_emissionerror_2017.png',units="in", width=8, height=6,
#                 dpi=1000)
#
# br |>
#   ggplot2::ggplot()+
#   ggplot2::geom_sf(fill="white", color="black",
#                    size=.15, show.legend = FALSE)+
#   ggplot2::geom_point(data=xco2_aux_detrend_new |>
#                         dplyr::mutate(
#                           xco2 = dplyr::case_when(
#                             beta_line > q3_xco2 ~ 'Source',
#                             beta_line < q1_xco2 ~'Sink',
#                             .default = 'Non Significant'
#                           )
#                         ) |>
#                         dplyr::filter(xco2!='Non Significant') |>
#                         dplyr::mutate(
#                           beta_molm=(10000*model_error)*44/24.45,
#                           fco2_error = beta_molm*12.01/1000,
#                         ),
#                       ggplot2::aes(x=lon,y=lat,color=fco2_error),
#   )+
#   ggplot2::scale_color_gradient(high='yellow',low='blue')+
#   tema_mapa()
#

####Balance

xco2_aux_detrend_new |>
  dplyr::mutate(
    xco2 = dplyr::case_when(
      beta_line > q3_xco2 ~ 'Source',
      beta_line < q1_xco2 ~'Sink',
      .default = 'Non Significant'
    )
  ) |>
  dplyr::filter(xco2!='Non Significant')|>
  dplyr::mutate(
    beta_molm=(10000*beta_line)*44/24.45,
    fco2 = beta_molm*30/1000,
    fco2=(fco2*3025000000)/1e+15
  ) |>
  dplyr::ungroup() |>
  dplyr::summarise(
    fco2=sum(fco2)
  )


xco2_aux_detrend_new |>
  dplyr::mutate(
    xco2 = dplyr::case_when(
      beta_line > q3_xco2 ~ 'Source',
      beta_line < q1_xco2 ~'Sink',
      .default = 'Non Significant'
    )
  ) |>
  dplyr::filter(xco2!='Non Significant') |>
  dplyr::mutate(
    beta_molm=beta_line*44/24.45,
    fco2 = beta_molm*30/1000)|>
  dplyr::ungroup() |>
  dplyr::summarise(
    fco2m=mean(fco2),
    fco2sd=sd(fco2)
  )

df2017 <- xco2_aux_detrend_new |> dplyr::mutate(
  xco2 = dplyr::case_when(
    beta_line > q3_xco2 ~ 'Source',
    beta_line < q1_xco2 ~'Sink',
    .default = 'Non Significant'
  )
) |>
  dplyr::filter(xco2!='Non Significant')

df2017all <- xco2_aux_detrend_new |> dplyr::mutate(
  xco2 = dplyr::case_when(
    beta_line > q3_xco2 ~ 'Source',
    beta_line < q1_xco2 ~'Sink',
    .default = 'Non Significant'
  )
)

#### 2018

xco2df_filter <- xco2df |>
  dplyr::filter(year == 2018) |>
  dplyr::filter(dist_xco2<0.25) |>
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


xco2df_filter |>
  dplyr::group_by(date) |>
  dplyr::summarise(xco2_mean=mean(xco2_mean)) |>
  ggplot2::ggplot(ggplot2::aes(x=date,y=xco2_mean )) +
  ggplot2::geom_point(shape=21,color="black",fill="gray") +
  ggplot2::geom_line(color="red") +
  ggplot2::geom_smooth(method = "lm") +
  ggpubr::stat_regline_equation(ggplot2::aes(
    label =  paste(..eq.label.., ..rr.label.., sep = "*plain(\",\")~~"))) +
  ggplot2::xlab('Date')+
  ggplot2::ylab(expression(
    'Xco'[2]~' (ppm)'
  ))+
  ggplot2::theme_bw()

ggplot2::ggsave('img/xco2_temporal_2018.png',units="in", width=8, height=6,
                dpi=300)

mod <- lm(xco2_mean~x,data =xco2df_filter |>
            dplyr::mutate(
              x = 1:dplyr::n()
            ))


xco2df_filter |>
  dplyr::mutate(
    x=1:dplyr::n(),
    xco2_est = mod$coefficients[1] + mod$coefficients[2]*x,
    delta=xco2_est - xco2_mean,
    xco2r = (mod$coefficients[1]-delta)-(mean(xco2_mean)-mod$coefficients[1])
  ) |>
  dplyr::group_by(date) |>
  dplyr::summarise(xco2_mean=mean(xco2r)) |>
  ggplot2::ggplot(ggplot2::aes(x=date,y=xco2_mean )) +
  ggplot2::geom_point(shape=21,color="black",fill="gray") +
  ggplot2::geom_line(color="red") +
  ggplot2::geom_smooth(method = "lm") +
  ggpubr::stat_regline_equation(ggplot2::aes(
    label =  paste(..eq.label.., ..rr.label.., sep = "*plain(\",\")~~"))) +
  ggplot2::xlab('Date')+
  ggplot2::ylab(expression(
    'Xco'[2][R]~' (ppm)'
  ))+
  ggplot2::theme_bw()
ggplot2::ggsave('img/xco2_detrendtemporal_2018.png',units="in", width=8, height=6,
                dpi=300)

xco2detrend <- xco2df_filter |>
  dplyr::mutate(
    x=1:dplyr::n(),
    xco2_est = mod$coefficients[1] + mod$coefficients[2]*x,
    delta=xco2_est - xco2_mean,
    xco2r = (mod$coefficients[1]-delta)-(mean(xco2_mean)-mod$coefficients[1])
  )
xco2_aux_detrend <- xco2detrend |>
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

((beta_r*10000)*44/24.45)/1000

# inferior/superior limit
ilbr <- beta_r-ep
slbr <- beta_r+ep


## model diagnostic
broom::augment(mod_detrend, interval="confidence")
plot(mod_detrend)
cooks.distance(mod_detrend)
shapiro.test(mod_detrend$residuals)
hist(xco2detrend$xco2r)

xco2_nest <- xco2detrend |>
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

### calculating for each pixel
xco2_nest_detrend <- xco2_nest|>
  dplyr::mutate(
    beta_line = purrr::map(data,linear_reg, output="beta1"),
    p_value = purrr::map(data,linear_reg, output="p_value"),
    partial = purrr::map(data,linear_reg, output="partial"),
    n_obs = purrr::map(data,linear_reg, output="n"),
    beta_error=purrr::map(data,linear_reg,output='betaerror'),
    model_error=purrr::map(data,linear_reg,output='modelerror')
  )



# xco2_nest_detrend |>dplyr::filter(n_obs > 7) |>
#   tidyr::unnest(cols = c(beta_error)) |>
#   dplyr::select(lon, lat, beta_error) |>
#   ggplot2::ggplot(ggplot2::aes(x=lon, y=lat, color = beta_error)) +
#   ggplot2::geom_point()
#
# xco2_nest_detrend |>
#   dplyr::filter(n_obs > 7) |>
#   tidyr::unnest(cols = c(beta_error,model_error)) |>
#   dplyr::ungroup() |>
#   dplyr::summarise(b=mean(beta_error),
#                    m=mean(model_error))
#
#
# xco2_nest_detrend |>dplyr::filter(n_obs > 5) |>
#   tidyr::unnest(cols = c(beta_error)) |>
#   dplyr::select(lon, lat, beta_error) |>
#   ggplot2::ggplot(ggplot2::aes(x=lon, y=lat, color = beta_error)) +
#   ggplot2::geom_point()
#
# xco2_nest_detrend |>
#   dplyr::filter(n_obs > 5) |>
#   tidyr::unnest(cols = c(beta_error,model_error)) |>
#   dplyr::ungroup() |>
#   dplyr::summarise(b=mean(beta_error),
#                    m=mean(model_error))
#
# xco2_nest_detrend |>dplyr::filter(n_obs > 4) |>
#   tidyr::unnest(cols = c(beta_error)) |>
#   dplyr::select(lon, lat, beta_error) |>
#   ggplot2::ggplot(ggplot2::aes(x=lon, y=lat, color = beta_error)) +
#   ggplot2::geom_point()
#
# xco2_nest_detrend |>
#   dplyr::filter(n_obs > 4) |>
#   tidyr::unnest(cols = c(beta_error,model_error)) |>
#   dplyr::ungroup() |>
#   dplyr::summarise(b=mean(beta_error),
#                    m=mean(model_error))
#
# xco2_nest_detrend |>dplyr::filter(n_obs > 3) |>
#   tidyr::unnest(cols = c(beta_error)) |>
#   dplyr::select(lon, lat, beta_error) |>
#   ggplot2::ggplot(ggplot2::aes(x=lon, y=lat, color = beta_error)) +
#   ggplot2::geom_point()
#
# xco2_nest_detrend |>
#   dplyr::filter(n_obs > 3) |>
#   tidyr::unnest(cols = c(beta_error,model_error)) |>
#   dplyr::ungroup() |>
#   dplyr::summarise(b=mean(beta_error),
#                    m=mean(model_error))

### creating a table

xco2_aux_detrend_new <- xco2_nest_detrend |>
  dplyr::filter(n_obs > 4) |>
  tidyr::unnest(cols = c(beta_line, partial,beta_error,model_error)) |>
  dplyr::ungroup() |>
  dplyr::select(lon, lat, beta_line, partial,beta_error,model_error)

q3_xco2 <- xco2_aux_detrend_new |> dplyr::pull(beta_line) |> quantile(.75)
q1_xco2 <- xco2_aux_detrend_new |> dplyr::pull(beta_line) |> quantile(.25)

xco2_aux_detrend_new <- xco2_aux_detrend_new  |>
  dplyr::mutate(
    anomaly = partial - xco2_aux_detrend_new  |>
      dplyr::pull(partial) |>
      median(),
    Dbeta = beta_line -xco2_aux_detrend_new  |>
      dplyr::pull(beta_line) |> mean(na.rm=TRUE)
  )

q3_anom <- xco2_aux_detrend_new  |> dplyr::pull(anomaly) |> quantile(.75)
q1_anom <- xco2_aux_detrend_new  |> dplyr::pull(anomaly) |> quantile(.25)

xco2_aux_detrend_new  <- xco2_aux_detrend_new  |>
  dplyr::mutate(
    beta_index =  ifelse(beta_line >=q1_xco2, 0, 1)
  )
xco2_aux_detrend_new |>
  ggplot2::ggplot(ggplot2::aes(x=beta_line)) +
  ggplot2::geom_histogram(bins=30,
                          fill="orange",
                          color="black") +
  ggplot2::labs(x="βpixel",y="Count") +
  ggplot2::geom_vline(xintercept = q1_xco2,
                      color = "red",
                      lty=2) +
  ggplot2::geom_vline(xintercept = q3_xco2,
                      color = "red",
                      lty=2)+
  gghighlight::gghighlight(beta_line < q1_xco2 | beta_line>q3_xco2,
                           unhighlighted_params = list(
                             color = "darkgray",
                             fill = "lightgray")) +
  ggplot2::theme_minimal()



### Source and Sink Indentification

south_america |>
  ggplot2::ggplot()+
  ggspatial::annotation_map_tile(type = 'cartolight')+
  ggplot2::geom_sf(col='grey',fill='white')+
  ggplot2::geom_sf(data=biomes,col='black',fill='NA',linetype='dashed')+
  ggplot2::ylim(-40,6)+
  ggplot2::xlim(-80,-35)+
  ggplot2::geom_tile(data=xco2_aux_detrend_new |>
                       dplyr::mutate(
                         xco2 = dplyr::case_when(
                           beta_line > q3_xco2 ~ 'Source',
                           beta_line < q1_xco2 ~'Sink',
                           .default = 'Non Significant'
                         )
                       ) |>
                       dplyr::filter(xco2!='Non Significant'),
                     ggplot2::aes(x=lon,y=lat,color=xco2,fill=xco2),
  )+
  tema_mapa2()+
  ggplot2::scale_color_manual(values = c('darkgreen','darkred'))+
  ggplot2::scale_fill_manual(values = c('darkgreen','darkred'))+
  ggplot2::labs(x='Longitude',y='Latitude',
                col=expression('Xco'[2]),fill=expression('Xco'[2]))

ggplot2::ggsave('img/xco2_identification_new_2018.png',units="in", width=8, height=6,
                dpi=300)

# br |>
#   ggplot2::ggplot()+
#   ggplot2::geom_sf(fill="white", color="black",
#                    size=.15, show.legend = FALSE)+
#   ggplot2::geom_point(data=xco2_aux_detrend_new |>
#                         dplyr::mutate(
#                           xco2 = dplyr::case_when(
#                             beta_line > q3_xco2 ~ 'Source',
#                             beta_line < q1_xco2 ~'Sink',
#                             .default = 'Non Significant'
#                           )
#                         ) |>
#                         dplyr::filter(xco2!='Non Significant'),
#                       ggplot2::aes(x=lon,y=lat,color=xco2),
#   )+
#   tema_mapa()+
#   ggplot2::scale_color_manual(values = c('darkblue','darkred'))+
#   ggplot2::labs(x='Longitude',y='Latitude',col=expression('Xco'[2]))
#
# ggplot2::ggsave('img/xco2_identification_2018.png',units="in", width=8, height=6,
#                 dpi=1000)

xco2_aux_detrend_new |>
  dplyr::filter(beta_line<0) |>
  dplyr::summarise(
    nobs=dplyr::n()
  )


### Beta
south_america |>
  ggplot2::ggplot()+
  ggspatial::annotation_map_tile(type = 'cartolight')+
  ggplot2::geom_sf(col='grey',fill='white')+
  ggplot2::geom_sf(data=biomes,col='black',fill='NA',linetype='dashed')+
  ggplot2::ylim(-40,6)+
  ggplot2::xlim(-80,-35)+
  ggplot2::geom_tile(data=xco2_aux_detrend_new |>
                       dplyr::mutate(
                         xco2 = dplyr::case_when(
                           beta_line > q3_xco2 ~ 'Source',
                           beta_line < q1_xco2 ~'Sink',
                           .default = 'Non Significant'
                         )
                       ) |>
                       dplyr::filter(xco2!='Non Significant'),
                     ggplot2::aes(x=lon,y=lat,
                                  col=beta_line*30,
                                  fill=beta_line*30),
  )+
  ggplot2::scale_color_gradient(high='yellow',low='blue')+
  ggplot2::scale_fill_gradient(high='yellow',low='blue')+
  tema_mapa2()+
  ggplot2::labs(x='Longitude',y='Latitude',
                col=expression('β (ppm '~month^-1~')'),
                fill=expression('β (ppm '~month^-1~')')
  )
ggplot2::ggsave('img/xco2_beta_new_2018.png',units="in", width=8, height=6,
                dpi=300)


# br |>
#   ggplot2::ggplot()+
#   ggplot2::geom_sf(fill="white", color="black",
#                    size=.15, show.legend = FALSE)+
#   ggplot2::geom_point(data=xco2_aux_detrend_new |>
#                         dplyr::mutate(
#                           xco2 = dplyr::case_when(
#                             beta_line > q3_xco2 ~ 'Source',
#                             beta_line < q1_xco2 ~'Sink',
#                             .default = 'Non Significant'
#                           )
#                         ) |>
#                         dplyr::filter(xco2!='Non Significant'),
#                       ggplot2::aes(x=lon,y=lat,col=beta_line*30),
#   )+
#   ggplot2::scale_color_gradient(high='yellow',low='blue')+
#   tema_mapa()+
#   ggplot2::labs(x='Longitude',y='Latitude',col=expression('β (ppm '~month^-1~')'))
#
# ggplot2::ggsave('img/xco2_beta_2018.png',units="in", width=8, height=6,
#                 dpi=1000)


### uncertanty
south_america |>
  ggplot2::ggplot()+
  ggspatial::annotation_map_tile(type = 'cartolight')+
  ggplot2::geom_sf(col='grey',fill='white')+
  ggplot2::geom_sf(data=biomes,col='black',fill='NA',linetype='dashed')+
  ggplot2::ylim(-40,6)+
  ggplot2::xlim(-80,-35)+
  ggplot2::geom_tile(data=xco2_aux_detrend_new |>
                       dplyr::mutate(
                         xco2 = dplyr::case_when(
                           beta_line > q3_xco2 ~ 'Source',
                           beta_line < q1_xco2 ~'Sink',
                           .default = 'Non Significant'
                         )
                       ) |>
                       dplyr::filter(xco2!='Non Significant'),
                     ggplot2::aes(x=lon,y=lat,
                                  col=beta_error*30,
                                  fill=beta_error*30),
  )+
  ggplot2::scale_color_gradient(high='yellow',low='blue')+
  ggplot2::scale_fill_gradient(high='yellow',low='blue')+
  tema_mapa2()+
  ggplot2::labs(x='Longitude',y='Latitude',
                col=expression('β error (ppm '~month^-1~')'),
                fill=expression('β error (ppm '~month^-1~')')
  )
ggplot2::ggsave('img/xco2_beta_error_new_2018.png',units="in", width=8, height=6,
                dpi=300)

# br |>
#   ggplot2::ggplot()+
#   ggplot2::geom_sf(fill="white", color="black",
#                    size=.15, show.legend = FALSE)+
#   ggplot2::geom_point(data=xco2_aux_detrend_new |>
#                         dplyr::mutate(
#                           xco2 = dplyr::case_when(
#                             beta_line > q3_xco2 ~ 'Source',
#                             beta_line < q1_xco2 ~'Sink',
#                             .default = 'Non Significant'
#                           )
#                         ) |>
#                         dplyr::filter(xco2!='Non Significant'),
#                       ggplot2::aes(x=lon,y=lat,col=beta_error*30),
#   )+
#   ggplot2::scale_color_gradient(high='yellow',low='blue')+
#   tema_mapa()+
#   ggplot2::labs(x='Longitude',y='Latitude',col=expression('β error (ppm '~month^-1~')'))
#
# ggplot2::ggsave('img/xco2_betaerror_2018.png',units="in", width=8, height=6,
#                 dpi=1000)
#
# br |>
#   ggplot2::ggplot()+
#   ggplot2::geom_sf(fill="white", color="black",
#                    size=.15, show.legend = FALSE)+
#   ggplot2::geom_point(data=xco2_aux_detrend_new |>
#                         dplyr::mutate(
#                           xco2 = dplyr::case_when(
#                             beta_line > q3_xco2 ~ 'Source',
#                             beta_line < q1_xco2 ~'Sink',
#                             .default = 'Non Significant'
#                           )
#                         ) |>
#                         dplyr::filter(xco2!='Non Significant'),
#                       ggplot2::aes(x=lon,y=lat,col=model_error),
#   )+
#   ggplot2::scale_color_gradient(high='yellow',low='blue')+
#   tema_mapa()+
#   ggplot2::labs(x='Longitude',y='Latitude',col=expression('Model error (ppm '~month^-1~')'))
#
# ggplot2::ggsave('img/xco2_model_2015.png',units="in", width=8, height=6,
#                 dpi=1000)


#### CO2 emission and assimilation
south_america |>
  ggplot2::ggplot()+
  ggspatial::annotation_map_tile(type = 'cartolight')+
  ggplot2::geom_sf(col='grey',fill='white')+
  ggplot2::geom_sf(data=biomes,col='black',fill='NA',linetype='dashed')+
  ggplot2::ylim(-40,6)+
  ggplot2::xlim(-80,-35)+
  ggplot2::geom_tile(data=xco2_aux_detrend_new |>
                       dplyr::mutate(
                         xco2 = dplyr::case_when(
                           beta_line > q3_xco2 ~ 'Source',
                           beta_line < q1_xco2 ~'Sink',
                           .default = 'Non Significant'
                         )
                       ) |>
                       dplyr::filter(xco2!='Non Significant') |>
                       dplyr::mutate(
                         beta_molm=(beta_line*10000)*44/24.45,
                         fco2 = beta_molm*30/1000,
                       ),
                     ggplot2::aes(x=lon,y=lat,color=fco2,fill=fco2)
  )+
  ggplot2::scale_color_gradient(high='yellow',low='blue')+
  ggplot2::scale_fill_gradient(high='yellow',low='blue')+
  tema_mapa2()+
  ggplot2::labs(x='Longitude',y='Latitude',
                col=expression('FCO'[2]~'(g'~m^-2*month^-1~')'),
                fill=expression('FCO'[2]~'(g'~m^-2*month^-1~')')
  )

ggplot2::ggsave('img/xco2_emission_nwe_2018.png',units="in", width=8, height=6,
                dpi=300)

# br |>
#   ggplot2::ggplot()+
#   ggplot2::geom_sf(fill="white", color="black",
#                    size=.15, show.legend = FALSE)+
#   ggplot2::geom_point(data=xco2_aux_detrend_new |>
#                         dplyr::mutate(
#                           xco2 = dplyr::case_when(
#                             beta_line > q3_xco2 ~ 'Source',
#                             beta_line < q1_xco2 ~'Sink',
#                             .default = 'Non Significant'
#                           )
#                         ) |>
#                         dplyr::filter(xco2!='Non Significant') |>
#                         dplyr::mutate(
#                           beta_molm=(beta_line*10000)*44/24.45,
#                           fco2 = beta_molm*30/1000,
#                         ),
#                       ggplot2::aes(x=lon,y=lat,color=fco2),
#   )+
#   ggplot2::scale_color_gradient(high='yellow',low='blue')+
#   tema_mapa()+
#   ggplot2::labs(x='Longitude',y='Latitude',col=expression('FCO'[2]~'(g'~m^-2*month^-1~')'))
#
# ggplot2::ggsave('img/xco2_emission_2018.png',units="in", width=8, height=6,
#                 dpi=1000)

south_america |>
  ggplot2::ggplot()+
  ggspatial::annotation_map_tile(type = 'cartolight')+
  ggplot2::geom_sf(col='grey',fill='white')+
  ggplot2::geom_sf(data=biomes,col='black',fill='NA',linetype='dashed')+
  ggplot2::ylim(-40,6)+
  ggplot2::xlim(-80,-35)+
  ggplot2::geom_tile(data=xco2_aux_detrend_new |>
                       dplyr::mutate(
                         xco2 = dplyr::case_when(
                           beta_line > q3_xco2 ~ 'Source',
                           beta_line < q1_xco2 ~'Sink',
                           .default = 'Non Significant'
                         )
                       ) |>
                       dplyr::filter(xco2!='Non Significant')|>
                       dplyr::mutate(
                         beta_molm=(beta_error*10000)*44/24.45,
                         fco2_error = beta_molm*30/1000),
                     ggplot2::aes(x=lon,y=lat,color=fco2_error,fill=fco2_error)
  )+
  ggplot2::scale_color_gradient(high='yellow',low='blue')+
  ggplot2::scale_fill_gradient(high='yellow',low='blue')+
  tema_mapa2()+
  ggplot2::labs(x='Longitude',y='Latitude',
                col=expression('FCO'[2]~'error (g'~m^-2*month^-1~')'),
                fill=expression('FCO'[2]~'error (g'~m^-2*month^-1~')')
  )

ggplot2::ggsave('img/xco2_emissionerror_new_2018.png',units="in", width=8, height=6,
                dpi=300)

# br |>
#   ggplot2::ggplot()+
#   ggplot2::geom_sf(fill="white", color="black",
#                    size=.15, show.legend = FALSE)+
#   ggplot2::geom_point(data=xco2_aux_detrend_new |>
#                         dplyr::mutate(
#                           xco2 = dplyr::case_when(
#                             beta_line > q3_xco2 ~ 'Source',
#                             beta_line < q1_xco2 ~'Sink',
#                             .default = 'Non Significant'
#                           )
#                         ) |>
#                         dplyr::filter(xco2!='Non Significant')|>
#                         dplyr::mutate(
#                           beta_molm=(beta_error*10000)*44/24.45,
#                           fco2_error = beta_molm*30/1000,
#                         ),
#                       ggplot2::aes(x=lon,y=lat,color=fco2_error),
#   )+
#   ggplot2::scale_color_gradient(high='yellow',low='blue')+
#   tema_mapa()+
#   ggplot2::labs(x='Longitude',y='Latitude',col=expression('FCO'[2]~' error (g'~m^-2*month^-1~')'))
#
# ggplot2::ggsave('img/xco2_emissionerror_2018.png',units="in", width=8, height=6,
#                 dpi=1000)
#
# br |>
#   ggplot2::ggplot()+
#   ggplot2::geom_sf(fill="white", color="black",
#                    size=.15, show.legend = FALSE)+
#   ggplot2::geom_point(data=xco2_aux_detrend_new |>
#                         dplyr::mutate(
#                           xco2 = dplyr::case_when(
#                             beta_line > q3_xco2 ~ 'Source',
#                             beta_line < q1_xco2 ~'Sink',
#                             .default = 'Non Significant'
#                           )
#                         ) |>
#                         dplyr::filter(xco2!='Non Significant') |>
#                         dplyr::mutate(
#                           beta_molm=(10000*model_error)*44/24.45,
#                           fco2_error = beta_molm*12.01/1000,
#                         ),
#                       ggplot2::aes(x=lon,y=lat,color=fco2_error),
#   )+
#   ggplot2::scale_color_gradient(high='yellow',low='blue')+
#   tema_mapa()


####Balance

xco2_aux_detrend_new |>
  dplyr::mutate(
    xco2 = dplyr::case_when(
      beta_line > q3_xco2 ~ 'Source',
      beta_line < q1_xco2 ~'Sink',
      .default = 'Non Significant'
    )
  ) |>
  dplyr::filter(xco2!='Non Significant')|>
  dplyr::mutate(
    beta_molm=(10000*beta_line)*44/24.45,
    fco2 = beta_molm*30/1000,
    fco2=(fco2*3025000000)/1e+15
  ) |>
  dplyr::ungroup() |>
  dplyr::summarise(
    fco2=sum(fco2)
  )


xco2_aux_detrend_new |>
  dplyr::mutate(
    xco2 = dplyr::case_when(
      beta_line > q3_xco2 ~ 'Source',
      beta_line < q1_xco2 ~'Sink',
      .default = 'Non Significant'
    )
  ) |>
  dplyr::filter(xco2!='Non Significant') |>
  dplyr::mutate(
    beta_molm=beta_line*44/24.45,
    fco2 = beta_molm*30/1000)|>
  dplyr::ungroup() |>
  dplyr::summarise(
    fco2m=mean(fco2),
    fco2sd=sd(fco2)
  )

df2018 <- xco2_aux_detrend_new |> dplyr::mutate(
  xco2 = dplyr::case_when(
    beta_line > q3_xco2 ~ 'Source',
    beta_line < q1_xco2 ~'Sink',
    .default = 'Non Significant'
  )
) |>
  dplyr::filter(xco2!='Non Significant')

df2018all <- xco2_aux_detrend_new |> dplyr::mutate(
  xco2 = dplyr::case_when(
    beta_line > q3_xco2 ~ 'Source',
    beta_line < q1_xco2 ~'Sink',
    .default = 'Non Significant'
  )
)

#### 2019

xco2df_filter <- xco2df |>
  dplyr::filter(year == 2019) |>
  dplyr::filter(dist_xco2<0.25) |>
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


xco2df_filter |>
  dplyr::group_by(date) |>
  dplyr::summarise(xco2_mean=mean(xco2_mean)) |>
  ggplot2::ggplot(ggplot2::aes(x=date,y=xco2_mean )) +
  ggplot2::geom_point(shape=21,color="black",fill="gray") +
  ggplot2::geom_line(color="red") +
  ggplot2::geom_smooth(method = "lm") +
  ggpubr::stat_regline_equation(ggplot2::aes(
    label =  paste(..eq.label.., ..rr.label.., sep = "*plain(\",\")~~"))) +
  ggplot2::xlab('Date')+
  ggplot2::ylab(expression(
    'Xco'[2]~' (ppm)'
  ))+
  ggplot2::theme_bw()

ggplot2::ggsave('img/xco2_temporal_2019.png',units="in", width=8, height=6,
                dpi=300)

mod <- lm(xco2_mean~x,data =xco2df_filter |>
            dplyr::mutate(
              x = 1:dplyr::n()
            ))



xco2df_filter |>
  dplyr::mutate(
    x=1:dplyr::n(),
    xco2_est = mod$coefficients[1] + mod$coefficients[2]*x,
    delta=xco2_est - xco2_mean,
    xco2r = (mod$coefficients[1]-delta)-(mean(xco2_mean)-mod$coefficients[1])
  ) |>
  dplyr::group_by(date) |>
  dplyr::summarise(xco2_mean=mean(xco2r)) |>
  ggplot2::ggplot(ggplot2::aes(x=date,y=xco2_mean )) +
  ggplot2::geom_point(shape=21,color="black",fill="gray") +
  ggplot2::geom_line(color="red") +
  ggplot2::geom_smooth(method = "lm") +
  ggpubr::stat_regline_equation(ggplot2::aes(
    label =  paste(..eq.label.., ..rr.label.., sep = "*plain(\",\")~~"))) +
  ggplot2::xlab('Date')+
  ggplot2::ylab(expression(
    'Xco'[2][R]~' (ppm)'
  ))+
  ggplot2::theme_bw()

ggplot2::ggsave('img/xco2_detrendtemporal_2019.png',units="in", width=8, height=6,
                dpi=300)

xco2detrend <- xco2df_filter |>
  dplyr::mutate(
    x=1:dplyr::n(),
    xco2_est = mod$coefficients[1] + mod$coefficients[2]*x,
    delta=xco2_est - xco2_mean,
    xco2r = (mod$coefficients[1]-delta)-(mean(xco2_mean)-mod$coefficients[1])
  )
xco2_aux_detrend <- xco2detrend |>
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
hist(xco2detrend$xco2r)

xco2_nest <- xco2detrend |>
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

### calculating for each pixel
xco2_nest_detrend <- xco2_nest|>
  dplyr::mutate(
    beta_line = purrr::map(data,linear_reg, output="beta1"),
    p_value = purrr::map(data,linear_reg, output="p_value"),
    partial = purrr::map(data,linear_reg, output="partial"),
    n_obs = purrr::map(data,linear_reg, output="n"),
    beta_error=purrr::map(data,linear_reg,output='betaerror'),
    model_error=purrr::map(data,linear_reg,output='modelerror')
  )


# xco2_nest_detrend |>
#   dplyr::filter(n_obs > 7) |>
#   tidyr::unnest(cols = c(beta_error)) |>
#   dplyr::select(lon, lat, beta_error) |>
#   ggplot2::ggplot(ggplot2::aes(x=lon, y=lat, color = beta_error)) +
#   ggplot2::geom_point()
#
# xco2_nest_detrend |>
#   dplyr::filter(n_obs > 7) |>
#   tidyr::unnest(cols = c(beta_error,model_error)) |>
#   dplyr::ungroup() |>
#   dplyr::summarise(b=mean(beta_error),
#                    m=mean(model_error))
#
#
# xco2_nest_detrend |>dplyr::filter(n_obs > 5) |>
#   tidyr::unnest(cols = c(beta_error)) |>
#   dplyr::select(lon, lat, beta_error) |>
#   ggplot2::ggplot(ggplot2::aes(x=lon, y=lat, color = beta_error)) +
#   ggplot2::geom_point()
#
# xco2_nest_detrend |>
#   dplyr::filter(n_obs > 5) |>
#   tidyr::unnest(cols = c(beta_error,model_error)) |>
#   dplyr::ungroup() |>
#   dplyr::summarise(b=mean(beta_error),
#                    m=mean(model_error))
#
# xco2_nest_detrend |>dplyr::filter(n_obs > 4) |>
#   tidyr::unnest(cols = c(beta_error)) |>
#   dplyr::select(lon, lat, beta_error) |>
#   ggplot2::ggplot(ggplot2::aes(x=lon, y=lat, color = beta_error)) +
#   ggplot2::geom_point()
#
# xco2_nest_detrend |>
#   dplyr::filter(n_obs > 4) |>
#   tidyr::unnest(cols = c(beta_error,model_error)) |>
#   dplyr::ungroup() |>
#   dplyr::summarise(b=mean(beta_error),
#                    m=mean(model_error))
#
# xco2_nest_detrend |>dplyr::filter(n_obs > 3) |>
#   tidyr::unnest(cols = c(beta_error)) |>
#   dplyr::select(lon, lat, beta_error) |>
#   ggplot2::ggplot(ggplot2::aes(x=lon, y=lat, color = beta_error)) +
#   ggplot2::geom_point()
#
# xco2_nest_detrend |>
#   dplyr::filter(n_obs > 3) |>
#   tidyr::unnest(cols = c(beta_error,model_error)) |>
#   dplyr::ungroup() |>
#   dplyr::summarise(b=mean(beta_error),
#                    m=mean(model_error))

### creating a table

xco2_aux_detrend_new <- xco2_nest_detrend |>
  dplyr::filter(n_obs > 4) |>
  tidyr::unnest(cols = c(beta_line, partial,beta_error,model_error)) |>
  dplyr::ungroup() |>
  dplyr::select(lon, lat, beta_line, partial,beta_error,model_error)

q3_xco2 <- xco2_aux_detrend_new |> dplyr::pull(beta_line) |> quantile(.75)
q1_xco2 <- xco2_aux_detrend_new |> dplyr::pull(beta_line) |> quantile(.25)


xco2_aux_detrend_new <- xco2_aux_detrend_new  |>
  dplyr::mutate(
    anomaly = partial - xco2_aux_detrend_new  |>
      dplyr::pull(partial) |>
      median(),
    Dbeta = beta_line -xco2_aux_detrend_new  |>
      dplyr::pull(beta_line) |> mean(na.rm=TRUE)
  )

q3_anom <- xco2_aux_detrend_new  |> dplyr::pull(anomaly) |> quantile(.75)
q1_anom <- xco2_aux_detrend_new  |> dplyr::pull(anomaly) |> quantile(.25)

xco2_aux_detrend_new  <- xco2_aux_detrend_new  |>
  dplyr::mutate(
    beta_index =  ifelse(beta_line >=q1_xco2, 0, 1)
  )
xco2_aux_detrend_new |>
  ggplot2::ggplot(ggplot2::aes(x=beta_line)) +
  ggplot2::geom_histogram(bins=30,
                          fill="orange",
                          color="black") +
  ggplot2::labs(x="βpixel",y="Count") +
  ggplot2::geom_vline(xintercept = q1_xco2,
                      color = "red",
                      lty=2) +
  ggplot2::geom_vline(xintercept = q3_xco2,
                      color = "red",
                      lty=2)+
  gghighlight::gghighlight(beta_line < q1_xco2 | beta_line>q3_xco2,
                           unhighlighted_params = list(
                             color = "darkgray",
                             fill = "lightgray")) +
  ggplot2::theme_minimal()




### Source and Sink Indentification

south_america |>
  ggplot2::ggplot()+
  ggspatial::annotation_map_tile(type = 'cartolight')+
  ggplot2::geom_sf(col='grey',fill='white')+
  ggplot2::geom_sf(data=biomes,col='black',fill='NA',linetype='dashed')+
  ggplot2::ylim(-40,6)+
  ggplot2::xlim(-80,-35)+
  ggplot2::geom_tile(data=xco2_aux_detrend_new |>
                       dplyr::mutate(
                         xco2 = dplyr::case_when(
                           beta_line > q3_xco2 ~ 'Source',
                           beta_line < q1_xco2 ~'Sink',
                           .default = 'Non Significant'
                         )
                       ) |>
                       dplyr::filter(xco2!='Non Significant'),
                     ggplot2::aes(x=lon,y=lat,color=xco2,fill=xco2),
  )+
  tema_mapa2()+
  ggplot2::scale_color_manual(values = c('darkgreen','darkred'))+
  ggplot2::scale_fill_manual(values = c('darkgreen','darkred'))+
  ggplot2::labs(x='Longitude',y='Latitude',
                col=expression('Xco'[2]),fill=expression('Xco'[2]))

ggplot2::ggsave('img/xco2_identification_new_2019.png',units="in", width=8, height=6,
                dpi=300)

# br |>
#   ggplot2::ggplot()+
#   ggplot2::geom_sf(fill="white", color="black",
#                    size=.15, show.legend = FALSE)+
#   ggplot2::geom_point(data=xco2_aux_detrend_new |>
#                         dplyr::mutate(
#                           xco2 = dplyr::case_when(
#                             beta_line > q3_xco2 ~ 'Source',
#                             beta_line < q1_xco2 ~'Sink',
#                             .default = 'Non Significant'
#                           )
#                         ) |>
#                         dplyr::filter(xco2!='Non Significant'),
#                       ggplot2::aes(x=lon,y=lat,color=xco2),
#   )+
#   tema_mapa()+
#   ggplot2::scale_color_manual(values = c('darkblue','darkred'))+
#   ggplot2::labs(x='Longitude',y='Latitude',col=expression('Xco'[2]))
#
# ggplot2::ggsave('img/xco2_identification_2019.png',units="in", width=8, height=6,
#                 dpi=1000)

xco2_aux_detrend_new |>
  dplyr::filter(beta_line<0) |>
  dplyr::summarise(
    nobs=dplyr::n()
  )


### Beta
south_america |>
  ggplot2::ggplot()+
  ggspatial::annotation_map_tile(type = 'cartolight')+
  ggplot2::geom_sf(col='grey',fill='white')+
  ggplot2::geom_sf(data=biomes,col='black',fill='NA',linetype='dashed')+
  ggplot2::ylim(-40,6)+
  ggplot2::xlim(-80,-35)+
  ggplot2::geom_tile(data=xco2_aux_detrend_new |>
                       dplyr::mutate(
                         xco2 = dplyr::case_when(
                           beta_line > q3_xco2 ~ 'Source',
                           beta_line < q1_xco2 ~'Sink',
                           .default = 'Non Significant'
                         )
                       ) |>
                       dplyr::filter(xco2!='Non Significant'),
                     ggplot2::aes(x=lon,y=lat,
                                  col=beta_line*30,
                                  fill=beta_line*30),
  )+
  ggplot2::scale_color_gradient(high='yellow',low='blue')+
  ggplot2::scale_fill_gradient(high='yellow',low='blue')+
  tema_mapa2()+
  ggplot2::labs(x='Longitude',y='Latitude',
                col=expression('β (ppm '~month^-1~')'),
                fill=expression('β (ppm '~month^-1~')')
  )
ggplot2::ggsave('img/xco2_beta_new_2019.png',units="in", width=8, height=6,
                dpi=300)


# br |>
#   ggplot2::ggplot()+
#   ggplot2::geom_sf(fill="white", color="black",
#                    size=.15, show.legend = FALSE)+
#   ggplot2::geom_point(data=xco2_aux_detrend_new |>
#                         dplyr::mutate(
#                           xco2 = dplyr::case_when(
#                             beta_line > q3_xco2 ~ 'Source',
#                             beta_line < q1_xco2 ~'Sink',
#                             .default = 'Non Significant'
#                           )
#                         ) |>
#                         dplyr::filter(xco2!='Non Significant'),
#                       ggplot2::aes(x=lon,y=lat,col=beta_line*30),
#   )+
#   ggplot2::scale_color_gradient(high='yellow',low='blue')+
#   tema_mapa()+
#   ggplot2::labs(x='Longitude',y='Latitude',col=expression('β (ppm '~month^-1~')'))
#
# ggplot2::ggsave('img/xco2_beta_2019.png',units="in", width=8, height=6,
#                 dpi=1000)


### uncertanty
south_america |>
  ggplot2::ggplot()+
  ggspatial::annotation_map_tile(type = 'cartolight')+
  ggplot2::geom_sf(col='grey',fill='white')+
  ggplot2::geom_sf(data=biomes,col='black',fill='NA',linetype='dashed')+
  ggplot2::ylim(-40,6)+
  ggplot2::xlim(-80,-35)+
  ggplot2::geom_tile(data=xco2_aux_detrend_new |>
                       dplyr::mutate(
                         xco2 = dplyr::case_when(
                           beta_line > q3_xco2 ~ 'Source',
                           beta_line < q1_xco2 ~'Sink',
                           .default = 'Non Significant'
                         )
                       ) |>
                       dplyr::filter(xco2!='Non Significant'),
                     ggplot2::aes(x=lon,y=lat,
                                  col=beta_error*30,
                                  fill=beta_error*30),
  )+
  ggplot2::scale_color_gradient(high='yellow',low='blue')+
  ggplot2::scale_fill_gradient(high='yellow',low='blue')+
  tema_mapa2()+
  ggplot2::labs(x='Longitude',y='Latitude',
                col=expression('β error (ppm '~month^-1~')'),
                fill=expression('β error (ppm '~month^-1~')')
  )
ggplot2::ggsave('img/xco2_beta_error_new_2019.png',units="in", width=8, height=6,
                dpi=300)
#
# br |>
#   ggplot2::ggplot()+
#   ggplot2::geom_sf(fill="white", color="black",
#                    size=.15, show.legend = FALSE)+
#   ggplot2::geom_point(data=xco2_aux_detrend_new |>
#                         dplyr::mutate(
#                           xco2 = dplyr::case_when(
#                             beta_line > q3_xco2 ~ 'Source',
#                             beta_line < q1_xco2 ~'Sink',
#                             .default = 'Non Significant'
#                           )
#                         ) |>
#                         dplyr::filter(xco2!='Non Significant'),
#                       ggplot2::aes(x=lon,y=lat,col=beta_error*30),
#   )+
#   ggplot2::scale_color_gradient(high='yellow',low='blue')+
#   tema_mapa()+
#   ggplot2::labs(x='Longitude',y='Latitude',col=expression('β error (ppm '~month^-1~')'))
#
# ggplot2::ggsave('img/xco2_betaerror_2019.png',units="in", width=8, height=6,
#                 dpi=1000)
#
# br |>
#   ggplot2::ggplot()+
#   ggplot2::geom_sf(fill="white", color="black",
#                    size=.15, show.legend = FALSE)+
#   ggplot2::geom_point(data=xco2_aux_detrend_new |>
#                         dplyr::mutate(
#                           xco2 = dplyr::case_when(
#                             beta_line > q3_xco2 ~ 'Source',
#                             beta_line < q1_xco2 ~'Sink',
#                             .default = 'Non Significant'
#                           )
#                         ) |>
#                         dplyr::filter(xco2!='Non Significant'),
#                       ggplot2::aes(x=lon,y=lat,col=model_error),
#   )+
#   ggplot2::scale_color_gradient(high='yellow',low='blue')+
#   tema_mapa()+
#   ggplot2::labs(x='Longitude',y='Latitude',col=expression('Model error (ppm '~month^-1~')'))
#
# ggplot2::ggsave('img/xco2_model_2019.png',units="in", width=8, height=6,
#                 dpi=1000)


#### CO2 emission and assimilation
south_america |>
  ggplot2::ggplot()+
  ggspatial::annotation_map_tile(type = 'cartolight')+
  ggplot2::geom_sf(col='grey',fill='white')+
  ggplot2::geom_sf(data=biomes,col='black',fill='NA',linetype='dashed')+
  ggplot2::ylim(-40,6)+
  ggplot2::xlim(-80,-35)+
  ggplot2::geom_tile(data=xco2_aux_detrend_new |>
                       dplyr::mutate(
                         xco2 = dplyr::case_when(
                           beta_line > q3_xco2 ~ 'Source',
                           beta_line < q1_xco2 ~'Sink',
                           .default = 'Non Significant'
                         )
                       ) |>
                       dplyr::filter(xco2!='Non Significant') |>
                       dplyr::mutate(
                         beta_molm=(beta_line*10000)*44/24.45,
                         fco2 = beta_molm*30/1000,
                       ),
                     ggplot2::aes(x=lon,y=lat,color=fco2,fill=fco2)
  )+
  ggplot2::scale_color_gradient(high='yellow',low='blue')+
  ggplot2::scale_fill_gradient(high='yellow',low='blue')+
  tema_mapa2()+
  ggplot2::labs(x='Longitude',y='Latitude',
                col=expression('FCO'[2]~'(g'~m^-2*month^-1~')'),
                fill=expression('FCO'[2]~'(g'~m^-2*month^-1~')')
  )

ggplot2::ggsave('img/xco2_emission_nwe_2019.png',units="in", width=8, height=6,
                dpi=300)

# br |>
#   ggplot2::ggplot()+
#   ggplot2::geom_sf(fill="white", color="black",
#                    size=.15, show.legend = FALSE)+
#   ggplot2::geom_point(data=xco2_aux_detrend_new |>
#                         dplyr::mutate(
#                           xco2 = dplyr::case_when(
#                             beta_line > q3_xco2 ~ 'Source',
#                             beta_line < q1_xco2 ~'Sink',
#                             .default = 'Non Significant'
#                           )
#                         ) |>
#                         dplyr::filter(xco2!='Non Significant') |>
#                         dplyr::mutate(
#                           beta_molm=(beta_line*10000)*44/24.45,
#                           fco2 = beta_molm*30/1000,
#                         ),
#                       ggplot2::aes(x=lon,y=lat,color=fco2),
#   )+
#   ggplot2::scale_color_gradient(high='yellow',low='blue')+
#   tema_mapa()+
#   ggplot2::labs(x='Longitude',y='Latitude',col=expression('FCO'[2]~'(g'~m^-2*month^-1~')'))
#
# ggplot2::ggsave('img/xco2_emission_2019.png',units="in", width=8, height=6,
#                 dpi=1000)

south_america |>
  ggplot2::ggplot()+
  ggspatial::annotation_map_tile(type = 'cartolight')+
  ggplot2::geom_sf(col='grey',fill='white')+
  ggplot2::geom_sf(data=biomes,col='black',fill='NA',linetype='dashed')+
  ggplot2::ylim(-40,6)+
  ggplot2::xlim(-80,-35)+
  ggplot2::geom_tile(data=xco2_aux_detrend_new |>
                       dplyr::mutate(
                         xco2 = dplyr::case_when(
                           beta_line > q3_xco2 ~ 'Source',
                           beta_line < q1_xco2 ~'Sink',
                           .default = 'Non Significant'
                         )
                       ) |>
                       dplyr::filter(xco2!='Non Significant')|>
                       dplyr::mutate(
                         beta_molm=(beta_error*10000)*44/24.45,
                         fco2_error = beta_molm*30/1000),
                     ggplot2::aes(x=lon,y=lat,color=fco2_error,fill=fco2_error)
  )+
  ggplot2::scale_color_gradient(high='yellow',low='blue')+
  ggplot2::scale_fill_gradient(high='yellow',low='blue')+
  tema_mapa2()+
  ggplot2::labs(x='Longitude',y='Latitude',
                col=expression('FCO'[2]~'error (g'~m^-2*month^-1~')'),
                fill=expression('FCO'[2]~'error (g'~m^-2*month^-1~')')
  )

ggplot2::ggsave('img/xco2_emissionerror_new_2019.png',units="in", width=8, height=6,
                dpi=300)

# br |>
#   ggplot2::ggplot()+
#   ggplot2::geom_sf(fill="white", color="black",
#                    size=.15, show.legend = FALSE)+
#   ggplot2::geom_point(data=xco2_aux_detrend_new |>
#                         dplyr::mutate(
#                           xco2 = dplyr::case_when(
#                             beta_line > q3_xco2 ~ 'Source',
#                             beta_line < q1_xco2 ~'Sink',
#                             .default = 'Non Significant'
#                           )
#                         ) |>
#                         dplyr::filter(xco2!='Non Significant')|>
#                         dplyr::mutate(
#                           beta_molm=(beta_error*10000)*44/24.45,
#                           fco2_error = beta_molm*30/1000,
#                         ),
#                       ggplot2::aes(x=lon,y=lat,color=fco2_error),
#   )+
#   ggplot2::scale_color_gradient(high='yellow',low='blue')+
#   tema_mapa()+
#   ggplot2::labs(x='Longitude',y='Latitude',col=expression('FCO'[2]~' error (g'~m^-2*month^-1~')'))
#
# ggplot2::ggsave('img/xco2_emissionerror_2019.png',units="in", width=8, height=6,
#                 dpi=1000)
#
# br |>
#   ggplot2::ggplot()+
#   ggplot2::geom_sf(fill="white", color="black",
#                    size=.15, show.legend = FALSE)+
#   ggplot2::geom_point(data=xco2_aux_detrend_new |>
#                         dplyr::mutate(
#                           xco2 = dplyr::case_when(
#                             beta_line > q3_xco2 ~ 'Source',
#                             beta_line < q1_xco2 ~'Sink',
#                             .default = 'Non Significant'
#                           )
#                         ) |>
#                         dplyr::filter(xco2!='Non Significant') |>
#                         dplyr::mutate(
#                           beta_molm=(10000*model_error)*44/24.45,
#                           fco2_error = beta_molm*12.01/1000,
#                         ),
#                       ggplot2::aes(x=lon,y=lat,color=fco2_error),
#   )+
#   ggplot2::scale_color_gradient(high='yellow',low='blue')+
#   tema_mapa()
#

####Balance

xco2_aux_detrend_new |>
  dplyr::mutate(
    xco2 = dplyr::case_when(
      beta_line > q3_xco2 ~ 'Source',
      beta_line < q1_xco2 ~'Sink',
      .default = 'Non Significant'
    )
  ) |>
  dplyr::filter(xco2!='Non Significant')|>
  dplyr::mutate(
    beta_molm=(10000*beta_line)*44/24.45,
    fco2 = beta_molm*30/1000,
    fco2=(fco2*3025000000)/1e+15
  ) |>
  dplyr::ungroup() |>
  dplyr::summarise(
    fco2=sum(fco2)
  )


xco2_aux_detrend_new |>
  dplyr::mutate(
    xco2 = dplyr::case_when(
      beta_line > q3_xco2 ~ 'Source',
      beta_line < q1_xco2 ~'Sink',
      .default = 'Non Significant'
    )
  ) |>
  dplyr::filter(xco2!='Non Significant') |>
  dplyr::mutate(
    beta_molm=beta_line*44/24.45,
    fco2 = beta_molm*30/1000)|>
  dplyr::ungroup() |>
  dplyr::summarise(
    fco2m=mean(fco2),
    fco2sd=sd(fco2)
  )

df2019 <- xco2_aux_detrend_new |> dplyr::mutate(
  xco2 = dplyr::case_when(
    beta_line > q3_xco2 ~ 'Source',
    beta_line < q1_xco2 ~'Sink',
    .default = 'Non Significant'
  )
) |>
  dplyr::filter(xco2!='Non Significant')

df2019all <- xco2_aux_detrend_new |> dplyr::mutate(
  xco2 = dplyr::case_when(
    beta_line > q3_xco2 ~ 'Source',
    beta_line < q1_xco2 ~'Sink',
    .default = 'Non Significant'
  )
)
#### 2020

xco2df_filter <- xco2df |>
  dplyr::filter(year == 2020) |>
  dplyr::filter(dist_xco2<0.25) |>
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


xco2df_filter |>
  dplyr::group_by(date) |>
  dplyr::summarise(xco2_mean=mean(xco2_mean)) |>
  ggplot2::ggplot(ggplot2::aes(x=date,y=xco2_mean )) +
  ggplot2::geom_point(shape=21,color="black",fill="gray") +
  ggplot2::geom_line(color="red") +
  ggplot2::geom_smooth(method = "lm") +
  ggpubr::stat_regline_equation(ggplot2::aes(
    label =  paste(..eq.label.., ..rr.label.., sep = "*plain(\",\")~~"))) +
  ggplot2::xlab('Date')+
  ggplot2::ylab(expression(
    'Xco'[2]~' (ppm)'
  ))+
  ggplot2::theme_bw()

ggplot2::ggsave('img/xco2_temporal_2020.png',units="in", width=8, height=6,
                dpi=300)

mod <- lm(xco2_mean~x,data =xco2df_filter |>
            dplyr::mutate(
              x = 1:dplyr::n()
            ))


xco2df_filter |>
  dplyr::mutate(
    x=1:dplyr::n(),
    xco2_est = mod$coefficients[1] + mod$coefficients[2]*x,
    delta=xco2_est - xco2_mean,
    xco2r = (mod$coefficients[1]-delta)-(mean(xco2_mean)-mod$coefficients[1])
  ) |>
  dplyr::group_by(date) |>
  dplyr::summarise(xco2_mean=mean(xco2r)) |>
  ggplot2::ggplot(ggplot2::aes(x=date,y=xco2_mean )) +
  ggplot2::geom_point(shape=21,color="black",fill="gray") +
  ggplot2::geom_line(color="red") +
  ggplot2::geom_smooth(method = "lm") +
  ggpubr::stat_regline_equation(ggplot2::aes(
    label =  paste(..eq.label.., ..rr.label.., sep = "*plain(\",\")~~"))) +
  ggplot2::xlab('Date')+
  ggplot2::ylab(expression(
    'Xco'[2][R]~' (ppm)'
  ))+
  ggplot2::theme_bw()
ggplot2::ggsave('img/xco2_detrendtemporal_2020.png',units="in", width=8, height=6,
                dpi=300)

xco2detrend <- xco2df_filter |>
  dplyr::mutate(
    x=1:dplyr::n(),
    xco2_est = mod$coefficients[1] + mod$coefficients[2]*x,
    delta=xco2_est - xco2_mean,
    xco2r = (mod$coefficients[1]-delta)-(mean(xco2_mean)-mod$coefficients[1])
  )
xco2_aux_detrend <- xco2detrend |>
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
hist(xco2detrend$xco2r)

xco2_nest <- xco2detrend |>
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

### calculating for each pixel
xco2_nest_detrend <- xco2_nest|>
  dplyr::mutate(
    beta_line = purrr::map(data,linear_reg, output="beta1"),
    p_value = purrr::map(data,linear_reg, output="p_value"),
    partial = purrr::map(data,linear_reg, output="partial"),
    n_obs = purrr::map(data,linear_reg, output="n"),
    beta_error=purrr::map(data,linear_reg,output='betaerror'),
    model_error=purrr::map(data,linear_reg,output='modelerror')
  )


#
# xco2_nest_detrend |>dplyr::filter(n_obs > 7) |>
#   tidyr::unnest(cols = c(beta_error)) |>
#   dplyr::select(lon, lat, beta_error) |>
#   ggplot2::ggplot(ggplot2::aes(x=lon, y=lat, color = beta_error)) +
#   ggplot2::geom_point()
#
# xco2_nest_detrend |>
#   dplyr::filter(n_obs > 7) |>
#   tidyr::unnest(cols = c(beta_error,model_error)) |>
#   dplyr::ungroup() |>
#   dplyr::summarise(b=mean(beta_error),
#                    m=mean(model_error))
#
#
# xco2_nest_detrend |>dplyr::filter(n_obs > 5) |>
#   tidyr::unnest(cols = c(beta_error)) |>
#   dplyr::select(lon, lat, beta_error) |>
#   ggplot2::ggplot(ggplot2::aes(x=lon, y=lat, color = beta_error)) +
#   ggplot2::geom_point()
#
# xco2_nest_detrend |>
#   dplyr::filter(n_obs > 5) |>
#   tidyr::unnest(cols = c(beta_error,model_error)) |>
#   dplyr::ungroup() |>
#   dplyr::summarise(b=mean(beta_error),
#                    m=mean(model_error))
#
# xco2_nest_detrend |>dplyr::filter(n_obs > 4) |>
#   tidyr::unnest(cols = c(beta_error)) |>
#   dplyr::select(lon, lat, beta_error) |>
#   ggplot2::ggplot(ggplot2::aes(x=lon, y=lat, color = beta_error)) +
#   ggplot2::geom_point()
#
# xco2_nest_detrend |>
#   dplyr::filter(n_obs > 4) |>
#   tidyr::unnest(cols = c(beta_error,model_error)) |>
#   dplyr::ungroup() |>
#   dplyr::summarise(b=mean(beta_error),
#                    m=mean(model_error))
#
# xco2_nest_detrend |>dplyr::filter(n_obs > 3) |>
#   tidyr::unnest(cols = c(beta_error)) |>
#   dplyr::select(lon, lat, beta_error) |>
#   ggplot2::ggplot(ggplot2::aes(x=lon, y=lat, color = beta_error)) +
#   ggplot2::geom_point()
#
# xco2_nest_detrend |>
#   dplyr::filter(n_obs > 3) |>
#   tidyr::unnest(cols = c(beta_error,model_error)) |>
#   dplyr::ungroup() |>
#   dplyr::summarise(b=mean(beta_error),
#                    m=mean(model_error))

### creating a table

xco2_aux_detrend_new <- xco2_nest_detrend |>
  dplyr::filter(n_obs > 4) |>
  tidyr::unnest(cols = c(beta_line, partial,beta_error,model_error)) |>
  dplyr::ungroup() |>
  dplyr::select(lon, lat, beta_line, partial,beta_error,model_error)

q3_xco2 <- xco2_aux_detrend_new |> dplyr::pull(beta_line) |> quantile(.75)
q1_xco2 <- xco2_aux_detrend_new |> dplyr::pull(beta_line) |> quantile(.25)

xco2_aux_detrend_new <- xco2_aux_detrend_new  |>
  dplyr::mutate(
    anomaly = partial - xco2_aux_detrend_new  |>
      dplyr::pull(partial) |>
      median(),
    Dbeta = beta_line -xco2_aux_detrend_new  |>
      dplyr::pull(beta_line) |> mean(na.rm=TRUE)
  )

q3_anom <- xco2_aux_detrend_new  |> dplyr::pull(anomaly) |> quantile(.75)
q1_anom <- xco2_aux_detrend_new  |> dplyr::pull(anomaly) |> quantile(.25)

xco2_aux_detrend_new  <- xco2_aux_detrend_new  |>
  dplyr::mutate(
    beta_index =  ifelse(beta_line >=q1_xco2, 0, 1)
  )
xco2_aux_detrend_new |>
  ggplot2::ggplot(ggplot2::aes(x=beta_line)) +
  ggplot2::geom_histogram(bins=30,
                          fill="orange",
                          color="black") +
  ggplot2::labs(x="βpixel",y="Count") +
  ggplot2::geom_vline(xintercept = q1_xco2,
                      color = "red",
                      lty=2) +
  ggplot2::geom_vline(xintercept = q3_xco2,
                      color = "red",
                      lty=2)+
  gghighlight::gghighlight(beta_line < q1_xco2 | beta_line>q3_xco2,
                           unhighlighted_params = list(
                             color = "darkgray",
                             fill = "lightgray"))+
  ggplot2::theme_minimal()


### Source and Sink Indentification

south_america |>
  ggplot2::ggplot()+
  ggspatial::annotation_map_tile(type = 'cartolight')+
  ggplot2::geom_sf(col='grey',fill='white')+
  ggplot2::geom_sf(data=biomes,col='black',fill='NA',linetype='dashed')+
  ggplot2::ylim(-40,6)+
  ggplot2::xlim(-80,-35)+
  ggplot2::geom_tile(data=xco2_aux_detrend_new |>
                       dplyr::mutate(
                         xco2 = dplyr::case_when(
                           beta_line > q3_xco2 ~ 'Source',
                           beta_line < q1_xco2 ~'Sink',
                           .default = 'Non Significant'
                         )
                       ) |>
                       dplyr::filter(xco2!='Non Significant'),
                     ggplot2::aes(x=lon,y=lat,color=xco2,fill=xco2),
  )+
  tema_mapa2()+
  ggplot2::scale_color_manual(values = c('darkgreen','darkred'))+
  ggplot2::scale_fill_manual(values = c('darkgreen','darkred'))+
  ggplot2::labs(x='Longitude',y='Latitude',
                col=expression('Xco'[2]),fill=expression('Xco'[2]))

ggplot2::ggsave('img/xco2_identification_new_2020.png',units="in", width=8, height=6,
                dpi=300)

# br |>
#   ggplot2::ggplot()+
#   ggplot2::geom_sf(fill="white", color="black",
#                    size=.15, show.legend = FALSE)+
#   ggplot2::geom_point(data=xco2_aux_detrend_new |>
#                         dplyr::mutate(
#                           xco2 = dplyr::case_when(
#                             beta_line > q3_xco2 ~ 'Source',
#                             beta_line < q1_xco2 ~'Sink',
#                             .default = 'Non Significant'
#                           )
#                         ) |>
#                         dplyr::filter(xco2!='Non Significant'),
#                       ggplot2::aes(x=lon,y=lat,color=xco2),
#   )+
#   tema_mapa()+
#   ggplot2::scale_color_manual(values = c('darkblue','darkred'))+
#   ggplot2::labs(x='Longitude',y='Latitude',col=expression('Xco'[2]))
#
# ggplot2::ggsave('img/xco2_identification_2020.png',units="in", width=8, height=6,
#                 dpi=1000)

xco2_aux_detrend_new |>
  dplyr::filter(beta_line<0) |>
  dplyr::summarise(
    nobs=dplyr::n()
  )


### Beta
south_america |>
  ggplot2::ggplot()+
  ggspatial::annotation_map_tile(type = 'cartolight')+
  ggplot2::geom_sf(col='grey',fill='white')+
  ggplot2::geom_sf(data=biomes,col='black',fill='NA',linetype='dashed')+
  ggplot2::ylim(-40,6)+
  ggplot2::xlim(-80,-35)+
  ggplot2::geom_tile(data=xco2_aux_detrend_new |>
                       dplyr::mutate(
                         xco2 = dplyr::case_when(
                           beta_line > q3_xco2 ~ 'Source',
                           beta_line < q1_xco2 ~'Sink',
                           .default = 'Non Significant'
                         )
                       ) |>
                       dplyr::filter(xco2!='Non Significant'),
                     ggplot2::aes(x=lon,y=lat,
                                  col=beta_line*30,
                                  fill=beta_line*30),
  )+
  ggplot2::scale_color_gradient(high='yellow',low='blue')+
  ggplot2::scale_fill_gradient(high='yellow',low='blue')+
  tema_mapa2()+
  ggplot2::labs(x='Longitude',y='Latitude',
                col=expression('β (ppm '~month^-1~')'),
                fill=expression('β (ppm '~month^-1~')')
  )
ggplot2::ggsave('img/xco2_beta_new_2020.png',units="in", width=8, height=6,
                dpi=300)


# br |>
#   ggplot2::ggplot()+
#   ggplot2::geom_sf(fill="white", color="black",
#                    size=.15, show.legend = FALSE)+
#   ggplot2::geom_point(data=xco2_aux_detrend_new |>
#                         dplyr::mutate(
#                           xco2 = dplyr::case_when(
#                             beta_line > q3_xco2 ~ 'Source',
#                             beta_line < q1_xco2 ~'Sink',
#                             .default = 'Non Significant'
#                           )
#                         ) |>
#                         dplyr::filter(xco2!='Non Significant'),
#                       ggplot2::aes(x=lon,y=lat,col=beta_line*30),
#   )+
#   ggplot2::scale_color_gradient(high='yellow',low='blue')+
#   tema_mapa()+
#   ggplot2::labs(x='Longitude',y='Latitude',col=expression('β (ppm '~month^-1~')'))
#
# ggplot2::ggsave('img/xco2_beta_2020.png',units="in", width=8, height=6,
#                 dpi=1000)


### uncertanty
south_america |>
  ggplot2::ggplot()+
  ggspatial::annotation_map_tile(type = 'cartolight')+
  ggplot2::geom_sf(col='grey',fill='white')+
  ggplot2::geom_sf(data=biomes,col='black',fill='NA',linetype='dashed')+
  ggplot2::ylim(-40,6)+
  ggplot2::xlim(-80,-35)+
  ggplot2::geom_tile(data=xco2_aux_detrend_new |>
                       dplyr::mutate(
                         xco2 = dplyr::case_when(
                           beta_line > q3_xco2 ~ 'Source',
                           beta_line < q1_xco2 ~'Sink',
                           .default = 'Non Significant'
                         )
                       ) |>
                       dplyr::filter(xco2!='Non Significant'),
                     ggplot2::aes(x=lon,y=lat,
                                  col=beta_error*30,
                                  fill=beta_error*30),
  )+
  ggplot2::scale_color_gradient(high='yellow',low='blue')+
  ggplot2::scale_fill_gradient(high='yellow',low='blue')+
  tema_mapa2()+
  ggplot2::labs(x='Longitude',y='Latitude',
                col=expression('β error (ppm '~month^-1~')'),
                fill=expression('β error (ppm '~month^-1~')')
  )
ggplot2::ggsave('img/xco2_beta_error_new_2020.png',units="in", width=8, height=6,
                dpi=300)

# br |>
#   ggplot2::ggplot()+
#   ggplot2::geom_sf(fill="white", color="black",
#                    size=.15, show.legend = FALSE)+
#   ggplot2::geom_point(data=xco2_aux_detrend_new |>
#                         dplyr::mutate(
#                           xco2 = dplyr::case_when(
#                             beta_line > q3_xco2 ~ 'Source',
#                             beta_line < q1_xco2 ~'Sink',
#                             .default = 'Non Significant'
#                           )
#                         ) |>
#                         dplyr::filter(xco2!='Non Significant'),
#                       ggplot2::aes(x=lon,y=lat,col=beta_error*30),
#   )+
#   ggplot2::scale_color_gradient(high='yellow',low='blue')+
#   tema_mapa()+
#   ggplot2::labs(x='Longitude',y='Latitude',col=expression('β error (ppm '~month^-1~')'))
#
# ggplot2::ggsave('img/xco2_betaerror_2020.png',units="in", width=8, height=6,
#                 dpi=1000)
#
# br |>
#   ggplot2::ggplot()+
#   ggplot2::geom_sf(fill="white", color="black",
#                    size=.15, show.legend = FALSE)+
#   ggplot2::geom_point(data=xco2_aux_detrend_new |>
#                         dplyr::mutate(
#                           xco2 = dplyr::case_when(
#                             beta_line > q3_xco2 ~ 'Source',
#                             beta_line < q1_xco2 ~'Sink',
#                             .default = 'Non Significant'
#                           )
#                         ) |>
#                         dplyr::filter(xco2!='Non Significant'),
#                       ggplot2::aes(x=lon,y=lat,col=model_error),
#   )+
#   ggplot2::scale_color_gradient(high='yellow',low='blue')+
#   tema_mapa()+
#   ggplot2::labs(x='Longitude',y='Latitude',col=expression('Model error (ppm '~month^-1~')'))
#
# ggplot2::ggsave('img/xco2_model_2020.png',units="in", width=8, height=6,
#                 dpi=1000)


#### CO2 emission and assimilation
south_america |>
  ggplot2::ggplot()+
  ggspatial::annotation_map_tile(type = 'cartolight')+
  ggplot2::geom_sf(col='grey',fill='white')+
  ggplot2::geom_sf(data=biomes,col='black',fill='NA',linetype='dashed')+
  ggplot2::ylim(-40,6)+
  ggplot2::xlim(-80,-35)+
  ggplot2::geom_tile(data=xco2_aux_detrend_new |>
                       dplyr::mutate(
                         xco2 = dplyr::case_when(
                           beta_line > q3_xco2 ~ 'Source',
                           beta_line < q1_xco2 ~'Sink',
                           .default = 'Non Significant'
                         )
                       ) |>
                       dplyr::filter(xco2!='Non Significant') |>
                       dplyr::mutate(
                         beta_molm=(beta_line*10000)*44/24.45,
                         fco2 = beta_molm*30/1000,
                       ),
                     ggplot2::aes(x=lon,y=lat,color=fco2,fill=fco2)
  )+
  ggplot2::scale_color_gradient(high='yellow',low='blue')+
  ggplot2::scale_fill_gradient(high='yellow',low='blue')+
  tema_mapa2()+
  ggplot2::labs(x='Longitude',y='Latitude',
                col=expression('FCO'[2]~'(g'~m^-2*month^-1~')'),
                fill=expression('FCO'[2]~'(g'~m^-2*month^-1~')')
  )

ggplot2::ggsave('img/xco2_emission_nwe_2020.png',units="in", width=8, height=6,
                dpi=300)

# br |>
#   ggplot2::ggplot()+
#   ggplot2::geom_sf(fill="white", color="black",
#                    size=.15, show.legend = FALSE)+
#   ggplot2::geom_point(data=xco2_aux_detrend_new |>
#                         dplyr::mutate(
#                           xco2 = dplyr::case_when(
#                             beta_line > q3_xco2 ~ 'Source',
#                             beta_line < q1_xco2 ~'Sink',
#                             .default = 'Non Significant'
#                           )
#                         ) |>
#                         dplyr::filter(xco2!='Non Significant') |>
#                         dplyr::mutate(
#                           beta_molm=(beta_line*10000)*44/24.45,
#                           fco2 = beta_molm*30/1000,
#                         ),
#                       ggplot2::aes(x=lon,y=lat,color=fco2),
#   )+
#   ggplot2::scale_color_gradient(high='yellow',low='blue')+
#   tema_mapa()+
#   ggplot2::labs(x='Longitude',y='Latitude',col=expression('FCO'[2]~'(g'~m^-2*month^-1~')'))
#
# ggplot2::ggsave('img/xco2_emission_2020.png',units="in", width=8, height=6,
#                 dpi=1000)

south_america |>
  ggplot2::ggplot()+
  ggspatial::annotation_map_tile(type = 'cartolight')+
  ggplot2::geom_sf(col='grey',fill='white')+
  ggplot2::geom_sf(data=biomes,col='black',fill='NA',linetype='dashed')+
  ggplot2::ylim(-40,6)+
  ggplot2::xlim(-80,-35)+
  ggplot2::geom_tile(data=xco2_aux_detrend_new |>
                       dplyr::mutate(
                         xco2 = dplyr::case_when(
                           beta_line > q3_xco2 ~ 'Source',
                           beta_line < q1_xco2 ~'Sink',
                           .default = 'Non Significant'
                         )
                       ) |>
                       dplyr::filter(xco2!='Non Significant')|>
                       dplyr::mutate(
                         beta_molm=(beta_error*10000)*44/24.45,
                         fco2_error = beta_molm*30/1000),
                     ggplot2::aes(x=lon,y=lat,color=fco2_error,fill=fco2_error)
  )+
  ggplot2::scale_color_gradient(high='yellow',low='blue')+
  ggplot2::scale_fill_gradient(high='yellow',low='blue')+
  tema_mapa2()+
  ggplot2::labs(x='Longitude',y='Latitude',
                col=expression('FCO'[2]~'error (g'~m^-2*month^-1~')'),
                fill=expression('FCO'[2]~'error (g'~m^-2*month^-1~')')
  )

ggplot2::ggsave('img/xco2_emissionerror_new_2020.png',units="in", width=8, height=6,
                dpi=300)

# br |>
#   ggplot2::ggplot()+
#   ggplot2::geom_sf(fill="white", color="black",
#                    size=.15, show.legend = FALSE)+
#   ggplot2::geom_point(data=xco2_aux_detrend_new |>
#                         dplyr::mutate(
#                           xco2 = dplyr::case_when(
#                             beta_line > q3_xco2 ~ 'Source',
#                             beta_line < q1_xco2 ~'Sink',
#                             .default = 'Non Significant'
#                           )
#                         ) |>
#                         dplyr::filter(xco2!='Non Significant')|>
#                         dplyr::mutate(
#                           beta_molm=(beta_error*10000)*44/24.45,
#                           fco2_error = beta_molm*30/1000,
#                         ),
#                       ggplot2::aes(x=lon,y=lat,color=fco2_error),
#   )+
#   ggplot2::scale_color_gradient(high='yellow',low='blue')+
#   tema_mapa()+
#   ggplot2::labs(x='Longitude',y='Latitude',col=expression('FCO'[2]~' error (g'~m^-2*month^-1~')'))
#
# ggplot2::ggsave('img/xco2_emissionerror_2020.png',units="in", width=8, height=6,
#                 dpi=1000)
#
# br |>
#   ggplot2::ggplot()+
#   ggplot2::geom_sf(fill="white", color="black",
#                    size=.15, show.legend = FALSE)+
#   ggplot2::geom_point(data=xco2_aux_detrend_new |>
#                         dplyr::mutate(
#                           xco2 = dplyr::case_when(
#                             beta_line > q3_xco2 ~ 'Source',
#                             beta_line < q1_xco2 ~'Sink',
#                             .default = 'Non Significant'
#                           )
#                         ) |>
#                         dplyr::filter(xco2!='Non Significant') |>
#                         dplyr::mutate(
#                           beta_molm=(10000*model_error)*44/24.45,
#                           fco2_error = beta_molm*12.01/1000,
#                         ),
#                       ggplot2::aes(x=lon,y=lat,color=fco2_error),
#   )+
#   ggplot2::scale_color_gradient(high='yellow',low='blue')+
#   tema_mapa()


####Balance

xco2_aux_detrend_new |>
  dplyr::mutate(
    xco2 = dplyr::case_when(
      beta_line > q3_xco2 ~ 'Source',
      beta_line < q1_xco2 ~'Sink',
      .default = 'Non Significant'
    )
  ) |>
  dplyr::filter(xco2!='Non Significant')|>
  dplyr::mutate(
    beta_molm=(10000*beta_line)*44/24.45,
    fco2 = beta_molm*30/1000,
    fco2=(fco2*3025000000)/1e+15
  ) |>
  dplyr::ungroup() |>
  dplyr::summarise(
    fco2=sum(fco2)
  )


xco2_aux_detrend_new |>
  dplyr::mutate(
    xco2 = dplyr::case_when(
      beta_line > q3_xco2 ~ 'Source',
      beta_line < q1_xco2 ~'Sink',
      .default = 'Non Significant'
    )
  ) |>
  dplyr::filter(xco2!='Non Significant') |>
  dplyr::mutate(
    beta_molm=beta_line*44/24.45,
    fco2 = beta_molm*30/1000)|>
  dplyr::ungroup() |>
  dplyr::summarise(
    fco2m=mean(fco2),
    fco2sd=sd(fco2)
  )

df2020 <- xco2_aux_detrend_new |> dplyr::mutate(
  xco2 = dplyr::case_when(
    beta_line > q3_xco2 ~ 'Source',
    beta_line < q1_xco2 ~'Sink',
    .default = 'Non Significant'
  )
) |>
  dplyr::filter(xco2!='Non Significant')

df2020all <- xco2_aux_detrend_new |> dplyr::mutate(
  xco2 = dplyr::case_when(
    beta_line > q3_xco2 ~ 'Source',
    beta_line < q1_xco2 ~'Sink',
    .default = 'Non Significant'
  )
)




#### 2021

xco2df_filter <- xco2df |>
  dplyr::filter(year == 2021) |>
  dplyr::filter(dist_xco2<0.25) |>
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


xco2df_filter |>
  dplyr::group_by(date) |>
  dplyr::summarise(xco2_mean=mean(xco2_mean)) |>
  ggplot2::ggplot(ggplot2::aes(x=date,y=xco2_mean )) +
  ggplot2::geom_point(shape=21,color="black",fill="gray") +
  ggplot2::geom_line(color="red") +
  ggplot2::geom_smooth(method = "lm") +
  ggpubr::stat_regline_equation(ggplot2::aes(
    label =  paste(..eq.label.., ..rr.label.., sep = "*plain(\",\")~~"))) +
  ggplot2::xlab('Date')+
  ggplot2::ylab(expression(
    'Xco'[2]~' (ppm)'
  ))+
  ggplot2::theme_bw()

ggplot2::ggsave('img/xco2_temporal_2021.png',units="in", width=8, height=6,
                dpi=300)

mod <- lm(xco2_mean~x,data =xco2df_filter |>
            dplyr::mutate(
              x = 1:dplyr::n()
            ))


xco2df_filter |>
  dplyr::mutate(
    x=1:dplyr::n(),
    xco2_est = mod$coefficients[1] + mod$coefficients[2]*x,
    delta=xco2_est - xco2_mean,
    xco2r = (mod$coefficients[1]-delta)-(mean(xco2_mean)-mod$coefficients[1])
  ) |>
  dplyr::group_by(date) |>
  dplyr::summarise(xco2_mean=mean(xco2r)) |>
  ggplot2::ggplot(ggplot2::aes(x=date,y=xco2_mean )) +
  ggplot2::geom_point(shape=21,color="black",fill="gray") +
  ggplot2::geom_line(color="red") +
  ggplot2::geom_smooth(method = "lm") +
  ggpubr::stat_regline_equation(ggplot2::aes(
    label =  paste(..eq.label.., ..rr.label.., sep = "*plain(\",\")~~"))) +
  ggplot2::xlab('Date')+
  ggplot2::ylab(expression(
    'Xco'[2][R]~' (ppm)'
  ))+
  ggplot2::theme_bw()
ggplot2::ggsave('img/xco2_detrendtemporal_2021.png',units="in", width=8, height=6,
                dpi=300)

xco2detrend <- xco2df_filter |>
  dplyr::mutate(
    x=1:dplyr::n(),
    xco2_est = mod$coefficients[1] + mod$coefficients[2]*x,
    delta=xco2_est - xco2_mean,
    xco2r = (mod$coefficients[1]-delta)-(mean(xco2_mean)-mod$coefficients[1])
  )
xco2_aux_detrend <- xco2detrend |>
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
hist(xco2detrend$xco2r)

xco2_nest <- xco2detrend |>
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

### calculating for each pixel
xco2_nest_detrend <- xco2_nest|>
  dplyr::mutate(
    beta_line = purrr::map(data,linear_reg, output="beta1"),
    p_value = purrr::map(data,linear_reg, output="p_value"),
    partial = purrr::map(data,linear_reg, output="partial"),
    n_obs = purrr::map(data,linear_reg, output="n"),
    beta_error=purrr::map(data,linear_reg,output='betaerror'),
    model_error=purrr::map(data,linear_reg,output='modelerror')
  )


#
# xco2_nest_detrend |>dplyr::filter(n_obs > 7) |>
#   tidyr::unnest(cols = c(beta_error)) |>
#   dplyr::select(lon, lat, beta_error) |>
#   ggplot2::ggplot(ggplot2::aes(x=lon, y=lat, color = beta_error)) +
#   ggplot2::geom_point()
#
# xco2_nest_detrend |>
#   dplyr::filter(n_obs > 7) |>
#   tidyr::unnest(cols = c(beta_error,model_error)) |>
#   dplyr::ungroup() |>
#   dplyr::summarise(b=mean(beta_error),
#                    m=mean(model_error))
#
#
# xco2_nest_detrend |>dplyr::filter(n_obs > 5) |>
#   tidyr::unnest(cols = c(beta_error)) |>
#   dplyr::select(lon, lat, beta_error) |>
#   ggplot2::ggplot(ggplot2::aes(x=lon, y=lat, color = beta_error)) +
#   ggplot2::geom_point()
#
# xco2_nest_detrend |>
#   dplyr::filter(n_obs > 5) |>
#   tidyr::unnest(cols = c(beta_error,model_error)) |>
#   dplyr::ungroup() |>
#   dplyr::summarise(b=mean(beta_error),
#                    m=mean(model_error))
#
# xco2_nest_detrend |>dplyr::filter(n_obs > 4) |>
#   tidyr::unnest(cols = c(beta_error)) |>
#   dplyr::select(lon, lat, beta_error) |>
#   ggplot2::ggplot(ggplot2::aes(x=lon, y=lat, color = beta_error)) +
#   ggplot2::geom_point()
#
# xco2_nest_detrend |>
#   dplyr::filter(n_obs > 4) |>
#   tidyr::unnest(cols = c(beta_error,model_error)) |>
#   dplyr::ungroup() |>
#   dplyr::summarise(b=mean(beta_error),
#                    m=mean(model_error))
#
# xco2_nest_detrend |>dplyr::filter(n_obs > 3) |>
#   tidyr::unnest(cols = c(beta_error)) |>
#   dplyr::select(lon, lat, beta_error) |>
#   ggplot2::ggplot(ggplot2::aes(x=lon, y=lat, color = beta_error)) +
#   ggplot2::geom_point()
#
# xco2_nest_detrend |>
#   dplyr::filter(n_obs > 3) |>
#   tidyr::unnest(cols = c(beta_error,model_error)) |>
#   dplyr::ungroup() |>
#   dplyr::summarise(b=mean(beta_error),
#                    m=mean(model_error))

### creating a table

xco2_aux_detrend_new <- xco2_nest_detrend |>
  dplyr::filter(n_obs > 4) |>
  tidyr::unnest(cols = c(beta_line, partial,beta_error,model_error)) |>
  dplyr::ungroup() |>
  dplyr::select(lon, lat, beta_line, partial,beta_error,model_error)

q3_xco2 <- xco2_aux_detrend_new |> dplyr::pull(beta_line) |> quantile(.75)
q1_xco2 <- xco2_aux_detrend_new |> dplyr::pull(beta_line) |> quantile(.25)

xco2_aux_detrend_new <- xco2_aux_detrend_new  |>
  dplyr::mutate(
    anomaly = partial - xco2_aux_detrend_new  |>
      dplyr::pull(partial) |>
      median(),
    Dbeta = beta_line -xco2_aux_detrend_new  |>
      dplyr::pull(beta_line) |> mean(na.rm=TRUE)
  )

q3_anom <- xco2_aux_detrend_new  |> dplyr::pull(anomaly) |> quantile(.75)
q1_anom <- xco2_aux_detrend_new  |> dplyr::pull(anomaly) |> quantile(.25)

xco2_aux_detrend_new  <- xco2_aux_detrend_new  |>
  dplyr::mutate(
    beta_index =  ifelse(beta_line >=q1_xco2, 0, 1)
  )
xco2_aux_detrend_new |>
  ggplot2::ggplot(ggplot2::aes(x=beta_line)) +
  ggplot2::geom_histogram(bins=30,
                          fill="orange",
                          color="black") +
  ggplot2::labs(x="βpixel",y="Count") +
  ggplot2::geom_vline(xintercept = q1_xco2,
                      color = "red",
                      lty=2) +
  ggplot2::geom_vline(xintercept = q3_xco2,
                      color = "red",
                      lty=2)+
  gghighlight::gghighlight(beta_line < q1_xco2 | beta_line>q3_xco2,
                           unhighlighted_params = list(
                             color = "darkgray",
                             fill = "lightgray"))+
  ggplot2::theme_minimal()



### Source and Sink Indentification

south_america |>
  ggplot2::ggplot()+
  ggspatial::annotation_map_tile(type = 'cartolight')+
  ggplot2::geom_sf(col='grey',fill='white')+
  ggplot2::geom_sf(data=biomes,col='black',fill='NA',linetype='dashed')+
  ggplot2::ylim(-40,6)+
  ggplot2::xlim(-80,-35)+
  ggplot2::geom_tile(data=xco2_aux_detrend_new |>
                       dplyr::mutate(
                         xco2 = dplyr::case_when(
                           beta_line > q3_xco2 ~ 'Source',
                           beta_line < q1_xco2 ~'Sink',
                           .default = 'Non Significant'
                         )
                       ) |>
                       dplyr::filter(xco2!='Non Significant'),
                     ggplot2::aes(x=lon,y=lat,color=xco2,fill=xco2),
  )+
  tema_mapa2()+
  ggplot2::scale_color_manual(values = c('darkgreen','darkred'))+
  ggplot2::scale_fill_manual(values = c('darkgreen','darkred'))+
  ggplot2::labs(x='Longitude',y='Latitude',
                col=expression('Xco'[2]),fill=expression('Xco'[2]))

ggplot2::ggsave('img/xco2_identification_new_2021.png',units="in", width=8, height=6,
                dpi=300)
#
# br |>
#   ggplot2::ggplot()+
#   ggplot2::geom_sf(fill="white", color="black",
#                    size=.15, show.legend = FALSE)+
#   ggplot2::geom_point(data=xco2_aux_detrend_new |>
#                         dplyr::mutate(
#                           xco2 = dplyr::case_when(
#                             beta_line > q3_xco2 ~ 'Source',
#                             beta_line < q1_xco2 ~'Sink',
#                             .default = 'Non Significant'
#                           )
#                         ) |>
#                         dplyr::filter(xco2!='Non Significant'),
#                       ggplot2::aes(x=lon,y=lat,color=xco2),
#   )+
#   tema_mapa()+
#   ggplot2::scale_color_manual(values = c('darkblue','darkred'))+
#   ggplot2::labs(x='Longitude',y='Latitude',col=expression('Xco'[2]))
#
# ggplot2::ggsave('img/xco2_identification_2021.png',units="in", width=8, height=6,
#                 dpi=1000)

xco2_aux_detrend_new |>
  dplyr::filter(beta_line<0) |>
  dplyr::summarise(
    nobs=dplyr::n()
  )


### Beta
south_america |>
  ggplot2::ggplot()+
  ggspatial::annotation_map_tile(type = 'cartolight')+
  ggplot2::geom_sf(col='grey',fill='white')+
  ggplot2::geom_sf(data=biomes,col='black',fill='NA',linetype='dashed')+
  ggplot2::ylim(-40,6)+
  ggplot2::xlim(-80,-35)+
  ggplot2::geom_tile(data=xco2_aux_detrend_new |>
                       dplyr::mutate(
                         xco2 = dplyr::case_when(
                           beta_line > q3_xco2 ~ 'Source',
                           beta_line < q1_xco2 ~'Sink',
                           .default = 'Non Significant'
                         )
                       ) |>
                       dplyr::filter(xco2!='Non Significant'),
                     ggplot2::aes(x=lon,y=lat,
                                  col=beta_line*30,
                                  fill=beta_line*30),
  )+
  ggplot2::scale_color_gradient(high='yellow',low='blue')+
  ggplot2::scale_fill_gradient(high='yellow',low='blue')+
  tema_mapa2()+
  ggplot2::labs(x='Longitude',y='Latitude',
                col=expression('β (ppm '~month^-1~')'),
                fill=expression('β (ppm '~month^-1~')')
  )
ggplot2::ggsave('img/xco2_beta_new_2021.png',units="in", width=8, height=6,
                dpi=300)


# br |>
#   ggplot2::ggplot()+
#   ggplot2::geom_sf(fill="white", color="black",
#                    size=.15, show.legend = FALSE)+
#   ggplot2::geom_point(data=xco2_aux_detrend_new |>
#                         dplyr::mutate(
#                           xco2 = dplyr::case_when(
#                             beta_line > q3_xco2 ~ 'Source',
#                             beta_line < q1_xco2 ~'Sink',
#                             .default = 'Non Significant'
#                           )
#                         ) |>
#                         dplyr::filter(xco2!='Non Significant'),
#                       ggplot2::aes(x=lon,y=lat,col=beta_line*30),
#   )+
#   ggplot2::scale_color_gradient(high='yellow',low='blue')+
#   tema_mapa()+
#   ggplot2::labs(x='Longitude',y='Latitude',col=expression('β (ppm '~month^-1~')'))
#
# ggplot2::ggsave('img/xco2_beta_2021.png',units="in", width=8, height=6,
#                 dpi=1000)


### uncertanty
south_america |>
  ggplot2::ggplot()+
  ggspatial::annotation_map_tile(type = 'cartolight')+
  ggplot2::geom_sf(col='grey',fill='white')+
  ggplot2::geom_sf(data=biomes,col='black',fill='NA',linetype='dashed')+
  ggplot2::ylim(-40,6)+
  ggplot2::xlim(-80,-35)+
  ggplot2::geom_tile(data=xco2_aux_detrend_new |>
                       dplyr::mutate(
                         xco2 = dplyr::case_when(
                           beta_line > q3_xco2 ~ 'Source',
                           beta_line < q1_xco2 ~'Sink',
                           .default = 'Non Significant'
                         )
                       ) |>
                       dplyr::filter(xco2!='Non Significant'),
                     ggplot2::aes(x=lon,y=lat,
                                  col=beta_error*30,
                                  fill=beta_error*30),
  )+
  ggplot2::scale_color_gradient(high='yellow',low='blue')+
  ggplot2::scale_fill_gradient(high='yellow',low='blue')+
  tema_mapa2()+
  ggplot2::labs(x='Longitude',y='Latitude',
                col=expression('β error (ppm '~month^-1~')'),
                fill=expression('β error (ppm '~month^-1~')')
  )
ggplot2::ggsave('img/xco2_beta_error_new_2021.png',units="in", width=8, height=6,
                dpi=300)

# br |>
#   ggplot2::ggplot()+
#   ggplot2::geom_sf(fill="white", color="black",
#                    size=.15, show.legend = FALSE)+
#   ggplot2::geom_point(data=xco2_aux_detrend_new |>
#                         dplyr::mutate(
#                           xco2 = dplyr::case_when(
#                             beta_line > q3_xco2 ~ 'Source',
#                             beta_line < q1_xco2 ~'Sink',
#                             .default = 'Non Significant'
#                           )
#                         ) |>
#                         dplyr::filter(xco2!='Non Significant'),
#                       ggplot2::aes(x=lon,y=lat,col=beta_error*30),
#   )+
#   ggplot2::scale_color_gradient(high='yellow',low='blue')+
#   tema_mapa()+
#   ggplot2::labs(x='Longitude',y='Latitude',col=expression('β error (ppm '~month^-1~')'))
#
# ggplot2::ggsave('img/xco2_betaerror_2021.png',units="in", width=8, height=6,
#                 dpi=1000)
#
# br |>
#   ggplot2::ggplot()+
#   ggplot2::geom_sf(fill="white", color="black",
#                    size=.15, show.legend = FALSE)+
#   ggplot2::geom_point(data=xco2_aux_detrend_new |>
#                         dplyr::mutate(
#                           xco2 = dplyr::case_when(
#                             beta_line > q3_xco2 ~ 'Source',
#                             beta_line < q1_xco2 ~'Sink',
#                             .default = 'Non Significant'
#                           )
#                         ) |>
#                         dplyr::filter(xco2!='Non Significant'),
#                       ggplot2::aes(x=lon,y=lat,col=model_error),
#   )+
#   ggplot2::scale_color_gradient(high='yellow',low='blue')+
#   tema_mapa()+
#   ggplot2::labs(x='Longitude',y='Latitude',col=expression('Model error (ppm '~month^-1~')'))
#
# ggplot2::ggsave('img/xco2_model_2021.png',units="in", width=8, height=6,
#                 dpi=1000)


#### CO2 emission and assimilation
south_america |>
  ggplot2::ggplot()+
  ggspatial::annotation_map_tile(type = 'cartolight')+
  ggplot2::geom_sf(col='grey',fill='white')+
  ggplot2::geom_sf(data=biomes,col='black',fill='NA',linetype='dashed')+
  ggplot2::ylim(-40,6)+
  ggplot2::xlim(-80,-35)+
  ggplot2::geom_tile(data=xco2_aux_detrend_new |>
                       dplyr::mutate(
                         xco2 = dplyr::case_when(
                           beta_line > q3_xco2 ~ 'Source',
                           beta_line < q1_xco2 ~'Sink',
                           .default = 'Non Significant'
                         )
                       ) |>
                       dplyr::filter(xco2!='Non Significant') |>
                       dplyr::mutate(
                         beta_molm=(beta_line*10000)*44/24.45,
                         fco2 = beta_molm*30/1000,
                       ),
                     ggplot2::aes(x=lon,y=lat,color=fco2,fill=fco2)
  )+
  ggplot2::scale_color_gradient(high='yellow',low='blue')+
  ggplot2::scale_fill_gradient(high='yellow',low='blue')+
  tema_mapa2()+
  ggplot2::labs(x='Longitude',y='Latitude',
                col=expression('FCO'[2]~'(g'~m^-2*month^-1~')'),
                fill=expression('FCO'[2]~'(g'~m^-2*month^-1~')')
  )

ggplot2::ggsave('img/xco2_emission_nwe_2021.png',units="in", width=8, height=6,
                dpi=300)

# br |>
#   ggplot2::ggplot()+
#   ggplot2::geom_sf(fill="white", color="black",
#                    size=.15, show.legend = FALSE)+
#   ggplot2::geom_point(data=xco2_aux_detrend_new |>
#                         dplyr::mutate(
#                           xco2 = dplyr::case_when(
#                             beta_line > q3_xco2 ~ 'Source',
#                             beta_line < q1_xco2 ~'Sink',
#                             .default = 'Non Significant'
#                           )
#                         ) |>
#                         dplyr::filter(xco2!='Non Significant') |>
#                         dplyr::mutate(
#                           beta_molm=(beta_line*10000)*44/24.45,
#                           fco2 = beta_molm*30/1000,
#                         ),
#                       ggplot2::aes(x=lon,y=lat,color=fco2),
#   )+
#   ggplot2::scale_color_gradient(high='yellow',low='blue')+
#   tema_mapa()+
#   ggplot2::labs(x='Longitude',y='Latitude',col=expression('FCO'[2]~'(g'~m^-2*month^-1~')'))
#
# ggplot2::ggsave('img/xco2_emission_2021.png',units="in", width=8, height=6,
#                 dpi=1000)

south_america |>
  ggplot2::ggplot()+
  ggspatial::annotation_map_tile(type = 'cartolight')+
  ggplot2::geom_sf(col='grey',fill='white')+
  ggplot2::geom_sf(data=biomes,col='black',fill='NA',linetype='dashed')+
  ggplot2::ylim(-40,6)+
  ggplot2::xlim(-80,-35)+
  ggplot2::geom_tile(data=xco2_aux_detrend_new |>
                       dplyr::mutate(
                         xco2 = dplyr::case_when(
                           beta_line > q3_xco2 ~ 'Source',
                           beta_line < q1_xco2 ~'Sink',
                           .default = 'Non Significant'
                         )
                       ) |>
                       dplyr::filter(xco2!='Non Significant')|>
                       dplyr::mutate(
                         beta_molm=(beta_error*10000)*44/24.45,
                         fco2_error = beta_molm*30/1000),
                     ggplot2::aes(x=lon,y=lat,color=fco2_error,fill=fco2_error)
  )+
  ggplot2::scale_color_gradient(high='yellow',low='blue')+
  ggplot2::scale_fill_gradient(high='yellow',low='blue')+
  tema_mapa2()+
  ggplot2::labs(x='Longitude',y='Latitude',
                col=expression('FCO'[2]~'error (g'~m^-2*month^-1~')'),
                fill=expression('FCO'[2]~'error (g'~m^-2*month^-1~')')
  )

ggplot2::ggsave('img/xco2_emissionerror_new_2021.png',units="in", width=8, height=6,
                dpi=300)

# br |>
#   ggplot2::ggplot()+
#   ggplot2::geom_sf(fill="white", color="black",
#                    size=.15, show.legend = FALSE)+
#   ggplot2::geom_point(data=xco2_aux_detrend_new |>
#                         dplyr::mutate(
#                           xco2 = dplyr::case_when(
#                             beta_line > q3_xco2 ~ 'Source',
#                             beta_line < q1_xco2 ~'Sink',
#                             .default = 'Non Significant'
#                           )
#                         ) |>
#                         dplyr::filter(xco2!='Non Significant')|>
#                         dplyr::mutate(
#                           beta_molm=(beta_error*10000)*44/24.45,
#                           fco2_error = beta_molm*30/1000,
#                         ),
#                       ggplot2::aes(x=lon,y=lat,color=fco2_error),
#   )+
#   ggplot2::scale_color_gradient(high='yellow',low='blue')+
#   tema_mapa()+
#   ggplot2::labs(x='Longitude',y='Latitude',col=expression('FCO'[2]~' error (g'~m^-2*month^-1~')'))
#
# ggplot2::ggsave('img/xco2_emissionerror_2021.png',units="in", width=8, height=6,
#                 dpi=1000)
#
# br |>
#   ggplot2::ggplot()+
#   ggplot2::geom_sf(fill="white", color="black",
#                    size=.15, show.legend = FALSE)+
#   ggplot2::geom_point(data=xco2_aux_detrend_new |>
#                         dplyr::mutate(
#                           xco2 = dplyr::case_when(
#                             beta_line > q3_xco2 ~ 'Source',
#                             beta_line < q1_xco2 ~'Sink',
#                             .default = 'Non Significant'
#                           )
#                         ) |>
#                         dplyr::filter(xco2!='Non Significant') |>
#                         dplyr::mutate(
#                           beta_molm=(10000*model_error)*44/24.45,
#                           fco2_error = beta_molm*12.01/1000,
#                         ),
#                       ggplot2::aes(x=lon,y=lat,color=fco2_error),
#   )+
#   ggplot2::scale_color_gradient(high='yellow',low='blue')+
#   tema_mapa()
#

####Balance

xco2_aux_detrend_new |>
  dplyr::mutate(
    xco2 = dplyr::case_when(
      beta_line > q3_xco2 ~ 'Source',
      beta_line < q1_xco2 ~'Sink',
      .default = 'Non Significant'
    )
  ) |>
  dplyr::filter(xco2!='Non Significant')|>
  dplyr::mutate(
    beta_molm=(10000*beta_line)*44/24.45,
    fco2 = beta_molm*30/1000,
    fco2=(fco2*3025000000)/1e+15
  ) |>
  dplyr::ungroup() |>
  dplyr::summarise(
    fco2=sum(fco2)
  )


xco2_aux_detrend_new |>
  dplyr::mutate(
    xco2 = dplyr::case_when(
      beta_line > q3_xco2 ~ 'Source',
      beta_line < q1_xco2 ~'Sink',
      .default = 'Non Significant'
    )
  ) |>
  dplyr::filter(xco2!='Non Significant') |>
  dplyr::mutate(
    beta_molm=beta_line*44/24.45,
    fco2 = beta_molm*30/1000)|>
  dplyr::ungroup() |>
  dplyr::summarise(
    fco2m=mean(fco2),
    fco2sd=sd(fco2)
  )

df2021 <- xco2_aux_detrend_new |> dplyr::mutate(
  xco2 = dplyr::case_when(
    beta_line > q3_xco2 ~ 'Source',
    beta_line < q1_xco2 ~'Sink',
    .default = 'Non Significant'
  )
) |>
  dplyr::filter(xco2!='Non Significant')

df2021all <- xco2_aux_detrend_new |> dplyr::mutate(
  xco2 = dplyr::case_when(
    beta_line > q3_xco2 ~ 'Source',
    beta_line < q1_xco2 ~'Sink',
    .default = 'Non Significant'
  )
)

#### 2022

xco2df_filter <- xco2df |>
  dplyr::filter(year == 2022) |>
  dplyr::filter(dist_xco2<0.25) |>
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


xco2df_filter |>
  dplyr::group_by(date) |>
  dplyr::summarise(xco2_mean=mean(xco2_mean)) |>
  ggplot2::ggplot(ggplot2::aes(x=date,y=xco2_mean )) +
  ggplot2::geom_point(shape=21,color="black",fill="gray") +
  ggplot2::geom_line(color="red") +
  ggplot2::geom_smooth(method = "lm") +
  ggpubr::stat_regline_equation(ggplot2::aes(
    label =  paste(..eq.label.., ..rr.label.., sep = "*plain(\",\")~~"))) +
  ggplot2::xlab('Date')+
  ggplot2::ylab(expression(
    'Xco'[2]~' (ppm)'
  ))+
  ggplot2::theme_bw()

ggplot2::ggsave('img/xco2_temporal_2022.png',units="in", width=8, height=6,
                dpi=300)

mod <- lm(xco2_mean~x,data =xco2df_filter |>
            dplyr::mutate(
              x = 1:dplyr::n()
            ))


xco2df_filter |>
  dplyr::mutate(
    x=1:dplyr::n(),
    xco2_est = mod$coefficients[1] + mod$coefficients[2]*x,
    delta=xco2_est - xco2_mean,
    xco2r = (mod$coefficients[1]-delta)-(mean(xco2_mean)-mod$coefficients[1])
  ) |>
  dplyr::group_by(date) |>
  dplyr::summarise(xco2_mean=mean(xco2r)) |>
  ggplot2::ggplot(ggplot2::aes(x=date,y=xco2_mean )) +
  ggplot2::geom_point(shape=21,color="black",fill="gray") +
  ggplot2::geom_line(color="red") +
  ggplot2::geom_smooth(method = "lm") +
  ggpubr::stat_regline_equation(ggplot2::aes(
    label =  paste(..eq.label.., ..rr.label.., sep = "*plain(\",\")~~"))) +
  ggplot2::xlab('Date')+
  ggplot2::ylab(expression(
    'Xco'[2][R]~' (ppm)'
  ))+
  ggplot2::theme_bw()
ggplot2::ggsave('img/xco2_detrendtemporal_2022.png',units="in", width=8, height=6,
                dpi=300)

xco2detrend <- xco2df_filter |>
  dplyr::mutate(
    x=1:dplyr::n(),
    xco2_est = mod$coefficients[1] + mod$coefficients[2]*x,
    delta=xco2_est - xco2_mean,
    xco2r = (mod$coefficients[1]-delta)-(mean(xco2_mean)-mod$coefficients[1])
  )
xco2_aux_detrend <- xco2detrend |>
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
hist(xco2detrend$xco2r)

xco2_nest <- xco2detrend |>
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

### calculating for each pixel
xco2_nest_detrend <- xco2_nest|>
  dplyr::mutate(
    beta_line = purrr::map(data,linear_reg, output="beta1"),
    p_value = purrr::map(data,linear_reg, output="p_value"),
    partial = purrr::map(data,linear_reg, output="partial"),
    n_obs = purrr::map(data,linear_reg, output="n"),
    beta_error=purrr::map(data,linear_reg,output='betaerror'),
    model_error=purrr::map(data,linear_reg,output='modelerror')
  )



# xco2_nest_detrend |>dplyr::filter(n_obs > 7) |>
#   tidyr::unnest(cols = c(beta_error)) |>
#   dplyr::select(lon, lat, beta_error) |>
#   ggplot2::ggplot(ggplot2::aes(x=lon, y=lat, color = beta_error)) +
#   ggplot2::geom_point()
#
# xco2_nest_detrend |>
#   dplyr::filter(n_obs > 7) |>
#   tidyr::unnest(cols = c(beta_error,model_error)) |>
#   dplyr::ungroup() |>
#   dplyr::summarise(b=mean(beta_error),
#                    m=mean(model_error))
#
#
# xco2_nest_detrend |>dplyr::filter(n_obs > 5) |>
#   tidyr::unnest(cols = c(beta_error)) |>
#   dplyr::select(lon, lat, beta_error) |>
#   ggplot2::ggplot(ggplot2::aes(x=lon, y=lat, color = beta_error)) +
#   ggplot2::geom_point()
#
# xco2_nest_detrend |>
#   dplyr::filter(n_obs > 5) |>
#   tidyr::unnest(cols = c(beta_error,model_error)) |>
#   dplyr::ungroup() |>
#   dplyr::summarise(b=mean(beta_error),
#                    m=mean(model_error))
#
# xco2_nest_detrend |>dplyr::filter(n_obs > 4) |>
#   tidyr::unnest(cols = c(beta_error)) |>
#   dplyr::select(lon, lat, beta_error) |>
#   ggplot2::ggplot(ggplot2::aes(x=lon, y=lat, color = beta_error)) +
#   ggplot2::geom_point()
#
# xco2_nest_detrend |>
#   dplyr::filter(n_obs > 4) |>
#   tidyr::unnest(cols = c(beta_error,model_error)) |>
#   dplyr::ungroup() |>
#   dplyr::summarise(b=mean(beta_error),
#                    m=mean(model_error))
#
# xco2_nest_detrend |>dplyr::filter(n_obs > 3) |>
#   tidyr::unnest(cols = c(beta_error)) |>
#   dplyr::select(lon, lat, beta_error) |>
#   ggplot2::ggplot(ggplot2::aes(x=lon, y=lat, color = beta_error)) +
#   ggplot2::geom_point()
#
# xco2_nest_detrend |>
#   dplyr::filter(n_obs > 3) |>
#   tidyr::unnest(cols = c(beta_error,model_error)) |>
#   dplyr::ungroup() |>
#   dplyr::summarise(b=mean(beta_error),
#                    m=mean(model_error))

### creating a table

xco2_aux_detrend_new <- xco2_nest_detrend |>
  dplyr::filter(n_obs > 4) |>
  tidyr::unnest(cols = c(beta_line, partial,beta_error,model_error)) |>
  dplyr::ungroup() |>
  dplyr::select(lon, lat, beta_line, partial,beta_error,model_error)

q3_xco2 <- xco2_aux_detrend_new |> dplyr::pull(beta_line) |> quantile(.75)
q1_xco2 <- xco2_aux_detrend_new |> dplyr::pull(beta_line) |> quantile(.25)

xco2_aux_detrend_new <- xco2_aux_detrend_new  |>
  dplyr::mutate(
    anomaly = partial - xco2_aux_detrend_new  |>
      dplyr::pull(partial) |>
      median(),
    Dbeta = beta_line -xco2_aux_detrend_new  |>
      dplyr::pull(beta_line) |> mean(na.rm=TRUE)
  )

q3_anom <- xco2_aux_detrend_new  |> dplyr::pull(anomaly) |> quantile(.75)
q1_anom <- xco2_aux_detrend_new  |> dplyr::pull(anomaly) |> quantile(.25)

xco2_aux_detrend_new  <- xco2_aux_detrend_new  |>
  dplyr::mutate(
    beta_index =  ifelse(beta_line >=q1_xco2, 0, 1)
  )
xco2_aux_detrend_new |>
  ggplot2::ggplot(ggplot2::aes(x=beta_line)) +
  ggplot2::geom_histogram(bins=30,
                          fill="orange",
                          color="black") +
  ggplot2::labs(x="βpixel",y="Count") +
  ggplot2::geom_vline(xintercept = q1_xco2,
                      color = "red",
                      lty=2) +
  ggplot2::geom_vline(xintercept = q3_xco2,
                      color = "red",
                      lty=2)+
  gghighlight::gghighlight(beta_line < q1_xco2 | beta_line>q3_xco2,
                           unhighlighted_params = list(
                             color = "darkgray",
                             fill = "lightgray"))+
  ggplot2::theme_minimal()



### Source and Sink Indentification

south_america |>
  ggplot2::ggplot()+
  ggspatial::annotation_map_tile(type = 'cartolight')+
  ggplot2::geom_sf(col='grey',fill='white')+
  ggplot2::geom_sf(data=biomes,col='black',fill='NA',linetype='dashed')+
  ggplot2::ylim(-40,6)+
  ggplot2::xlim(-80,-35)+
  ggplot2::geom_tile(data=xco2_aux_detrend_new |>
                       dplyr::mutate(
                         xco2 = dplyr::case_when(
                           beta_line > q3_xco2 ~ 'Source',
                           beta_line < q1_xco2 ~'Sink',
                           .default = 'Non Significant'
                         )
                       ) |>
                       dplyr::filter(xco2!='Non Significant'),
                     ggplot2::aes(x=lon,y=lat,color=xco2,fill=xco2),
  )+
  tema_mapa2()+
  ggplot2::scale_color_manual(values = c('darkgreen','darkred'))+
  ggplot2::scale_fill_manual(values = c('darkgreen','darkred'))+
  ggplot2::labs(x='Longitude',y='Latitude',
                col=expression('Xco'[2]),fill=expression('Xco'[2]))

ggplot2::ggsave('img/xco2_identification_new_2022.png',units="in", width=8, height=6,
                dpi=300)

# br |>
#   ggplot2::ggplot()+
#   ggplot2::geom_sf(fill="white", color="black",
#                    size=.15, show.legend = FALSE)+
#   ggplot2::geom_point(data=xco2_aux_detrend_new |>
#                         dplyr::mutate(
#                           xco2 = dplyr::case_when(
#                             beta_line > q3_xco2 ~ 'Source',
#                             beta_line < q1_xco2 ~'Sink',
#                             .default = 'Non Significant'
#                           )
#                         ) |>
#                         dplyr::filter(xco2!='Non Significant'),
#                       ggplot2::aes(x=lon,y=lat,color=xco2),
#   )+
#   tema_mapa()+
#   ggplot2::scale_color_manual(values = c('darkblue','darkred'))+
#   ggplot2::labs(x='Longitude',y='Latitude',col=expression('Xco'[2]))
#
# ggplot2::ggsave('img/xco2_identification_2022.png',units="in", width=8, height=6,
#                 dpi=1000)

xco2_aux_detrend_new |>
  dplyr::filter(beta_line<0) |>
  dplyr::summarise(
    nobs=dplyr::n()
  )


### Beta
south_america |>
  ggplot2::ggplot()+
  ggspatial::annotation_map_tile(type = 'cartolight')+
  ggplot2::geom_sf(col='grey',fill='white')+
  ggplot2::geom_sf(data=biomes,col='black',fill='NA',linetype='dashed')+
  ggplot2::ylim(-40,6)+
  ggplot2::xlim(-80,-35)+
  ggplot2::geom_tile(data=xco2_aux_detrend_new |>
                       dplyr::mutate(
                         xco2 = dplyr::case_when(
                           beta_line > q3_xco2 ~ 'Source',
                           beta_line < q1_xco2 ~'Sink',
                           .default = 'Non Significant'
                         )
                       ) |>
                       dplyr::filter(xco2!='Non Significant'),
                     ggplot2::aes(x=lon,y=lat,
                                  col=beta_line*30,
                                  fill=beta_line*30),
  )+
  ggplot2::scale_color_gradient(high='yellow',low='blue')+
  ggplot2::scale_fill_gradient(high='yellow',low='blue')+
  tema_mapa2()+
  ggplot2::labs(x='Longitude',y='Latitude',
                col=expression('β (ppm '~month^-1~')'),
                fill=expression('β (ppm '~month^-1~')')
  )
ggplot2::ggsave('img/xco2_beta_new_2022.png',units="in", width=8, height=6,
                dpi=300)


# br |>
#   ggplot2::ggplot()+
#   ggplot2::geom_sf(fill="white", color="black",
#                    size=.15, show.legend = FALSE)+
#   ggplot2::geom_point(data=xco2_aux_detrend_new |>
#                         dplyr::mutate(
#                           xco2 = dplyr::case_when(
#                             beta_line > q3_xco2 ~ 'Source',
#                             beta_line < q1_xco2 ~'Sink',
#                             .default = 'Non Significant'
#                           )
#                         ) |>
#                         dplyr::filter(xco2!='Non Significant'),
#                       ggplot2::aes(x=lon,y=lat,col=beta_line*30),
#   )+
#   ggplot2::scale_color_gradient(high='yellow',low='blue')+
#   tema_mapa()+
#   ggplot2::labs(x='Longitude',y='Latitude',col=expression('β (ppm '~month^-1~')'))
#
# ggplot2::ggsave('img/xco2_beta_2022.png',units="in", width=8, height=6,
#                 dpi=1000)
#

### uncertanty
south_america |>
  ggplot2::ggplot()+
  ggspatial::annotation_map_tile(type = 'cartolight')+
  ggplot2::geom_sf(col='grey',fill='white')+
  ggplot2::geom_sf(data=biomes,col='black',fill='NA',linetype='dashed')+
  ggplot2::ylim(-40,6)+
  ggplot2::xlim(-80,-35)+
  ggplot2::geom_tile(data=xco2_aux_detrend_new |>
                       dplyr::mutate(
                         xco2 = dplyr::case_when(
                           beta_line > q3_xco2 ~ 'Source',
                           beta_line < q1_xco2 ~'Sink',
                           .default = 'Non Significant'
                         )
                       ) |>
                       dplyr::filter(xco2!='Non Significant'),
                     ggplot2::aes(x=lon,y=lat,
                                  col=beta_error*30,
                                  fill=beta_error*30),
  )+
  ggplot2::scale_color_gradient(high='yellow',low='blue')+
  ggplot2::scale_fill_gradient(high='yellow',low='blue')+
  tema_mapa2()+
  ggplot2::labs(x='Longitude',y='Latitude',
                col=expression('β error (ppm '~month^-1~')'),
                fill=expression('β error (ppm '~month^-1~')')
  )
ggplot2::ggsave('img/xco2_beta_error_new_2022.png',units="in", width=8, height=6,
                dpi=300)

# br |>
#   ggplot2::ggplot()+
#   ggplot2::geom_sf(fill="white", color="black",
#                    size=.15, show.legend = FALSE)+
#   ggplot2::geom_point(data=xco2_aux_detrend_new |>
#                         dplyr::mutate(
#                           xco2 = dplyr::case_when(
#                             beta_line > q3_xco2 ~ 'Source',
#                             beta_line < q1_xco2 ~'Sink',
#                             .default = 'Non Significant'
#                           )
#                         ) |>
#                         dplyr::filter(xco2!='Non Significant'),
#                       ggplot2::aes(x=lon,y=lat,col=beta_error*30),
#   )+
#   ggplot2::scale_color_gradient(high='yellow',low='blue')+
#   tema_mapa()+
#   ggplot2::labs(x='Longitude',y='Latitude',col=expression('β error (ppm '~month^-1~')'))
#
# ggplot2::ggsave('img/xco2_betaerror_2022.png',units="in", width=8, height=6,
#                 dpi=1000)
#
# br |>
#   ggplot2::ggplot()+
#   ggplot2::geom_sf(fill="white", color="black",
#                    size=.15, show.legend = FALSE)+
#   ggplot2::geom_point(data=xco2_aux_detrend_new |>
#                         dplyr::mutate(
#                           xco2 = dplyr::case_when(
#                             beta_line > q3_xco2 ~ 'Source',
#                             beta_line < q1_xco2 ~'Sink',
#                             .default = 'Non Significant'
#                           )
#                         ) |>
#                         dplyr::filter(xco2!='Non Significant'),
#                       ggplot2::aes(x=lon,y=lat,col=model_error),
#   )+
#   ggplot2::scale_color_gradient(high='yellow',low='blue')+
#   tema_mapa()+
#   ggplot2::labs(x='Longitude',y='Latitude',col=expression('Model error (ppm '~month^-1~')'))
#
# ggplot2::ggsave('img/xco2_model_2022.png',units="in", width=8, height=6,
#                 dpi=1000)


#### CO2 emission and assimilation
south_america |>
  ggplot2::ggplot()+
  ggspatial::annotation_map_tile(type = 'cartolight')+
  ggplot2::geom_sf(col='grey',fill='white')+
  ggplot2::geom_sf(data=biomes,col='black',fill='NA',linetype='dashed')+
  ggplot2::ylim(-40,6)+
  ggplot2::xlim(-80,-35)+
  ggplot2::geom_tile(data=xco2_aux_detrend_new |>
                       dplyr::mutate(
                         xco2 = dplyr::case_when(
                           beta_line > q3_xco2 ~ 'Source',
                           beta_line < q1_xco2 ~'Sink',
                           .default = 'Non Significant'
                         )
                       ) |>
                       dplyr::filter(xco2!='Non Significant') |>
                       dplyr::mutate(
                         beta_molm=(beta_line*10000)*44/24.45,
                         fco2 = beta_molm*30/1000,
                       ),
                     ggplot2::aes(x=lon,y=lat,color=fco2,fill=fco2)
  )+
  ggplot2::scale_color_gradient(high='yellow',low='blue')+
  ggplot2::scale_fill_gradient(high='yellow',low='blue')+
  tema_mapa2()+
  ggplot2::labs(x='Longitude',y='Latitude',
                col=expression('FCO'[2]~'(g'~m^-2*month^-1~')'),
                fill=expression('FCO'[2]~'(g'~m^-2*month^-1~')')
  )

ggplot2::ggsave('img/xco2_emission_nwe_2022.png',units="in", width=8, height=6,
                dpi=300)

# br |>
#   ggplot2::ggplot()+
#   ggplot2::geom_sf(fill="white", color="black",
#                    size=.15, show.legend = FALSE)+
#   ggplot2::geom_point(data=xco2_aux_detrend_new |>
#                         dplyr::mutate(
#                           xco2 = dplyr::case_when(
#                             beta_line > q3_xco2 ~ 'Source',
#                             beta_line < q1_xco2 ~'Sink',
#                             .default = 'Non Significant'
#                           )
#                         ) |>
#                         dplyr::filter(xco2!='Non Significant') |>
#                         dplyr::mutate(
#                           beta_molm=(beta_line*10000)*44/24.45,
#                           fco2 = beta_molm*30/1000,
#                         ),
#                       ggplot2::aes(x=lon,y=lat,color=fco2),
#   )+
#   ggplot2::scale_color_gradient(high='yellow',low='blue')+
#   tema_mapa()+
#   ggplot2::labs(x='Longitude',y='Latitude',col=expression('FCO'[2]~'(g'~m^-2*month^-1~')'))
#
# ggplot2::ggsave('img/xco2_emission_2022.png',units="in", width=8, height=6,
#                 dpi=1000)

south_america |>
  ggplot2::ggplot()+
  ggspatial::annotation_map_tile(type = 'cartolight')+
  ggplot2::geom_sf(col='grey',fill='white')+
  ggplot2::geom_sf(data=biomes,col='black',fill='NA',linetype='dashed')+
  ggplot2::ylim(-40,6)+
  ggplot2::xlim(-80,-35)+
  ggplot2::geom_tile(data=xco2_aux_detrend_new |>
                       dplyr::mutate(
                         xco2 = dplyr::case_when(
                           beta_line > q3_xco2 ~ 'Source',
                           beta_line < q1_xco2 ~'Sink',
                           .default = 'Non Significant'
                         )
                       ) |>
                       dplyr::filter(xco2!='Non Significant')|>
                       dplyr::mutate(
                         beta_molm=(beta_error*10000)*44/24.45,
                         fco2_error = beta_molm*30/1000),
                     ggplot2::aes(x=lon,y=lat,color=fco2_error,fill=fco2_error)
  )+
  ggplot2::scale_color_gradient(high='yellow',low='blue')+
  ggplot2::scale_fill_gradient(high='yellow',low='blue')+
  tema_mapa2()+
  ggplot2::labs(x='Longitude',y='Latitude',
                col=expression('FCO'[2]~'error (g'~m^-2*month^-1~')'),
                fill=expression('FCO'[2]~'error (g'~m^-2*month^-1~')')
  )

ggplot2::ggsave('img/xco2_emissionerror_new_2022.png',units="in", width=8, height=6,
                dpi=300)

# br |>
#   ggplot2::ggplot()+
#   ggplot2::geom_sf(fill="white", color="black",
#                    size=.15, show.legend = FALSE)+
#   ggplot2::geom_point(data=xco2_aux_detrend_new |>
#                         dplyr::mutate(
#                           xco2 = dplyr::case_when(
#                             beta_line > q3_xco2 ~ 'Source',
#                             beta_line < q1_xco2 ~'Sink',
#                             .default = 'Non Significant'
#                           )
#                         ) |>
#                         dplyr::filter(xco2!='Non Significant')|>
#                         dplyr::mutate(
#                           beta_molm=(beta_error*10000)*44/24.45,
#                           fco2_error = beta_molm*30/1000,
#                         ),
#                       ggplot2::aes(x=lon,y=lat,color=fco2_error),
#   )+
#   ggplot2::scale_color_gradient(high='yellow',low='blue')+
#   tema_mapa()+
#   ggplot2::labs(x='Longitude',y='Latitude',col=expression('FCO'[2]~' error (g'~m^-2*month^-1~')'))
#
# ggplot2::ggsave('img/xco2_emissionerror_2022.png',units="in", width=8, height=6,
#                 dpi=1000)
#
# br |>
#   ggplot2::ggplot()+
#   ggplot2::geom_sf(fill="white", color="black",
#                    size=.15, show.legend = FALSE)+
#   ggplot2::geom_point(data=xco2_aux_detrend_new |>
#                         dplyr::mutate(
#                           xco2 = dplyr::case_when(
#                             beta_line > q3_xco2 ~ 'Source',
#                             beta_line < q1_xco2 ~'Sink',
#                             .default = 'Non Significant'
#                           )
#                         ) |>
#                         dplyr::filter(xco2!='Non Significant') |>
#                         dplyr::mutate(
#                           beta_molm=(10000*model_error)*44/24.45,
#                           fco2_error = beta_molm*12.01/1000,
#                         ),
#                       ggplot2::aes(x=lon,y=lat,color=fco2_error),
#   )+
#   ggplot2::scale_color_gradient(high='yellow',low='blue')+
#   tema_mapa()


####Balance

xco2_aux_detrend_new |>
  dplyr::mutate(
    xco2 = dplyr::case_when(
      beta_line > q3_xco2 ~ 'Source',
      beta_line < q1_xco2 ~'Sink',
      .default = 'Non Significant'
    )
  ) |>
  dplyr::filter(xco2!='Non Significant')|>
  dplyr::mutate(
    beta_molm=(10000*beta_line)*44/24.45,
    fco2 = beta_molm*30/1000,
    fco2=(fco2*3025000000)/1e+15
  ) |>
  dplyr::ungroup() |>
  dplyr::summarise(
    fco2=sum(fco2)
  )


xco2_aux_detrend_new |>
  dplyr::mutate(
    xco2 = dplyr::case_when(
      beta_line > q3_xco2 ~ 'Source',
      beta_line < q1_xco2 ~'Sink',
      .default = 'Non Significant'
    )
  ) |>
  dplyr::filter(xco2!='Non Significant') |>
  dplyr::mutate(
    beta_molm=beta_line*44/24.45,
    fco2 = beta_molm*30/1000)|>
  dplyr::ungroup() |>
  dplyr::summarise(
    fco2m=mean(fco2),
    fco2sd=sd(fco2)
  )

df2022 <- xco2_aux_detrend_new |> dplyr::mutate(
  xco2 = dplyr::case_when(
    beta_line > q3_xco2 ~ 'Source',
    beta_line < q1_xco2 ~'Sink',
    .default = 'Non Significant'
  )
) |>
  dplyr::filter(xco2!='Non Significant')

df2022all <- xco2_aux_detrend_new |> dplyr::mutate(
  xco2 = dplyr::case_when(
    beta_line > q3_xco2 ~ 'Source',
    beta_line < q1_xco2 ~'Sink',
    .default = 'Non Significant'
  )
)


####

df2015all |>
  dplyr::mutate(year=2015) |>
  rbind(df2016all |> dplyr::mutate(year=2016)) |>
  rbind(df2017all|> dplyr::mutate(year=2017)) |>
  rbind(df2018all|> dplyr::mutate(year=2018)) |>
  rbind(df2019all|> dplyr::mutate(year=2019)) |>
  rbind(df2020all|> dplyr::mutate(year=2020)) |>
  rbind(df2021all|> dplyr::mutate(year=2021)) |>
  rbind(df2022all|> dplyr::mutate(year=2022)) |>
  ggplot2::ggplot(ggplot2::aes(x=beta_line*30, y=as.character(year),
                               col=as.character(year), fill=as.character(year)))+
  ggridges::geom_density_ridges(alpha=.2)+
  ggplot2::geom_vline(xintercept =df2015all |>
                        dplyr::mutate(year=2015) |>
                        rbind(df2016all |> dplyr::mutate(year=2016)) |>
                        rbind(df2017all|> dplyr::mutate(year=2017)) |>
                        rbind(df2018all|> dplyr::mutate(year=2018)) |>
                        rbind(df2019all|> dplyr::mutate(year=2019)) |>
                        rbind(df2020all|> dplyr::mutate(year=2020)) |>
                        rbind(df2021all|> dplyr::mutate(year=2021)) |>
                        rbind(df2022all|> dplyr::mutate(year=2022)) |>
                        dplyr::summarise(b=median(beta_line)) |>
                        dplyr::pull(b)*30, linetype='dashed', color='black')+
  ggplot2::scale_color_viridis_d()+
  ggplot2::scale_fill_viridis_d()+
  #ggplot2::facet_wrap(~xco2,scales = 'free_x')+
  ggplot2::theme_bw()+
  ggplot2::labs(x=expression('βxco'[2]~'(ppm month'^-1~')'),y='',col='',fill='')
ggplot2::ggsave('img/beta_density_year.png',units="in", width=8, height=6,
                dpi=300)

####

dfall <- df2015all |>
  dplyr::mutate(year=2015) |>
  rbind(df2016all |> dplyr::mutate(year=2016)) |>
  rbind(df2017all|> dplyr::mutate(year=2017)) |>
  rbind(df2018all|> dplyr::mutate(year=2018)) |>
  rbind(df2019all|> dplyr::mutate(year=2019)) |>
  rbind(df2020all|> dplyr::mutate(year=2020)) |>
  rbind(df2021all|> dplyr::mutate(year=2021)) |>
  rbind(df2022all|> dplyr::mutate(year=2022))



df <- df2015 |>
  dplyr::mutate(year=2015) |>
  rbind(df2016 |> dplyr::mutate(year=2016)) |>
  rbind(df2017|> dplyr::mutate(year=2017)) |>
  rbind(df2018|> dplyr::mutate(year=2018)) |>
  rbind(df2019|> dplyr::mutate(year=2019)) |>
  rbind(df2020|> dplyr::mutate(year=2020)) |>
  rbind(df2021|> dplyr::mutate(year=2021)) |>
  rbind(df2022|> dplyr::mutate(year=2022)) |>
  dplyr::mutate(
    beta_molm=(10000*beta_line)*44/24.45,
    beta_fco2 = beta_molm*30/1000,
    beta_molm_erro=(10000*beta_error)*44/24.45,
    betaerror_fco2 = beta_molm_erro*30/1000,
    model_molm=(10000*model_error)*44/24.45,
    model_fco2=model_molm*30/1000
  )

df |> dplyr::glimpse()


for(i in 15:21){
  xco2_geo <- df |>
    dplyr::filter(
      year == as.numeric(paste0(20,i)))
  print(xco2_geo)
  write.csv(xco2_geo, paste0('data/xco2_siggeo_20',i,'.csv'))
}

write.csv(df,'data/beta_final.csv')


df |>
  dplyr::group_by(year) |>
  dplyr::summarise(
    fco2=mean(beta_fco2),
    fco2err=mean(betaerror_fco2)
  ) |>
  ggplot2::ggplot(ggplot2::aes(x=year,y=fco2,
                               ymax=fco2+fco2err,
                               ymin=fco2))+
  ggplot2::geom_col()+
  ggplot2::geom_errorbar()+
  ggplot2::labs(
    x='',
    y=expression('FCO'[2]~ ' (g CO'[2]~'m'^-2~'month'^-1~')')
  )+
  ggplot2::theme_bw()

ggplot2::ggsave('img/mean_fco2_year.png',units="in", width=8, height=6,
                dpi=300)
df |>
  dplyr::group_by(year) |>
  dplyr::summarise(
    fco2=mean(beta_fco2),
    fco2err=mean(betaerror_fco2),
    balance = sum(12*beta_fco2*(55000^2))/1e15,
    mean_balance = 12*fco2*8510000000000/1e15
  ) |>
  ggplot2::ggplot(ggplot2::aes(x=year,y=balance))+
  ggplot2::geom_col()+
  #ggplot2::geom_errorbar()+
  ggplot2::labs(
    x='',
    y=expression('CO'[2]~' Balance (Pg)')
  )+
  ggplot2::theme_bw()


df |>
  dplyr::group_by(year) |>
  dplyr::summarise(
    fco2=mean(beta_fco2),
    fco2err=mean(betaerror_fco2),
    balance = sum(12*beta_fco2*(55000^2))/1e15,
    mean_balance = 12*fco2*8510000000000/1e15,
    mean_error = 12*fco2err*8510000000000/1e15
  ) |>
  ggplot2::ggplot(ggplot2::aes(x=year,y=mean_balance,
                               ymax=mean_balance+mean_error,
                               ymin=mean_balance))+
  ggplot2::geom_col()+
  ggplot2::geom_errorbar()+
  ggplot2::labs(
    x='',
    y=expression('CO'[2]~' Balance (Pg)')
  )+
  ggplot2::theme_bw()

ggplot2::ggsave('img/br_balance_year.png',units="in", width=8, height=6,
                dpi=300)


dfall |>
  dplyr::group_by(year) |>
  dplyr::summarise(
    x = min(beta_line)
  )


####

south_america |>
  ggplot2::ggplot()+
  ggspatial::annotation_map_tile(type = 'cartolight')+
  ggplot2::geom_sf(col='grey',fill='white')+
  ggplot2::geom_sf(data=biomes,col='black',fill='NA',linetype='dashed')+
  ggplot2::ylim(-35,5.5)+
  ggplot2::xlim(-75,-35)+
  ggplot2::geom_tile(data=df |>
                       dplyr::filter(xco2!='Non Significant'),
                     ggplot2::aes(x=lon,y=lat,color=xco2,fill=xco2),
  )+
  ggplot2::facet_wrap(~year)+
  ggplot2::theme(axis.text = ggplot2::element_text(size=12),
                 axis.title = ggplot2::element_text(size=20),
                 text = ggplot2::element_text(size=20)
                 )+
  tema_mapa2()+
  ggplot2::scale_color_manual(values = c('darkgreen','darkred'))+
  ggplot2::scale_fill_manual(values = c('darkgreen','darkred'))+
  ggplot2::labs(x='Longitude',y='Latitude',
                col=expression('Xco'[2]),fill=expression('Xco'[2]))

ggplot2::ggsave('img/xco2_identification_all.png',units="in", width=11, height=11,
                dpi=300)


south_america |>
  ggplot2::ggplot()+
  ggspatial::annotation_map_tile(type = 'cartolight')+
  ggplot2::geom_sf(col='grey',fill='white')+
  ggplot2::geom_sf(data=biomes,col='black',fill='NA',linetype='dashed')+
  ggplot2::ylim(-35,5.5)+
  ggplot2::xlim(-75,-35)+
  ggplot2::geom_tile(data=df |>
                       dplyr::filter(xco2!='Non Significant'),
                     ggplot2::aes(x=lon,y=lat,color=beta_fco2,fill=beta_fco2),
  )+
  ggplot2::facet_wrap(~year)+
  ggplot2::theme(axis.text = ggplot2::element_text(size=12),
                 axis.title = ggplot2::element_text(size=20),
                 text = ggplot2::element_text(size=20)
  )+
  tema_mapa2()+
  ggplot2::scale_color_gradient(high='yellow',low='blue')+
  ggplot2::scale_fill_gradient(high='yellow',low='blue')+
  ggplot2::labs(x='Longitude',y='Latitude',
                col=expression('FCO'[2]),
                fill=expression('FCO'[2])
  )

ggplot2::ggsave('img/xco2_fco2_all.png',units="in", width=11, height=11,
                dpi=300)


south_america |>
  ggplot2::ggplot()+
  ggspatial::annotation_map_tile(type = 'cartolight')+
  ggplot2::geom_sf(col='grey',fill='white')+
  ggplot2::geom_sf(data=br,col='red',fill='NA')+
  ggplot2::ylim(-35,5.5)+
  ggplot2::xlim(-75,-35)+
  ggplot2::geom_tile(data=df |>
                       dplyr::filter(xco2!='Non Significant'),
                     ggplot2::aes(x=lon,y=lat,color=betaerror_fco2,fill=betaerror_fco2),
  )+
  ggplot2::facet_wrap(~year)+
  ggplot2::theme(axis.text = ggplot2::element_text(size=12),
                 axis.title = ggplot2::element_text(size=20),
                 text = ggplot2::element_text(size=20)
  )+
  tema_mapa2()+
  ggplot2::scale_color_gradient(high='yellow',low='blue')+
  ggplot2::scale_fill_gradient(high='yellow',low='blue')+
  ggplot2::labs(x='Longitude',y='Latitude',
                col=expression(epsilon~'FCO'[2]),
                fill=expression(epsilon~'FCO'[2])
  )

ggplot2::ggsave('img/xco2_fco2_erro_all.png',units="in", width=11, height=11,
                dpi=300)



biomes |>
  ggplot2::ggplot()+
  ggspatial::annotation_map_tile(type = 'cartolight')+
  ggplot2::geom_sf(col='grey',fill='white')+
  #ggplot2::geom_sf(data=br,col='red',fill='NA')+
  ggplot2::ylim(-35,5.5)+
  ggplot2::xlim(-75,-35)+
  ggplot2::geom_tile(data=df |>
                       dplyr::filter(xco2!='Non Significant'),
                     ggplot2::aes(x=lon,y=lat,color=betaerror_fco2,fill=betaerror_fco2),
  )+
  ggplot2::facet_wrap(~year)+
  ggplot2::theme(axis.text = ggplot2::element_text(size=12),
                 axis.title = ggplot2::element_text(size=20),
                 text = ggplot2::element_text(size=20)
  )+
  tema_mapa2()+
  ggplot2::scale_color_gradient(high='yellow',low='blue')+
  ggplot2::scale_fill_gradient(high='yellow',low='blue')+
  ggplot2::labs(x='Longitude',y='Latitude',
                col=expression(epsilon~'FCO'[2]),
                fill=expression(epsilon~'FCO'[2])
  )

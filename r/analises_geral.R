source('r/linear_func.R')
source('r/mapa.R')
source('r/detrend_fun.R')

br <- geobr::read_country(showProgress = FALSE)
xco2df <- readr::read_rds('data/xco2_0.5deg_full_trend.rds')


xco2df |>
  dplyr::filter(dist_xco2<0.25) |>
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
  dplyr::filter(dist_xco2<0.25) |>
  dplyr::group_by(date) |>
  dplyr::summarise(xco2_mean=mean(xco2)) |>
  ggplot2::ggplot(ggplot2::aes(x=date,y=xco2_mean)) +
  ggplot2::geom_point(shape=21,color="black",fill="gray") +
  #ggplot2::geom_line(color="red") +
  ggplot2::geom_smooth(method = "lm") +
  ggpubr::stat_regline_equation(ggplot2::aes(
    label =  paste(..eq.label.., ..rr.label.., sep = "*plain(\",\")~~"))) +
  ggplot2::theme_bw()

xco2df |>
  dplyr::filter(dist_xco2<0.25) |>
  dplyr::group_by(date) |>
  dplyr::summarise(xco2_mean=mean(xco2)) |>
  ggplot2::ggplot(ggplot2::aes(x=date,y=xco2_mean)) +
  ggplot2::geom_point(shape=21,color="black",fill="gray") +
  #ggplot2::geom_line(color="red") +
  ggplot2::geom_smooth(method = "lm") +
  ggplot2::facet_wrap(~lubridate::year(date),scales ='free')+
  ggpubr::stat_regline_equation(ggplot2::aes(
    label =  paste(..eq.label.., ..rr.label.., sep = "*plain(\",\")~~"))) +
  ggplot2::theme_bw()

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
  ggplot2::theme_bw()

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
  ggplot2::theme_bw()


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
  dplyr::summarise(xco2 = mean(xco2_mean, na.rm=TRUE)) |>
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
    anomaly =  partial - xco2_aux_detrend_new  |>
      dplyr::pull(partial) |>
      mean(),
    Dbeta = beta_line -xco2_aux_detrend_new  |>
      dplyr::pull(beta_line) |> mean(na.rm=TRUE)
  )

q3_anom <- xco2_aux_detrend_new  |> dplyr::pull(anomaly) |> quantile(.75)

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
  tema_mapa()




### Beta

br |>
  ggplot2::ggplot()+
  ggplot2::geom_sf(fill="white", color="black",
                   size=.15, show.legend = FALSE)+
  ggplot2::geom_point(data=xco2_aux_detrend_new,
                      ggplot2::aes(x=lon,y=lat,col=beta_line),
  )+
  ggplot2::scale_color_gradient(high='yellow',low='blue')+
  tema_mapa()


### uncertanty
br |>
  ggplot2::ggplot()+
  ggplot2::geom_sf(fill="white", color="black",
                   size=.15, show.legend = FALSE)+
  ggplot2::geom_point(data=xco2_aux_detrend_new,
                      ggplot2::aes(x=lon,y=lat,col=beta_error),
  )+
  ggplot2::scale_color_gradient(high='yellow',low='blue')+
  tema_mapa()

br |>
  ggplot2::ggplot()+
  ggplot2::geom_sf(fill="white", color="black",
                   size=.15, show.legend = FALSE)+
  ggplot2::geom_point(data=xco2_aux_detrend_new,
                      ggplot2::aes(x=lon,y=lat,col=model_error),
  )+
  ggplot2::scale_color_gradient(high='yellow',low='blue')+
  tema_mapa()


#### CO2 emission and assimilation


br |>
  ggplot2::ggplot()+
  ggplot2::geom_sf(fill="white", color="black",
                   size=.15, show.legend = FALSE)+
  ggplot2::geom_point(data=xco2_aux_detrend_new |>
                        dplyr::mutate(
                          beta_molm=beta_line*44/24.45,
                          fco2 = beta_molm*12.01,
                        ),
                      ggplot2::aes(x=lon,y=lat,color=fco2),
  )+
  ggplot2::scale_color_gradient(high='yellow',low='blue')+
  tema_mapa()

br |>
  ggplot2::ggplot()+
  ggplot2::geom_sf(fill="white", color="black",
                   size=.15, show.legend = FALSE)+
  ggplot2::geom_point(data=xco2_aux_detrend_new |>
                        dplyr::mutate(
                          beta_molm=beta_error*44/24.45,
                          fco2_error = beta_molm*12.01,
                        ),
                      ggplot2::aes(x=lon,y=lat,color=fco2_error),
  )+
  ggplot2::scale_color_gradient(high='yellow',low='blue')+
  tema_mapa()

br |>
  ggplot2::ggplot()+
  ggplot2::geom_sf(fill="white", color="black",
                   size=.15, show.legend = FALSE)+
  ggplot2::geom_point(data=xco2_aux_detrend_new |>
                        dplyr::mutate(
                          beta_molm=model_error*44/24.45,
                          fco2_error = beta_molm*12.01,
                        ),
                      ggplot2::aes(x=lon,y=lat,color=fco2_error),
  )+
  ggplot2::scale_color_gradient(high='yellow',low='blue')+
  tema_mapa()


####Balance

xco2_aux_detrend_new |>
  dplyr::mutate(
    beta_molm=beta_line*44/24.45,
    fco2 = beta_molm*12.01,
    fco2=(fco2*3.086358025e+15)/1e+15
  ) |>
  dplyr::ungroup() |>
  dplyr::summarise(
    fco2=sum(fco2)
  )

##### anomaly
br |>
  ggplot2::ggplot()+
  ggplot2::geom_sf(fill="white", color="black",
                   size=.15, show.legend = FALSE)+
  ggplot2::geom_point(data=xco2_aux_detrend_new,
                      ggplot2::aes(x=lon,y=lat,col=anomaly),
  )+
  ggplot2::scale_color_gradient(high='yellow',low='blue')+
  tema_mapa()

br |>
  ggplot2::ggplot()+
  ggplot2::geom_sf(fill="white", color="black",
                   size=.15, show.legend = FALSE)+
  ggplot2::geom_point(data=xco2_aux_detrend_new |>
                        dplyr::mutate(
                          beta_molm=anomaly*44/24.45,
                          fco2 = beta_molm*12.01,
                        ),
                      ggplot2::aes(x=lon,y=lat,color=fco2),
  )+
  tema_mapa()


xco2_aux_detrend_new |>
  dplyr::mutate(
    beta_molm=anomaly*44/24.45,
    fco2 = beta_molm*12.01,
    fco2=(fco2*3.086358025e+15)/1e+15
  ) |>
  dplyr::ungroup() |>
  dplyr::summarise(
    fco2=sum(fco2)
  )

df <- read.csv('data/beta_final.csv')
xco2df <- readr::read_rds('data/xco2_0.5deg_full_trend.rds')
df |>
  dplyr::group_by(lon,lat) |>
  dplyr::filter(lon!=-72.5,lat!=-8.5)
  dplyr::mutate(
    ct=cor.test(year,beta_line)
  )


df |>
  dplyr::group_by(
    lon,lat
  ) |>
  dplyr::mutate(
    n_obs = dplyr::n()
  ) |>
  dplyr::filter(
    n_obs >=3
  ) |>
  dplyr::mutate(
    ct = cor.test(year,beta_line)[4],
    ct_pvalue = cor.test(year,beta_line)[3]
  ) |>
  dplyr::filter(ct_pvalue<=0.01)


df |>
  dplyr::filter(lon == -50, lat ==-19)



xco2df_filter <- xco2df |>
  dplyr::filter(dist_xco2<0.25) |>
  dplyr::mutate(
    lon = lon_grid,
    lat = lat_grid,
  ) |>
  dplyr::filter(lon == -50, lat ==-19) |>
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

years_n <- c(2015:2022)

for(i in 1:length(years_n)){
  mod <- lm(xco2_mean~x,data =xco2df_filter |>
              dplyr::filter(year == years_n[i]) |>
              dplyr::mutate(
                x = 1:dplyr::n()
              ))
  plot_reg <- xco2df_filter |>
    dplyr::filter(year == years_n[i]) |>
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
    ggplot2::facet_wrap(~lubridate::year(date),scales = 'free_x')+
    ggplot2::theme_bw()+
    ggplot2::xlab('Date')+
    ggplot2::ylab(expression(
      'Xco'[2][R]~' (ppm)'
    ))

  print(plot_reg)
}


mod <- lm(xco2_mean~x,data =xco2df_filter |>
            dplyr::mutate(
              x = 1:dplyr::n()
            ))


xco2df_filter |>
  dplyr::filter(lubridate::year(date)<2023) |>
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
  ggplot2::facet_wrap(~lubridate::year(date),scales = 'free_x')+
  ggplot2::theme_bw()+
  ggplot2::xlab('Month')+
  ggplot2::ylab(expression(
    'Xco'[2][R]~' (ppm)'
  ))
ggplot2::ggsave('img/rationality_beta.png',units="in", width=8, height=6,
                dpi=300)



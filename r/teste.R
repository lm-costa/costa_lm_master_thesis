xco2df <- readr::read_rds('data/xco2_025_full_trend.rds')
xco2df |>
  dplyr::filter(dist_xco2<0.15) |>
  dplyr::group_by(date) |>
  dplyr::summarise(xco2_mean=mean(xco2)) |>
  ggplot2::ggplot(ggplot2::aes(x=date,y=xco2_mean )) +
  ggplot2::geom_point(shape=21,color="black",fill="gray") +
  ggplot2::geom_line(color="red") +
  ggplot2::geom_smooth(method = "lm") +
  ggpubr::stat_regline_equation(ggplot2::aes(
    label =  paste(..eq.label.., ..rr.label.., sep = "*plain(\",\")~~"))) +
  ggplot2::theme_bw()



xco2agg <- xco2df |>
  dplyr::filter(dist_xco2<0.15) |>
  dplyr::group_by(lon_grid,lat_grid,year,month) |>
  dplyr::summarise(
    xco2 = mean(xco2),
    xco2_sd = sd(xco2),
    uncertanty_m = mean(uncertanty),
    nobs= dplyr::n()
  ) |>
  dplyr::mutate(
    date = lubridate::make_date(year,month,'1')
  )



xco2agg |>
  dplyr::group_by(date) |>
  dplyr::summarise(xco2_mean = mean(xco2, na.rm =TRUE)) |>
  ggplot2::ggplot(ggplot2::aes(x=date,y=xco2_mean )) +
  ggplot2::geom_point(shape=21,color="black",fill="gray") +
  ggplot2::geom_line(color="red") +
  ggplot2::geom_smooth(method = "lm") +
  ggpubr::stat_regline_equation(ggplot2::aes(
    label =  paste(..eq.label.., ..rr.label.., sep = "*plain(\",\")~~"))) +
  ggplot2::theme_bw()

xco2agg2 <- xco2df |>
  dplyr::filter(dist_xco2<0.15) |>
  dplyr::group_by(lon_grid,lat_grid,date) |>
  dplyr::summarise(
    xco2 = mean(xco2),
    xco2_sd = sd(xco2),
    uncertanty_m = mean(uncertanty),
    nobs= dplyr::n()
  )

xco2agg2 |>
  dplyr::mutate(
    year=lubridate::year(date),
    month=lubridate::month(date),
    date = lubridate::make_date(year,month,'1')
  ) |>
  dplyr::group_by(date) |>
  dplyr::summarise(xco2_mean = mean(xco2, na.rm =TRUE)) |>
  ggplot2::ggplot(ggplot2::aes(x=date,y=xco2_mean )) +
  ggplot2::geom_point(shape=21,color="black",fill="gray") +
  ggplot2::geom_line(color="red") +
  ggplot2::geom_smooth(method = "lm") +
  ggpubr::stat_regline_equation(ggplot2::aes(
    label =  paste(..eq.label.., ..rr.label.., sep = "*plain(\",\")~~"))) +
  ggplot2::theme_bw()


xco2agg2 |>
  dplyr::filter(lon_grid == -73, lat_grid == -6.5) |>
  dplyr::mutate(
    year=lubridate::year(date),
    month=lubridate::month(date),
    date = lubridate::make_date(year,month,'1')
  ) |>
  dplyr::group_by(date) |>
  dplyr::summarise(xco2_mean = mean(xco2, na.rm =TRUE)) |>
  ggplot2::ggplot(ggplot2::aes(x=date,y=xco2_mean )) +
  ggplot2::geom_point(shape=21,color="black",fill="gray") +
  ggplot2::geom_line(color="red") +
  ggplot2::geom_smooth(method = "lm") +
  ggpubr::stat_regline_equation(ggplot2::aes(
    label =  paste(..eq.label.., ..rr.label.., sep = "*plain(\",\")~~"))) +
  ggplot2::theme_bw()


xco2_aux  <- xco2agg2 |>
  dplyr::ungroup() |>
  dplyr::group_by(date) |>
  dplyr::summarise(xco2 = mean(xco2, na.rm=TRUE))

mod <- lm(xco2 ~date,data=xco2_aux)

br <- mod$coefficients[2]
ep <- summary(mod)$coefficients[2,2]

libr <- br - ep
lsbr <- br + ep


broom::augment(mod, interval="confidence")
plot(mod)
cooks.distance(mod)



xco2_nest <- xco2agg2 |>
  tibble::as_tibble() |>
  dplyr::mutate(year =lubridate::year(date),
                quarter = lubridate::quarter(date),
                quarter_year = lubridate::make_date(year, quarter, 1)) |>
  dplyr::group_by(lon_grid, lat_grid,date) |>
  dplyr::summarise(xco2_mean = mean(xco2, na.rm=TRUE)) |>
  dplyr::mutate(
    id_time = date
  ) |>
  dplyr::group_by(lon_grid,lat_grid) |>
  tidyr::nest()




linear_reg <- function(df,output="beta1"){
  # Modelo para cada pixel
  modelo <- lm(xco2_mean ~ date, data=df)
  beta_1 <- c(summary(modelo)$coefficients[2])

  # Definindo o modelo
  if(output=="beta1"){
    return(beta_1)
  }

  # Salvando o valor P
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

  # Criando gráfico
  if(output=="plot"){
    plot <- df |>
      ggplot2::ggplot(ggplot2::aes(x=date,y=xco2_mean)) +
      ggplot2::geom_point() +
      ggplot2::theme_bw()
    return(plot)
  }
  if(output=="hist"){
    hist <- df |>
      ggplot2::ggplot(ggplot2::aes(x=xco2_mean, y=..density..)) +
      ggplot2::geom_histogram(bins=10, color="black", fill="lightgray") +
      ggplot2::geom_density()+
      ggplot2::theme_bw()
    return(hist)
  }

  # Anomalia é o Xco2 do regional menos o Xco2 do pixel, melhor é o contrário.
  if(output == "partial"){
    partial <- df |>
      dplyr::summarise(xco2 = mean(xco2_mean), na.mr=TRUE) |>
      dplyr::pull(xco2)
    return(partial)
  }

  if(output == "n"){
    return(nrow(df))
  }
}


xco2_nest <- xco2_nest |>
  dplyr::mutate(
    beta_line = purrr::map(data,linear_reg, output="beta1"),
    p_value = purrr::map(data,linear_reg, output="p_value"),
    partial = purrr::map(data,linear_reg, output="partial"),
    n_obs = purrr::map(data,linear_reg, output="n")
    #plot = purrr::map(data,linear_reg, output="plot"),
    #hist = purrr::map(data,linear_reg, output="hist")
  )


xco2_nest |>
  # dplyr::filter(p_value < 0.05) |>
  dplyr::filter(n_obs > 7) |>
  # dplyr::mutate(class = ifelse(beta_line > limite_inferior_beta_regional,
  #                              1,ifelse(beta_line < limite_inferior_beta_regional, -1, 0))
  #               ) |>
  dplyr::select(lon_grid, lat_grid, n_obs) |>
  ggplot2::ggplot(ggplot2::aes(x=lon_grid, y=lat_grid, color = n_obs)) +
  ggplot2::geom_point()



xco2_aux <- xco2_nest |>
  # dplyr::filter(region == "norte") |>
  # dplyr::filter(p_value < 0.05) |>
  dplyr::filter(n_obs > 7) |>
  tidyr::unnest(cols = c(beta_line, partial)) |>
  dplyr::ungroup() |>
  dplyr::select(lon_grid, lat_grid, beta_line, partial)

q3_xco2 <- xco2_aux |> dplyr::pull(beta_line) |> quantile(.75)

xco2_aux <- xco2_aux |>
  dplyr::mutate(
    anomaly =  partial - xco2_aux |>
      dplyr::pull(partial) |>
      mean(),
    Dbeta = beta_line - xco2_aux |>
      dplyr::pull(beta_line) |> mean(na.rm=TRUE)
  )
q3_anom <- xco2_aux |> dplyr::pull(anomaly) |> quantile(.75)


xco2_aux <- xco2_aux |>
  dplyr::mutate(
    beta_index =  ifelse(beta_line <=q3_xco2, 0, 1)
  )


xco2_aux |>
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


xco2_aux |>
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



xco2_aux |>
  ggplot2::ggplot(ggplot2::aes(x=lon_grid,y=lat_grid, col = beta_line))+
  ggplot2::geom_point()

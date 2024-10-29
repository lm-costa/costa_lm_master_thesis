
``` r
functions_files <- list.files('r/functions/',full.names = T)
purrr::map(functions_files,source)
#> [[1]]
#> [[1]]$value
#> function (x, y, pol) 
#> {
#>     as.logical(sp::point.in.polygon(point.x = x, point.y = y, 
#>         pol.x = pol[, 1], pol.y = pol[, 2]))
#> }
#> 
#> [[1]]$visible
#> [1] FALSE
#> 
#> 
#> [[2]]
#> [[2]]$value
#> function (df, output = "beta1") 
#> {
#>     modelo <- lm(xco2 ~ date, data = df)
#>     beta_1 <- c(summary(modelo)$coefficients[2])
#>     if (output == "beta1") {
#>         return(beta_1)
#>     }
#>     if (output == "p_value") {
#>         if (is.nan(beta_1)) {
#>             beta_1 <- 0
#>             p <- 1
#>         }
#>         else {
#>             p <- summary(modelo)$coefficients[2, 4]
#>             if (is.nan(p)) 
#>                 p <- 1
#>         }
#>         return(p)
#>     }
#>     if (output == "partial") {
#>         partial <- dplyr::pull(dplyr::summarise(df, xco2 = mean(xco2), 
#>             na.mr = TRUE), xco2)
#>         return(partial)
#>     }
#>     if (output == "n") {
#>         return(nrow(df))
#>     }
#>     if (output == "betaerror") {
#>         betaerror <- as.numeric(sqrt(diag(vcov(modelo)))[2])
#>         return(betaerror)
#>     }
#>     if (output == "modelerror") {
#>         modelerror <- summary(modelo)$sigma
#>         return(modelerror)
#>     }
#> }
#> 
#> [[2]]$visible
#> [1] FALSE
#> 
#> 
#> [[3]]
#> [[3]]$value
#> function () 
#> {
#>     list(ggplot2::theme(panel.background = ggplot2::element_rect(color = "black", 
#>         fill = "white"), panel.grid.major = ggplot2::element_line(color = "black", 
#>         linetype = 3)), ggspatial::annotation_scale(location = "bl", 
#>         height = ggplot2::unit(0.2, "cm")), ggspatial::annotation_north_arrow(location = "tr", 
#>         style = ggspatial::north_arrow_nautical, height = ggplot2::unit(1.5, 
#>             "cm"), width = ggplot2::unit(1.5, "cm")))
#> }
#> 
#> [[3]]$visible
#> [1] FALSE
#> 
#> 
#> [[4]]
#> [[4]]$value
#> function () 
#> {
#>     list(ggplot2::theme(panel.background = ggplot2::element_rect(color = "black", 
#>         fill = "white"), panel.grid.major = ggplot2::element_line(color = "black", 
#>         linetype = 3)), ggspatial::annotation_scale(location = "br", 
#>         height = ggplot2::unit(0.2, "cm")), ggspatial::annotation_north_arrow(location = "tr", 
#>         style = ggspatial::north_arrow_nautical, height = ggplot2::unit(1.5, 
#>             "cm"), width = ggplot2::unit(1.5, "cm")))
#> }
#> 
#> [[4]]$visible
#> [1] FALSE
#> 
#> 
#> [[5]]
#> [[5]]$value
#> function (ncdf4_file) 
#> {
#>     df <- ncdf4::nc_open(ncdf4_file)
#>     if (df$ndims != 0) {
#>         dft <- tibble::as_tibble(dplyr::filter(dplyr::filter(data.frame(lon = ncdf4::ncvar_get(df, 
#>             varid = "longitude"), lat = ncdf4::ncvar_get(df, 
#>             varid = "latitude"), time = ncdf4::ncvar_get(df, 
#>             varid = "time"), xco2 = ncdf4::ncvar_get(df, varid = "xco2"), 
#>             uncertanty = ncdf4::ncvar_get(df, varid = "xco2_uncertainty"), 
#>             quality_flag = ncdf4::ncvar_get(df, varid = "xco2_quality_flag")), 
#>             lon < -35 & lon > -75 & lat < 5 & lat > -35), quality_flag == 
#>             0))
#>     }
#>     ncdf4::nc_close(df)
#>     return(dft)
#> }
#> 
#> [[5]]$visible
#> [1] FALSE
#> 
#> 
#> [[6]]
#> [[6]]$value
#> function (x) 
#> {
#>     n <- length(x)
#>     n_na <- sum(is.na(x))
#>     x <- na.omit(x)
#>     m <- mean(x)
#>     dp <- sd(x)
#>     md <- median(x)
#>     cv <- meu_cv(x)
#>     mini <- min(x)
#>     maxi <- max(x)
#>     q1 <- quantile(x, 0.25)
#>     q3 <- quantile(x, 0.75)
#>     s2 <- var(x)
#>     g1 <- agricolae::skewness(x)
#>     g2 <- agricolae::kurtosis(x)
#>     epm <- meu_erro_padrao(x)
#>     normtest <- shapiro.test(x)
#>     return(c(N = n, N_perdidos = n_na, Media = m, Mediana = md, 
#>         Min = mini, Max = maxi, Var = s2, DP = dp, Q1 = q1, Q3 = q3, 
#>         CV = cv, EPM = epm, G1 = g1, G2 = g2, Shapiro = normtest$p.value))
#> }
#> 
#> [[6]]$visible
#> [1] FALSE
#> 
#> 
#> [[7]]
#> [[7]]$value
#> function (url_unique, user = "input your user", password = "input your password") 
#> {
#>     if (is.character(user) == TRUE & is.character(password) == 
#>         TRUE) {
#>         n_split <- length(stringr::str_split(url_unique, "/", 
#>             simplify = TRUE))
#>         filenames_nc <- stringr::str_split(url_unique, "/", simplify = TRUE)[, 
#>             n_split]
#>         repeat {
#>             dw <- try(download.file(url_unique, paste0("data-raw/", 
#>                 filenames_nc), method = "wget", extra = c(paste0("--user=", 
#>                 user, " --password ", password))))
#>             if (!(inherits(dw, "try-error"))) 
#>                 break
#>         }
#>     }
#>     else {
#>         print("input a string")
#>     }
#> }
#> 
#> [[7]]$visible
#> [1] FALSE
```

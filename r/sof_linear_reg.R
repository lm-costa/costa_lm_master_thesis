# function to create a linear model for each pixel
linear_reg_sif <- function(df,output="beta1"){
  # model for each grid cell
  modelo <- lm(sif ~ date, data=df)
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
      dplyr::summarise(sif = mean(sif), na.mr=TRUE) |>
      dplyr::pull(sif)
    return(partial)
  }

  if(output == "n"){
    return(nrow(df))
  }

  if(output == 'betaerror'){
    betaerror <- as.numeric(sqrt(diag(vcov(modelo)))[2])
    return(betaerror)
  }

  if(output == 'modelerror'){
    modelerror <- summary(modelo)$sigma
    return(modelerror)
  }
}

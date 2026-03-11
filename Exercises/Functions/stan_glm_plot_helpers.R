stan_glm_fit_df <- function(model, newdata, interval = 0.95) {
  if (!requireNamespace("rstanarm", quietly = TRUE)) {
    stop("Package 'rstanarm' is required for stan_glm_fit_df().")
  }
  
  alpha <- (1 - interval) / 2
  
  epred <- rstanarm::posterior_epred(model, newdata = newdata)
  
  out <- newdata
  out$.mean  <- colMeans(epred)
  out$.lower <- apply(epred, 2, stats::quantile, probs = alpha)
  out$.upper <- apply(epred, 2, stats::quantile, probs = 1 - alpha)
  
  out
}
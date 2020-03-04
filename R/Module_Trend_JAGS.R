#' trend.bugs.1
#'
#' Default JAGS model for use in calcPercChangeMCMC(). User-specified variations are also possible.
#' @keywords percent change, JAGS, BUGS, MCMC
#' @export
#'

trend.bugs.1 <- function(){
  for(i in 1:N){
    Abd[i] ~ dnorm(Pred_Abd[i], tau_abd)
    Pred_Abd[i] <- intercept + slope * Yr[i]
		}

  # For Perc Change
  Fit_Start <- Pred_Abd[1]
  Fit_End <- Pred_Abd[N]
  #Perc_Change <- (Fit_End - Fit_Start) /  Fit_Start * 100 # gives wrong outpout if Fit_Start <0 -> calc after

  #Priors
  slope ~ dnorm(p_slope, tau_slope)
  intercept ~ dnorm(p_intercept, tau_intercept)

  tau_abd ~ dgamma(0.00001, 0.00001)
  sigma <- 1/sqrt(tau_abd)

  # alt version
  # trying to feed in sigma from lm, but not working yet
  #tau_abd <- pow(sigma, -2)
  #sigma ~  dnorm(0,0.1)  #dnorm(p_sigma, tau_sigma)

}



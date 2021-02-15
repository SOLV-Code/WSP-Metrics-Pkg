#' comparePercChange
#'
#' this function calculates 3 alternative versions of the Perc Change metric: 1 deterministic (MLE) and 2 Bayesian (JAGS via rjags, STAN via RStanArm)
#' @param du.label short label for the the Designatable Unit (DU)
#' @param du.df data frame with the DU time series for : 2 columns, first one is "Year", second the abundance.
#' @param yrs.window number of years to use for the percent change calculation (e.g. 3 gen +1)
#' @param calc.yr year for which the perc change is calculated
#' @param samples.out if TRUE, include the posterior samples in the output
#' @param plot.pattern if TRUE, create a plot of the time series with alternative slope estimates
#' @param plot.distributions if TRUE, create a plot of the posterior distributions (kernel density plot)
#' @param plot.boxes if TRUE, create a plot of the posterior distributions (box plots)
#' @export


comparePercChange  <- function(du.label,du.df, yrs.window, calc.yr, samples.out = TRUE,
plot.pattern = TRUE, plot.posteriors = TRUE, plot.boxes  = TRUE){

#warning("NOTE: input time series is log-transformed before slope calc, but Perc Change estimate is backtransformed")

du.df.sub <- du.df %>% dplyr::filter(Year > calc.yr - yrs.window )
du.df.sub

est.simple <- calcPercChangeSimple(log(du.df.sub[,2]))

est.jags <- calcPercChangeMCMC(vec.in = log(du.df.sub[,2]),
                               method = "jags",
                               model.in = NULL, # this defaults to the BUGS code in the built in function trend.bugs.1()
                               perc.change.bm = -25,
                               out.type = "long",
                               mcmc.plots = FALSE,
                               convergence.check = FALSE# ??Conv check crashes on ts() ??? -> change to Rhat check
                                )

est.rstanarm <- calcPercChangeMCMC(vec.in = log(du.df.sub[,2]),
                                   method = "rstanarm",
                                   model.in = NULL, # hardwired regression model form, so no input
                                   perc.change.bm = -25,
                                   out.type = "long",
                                   mcmc.plots = FALSE,
                                   convergence.check = FALSE# NOT IMPLEMENTED YET
)


percentile.values <- c(0.025,0.25,0.5,0.75,0.975)
percentile.labels <- c("p2.5","p25","Med","p75","p97.5","Rhat")
extract.labels <- c("2.5%","25%","50%","75%","97.5%","Rhat")

out.mat <- matrix(NA,ncol = 4, nrow=13,
                        dimnames = list(c("MLE",paste("Jags",percentile.labels,sep="_"),
													paste("RStanArm",percentile.labels,sep="_")),
                                      c("pchange","probdecl","slope","intercept"))
                            )

out.mat["MLE",] <-round(c(est.simple$pchange,NA,est.simple$slope,est.simple$intercept),5)


out.mat[grepl("Jags",dimnames(out.mat)[[1]]),"slope"] <- round(est.jags$summary["slope",extract.labels],5)
out.mat[grepl("Jags",dimnames(out.mat)[[1]]),"intercept"] <- round(est.jags$summary["intercept",extract.labels],5)
out.mat[grepl("Jags",dimnames(out.mat)[[1]]),"pchange"] <- c(quantile(est.jags$samples$Perc_Change,probs = percentile.values),NA)
out.mat["Jags_Med","probdecl"] <- round(est.jags$probdecl,5)


out.mat[grepl("RStanArm",dimnames(out.mat)[[1]]),"slope"] <- unlist(round(est.rstanarm$summary["slope",extract.labels],5))
out.mat[grepl("RStanArm",dimnames(out.mat)[[1]]),"intercept"] <- unlist(round(est.rstanarm$summary["intercept",extract.labels],5))

out.mat[grepl("RStanArm",dimnames(out.mat)[[1]]),"pchange"] <- c(quantile(est.rstanarm$samples$Perc_Change,probs = percentile.values),NA)
out.mat["RStanArm_Med","probdecl"] <- round(est.rstanarm$probdecl,5)

percchange.df <- data.frame(
  MLE = c(NA,NA,est.simple$pchange,NA, NA),
  Jags = quantile(est.jags$samples$Perc_Change,probs = percentile.values),
  Stan =  quantile(est.rstanarm$samples$Perc_Change,probs = percentile.values))


out.list <- list(Summary = out.mat)
if(samples.out){  out.list <- c(out.list, samples = list(rstanarm = est.rstanarm$samples, jags = est.jags$samples)) }


if(plot.pattern){

plotPattern(yrs = du.df$Year ,vals = log(du.df[,2]),
            width=1,color="darkblue",
            yrs.axis=TRUE,vals.axis=TRUE,
            vals.lim=NULL, hgrid=TRUE,vgrid=FALSE,
            pch.val=19,pch.bg=NULL)


addFit(data.df = du.df.sub, coeff = list(intercept = est.rstanarm$summary["intercept","50%"],
                                           slope = est.rstanarm$summary["slope","50%"] ) )

addFit(data.df = du.df.sub, coeff = list(intercept = est.jags$summary["intercept","50%"],
                                           slope = est.jags$summary["slope","50%"] ) )

addFit(data.df = du.df.sub, coeff = list(intercept = est.simple$intercept,slope = est.simple$slope )  )


}

if(plot.posteriors){

plotDistribution(
  x.lab = "Perc Change",
  samples = list(jags = est.jags$samples$Perc_Change ,rstanarm = est.rstanarm$samples$Perc_Change   ),
  ref.lines = list(MLE = est.simple$pchange,BM = -25),
  plot.range = c(-90,90) #NULL #c(-90,90)
	)
}


if(plot.boxes){
plotBoxes(box.df = percchange.df, y.lab  = "Perc Change", ref.lines = list(BM = -25), plot.range = NULL)
}

return(out.list)

} # end comparePercChange function


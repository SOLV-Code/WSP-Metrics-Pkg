#' calcPercChangeMCMC
#'
#' MCMC version of trend metric calculation
#' This function calculates the percent change in abundances based on an exponential model of population decline, as per IUCN guidelines
#' It esimates a distribution of percent declines over the period of the time-seris, and provides the probability of declines being greater than a specified threhold
#' @param vec.in vector with numeric values
#' @param method either "jags" (default), "rstanarm", or "rstan". For properties and discussion of strengths/limitations, refer to the \href{https://github.com/SOLV-Code/MetricsCOSEWIC/wiki/1-Probability-of-Decline:-Estimation-Methods}{MetricsCOSEWIC wiki}.
#' @param model.in if NULL, use the built in functions for each method: trend.bugs.1() for jags, ETC
#' @param perc.change.bm  benchmark for Prob(Decl>BM), default = -25
#' @param na.skip  if TRUE, skip the calculations when vec.in contains any NA
#' @param out.type  "short" or "long".
#'    "short" gives summary table of posterior plus "PercChange" and "ProbDecl"
#'    "long" also includes the full mcmc samples and the jags output object
#' @param mcmc.plots if true, create the standard series of MCMC diagnostic plots
#'    Note that these are printed to the default device
#'    (i.e. need to external wrap the function call inside a pdf /dev.off call)
#'    To get a plot of the model fit, run this function with out.type = "long",
#'    and then use plot.trend.fit().
#' @param convergence.check if TRUE, do an automated convergence check
#' @keywords MCMC, slope, trend
#' @export

calcPercChangeMCMC <-function(vec.in,method = "jags",model.in = NULL , perc.change.bm = -25 , na.skip = FALSE,
							out.type = "short",
							mcmc.plots = FALSE,
							convergence.check = FALSE
							){

  na.flag <- sum(is.na(vec.in)) > 0  & na.skip
  if(na.flag){	out.list <- list(pchange = NA) }


# if no NA  (left) in input
  if(!na.flag){

  yrs.in <- 0:(length(vec.in)-1)

#######################################################################################
# JAGS


if(method == "jags"){
# WARNING: Still contains some temporary patches
# for details, check https://github.com/carrieholt/WSP-Metrics-Code/issues/37

if(is.null(model.in)){model.in <- trend.bugs.1}




	# for details on priors, see https://github.com/SOLV-Code/WSP-Metrics-Code/issues/36
	data.in  <- list(Yr = yrs.in ,
				Abd = vec.in,
				N = length(yrs.in),
				p_intercept = median(vec.in,na.rm=TRUE),
				tau_intercept = (1/ max(vec.in,na.rm=TRUE))^2 ,
				p_slope = 0,
				tau_slope =  (1 / ( max(vec.in,na.rm=TRUE)/ max(yrs.in) ))^2
			 )

	#print(data.in)

		# seems to work fine with auto-generated  inits, so don't bother with specific values or function
		init.values <- NULL

		# Alt option
		# do a simple linear regression to get INITS for slope and intercept
		# 	det.fit <- lm(test.data$Abd ~ test.data$Yr )
		# init.values <-
		#   test.det.fit$coefficients[1] (intercept)
		#   test.det.fit$coefficients[2]  (slope)
		# need 1 list element per chain! each element needs slope, intercept and tau inits



		params <- c("intercept", "slope", "sigma", "Fit_Start", "Fit_End")

		fit_mcmc <- jags(data = data.in , inits = init.values,
					parameters.to.save = params, model.file = model.in,
					n.chains = 3, n.iter = 12000, n.burnin = 2000, n.thin = 10, DIC = F)

		mcmc.samples <- fit_mcmc$BUGSoutput$sims.matrix
		mcmc.summary <- fit_mcmc$BUGSoutput$summary


		# add in % change
		mcmc.samples <- cbind(mcmc.samples,Perc_Change = NA,Perc_Change_Raw = NA)
		neg.start.idx <- mcmc.samples[,"Fit_Start"] < 0


		mcmc.samples[,"Perc_Change_Raw"][!neg.start.idx] <- (mcmc.samples[,"Fit_End"][!neg.start.idx] - mcmc.samples[,"Fit_Start"][!neg.start.idx]) /  mcmc.samples[,"Fit_Start"][!neg.start.idx] * 100
		mcmc.samples[,"Perc_Change_Raw"][neg.start.idx] <- (mcmc.samples[,"Fit_End"][neg.start.idx] + mcmc.samples[,"Fit_Start"][neg.start.idx]) /  abs(mcmc.samples[,"Fit_Start"][neg.start.idx]) * 100


		mcmc.samples[,"Perc_Change"][!neg.start.idx] <- (exp(mcmc.samples[,"Fit_End"][!neg.start.idx]) - exp(mcmc.samples[,"Fit_Start"][!neg.start.idx])) /  exp(mcmc.samples[,"Fit_Start"][!neg.start.idx]) * 100
		# NEED TO DISCUSS/CHECK THIS
		mcmc.samples[,"Perc_Change"][neg.start.idx] <- (exp(mcmc.samples[,"Fit_End"][neg.start.idx]) + exp(mcmc.samples[,"Fit_Start"][neg.start.idx])) /  exp(abs(mcmc.samples[,"Fit_Start"][neg.start.idx])) * 100
		# should do the same for summary table

		#print(head(mcmc.samples))
		#print(head(mcmc.summary))

		pchange <- median(mcmc.samples[,"Perc_Change"])
		probdecl <- sum(mcmc.samples[,"Perc_Change"] <= perc.change.bm) / dim(mcmc.samples)[1] *100

		pchange.raw <- median(mcmc.samples[,"Perc_Change_Raw"])
		probdecl.raw <- NA #sum(mcmc.samples[,"Perc_Change_Raw"] <= perc.change.bm) / dim(mcmc.samples)[1] *100 need to convert BM



		coda.obj1 <- as.mcmc(fit_mcmc$BUGSoutput$sims.matrix)
		coda.obj2 <- as.mcmc(fit_mcmc) # need this alt version for the gelman plot (this one is by chain)


	if(mcmc.plots){

		plot(fit_mcmc)
		R2jags::traceplot(fit_mcmc,ask=FALSE)
		plot(coda.obj1)
		gelman.plot(coda.obj2)
		crosscorr.plot(coda.obj1,main="crosscorr.plot")
		cumuplot(coda.obj1)
		densplot(coda.obj1)
		geweke.plot(coda.obj1)

		}

 if(!convergence.check){conv.out <- NA ; slope.converged <-  NA }

 if(convergence.check){

# get the detailed conv check summary
  conv.out <- checkConvergence(mcmc.out=fit_mcmc, vars.check = c("slope","intercept","sigma"))

# call it "converged" if all criteria are met for "slope" (FOR NOW)
# i.e. intercept or sigma could fail on one or more criteria, and it would still show "converged"
# summary shows "flagged", so need opposite (not flagged = converged)
# slope.converged <- !any(conv.out[grepl("slope",conv.out$Check),"Flag"])
# this version gets triggered too often, even with the increased trigger values

# changed it to just checking gelman-rubin < 1.1 for slope
# old: slope.converged <- !conv.out[conv.out$Check=="gelman.rubin.slope","Flag"]
  print(conv.out)
  slope.converged <- conv.out[conv.out$Check=="gelman.rubin.slope",]

 }

	mcmc.samples <- as.data.frame(mcmc.samples)

	if(out.type=="short"){ out.list <- list(pchange = pchange,probdecl = probdecl, summary = mcmc.summary,
											slope.converged = slope.converged, conv.details = conv.out
												)}

	if(out.type=="long"){ out.list <- list(pchange = pchange,probdecl = probdecl, summary = mcmc.summary,
											slope.converged = slope.converged, conv.details = conv.out,
											samples = mcmc.samples,fit.obj = fit_mcmc)}






} # end if method = jags


  #######################################################################################
  # RSTANARM


  if(method == "rstanarm"){


    stan.in <- data.frame(Year = yrs.in,Val = vec.in)


    fit.stan <- stan_lm(Val ~ Year, data = stan.in,
                        prior = NULL, # use DEFAULT
                        seed = 12345)

    # info on extracting: https://cran.r-project.org/web/packages/rstan/vignettes/stanfit-objects.html
    mcmc.samples <- as.data.frame(fit.stan$stanfit)
    names(mcmc.samples)[1:2] <- c("intercept","slope")
    mcmc.summary <- as.data.frame(fit.stan$stan_summary)
    row.names(mcmc.summary)[1:2] <- c("intercept","slope")

    print(head(mcmc.samples))
    print(head(mcmc.summary))


    # for consistency with jags output
    mcmc.samples <- mcmc.samples %>%
                     mutate(Fit_Start = intercept) %>%
                     mutate(Fit_End = intercept + slope * length(fit.stan$fitted.values))

    print(head(mcmc.samples))

    # add in % change
    mcmc.samples <- cbind(mcmc.samples,Perc_Change = NA,Perc_Change_Raw = NA)
    neg.start.idx <- mcmc.samples[,"Fit_Start"] < 0


    mcmc.samples[,"Perc_Change_Raw"][!neg.start.idx] <- (mcmc.samples[,"Fit_End"][!neg.start.idx] - mcmc.samples[,"Fit_Start"][!neg.start.idx]) /  mcmc.samples[,"Fit_Start"][!neg.start.idx] * 100
    mcmc.samples[,"Perc_Change_Raw"][neg.start.idx] <- (mcmc.samples[,"Fit_End"][neg.start.idx] + mcmc.samples[,"Fit_Start"][neg.start.idx]) /  abs(mcmc.samples[,"Fit_Start"][neg.start.idx]) * 100


    mcmc.samples[,"Perc_Change"][!neg.start.idx] <- (exp(mcmc.samples[,"Fit_End"][!neg.start.idx]) - exp(mcmc.samples[,"Fit_Start"][!neg.start.idx])) /  exp(mcmc.samples[,"Fit_Start"][!neg.start.idx]) * 100
    # NEED TO DISCUSS/CHECK THIS
    mcmc.samples[,"Perc_Change"][neg.start.idx] <- (exp(mcmc.samples[,"Fit_End"][neg.start.idx]) + exp(mcmc.samples[,"Fit_Start"][neg.start.idx])) /  exp(abs(mcmc.samples[,"Fit_Start"][neg.start.idx])) * 100


    # should do the same for summary table

    pchange <- median(mcmc.samples[,"Perc_Change"])
    probdecl <- sum(mcmc.samples[,"Perc_Change"] <= perc.change.bm) / dim(mcmc.samples)[1] *100

    pchange.raw <- median(mcmc.samples[,"Perc_Change_Raw"])
    probdecl.raw <- NA #sum(mcmc.samples[,"Perc_Change_Raw"] <= perc.change.bm) / dim(mcmc.samples)[1] *100 need to convert BM



    if(out.type=="short"){ out.list <- list(pchange = pchange,probdecl = probdecl,
                                            pchange.raw = pchange.raw,probdecl.raw = probdecl.raw,
                                            summary = mcmc.summary#,
                                            #slope.converged = slope.converged, conv.details = conv.out
    )}

    if(out.type=="long"){ out.list <- list(pchange = pchange,probdecl = probdecl, summary = mcmc.summary,
                                           pchange.raw = pchange.raw,probdecl.raw = probdecl.raw,
                                           #slope.converged = slope.converged, conv.details = conv.out,
                                           samples = mcmc.samples,fit.obj = fit.stan)}





} # end if method = rstanarm






  } # end if not na.flag


  	return(out.list)


} # end calcPercChangeMCMC


###################################################
# FUNCTION TO CALCULATE A CONVERGENCE SUMMARY FOR MCMC OUTPUT


checkConvergence <- function(mcmc.out, vars.check = c("slope","intercept","sigma")){
# function to calculate acf, acf critical value, and geweke score, then output a summary table
# mcmc.out = output from a calcPercChangeMCMC() call
# vars.check = variables for which to check convergence

# Note: acf critical values should always be equal for all variables (same sample size),
# but calculating separately just in case

#geweke.vals <- c(abs(mcmc.out$gintercept),abs(mcmc.out$gslope),abs(mcmc.out$gsig))
#names(geweke.vals) <- list("gintercept","gslope","gsigma")

checks.vec <- c("Overall","all.acf","all.geweke","all.gelman.rubin",
				as.vector(outer(c("max.abs.acf","abs.geweke","gelman.rubin"),vars.check, paste, sep="."))
				)


conv.summary <- data.frame(Check=checks.vec,
							Values = rep(NA,length(checks.vec)),
							Trigger = rep(NA,length(checks.vec)),
							Flag = rep(NA,length(checks.vec)),
							Perc.Over = rep(NA,length(checks.vec)),
							stringsAsFactors=FALSE)

#	Values = c(mcmc.out$fit$fit$convergence,, geweke.vals, NA,NA,NA,NA),
#	Trigger = c(1,crit.val.int,crit.val.slope,crit.val.sigma,2,2,2,NA,NA,NA,NA),
# max(abs(acf.int$acf[-1])),max(abs(acf.slope$acf[-1])),max(abs(acf.sigma$acf[-1])), geweke.vals

# loop through variables
for(var.use in vars.check){

acf.out <- calcACF(mcmc.par=mcmc.out$samples[,var.use],  crit.acf.buffer = 1.5,acf.plot=FALSE)
print("FLAG")
geweke.out <- calcGeweke(mcmc.par=mcmc.out$samples[,var.use])
gelman.out <- gelman.diag(as.mcmc(mcmc.out$jags.out)[,var.use])$psrf[2] # extracts the Upper 95 CI

conv.summary[conv.summary$Check==paste0("max.abs.acf.",var.use) ,c("Values","Trigger")] <- round(c(acf.out[[2]],acf.out[[3]]),4)
conv.summary[conv.summary$Check==paste0("abs.geweke.",var.use) ,c("Values","Trigger")] <- c(round(geweke.out,4),2.3)
conv.summary[conv.summary$Check==paste0("gelman.rubin.",var.use) ,c("Values","Trigger")] <- c(round(gelman.out,4),1.1)

}



conv.summary[,"Flag"] <- conv.summary[,"Values"]>=conv.summary[,"Trigger"]
#conv.summary[conv.summary$Check=="all.acf","Flag"] <- any(conv.summary[conv.summary$Check %in% c("max.abs.acf.int","max.abs.acf.slope","max.abs.acf.sigma"),"Flag"],na.rm=TRUE)
#conv.summary[conv.summary$Check=="all.geweke","Flag"] <- any(conv.summary[conv.summary$Check %in% c("abs.geweke.int","abs.geweke.slope","abs.geweke.sigma"),"Flag"],na.rm=TRUE)

conv.summary[conv.summary$Check=="all.acf","Flag"] <- any(conv.summary[grepl("max.abs.acf.",conv.summary$Check) ,"Flag"],na.rm=TRUE)
conv.summary[conv.summary$Check=="all.geweke","Flag"] <- any(conv.summary[grepl("abs.geweke.",conv.summary$Check) ,"Flag"],na.rm=TRUE)
conv.summary[conv.summary$Check=="all.gelman.rubin","Flag"] <- any(conv.summary[grepl("gelman.rubin.",conv.summary$Check) ,"Flag"],na.rm=TRUE)

conv.summary[conv.summary$Check=="Overall","Flag"] <- any(conv.summary[,"Flag"],na.rm=TRUE)

conv.summary[,"Perc.Over"] <- round(100*(as.numeric(conv.summary[,"Values"])-as.numeric(conv.summary[,"Trigger"]))/as.numeric(conv.summary[,"Trigger"]))


return(conv.summary)

}



###################################################
# SUB-FUNCTION TO CALCULATE ACF AND CRITICAL VALUE


calcACF <- function(mcmc.par,  crit.acf.buffer = 1,acf.plot=FALSE, plot.title = "ACF Plot"){
# mcmc.par = mcmc sample for 1 parameter
# crit.acf.buffer =   describe
# acf.plot = if TRUE, produce the default plot from acf()
# plot.title = title for plot from acf()

# critical value calc based on http://www.squaregoldfish.co.uk/2010/01/20/r-the-acf-function-and-statistical-significance/
# checked resulting value against default R plot to verify
# R default acf plot uses 0.95, but that was usually cutting off at one or the other lag in the MCMC sample
# Changing it to .90 actually lowers the critical value -> Need to work throug underlying rationale for this calc at some future point
# for now, just adding an x% tolerance around it

acf.val <- acf(mcmc.par, main=plot.title,plot=acf.plot)
acf.crit.val <- crit.acf.buffer * qnorm((1 + 0.95)/2)/sqrt(sum(!is.na(mcmc.par)))

return(list(acf = acf.val, max.abs.val = max(abs(acf.val$acf[-1])), crit = acf.crit.val))


}

###################################################
# SUB-FUNCTION TO CALCULATE GEWEKE SCORE


calcGeweke<- function(mcmc.par){
# mcmc.par = mcmc sample for 1 parameter

gew.val <- abs(try(geweke.diag(mcmc.par), silent=TRUE)[[1]])

names(gew.val) <- "AbsGewekeScore"

return(gew.val)

}








##################################################
# Function to calculate a generational average
# apply this to raw time series! (not logged and/or smoothed values!)
# NOT USED FOR NOW
wsp.avg <- function(vec,na.rm=TRUE){

	if(na.rm){vec<-na.omit(vec)}
	if(min(vec)==0){warning("Vector constains 0. log() results in -Inf"); stop()}
	out.val <- exp(mean(log(vec)))
	return(out.val)
}



##################################################
# PLOT OF TREND FIT


plot.trend.fit <- function(vec.in , mcmc.fit,n.plot = 1000){
# vec.in is the series of abd values used to fit the trend
# mcmc.fit is an object created by calling calcPercChangeMCMC()
#  with out.type = "long" or "full"
# n.plot is the number of subsamples mcmc pars to plot to illustrate the scatter

mcmc.have <- "samples" %in% names(mcmc.fit)

yrs <- 0:(length(vec.in)-1)

det.fit <- lm(vec.in ~ yrs) # simple regression

print(vec.in)
print(det.fit)

plot(yrs,vec.in, bty="n",type="o",xlab="Year",ylab= "Abd",col="darkblue",pch=19) # Data

# plot all the mcmc fitted lines (full posterior)
if(mcmc.have){
	for(i in sample(dim(mcmc.fit$samples)[1],n.plot,replace=FALSE)){
	abline(mcmc.fit$samples[i,"intercept"], mcmc.fit$samples[i,"slope"], col="lightgrey")
		}
	}

points(yrs,vec.in,type="o",col="darkblue",pch=19) # replot all the data points
abline(det.fit,col="darkblue",lwd=2) # simple regression - plot fitted line

# independent medians
if(mcmc.have){
abline(median(mcmc.fit$samples[,"intercept"]), median(mcmc.fit$samples[,"slope"]),
					col="red",lwd=2,lty=2)
}


# did some testing: generally almost identical (in examples so far), but
# need to discuss
# median slope with matching intercept
#med.slope <- median(mcmc.samples[,"slope"])
#med.slope.idx <- (mcmc.samples[,"slope"] > (med.slope - 0.001))  &  (mcmc.samples[,"slope"] < (med.slope +0.001) )
#sum(med.slope.idx )
#abline(median(mcmc.samples[med.slope.idx,"intercept"]), median(mcmc.samples[med.slope.idx,"slope"]), col="red",lwd=2,lty=2)


legend("top",legend=c("Data","Det Fit","MCMC Samples", "Median MCMC"),
			pch= c(19,NA,NA,NA),lty=c(NA,1,1,2),lwd=c(NA,2,1,2),col=c("darkblue","darkblue"
						,"lightgrey","red"),bty="n",cex=0.8)




} # end plot.trend.fit








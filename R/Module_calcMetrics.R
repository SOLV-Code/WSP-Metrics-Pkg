#' calcMetrics
#'
#' Main function that applies all the individual metric functions and produces a summary output
#' @param series.in vector with numeric values
#' @param yrs.in vector of years corresponding to the values in series.in
#'    (missing yrs need to be included as NA values in series.in)
#' @param gen.in  integer value with avg generation time
#' @param stk.label  label to be used in output file (e.g. "EStu")
#' @param species.label  label to be used in output file (e.g. "Sk")
#' @param series.label  label to be used in output file   (e.g. "LogSm_TotSpn")
#' @param slope.specs  list with arguments for the slope function NAME, see details there
#' @param avg.specs  list with arguments for the individual metric functions, see details there. (SHOULD CHANGE ARG NAME)
#' @param metric.bm list with upper and lower benchmarks for each metric
#' @param retro.start user-specified year. if NULL, do retrospective for last 9 yrs of the series
#' @param tracing if TRUE, print lots of diagnostic/tracking info to the command lines
#' @keywords metrics, benchmarks, slope, trend
#' @export




calcMetrics <- function(  series.in, yrs.in, gen.in,
							stk.label = "Stock",
							species.label = "Species",
							series.label = "DataVersion",
							slope.specs = list(num.gen = 3, extra.yrs = 0,filter.sides=1, slope.smooth=TRUE,
										log.transform = TRUE, out.exp = TRUE,na.rm=FALSE),
							avg.specs = list(avg.type = "geomean",recent.excl=FALSE,
												lt.smooth=TRUE, rel.avg.type="regular",
												min.lt.yrs =20,min.perc.yrs =20),
							metric.bm =  list(RelAbd = c(NA,NA),  AbsAbd = c(1000,10000),
										LongTrend = c(0.5,0.75),
										PercChange = c(-25,-15),
										ProbDeclBelowLBM = c(NA,NA),
										Percentile = c(0.25,0.5)),
							retro.start = NULL,
							tracing = TRUE){


#Note: Bronwyn MacDonald edits described at https://github.com/SOLV-Code/SOS-Data-Processing/issues/49
# have been incorporated here (except for the special cyclic version, as per WG decision)

# load packages
require(tidyverse)
require(R2jags)


if(is.null(retro.start)){retro.start <- max(yrs.in)-9}



# create output data frame
metric.labels <- names(metric.bm)
val.cols <- c("Value","LBM","UBM","Status")
retro.yrs <- yrs.in[yrs.in >= retro.start]

out.df <- expand.grid(species.label, stk.label,series.label,retro.yrs,metric.labels)
names(out.df) <- c("Species", "Stock","Label", "Year","Metric")
out.df[val.cols] <- NA

# STEP SMOOTH!
# create smoothed/transformed version of the series for slope calcs
series.smoothed <- smoothSeries(series.in,gen = gen.in, filter.sides=slope.specs$filter.sides,
            log.transform = slope.specs$log.transform,
            out.exp = slope.specs$out.exp,na.rm=slope.specs$na.rm)


# start looping through years
for(yr.do in retro.yrs ){

if(tracing){
  print("-------------------------------------------")
  print(yr.do)
}





# -----------------------------------------------------------
# CALCULATE LONG TERM TREND
#  also produces last gen avg (based on specs)
# NOTE: uses the smoothed series


if(avg.specs$lt.smooth == TRUE) {lt.trend.out <- calcLongTermTrendSimple(vec.in=series.smoothed[yrs.in <= yr.do],gen.in = gen.in,
                                                                        min.lt.yrs = avg.specs$min.lt.yrs,
                                                                        avg.type = avg.specs$avg.type,
                                                                        tracing=FALSE,
                                                                        recent.excl = avg.specs$recent.excl)}

if(avg.specs$lt.smooth == FALSE){ lt.trend.out <- calcLongTermTrendSimple(vec.in=series.in[yrs.in <= yr.do],gen.in = gen.in,
                                                                         min.lt.yrs = avg.specs$min.lt.yrs,
                                                                         avg.type = avg.specs$avg.type,
                                                                         tracing=FALSE,
                                                                         recent.excl = avg.specs$recent.excl)}

out.df$Value[out.df$Year == yr.do & out.df$Metric == "LongTrend"] <- round(lt.trend.out$lt.trend/100,4)
out.df[out.df$Year == yr.do & out.df$Metric == "LongTrend",c("LBM","UBM")] <- metric.bm$LongTrend





# -----------------------------------------------------------
# CALCULATE RELATIVE BM - PERCENTILE
# NOTE: uses the smoothed series

perc.out <- calcPercentileSimple(vec.in=series.smoothed[yrs.in <= yr.do],gen.in = gen.in,
                                    min.perc.yrs = avg.specs$min.perc.yrs,
                                    avg.type = avg.specs$avg.type,
                                    tracing=FALSE,
                                    recent.excl = avg.specs$recent.excl)

out.df$Value[out.df$Year == yr.do & out.df$Metric == "Percentile"] <- round(perc.out$percentile,4)
out.df[out.df$Year == yr.do & out.df$Metric == "Percentile",c("LBM","UBM")] <- metric.bm$Percentile


# -----------------------------------------------------------
# CALCULATE RELATIVE ABUNDANCE  METRIC

# OLD VERSION: out.df$Value[out.df$Year == yr.do & out.df$Metric == "RelAbd"] <- round(lt.trend.out$recent.avg,4)

# FIXED VERSION (excluding cyclic special cases):

# We used the average of the last 4 years, not the average of the average of the last 4 years.
if(avg.specs$rel.avg.type == "smoothed" & avg.specs$lt.smooth == TRUE){
        out.df$Value[out.df$Year == yr.do & out.df$Metric == "RelAbd"] <- round(lt.trend.out$recent.avg,4)
}

if(avg.specs$rel.avg.type == "smoothed" & avg.specs$lt.smooth == FALSE) {
  out.df$Value[out.df$Year == yr.do & out.df$Metric == "RelAbd"] <- round(calcLongTermTrendSimple(vec.in=series.smoothed[yrs.in <= yr.do],gen.in = gen.in,
                                                                                                  min.lt.yrs = avg.specs$min.lt.yrs,
                                                                                                  avg.type = avg.specs$avg.type,
                                                                                                  tracing=FALSE, recent.excl = avg.specs$recent.excl)$recent.avg,4)
                }

if(avg.specs$rel.avg.type == "regular"){
  out.df$Value[out.df$Year == yr.do & out.df$Metric == "RelAbd"] <- series.smoothed[yrs.in == yr.do]}



# calculate  p25 and p75 for the BM if NA
# # BMac edit - do not use the the p25 or p75 if no BM available, this should just be NA if not available
#if(is.na(metric.bm$RelAbd[1])){lbm.use <- quantile(series.smoothed[yrs.in <= yr.do],probs = 0.25,na.rm=TRUE) }
#if(!is.na(metric.bm$RelAbd[1])){lbm.use <- metric.bm$RelAbd[1] }
#if(is.na(metric.bm$RelAbd[2])){ubm.use <- quantile(series.smoothed[yrs.in <= yr.do],probs = 0.75,na.rm=TRUE) }
#if(!is.na(metric.bm$RelAbd[2])){ubm.use <- metric.bm$RelAbd[2] }

if(!is.na(metric.bm$RelAbd[1])){lbm.use <- metric.bm$RelAbd[1]
                                ubm.use <- metric.bm$RelAbd[2]
                                }

if(is.na(metric.bm$RelAbd[1])){lbm.use <- ubm.use <- NA }

out.df[out.df$Year == yr.do & out.df$Metric == "RelAbd",c("LBM","UBM")] <- c(lbm.use,ubm.use)



# -----------------------------------------------------------
# CALCULATE ABSOLUTE ABUNDANCE METRIC

if(avg.specs$rel.avg.type == "smoothed" & avg.specs$lt.smooth == TRUE){
    out.df$Value[out.df$Year == yr.do & out.df$Metric == "AbsAbd"] <- round(lt.trend.out$recent.avg,4)
      }

if(avg.specs$rel.avg.type == "smoothed" & avg.specs$lt.smooth == FALSE){
  out.df$Value[out.df$Year == yr.do & out.df$Metric == "RelAbd"] <- round(calcLongTermTrendSimple(vec.in=series.smoothed[yrs.in <= yr.do],gen.in = gen.in,
                                                                                                  min.lt.yrs = avg.specs$min.lt.yrs,
                                                                                                  avg.type = avg.specs$avg.type,
                                                                                                  tracing=FALSE,
                                                                                                    recent.excl = avg.specs$recent.excl)$recent.avg,4)
        }

  if(avg.specs$rel.avg.type == "regular"){
  out.df$Value[out.df$Year == yr.do & out.df$Metric == "AbsAbd"] <- series.smoothed[yrs.in == yr.do]
  }


out.df[out.df$Year == yr.do & out.df$Metric == "AbsAbd",c("LBM","UBM")] <- metric.bm$AbsAbd



# -----------------------------------------------------------
# CALCULATE PERC CHANGE AND PROB DECL
if(tracing){ print("starting Perc Change and Prob Decl -")}

# get the last n gen (+ extra years if specified)
yrs.use <- (yr.do - (slope.specs$extra.yrs -1 + gen.in*slope.specs$num.gen)) :  yr.do

# Add in smoothed or not! *BLM July 22 2020 ********************************
# Added in log y/n GP Feb 2021
# if out.exp =  TRUE, then the smoothed series was back-transformed from log space
# and need to log-transform before trend calc, b/c
# using logged = TRUE in the call to calcPercChangeMCMC() below
if(slope.specs$slope.smooth == TRUE){
						vec.use <- series.smoothed[yrs.in %in% yrs.use]
							if(slope.specs$out.exp){trend.vec <- log(vec.use)}
							if(!slope.specs$out.exp){trend.vec <- vec.use}
							}

if(slope.specs$slope.smooth == FALSE){ trend.vec <- log(series.in[yrs.in %in% yrs.use])   }

if(tracing){print(yrs.use); print(trend.vec)}


#old fn call
#pchange.mcmc <- calcPercChangeMCMC(vec.in= vec.use,model.in = trend.bugs.1 ,
#                                   perc.change.bm = metric.bm$PercChange[1] , na.skip=FALSE,
#                                   out.type = "short", mcmc.plots = FALSE)


# NEW FEB 2021: Need at least half the data points before trying MCMC
# COMMENTED OUT AUG 2024: This was not doing anything because of the infill below, now moved it to be part of the infill criterion -> do not infill with random low number if more than 1/3 of records are missing
#if(sum(!is.na(trend.vec)) < length(trend.vec/2) ){na.skip.use <- TRUE} # this results in NA outputs, but stops crashing
#if(sum(!is.na(trend.vec)) >= length(trend.vec/2) ){na.skip.use <- FALSE}

# NEW FEB 2021:
# If any zeroes in the data, trend.vec contains -Inf, and it crashes below
# using the strategy from Perry et al 2021 (https://journals.plos.org/plosone/article/comments?id=10.1371/journal.pone.0245941)
# as suggested by Carrie Holt at https://github.com/SOLV-Code/MetricsCOSEWIC/issues/15
# -> replacing  - inf with log(random number between 0 and half of min obs)
  # used to be
  # inf.idx <- !is.finite(trend.vec)
  # inf.idx[is.na(inf.idx)] <- FALSE
  # trend.vec[inf.idx] <- log(runif(sum(inf.idx,na.rm = TRUE),0.00000001, min(trend.vec[!inf.idx],na.rm = TRUE)/2))
  # fixed to address this issue: https://github.com/SOLV-Code/SOS-Data-Processing/issues/67
  # the old way way filled in a bunch of small numbers for any missing years (e.g. at beginning of retro for short series)
  # causing steeply increasing trend metrics.

# AUG 2024 FIX: 
# Only fill in any NA/0 AFTER THE FIRST FINITE LOG(Spn) IN THE SERIES.
# Only fill in 1/3 or less of data, changed na.skip argument below to TRUE (rather than how was done before, see above)
# NEW BEHAVIOUR: 
# - Fill in any NA/0 after the first data point and calculate slope, unless too many data points are missing.
# - If too many data points are missing, leave the NAs, and the call to calcPErcCHangeMCMC will na.skip=TRUE to give 
# properly formatted output with NA values for the slope.

# !is.finite catches NA and -Inf; -Inf comes from log(0)
# also need to deal with cases where spn =1, because that gives ln(1) = 0, which would mess up the runif() call
# (upper limit below the lower limit)
# using half of smallest non-zero spn for the upper bound

prop.finite <- sum(is.finite(trend.vec))/length(trend.vec)


if(prop.finite >= 2/3){ 
nonfinite.pos <- which(!is.finite(trend.vec))
finite.pos <- which(is.finite(trend.vec))

first.finite.pos <- min(finite.pos)
last.finite.pos <- max(finite.pos)
zero.pos <- which(trend.vec == 0)

fix.pos <- nonfinite.pos[nonfinite.pos > first.finite.pos & nonfinite.pos < last.finite.pos]
nonzero.min <- min(trend.vec[setdiff(finite.pos,zero.pos)]) # calc min after excluding zeroes
trend.vec[fix.pos] <- runif(length(fix.pos),0.00000001, nonzero.min/2)  # removed log (already feeding in log values)
}

if(prop.finite < 2/3){ 
nonfinite.pos <- which(!is.finite(trend.vec))
trend.vec[nonfinite.pos] <- NA
}

  
  
if(tracing){print("trend.vec after 0,NA fixing"); print(trend.vec)}
  


pchange.mcmc <- calcPercChangeMCMC(vec.in = trend.vec,
                               method = "jags",
                               model.in = NULL, # this defaults to the BUGS code in the built in function trend.bugs.1()
                               perc.change.bm = -25,
							    na.skip = TRUE,
                               out.type = "long",
                               mcmc.plots = FALSE,
                               convergence.check = FALSE, # ??Conv check crashes on ts() ??? -> change to Rhat check
							   logged = TRUE
                                )


out.df$Value[out.df$Year == yr.do & out.df$Metric == "PercChange"] <- round(pchange.mcmc$pchange,4)
out.df[out.df$Year == yr.do & out.df$Metric == "PercChange",c("LBM","UBM")] <- metric.bm$PercChange

out.df$Value[out.df$Year == yr.do & out.df$Metric == "ProbDeclBelowLBM"] <- round(pchange.mcmc$probdecl,4)
out.df[out.df$Year == yr.do & out.df$Metric == "ProbDeclBelowLBM",c("LBM","UBM")] <- metric.bm$ProbDeclBelowLBM



} # end looping through years




# -----------------------------------------------------------
# ADD IN  STATUS
# how to do this in tidyverse?

out.df$Status[out.df$Value <= out.df$LBM] <- "Red"
out.df$Status[out.df$Value > out.df$UBM] <- "Green"
out.df$Status[out.df$Value > out.df$LBM & out.df$Value <= out.df$UBM] <- "Amber"



out.df






} #end calcMetrics

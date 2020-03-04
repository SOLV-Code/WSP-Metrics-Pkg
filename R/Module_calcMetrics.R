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
#' @param avg.specs  list with arguments for the avg function NAME, see details there 
#' @param metric.bm list with upper and lower benchmarks for each metric
#' @param retro.start user-specified year. if NULL, do retrospective for last 9 yrs of the series
#' @param tracing if TRUE, print lots of diagnostic/tracking info to the command lines
#' @keywords metrics, benchmarks, slope, trend
#' @export




calcMetrics <- function(  series.in, yrs.in, gen.in,
							stk.label = "Stock",
							species.label = "Species",
							series.label = "DataVersion",
							slope.specs = list(num.gen = 3, extra.yrs = 0,filter.sides=1, 
										log.transform = TRUE, out.exp = TRUE,na.rm=FALSE),
							avg.specs = list(avg.type = "geomean",recent.excl=FALSE, 
										min.lt.yrs =20),
							metric.bm =  list(RelAbd = c(NA,NA),  AbsAbd = c(1000,10000),
										LongTrend = c(0.5,0.75),
										PercChange = c(-25,-15),
										ProbDeclBelowLBM = c(NA,NA)),
							retro.start = NULL, 
							tracing = TRUE){


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

# create smoothed/transformed version of the series for slope calcs
series.smoothed <- smoothSeries(series.in,gen = gen.in, filter.sides=slope.specs$filter.sides, 
            log.transform = slope.specs$log.transform, out.exp = slope.specs$out.exp,na.rm=slope.specs$na.rm)


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
  
lt.trend.out <- calcLongTermTrendSimple(vec.in=series.smoothed[yrs.in <= yr.do],gen.in = gen.in,
                                        min.lt.yrs = avg.specs$min.lt.yrs, 
                                        avg.type = avg.specs$avg.type, 
                                        tracing=FALSE,
                                        recent.excl = avg.specs$recent.excl)
  
out.df$Value[out.df$Year == yr.do & out.df$Metric == "LongTrend"] <- round(lt.trend.out$lt.trend/100,4)
out.df[out.df$Year == yr.do & out.df$Metric == "LongTrend",c("LBM","UBM")] <- metric.bm$LongTrend

  
    
    
# -----------------------------------------------------------    
# CALCULATE RELATIVE ABUNDANCE  METRIC  
    
out.df$Value[out.df$Year == yr.do & out.df$Metric == "RelAbd"] <- round(lt.trend.out$recent.avg,4)

# calculate  p25 and p75 for the BM if NA
if(is.na(metric.bm$RelAbd[1])){lbm.use <- quantile(series.smoothed[yrs.in <= yr.do],probs = 0.25,na.rm=TRUE) }
if(!is.na(metric.bm$RelAbd[1])){lbm.use <- metric.bm$RelAbd[1] }
if(is.na(metric.bm$RelAbd[2])){ubm.use <- quantile(series.smoothed[yrs.in <= yr.do],probs = 0.75,na.rm=TRUE) }
if(!is.na(metric.bm$RelAbd[2])){ubm.use <- metric.bm$RelAbd[2] }


out.df[out.df$Year == yr.do & out.df$Metric == "RelAbd",c("LBM","UBM")] <- c(lbm.use,ubm.use)



# -----------------------------------------------------------    
# CALCULATE ABSOLUTE ABUNDANCE METRIC
    
out.df$Value[out.df$Year == yr.do & out.df$Metric == "AbsAbd"] <- round(lt.trend.out$recent.avg,4)
out.df[out.df$Year == yr.do & out.df$Metric == "AbsAbd",c("LBM","UBM")] <- metric.bm$AbsAbd

  
# -----------------------------------------------------------  
# CALCULATE PERC CHANGE AND PROB DECL
if(tracing){ print("starting Perc Change and Prob Decl -")}
  
# get the last n gen (+ extra years if specified)
yrs.use <- (yr.do - (slope.specs$extra.yrs -1 + gen.in*slope.specs$num.gen)) :  yr.do
vec.use <- series.smoothed[yrs.in %in% yrs.use]
  
if(tracing){print(yrs.use); print(vec.use)} 
  
pchange.mcmc <- calcPercChangeMCMC(vec.in= vec.use,model.in = trend.bugs.1 , 
                                   perc.change.bm = metric.bm$PercChange[1] , na.skip=FALSE, 
                                   out.type = "short", mcmc.plots = FALSE)


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

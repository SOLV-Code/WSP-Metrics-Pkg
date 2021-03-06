#' calcPercChange
#'
#' This function applies the calculation of percent change in abundance over time to a data frame (Year x Stock) iteratively for each year
#' of the time-series using only the data prior to that year.
#' It calculate declines over a specified number of generation and uses an exponential model to estimate decline rate, as per IUCN guidelines.
#' For a single vector use  calcPercChangeSimple() to calculate percent declines over the entire time period.
#' @param X a data frame of raw values with dimensions Years x Stocks. Row labels are years. NAs values are allowed.
#' @param gen.in = average generation time
#' @param slope.num.gen number of generations over which to calculate the perc change
#' @param extra.yrs to handle COSEWIC "extra year". default is 0
#' @param genmean.smoothing  if TRUE, apply the smoothSeries() function. default is TRUE
#' @param log.transform   if TRUE, log-transform the time series before calculating the smoothed series. default is TRUE
#' @param out.exp  if TRUE, take the exponent of the smoothed series for output. default is TRUE
#' @param tracing if TRUE, print various diagnostics to the command line. default is FALSE
#' @keywords percent change, slope
#' @export


calcPercChange  <- function(X,gen.in = 4, slope.num.gen = 3, extra.yrs = 0, genmean.smoothing = TRUE,
                            log.transform = TRUE, out.exp = TRUE, tracing=FALSE){

# if genmean.smoothing
if(genmean.smoothing){
	series.use <- apply(X,MARGIN=2,FUN=smoothSeries,gen = gen.in , filter.sides=1, log.transform = log.transform, out.exp = out.exp,na.rm=FALSE)
	dimnames(series.use) <- dimnames(X)
	}

if(!genmean.smoothing){
	series.use <- X
	}


if(tracing){ print("series.use ------"); print(tail(series.use))}

# create output template
out.mat <- series.use
out.mat[,] <- NA


yrs.list <-   as.numeric(dimnames(series.use)[[1]])


# get first smoothed value at 1 gen.in, then need 3 more gen for the slope
# (e.g. if gen = 4, start at 15th year (stocks with missing data at beginning will juts get NA (b/c na.rm =FALSE below)
# plus account for the COSEWIC extra yr option
yrs.list <- yrs.list[  (gen.in -1 + extra.yrs + gen.in*slope.num.gen) :  length(yrs.list) ]
if(tracing){print("yrs.list"); print(yrs.list)}


for(yr.use in yrs.list){

yrs.vec <-  seq(yr.use- ((gen.in*slope.num.gen)-1+extra.yrs) ,yr.use)
sub.idx <- dimnames(series.use)[[1]] %in% yrs.vec
tmp.mat <- series.use[sub.idx,]
out.mat[as.character(yr.use),] <- apply(tmp.mat, MARGIN=2,FUN= per.change.mod.fast,na.rm=TRUE)

} # end looping through years

return(out.mat)

} # end calcPercChange function


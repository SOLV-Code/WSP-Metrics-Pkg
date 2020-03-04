#' smoothSeries
#'
#' function to calculate a smoothed version of an input vector (calculate the running geometric mean of log_e
#' transformed data)
#' @param vec.in vector with numeric values
#' @param gen  generation time (i.e. number of years to average)
#' @param filter.sides time window for gen avg (1 = past values only, 2= centered)
#' @param out.exp if TRUE, then convert the output back from log space)
#' @param na.rm if FALSE, then any gen avg including NA values will be NA
#' @param convergence.check if TRUE, do an automated convergence check
#' @keywords smoothing,generational average, log transform
#' @export

smoothSeries <- function(vec.in, gen , filter.sides=2, log.transform = TRUE, out.exp = TRUE,na.rm=FALSE){


if(sum(vec.in < 0,na.rm=TRUE)>0){warning("Function smoothSeries() does not accept negative values");stop()}

# log transform
if(log.transform){ vec.use <- log1p(vec.in) }
if(!log.transform){ vec.use <- vec.in }


# running mean, centered on each value (if gen is an even number, then more forward than back)
vec.out <- as.vector(stats::filter(vec.use, filter=rep(1/gen,gen),sides=filter.sides))

# default is to convert back from log space (but for slope calcs want to keep in log space)
if(log.transform & out.exp){vec.out <- expm1(vec.out)}

return(vec.out)

} # end smoothSeries










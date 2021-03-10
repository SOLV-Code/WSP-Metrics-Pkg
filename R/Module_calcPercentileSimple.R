#' calcPercentileSimple
#'
#' this function calculates the percentile for the last generational average in a single vector.
#' Function structure and arguments match \code{\link{calcLongTermTrendSimple}}.
#' NOTE: This function only calculates the percentile value. It is the \code{\link{calcMetrics}} wrapper function that compares
#' this value to user-specified benchmarks and assigns a status category.
#' Also note the comments re: percentile benchmarks in the documentation for \code{\link{calcMetrics}}.
#' @param vec.in  a vector of values. NA are possible, but will result in NA trend for any recent time window that includes one or more NA
#' @param gen.in  use the last gen.in values for the "recent" avg
#' @param avg.type  "mean","geomean", or "median"
#' @param min.perc.yrs return NA if don't have at least this many years to calculate longterm avg
#' @param recent.excl if TRUE, then don't use the values from the recent gen as part of the LT avg
#' @keywords trend
#' @export
#' @examples
#' calcPercentileSimple(vec.in = as.vector(Nile) ,
#' gen.in = 4,min.perc.yrs = 20, avg.type = "geomean",
#' tracing=FALSE,
#' recent.excl = FALSE)

calcPercentileSimple  <- function(vec.in,gen.in = 4,min.perc.yrs = 20, avg.type = "geomean", tracing=FALSE,
							recent.excl = FALSE){

print(avg.type)

if(tracing){ print("vec.in ------"); print(vec.in)}

# if not enough data
if(sum(!is.na(vec.in))< min.perc.yrs){perc.out <- NA ; recent.avg <- NA }


# if enough data
if(sum(!is.na(vec.in))>= min.perc.yrs){

recent.vals <-  vec.in[(length(vec.in) - gen.in +1) :length(vec.in)]

if(!recent.excl){sample.vals <-  vec.in}
if(recent.excl){ sample.vals <-  vec.in[1: (length(vec.in) - gen.in)] }

if(tracing){print(recent.vals); print(sample.vals )}

#calculate recent avg (if any NA , get NA)
if(tolower(avg.type) == "mean"){ 	recent.avg <- mean(recent.vals, na.rm=FALSE)}
if(tolower(avg.type) == "geomean"){	recent.avg <- expm1(mean(log1p(recent.vals), na.rm=FALSE)) }
if(tolower(avg.type) == "median"){median(recent.vals, na.rm=FALSE) }

# using the approach from here:
#https://stats.stackexchange.com/questions/50080/estimate-quantile-of-value-in-a-vector

perc.out <- ecdf(sample.vals)(recent.avg)

} # end if have enough data

return(list(percentile= perc.out, recent.avg = recent.avg))

} # end calcPercentileSimple function

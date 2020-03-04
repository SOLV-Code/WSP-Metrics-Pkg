#' calcLongTermTrendSimple
#'
#' this function just does a basic long-term trend calculation on a single vector.
#' calcLongTermTrend() applies the same basic calc retrospectively to a data frame of Year x Stock,
#' @param vec.in  a vector of values. NA are possible, but will result in NA trend for any recent time window that includes one or more NA
#' @param gen.in  use the last gen.in values for the "recent" avg
#' @param avg.type  "mean","geomean", or "median"
#' @param min.lt.yrs return NA if don't have at least this many years to calculate longterm avg
#' @param recent.excl if TRUE, then don't use the values from the recent gen as part of the LT avg
#' @keywords trend
#' @export
#' @examples
#' calcLongTermTrendSimple(vec.in = as.vector(Nile) ,
#' gen.in = 4,min.lt.yrs = 20, avg.type = "geomean",
#' tracing=FALSE,
#' recent.excl = FALSE)

calcLongTermTrendSimple  <- function(vec.in,gen.in = 4,min.lt.yrs = 20, avg.type = "geomean", tracing=FALSE,
							recent.excl = FALSE){


if(tracing){ print("vec.in ------"); print(vec.in)}

if(sum(!is.na(vec.in))< min.lt.yrs){lt.trend <- NA ; recent.avg <- NA; longterm.avg <- NA }

if(sum(!is.na(vec.in))>= min.lt.yrs){

recent.vals <-  vec.in[(length(vec.in) - gen.in +1) :length(vec.in)]

if(!recent.excl){longterm.vals <-  vec.in}
if(recent.excl){ longterm.vals <-  vec.in[1: (length(vec.in) - gen.in)] }

if(tracing){print(recent.vals); print(longterm.vals )}

if(tolower(avg.type) == "mean"){
	recent.avg <- mean(recent.vals, na.rm=FALSE)  # if any NA in a column, get NA
	longterm.avg <- mean(longterm.vals, na.rm=TRUE)  # ignore NA
	}

if(tolower(avg.type) == "geomean"){
	recent.avg <- expm1(mean(log1p(recent.vals), na.rm=FALSE))
	longterm.avg <- expm1(mean(log1p(longterm.vals), na.rm=TRUE))
	}

if(tolower(avg.type) == "median"){
	recent.avg <- median(recent.vals, na.rm=FALSE)  # if any NA in a column, get NA
	longterm.avg <- median(longterm.vals, na.rm=TRUE)  # ignore NA
	}

lt.trend<- round(recent.avg/longterm.avg *100)

} # end if have enough data

return(list(lt.trend= lt.trend, recent.avg = recent.avg, longterm.avg = longterm.avg))

} # end calcLongTermTrendSimple function

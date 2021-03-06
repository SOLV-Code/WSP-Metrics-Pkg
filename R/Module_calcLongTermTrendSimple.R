#' calcLongTermTrendSimple
#'
#' this function just does a basic long-term trend calculation on a single vector.
#' calcLongTermTrend() applies the same basic calculation iteratively for each year and stock in a data frame (Year X Stock) using only data prior to that year.
#' @param vec.in  a vector of raw values. NA are possible, but will result in NA trend for any recent time window that includes one or more NA
#' @param gen.in  use the last gen.in values for the "recent" avg
#' @param avg.type  "mean","geomean", or "median"
#' @param min.lt.yrs return NA if don't have at least this many years to calculate longterm avg
#' @param recent.excl if TRUE, then don't use the values from the recent gen as part of the LT avg
#' @keywords trend
#' @export
#' @examples
#' calcLongTermTrendSimple(vec.in = exampleData$Stock1, gen.in = 4,min.lt.yrs = 20,
#' avg.type = "geomean", tracing=FALSE,recent.excl = FALSE)


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

  # If any zeroes in the data, then this version generates longterm.avg = 0 and Longtrend metrics  = Inf
  #recent.avg <- exp(mean(log(recent.vals), na.rm=FALSE))
	#longterm.avg <- exp(mean(log(longterm.vals), na.rm=TRUE))

	# using the strategy from Perry et al 2021 (https://journals.plos.org/plosone/article/comments?id=10.1371/journal.pone.0245941)
  # as suggested by Carrie Holt at https://github.com/SOLV-Code/MetricsCOSEWIC/issues/15

  tmp.vals <- longterm.vals
  zero.idx <- tmp.vals == 0
  zero.idx[is.na(zero.idx)] <- FALSE

  tmp.vals[zero.idx] <- runif(sum(zero.idx,na.rm = TRUE),0.00000001, min(longterm.vals[!zero.idx],na.rm = TRUE)/2)

  recent.avg <- exp(mean(log(recent.vals), na.rm=FALSE))
  longterm.avg <- exp(mean(log(tmp.vals), na.rm=TRUE))


	}

if(tolower(avg.type) == "median"){
	recent.avg <- median(recent.vals, na.rm=FALSE)  # if any NA in a column, get NA
	longterm.avg <- median(longterm.vals, na.rm=TRUE)  # ignore NA
	}

lt.trend<- round(recent.avg/longterm.avg *100)

} # end if have enough data

return(list(lt.trend= lt.trend, recent.avg = recent.avg, longterm.avg = longterm.avg))

} # end calcLongTermTrendSimple function

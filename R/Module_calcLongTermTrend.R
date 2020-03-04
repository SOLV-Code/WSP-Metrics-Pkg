#' calcLongTermTrend
#'
#' this function applies a basic long-term trend to a data frame of Year x Stock,
#' with various user options (e.g. log transform, gen avg smoothing, time window to use).
#' For a single vector, use calcLongTermTrendSimple()
#' @param X a data frame with Years x Stocks. Row labels are years, no missing years allowed, NA are possible, but will result in NA Trend for
# any recent time window that includes one or more NA (for now: discuss infill options for future extensions, as in Ck implementation)
#' @param extra.yrs to handle COSEWIC "extra year"
#' @param avg.type "mean","geomean", or "median"
#' @param recent.excl if TRUE, then don't use the values from the recent gen as part of the LT avg
#' @keywords trend
#' @export

calcLongTermTrend  <- function(X,gen.in = 4, recent.num.gen = 1, extra.yrs = 0,
							min.lt.yrs = 20, avg.type = "geomean", tracing=FALSE,
							recent.excl = FALSE){



series.use <- X  # to keep var names consistent with calcPercChange

if(tracing){ print("series.use ------"); print(tail(series.use))}

# create output template
out.mat <- series.use
out.mat[,] <- NA

yrs.list <-   as.numeric(dimnames(series.use)[[1]])

first.yr <- min(yrs.list)

# get first "recent" value at min.lt.yrs + 1, plus account for the COSEWIC extra yr option
yrs.list <- yrs.list[  (min.lt.yrs + 1 + extra.yrs) :  length(yrs.list) ]
if(tracing){print("yrs.list"); print(yrs.list)}


for(yr.use in yrs.list){

if(tracing){print("--------------------");	print(yr.use)}

recent.yrs <-  seq(yr.use - ((gen.in*recent.num.gen)-1+extra.yrs) ,yr.use)
recent.idx <- dimnames(series.use)[[1]] %in% recent.yrs
recent.mat <- series.use[recent.idx,]

if(!recent.excl){longterm.yrs <-  seq(first.yr ,yr.use)}
if(recent.excl){ longterm.yrs <-  seq(first.yr , min(recent.yrs)-1)}
longterm.idx <- dimnames(series.use)[[1]] %in% longterm.yrs
longterm.mat <- series.use[longterm.idx,]

if(tracing){
	print(recent.yrs)
	print(longterm.yrs)
	print("-----");	print(recent.mat)
	print("-----");	print(longterm.mat)
	}

if(tolower(avg.type) == "mean"){
	recent.avg <- colMeans(recent.mat, na.rm=FALSE)  # if any NA in a column, get NA
	longterm.avg <- colMeans(longterm.mat, na.rm=TRUE)  # ignore NA
	}

if(tolower(avg.type) == "geomean"){
	recent.avg <- expm1(colMeans(log1p(recent.mat), na.rm=FALSE))
	longterm.avg <- expm1(colMeans(log1p(longterm.mat), na.rm=TRUE))
	}

if(tolower(avg.type) == "median"){
	recent.avg <- apply(recent.mat,MARGIN=2,FUN=median, na.rm=FALSE)  # if any NA in a column, get NA
	longterm.avg <- apply(longterm.mat,MARGIN=2,FUN=median, na.rm=TRUE)   # ignore NA
	}

if(tracing){ print(recent.avg); print(longterm.avg) }

out.mat[as.character(yr.use),] <- round(recent.avg/longterm.avg *100)

} # end looping through years

return(out.mat)

} # end calcLongTermTrend function


#' calcPercChangeSimple
#'
#' this function just does a basic percent change calculation on a single vector.
#' calcPercChange() applies the same basic calc retrospectively to a data frame of Year x Stock,
#' Note: for now this simply replicates the internal function per.change.mod.fast
#' @param vec.in  a vector of values. NA are possible, but will result in NA trend (no infill for now)
#' @export
#' @examples
#' calcPercChangeSimple(vec.in = as.vector(Nile))

calcPercChangeSimple  <- function(vec.in){

  na.rm <- TRUE
  if(na.rm){vec.use <- na.omit(vec.in)} # NEED TO DISCUSS THIS
  if(!na.rm){vec.use <- vec.in}


  if(sum(is.na(vec.use)) == 0 ){
    n<-length(vec.use)
    yrs <- 1:n
    lm.coeff <- .lm.fit(cbind(1,yrs),vec.use)$coefficients # uses model matrix that is usually created inside lm()
    #print(lm.coeff)
    pchange <- (exp(lm.coeff[1]+lm.coeff[2]*n) -  exp(lm.coeff[1]+lm.coeff[2])) / exp(lm.coeff[1]+lm.coeff[2]) *100
    #print(pchange)
  }

  if(sum(is.na(vec.in)) >0 ){	pchange <- NA }


out.list <- list(pchange = pchange )


return(out.list)

} # end calcPercChange function


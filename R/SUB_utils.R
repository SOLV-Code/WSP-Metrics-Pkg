include.dependencies <- function(){
#' @title Internal handling of dependencies
#'
#' @description Seems like this is needed in addition to the import list in the DESCRIPTION file, so keeping them all together in one place.
#' @import tidyverse
#' @import R2jags
#' @import coda
#' @import rstanarm
#' @import rstan
#' @export


print("including dependencies")


} # end function include.dependencies


p.label <- function(x){

if(x >= 0.1){p.out <- ">0.1"}
if(x < 0.1){p.out <- "<0.1"}
if(x < 0.05){p.out <- "<0.05"}
if(x < 0.01){p.out <- "<0.01"}
if(x < 0.005){p.out <- "<0.005"}


return(p.out)

}

# geometric mean
# https://stackoverflow.com/questions/2602583/geometric-mean-is-there-a-built-in


gm_mean = function(x, na.rm=TRUE){
  exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x))
}


alt.means <- function(x){

out.vec <- c(mean(x,na.rm=TRUE),median(x,na.rm=TRUE),gm_mean(x))

names(out.vec) <- c("mean","median","geomean")

return(out.vec)


}
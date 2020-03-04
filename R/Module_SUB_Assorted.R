empty.plot <-function(x){plot(1:5,1:5,type="n",bty="n",axes=FALSE,xlab="",ylab="")}




slope.reflines <- function(dir="h",bm = c(-15,-25),cols = c("green","red") ){
				if(dir=="h"){abline(h=bm,col=cols) }
				if(dir=="v"){abline(v=bm,col=cols) }
}


data.rescale <- function(x){
# x is a vector
x.out <- x/max(x,na.rm=TRUE)*10
}

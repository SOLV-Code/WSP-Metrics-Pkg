#' plotPattern
#'
#' Function to plot time series pattern with minimal labels
#' @param yrs numeric vector of years
#' @param vals numeric vector of values
#' @param width line width, default is 1
#' @param color line color, default is "grey"
#' @param yrs.axis if TRUE, display the year axis. default is FALSE
#' @param vals.axis if true, display the value axis. default is FALSE
#' @param vals.lim vector of length 2 with y-axis limits
#' @param hgrid if TRUE, display horizontal grid lines. default is FALSE
#' @param vgrid if TRUE, display vertical grid lines. default is FALSE
#' @param pch.val numeric value for plotting character. default is 19 (solid pt)
#' @param pch.bg  text string with color for point fill (if pch allows, e.g. 21)
#' @export


plotPattern <- function(yrs,vals,width=1,color="grey",yrs.axis=FALSE,vals.axis=FALSE,
			vals.lim=NULL, hgrid=FALSE,vgrid=FALSE,pch.val=19,pch.bg=NULL){
options(scipen=3)

##############################################################################
#

# TO DO:
# - add option to label hgrid with modified values (e.g. plot logs, but label with raw numbers)


if(is.null(vals.lim)){vals.lim <- c(0,max(pretty(vals)))}

plot(yrs,vals,axes=FALSE,ylim=vals.lim,type="l",lwd=width,col=color,xlab="",ylab="")

if(yrs.axis==TRUE){axis(side=1,at=pretty(yrs),labels=pretty(yrs),cex.axis=1,lwd=0,lwd.tick=1)}

if(vals.axis==TRUE){

	axis(side=2,at=pretty(vals.lim),labels=NA,cex.axis=1,lwd=0,lwd.tick=1,xpd=NA)
	y.labels<-pretty(vals.lim)
	yrs.range <- max(yrs)-min(yrs)
	text(rep(min(yrs)-(yrs.range*0.065),length(y.labels)),y.labels,labels=prettyNum(y.labels,big.mark=","),xpd=NA,cex=1,adj=1)
	}

if(hgrid==TRUE){
	ticks<-pretty(vals.lim)
	segments(rep(min(yrs),length(ticks)),ticks,rep(max(yrs),length(ticks)),ticks ,lty=2,col="lightgrey",xpd=NA)
	}

if(vgrid[1]!=FALSE){abline(v=vgrid,col="red",lty=2)	  }

lines(yrs, vals,lwd=width,col=color) # replot line to "move it in front of hgrid and v grid

# add a simple point with whichever symbol specified by pch.val
if(is.null(pch.bg)){points(yrs, vals,col=color,pch=pch.val,cex=1)} # NEW LAYOUT: larger points
# add a two-cloured point iv pch.bg is specified -> valid only for pch = 21-25
if(!is.null(pch.bg)){points(yrs, vals,col=color,pch=pch.val,cex=1,bg=pch.bg)}

}



#' addFit
#'
#' Function to plot time series pattern with minimal labels
#' @param data.df data frame used for the slope calculation that produced the coefficients (using the Year column here)
#' @param coeff are the coefficients (median from posterior or MLE values)
#' @param col line color, default is "red"
#' @param lwd line width, default is 2
#' @export


addFit <- function(data.df, coeff,col = "red",lwd = 2,lty=1){
#
# To Do: add bounds for MCMC versions

segments(min(data.df$Year), coeff$intercept ,
         max(data.df$Year), coeff$intercept + length(data.df$Year) * coeff$slope , col=col,lwd=lwd,lty=lty)


}





#' plotDistribution
#'
#' Function to plot one or more posterior distributions (density plots)
#' @param x.lab named list, each element is one set of MCMC sample
#' @param ref.lines names list, each element is one value for a vertical ref line
#' @param plot.range numeric vector of length 2, specifying the plotting range for the variable.
#' @export


plotDistribution <- function(x.lab,samples,ref.lines, plot.range = NULL){

  num.samples <- length(names(samples))

  if(num.samples < 4){col.use <- c("darkblue","red", "red");lwd.use <- c(2,1,1); lty.use <- c(1,1,2)}
  if(num.samples >= 4){col.use <- c("darkbgrey"); lwd.use <- 1; lty.use <- 1}

  kernels <- lapply(samples, function(x){dens.out <- density(x);  return(data.frame(x = dens.out$x, y = dens.out$y)) })

  dens.range <- c(0, max(unlist(lapply(kernels,function(z){ return(max(z$y))  }))))
  if(is.null(plot.range)){plot.range <- range(c(unlist(ref.lines),unlist(lapply(kernels,function(z){ return(range(z$x))  }))   ))  }

  plot(1:5,1:5,type="n", xlim = plot.range, ylim = dens.range,axes=FALSE, bty="n",xlab = x.lab, ylab = "Density")
  axis(1)

  for(i in 1:length(names(kernels))){
    lines(kernels[[i]]$x,kernels[[i]]$y,col=col.use[i],lwd=lwd.use[i],lty=lty.use[i])
  }

  if(num.samples < 4){legend("topright", legend = names(samples),bty="n",col=col.use,lwd=lwd.use,lty=lty.use)}

  for(j in 1:length(names(ref.lines)) ){
    abline(v = ref.lines[[j]],col="darkblue")
    text(ref.lines[[j]],par("usr")[4],names(ref.lines)[j],xpd=NA,adj = c(0.5,0),col="darkblue")
  }

} # end plotDistribution()




#' plotBoxes
#'
#' Function to plot one or more posterior distributions (boxplots)
#' @param box.df data frame with 5 rows (values for top whisker, top of box, median, bottom of box, bottom whisker) and 1 column for each distribution. Columns labels become axis labels.
#' @param y.lab label for the value axis
#' @param ref.lines named list, each element is one value for a vertical ref line
#' @param plot.range numeric vector of length 2, specifying the plotting range for the variable.
#' @export

plotBoxes <- function(box.df, y.lab  = "Label", ref.lines = list(BM = -25), plot.range = NULL){


num.boxes <- length(box.df)

if(is.null(plot.range)){plot.range <- range(box.df,na.rm = TRUE)  }

plot(1:5,1:5,type="n", ylim = plot.range, xlim = c(0,num.boxes+1),axes=FALSE, bty="n",xlab = "", ylab = y.lab)
axis(2,las=2)
axis(1,at = 1:dim(box.df)[2], labels = names(box.df))

for(j in 1:length(names(ref.lines)) ){
  abline(h = ref.lines[[j]],col="red")
  text(par("usr")[1],ref.lines[[j]],names(ref.lines)[j],xpd=NA,adj = c(1,0.5),col="red")
}

for(i in 1:dim(box.df)[2]){


  segments(i,box.df[1,i],i,box.df[5,i],col="darkblue",lwd=2,lend=1)
  rect(i-0.25,box.df[2,i],i+0.25,box.df[4,i],col="white",border="darkblue")
  segments(i-0.25,box.df[3,i],i+0.25,box.df[3,i],col="darkblue",lwd=4,lend=1)

}

} # end plotBoxes

##############################################################################
# function to write "Some Text" instead of a plot
na.plot <- function(txt,xp=1.6){
	plot(1870:2020,0:150,type="n",axes=FALSE, bty="none",xlab="",ylab="")
	text(1945,120,txt,cex=xp,xpd=NA,adj=0.5,col="lightgrey")
}





##############################################################################
# function to plot time series pattern with minimal labels

# TO DO: 
# - add option to label hgrid with modified values (e.g. plot logs, but label with raw numbers)




pattern.plot <- function(yrs,vals,width=1,color="grey",yrs.axis=FALSE,vals.axis=FALSE,
			vals.lim=NULL, hgrid=FALSE,vgrid=FALSE,pch.val=19,pch.bg=NULL){
options(scipen=3)

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
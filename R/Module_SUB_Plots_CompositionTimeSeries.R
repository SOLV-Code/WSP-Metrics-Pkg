# # functions to plot 2,3, or 4 components of a a total over time



###############################################################################
# function to plot stacked bars showing % composition for 2 time series, where 1 is the total and 2 is some subset

composition.plot2 <- function(total.vals,comp.vals,years,title.txt="% Component",
								legend.labels = c("Part","Total"),
								yr.range = NULL	,tracing= FALSE){

	vals.mat <- rbind(comp.vals, total.vals-comp.vals)
	if(tracing){print("--"); print(vals.mat)}
	
	vals.mat <- prop.table(vals.mat,margin=2)
	if(tracing){print("--"); print(vals.mat)}
	
	
	# OLD , causing too many problems (missing years, uneven ticks
	#ticks<-barplot(vals.mat,border=NA,space=0, axes=FALSE,legend=FALSE,col=c("dodgerblue","lightgrey"),main=title.txt)    #,xaxt="n"
	#axs.labels <- seq(min(years),max(years),by=10)
	#axs.at <- ticks[years %in% axs.labels]	
	#axis(1, at=c(0,max(ticks)+0.5), labels=c("",""), lwd.ticks=0)
	#axis(side=1,at=axs.at,labels=axs.labels)	
	
	if(is.null(yr.range)){ yr.range <- range(years) }
	
	# create panel
	plot(1:5,1:5,type="n",xlab="",ylab="",axes=FALSE,ylim=c(0,1),xlim=yr.range,main=title.txt)
	axis(1)

	rect(years,0,years+1,vals.mat[1,],col="dodgerblue",border="dodgerblue")




	abline(h=seq(0.2,0.8,by=0.2),col="white",lwd=0.8,lty=2)
	
	ylabels.x <-   par("usr")[1] - (par("usr")[2] - par("usr")[1]) * 0.01
	text(rep(ylabels.x,5),seq(0,1,by=0.2),labels=paste(seq(0,1,by=0.2)*100,"%",sep=""),cex=0.9,adj=1,xpd=TRUE)
	
	x.incr <-  (par("usr")[2]-par("usr")[1])*0.01
	x1 <- rep(par("usr")[2] + x.incr ,2)
	x2 <- x1 + 3* x.incr 
	y1 <- c(0.65,0.85)
	y2 <- y1 + 0.1
	print(x1)
	rect(x1,y1,x2,y2, col=c("dodgerblue","white"), border=c("dodgerblue","lightgrey"),xpd=NA)
	text(x2+x.incr,y1+0.05,label=legend.labels,adj=0,xpd=NA)
	
	
	
}


###############################################################################
# function to plot stacked bars showing % composition for 3 time series, where each is contributes to some total

composition.plot3 <- function(bottom.vals,middle.vals,top.vals,years,title.txt="Survey Quality",legend.labels = c("L","M","H")){

	vals.mat <- rbind(bottom.vals,middle.vals,top.vals)
	vals.mat <- prop.table(vals.mat,margin=2)

	ticks<-barplot(vals.mat,border=NA,space=0, axes=FALSE,legend=FALSE,col=c("darkblue","dodgerblue","lightgrey"),main=title.txt,xaxt="n")
	
	axs.labels <- seq(min(years),max(years),by=10)
	axs.at <- ticks[seq(1,length(ticks),by=10)]
	axis(1, at=c(0,max(ticks)+0.5), labels=c("",""), lwd.ticks=0)
	axis(side=1,at=axs.at,labels=axs.labels)
	
	abline(h=seq(0.2,0.8,by=0.2),col="white",lwd=0.8,lty=2)
	text(rep(-2,5),seq(0,1,by=0.2),labels=paste(seq(0,1,by=0.2)*100,"%",sep=""),cex=0.9,adj=1,xpd=TRUE)
	
	
	x1 <- rep(max(ticks)*1.04,3)
	x2 <- x1 *1.04
	y1 <- c(0.45,0.65,0.85)
	y2 <- y1 + 0.1
	
	rect(x1,y1,x2,y2, col=c("darkblue","dodgerblue","lightgrey"), border=c("darkblue","dodgerblue","lightgrey"),xpd=NA)
	text(x2*1.01,y1+0.05,label=c("L","M","H"),adj=0,xpd=NA)
}





###############################################################################
# function to plot stacked bars showing % composition for 4 time series, where each contributes to some total
# TO DO
  

composition.plot4 <- function(bottom.vals,middle.vals,top.vals,remaining.vals,years,title.txt="Survey Quality",legend.labels=c("B","M","T","R") ){

	vals.mat <- rbind(bottom.vals,middle.vals,top.vals,remaining.vals)
	vals.mat <- prop.table(vals.mat,margin=2)

	print(vals.mat)



	ticks<-barplot(vals.mat,border=NA,space=0, axes=FALSE,legend=FALSE,col=c("darkblue","dodgerblue","lightgrey","azure2"),main=title.txt,xaxt="n")
	axs.labels <- seq(min(years),max(years),by=10)
	axs.at <- ticks[seq(1,length(ticks),by=10)]
	axis(1, at=c(0,max(ticks)+0.5), labels=c("",""), lwd.ticks=0)
  axis(side=1,at=axs.at,labels=axs.labels,lwd.ticks=1)
	
	abline(h=seq(0.2,0.8,by=0.2),col="white",lwd=0.8,lty=2)
	text(rep(-2,5),seq(0,1,by=0.2),labels=paste(seq(0,1,by=0.2)*100,"%",sep=""),cex=0.9,adj=1,xpd=TRUE)
	
	
	x1 <- rep(max(ticks)*1.02,4)
	x2 <- x1 *1.04
	y1 <- c(0.45,0.65,0.85,1.05)
	y2 <- y1 + 0.1
	
	rect(x1,y1,x2,y2, col=c("darkblue","dodgerblue","lightgrey","azure2"), border=c("darkblue","darkblue","darkblue","black"),xpd=NA)
	text(x2*1.01,y1+0.05,label=legend.labels,adj=0,xpd=NA)
	
	
}


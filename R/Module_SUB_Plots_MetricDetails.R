
##############################################################################
# function to write plot display for current value of a status metric
# To Do:

# CAN ONLY BE ADDED TO AN EXISTING PLOT!

current.status.plot <- function(x,y, w, h,text="Metric",value=1234.56,zone="Amber",ft.size=0.8,ft.size.label=0.8,full=TRUE,perc.disp=FALSE){
# x,y are the coordinates for the top left corner of the box
# can only be added to an existing plot !


if(zone=="Green"){status.col <- "green"}
if(zone=="Amber"){status.col <- "orange"}
if(zone=="Red"){status.col <- "firebrick1"}
if(zone!="Red" && zone!="Amber" && zone!="Green"){status.col <- "lightgrey"}

if(full==TRUE){
	rect(x,y-h,x+w,y,col=status.col,border="darkblue")
	segments(x+(w*0.1),y-(h*0.5), x+(w*0.9) , y-(h*0.5),col="darkblue") 
	text(x+(w/2),y,label=text,adj=c(0.5,-0.3),col="darkblue",cex=ft.size.label,xpd=NA)
	text(x+(w/2),y-(h*0.25),label=zone,col="white",font=2,cex=ft.size,xpd=NA)
	if(!is.null(value)){ 
		if(!perc.disp){text(x+(w/2),y-(h*0.75),label=prettyNum(value,big.mark =","),col="white",font=1,cex=ft.size,xpd=NA)}
		if(perc.disp){text(x+(w/2),y-(h*0.75),label=paste(prettyNum(value,big.mark =","),"%",sep=""),col="white",font=1,cex=ft.size,xpd=NA)} # version with "%" after value
		
		} # end if(!is.null(value))
	} # end if(full==TRUE)

if(full==FALSE){
	rect(x,y-h,x+w,y,col=status.col,border="darkblue")
	text(x+(w/2),y,label=text,adj=c(0.5,-0.3),col="darkblue",cex=ft.size,xpd=NA)
	text(x+(w/2),y-(h*0.5),label=zone,col="white",font=2,cex=ft.size,xpd=NA)
	}
	
	
	
	
}









##############################################################################
# function to plot details for a status metric (sensitivity, distr, pattern)
# To Do:
# - fix plots under panel = full  for better = lower
# - fix year ranges in plotting canvas and  bottom panel
# - add option for BM changing over time

metricdetail.plot <- function(vals,yrs,val.label, panel.label,low.bm,high.bm,sens.mat=NULL,lowbm.range=NULL,highbm.range=NULL, bmrange.type="band",zonelabel.col=c("firebrick1","orange","green"),axis.type="free",decimals=0,panel="full",better="Higher",perc.displ=FALSE){
# sens.mat is a matrix with exactly 5 rows and 3 cols
# 	cols are: "text", "value", "zone"	
#   rows are the alternative data streams, with NA for any empties	
#  bmrange.type is "band" or "sidebar"
	# axis.type is "free", , "mirror", (mirror options does not handle bm ranges yet
	# panel = "full" or "toponly"
	#  could also add "0to1" option for long trend


if(better == "Lower"){ warning("Better = Lower not yet fully implemented"); stop()}

# temp
print("current val"); print(vals[length(vals)])
print("low.bm"); print(low.bm)
print("vals"); print(vals)	

no.na.vals <- na.omit(vals)
attributes(no.na.vals)<-NULL # need this to strip out the na.action attributes left by na.omit


if(axis.type=="free"){
	
	val.scale<-max(no.na.vals,low.bm,high.bm)/100
	if(!is.null(lowbm.range)){val.scale<-max(no.na.vals,low.bm,high.bm,lowbm.range)/100}
	if(!is.null(highbm.range)){val.scale<-max(no.na.vals,low.bm,high.bm,highbm.range)/100}
	if(!is.null(highbm.range)&&!is.null(highbm.range)){val.scale<-max(no.na.vals,low.bm,high.bm,lowbm.range,highbm.range)/100}

	
print("val.scale"); print(val.scale)	
	
	
	# rescale values from 0 to 100
	low.bm <-low.bm/val.scale
	high.bm <-high.bm/val.scale
	vals <- vals/val.scale
	no.na.vals <- no.na.vals/val.scale
	
	shift.val<-0
	
	}


if(axis.type=="mirror"){
	
	val.range<- max(abs(c(no.na.vals,low.bm,high.bm)))
	#print(val.range)
	
	val.scale<- val.range/50
	
	# rescale values from -50 to 50
	low.bm <-low.bm/val.scale
	high.bm <-high.bm/val.scale
	vals <- vals/val.scale
	no.na.vals <- no.na.vals/val.scale
	
	shift.val<-50
	
	
	}
	

# set up a blank plot as a canvas with coordinates; 
plot(1870:2020,0:150,type="n",axes=FALSE, bty="none",xlab="",ylab="")

xc<-1870

mtext(text=panel.label,outer=FALSE,line=1,font=2,cex=1.3,col="darkblue")

# SECTION 1: CURRENT STATUS

# use last entry in vals to identify current status and plot the status box
current.value <- vals[length(vals)]

# temp
print("current"); print(current.value)
print("low.bm"); print(low.bm)


if(better=="Higher"){
	if(current.value<=low.bm){ current.zone <- "Red"}
	if(current.value>high.bm){ current.zone <- "Green"}
	if(current.value>low.bm && current.value<=high.bm){ current.zone <- "Amber"}
}


if(better=="Lower"){
	if(current.value<=low.bm){ current.zone <- "Green"}
	if(current.value>high.bm){ current.zone <- "Red"}
	if(current.value>low.bm && current.value<=high.bm){ current.zone <- "Amber"}
}


print("current.zone");print(current.zone)

# plot main status metric
current.status.plot(x=xc+2,y=150,w=25, h=35,text=val.label,value=round(current.value*val.scale,digits=2),zone=current.zone, ft.size=1.2,perc.disp=perc.displ)  


# add min, med, max
text(rep(xc+40,3),c(148,135,123),c("Max","Median","Min"),col="darkblue", cex=0.7)
# add or don't add "%" after each value
if(!perc.displ){text(rep(xc+40,3),c(142,129,117),prettyNum(round(c(max(no.na.vals*val.scale),median(no.na.vals*val.scale),min(no.na.vals*val.scale)),digits=decimals),big.mark=","),col="darkblue",font=2, cex=0.8)}
if(perc.displ){text(rep(xc+40,3),c(142,129,117),paste(prettyNum(round(c(max(no.na.vals*val.scale),median(no.na.vals*val.scale),min(no.na.vals*val.scale)),digits=decimals),big.mark=","),"%",sep=""),col="darkblue",font=2, cex=0.8)}




# cycle through and plot status display for alternative data streams (sensitivity analyses)
x.box<-seq(68,160,by=18)


if(!is.null(sens.mat)){
	for(datastream in 1:dim(sens.mat)[1]){
		#print(datastream)
		if(!is.na(sens.mat[datastream,"value"])){current.status.plot(x=xc+x.box[datastream],y=140,w=15, h=23,text=sens.mat[datastream,"text"],value=sens.mat[datastream,"value"],zone=sens.mat[datastream,"zone"], ft.size=0.8,ft.size.label=0.65,perc.disp=perc.displ) }
		}
		# add a line and label
		segments(xc+65,148,xc+160,148,col="lightgrey",lwd=1.5)
		text(xc+65,149,"Alternative Data Streams",adj=c(0,0), col="darkblue")
		}




if(panel=="toponly"){abline(h=109,col="lightgrey",lwd=2)}
	


if(panel=="full"){
	
	
# separate top and bottom
abline(h=109,col="lightgrey",lwd=3)
segments(xc+13.5,114.5,xc+13.5,109,col="lightgrey",lwd=3,lend="butt")
	
	
	

# SECTION 2 : DISTRIBUTION AND PATTERN OVER TIME



# add main BM lines
segments(1965,low.bm+shift.val,2020,low.bm+shift.val, lwd=1,lty=1,col="red")
text(1963,low.bm+shift.val,"Lower\nBM",cex=0.8,col="red",adj=1)
text(2022,low.bm+shift.val,prettyNum(low.bm*val.scale,big.mark=","),cex=0.8,col="red",xpd=NA,adj=0)
segments(1965,high.bm+shift.val,2020,high.bm+shift.val, lwd=1,lty=1,col="green")
text(1963,high.bm+shift.val,"Upper\nBM",cex=0.8,col="green",adj=1)
text(2022,high.bm+shift.val,prettyNum(high.bm*val.scale,big.mark=","),cex=0.8,col="green",xpd=NA,adj=0)

# add 0 line
if(axis.type=="mirror"){
	segments(1965,shift.val,2020,shift.val, lwd=1,lty=1,col="darkgrey")
	text(2022,shift.val,"0",cex=0.8,col="darkgrey",xpd=NA,adj=0)
}



# add low BM range if supplied
if(!is.null(lowbm.range)){
	lowbm.range<-lowbm.range/val.scale
	if(bmrange.type=="band"){segments(rep(1965,2),lowbm.range,rep(2020,2),lowbm.range, lwd=1,lty=2,col="firebrick1")}
	if(bmrange.type=="sidebar"){rect(2017.5,lowbm.range[1],2019,lowbm.range[2],col="firebrick1", border="firebrick1") } 
	}	

# add high BM range if supplied
if(!is.null(highbm.range)){
	highbm.range<-highbm.range/val.scale
	if(bmrange.type=="band"){segments(rep(1965,2),highbm.range,rep(2020,2),highbm.range, lwd=1,lty=2,col="green")}
	if(bmrange.type=="sidebar"){rect(2017.5,highbm.range[1],2019,highbm.range[2],col="green", border="green") } 
}	



# add trend line 
lines(yrs,vals+shift.val,col="grey",lwd=2)
points(yrs,vals+shift.val,pch=21,col="darkblue",bg="grey")
axis(side=1,at=c(1970,1990,2010),labels=FALSE,xpd=NA)
text(x=c(1970,1990,2010),y=rep(-15,3),labels=c(1970,1990,2010),xpd=NA,cex=0.8)
text(1990,-24,labels="Is there a pattern?",xpd=NA,font=2,cex=0.8)



# add distribution summaries

# Handle special cases:  max < Upper BM, Min > Lower BM etc
if(min(no.na.vals)<=low.bm){min.label<-"Min";min.val<-min(no.na.vals)*val.scale}
if(min(no.na.vals)>low.bm){min.label<-""; min.val<-0}
if(max(no.na.vals)<=high.bm){max.label<-"";max.val<-"x"}
if(max(no.na.vals)>high.bm){max.label<-"Max";max.val<-max(no.na.vals)*val.scale}

#plot lines and label

ys <- c(0,100/3,100/3*2,100)
segments(rep(xc+2,5),c(min(ys),ys),c(xc+2,rep(xc+8,4)),c(max(ys),ys),col="darkblue")
text(rep(xc,4),ys,c(min.label,"Lower\nBM","Upper\nBM",max.label),adj=1,cex=0.8,col="darkblue",xpd=NA)

# add values
text(rep(xc+9,4),ys,prettyNum(c(min.val,low.bm*val.scale,high.bm*val.scale,max.val),big.mark=","), 
	cex=0.8,col="darkblue",xpd=NA,adj=0)
text(xc+8,-24,labels="Value Ranges",xpd=NA,font=2,cex=0.8)

# calculate values for horizontal bars
above.count <- length(no.na.vals[no.na.vals>=high.bm])
between.count <- length(no.na.vals[no.na.vals>=low.bm])-above.count
below.count <-   length(no.na.vals[no.na.vals<low.bm])
counts <- c(below.count,between.count,above.count)

# plot values, labels, and corresponding bars
bars.mid <-c(100/6,100/2,100-(100/6))
text(rep(1900,3),bars.mid,counts,col="darkblue",font=2)
text(1930,-24,labels="How often has this happened?",xpd=NA,font=2,cex=0.8)
text(rep(1885,3),bars.mid,c("Red","Amber","Green"),col=zonelabel.col,font=1)
scaled.counts <-counts/max(counts)*45
rect(rep(1905,3),bars.mid-6,1905+scaled.counts,bars.mid+6,lwd=1,border="darkblue",
		col=c("firebrick1","orange","green"))


} # end if panel="full"
		
		
} # end plot metric detail



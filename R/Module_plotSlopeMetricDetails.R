#' plotSlopeMetricDetails
#'
#' this function creates a 3-panel diagnostic plot for the slope metric for 1 specified CU for 1 specified year
#' @param cu.file a data frame with CU time series. This is the same data file used by calculateMetricsByCU(). See details there.
#' @param metrics.results output from  calcMetrics()
#' @param status.results output from generateRapidStatus()
#' @param cu.plot CU ID for the conservation unit to be plotted
#' @param year.plot year for which to plot the slope diagnostics
#' @param year.range time window to be used for x-axis in panel 1, showing the time series.
#' @param spn.label label to use for spawner axis in he plots. Default is "Wild Spawners"
#' @param cu.label Short label to use for the CU. Default is to use CU_ID provided in argument cu.plot
#' @keywords slope metric details
#' @export



plotSlopeMetricDetails <- function(cu.file, metrics.result, status.results,
                       cu.plot,year.plot, year.range = c(1960,2030),
					   spn.label = "Wild Spawners",
                       cu.label = NULL){


# FOR NOW: Plotting smoothed log series in panel 2
# PROPER: Panel 2 should respond to settings for TREND_SMOOTH and TREND_LOG in spec file

  if(is.null(cu.label)){cu.label <- cu.plot}

  graphics::layout(mat=matrix(c(1,1,2,3),ncol=2,byrow=TRUE),heights = c(1,1))



  # extract CU data, specs, and metrics
  status.summary.df <- status.results$SummaryTable %>% dplyr::filter(CU_ID == cu.plot)
  specs.used <- metrics.results$Specs  %>% dplyr::filter(CU_ID == cu.plot)
  cu.abd <- cu.file %>% dplyr::filter(CU_ID == cu.plot) %>% select(Year,Escapement_Wild=SpnForAbd_Wild)
  yrs.idx <- cu.abd$Year >= year.range[1] & cu.abd$Year <= year.range[2]
  yrs.src <- data.frame(Year=min(cu.abd$Year):max(cu.abd$Year))
  cu.abd <- left_join(yrs.src,cu.abd ,by="Year")
  cu.avggen <- specs.used %>% select(Avg_Gen) %>% unlist()



  # PANEL 1: regular time series (MODIFIED PANEL 1 FROM STANDARD STATUS DASHBOARD)
  # calculate running geomean
  gm.in <- log(cu.abd[,2])
  gm.out <- exp(stats::filter(gm.in,rep(1/cu.avggen,times =cu.avggen),sides = 1))

  ylim <- c(0, max(unlist(cu.abd[yrs.idx,2]),na.rm=TRUE))

  axis.scale <- 1
  axis.scale.label <- ""

  if(ylim[2] >= 10^3 & ylim[2] < 10^4){ axis.scale <- 100; axis.scale.label <- "(100s)"   }
  if(ylim[2] >= 10^4 & ylim[2] < 10^6){ axis.scale <- 1000; axis.scale.label <- "(1000s)"   }
  if(ylim[2] >= 10^6 ){axis.scale <- 10^6; axis.scale.label <- "(Mill)"   }

  y.label.use <- paste(spn.label,axis.scale.label)


  plot(cu.abd$Year,cu.abd[,2]/axis.scale,col="darkblue",pch=19, lwd=3, bty="n",
       xlim = main.yrs.plot, cex=1.3, ylim = ylim/axis.scale,
       xlab = "Year", ylab = y.label.use,type="o",axes = FALSE,cex.lab=1.4
  )



  # highlight 3 gen time window

  slope.yrs <- (year.plot + 1 - cu.avggen *3) : year.plot

  rect(min(slope.yrs), par("usr")[3],
       year.plot, par("usr")[4], col="lightgrey",
       border = "lightgrey")
  abline(v=year.plot,col="darkgrey",lwd=4)


  axis(1,at = seq(main.yrs.plot[1],main.yrs.plot[2],by=5),  cex.axis=1.5)
  axis(2,las=1,  cex.axis=1.5)


  # replot the series and gen avg
  lines(cu.abd$Year,cu.abd[,2]/axis.scale,col="darkblue",pch=19, lwd=3,
        cex=1.3, type="o")

  lines(cu.abd$Year,gm.out/axis.scale,type="o",col="red",lwd=4,pch=15,cex=0.8)

  title(main ="Spawner Time Series", cex.main = 1.8,col.main="darkblue",line=1)

  text(par("usr")[2],par("usr")[4]*1.15, labels= year.plot,adj=1,col="darkblue",font=2,xpd=NA,cex=3)
  text(par("usr")[1],par("usr")[4]*1.15, labels= cu.label,adj=0,col="darkblue",font=2,xpd=NA,cex=3)

  pt.idx <- cu.abd$Year == year.plot
  points(cu.abd$Year[pt.idx],cu.abd[pt.idx,2]/axis.scale,
         col="red",pch=21, lwd=3, cex=1.9, bg="white")


  # PANEL 2: LOG-smoothed series and resulting slope
  # FIX SO IT RESPONDS TO SPECS

  slope.idx <- cu.abd$Year %in% slope.yrs

  plot(cu.abd$Year[slope.idx],log(gm.out[slope.idx]),
       type="o",col="red",lwd=4,pch=15, bty="n",
       xlim = range(slope.yrs), #+c(-1,1),
       cex=1.3,
       xlab = "Year", ylab = "Log of Generation Average",axes = FALSE,cex.lab=1.4
  )
  axis(1, at = slope.yrs)
  axis(2, las=1)


  coeff.fitted <- calcPercChangeSimple(unlist(log(gm.out[slope.idx])))
  coeff.fitted

  segments(min(slope.yrs), coeff.fitted$intercept ,
           max(slope.yrs), coeff.fitted$intercept + sum(slope.idx) * coeff.fitted$slope,
           col = "red",
           lwd = 3, lty = 2)

  title(main ="Fitted 3 Generation Slope", cex.main = 1.8,col.main="darkblue",line=1)



  # PANEL 3: SLOE METRIC PATTERN (PANEL 4 IN STATUS DASHBOARD)


  ylim.use <- range(-55,50,status.summary.df$PercChange,na.rm=TRUE)

  plot(status.summary.df$Year,status.summary.df$PercChange,col="darkblue",pch=19, lwd=3, bty="n",
       xlim = range(status.summary.df$Year), cex=1.3, ylim = ylim.use,
       xlab = "Year", ylab = "% Change - 3 Gen",type="o",axes = FALSE,cex.lab=1.4)
  axis(1,cex.axis=1.4)
  axis(2,cex.axis=1.4,las=1)
  title(main = "Percent Change Metric", cex.main = 1.8,col.main="darkblue",line=1)

  rect(par("usr")[1],specs.used$PercChange_LBM, par("usr")[2],specs.used$PercChange_UBM, col=amber.use,
       border = amber.use)
  rect(par("usr")[1],specs.used$PercChange_UBM, par("usr")[2],par("usr")[4], col=green.use,
       border = green.use)
  rect(par("usr")[1],specs.used$PercChange_LBM, par("usr")[2],par("usr")[3], col=red.use,
       border = red.use)


  first.retro.yr <- min(status.summary.df$Year )
  abline(v=first.retro.yr,col="darkgrey",lty=2,lwd=4)

  abline(h=0,col="darkgrey",lwd=2,lty=2); text(par("usr")[2],0,"Same",cex=1.4,font=1,adj=c(1,0),xpd=NA)
  if(par("usr")[4] > 100){abline(h=100,col="darkgrey",lwd=2,lty=2); text(par("usr")[2],100,"Double",cex=1.4,font=1,adj=c(1,0),xpd=NA)}
  if(par("usr")[3] < -50){abline(h=-50,col="darkgrey",lwd=2,lty=2); text(par("usr")[2],-50,"Half",cex=1.4,font=1,adj=c(1,0,xpd=NA))}


  lines(status.summary.df$Year,status.summary.df$PercChange,col="darkblue",pch=19, lwd=3, cex=1.3, type="o")

  status.summary.df.sub <- status.summary.df %>% dplyr::filter(Year %in% spn.add.yrs)
  points(status.summary.df.sub$Year,status.summary.df.sub$PercChange,
         col="darkblue",pch=21, lwd=3, cex=1.4, bg="white")


  pt.idx <- status.summary.df$Year == year.plot
  points(status.summary.df$Year[pt.idx],status.summary.df$PercChange[pt.idx],
         col="red",pch=21, lwd=3, cex=1.9, bg="white")



  }



#' calculateMetricsByCU 
#'
#' this function loops through a data set of CU spawner time series and applies the calcMetrics() function
#' @param cu.file a data frame with CU time series. REQUIRED variables are  CU_ID (e.g., SEL-24-05), CU_Name (e.g., Meziadin)	Species (e.g., Sockeye), Year,	SpnForAbd_Wild	(Spawner estimate used for abundance metrics), SpnForTrend_Wild	(spawner estimate used for trend metrics) Abd_StartYr (first year to use for abundance metric),	UseYear (T/F whether this year's estimate can be used for metric calculations). Note, for most CUs, SpnForAbd_Wild and SpnForTrend_Wild have the same values. However, for Fraser Sockeye CUs, effective female spawners are used for trend metrics and effective total spawners for abundance metrics). OPTIONAL variables that are not used for metric calculations but may be used in various diagnostics that provide context:  SpnForAbd_Total,	SpnForTrend_Total,	Recruits_Wild,	Recruits_Total..
#' @param cu.info a data frame with specifications for each CU. Required variables are Species (e.g., Sockeye), Species_Short (e.g., Sk), CU_ID (e.g., SEL-24-05), CU_Name (e.g., Meziadin),DataQualkIdx (one of Abs_Abd or Rel_Idx), AbdMetric (T/F is relative abundance metric applicable for this CU?), AbsAbdMetric (T/F is absolute abundance metric applicable for this CU?), ShortTrendMetric (T/F for x generation slope metric), LongTrendMetric (T/F for long-term trend metric),PercentileMetric (T/F for percentile metric, which is not currently used in rapid status decision tree), Avg_Gen (integer with average generation time),	Cyclic (T/F whether CU is cyclic),	Cyc_Dom and Cyc_Dom_Year (index and base year for dominant cycle, see below), TrendNumGen (number of generations for slope metric),	TrendExtraYears (any extra years for slope metric? For WSP =0, for some COSEWIC = 1), 	TrendLog (T/F for log-transform before slope calc. WSP: True),	TrendSmooth (T/F for generation smoothing before slope calc. WSP: T for most sockeye, Coho, F for most Chinook), AvgType (type of average used for generational average. WSP: always 'geomean'),	AvgSmooth (T/F for smoothing before average. WSP :almost always F),	AvgRecentExcl (T/F for excluding the most recent generation from long-term average. WSP: always F)	LongTrendMinYears (minimum number of years for long-term calculation. WSP: always 15),	RelAbd_AvgData (EXPLAIN THIS ONE? STILL USED?),	RelAbd_LBM and RelAbd_UBM (Lower and upper benchmark for Relative Abundance Metric. WSP: generally Sgen and 80% Smsy, where available),	AbsAbd_LBM	and AbsAbd_UBM (lower and upper benchmarks for absolute abundance metric. WSP: 1,000 and 10,000), LongTrend_LBM	and LongTrend_UBM (lower and upper benchmarks for long-term trend metric. WSP: 0.5 and 0.75), PercChange_LBM and PercChange_UBM	(lower and upper benchmark for slope metric. WSP: -25 and -15), ProbDeclBelowLBM_LBM and ProbDeclBelowLBM_UBM (lower and upper benchmarks for probability of decline. WSP: NA),	Trends_StartYr (first year for which trend time series can be used), Abd_StartYr (first year for which abundance time series can be used).  Note: some CUs use different time series for trend metrics and abundance metrics (see above).
#' @param cu.cyclic.bm if the data set includes highly cyclic Fraser sockeye CUs, an extra input file is needed. This is a data frame with the following required variables: CU_ID, CU_Name,Cycle,cyc,RelAbd_LBM,RelAbd_UBM. 'Cycle' is the base year (one of 1950-1953) and cyc is the corresponding cycle line index (1951=1,1952=2,1953=3,1950=4)
#' @param retro.start.use first year for the retrospective metrics calculations (if data are available)
#' @param out.label label to use in the filenames for the output
#' @param out.filepath folder for storing the output files 
#' @keywords trend
#' @export





calculateMetricsByCU <- function(cu.file,cu.info,cyclic.cu.bm = NULL,
                                 retro.start.use = 1995,
								 out.label = "MetricsOut",
								 out.filepath = ""){
  
# retro.start.use start year for retrospective calculation of metrics  
# WARNING: THIS MUST INCLUDE AT LEAST 4 YEARS (so that  all 4 cycle-specific BM calc get used to populate outputs below; crashes otherwise!)
  
  
library("tidyverse")

# fix the CU_ID ("_" vs. "-") (just in case, this is a recurring issue)
cu.file$CU_ID <- gsub("_","-",cu.file$CU_ID)
cu.info$CU_ID <- gsub("_","-",cu.info$CU_ID)
cyclic.cu.bm$CU_ID <- gsub("_","-",cyclic.cu.bm$CU_ID)

cu.list <- unique(cu.file[,c("Species","CU_Name","CU_ID")])
cu.list

print("----------------")
print(cu.list)
print("----------------")

# if have any cyclic CUs in the set, need an extra input file with cyclic BM
if(any(cu.info$Cyclic) & is.null(cyclic.cu.bm)){
  warning("Your input file contains one or more cyclic CUs. An input filr with cyclic benchmarks is needed!")
  stop()}


# clear the output (need this if inside a function?)
#if(exists("metrics.cu.out")){rm(metrics.cu.out)}

start.time <- proc.time()

for(i in 1:dim(cu.list)[1]){
  print("----------------------------")
  print(i)
  cu.id <- cu.list$CU_ID[i] 
  cu.name <- cu.list$CU_Name[i] 
  cu.species <- cu.list$Species[i]  

  print(cu.name)
  print(cu.id)

  cu.lookup.sub <- dplyr::filter(cu.info,CU_ID == cu.id)
  cu.lookup.sub

print("cu.lookup.sub --")
print(cu.lookup.sub)

if( dim(cu.lookup.sub)[1]==1){ # do only if have exactly 1 matching CU_ID in the lookup file



  cu.avggen <- cu.lookup.sub$Avg_Gen
  cu.avggen

  cu.slope.specs <- list(num.gen = cu.lookup.sub$TrendNumGen,
                         extra.yrs =cu.lookup.sub$TrendExtraYears ,
                         filter.sides=1,  # backward looking
                         slope.smooth = cu.lookup.sub$TrendSmooth ,
                         log.transform = cu.lookup.sub$TrendLog,
                         out.exp = TRUE,  # exponentiate the log-smoothed series?
                         na.rm=FALSE)
  print("cu.slope.specs")
  print(cu.slope.specs)

  cu.avg.specs <- list(avg.type =cu.lookup.sub$AvgType ,
                       recent.excl= cu.lookup.sub$AvgRecentExcl,  # use all for the long-term avg?
                       lt.smooth=cu.lookup.sub$AvgSmooth,
                       rel.avg.type = cu.lookup.sub$RelAbd_AvgData,
                       min.lt.yrs = cu.lookup.sub$LongTrendMinYears,
                       min.perc.yrs = cu.lookup.sub$LongTrendMinYears
                       # USE SAME SETTING AS LT TREND FOR NOW -> DISCUSS
                       # https://github.com/SOLV-Code/SOS-Data-Processing/issues/52
                       )
  print("cu.avg.specs")
  print(cu.avg.specs)

  cu.bm <- list(RelAbd = cu.lookup.sub[,c("RelAbd_LBM","RelAbd_UBM")],
                AbsAbd = cu.lookup.sub[,c("AbsAbd_LBM","AbsAbd_UBM")],
                LongTrend = cu.lookup.sub[,c("LongTrend_LBM","LongTrend_UBM")],
                PercChange = cu.lookup.sub[,c("PercChange_LBM","PercChange_UBM")],
                ProbDeclBelowLBM = cu.lookup.sub[,c("ProbDeclBelowLBM_LBM","ProbDeclBelowLBM_UBM")],
                Percentile = c(0.25,0.5) #using hardwired for now-> DISCUSS
                # https://github.com/SOLV-Code/SOS-Data-Processing/issues/52
                )
  print("cu.bm")
  print(cu.bm)

  data.sub <- cu.file %>% filter(CU_ID == cu.id)

  print("data used -----")
  print(head(data.sub))

  # GP ADDED March 2023: Now that data series were trimmed, need to add a bunch of NA years before the retro start (else the MCMC slope calc crashes)
  if(retro.start.use-10 < min(data.sub$Year)){
    yrs.add <- data.frame(CU_ID = cu.id, Year = (retro.start.use-10):min(data.sub$Year)-1)
    yrs.add

    data.sub <- bind_rows(data.sub, yrs.add) %>% arrange(Year)
    data.sub
  }


  if(cu.lookup.sub$Cyclic==TRUE) {cyclic.bm.sub <- cyclic.cu.bm[cyclic.cu.bm$CU_ID==cu.id,] }
  if(cu.lookup.sub$Cyclic==FALSE){cyclic.bm.sub <- NA }


  series.list <- c("SpnForAbd_Wild","SpnForTrend_Wild" )	

  for(series.do in series.list){  #"SpnForAbd_Total","SpnForTrend_Total",

print("---- series ---")
  print(series.do)

  cu.series <- data.sub[,series.do] %>% unlist()
  cu.series[!is.finite(cu.series)] <- NA
  cu.yrs <- data.sub[,"Year"] %>% unlist()


print(cu.series)
print(cu.yrs)

  #cu.series
  #cu.yrs

# calculate metrics only if have at least 5 non-zero values
  if(sum(!is.na(cu.series))>5 &  sum(cu.series>0,na.rm=TRUE)>5){

	print("DOING CALCS")

  metrics.tmp <- WSPMetrics::calcMetrics(series.in = cu.series,
                              yrs.in = cu.yrs,
                              gen.in = cu.avggen,
                              stk.label = cu.name,
                              species.label = cu.species,
                              series.label = series.do,
                              slope.specs = cu.slope.specs,
                              avg.specs = cu.avg.specs,
                              metric.bm =  cu.bm,
                              retro.start = retro.start.use,
                              tracing = FALSE)





print("Flag 1--------------------")


	print(str(cu.id))
	print(str(metrics.tmp))




    metrics.tmp <- cbind(CU_ID = cu.id, metrics.tmp)

  # ********************************* BMac changes ***********************************





#	CHECK THAT LOOKUPS AND INPUT VALUES ARE FED IN PROPERLY

	if(cu.lookup.sub$Cyclic==TRUE){
	  
    print(paste("starting  BMAC changes for", cu.name, series.do))	  
	  
        # Set to NA first (AbsAbd BMs don't need to be changed)
        metrics.tmp[metrics.tmp$Metric == "RelAbd", c("Value","LBM","UBM","Status")] <- NA
       # metrics.tmp[metrics.tmp$Metric == "AbsAbd", c("Value","Status")] <- NA   # Changed Nov 9 2021 as we decided to use the geom average for
                                                                                  # cyclic CUs, which is what is used for the regular CUs

        # Dominant cycle Benchmarks for RelAbd
       # cycs <-rep(seq(from=1,to=cu.avggen), length=length(cu.yrs))   CHANGES MAY 28 2021 to REFER TO THE CYCLE LINE INSTEAD OF YEAR SO WORKS ON RETRO
        cycs <- cu.lookup.sub$Cyc_Dom
        #cyc.yrs <-  cu.yrs[(cycs == cycs[which(cu.yrs==cu.lookup.sub$Cyc_Dom)] & cu.yrs >= retro.start.use)]
        all.cyc.yrs <- seq(from=cyclic.bm.sub$Cycle[cyclic.bm.sub$cyc == cycs], to=last(cu.yrs), by=cu.avggen)
        cyc.yrs <- all.cyc.yrs[all.cyc.yrs >= retro.start.use]

        # puts dominant BM into all rows
        metrics.tmp[metrics.tmp$Metric == "RelAbd", c("LBM","UBM")] <- cyclic.bm.sub[(cyclic.bm.sub$cyc == cycs) ,c("RelAbd_LBM","RelAbd_UBM")]


        # Most recent Dominant cycle Abundance for RelAbd metric Value
        metrics.tmp[metrics.tmp$Year %in% cyc.yrs & metrics.tmp$Metric == "RelAbd", "Value"] <- cu.series[cu.yrs%in%cyc.yrs]
        sub <- metrics.tmp %>% filter(Metric == "RelAbd") %>%
                               fill(Value, .direction="down")
        # insert early years if not filled in
        #first.cycyr <- last(cu.yrs[(cycs == cycs[which(cu.yrs==cu.lookup.sub$Cyc_Dom)] & cu.yrs < retro.start.use)])
        first.cycyr <- last(all.cyc.yrs[all.cyc.yrs < retro.start.use])

        if(is.na(first(sub$Value))) sub$Value[sub$Year<first(cyc.yrs)] <-cu.series[cu.yrs==first.cycyr]

        first.abund <- cu.series[cu.yrs==first.cycyr]
        metrics.tmp <- rbind(sub, filter(metrics.tmp, Metric != "RelAbd"))

        # AbsAbd   *********************** BMAC replaced this Oct 22 2021 to DOMINANT cycle abundance only for COSEWIC criteria BUT NO LONGER USING THIS SEE ABOVE 09/11/2021
        #metrics.tmp$Value[metrics.tmp$Metric == "AbsAbd"] <-  sub$Value  # cu.series[cu.yrs>=retro.start.use]


        # Insert Statuses
        metrics.tmp <- metrics.tmp %>%
                               mutate(Status = ifelse(Value <= LBM, "Red", ifelse(Value > UBM, "Green", "Amber")))
        
        
        print(paste("ending  BMAC changes for", cu.name, series.do))
        
  } # end cyclic



  # ********************************** End BMAC changes ******************************

    
	
	
	# not working inside function
	#if(exists("metrics.cu.out")){metrics.cu.out <- rbind(metrics.cu.out,metrics.tmp)  }
    #if(!exists("metrics.cu.out")){metrics.cu.out <- metrics.tmp }

	# not handling first instance properly, b/c i = 1 means first CU, but do 2 series for that CU, first gets overwritten
	#if(i >1){metrics.cu.out <- rbind(metrics.cu.out,metrics.tmp)  }
	#if(i == 1){metrics.cu.out <- metrics.tmp }

	#this fixes it
	start.new.output <- i == 1 & series.do == series.list[1]
	if(!start.new.output){metrics.cu.out <- rbind(metrics.cu.out,metrics.tmp)  }
	if(start.new.output){metrics.cu.out <- metrics.tmp }


    if(cu.slope.specs$slope.smooth){
      #log transform, smooth, and convert back

      trend.series <- WSPMetrics::smoothSeries(vec.in = cu.series,gen = cu.avggen,
                                   filter.sides=cu.slope.specs$filter.sides,
                                   log.transform = cu.slope.specs$log.transform,
                                   out.exp = cu.slope.specs$out.exp,
                                   na.rm=cu.slope.specs$na.rm)

    }


    if(!cu.slope.specs$slope.smooth){trend.series <- cu.series}


  } # end if have at least 5 obs
  } # end looping through series
  } # end if have  matching CU_ID in the lookup file

  } # end looping through CUs

print(paste("last row done =",i))

print(head(metrics.cu.out))




#------------------------------------------------------------------------------------
# clear out metrics that are not meaningful (e.g. abs BM on data for trends)
# (already skipping the _Total series above, just doing _Wild)

# adapted from BMac code
#filter( !(grepl("Abd", Label) & (grepl("LongTrend", Metric) | grepl("Perc", Metric) |  grepl("Decl", Metric)) )) %>%
#filter( !(grepl("Trend", Label) & (grepl("RelBM", Metric) | grepl("AbsBM", Metric)) ))
metrics.cu.out.cleaned  <-  rbind(
        metrics.cu.out %>% dplyr::filter(grepl("Abd", Label) & (grepl("RelAbd", Metric) | grepl("AbsAbd", Metric))),
        metrics.cu.out %>% dplyr::filter(grepl("Trend", Label) & !(grepl("RelAbd", Metric) | grepl("AbsAbd", Metric)))
                                             ) %>%
        left_join(cu.info %>% select(CU_ID,DataQualkIdx) %>% rename(Data_Type = DataQualkIdx) , by ="CU_ID")




#####################################
# METRIC USABILITY ADJUSTMENTS NEW (GP Revised 2023-05-31)
# linking it directly to cu info lookup file
# added a new column there "AbsAbdMetric" for future cases where want AbdAbd but not RelAbd (as per BMac discussion)
# for now settings in AbdMetric and AbsAbdMetric are the same

not.abd.list <- cu.info$CU_ID[!cu.info$AbdMetric]
not.absabd.list <- cu.info$CU_ID[!cu.info$AbsAbdMetric]
not.shorttrend.list <- cu.info$CU_ID[!cu.info$ShortTrendMetric]
not.longtrend.list <- cu.info$CU_ID[!cu.info$LongTrendMetric]

abd.fix.idx <- grepl("RelAbd", metrics.cu.out.cleaned$Metric) & unlist(metrics.cu.out.cleaned$CU_ID) %in%  not.abd.list
absabd.fix.idx <- grepl("AbsAbd", metrics.cu.out.cleaned$Metric) & unlist(metrics.cu.out.cleaned$CU_ID) %in%  not.absabd.list
shorttrend.fix.idx <- grepl("ShortTrend", metrics.cu.out.cleaned$Metric) & unlist(metrics.cu.out.cleaned$CU_ID) %in%  not.shorttrend.list
longtrend.fix.idx <- grepl("LongTrend", metrics.cu.out.cleaned$Metric) & unlist(metrics.cu.out.cleaned$CU_ID) %in%  not.longtrend.list


write.csv(metrics.cu.out.cleaned,
 paste0(out.filepath,"/",out.label,"_METRICS_FILE_BY_CU_PRE_CLEAN.csv"),
 row.names=FALSE)

# GP New 2024-05-28: Extract Gen Avg  so can merge back in later (get deleted below of AbsAbd/RelAbd metrics are turned off)
gen.avg.used.df <- metrics.cu.out.cleaned %>% dplyr::filter(Metric == "RelAbd") %>% select(CU_ID, Year,Value)
write.csv(gen.avg.used.df,  paste0(out.filepath,"/",out.label,"_GenerationalAvg_Values.csv"),row.names=FALSE)

metrics.cu.out.cleaned[abd.fix.idx,c("Value","Status")] <- c(NA, NA)
metrics.cu.out.cleaned[absabd.fix.idx,c("Value","Status")] <- c(NA, NA)
metrics.cu.out.cleaned[shorttrend.fix.idx,c("Value","Status")] <- c(NA, NA)
metrics.cu.out.cleaned[longtrend.fix.idx,c("Value","Status")] <- c(NA, NA)

print("Running of calcMetrics() took:")
print( proc.time() - start.time)


write.csv(metrics.cu.out.cleaned, paste0(out.filepath,"/",out.label,"_RetrospectiveMetrics.csv"), row.names=FALSE)

return(list(Metrics = metrics.cu.out.cleaned,
GenAvg = gen.avg.used.df,
Specs = cu.info,
Data = cu.file) )


}
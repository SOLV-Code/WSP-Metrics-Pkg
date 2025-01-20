
#' generateRapidStatus
#'
#' this function reorganizes the outputs from prepDataForRapidStatus(), applies the function applyRapidStatusTree(), and then does some post-processing. Note: "proxy" statuses for some CUs are being explored, but are not yet included in this function!
#' @param cu.info a data frame with specifications for each CU. For details, see help file for calculateMetricsByCU().
#' @param cu.data a data frame with CU time series. This is the same data frame used for the cu.file argument in the call to calculateMetricsbyCU() and is included as the $Data list element in the output from that function. For details, see that help file.
#' @param publ.status.src a data frame with Red/Amber/Green designations from integrated status workshops. Required variables are CU_ID,Year, Status. Status is one of Red, RedAmber, Amber, AmberGreen, or Green.
#' @param retro.values data frame with retrospective values for individual WSP status metrics (e.g., long-term trend). Part of output from prepDataForRapidStatus().
#' @param retro.status data frame with retrospective status categories for individual WSP status metrics (e.g., long-term trend). Part of output from prepDataForRapidStatus().
#' @param metrics.long long form version of retrospective metric results. Part of output from prepDataForRapidStatus().
#' @param group.var variable for grouping output. Default is 'Species'.
#' @param out.label label to use in the filenames for the output
#' @param out.filepath folder for storing the output files 
#' @keywords trend
#' @export



generateRapidStatus <- function(cu.info,cu.data,publ.status.src,
						retro.values, retro.status, metrics.long,
						group.var = "Species",
						out.label = "RapidStatusOut",
								 out.filepath = ""){

library(tidyverse)


# fix the CU_ID ("_" vs. "-") (just in case, this is a recurring issue)
cu.info$CU_ID <- gsub("_","-",cu.info$CU_ID)
cu.data$CU_ID <- gsub("_","-",cu.data$CU_ID)
publ.status.src$CU_ID <- gsub("_","-",publ.status.src$CU_ID)
retro.values$CU_ID <- gsub("_","-", retro.values$CU_ID)
retro.status$CU_ID <- gsub("_","-", retro.status$CU_ID)
metrics.long$CU_ID <- gsub("_","-", metrics.long$CU_ID)




qual.score.tab <- data.frame(
	BinLabel = c("Node3","Node17","Node19","Node20","Node21","Node22","Node23","Node33","Node36","Node37","Node64","Node65"),
	ConfidenceRating5 = c("High","Moderate","High","Moderate","Moderate","High","High","Moderate","High","High","Low","Low"),
	ConfidenceRating3 = c("High","Moderate","High","Moderate","Moderate","High","High","Moderate","High","High","Low","Low"),
	ConfidenceRating2 = c("High","High","High","Moderate","High","High","High","High","High","Moderate","High","Moderate")
	)



# add grouping variable to spn data file
cu.data.group <- cu.data %>%
                    mutate(CU_ID = gsub("_","-",CU_ID)) %>% 
                    dplyr::filter(!is.na(CU_Name)) %>%
                    left_join(cu.info %>% select(CU_Name,Group), by="CU_Name" )

print(names(cu.data.group))

# Reorg WSP integrated statuses

publ.int.status <- publ.status.src %>%  dplyr::filter(Metric == "IntStatus") %>%
                                        select(CU_ID,Year, Status) %>% 
                                        dplyr::rename(IntStatusRaw = Status) %>%
                                        mutate(IntStatus = recode(IntStatusRaw, RedAmber = "Red", AmberGreen = "Amber")) %>%
                                        mutate(IntStatusRaw_Short = recode(IntStatusRaw, Red = "R",RedAmber = "RA", Amber = "A",AmberGreen = "AG",Green = "G"),
                                               IntStatus_Short = recode(IntStatus, Red = "R", Amber = "A",Green = "G"))





# == Apply the Rapid Status Decision Tree ========================================= #

retro.rapid.status <- applyRapidStatusTree(data.df = retro.values %>%
					left_join( as.data.frame(publ.int.status),by=c("CU_ID","Year")),
					id.col = "CU_ID", group.var = group.var) 

# ================================================================================== #


write.csv(retro.rapid.status$data, 
		paste0(out.filepath,"/Retrospective_RapidStatus_",out.label,".csv"),row.names = FALSE)

retro.summary.tbl <- retro.rapid.status$data %>%
                              dplyr::rename(RapidStatus = SynStatus,RapidScore = SynScore) %>%
                              select(-IntStatus,-IntScore,-ErrorScore,-ErrorType) %>%
                              left_join(retro.status %>% select(-NumStdMetrics,-Value) %>% #,-Group
                                                         rename(RelAbdCat = RelAbd, AbsAbdCat = AbsAbd, LongTrendCat = LongTrend,              
                                                                 PercChangeCat = PercChange, ProbDeclBelowLBMCat = ProbDeclBelowLBM,
                                                                 PercentileCat = Percentile, RelLBMCat = RelLBM, RelUBMCat = RelUBM,
                                                                 AbsLBMCat = AbsLBM, AbsUBMCat = AbsUBM   )      , 
                                                          by = c("CU_ID","Species","Stock","DataType","Year")) %>%
                              left_join(metrics.long %>% filter(Metric == "RelLBM") %>% 
                                                         select(CU_ID, Stock, Year, RelAbd_LBM=LBM, RelAbd_UBM=UBM),
                                                         by = c("CU_ID","Stock","Year")) %>%
                              #left_join( as.data.frame(publ.int.status),by=c("CU_ID","Year")) %>%
                              arrange(Species,Stock,Year) %>%
                              left_join(cu.data.group  %>%   
                                                 select(CU_ID,Year,SpnForAbd_Wild, SpnForTrend_Wild, SpnForAbd_Total, SpnForTrend_Total),
                                                 by= c("CU_ID","Year")   ) %>%
                              select(CU_ID,Species,Stock,	DataType,Year,SpnForAbd_Wild, SpnForTrend_Wild, everything()) %>%
                              left_join(qual.score.tab %>% 
                                           select(BinLabel, ConfidenceRating5, ConfidenceRating3, ConfidenceRating2),
                                                      by = "BinLabel")
                                

retro.summary.tbl$IntStatus5 <- retro.summary.tbl$IntStatusRaw
retro.summary.tbl <- retro.summary.tbl %>% 
						mutate(IntStatus3 = recode(IntStatusRaw, RedAmber = "Red", AmberGreen = "Amber")) %>% 
						mutate(IntStatus2 = recode(IntStatus3, "Amber" = "NotRed","Green" = "NotRed"))

write.csv(retro.summary.tbl, 
	paste0(out.filepath,"/Retro_Synoptic_Details_",out.label,".csv"),
	row.names = FALSE)   


out.obj <- list(SummaryTable = retro.summary.tbl,
			Rules = retro.rapid.status$rules,
			Data = retro.rapid.status$data,
			Summary  = retro.rapid.status$summary)

if("confusion" %in% names(retro.rapid.status)){
out.obj <- c(out.obj,list(ConfusionMatrix = retro.rapid.status$confusion,
						  RawConfusionMatrix = retro.rapid.status$confusion.raw,
						  Accuracy = retro.rapid.status$accuracy))
	}


return(out.obj)


}




























#' applyRapidStatusTree
#'
#' this function applies the current version of the rapid status decision tree to a data set of retrospective status metrics calculated with the calcMetrics() function
#' @param data.df data file with metric values, with labels in first column (and optionally an IntStatus column that has results from any integrated status assessments that have been done)
#' @param id.col column label for unique case identifier (e.g. "CU_ID")
#' @param group.var column name used for grouping summaries (e.g. by species). default is NULL
#' @keywords trend
#' @export


applyRapidStatusTree <- function(data.df, id.col = "CU_ID", group.var = NULL){



require(tidyverse)

data.out <- data.df # start a new data object

# These used to be arguments, now that things have stabilized, they're just hardwired
status.labels <- c("Red","RedAmber","Amber","AmberGreen","Green", "None","NotRed")
status.scores <- c(5,4,3,2,1,NA,2)
scores.df <- data.frame(Status = status.labels,Score = status.scores)


# clean up the int status column (if it exists)
if("IntStatus" %in% names(data.df)){

      data.out$IntStatus[!(data.out$IntStatus %in% status.labels)] <- "None"
}


# START DECISION TREE APPLICATION (Learning Tree 3 Version)

rules.df <- data.frame(
    Node = paste0("Node", c(3,17,19,20,21,22,23,33,36,37,64,65)),
    Status = c("Red","Red","Red","Amber","Red","Amber","Red","Red","Green","Amber","Green","Amber"),
    Rule = c("AbsLBM < 1.5",
             "AbsLBM = NA OR AbsLBM >= 1.5, then AbsLBM = NA OR AbsUBM >= 1, then Don't have RelLBM, then LongTrend < 79",  # Bmac changed March  2025
             "AbsLBM = NA OR AbsLBM >= 1.5, then AbsLBM = NA OR AbsUBM >= 1, then Have RelLBM, then RelLBM < 1",
             "AbsLBM = NA OR AbsLBM >= 1.5, then AbsUBM < 1,then Don't have RelLBM, then LongTrend >= 79",
             "AbsLBM = NA OR AbsLBM >= 1.5, then AbsUBM < 1,then Don't have RelLBM, then LongTrend < 79" ,
             "AbsLBM = NA OR AbsLBM >= 1.5, then AbsUBM < 1,then Have RelLBM, then RelLBM >= 1",
             "AbsLBM = NA OR AbsLBM >= 1.5, then AbsUBM < 1,then Have RelLBM, then RelLBM < 1",
             "AbsLBM = NA OR AbsLBM >= 1.5, then AbsLBM = NA OR AbsUBM >= 1, then Don't have RelLBM, then LongTrend >= 79, then PercChange < -70",
             "AbsLBM = NA OR AbsLBM >= 1.5, then AbsLBM = NA OR AbsUBM >= 1, then Have RelLBM, then RelLBM >= 1, then RelUBM >= 1.1",
             "AbsLBM = NA OR AbsLBM >= 1.5, then AbsLBM = NA OR AbsUBM >= 1, then Have RelLBM, then RelLBM >= 1, then RelUBM < 1.1",
             "AbsLBM = NA OR AbsLBM >= 1.5, then AbsLBM = NA OR AbsUBM >= 1, then Don't have RelLBM, then LongTrend >= 79, then PercChange >= -70, then LongTrend >= 233",
             "AbsLBM = NA OR AbsLBM >= 1.5, then AbsLBM = NA OR AbsUBM >= 1, then Don't have RelLBM, then LongTrend >= 79, then PercChange >= -70, then LongTrend < 233"
    ))


# NODE 1 ---------------------------------------

     node1.df <- data.df


# NODES 2 & 3 ---------------------------------------------------------

#write.csv(node1.df,"test_node1.csv")


node2.df <- node1.df %>% dplyr::filter( is.na(AbsLBM) | AbsLBM >= 1.5 ) #| DataType == "Rel_Idx" ) #Bmac change March 5 2025 - remove data type filtering as unnecessary
# not a leaf, so no status yet
if(dim(node2.df)[1] == 0){
  node2.df$SynStatus <- character()
  node2.df$BinLabel <- character()
  node2.df$BinPath <- character()
}
#write.csv(node2.df,"test_node2.csv")

# node2.na <- node1.df %>% dplyr::filter(is.na(DataType)) # Bmac change March 5 2025 - unnecessary node
# if(dim(node2.na)[1] > 0){
#   node2.na$SynStatus <- "None"
#   node2.na$BinLabel <- "Node2.NA"
#   node2.na$BinPath <- "No AbsLBM or no DataType, so no SynStatus"
#   #write.csv(node2.na,"test_node2na.csv")
# }


#node3.df <- node1.df %>% dplyr::filter( DataType == "Abs_Abd" & AbsLBM < 1.5 )
node3.df <- node1.df %>% dplyr::filter( !is.na(AbsLBM) & AbsLBM < 1.5 ) #Bmac change March 5 2025 - remove data type filtering, use metric availability
# not a leaf, so no status yet
if(dim(node3.df)[1] > 0){
  node3.df$SynStatus <- "Red"
  node3.df$BinLabel <- "Node3"
  node3.df$BinPath <- "Have AbsLBM, then AbsLBM < 1.5"
  #write.csv(node3.df,"test_node3.csv")
}



# Nodes 4 & 5 (ABS UBM < 1) -----------------------------------------------------


node4.df <- node2.df %>% dplyr::filter( is.na(AbsUBM) | AbsUBM >= 1) #| DataType == "Rel_Idx" ) #Bmac fix March 5 2025
# not a leaf, so no status yet
if(dim(node4.df)[1] == 0){
  node4.df$SynStatus <- character()
  node4.df$BinLabel <- character()
  node4.df$BinPath <- character()
}
#write.csv(node4.df,"test_node4.csv")

# node4.na NOT NEEDED


#node5.df <- node2.df %>% dplyr::filter( DataType == "Abs_Abd" & AbsUBM < 1)
node5.df <- node2.df %>% dplyr::filter( !is.na(AbsUBM) & AbsUBM < 1) # Bmac fix March 5 2025
# not a leaf, so no status yet
if(dim(node5.df)[1] == 0){
  node5.df$SynStatus <- character()
  node5.df$BinLabel <- character()
  node5.df$BinPath <- character()
}
#write.csv(node5.df,"test_node5.csv")





# NODES SUB-5 - HAVE REL BM ----------------------------------------------------


node11.df <- node5.df  %>% dplyr::filter(!is.na(RelLBM))
# not a leaf, so no status yet
if(dim(node11.df)[1] == 0){
  node11.df$SynStatus <- character()
  node11.df$BinLabel <- character()
  node11.df$BinPath <- character()
}
#write.csv(node11.df,"test_node11.csv")

# node11.NA not needed b/c checking for RelLBM = NA already



node22.df <- node11.df %>% dplyr::filter(RelLBM >=1)
if(dim(node22.df)[1] > 0){
  node22.df$SynStatus <- "Amber"
  node22.df$BinLabel <- "Node22"
  node22.df$BinPath <- "Have AbsLBM, then AbsLBM >= 1.5, then have AbsUBM, then AbsUBM < 1,then Have RelLBM, then RelLBM >= 1"
  #write.csv(node22.df,"test_node22.csv")
}


node23.df <- node11.df %>% dplyr::filter(RelLBM < 1)
if(dim(node23.df)[1] > 0){
  node23.df$SynStatus <- "Red"
  node23.df$BinLabel <- "Node23"
  node23.df$BinPath <- "Have AbsLBM, then AbsLBM >= 1.5, then have AbsUBM, then AbsUBM < 1,then Have RelLBM, then RelLBM < 1"
  #write.csv(node23.df,"test_node23.csv")
}



# NODES SUB-5 - DO NOT HAVE REL BM -----------------------------------


node10.df <- node5.df  %>% dplyr::filter(is.na(RelLBM))
# not a leaf, so no status yet
if(dim(node10.df)[1] == 0){
  node10.df$SynStatus <- character()
  node10.df$BinLabel <- character()
  node10.df$BinPath <- character()
}
#write.csv(node10.df,"test_node10.csv")

# Node 10 NA not needed b/c checking for NA already

node20.df <- node10.df %>% dplyr::filter(LongTrend >= 79)
if(dim(node20.df)[1] > 0){
  node20.df$SynStatus <- "Amber"
  node20.df$BinLabel <- "Node20"
  node20.df$BinPath <- "Have AbsLBM, then AbsLBM >= 1.5, then have AbsUBM, then AbsUBM < 1, then Don't have RelLBM, then LongTrend >= 79"
  #write.csv(node20.df,"test_node20.csv")
}


node21.df <- node10.df %>% dplyr::filter(LongTrend < 79)
if(dim(node21.df)[1] > 0){
  node21.df$SynStatus <- "Red"
  node21.df$BinLabel <- "Node21"
  node21.df$BinPath <- "Have AbsLBM, then AbsLBM >= 1.5, then have AbsUBM, then AbsUBM < 1, then Don't have RelLBM, then LongTrend < 79"
  #write.csv(node21.df,"test_node21.csv")
}


# NODES SUB-4 - HAVE REL BM


node9.df <- node4.df  %>% dplyr::filter(!is.na(RelLBM))
# not a leaf, so no status yet
if(dim(node9.df)[1] == 0){
  node9.df$SynStatus <- character()
  node9.df$BinLabel <- character()
  node9.df$BinPath <- character()
}
#write.csv(node9.df,"test_node9.csv")


# Node9NA not needed b/c checking for NA already

node19.df <- node9.df %>% dplyr::filter(RelLBM < 1)
if(dim(node19.df)[1] > 0){
  node19.df$SynStatus <- "Red"
  node19.df$BinLabel <- "Node19"
  node19.df$BinPath <- "AbsLBM = NA OR AbsLBM >= 1.5, then AbsUBM = NA OR AbsUBM >= 1, then Have RelLBM, then RelLBM < 1"
  #write.csv(node19.df,"test_node19.csv")
}


node18.df <- node9.df %>% dplyr::filter(RelLBM >=1)
# not a leaf, so no status yet
if(dim(node18.df)[1] == 0){
  node18.df$SynStatus <- character()
  node18.df$BinLabel <- character()
  node18.df$BinPath <- character()
}
#write.csv(node18.df,"test_node18.csv")

#Node 18NA not needed b/c checking for NA in node 9 already

node36.df <- node18.df %>% dplyr::filter(RelUBM >= 1.1)
if(dim(node36.df)[1] > 0){
  node36.df$SynStatus <- "Green"
  node36.df$BinLabel <- "Node36"
  node36.df$BinPath <- "AbsLBM = NA OR AbsLBM >= 1.5, then AbsUBM = NA OR AbsUBM >= 1, then Have RelLBM, then RelLBM >= 1, then RelUBM >= 1.1"
  #write.csv(node36.df,"test_node36.csv")
}

node37.df <- node18.df %>% dplyr::filter(RelUBM < 1.1)
if(dim(node37.df)[1] > 0){
  node37.df$SynStatus <- "Amber"
  node37.df$BinLabel <- "Node37"
  node37.df$BinPath <- "AbsLBM = NA OR AbsLBM >= 1.5, then AbsUBM = NA OR AbsUBM >= 1, then Have RelLBM, then RelLBM >= 1, then RelUBM < 1.1"
  #write.csv(node37.df,"test_node37.csv")
}



# NODES SUB-4 - DO NOT HAVE REL BM

node8.df <- node4.df  %>% dplyr::filter(is.na(RelLBM))
# not a leaf, so no status yet
if(dim(node8.df)[1] == 0){
  node8.df$SynStatus <- character()
  node8.df$BinLabel <- character()
  node8.df$BinPath <- character()
}
#write.csv(node8.df,"test_node8.csv")



# Node8 NA not needed, already checking for RelLBM  = NA

node17.df <- node8.df %>% dplyr::filter(LongTrend < 79)
if(dim(node17.df)[1] > 0){
  node17.df$SynStatus <- "Red"
  node17.df$BinLabel <- "Node17"
  node17.df$BinPath <- "AbsLBM = NA OR AbsLBM >= 1.5, then AbsUBM = NA OR AbsUBM >= 1, then Don't have RelLBM, then LongTrend < 79"
  #write.csv(node17.df,"test_node17.csv")
}


# just in case there is a info here, but no LongTrend for the next step
node16.na <- node8.df %>% dplyr::filter(is.na(LongTrend))
if(dim(node16.na)[1] > 0){
  #print("node 16 na")
  node16.na$SynStatus <- "None"
  node16.na$BinLabel <- "Node16.NA"
  node16.na$BinPath <- "No LongTrend, so no SynStatus"
  #write.csv(node16.na,"test_node16NA.csv")
}

node16.df <- node8.df %>% dplyr::filter(LongTrend >= 79)
# not a leaf, so no status yet
if(dim(node16.df)[1] == 0){
  node16.df$SynStatus <- character()
  node16.df$BinLabel <- character()
  node16.df$BinPath <- character()
}
#write.csv(node16.df,"test_node16.csv")

# COMMENTED OUT MARCH 2024
#NEW May 2023 For cases where have long trend but no short trend
#node32.na <- node16.df %>% dplyr::filter(is.na(PercChange))
# ALSO REMOVED node32.na FROM THE OUTPUT OBJECT AT THE END
#if(dim(node32.na)[1] > 0){
  #print("node 32 na")
#  node32.na$SynStatus <- "None"
#  node32.na$BinLabel <- "Node32.NA"
#  node32.na$BinPath <- "LongTrend > 79, but no ShortTrend, so no SynStatus"
  #write.csv(node16.na,"test_node16NA.csv")
#}

# REVISED MAR 2014 TO DEAL WITH FRASER CK CUs
# CASES WITH NO SHORT TERM TREND
# OLD: node32.df <- node16.df %>% dplyr::filter(PercChange >= -70)
node32.df <- node16.df %>% dplyr::filter(PercChange >= -70 | is.na(PercChange))
# not a leaf, so no status yet
if(dim(node32.df)[1] == 0){
  node32.df$SynStatus <- character()
  node32.df$BinLabel <- character()
  node32.df$BinPath <- character()
}


node33.df <- node16.df %>% dplyr::filter(PercChange < -70)
if(dim(node33.df)[1] > 0){
  node33.df$SynStatus <- "Red"
  node33.df$BinLabel <- "Node33"
  node33.df$BinPath <- "AbsLBM = NA OR AbsLBM >= 1.5, then AbsUBM = NA OR AbsUBM >= 1, then Don't have RelLBM, then LongTrend >= 79, then PercChange < -70"
  #write.csv(node33.df,"test_node33.csv")
}


node64.df <- node32.df %>% dplyr::filter(LongTrend >= 233)
if(dim(node64.df)[1] > 0){
  node64.df$SynStatus <- "Green"
  node64.df$BinLabel <- "Node64"
  node64.df$BinPath <- " AbsLBM = NA OR AbsLBM >= 1.5, then AbsUBM = NA OR AbsUBM >= 1, then Don't have RelLBM, then LongTrend >= 79, then PercChange >= -70, then LongTrend >= 233"

}


node65.df <- node32.df %>% dplyr::filter(LongTrend < 233)
if(dim(node65.df)[1] > 0){
  node65.df$SynStatus <- "Amber"
  node65.df$BinLabel <- "Node65"
  node65.df$BinPath <- "AbsLBM = NA OR AbsLBM >= 1.5, then AbsUBM = NA OR AbsUBM >= 1, then Don't have RelLBM, then LongTrend >= 79, then PercChange >= -70, then LongTrend < 233"

}


data.out <- rbind(node16.na,#node10.na, #node2.na, #node10na added Feb 2025  #node32.na,# REMOVED MAR 2024
                  node3.df,node17.df,node19.df,
                  node20.df,node21.df,node22.df,node23.df,
                  node33.df,
                  node36.df,node37.df,
                  node64.df,node65.df)





#------------------------------------------------
# CREATE SUMMARIES

summary.tab <- as.data.frame(matrix(NA,ncol=length(status.labels),dimnames=list("Tmp", status.labels)))
summary.tab <- bind_rows(summary.tab, as.data.frame(t(as.matrix(table(data.out$SynStatus)))))
summary.tab <- summary.tab[-1,,drop = FALSE]
dimnames(summary.tab)[[1]][1] <- "SynStatus"

#print("flag:summary.tab done")


if("IntStatus" %in% names(data.df)){

  intstatus.summary  <- as.data.frame(t(as.matrix(table(data.out$IntStatus)))   )

  
  summary.tab <- bind_rows(summary.tab, intstatus.summary)
  dimnames(summary.tab)[[1]][2] <- "IntStatus"

  confusion.tab <- as.data.frame.matrix(table(data.out$SynStatus,data.out$IntStatus)) %>% rownames_to_column(var="Predicted")
  dimnames(confusion.tab)[[2]][-1] <-  paste0("Int",dimnames(confusion.tab)[[2]][-1])
  
  confusion.raw.tab <- as.data.frame.matrix(table(data.out$SynStatus,data.out$IntStatusRaw)) %>% rownames_to_column(var="Predicted")
  dimnames(confusion.raw.tab)[[2]][-1] <-  paste0("Int",dimnames(confusion.raw.tab)[[2]][-1])
  
  
}



# include status scores and error scores
#print("flag:error scores start")
if("IntStatus" %in% names(data.df)){ data.out <- left_join(data.out,scores.df %>% rename(IntStatus = Status,IntScore = Score))}
if(!("IntStatus" %in% names(data.df))){ data.out$IntStatus <- NA; data.out$IntScore <- NA}
#print("flag:error scores done")

data.out <- left_join(data.out,scores.df %>% rename(SynStatus = Status,SynScore = Score))

data.out$ErrorScore <- data.out$IntScore - data.out$SynScore




data.out$ErrorType <- "NA"
data.out[data.out$ErrorScore == 0 &!is.na(data.out$ErrorScore) , "ErrorType"] <- "None"
data.out[data.out$ErrorScore < 0 &!is.na(data.out$ErrorScore) , "ErrorType"] <- "PredWorse"
data.out[data.out$ErrorScore > 0 &!is.na(data.out$ErrorScore) , "ErrorType"] <- "PredBetter"


# ---------------------------------------------------------------------------
# summarize accuracy, error types etc


# Freq of error scores

error.score.freq <- as.data.frame(as.matrix(table(data.out$ErrorScore))) %>% rename(All=V1) %>% rownames_to_column(var="ErrorScore")

if(!is.null(group.var)){

  error.score.freq.grp <- data.out %>% group_by_at(c(group.var,"ErrorScore")) %>%
    summarise(Freq = n()) %>%
    pivot_wider(id_cols = ErrorScore,names_from = all_of(group.var), values_from = Freq) %>%
    mutate(ErrorScore = as.character(ErrorScore))
  error.score.freq <- left_join(error.score.freq,error.score.freq.grp,by="ErrorScore")

}

error.score.freq <- bind_rows(error.score.freq,
                             cbind(ErrorScore = "Total",
                                   as.data.frame(t(colSums(error.score.freq %>%
                                                            select(-ErrorScore),na.rm=TRUE ))))
)



# Frequ of error types
error.type.freq <- as.data.frame(as.matrix(table(data.out$ErrorType))) %>% rename(All=V1) %>% rownames_to_column(var="ErrorType")

if(!is.null(group.var)){

error.type.freq.grp <- data.out %>% group_by_at(c(group.var,"ErrorType")) %>%
                                               summarise(Freq = n()) %>%
                                               pivot_wider(id_cols = ErrorType,names_from = all_of(group.var), values_from = Freq)
error.type.freq <- left_join(error.type.freq,error.type.freq.grp,by="ErrorType")

}

# fill in any missing categories
cat.list <- c("NA","None","PredWorse","PredBetter")
missing.cat.idx <-  !(cat.list  %in% error.type.freq$ErrorType)

# clean up this code!!!!!!!!!
if(sum(missing.cat.idx)>0){
  n.rows <- sum(missing.cat.idx)
  #n.cols <- length(names(error.type.freq))-1
  add.rows.df <-  error.type.freq[1:n.rows,]
  add.rows.df[,] <- 0
  add.rows.df[,1] <- cat.list[missing.cat.idx]

  error.type.freq <- bind_rows(error.type.freq, add.rows.df)


}





error.type.freq <- bind_rows(error.type.freq,
                             cbind(ErrorType = "Total(excl.NA)",
                                   as.data.frame(t(colSums(error.type.freq %>%
                                                             dplyr::filter(ErrorType != "NA") %>%
                                                              select(-ErrorType),na.rm=TRUE ))))
                                                          )


error.type.freq <- bind_rows(error.type.freq,
                             cbind(ErrorType = "PercCorrect",
                                  round( (error.type.freq %>% dplyr::filter(ErrorType == "None") %>% select(-ErrorType)) /
                                     (error.type.freq %>% dplyr::filter(ErrorType == "Total(excl.NA)") %>% select(-ErrorType))*100)
                             ),
                             cbind(ErrorType = "PercWorse",
                                   round( (error.type.freq %>% dplyr::filter(ErrorType == "PredWorse") %>% select(-ErrorType)) /
                                            (error.type.freq %>% dplyr::filter(ErrorType == "Total(excl.NA)") %>% select(-ErrorType))*100)
                             ),
                             cbind(ErrorType = "PercBetter",
                                   round( (error.type.freq %>% dplyr::filter(ErrorType == "PredBetter") %>% select(-ErrorType)) /
                                            (error.type.freq %>% dplyr::filter(ErrorType == "Total(excl.NA)") %>% select(-ErrorType))*100)
                             )
                             )






if("IntStatus" %in% names(data.df)){  # do Chi Sq tests

# NOTE: getting warning due to small sample size (for some bins),
# so using "simulate.p.value = TRUE"
# as per https://stats.stackexchange.com/questions/81483/warning-in-r-chi-squared-approximation-may-be-incorrect

  data.use <- data.out

  data.use$IntStatus <- dplyr::recode(data.out$IntStatus, DD = "None", UD = "None")

if(!("RedAmber" %in% rules.df$Status | "AmberGreen" %in% rules.df$Status)){
          data.use$IntStatus <- dplyr::recode(data.use$IntStatus, RedAmber = "Red", AmberGreen = "Amber")}

chisq.table <-   table(data.use$SynStatus,data.use$IntStatus)
# chi sq test call only works if all CUs have at least 1 Int status (Or something like that) - Skipping for now
#chisq.test.out <- chisq.test(chisq.table,simulate.p.value = TRUE,B=2000)

chisq.list <- list(input = chisq.table,
                   statistic = NA, #chisq.test.out$statistic,
                   p.value = NA, #chisq.test.out$p.value,
				   p.label = NA #p.label(chisq.test.out$p.value)
                   #observed = chisq.test.out$observed#,
                   #expected = chisq.test.out$expected
                   )


}

if(!("IntStatus" %in% names(data.df))){
accuracy.list <- list(error.scores.freq = error.score.freq,
                      error.type.freq = error.type.freq)
					  }
if("IntStatus" %in% names(data.df)){

accuracy.list <- list(error.scores.freq = error.score.freq,
                      error.type.freq = error.type.freq,
					  chisq = chisq.list)
}

out.obj <- list(rules = rules.df,
                data = data.out,
                summary = summary.tab %>% rownames_to_column(var="Type")  )

if("IntStatus" %in% names(data.df)){  # do confusion matrix
   out.obj <- c(out.obj, list(confusion = confusion.tab,confusion.raw = confusion.raw.tab, accuracy = accuracy.list))
   }


return(out.obj)

}







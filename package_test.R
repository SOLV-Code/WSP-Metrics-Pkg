library(tidyverse)
library(WSPMetrics)
library(R2jags)


#ls(getNamespace("MetricsTest"), all.names=TRUE)


# TEST LONG-TERM TREND  AND PERC CHANGE - Simple versions for single vector from example Data. Perc change calculated over entire time period.

lt.trend.single <- calcLongTermTrendSimple(vec.in = exampleData$Stock1 ,
            gen.in = 4,min.lt.yrs = 20, avg.type = "geomean",
            tracing=FALSE,
            recent.excl = FALSE)

lt.trend.single

calcPercChangeSimple(vec.in = exampleData$Stock1)
calcPercChangeSimple(vec.in = exampleData$Stock2)
calcPercChangeSimple(vec.in = exampleData$Stock3)



# TEST LONG-TERM TREND AND PERC CHANGE - data frame version. Perc change calculated over 3 generations


lt.trend <- calcLongTermTrend(X = exampleData,gen.in = 4, recent.num.gen = 1, extra.yrs = 0,
                               min.yrs.used = 20, avg.type = "geomean", tracing=FALSE,
                               recent.excl = FALSE)

lt.trend

perc.change <- calcPercChange(X = exampleData,gen.in = 4,
                  slope.num.gen = 3, extra.yrs = 0,
                  genmean.smoothing = TRUE,
                  log.transform = TRUE,
                  out.exp = TRUE, tracing=FALSE)

perc.change



# TEST BAYESIAN PERC CHANGE

pdf("ProbDecl_Fits.pdf",onefile=TRUE,height=8.5, width=11)

# Smooth the first 15 years of the Example Data- Stock1
vec.use <- smoothSeries(exampleData[1:15,1], gen=4, filter.sides=1, log.transform = TRUE, out.exp = TRUE,na.rm=FALSE)
vec.use <- vec.use[4:15] # Removing intial NAs

slope.mcmc.fit <- calcPercChangeMCMC(vec.in= vec.use,model.in = NULL ,
                                     perc.change.bm = -25 , na.skip=FALSE,
                                     out.type = "long", mcmc.plots = TRUE)
dev.off()

names(slope.mcmc.fit)
slope.mcmc.fit$pchange
slope.mcmc.fit$probdecl
slope.mcmc.fit$summary







###########
?calcLongTermTrendSimple


#################
# Testing Manual Generation


library(devtools)

setwd("../") # have to be outside the package folder

build_manual(pkg = "WSP-Metrics-Pkg") # takes a while (first time only? installs some stuff)

setwd("WSP-Metrics-Pkg") # move back into pkg folder




########################
# testing NA fix

test.vec <- c(1,2,3,4,5,NA,NA,-Inf,0,8,9)
test.vec


inf.idx <- !is.finite(test.vec)
inf.idx
inf.idx[is.na(inf.idx)] <- FALSE








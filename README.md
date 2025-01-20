# Calculate and use metrics for status assessments under *Canada's Wild Salmon Policy* (WSP)


* [Introduction](#Introduction)
* [Background](#Background)
* [Install](#Install)
* [Illustrations](#Illustrations)
* [References](#References)

## Introduction

This package includes functions to calculate the standard status metrics originally defined by Holt et al. 2009, and further refined in subsequent test cases and additional explorations ([References](#References)). The package also includes functions for using the resulting metrics in a status assessment (apply rapid status decision tree, generate dashboards).


For questions, comments, or suggestions, please contact 

* Dr. Carrie Holt (Project Lead, Carrie.Holt"AT"dfo-mpo.gc.ca)
* Gottfried Pestal (Developer, gpestal"AT"solv.ca)

or start a discussion thread [here](https://github.com/Pacific-salmon-assess/WSP-Metrics-Pkg/issues).

If you use this package in your work, please cite it as follows:

*Holt C, Pestal G, MacDonald B, and Grant S (2025) WSPMetrics: Calculate and use metrics for status assessments under Canada's Wild Salmon Policy (WSP). R package version 0.9. https://github.com/Pacific-salmon-assess/WSP-Metrics-Pkg.*


## Background

### WSP Status Metrics

The WSP metrics are designed to capture different considerations related to *status*, and have been used *in combination* to develop *integrated status designations*. Below is a brief overview with general definitions. For more detail about these metrics, alternative calculation approaches, and their use in formal status asessments, see the [References](#References). 


**Metric** | **General Definition** | **Variations** | **Function** 
-- | -- | -- | --
Long-Term Trend | Recent Avg / Long-term Avg |  geometric mean vs. arithmetic mean |  *calcLongTermTrendSimple()* for a single vector. *calcLongTermTrend()* for a data frame with Years x Stocks (includes retrospective calculations).
Percent Change  | change over n generations | alternative smoothing (log, gen avg), time window (n gen, n gen +1) | *calcPercChangeSimple()* for a single vector of raw values. *calcPercChange()* for a data frame with Years x Stocks (includes retrospective calculations).
Probability of Decline | Prob(PercChange> X%) | alternative smoothing (log, gen avg), time window (n gen, n gen +1) |   *calcPercChangeSimple()* for regression based calculation in a single vector. *calcPercChange()* for a data frame with Years x Stocks (includes retrospective calculations). *calcPercChangeMCMC()* for a Bayesian probability (based on posterior distribution of regression slopes) calculated on a single vector of raw values.
Relative Abundance - SR-Based BM| Recent Avg compared to biological benchmarks (Sgen, 80%Smsy) | geometric mean vs. arithmetic mean, alternative benchmark estimate approaches | This package is focused on calculating status metrics for WSP implementation, but the relative abundance metric relies on CU-specific estimates of biological benchmarks. Due to the diversity of salmon life history types, available data, spawner-recruit model forms, and estimation approaches, it is not feasible to provide a single generic estimation function that can reliably generate meaningful estimates of Sgen and 80%Smsy with default settings. In order to calculate the relative abundance metric for a CU, users of the *WSPMetrics* package need to provide benchmark estimates as part of the input specifications. A separate R package focused on estimating SR parameters and associated benchmarks is available: [samEst Package](https://github.com/Pacific-salmon-assess/samEst), [samEst functions for Sgen, Smsy](https://github.com/Pacific-salmon-assess/samEst/blob/main/R/RP_functions.R), [Course Material with Examples](https://github.com/TESA-workshops/run-samEst)
Relative Abd - Percentile BM| Recent Avg compared to user-specified %iles | geometric mean vs. arithmetic mean | *calcPercentileSimple()* for a single vector. NOTE: This function only calculates the percentile value. It is the *calcMetrics()* wrapper function that compares this value to user-specified benchmarks and assigns a status category. [Holt et al. (2018)](https://www.dfo-mpo.gc.ca/csas-sccs/Publications/ResDocs-DocRech/2018/2018_011-eng.html) recommend lower and upper percentile-based status benchmarks at the 25th and 50th percentiles of observed spawner abundances for data-limited conservation units of Chum Salmon when productivity is moderate to high (>2.5 recruits/spawner) and harvest rates are low to moderate (≤40%).  [Holt et al. (2018)](https://www.dfo-mpo.gc.ca/csas-sccs/Publications/ResDocs-DocRech/2018/2018_011-eng.html) suggest further evaluation of percentile benchmarks and the consideration of alternatives when productivity is expected to be low and/or harvest rates high. The function *calcMetrics()* uses 0.25 and 0.50 as the default benchmarks for the percentiles.

### Rapid Status Decision Tree

Pestal et al. (2024) developed a decision tree that captures how experts in large-scale workshops interpreted the individual status metrics to determine an overall status for each CU. The tree works through a series of criteria based on the standard status metrics and produces a Red/Amber/Green designation. For details, see the [References](#References).

## Install

To install this package directly from github, use

```
install.packages("devtools") # Install the devtools package
library(devtools) # Load the devtools package.
install_github("Pacific-salmon-assess/WSP-Metrics-Pkg", dependencies = TRUE, build_vignettes = FALSE)
library(WSPMetrics)				
```

**Note: If you want to use the MCMC-based functions, you also need to install [JAGS](http://mcmc-jags.sourceforge.net/).**



## Illustrations

### Illustration of Individual Metric Functions

First install the package as per above.

```

# TEST LONG-TERM TREND  AND PERC CHANGE - Simple versions for single vector using exampleData

lt.trend.single <- calcLongTermTrendSimple(vec.in = exampleData$Stock1 ,
            gen.in = 4,min.lt.yrs = 20, avg.type = "geomean",
            tracing=FALSE,
            recent.excl = FALSE)

lt.trend.single

calcPercChangeSimple(vec.in = exampleData$Stock1) #Calculated over the entire time-series
calcPercChangeSimple(vec.in = exampleData$Stock2)
calcPercChangeSimple(vec.in = exampleData$Stock3)


# TEST LONG-TERM TREND AND PERC CHANGE - data frame version. Perc change calculated over "slope.num.gen" number of generations

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
```



### Illustration of Overall Wrapper Function for Metrics

The *calcMetrics()* function takes a time series and applies the individual metric calculations, with user-specified settings, to calculate a full suite of outputs (including retrospective calculation of metrics)


**IMPORTANT NOTE**: Relative and absolute abundance values calculated in *calcMetrics()* use the user-specified settings to choose the method of calculating, and the settings for *avg.specs* and *slope.specs* interact. If the *rel.avg.type* is “regular” instead of “smoothed” it just uses the smoothed series. The smoothing function depends on the whether or not log transforming is used for the slope (```slope.specs$log.transform=TRUE```), otherwise it just takes the average of the generation instead of the geometric average. The typical use case for our applications so far has always had ```slope.specs$log.transform=TRUE```, so there hasn’t been an issue where the wrong type of average is being calculated, but the function was set up for maximum flexibility, so end-users need to keep this interaction in mind.

The function help file has additional details for the arguments. To access the function help file, use

```
?calcMetrics
```

Here is an example of running the calcMetrics() function:


```


head(exampleData)

yrs.vec <- 1:length(exampleData$Stock1) + 1979 
# if your data doesn't have yrs, need to generate a yr vector

plot(yrs.vec, exampleData$Stock1,type="o")



test.out <- calcMetrics (  series.in = exampleData$Stock1 , # vector of values
                          yrs.in  = yrs.vec,
                          gen.in = 4, # avg generation time
                          stk.label = "Stock 1", # used for labeling output
                          species.label = "Species", # used for labeling output
                          series.label = "DataVersion", # used for labeling output
                          slope.specs = list(num.gen = 3, 
											 extra.yrs = 0,filter.sides=1, 
											 slope.smooth=TRUE,
                                             log.transform = TRUE, 
											 out.exp = TRUE,
											 na.rm=FALSE),
                          avg.specs = list(avg.type = "geomean",
										   recent.excl=FALSE,
                                           lt.smooth=TRUE, 
										   rel.avg.type="regular",
                                           min.lt.yrs =20,min.perc.yrs =20),
                          metric.bm =  list(RelAbd = c(30,50),   
						  # set the Rel Abd BM at arbitrary numbers for the illustration
                                            AbsAbd = c(1000,10000),
                                            LongTrend = c(0.5,0.75),
                                            PercChange = c(-25,-15),
                                            ProbDeclBelowLBM = c(NA,NA),
                                            Percentile = c(0.25,0.5)),
                          retro.start = NULL,
                          tracing = TRUE)



head(test.out)


```


### Illustration of Rapid Status Decision Tree Functions

The core function is:

* *applyRapidStatusTree()*: function that applies the rapid status decision tree to a data set of WSP metrics generated with *calcMetrics()*

Additional functions that support the overall workflow are:


* *calculateMetricsByCU()*: loops through a data set of CU spawner time series and applies the calcMetrics() function

* *prepDataForRapidStatus.()*: reorganizes the output from calculateMetricsByCU() into the format requires by generateRapidStatus()

* *generateRapidStatus()*: reorganizes the outputs from prepDataForRapidStatus(), applies the function applyRapidStatusTree() to each CU, and then does some post-processing

* *plotStatusDashboards()*: takes outputs from generateRapidStatus() and creates 1 png file with standard diagnostics for each CU

* *generateReportTemplate()*: creates a basic markdown template (Git repo Readme or RStudio Quarto template) after the other functions have been used to calculate metrics, apply the rapid status decision tree, and plot dashboards.


[This repository](https://github.com/SOLV-Code/WSP-Rapid-Status-WorkedExamples) has
worked examples of the full workflow from metric calculations to status dashboards, using all the functions from this package.

## References 

### Standard WSP Metrics

Holt, C., Cass, A., Holtby, B., and Riddell, B. 2009. [Indicators of status and benchmarks for conservation units in Canada’s Wild Salmon Policy](https://waves-vagues.dfo-mpo.gc.ca/library-bibliotheque/339096.pdf). DFO Can. Sci. Advis. Sec. Res. Doc. 2009/058. viii + 74 p.

Holt, C.A. 2012. [Identifying benchmarks and assessing status of CUs under the Wild Salmon Policy: converging on consistent methods. Summary of progress meeting](https://waves-vagues.dfo-mpo.gc.ca/library-bibliotheque/348218.pdf). Can. Tech. Rep. Fish. Aquat. Sci. 3019: v + 23.

Holt, C.A., and Ogden, A. 2013. [Software for assessing status of Conservation Unit under Canada’s Wild Salmon Policy: instructional manual](https://publications.gc.ca/collections/collection_2014/mpo-dfo/Fs97-6-3058-eng.pdf). Can. Tech. Rep. Fish. Aquat. Sci. 3058.

Holt, C.A., Davis, B., Dobson, D., Godbout, L., Luedke, W., Tadey, J., and Van Will, P. 2018. [Evaluating Benchmarks of Biological Status for Data-limited Conservation Units of Pacific Salmon, Focusing on Chum Salmon in Southern BC](https://waves-vagues.dfo-mpo.gc.ca/library-bibliotheque/40759386.pdf). DFO Can. Sci. Advis. Sec. Res. Doc. 2018/011. ix + 77 p.


### Completed WSP Status assessments

DFO. 2012. [Integrated biological status of Fraser River sockeye salmon (*Oncorhynchus nerka*) under the Wild Salmon Policy](http://www.dfo-mpo.gc.ca/csas-sccs/Publications/SAR-AS/2012/2012_056-eng.html). Can. Sci. Advis. Sec. Sci. Advis. Rep. 2012/056: 13 pp. 

Grant, S.C.H., and Pestal, G. 2013. [Integrated biological status assessments under the Wild Salmon Policy using standardized metrics and expert judgement: Fraser River sockeye salmon (*Oncorhynchus nerka*) case studies](https://waves-vagues.dfo-mpo.gc.ca/Library/349637.pdf). Can. Sci. Advis. Sec. Res. Doc. 2012/106: v + 132 pp. 

DFO. 2015. [Wild Salmon Policy status assessment for conservation units of Interior Fraser River coho (*Oncorhynchus kisutch*)](https://waves-vagues.dfo-mpo.gc.ca/Library/364851.pdf). Can. Sci. Advis. Sec. Sci. Advis. Rep. 2015/022: 12 pp. 

DFO. 2016. [Integrated biological status of southern British Columbia Chinook salmon (*Oncorhynchus tshawytscha*) under the Wild Salmon Policy](https://waves-vagues.dfo-mpo.gc.ca/Library/40595419.pdf). Can. Sci. Advis. Sec. Sci. Advis. Rep. 2016/042: 15 pp. 

DFO. 2018. [The 2017 Fraser Sockeye salmon (*Oncorhynchus nerka*) integrated biological status re-assessments under the Wild Salmon Policy](http://waves-vagues.dfo-mpo.gc.ca/Library/40712163.pdf). Can. Sci. Advis. Sec. Sci. Advis. Rep. 2018/017: 17 pp. 

Grant, S.C.H., Holt, C.A., Pestal, G., Davis, B.M., and MacDonald, B.L. 2020. [The 2017 Fraser Sockeye salmon (*Oncorhynchus nerka*) integrated biological status re-assessments under the Wild Salmon Policy using standardized metrics and expert judgement](http://www.dfo-mpo.gc.ca/csas-sccs/Publications/ResDocs-DocRech/2020/2020_038-eng.pdf). Can. Sci. Advis. Sec. Res. Doc. 2020/038: vii+ 211. 


### Rapid Status Decision Tree

DFO. 2024. [Rapid status approximations for Pacific salmon derived from integrated status assessments under DFO’s Wild Salmon Policy](https://waves-vagues.dfo-mpo.gc.ca/library-bibliotheque/41207890.pdf). CSAS Sci. Resp. 2024/004: 42 p. 

Pestal, G., MacDonald, B.L., Grant, S.C.H., and Holt, C.A. 2023. [State of The Salmon: rapid status assessment approach for Pacific salmon under Canada’s Wild Salmon Policy](https://waves-vagues.dfo-mpo.gc.ca/library-bibliotheque/41207890.pdf). Can. Tech. Rep. Fish. Aquat. Sci. 3570: xiv + 200 p. 


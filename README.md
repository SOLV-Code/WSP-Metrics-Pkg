## Code to Calculate Status Metrics under *Canada's Wild Salmon Policy* (WSP)


### Disclaimer

*This package is under development. Functions will change rapidly and substantially.
Do not use these if you are not part of the development team!*

* Dr. Carrie Holt (Project Lead, Carrie.Holt"AT"dfo-mpo.gc.ca)
* Gottfried Pestal (Developer, gpestal"AT"solv.ca)


### Install

To install this package directly from github, use

```
install.packages("devtools") # Install the devtools package
library(devtools) # Load the devtools package.
install_github("Pacific-salmon-assess/WSP-Metrics-Pkg", dependencies = TRUE, build_vignettes = FALSE)
library(WSPMetrics)				
```

**Note: If you want to use the MCMC-based functions, you also need to install [JAGS](http://mcmc-jags.sourceforge.net/).**

### WSP Status Metrics

This package includes functions to calculate the metrics originally defined by Holt et al. (2009), and further refined in subsequent test cases and additional explorations. 


The WSP metrics are designed to capture different considerations related to *status*, and have been used *in combination* to develop *integrated status designations*. Below is a brief overview with general definitions. For more detail about these metrics, alternative calculation approaches, and their use in formal status asessments, go to the [WSP Metrics wiki](https://github.com/SOLV-Code/MetricsTest/wiki). 


**Metric** | **General Definition** | **Variations** | **Function** 
-- | -- | -- | --
Long-Term Trend | Recent Avg / Long-term Avg |  geometric mean vs. arithmetic mean |  *calcLongTermTrendSimple()* for a single vector. *calcLongTermTrend()* for a data frame with Years x Stocks (includes retrospective calculations).
Percent Change  | change over n generations | alternative smoothing (log, gen avg), time window (n gen, n gen +1) | *calcPercChangeSimple()* for a single vector of raw values. *calcPercChange()* for a data frame with Years x Stocks (includes retrospective calculations).
Probability of Decline | Prob(PercChange> X%) | alternative smoothing (log, gen avg), time window (n gen, n gen +1) |   *calcPercChangeSimple()* for regression based calculation in a single vector. *calcPercChange()* for a data frame with Years x Stocks (includes retrospective calculations). *calcPercChangeMCMC()* for a Bayesian probability (based on posterior distribution of regression slopes) calculated on a single vector of raw values.
Relative Abundance - SR-Based BM| Recent Avg compared to biological benchmarks (Sgen, 80%Smsy) | geometric mean vs. arithmetic mean, alternative benchmark estimate approaches | first step will be to integrate a subroutine *calcRickerSgen* that converts Ricker parameters to benchmark estimates. For long-term, consider building a larger module *calcRelAbdBM()* that does the model fitting as well.
Relative Abd - Percentile BM| Recent Avg compared to user-specified %iles | geometric mean vs. arithmetic mean | *calcPercentileSimple()* for a single vector. NOTE: This function only calculates the percentile value. It is the *calcMetrics()* wrapper function that compares this value to user-specified benchmarks and assigns a status category. [Holt et al. 2018](https://www.dfo-mpo.gc.ca/csas-sccs/Publications/ResDocs-DocRech/2018/2018_011-eng.html) recommend percentile-based status benchmarks of *25%* and *75%* for data-limited conservation units of Chum Salmon when productivity is moderate to high (>2.5 recruits/spawner) and harvest rates are low to moderate (≤40%). Subsequent work (**REF?**) identified *25%* and *50%* as **XYZ**. The function *calcMetrics()* uses 0.25 and 0.50 as the default benchmarks for the percentiles.


### Illustration

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




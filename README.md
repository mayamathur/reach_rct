


# Overview

This repository is for the paper:

> [INSERT CITATION]

This repository contains all data (except participants' counties of residence per our IRB approval), intervention materials, questionnaire materials, and analysis code required to reproduce all results in the paper. All methods and statistical analyses were preregistered in detail (LINKS). The minor deviations from those protocols (i.e., analyses that we omitted given the null results) are disclosed in the main text or Supplement. All measures and experiments are reported, and we determined
sample sizes in advance. 


# Materials

[NEEDS TO BE DONE]


# Datasets

The folder `Data` contains versions of the dataset that has been prepped for analysis in R and a more raw version provided by the data management team to the statistical analyst.


# Data preparation and analysis code

We conducted analyses as follows:

1. We ran the code in `prep.R` (which calls `preliminaries.R` and also functions in `helper.R`). The prep script takes the raw datasets and produces the analysis-ready prepped datasets.

2. We ran `analyze.R` to conduct all analyses. This script is organized into analysis "sets", each of which corresponds to one type of model fit to multiple outcomes. The sets are as follows, with bold text indicating preregistered analyses:

    * **Analysis set 1 (preregistered primary analysis)**: Effect at T2 on primary and secondary outcomes
    * Analysis set 1B (post hoc): Effect at T2 on each flourishing domain (complete-case analysis only)
    * **Analysis set 2 (preregistered secondary analysis)**: Effect modification by baseline trait forgiveness
    * Analysis set 3 (sanity check for set 6): Effect at T2 on primary and secondary outcomes within each site
    * **Analysis set 6 (preregistered secondary)**: Global test of site heterogeneity
    * **Analysis set 4 (preregistered secondary)**: Controlling for precision covariates
    * **Analysis set 5A (preregistered secondary)**: Analysis including T3; fixed effects of wave 
    * Analysis set 5B (post hoc): Analysis including T3; omitting fixed effects of wave 
    * Analysis set 5C (post hoc): Analysis including T3; treat wave as continuous variable

The above numbering of the "analysis sets'' matches that in the R code so that the code can be searched easily; that ordering that based on structural rather than conceptual similarity of the analyses. Above, the results are grouped conceptually, so the analysis sets are out of cardinal order.

This script writes various kinds of results files, listed below.


#### Nuances  

* The code files in the `Code (git)` directory are also version-controlled on [Github](https://github.com/mayamathur/ev_rct).

* The code files use the R package `renv` to snapshot the package environment that we had when we analyzed the data. Instructions for how to restore that environment are at the top of the analysis code files. Alternatively, you can directly look at the contents of `renv.lock` to see session information details. Also, the output from `sessionInfo()` is saved in `2022-4-13 sessionInfo.txt`.


# Results files

### The "Tables" directory

The key results are in the directory `Tables`, where results have been aggregated across outcomes for most (not all) analysis "sets". For analysis sets that had too many results to nicely aggregate into a single table, the results are only in the the appropriately numbers "Analysis Set N" folder. See above for the meaning of each analysis set. 

### The "Analysis Set N" folders

Full results of each analysis set (i.e., full coefficients for each regression model) are in the appropriate "Analysis set N" folder, listed above. Each of these folders is sub-organized by missing data method (multiple imputation for primary analyses and complete-case for sensitivity analyses). Then, each table comes in 2 versions: the version suffixed "pretty" has results rounded nicely for manuscript-writing; the version suffixed "raw" is unrounded and has additional columns (e.g., GEE convergence information) that won't be in the manuscript tables. The raw versions are machine-readable.


### Other, less important results files 

* Certain sanity checks appear in the directory `Auxiliary`.

* Selected numerical results in a single csv file, `Results from R/stats_for_paper.csv`. We used that file to pipe numerical results into an initial results writeup prepared in TeX.



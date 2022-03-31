

# PACKAGES  -----------------------------------------------------------

library(dplyr)
library(mice)
library(readr)
library(tableone)
library(testthat)
library(qdapTools)
library(Amelia)
library(tableone)
library(here)
library(stringr)
library(data.table)
library(tidyverse)
library(geepack)
library(geesmv)
library(gee)
library(ggplot2)
library(gridExtra)
library(sandwich)
library(harmonicmeanp)
library(readxl)
library(xlsx)

options(scipen = 999)

# WORKING DIRECTORIES  -----------------------------------------------------------

# these are datasets that still have the county variable
# includes the raw imputations because we use county to help impute
# raw (identifiable) data are stored a level up from the public dirs
raw.data.dir = str_replace_all( string = here(),
                                pattern = "Code",
                                replacement = "Data/Merged site data from Man Yee" )

prepped.data.dir = str_replace_all( string = here(),
                                            pattern = "Code",
                                            replacement = "Data/Prepped" )

imputed.data.dir = str_replace_all( string = here(),
                                    pattern = "Code",
                                    replacement = "Data/Imputed datasets" )



# this script will save some results of sanity checks
results.dir = str_replace_all( string = here(),
                               pattern = "Code",
                               replacement = "Results from R" )

# for auxiliary results and sanity checks
results.aux.dir = paste(results.dir, "/Auxiliary", sep="")


overleaf.dir = "/Users/mmathur/Dropbox/Apps/Overleaf/REACH results and preregistration/R_objects"


code.dir = here()
setwd(code.dir)
source("helper.R")



##### Lists of Variables #####

# primary outcomes (3)
# BSI-depression
# BSI-anxiety
# TRIM
primYNames = c("BSIdep", "BSIanx", "TRIM")
# names with times, as in wide-format data
primYNamesWide = c( paste( "T1_", primYNames, sep=""),
                    paste( "T2_", primYNames, sep=""),
                    paste( "T3_", primYNames, sep="") )

# secondary outcomes (4)
# Trait Forgiveness (TrFS)
# Forbearance Scale (FOR)
# Decision to Forgive Scale (DTFS)
# Flourishing Index (FSFS per codebook in xlsx file)
secYNames = c("TrFS", "FSFS", "DTFS", "FOR" )
# names with times, as in wide-format data
secYNamesWide = c( paste( "T1_", secYNames, sep=""),
                   paste( "T2_", secYNames, sep=""),
                   paste( "T3_", secYNames, sep="") )

allYNames = c(primYNames, secYNames)

# not used in analysis, but included in imputation model
unusedYnames = c("EFS", "PGI", "TSHS")
# names with times, as in wide-format data
unusedYnamesWide = c( paste( "T1_", unusedYnames, sep=""),
                      paste( "T2_", unusedYnames, sep=""),
                      paste( "T3_", unusedYnames, sep="") )

# these are used in sensitivity analyses that control for precision covariates
demoVarsToAnalyze = c("site", "age", "gender")

# auxiliary vars in imputation model
demoVarsAux = c("eth", "educ", "income", "hasReligiousAffil", "religion", "marstat")




### Other Global Vars ###

# rounding digits
digits = 2



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


# WORKING DIRECTORIES  -----------------------------------------------------------

# these are datasets that still have the county variable
# includes the raw imputations because we use county to help impute
# raw (identifiable) data are stored a level up from the public dirs
raw.data.dir = str_replace_all( string = here(),
                                pattern = "Code",
                                replacement = "Data/Merged site data from Man Yee" )
# for intermediate steps that still have county variable
prepped.data.dir.private = str_replace_all( string = here(),
                                            pattern = "Code",
                                            replacement = "Data/Prepped" )

imputed.data.dir = str_replace_all( string = here(),
                                    pattern = "Code",
                                    replacement = "Data/Imputed datasets" )



# this script will save some results of sanity checks
results.dir = str_replace_all( string = here(),
                               pattern = "Code",
                               replacement = "Results from R" )


code.dir = here()

setwd(code.dir)
source("helper.R")



##### Lists of Variables #####

# primary outcomes (3)
# BSI-depression
# BSI-anxiety
# TRIM
primYNames = c("BSI", "TRIM")

# secondary outcomes (4)
#@not sure which is flourishing index vs. forbearance scale
# Trait Forgiveness (TrFS)
# Forbearance Scale (?)
# Decision to Forgive Scale (DTFS)
# Flourishing Index (?)
secYNames = c("TrFS", "FSFS", "DTFS" )

# not used in analysis, but included in imputation model
unusedYnames = c("EFS", "PGI", "TSHS")


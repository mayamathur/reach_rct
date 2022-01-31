

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

# WORKING DIRECTORIES  -----------------------------------------------------------


##### Working Directories - Deidentified Data #####
imputed.data.dir = here( paste("Data/Prepped/", study.string, "/Saved imputations",
                               sep = "") )

prepped.data.dir = here( paste("Data/Prepped/", study.string, sep = "" ) )
# this script will save some results of sanity checks
results.dir = here( paste("Results from R/", study.string, sep = "" ) )

# county political affiliation data
county.prepped.data.dir = here("Data/Prepped")


##### Working Directories - Identifiable Data (Private) #####
# these are datasets that still have the county variable
# includes the raw imputations because we use county to help impute
# raw (identifiable) data are stored a level up from the public dirs
raw.data.dir = str_replace_all( string = here(),
                                pattern = "Linked to OSF \\(EatingVeg\\)",
                                replacement = "Data (IDENTIFIABLE)/Raw" )
# for intermediate steps that still have county variable
prepped.data.dir.private = str_replace_all( string = here(),
                                            pattern = "Linked to OSF \\(EatingVeg\\)",
                                            replacement = "Data (IDENTIFIABLE)/Prepped intermediate" )
imputed.dir.private = str_replace_all( string = here(),
                                       pattern = "Linked to OSF \\(EatingVeg\\)",
                                       replacement = paste( "Data (IDENTIFIABLE)/Prepped intermediate/Study ",
                                                            study,
                                                            "/Raw imputed datasets",
                                                            sep = ""  ) )


code.dir = here("Code (git)")

setwd(code.dir)
source("helper_prep.R")



##### Lists of Variables #####
demoVars = c( "sex",
              "age", 
              "educ",
              "cauc",
              "hisp",
              "black",
              "midEast",
              "pacIsl",
              "natAm", 
              "SAsian",
              "EAsian",
              "SEAsian",
              "party",
              "pDem",
              "state",
              "covid" )

# didn't have this variable in Study 3
if ( study == 3 ) demoVars = demoVars[ !demoVars == "covid" ]

meats = c("chicken", "turkey", "fish", "pork", "beef", "otherMeat")
animProds = c("dairy", "eggs")
decoy = c("refined", "beverages")
goodPlant = c("leafyVeg", "otherVeg", "fruit", "wholeGrain", "legumes")
allFoods = c(meats, animProds, decoy, goodPlant)

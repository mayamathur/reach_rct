
rm(list=ls())


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#                                      PRELIMINARIES
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

##### Packages #####
library(dplyr)
library(mice)
library(readr)
library(tableone)
library(testthat)
library(qdapTools)
library(Amelia)
library(tableone)
library(stringr)

# overwrite old results?
overwrite.res = TRUE

# should we overwrite previous prepped versions of the data?
overwrite.prepped.data = TRUE

# make_derived_vars will want this global var
study = 2
study.string = "Study 2"


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



##### Working Directories - Deidentified Data #####
imputed.data.dir = here( paste("Data/Prepped/", study.string, "/Saved imputations",
                               sep = "") )

prepped.data.dir = here( paste("Data/Prepped/", study.string, sep = "" ) )
# this script will save some results of sanity checks
results.dir = here( paste("Results from R/", study.string, sep = "" ) )

# county political affiliation data
county.prepped.data.dir = here("Data/Prepped")

code.dir = here("Code (git)")

setwd(code.dir)
source("helper_prep.R")

# raw.data.dir = "~/Dropbox/Personal computer/Independent studies/2020/EatingVeg RCT/Linked to OSF (EatingVeg)/Data/Raw/Study 2"
# 
# prepped.data.dir = "~/Dropbox/Personal computer/Independent studies/2020/EatingVeg RCT/Linked to OSF (EatingVeg)/Data/Prepped/Study 2"
# 
# # this script will save some results of sanity checks
# results.dir = "~/Dropbox/Personal computer/Independent studies/2020/EatingVeg RCT/Linked to OSF (EatingVeg)/Results from R/Study 2"
# 
# county.prepped.data.dir = "~/Dropbox/Personal computer/Independent studies/2020/EatingVeg RCT/Linked to OSF (EatingVeg)/Data/Prepped"
# 
# code.dir = "~/Dropbox/Personal computer/Independent studies/2020/EatingVeg RCT/Linked to OSF (EatingVeg)/Code (git)"
# 
# setwd(code.dir)
# source("helper_prep.R")


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

meats = c("chicken", "turkey", "fish", "pork", "beef", "otherMeat")
animProds = c("dairy", "eggs")
decoy = c("refined", "beverages")
goodPlant = c("leafyVeg", "otherVeg", "fruit", "wholeGrain", "legumes")
allFoods = c(meats, animProds, decoy, goodPlant)


##### Read in Wave 1 Data #####
# these data have already been run through TaskMaster's Shiny App to parse the on-task 
#  time strings
# and I already manually removed 2 extra header rows from Qualtrics
setwd(raw.data.dir)
setwd(study.string)
d1 = read.csv("study2_R1_from_qualtrics_noheader.csv", header = TRUE)
expect_equal( nrow(d1), 300 )



################################ RENAME AND RECODE VARIABLES ################################ 

# rename variables
d1 = d1 %>% rename( d1.ID = PROLIFIC_PID,
                    d1.date = StartDate,
                    d1.totalQuestionnaireMin = Duration..in.seconds./60,
                    d1.finishedQuestionnaire = Finished,
                    d1.IPlat = LocationLatitude,
                    d1.IPlong = LocationLongitude,
                    state = stateCounty_1,
                    county = stateCounty_2,
                    importAnimals = animals_Important,
                    importHealth = healthy_Important,
                    importEnviro = enviro_Important,
                    d1.problemsBin = problemsBin,
                    d1.problemsText = problemsText)

# recode checkbox variables as non-mutually-exclusive dummies
d1 = recode_checkboxes(.d = d1, var = "race")

# combined state-country variable for later merging joy
d1$stateCounty = paste( d1$state, d1$county, sep = " " )


##### Video Time Variables (TaskMaster and Qualtrics) #####
# combine video time variables (1 for each treatment)
d1$videoMinQualtrics = d1$videoTime_Page.Submit  # still in seconds
d1$videoMinQualtrics[ is.na(d1$videoTime_Page.Submit) ] = d1$videoTime_Page.Submit.1[ is.na(d1$videoTime_Page.Submit) ]
# convert to minutes
d1$videoMinQualtrics = d1$videoMinQualtrics / 60
# sanity check
expect_equal( any(is.na(d1$videoMinQualtrics)), FALSE )
expect_equal( min(d1$videoMinQualtrics) > 20, TRUE ) 


# merge in county-level politics data (already prepped by data_prep_counties.R)
#  this just adds the variable pDem to the dataset
setwd(county.prepped.data.dir)
cn = read.csv("counties_prepped.csv")
d1 = merge(d1, cn, by = "stateCounty")



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#                                      MAKE DERIVED VARIABLES
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

# recode CC dataset
d2 = make_derived_vars(d1)


# make intention variables (not in make_derived_vars because it wasn't in Study 1)
# continuous
# signed so that *positive* intervention effects are good
table(d2$intention, useNA="ifany")
d2$intentionCont = dplyr::recode( d2$intention,
                                  a.stronglyDecrease = 3,
                                  b.decrease = 2,
                                  c.somewhatDecrease = 1,
                                  d.noChange = 0,
                                  e.somewhatIncrease = -1,
                                  f.increase = -2,
                                  g.stronglyIncrease = -3 ) 
table(d2$intentionCont)

# binary
d2$intentionReduce = d2$intentionCont > 0
mean(d2$intentionReduce)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#                                    PRETTIFY VARIABLES
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

d2$treat.pretty = NA
d2$treat.pretty[ d2$treat == 0 ] = "Control"
d2$treat.pretty[ d2$treat == 1 ] = "Documentary"


################################ EXCLUDE SUBJECTS IF NEEDED ################################

# review subjects' stated problems to see if any are serious enough to exclude
setwd(results.dir)
write.csv( d2 %>% select(d1.problemsText) %>%
             filter( d1.problemsText != ""),
           "study2_R1_problemsText_for_review.csv")
# nothing serious

# any repeated Prolific IDs?
t = d2 %>% group_by(pID) %>%
  summarise(n())
table( t$`n()` )  # responses per pID
# no repeats



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#                                    WRITE RESULTS
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

setwd(prepped.data.dir)
write.csv(d2, "prepped_data.csv")


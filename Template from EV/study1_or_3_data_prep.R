
# Note: For Study 1, when creating the time-on-task variable, need to use page 3 time on and off task
# from the TaskMaster Shiny parser, whose sum agrees with Qualtrics' own timer

rm(list=ls())

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#                                      PRELIMINARIES
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


##### Set Parameters Here #####
# overwrite old results?
overwrite.res = FALSE

# should we overwrite previous prepped versions of the data?
overwrite.prepped.data = TRUE

# should sanity checks be run?
run.sanity = TRUE

# should we impute from scratch or read in saved datasets?
# from scratch takes about 5 min
impute.from.scratch = FALSE
# number of imputations
M = 10 

# which study's data to prep?
# must be 1 or 3
study = 3
# for making strings
if ( study %in% c(1,3) ) study.string = paste("Study", study, sep = " ") else stop("Invalid study spec.")


##### Packages #####
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



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#                                   PREP WAVE 1 (BASELINE) DATA
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

##### Read in Wave 1 Data #####
# these data have already been run through TaskMaster's Shiny App to parse the on-task 
#  time strings
# and I already manually removed 2 extra header rows from Qualtrics
setwd(raw.data.dir)
if ( study == 1 ){
  setwd("Study 1")
  w1 = read.csv("wave1_R1_noheader_taskmaster.csv", header = TRUE)
} else if ( study == 3 ) {
  setwd("Study 3")
  w1 = read.csv("study3_wave1_R1_from_qualtrics_noheader.csv", header = TRUE)
}

# people who closed survey after starting
table(w1$Finished)

# check number of rows vs. Qualtrics
# this includes people who weren't even randomized because they closed
#  survey before that point
if ( study == 1 ) expect_equal( nrow(w1), 650 )
if ( study == 3 ) expect_equal( nrow(w1), 797 )

################################ RENAME AND RECODE VARIABLES ################################ 

# rename variables

if ( study == 1 ) w1 = w1 %>% rename( w1.ID = PROLIFIC_PID )
if ( study == 3 ) {
  w1 = w1 %>% rename( w1.ID = ID,
                      # order of foods here can be confirmed using the "withheader" raw dataset's
                      #  second header row:
                      "pledgeChicken" = pledge_1,
                      "pledgeFish" = pledge_2,
                      "pledgePork" = pledge_3,
                      "pledgeBeef"  = pledge_4,
                      "pledgeOtherMeat" = pledge_5,
                      "pledgeEggs" = pledge_6,
                      "pledgeDairy" = pledge_7,
                      "pledgeStrategiesFreeText" = pledgeStrategies_6_TEXT )
}

w1 = w1 %>% rename( w1.date = StartDate,
                    w1.totalQuestionnaireMin = Duration..in.seconds./60,
                    w1.finishedQuestionnaire = Finished,
                    w1.IPlat = LocationLatitude,
                    w1.IPlong = LocationLongitude,
                    state = stateCounty_1,
                    county = stateCounty_2,
                    videoContent = attention,
                    w1.problemsBin = problemsBin,
                    w1.problemsText = problemsText)

# recode checkbox variables as non-mutually-exclusive dummies
w1 = recode_checkboxes(.d = w1, var = "race")

# combined state-country variable for later merging joy
# in Study 3, it was possible for people to have a missing stateCounty (unintentionally on our part)
if ( study == 3 ){
  w1$state[ w1$state == "" ] = NA
  w1$county[ w1$county == "" ] = NA
}
w1$stateCounty = paste( w1$state, w1$county, sep = " " )
w1$stateCounty[ is.na(w1$state) | is.na(w1$county) ] = NA


# passing manipulation check
w1$passCheck = (w1$videoContent == "The ways we raise animals for human consumption causes the animals to suffer.")


##### Video Time Variables (TaskMaster and Qualtrics) #####

if ( study == 1 ) {
  # combine video time variables (1 for each treatment)
  w1$videoMinQualtrics = w1$videoTime_Page.Submit  # still in seconds
  w1$videoMinQualtrics[ is.na(w1$videoTime_Page.Submit) ] = w1$videoTime_Page.Submit.1[ is.na(w1$videoTime_Page.Submit) ]
  # convert to minutes
  w1$videoMinQualtrics = w1$videoMinQualtrics / 60
  # sanity check
  expect_equal( any(is.na(w1$videoMinQualtrics)), FALSE )
  
  # TaskMaster data about on-task time 
  w1$onTaskMin = w1$Page_3_TimeOnPage/60
  w1$offTaskMin = w1$Page_3_TimeOffPage/60
  w1$totalTaskMin = w1$onTaskMin + w1$offTaskMin
  # sanity check: should generally be similar
  # but could potentially differ if, e.g., subject closes tab and starts it up again
  table( abs( w1$totalTaskMin - w1$videoMinQualtrics ) < .01 )
  w1$finishedVid = w1$onTaskMin >= 20
  
  # percentage of the 20 minutes for which people are on task
  100 * round( summary( w1$onTaskMin / 20 ), 2 )
  # percentage of total time on the page for which people are on task
  100 * round( summary( w1$onTaskMin / w1$totalTaskMin ), 2 )
  
}


################################ MERGE IN COUNTY POLITICS DATA ################################ 

# merge in county-level politics data (already prepped by data_prep_counties.R)
#  this just adds the variable pDem to the dataset
setwd(county.prepped.data.dir)
cn = read.csv("counties_prepped.csv")

w1 = left_join( w1, cn, 
                by = "stateCounty" )

if ( study == 1 ) expect_equal( nrow(w1), 650 )
if ( study == 3 ) expect_equal( nrow(w1), 797 )



################################ DROP AND REORGANIZE VARIABLES ################################ 

if ( study == 1 ) {
  w1 = w1 %>% select( # analysis variables
    w1.ID,
    w1.date,
    treat,
    sex,
    age, 
    educ,
    cauc,
    hisp,
    black,
    midEast,
    pacIsl,
    natAm, 
    SAsian,
    EAsian,
    SEAsian,
    party,
    pDem,
    state,
    county,
    stateCounty,
    passCheck,
    
    # related to analysis variables but not directly used 
    #  in analysis
    videoContent,
    
    # non-analysis meta-data
    w1.totalQuestionnaireMin,
    w1.finishedQuestionnaire,
    w1.IPlat,
    w1.IPlong,
    onTaskMin,
    offTaskMin,
    w1.problemsBin,
    w1.problemsText )
}


if ( study == 3 ) {
  w1 = w1 %>% select( # analysis variables
    w1.ID,
    w1.date,
    treat,
    sex,
    age, 
    educ,
    highEduc,
    cauc,
    hisp,
    black,
    midEast,
    pacIsl,
    natAm, 
    SAsian,
    EAsian,
    SEAsian,
    party,
    pDem,
    state,
    county,
    stateCounty,
    targetDemographics,
    passCheck,
    
    # related to analysis variables but not directly used 
    #  in analysis
    videoContent,
    healthFreeText,
    enviroFreeText,
    animalFreeText,
    
    pledgeChicken,
    pledgeFish,
    pledgePork,
    pledgeBeef,
    pledgeOtherMeat,
    pledgeEggs,
    pledgeDairy,
    pledgeDateGoal,
    pledgeStrategies,
    pledgeStrategiesFreeText,
    madeReducePledge,
    madeEliminatePledge,
    
    # non-analysis meta-data
    w1.totalQuestionnaireMin,
    w1.finishedQuestionnaire,
    w1.IPlat,
    w1.IPlong,
    
    w1.problemsBin,
    w1.problemsText )
}



################################ EXCLUDE SUBJECTS IF NEEDED ################################

# review subjects' stated problems to see if any are serious enough to exclude
setwd(results.dir)
setwd("Sanity checks after W1")
write.csv( w1 %>% select(w1.ID, w1.problemsText) %>%
             filter( w1.problemsText != ""),
           "w1_R1_problemsText_for_review.csv")
# Study 1: nothing requiring exclusion
#  mostly just confusion from control subjects about why none of manipulation
# check items matched the control video content

# any repeated Prolific IDs?
t = w1 %>% group_by(w1.ID) %>%
  summarise(n())
table( t$`n()` )  # responses per pID

# **Study 1: one person somehow did it twice; note this in manuscript
# this is the only exclusion for W1
# Study 2: same; one person did it twice
dupID = w1$w1.ID[ duplicated(w1$w1.ID) ]

# look at the duplicated one
w1 %>% filter(w1.ID == dupID)
# this person somehow did the questionnaire twice within a span of 30 minutes

# keep only this person's first submission
w1 = w1 %>% filter( !duplicated(w1.ID) )

# keep only subjects who were randomized
w1 = w1 %>% filter( !is.na(treat) )

if ( study == 1 ) expect_equal( nrow(w1), 649 )
if ( study == 3 ) expect_equal( nrow(w1), 665 )

################################ SAVE PREPPED W1 DATA ################################ 


if ( overwrite.prepped.data == TRUE ) {
  setwd(prepped.data.dir.private)
  setwd(study.string)
  write.csv(w1, "prepped_data_W1_R1.csv")
}


################################ SANITY CHECKS ################################ 

# read back in
setwd(prepped.data.dir.private)
setwd(study.string)
w1 = read.csv("prepped_data_W1_R1.csv")


if ( run.sanity == TRUE ){
  # quick look at demographics
  if ( study == 1 ) {
    temp = w1 %>% select( c("treat",
                            demoVars[ !demoVars == "covid" ],  # because this var was in W2 only
                            "passCheck",
                            "onTaskMin") )
  }
  
  if ( study == 3 ) {
    temp = w1 %>% select( c("treat",
                            demoVars[ !demoVars == "covid" ],
                            "passCheck") )
  }
  
  t = CreateTableOne(data = temp, strata = "treat")
  setwd(results.dir)
  setwd("Sanity checks after W1")
  write.csv( print(t, noSpaces = TRUE, printToggle = FALSE), 
             "ugly_table1_W1.csv")
  
  
  ##### Look for Expected Associations Among Variables #####
  
  # reported parties vs. counties' pDem
  # but exclude states with very few responses
  summary( glm( party == "Democrat" ~ pDem,
                data = w1,
                family = binomial( link = "log" ) ) )
  # makes sense :)
  
  # age and education
  summary( lm( age ~ educ, data = w1 ) )
  
  
  ##### Map of Subjects' Locations #####
  # just for fun
  # location map
  temp = w1 %>% group_by(state) %>%
    summarise( count = n(),
               pDem = mean(pDem),
               pDemReported = mean( (party == "Democrat")[ party %in% c("Democrat", "Republican") ] ) )
  dim(temp) # should be <= 52
  temp$region <- tolower(temp$state)
  library(ggplot2)
  library(maps)
  library(mapproj)
  states <- map_data("state")
  map.df <- merge(states,temp, by="region", all.x=T)
  map.df <- map.df[order(map.df$order),]
  
  ggplot(map.df, aes(x=long,y=lat, group=group))+
    geom_polygon(aes(fill=count))+
    geom_path()+ 
    scale_fill_gradientn(colours=rev(heat.colors(10)),na.value="grey90")+
    coord_map()
  ggsave("states_map_W1.pdf",
         height = 4.52,
         width = 7.24,
         units = "in")
  
  # # pDem from MIT data
  # ggplot(map.df, aes(x=long,y=lat, group=group))+
  #   geom_polygon(aes(fill=pDem))+
  #   geom_path()+ 
  #   scale_fill_gradient(low = "red", high = "blue", na.value="grey90")+
  #   coord_map()
}



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#                              PREP AND MERGE WAVE 2 (FOLLOW-UP) DATA
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


##### Read in Wave 2 Data #####
# I already manually removed 2 extra header rows from Qualtrics
setwd(raw.data.dir)
if ( study == 1 ) {
  setwd("Study 1")
  w2 = read.csv( "wave2_R2_noheader.csv", header = TRUE)
}


if ( study == 3 ) {
  setwd("Study 3")
  w2 = read.csv( "study3_wave2_R1_from_qualtrics_noheader.csv", header = TRUE)
}



# a couple people started the survey, but then did not finish
table(w2$Finished)
#@REMOVED THIS; COULD SLIGHTLY AFFECT MULTIPLE IMPUTATION (already re-run)
#if ( study == 1 ) w2 = w2[ w2$Finished == TRUE, ]

nrow(w2)  # number of completers
nrow(w2) / nrow(w1)  # completion rate vs. n from wave 1


# rename wave 2 variables
if ( study == 1 ) w2 = w2 %>% rename( w2.ID = PROLIFIC_PID )
if ( study == 3 ) w2 = w2 %>% rename( w2.ID = ID )

w2 = w2 %>% rename( w2.date = StartDate,
                    w2.totalQuestionnaireMin = Duration..in.seconds./60,
                    w2.finishedQuestionnaire = Finished,
                    w2.IPlat = LocationLatitude,
                    w2.IPlong = LocationLongitude,
                    w2.problemsBin = problemsBin,
                    w2.problemsText = problemsText,
                    importAnimals = animals_Important,
                    importHealth = healthy_Important,
                    importEnviro = enviro_Important)


# more variable lists (created here because they use the actual dataset)
foodVars = c( names(w2)[ grepl(pattern = "Freq", names(w2) ) ],
              names(w2)[ grepl(pattern = "Ounces", names(w2) ) ] )
expect_equal( 15*2, length(foodVars) )  # 15 food variables (in prereg) * 2 variables each

secondaryY = c("spec",
               "dom",
               "activ",
               "importHealth",
               "importEnviro",
               "importAnimals")



################################ EXCLUDE SUBJECTS IF NEEDED ################################

# review subjects' stated problems to see if any are serious enough to exclude
setwd(results.dir)
setwd("Sanity checks after W2")
write.csv( w2 %>% select(w2.ID, w2.problemsText) %>%
             filter( w2.problemsText != ""),
           "w2_R2_problemsText_for_review.csv")


# any repeated IDs?
t = w2 %>% group_by(w2.ID) %>%
  summarise(n())
table( t$`n()` )  # responses per pID

# if so, keep only each person's first submission
w2 = w2 %>% filter( !duplicated(w2.ID) )

# sanity check
# were all wave 2 subjects also in wave 1?
# in Study 1, it should be impossible to have an ID in W2 that doesn't
#  appear in W1...
if ( study == 1 ) expect_equal( all( w2$w2.ID %in% w1$w1.ID == TRUE ), TRUE )
# ...but in Study 3, they could have mis-entered their ID code
( nBadIDs = sum( !w2$w2.ID %in% w1$w1.ID ) )
# exclude these
w2 = w2[ w2$w2.ID %in% w1$w1.ID, ]


nrow(w2)


################################ MERGE WAVES ################################

# read wave 1 in again
setwd(prepped.data.dir.private)
setwd(study.string)
w1 = read.csv( "prepped_data_W1_R1.csv", header = TRUE)

# merge waves
d = merge( w1, w2, by.x = "w1.ID", by.y = "w2.ID", all.x = TRUE)

if ( study == 1 ) expect_equal( nrow(d), 649 )
if ( study == 3 ) expect_equal( nrow(d), 665 )



##### Recode Character Vars as Factors #####
# this is needed to avoid "constant" problem in mice's loggedEvents
sum(sapply(d, is.character))  # number of character vars
d = d %>% mutate_if(sapply(d, is.character), as.factor)
sum(sapply(d, is.character))  # check again; should be 0

# drop variables that aren't useful for analysis
# also drop all identifying variables so that imputed datasets and prepped main datset
#   are deidentified
d$ID = d$w1.ID  # have just one ID variable

if ( study == 1 ) {
  d = d %>% select( -c("X",
                       "w1.IPlat",
                       "w1.IPlong",
                       "EndDate",
                       "Progress",
                       "RecordedDate",
                       "ResponseId",
                       "RecipientLastName",
                       "RecipientFirstName",
                       "RecipientEmail",
                       "ExternalReference",
                       "Status",
                       "w2.IPlat",
                       "w2.IPlong",
                       "DistributionChannel",
                       "UserLanguage",
                       "IPAddress",
                       "pID",
                       "w1.problemsBin",
                       "w1.problemsText",
                       "w2.problemsBin",
                       "w2.problemsText",
                       "w1.ID") )
}


if ( study == 3 ) {
  d = d %>% select( -c("X",
                       "w1.IPlat",
                       "w1.IPlong",
                       "EndDate",
                       "Progress",
                       "RecordedDate",
                       "ResponseId",
                       "RecipientLastName",
                       "RecipientFirstName",
                       "RecipientEmail",
                       "ExternalReference",
                       "Status",
                       "w2.IPlat",
                       "w2.IPlong",
                       "DistributionChannel",
                       "UserLanguage",
                       "IPAddress",
                       "ID",
                       "w1.problemsBin",
                       "w1.problemsText",
                       "w2.problemsBin",
                       "w2.problemsText",
                       "w1.ID",
                       "wantsRaffle", 
                       "raffleEmail") )
}



# check the remaining variables
names(d)


# save intermediate dataset
setwd(prepped.data.dir.private)
setwd(study.string)
write_interm(d, "d_intermediate_1.csv")



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#                                      MAKE DERIVED VARIABLES
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


d = read_interm("d_intermediate_1.csv")


# recode CC dataset
# this is now de-ID'ed thanks to make_derived_vars
d2 = make_derived_vars(d)

# sanity checks on make_derived_vars
if ( run.sanity == TRUE ) {
  
  ### Do Missingness Patterns Make Sense? ###
  # mainY should be missing whenever any frequency variable is missing
  # but not necessarily when the ounces variables are missing, because those 
  #  are also missing if the subject reported never eating that food
  freqVars = c("chicken_Freq",
               "turkey_Freq",
               "fish_Freq",
               "pork_Freq",
               "beef_Freq",
               "otherMeat_Freq",
               "dairy_Freq",
               "eggs_Freq")
  
  temp = d2[ ,freqVars ]
  # Study 3: Equals 0 or 8, meaning that either someone dropped out of study entirely
  #  or they answered all food freq questions, which is good
  d2$numFoodFreqMissing = rowSums( is.na(temp) )
  expect_equal( d2$numFoodFreqMissing > 0, is.na(d2$mainY) )
  
  
  ### Reproduce One Food Variable ###
  # sanity check on recode_food_Y
  beef_Freq = suppressWarnings( dplyr::recode( d$beef_Freq,
                                               a.Never = 0,
                                               b.1Weekly = 1,
                                               c.2Weekly = 2,
                                               d.3to4Weekly = 3.5,
                                               e.5to6Weekly = 5.5,
                                               f.1Daily = 7,
                                               g.2PlusDaily = 14) )
  
  table(beef_Freq, useNA = "ifany")
  
  myBeef = beef_Freq * d$beef_Ounces
  # overwrite spurious NAs that happen when frequency is 0, so oz are NA
  myBeef[ beef_Freq == 0 ] = 0 
  
  expect_equal(myBeef, d2$beef)
  
  ### Reproduce One Psych Variable: Speciesism ###
  # sanity check on recode_psych_var
  vars = names(d)[ grepl( pattern = "spec", 
                          x = names(d) ) ]
  
  temp = d[ ,vars ]
  
  # reverse-code as needed
  temp$X5_spec = -temp$X5_spec 
  
  my.spec.raw = rowSums(temp)
  
  # standardize
  my.spec.std = ( my.spec.raw - mean( my.spec.raw, na.rm = TRUE ) ) / sd( my.spec.raw, na.rm = TRUE )
  
  expect_equal(my.spec.std, d2$spec)
  
  ### Reproduce Composite Food Outcomes ###
  # start from the recoded specific-food outcomes
  my.totalMeat = d2$chicken + d2$turkey + d2$fish + d2$pork + d2$beef + d2$otherMeat
  expect_equal( my.totalMeat, d2$totalMeat )
  
  my.animProds = d2$dairy + d2$eggs
  expect_equal( my.animProds, d2$totalAnimProd )
  
  expect_equal( my.totalMeat + my.animProds, d2$mainY)
}




# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#                               MULTIPLE IMPUTATION FOR STUDY 1 OR 3 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# for Study 3, the MI models weren't converging when we tried to impute BEFORE
# making derived variables
# so instead do it after
#**this still creates logged events: note this in manuscript
#if ( impute.from.scratch == TRUE & study == 3 ) {
if ( impute.from.scratch == TRUE & study %in% c(1,3) ) {
  ini = mice(d2, m=1, maxit = 0 )
  
  
  # check default methods
  # all PMM, as desired
  ini$method
  
  # variables to be imputed: those measured at follow-up
  # these are the only vars that can have missing data
  # lists of variables from helper_analysis.R::prelims()
  meats <<- c("chicken", "turkey", "fish", "pork", "beef", "otherMeat")
  animProds <<- c("dairy", "eggs")
  decoy <<- c("refined", "beverages")
  goodPlant <<- c("leafyVeg", "otherVeg", "fruit", "wholeGrain", "legumes")
  allFoods <<- c(meats, animProds, decoy, goodPlant)
  
  foodVars <<- c( names(d)[ grepl(pattern = "Freq", names(d) ) ],
                  names(d)[ grepl(pattern = "Ounces", names(d) ) ] )
  
  # exploratory psych variables
  psychY <<- c("importHealth",
               "importEnviro",
               "importAnimals",
               "activ",
               "spec",
               "dom")
  
  # secondary food outcomes
  secFoodY <<- c("totalMeat",
                 "totalAnimProd",
                 meats,
                 animProds,
                 "totalGood")
  toAnalyze = c("mainY",
                secFoodY,
                psychY )
  
  # variables to be used in imputation model:
  # "real" variables measured at baseline and the F/U variables
  w1Vars = c( "treat",
              demoVars )
  # state has too many categories to work well as predictor
  impModelVars = w1Vars[ !w1Vars == "state" ]
  
  # make own predictor matrix by modifying mice's own predictor matrix to keep structure the same
  #  from mice docs: "Each row corresponds to a variable block, i.e., a set of variables to be imputed. A value of 1 means that the column variable is used as a predictor for the target block (in the rows)"
  myPred = ini$pred
  myPred[myPred == 1] = 0
  # impute all F/U variables using the sensible ones from baseline as well as all the other F/U vars
  myPred[ names(d2) %in% toAnalyze, # vars to be imputed
          names(d2) %in% c(impModelVars, toAnalyze) ] = 1  # ...and vars in the imputation model
  diag(myPred) = 0  # but a variable can't impute itself
  sum(myPred)
  
  
  myMethod = ini$method
  myMethod[ !names(myMethod) %in% toAnalyze ] = ""
  table(myMethod)
  
  # imputing secfoodY variables themselves seem to cause issues
  imps = mice( d2,
               m=M,  
               predictorMatrix = myPred,
               method = myMethod,
               seed = 451)
  
  imps$loggedEvents$dep
  
  # make sure there is no missing data in the imputations
  any.missing = apply( complete(imps,1)[ ,toAnalyze],
                       2,
                       function(x) any(is.na(x)) ) # should be FALSE
  if ( any(any.missing) == TRUE ) warning("Imputed datasets have missing data! Look at logged events.")
  
  #cbind(d2$chicken, complete(imps,1)$chicken)
  
  
  ##### Save Imputations for Reproducibility #####
  if ( overwrite.res == TRUE ) {
    
    # save imputations for reproducibility
    setwd(imputed.dir.private)
    save( imps, file = "imputed_datasets.RData" )
    
    for (i in 1:M) {
      impDat = complete(imps, i)
      write.csv( impDat,
                 paste("imputed_dataset_prepped", i, ".csv", sep="") )
    }
  }
  
}



##### Recode the Imputations - Either Study #####

if ( study %in% c(1,3) ) {
  
  # first read imps back in
  # we're doing this even if impute.from.scratch=TRUE to have same data format
  # i.e., a list of imputed datasets instead of a mids object
  setwd(imputed.dir.private)
  
  # avoid trying to recode other files in that directory
  toRecode = paste("imputed_dataset_prepped", 1:M, ".csv", sep="")
  
  imps = lapply( toRecode,
                 function(x) suppressMessages(read_csv(x)) )
  
  # saves a new version of the imputation dataset (does not overwrite the old one)
  setwd(imputed.dir.private)
  for ( i in 1:M ) {
    imp = as.data.frame( imps[[i]] )
    
    # remove county variables to deidentify
    if ( "stateCounty" %in% names(imp) ) imp = imp %>% select(-stateCounty)
    if ( "county" %in% names(imp) ) imp = imp %>% select(-county)
    
    # save into the public directory now that the datasets are de-ID'ed
    setwd(imputed.data.dir)
    write.csv( imp, paste("imputed_dataset_prepped_", i, ".csv", sep="") )
  }
}




# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#                                    PRETTIFY VARIABLES
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

d2$treat.pretty = NA
d2$treat.pretty[ d2$treat == 0 ] = "Control"
d2$treat.pretty[ d2$treat == 1 ] = "Documentary"

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
#                                    WRITE RESULTS
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

setwd(prepped.data.dir)
write.csv(d2, "prepped_merged_data.csv")


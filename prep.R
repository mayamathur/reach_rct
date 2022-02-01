
# NOTES -----------------------------------------------------------

# To do:
# - Exclude sites with <70% retention at wave 2
# - Standardize all outcomes
# - Imputation

# Questions for Man Yee:
# - Do any items need reverse-coding?
# - South Africa is only 54% complete for T2
# - Shouldn't there be 2 BSI variables?

# - Subject 171 has gender 0, but that option isn't in the codebook
# - race "Colored" vs. "Black African"; the former shows up in South Africa only
# - codebook says there should be 6 income levels, but in fact there are 9
# - similar issue with religion, and also there is both Christian and Catholic and Protestant

# PRELIMINARIES -----------------------------------------------------------

library(here)
setwd(here())
# load packages, set working dirs, etc.
source("preliminaries.R")


##### Set Parameters Here #####
# overwrite old results?
overwrite.res = FALSE

# should we overwrite previous prepped versions of the data?
overwrite.prepped.data = TRUE

# should sanity checks be run?
run.sanity = FALSE

# should we impute from scratch or read in saved datasets?
impute.from.scratch = TRUE
# number of imputations
M = 10 

# read in raw data
setwd(raw.data.dir)
d = read_csv("RCT Compiled dataset.csv") 



# SIMPLE SANITY CHECKS -----------------------------------------------------------

# ~ Missing data ----------------------------


if ( run.sanity == TRUE ) {
  # percentages of *non-missing* data on each variable
  # across all sites
  t = d %>% summarise( across( everything(), ~ round( 100*mean( !is.na(.x) ) ) ) )
  
  # by site
  t = d %>% group_by(site) %>%
    summarise( across( everything(), ~ round( 100*mean( !is.na(.x) ) ) ) )
  setwd(results.dir)
  setwd("Auxiliary")
  fwrite(t, "nonmissing_perc_by_site.csv")
  
  # look at missingness on T2 vars for each site
  View( t %>% select( c(site, contains("T2") ) ) )
  
  
  # sanity check
  # **South Africa is only 54% complete for T2
  mean( !is.na(d$T2_TRIM1[ d$site == 2 ]) )      
  
}



# INITIAL RECODING -----------------------------------------------------------

d$site = recode(d$site, 
                `1` = "Hong Kong",
                `2` = "South Africa",
                `3` = "Colombia",
                `4` = "Indonesia",
                `5` = "Ukraine (UISA)",
                `6` = "Ukraine (Realis)",
                .default = "RECODE TROUBLE")
table(d$site)


d$treat = recode(d$Group,
                 `1` = 1,
                 `2` = 0)

#@ subject 171 has gender=0, but that's not in codebook
d$gender = recode(d$GENDER,
                  `1` = "Male", 
                  `2` = "Female",
                  `3` = "Other",
                  .default = "RECODE TROUBLE")

d$eth = recode(d$Ethnicity,
               `1` = "Asian", 
               `2` = "Black African",
               `3` = "Coloured",
               `4` = "Indian",
               `5` = "White",
               `6` = "Other",
               .default = "RECODE TROUBLE")

d$educ = recode(d$EDUCATION,
               `1` = "a.LtHS", 
               `2` = "b.HS",
               `3` = "c.CollegePlus",
               `4` = "d.Other",
               .default = "RECODE TROUBLE")

# these differed based on countries' own currencies
#@check about number of categories
d$income = recode(d$HOUSEHOLD_INCOME,
                  `1` = "a", 
                  `2` = "b",
                  `3` = "c",
                  `4` = "d",
                  `5` = "e",
                  `6` = "f",
                  `7` = "g",
                  `8` = "h",
                  `9` = "i",
                  .default = "RECODE TROUBLE")

d$isReligious = recode(d$RELIGION_YN,
                       `1` = 1,
                       `2` = 0)

d$religion = recode(d$RELIGION,
                    `1` = "Christian", 
                    `2` = "Catholic",
                    `3` = "Protestant",
                    `4` = "Buddhist",
                    `5` = "Muslim",
                    `6` = "Other",
                    `8` = "UNKNOWN_CAT",
                    .default = "RECODE TROUBLE")


d$marstat = recode(d$MARRIAGE,
                   `1` = "Single", 
                   `2` = "In relationship",
                   `3` = "Married",
                   `4` = "Separated",
                   `5` = "Divorced",
                   `6` = "Widowed",
                   .default = "RECODE TROUBLE")




# relabel the TFS scales so that recode_psych_var doesn't confused them with DTFS
( varNames = stringsWith(pattern="_TFS", names(d)) )
newNames = str_replace(string = varNames, pattern = "_TFS", replacement = "_TrFS")
names(d)[ names(d) %in% varNames ] = newNames



# RENAME VARIABLES THAT DON'T NEED RECODING -----------------------------------------------------------

d = d %>% rename( age = AGE )

# DROP VARIABLES -----------------------------------------------------------

# need to remove T1_DTFS_EVENT because it will confuse recode_psych_var
d = d %>% select( -c(T1_DATE,
                     T2_DATE,
                     T3_DATE,
                     GENDER,
                     GENDER_OTHER,
                     Group,
                     RACE_OTHER,
                     Ethnicity,
                     MARRIAGE,
                     EDUCATION,
                     EDU_OTHER,
                     HOUSEHOLD_INCOME,
                     RELIGION,
                     RELIGION_YN,
                     RELI_OTHER,
                     FRE_RELI_EVENT,
                     T1_severity,
                     T1_DTFS_EVENT) )




# RECODE SCALES -----------------------------------------------------------

# scale: part of scale string that appears in each subscale's variable name (e.g., "spec" for speciesism); also becomes the name of the new composite variable
# revCode: quoted names of any subscales that need to be reverse-coded


for ( .v in c(primYNames, secYNames, unusedYnames) ) {
  
  # make new variable for a given time point
  varName = paste("T1", .v, sep = "_")
  d = recode_psych_scale(.d = d,
                         scale = varName, 
                         revCode = NA,
                         printCorMat = TRUE,
                         dropSubscaleVars = TRUE)
  
  # make new variable for a given time point
  varName = paste("T2", .v, sep = "_")
  d = recode_psych_scale(.d = d,
                         scale = varName, 
                         revCode = NA,
                         printCorMat = TRUE,
                         dropSubscaleVars = TRUE)
  
  # make new variable for a given time point
  varName = paste("T3", .v, sep = "_")
  d = recode_psych_scale(.d = d,
                         scale = varName, 
                         revCode = NA,
                         printCorMat = TRUE,
                         dropSubscaleVars = TRUE)
  
  # check it
  message( paste("Final variables for", .v, ":",
               paste( stringsWith( pattern=.v, names(d) ), collapse = ", " ) ) )
  
}



# quick sneak peak at results!!!!
summary( lm(T2_TRIM ~ treat, data = d) )
summary( lm(T2_DTFS ~ treat, data = d) )


# IMPUTATION IN WIDE FORMAT-----------------------------------------------------------


#bm: ready to try this with newly trimmed df :)

if ( impute.from.scratch == TRUE ) {
  ini = mice(d, m=1, maxit = 0 )
  
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











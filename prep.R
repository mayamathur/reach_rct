
# PRELIMINARIES -----------------------------------------------------------

library(here)
setwd(here())
# load packages, set working dirs, etc.
source("preliminaries.R")


##### Set Parameters Here #####


# overwrite old results?
overwrite.res = TRUE

# should we overwrite previous prepped versions of the data?
overwrite.prepped.data = TRUE

# should sanity checks be run?
run.sanity = TRUE

# should we impute from scratch or read in saved datasets?
impute.from.scratch = TRUE
# number of imputations
M = 10

# read in raw data
setwd(raw.data.dir)
setwd("2022-7-15")
d = read_excel("22-7-15 Merged Dataset (PTGI Included).xlsx", sheet = 1) 


# fix site_t3 variable because we also need to exclude Ukraine-Realis
#  since it didn't collect any data at T3
d$site_t3[ d$site == "Ukraine (Realis)" ] = "no"

# fix irregular variable names
# currently coded with a number separating "BSI" from "dep", which will confuse later recoding
namesToFix = stringsWith( pattern = "_dep", names(d) )
inds = whichStrings( pattern = "_dep", names(d) )
names(d)[inds] = str_replace(string = namesToFix, pattern = "BSI", replacement = "BSIdep_")

namesToFix = stringsWith( pattern = "_anx", names(d) )
inds = whichStrings( pattern = "_anx", names(d) )
names(d)[inds] = str_replace(string = namesToFix, pattern = "BSI", replacement = "BSIanx_")



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
}



# INITIAL RECODING -----------------------------------------------------------

# unique ID, because some participants from different sites have same ID
d$uid = 1:nrow(d)

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

# Richard's suggested category names for table:
#"Some secondary education or below," "Completed secondary education," and "Some postsecondary education or higher."
d$educ = recode(d$EDUCATION,
               `1` = "a.LtHS", 
               `2` = "b.HS",
               `3` = "c.CollegePlus",
               .default = "RECODE TROUBLE")


d$income = recode(d$HOUSEHOLD_INCOME,
                  `1` = "a.LtAvg", 
                  `2` = "b.Avg",
                  `3` = "c.1SDAboveAvg",
                  `4` = "d.3SDAboveAvg",
                  .default = "RECODE TROUBLE")

d$hasReligAffil = recode(d$RELIGION_YN,
                       `1` = 1,
                       `2` = 0)

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
d$age = as.numeric(d$age)

# DROP VARIABLES -----------------------------------------------------------

# need to remove T1_DTFS_EVENT because it will confuse recode_psych_var
d = d %>% select( -c(#T1_DATE,
                     #T2_DATE,
                     #T3_DATE,
                     GENDER,
                     #GENDER_OTHER,
                     Group,
                     #RACE_OTHER,
                     Ethnicity,
                     MARRIAGE,
                     EDUCATION,
                     #EDU_OTHER,
                     HOUSEHOLD_INCOME,
                     #RELIGION,
                     RELIGION_YN
                     #RELI_OTHER,
                     #FRE_RELI_EVENT,
                     #T1_severity,
                     #T1_DTFS_EVENT
                     ) )


# EXCLUDE MINORS -----------------------------------------------------------

d = d %>% filter(age >= 18)

# save intermediate dataset
write_interm(d, "prepped_data_intermediate1.csv")


# RECODE SCALES -----------------------------------------------------------

# read in intermediate dataset
d = read_interm("prepped_data_intermediate1.csv")

# keep a running list of all subscale variable names for easy removal later
subscales_all = c()

# ~ Make rowwise mean and sum variables for each scale -------------------------------------------------
for ( .v in c(primYNames, secYNames, unusedYnames) ) {
  
  # make new variable for a given time point
  varName = paste("T1", .v, sep = "_")
  temp = recode_psych_scale(.d = d,
                         scale = varName, 
                         revCode = NA,
                         printCorMat = TRUE,
                         dropSubscaleVars = FALSE)
  d = temp$data
  subscales_all = c(subscales_all, temp$subscales)
  
  # make new variable for a given time point
  varName = paste("T2", .v, sep = "_")
  temp = recode_psych_scale(.d = d,
                         scale = varName, 
                         revCode = NA,
                         printCorMat = TRUE,
                         dropSubscaleVars = FALSE)
  d = temp$data
  subscales_all = c(subscales_all, temp$subscales)
  
  # make new variable for a given time point
  varName = paste("T3", .v, sep = "_")
  temp = recode_psych_scale(.d = d,
                         scale = varName, 
                         revCode = NA,
                         printCorMat = TRUE,
                         dropSubscaleVars = FALSE)
  d = temp$data
  subscales_all = c(subscales_all, temp$subscales)
  
  # check it
  message( paste("Final variables for", .v, ":",
               paste( stringsWith( pattern=.v, names(d) ), collapse = ", " ) ) )
  
}

# sanity check
if ( run.sanity == TRUE ) {
  dput( stringsWith( pattern = "TRIM", x = names(d) ) ) # find subscales manually
  vars = c("T1_TRIM1", "T1_TRIM2", "T1_TRIM3", "T1_TRIM4", "T1_TRIM5", 
           "T1_TRIM6", "T1_TRIM7", "T1_TRIM8", "T1_TRIM9", "T1_TRIM10", 
           "T1_TRIM11", "T1_TRIM12", "T1_TRIM13", "T1_TRIM14", "T1_TRIM15", 
           "T1_TRIM16", "T1_TRIM17", "T1_TRIM18")
  temp = d %>% select( all_of(vars) ) 
  expect_equal( rowMeans(temp), d$T1_TRIM_raw_mean )
  expect_equal( rowSums(temp), d$T1_TRIM_raw_sum )
}

  

# ~ Standardize each scale  -------------------------------------------------
for ( .v in c(primYNames, secYNames, unusedYnames) ) {
  
  # all variables (3 time points) for this outcome
  ( raw_mean_names = stringsWith( pattern = paste(.v, "raw_mean", sep = "_"),
                                 x = names(d) ) )


  #( subscales = subscales_all[ grepl( x = subscales_all, pattern = .v ) ] )
  
  
  # d %>% select( all_of(raw_mean_names) )

  # all entries for all 3 variables
  all_entries = as.numeric( unlist( d %>% select( all_of(raw_mean_names) ) ) )
  # sanity check: 3 time points
  expect_equal( nrow(d) * 3, length(all_entries))
  
  # overall (across time points) mean and SD
  ( .mean = meanNA( all_entries ) )
  ( .sd = sd( all_entries, na.rm = TRUE ) )
  
  # make new standardized variable whose name is just the root
  # new variable is mean by subject of the subscales
  for ( t in 1:3 ){
    new.name = str_replace(string = raw_mean_names[t], pattern = "_raw_mean", "")
    d[[ new.name ]] = ( d[[ raw_mean_names[t] ]] - c(.mean) ) / .sd
  }
}


# sanity check
if ( run.sanity == TRUE ) {
  dput( stringsWith( pattern = "BSIdep", x = names(d) ) ) # find subscales manually
  
  all_vars = c("T1_BSIdep_raw_mean", "T2_BSIdep_raw_mean", 
               "T3_BSIdep_raw_mean")

  all_entries = as.numeric( unlist( d %>% select( all_of(all_vars) ) ) )
  
  my_std = ( d$T1_BSIdep_raw_mean - meanNA(all_entries) ) / sd(all_entries, na.rm = TRUE)
  
  expect_equal( my_std, d$T1_BSIdep )
}


# save a version of dataset that still has the subscales
write_interm(d, "prepped_data_intermediate1.5_with_subscales.csv")


# sanity checks
if ( run.sanity == TRUE ) {
  meanNA(d$T1_TRIM_raw_mean[d$treat == 1])
  meanNA(d$T2_TRIM_raw_mean[d$treat == 1])
  meanNA(d$T3_TRIM_raw_mean[d$treat == 1])
  
  sd(d$T1_TRIM_raw_mean[d$treat == 1], na.rm = TRUE)
  sd(d$T2_TRIM_raw_mean[d$treat == 1], na.rm = TRUE)
  sd(d$T3_TRIM_raw_mean[d$treat == 1], na.rm = TRUE)
  
  yNames = c("T1_BSIdep", "T1_BSIanx", "T1_TRIM",
             "T2_BSIdep", "T2_BSIanx", "T2_TRIM",
             "T3_BSIdep", "T3_BSIanx", "T3_TRIM")
  CreateTableOne(vars = c("treat", vars),
                 strata = "treat",
                 data = d)
}


# keep only analysis variables
d = d %>% select( all_of( c( "site", "site_t3", "uid", "treat",
                             primYNamesWide, secYNamesWide, unusedYnamesWide,
                          demoVarsToAnalyze, demoVarsAux ) ) )



# quick sneak peak at results!!!!
summary( lm(T2_TRIM ~ treat, data = d) )
summary( lm(T2_DTFS ~ treat, data = d) )



# save intermediate dataset
write_interm(d, "prepped_data_intermediate2.csv")




# IMPUTATION IN WIDE FORMAT -----------------------------------------------------------

# read in intermediate dataset
d = read_interm("prepped_data_intermediate2.csv")

# recode characters as factors to prevent issues with mice()
sum(sapply(d, is.character))  # check number of character vars
d = d %>% mutate_if(sapply(d, is.character), as.factor)

# check again; should be 0
expect_equal( 0, sum(sapply(d, is.character)) )

# ~ Look at Missingness --------------------------------------


if ( run.sanity == TRUE ) {
  t = d %>% summarise( across( everything(), ~ round( 100*mean( !is.na(.x) ) ) ) )
  t = as.data.frame( t(t) ) # transpose it
  names(t) = "perc.nonmissing"
  
  t %>% arrange(perc.nonmissing)
  #**save and give to Man Yee
  
  setwd(results.dir)
  setwd("Auxiliary")
  fwrite(t, "perc_nonmissing_by_var.csv")
  
  # a bit slow
  #missmap(d)
}




# ~ Make imputations --------------------------------------

if ( impute.from.scratch == TRUE ) {
  
  ini = mice(d, m=1, maxit = 0 )
  
  # check default methods
  # all PMM, as desired
  ini$method
  
  # set variables to be imputed, and those to use as predictors
  # ***Key to avoiding having missing data in the imputations (despite no loggedEvents):
  #  You cannot use aux variables which *themselves* have missingness unless you *also* impute those variables
  varsToImpute = c( demoVarsToAnalyze,
                    #demoVarsAux, #can't include these!
                    primYNamesWide,
                    secYNamesWide )
  
  # save in case you want to adjust
  # ( impModelPredictors = c( demoVarsToAnalyze,
  #                           primYNamesWide,
  #                           secYNamesWide ) )
  
  impModelPredictors = varsToImpute
  
  # make own predictor matrix by modifying mice's own predictor matrix to keep structure the same
  #  from mice docs: "Each row corresponds to a variable block, i.e., a set of variables to be imputed. A value of 1 means that the column variable is used as a predictor for the target block (in the rows)"
  myPred = ini$pred
  
  myPred[myPred == 1] = 0
  # impute all F/U variables using the sensible ones from baseline as well as all the other F/U vars
  myPred[ names(d) %in% varsToImpute, # vars to be imputed
          names(d) %in% impModelPredictors ] = 1  # ...and vars in the imputation model
  diag(myPred) = 0  # but a variable can't impute itself
  sum(myPred)
  
  
  myMethod = ini$method
  myMethod[ names(myMethod) %in% varsToImpute ] = "pmm"
  myMethod[ !( names(myMethod) %in% varsToImpute ) ] = ""
  myMethod

  imps = mice( d,
               m=M,  
               predictorMatrix = myPred,
               method = myMethod,
               seed = 451)
  
  imps$loggedEvents 
  imps$loggedEvents$dep

  # make sure there is no missing data in the imputations
  any.missing = apply( complete(imps,1),
                       2,
                       function(x) any(is.na(x)) ) # should be FALSE
  
  fake = complete(imps,1)
  table(is.na(fake$T2_TRIM))  # imputed data
  table(is.na(d$T2_TRIM))  # original data
  
  table(is.na(fake$T2_DTFS))
  table(is.na(d$T2_DTFS))
  table(is.na(fake$T2_FOR))

  # **important: any.missing WILL have missing values on auxiliary vars used to make the imputation model
  # because we're not imputing those vars
  # what's important is that there should be no missing data in the varsToImpute
  if ( any( any.missing[varsToImpute] ) == TRUE ) warning("*****Imputed datasets have missing data! Look at logged events.")
  

  ##### Save Imputations for Reproducibility #####
  if ( overwrite.res == TRUE ) {
    
    # save imputations for reproducibility
    setwd(imputed.data.dir)
    save( imps, file = "imputed_datasets.RData" )
    
    for (i in 1:M) {
      impDat = complete(imps, i)
      impDat = wrangle_post_imputation(impDat)
      setwd(imputed.data.dir)
      write_csv( impDat,
                 paste("imputed_dataset_prepped", i, ".csv", sep="") )
      
      # make long datasets
      impl = make_long_dataset(.dat = impDat)
      write_csv( impl,
                 paste("imputed_dataset_long_prepped", i, ".csv", sep="") )
      
    }  # end "for (i in 1:M)"
  }  # end "if ( overwrite.res == TRUE )"
  
  
}  # end "if (impute.from.scratch == TRUE)"



# read in existing imputations
if ( impute.from.scratch == FALSE ) {
  
  setwd(imputed.data.dir)
  to.read = list.files()[ grepl( pattern = "dataset_prepped", x = list.files() ) ]
  imps <<- lapply( to.read,
                   function(x) suppressMessages(read_csv(x)) )
  
}



# POST-IMPUTATION DATA WRANGLING -----------------------------------------------------------


# read in intermediate dataset
d = read_interm("prepped_data_intermediate2.csv")


d = wrangle_post_imputation(.dat = d)
l = make_long_dataset(.dat = d)

# check standardization
if (run.sanity == TRUE) {
  expect_equal( meanNA(l$TRIM), 0)
  expect_equal( sd(l$TRIM, na.rm = TRUE), 1)
  
  expect_equal( meanNA(l$BSIdep), 0)
  expect_equal( sd(l$BSIdep, na.rm = TRUE), 1)
  
  expect_equal( meanNA(l$BSIanx), 0)
  expect_equal( sd(l$BSIanx, na.rm = TRUE), 1)
}



# SAVE FINAL DATASET -----------------------------------------------------------

setwd(prepped.data.dir)
fwrite(d, "prepped_data.csv")
fwrite(l, "prepped_data_long.csv")






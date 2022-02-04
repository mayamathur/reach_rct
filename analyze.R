
# NOTES -----------------------------------------------------------

# Models that need to be fit:
# - GEE of all time points (need data in long format)
# - Maybe fill in #@ things?

# Game plan:
# - Fn similar to my_ols_hc0_all, but arg is the entire model formula
# - Then should be able to modify analyze_all_outcomes to handle MI, etc.

# - Remember you currently have a scrambled Tx indicator in prep.R

# For read-me:


# PRELIMINARIES -----------------------------------------------------------

library(here)
setwd(here())
# load packages, set working dirs, etc.
source("preliminaries.R")


# Set Parameters Here --------------------------------
# overwrite old results?
overwrite.res = FALSE

# should sanity checks be run?
run.sanity = FALSE

# Read in data  --------------------------------
setwd(prepped.data.dir)
d = read_csv("prepped_data.csv") 
expect_equal( nrow(d), 4571 )  # from 2022-2-1


# Read in imputations  --------------------------------
# read in the imputations as a list rather than a mids object so that we can pool manually
setwd(imputed.data.dir)
to.read = list.files()[ grepl( pattern = "prepped", x = list.files() ) ]
imps <<- lapply( to.read,
                 function(x) suppressMessages(read_csv(x)) )


names(imps[[1]])
meanNA(imps[[1]]$T2_TRIM)
meanNA(imps[[2]]$T2_TRIM)



# SANITY CHECKS -----------------------------------------------------------


if ( run.sanity == TRUE ) {
  
  # ~ Marginal Table 1 with sanity checks -----------------------------------
  t1 = make_table_one(.d = d,
                      .include.sanity.checks = TRUE )
  
  setwd(results.aux.dir)
  setwd("Marginal and site-specific Table 1's with sanity checks")
  write_csv(t1, "marginal_table1_cc_sanity.csv")

  # ~ Site-specific Table 1's with sanity checks -----------------------------------
  
  for ( .s in unique(d$site) ) {
    t1 = make_table_one(.d = d %>% filter(site == .s),
                        .include.sanity.checks = TRUE )
    
    setwd(results.aux.dir)
    setwd("Marginal and site-specific Table 1's with sanity checks")
    write_csv(t1, paste(.s, "_table1_cc_sanity.csv", sep ="") )
  }
  
  
}


# TABLE 1: BASELINE DEMOGRAPHICS -----------------------------------------------------------

#@move this to analyze.R

# stratify demographics by treatment group
t1.treat = make_table_one(.d = d %>% filter( treat == 1 ) )
t1.cntrl = make_table_one(.d = d %>% filter( treat == 0 ) )

# look for dimension mismatches caused by missing categories in one treatment group
dim(t1.treat); dim(t1.cntrl)
t1.treat$Characteristic[ !t1.treat$Characteristic %in% t1.cntrl$Characteristic ]

#@there's a lot of missing data on ethnicity

# DEBUG GEE -----------------------------------------------------------


#CreateTableOne( data = d %>% select( c(treat, demoVars, ) ) )

setwd(prepped.data.dir)
d = read_csv("prepped_data.csv") 
expect_equal( nrow(d), 4571 )  # from 2022-2-1



### Outcome BSI
# independent working structure: gee WORKS; Mancl FAILS
analyze_one_outcome( missMethod = "CC",
                     yName = "BSI",
                     formulaString = "T2_BSI ~ treat + site",
                     analysisVarNames = c("T2_BSI", "treat", "site"),
                     analysisLabel = "set1",
                     
                     corstr = "independence",
                     
                     .results.dir = NA )

# exch working structure: gee WARNS; Mancl WORKS
# "Working correlation estimate not positive definite"
analyze_one_outcome( missMethod = "CC",
                     yName = "BSI",
                     formulaString = "T2_BSI ~ treat + site",
                     analysisVarNames = c("T2_BSI", "treat", "site"),
                     analysisLabel = "set1",
                     
                     corstr = "exchangeable",
                     
                     .results.dir = NA )

# with corstr = "independence", the GEE fits but the Mancl part says "computationally singular"
# with corstr = "exchangeable", the GEE warns that the working correlation estimate isn't pos def



### Outcome TRIM
# independent working structure: gee WORKS; Mancl FAILS
analyze_one_outcome( missMethod = "CC",
                     yName = "TRIM",
                     formulaString = "T2_TRIM ~ treat + site",
                     analysisVarNames = c("T2_TRIM", "treat", "site"),
                     analysisLabel = "set1",
                     
                     corstr = "independence",
                     
                     .results.dir = NA )

# exch working structure: gee WARNS; Mancl FAILS
# "  Working correlation estimate not positive definite"
analyze_one_outcome( missMethod = "CC",
                     yName = "TRIM",
                     formulaString = "T2_TRIM ~ treat + site",
                     analysisVarNames = c("T2_TRIM", "treat", "site"),
                     analysisLabel = "set1",
                     
                     corstr = "exchangeable",
                     
                     .results.dir = NA )

# with corstr = "independence", the GEE fits but the Mancl part says "computationally singular"
# with corstr = "exchangeable", the GEE warns that the working correlation estimate isn't pos def


# SET 1 GEE MODELS -----------------------------------------------------------


# - GEE of primary and secondary Y's ~ treat + site (Bonferroni for secondaries)


#@need to add Bonferronis
for ( .y in primYNames ) {
  
  .fullYName = paste("T2_", .y, sep = "")
  .formulaString = paste(.fullYName, " ~ treat + site", sep = "" )

  
  for ( .missMethod in c("CC", "MI") ) {
    
    cat( paste("\n\n**********Starting outcome", .y, "; method", .missMethod) )
    
    if (.missMethod == "MI") missingString = "Multiple imputation"
    if (.missMethod == "CC") missingString = "Complete-case"
    
    .results.dir = paste( results.dir, "/Analysis set 1/", missingString, sep = "" )
    
    analyze_one_outcome( missMethod = .missMethod,
                         yName = .y,
                         formulaString = .formulaString,
                         analysisVarNames = c(.fullYName, "treat", "site"),
                         analysisLabel = "set1",
                         corstr = "exchangeable",
                         .results.dir = .results.dir )
    
    
  }
}




# SET 2 GEE MODELS -----------------------------------------------------------


# GEE of primary Y's ~ treat*T1_TFS(binary) + site

for ( .y in primYNames ) {
  
  .fullYName = paste("T2_", .y, sep = "")
  .formulaString = paste("T2_", .y, " ~ treat*T1_high_TrFS + site", sep = "" )
  
  
  for ( .missMethod in c("MI", "CC") ) {
    
    if (.missMethod == "MI") missingString = "Multiple imputation"
    if (.missMethod == "CC") missingString = "Complete-case"
    
    .results.dir = paste( results.dir, "/Analysis set 2/", missingString, sep = "" )
    
    analyze_one_outcome( missMethod = .missMethod,
                         yName = .y,
                         formulaString = .formulaString,
                         analysisVarNames = c(.fullYName, "treat", "site", "T1_high_TrFS"),
                         analysisLabel = "set2",
                         corstr = "exchangeable",
                         .results.dir = .results.dir )
    
    
  }
}




# SET 3 GEE MODELS -----------------------------------------------------------


# - GEE of primary and secondary Y's ~ treat + site + age + sex + all baseline primY


for ( .y in c(primYNames, secYNames) ) {
  
  .fullYName = paste("T2_", .y, sep = "")
  .formulaString = paste("T2_", .y, " ~ treat + site + age + gender + T1_BSI + T1_TRIM", sep = "" )
  
  
  for ( .missMethod in c("MI", "CC") ) {
    
    if (.missMethod == "MI") missingString = "Multiple imputation"
    if (.missMethod == "CC") missingString = "Complete-case"
    
    .results.dir = paste( results.dir, "/Analysis set 3/", missingString, sep = "" )
    
    analyze_one_outcome( missMethod = .missMethod,
                         yName = .y,
                         formulaString = .formulaString,
                         analysisVarNames = c(.fullYName, "treat", "site", "age", "gender", "T1_BSI", "T1_TRIM"),
                         analysisLabel = "set3",
                         corstr = "exchangeable",
                         .results.dir = .results.dir )
    
    
  }
}










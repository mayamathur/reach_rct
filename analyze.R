
# NOTES -----------------------------------------------------------

# Models that need to be fit:
# - GEE of primary and secondary Y's ~ treat + site (Bonferroni for secondaries)
# - GEE of primary Y's ~ treat*T1_TFS(binary) + site 
# - GEE of primary and secondary Y's ~ treat + site + age + sex + all baseline primY

# - GEE of all time points (need data in long format)

# Game plan:
# - Fn similar to my_ols_hc0_all, but arg is the entire model formula
# - Then should be able to modify analyze_all_outcomes to handle MI, etc.


# For read-me:
# - Explain why it's fine that imps have missing data on aux vars like income

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


# SET 1 GEE MODELS -----------------------------------------------------------

# - GEE of primary and secondary Y's ~ treat + site (Bonferroni for secondaries)


# test the fn

# example for main analyses (to be done for each prim and secY)
report_gee_table(dat = d,
                 formulaString = "T2_BSI ~ treat + site",
                 analysisLabel = "set1_T2_BSI",
                 write.dir = results.aux.dir)


# SET 2 GEE MODELS -----------------------------------------------------------


# GEE of primary Y's ~ treat*T1_TFS(binary) + site
#@need to dichotomize T1_TFS and adjust model string below

# example 
report_gee_table(dat %>% filter( !is.na(T1_TrFS) ),
                 formulaString = "T2_BSI ~ treat*T1_TrFS + site",
                 analysisLabel = "set2_T2_BSI",
                 write.dir = results.aux.dir)

# SET 3 GEE MODELS -----------------------------------------------------------


# - GEE of primary and secondary Y's ~ treat + site + age + sex + all baseline primY

# example 
report_gee_table(dat %>% filter( !is.na(age) &
                                   !is.na(gender) & 
                                   !is.na(T1_BSI) & 
                                   !is.na(T1_TRIM) ),
                 formulaString = "T2_BSI ~ treat + site + age + gender + T1_BSI + T1_TRIM",
                 analysisLabel = "set3_T2_BSI",
                 write.dir = results.aux.dir)











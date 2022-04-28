
# PRELIMINARIES -----------------------------------------------------------

library(here)
setwd(here())
# load packages, set working dirs, etc.
source("preliminaries.R")


# Set Parameters Here --------------------------------
# overwrite old results?
overwrite.res = TRUE

# should sanity checks be run?
run.sanity = TRUE

# use scrambled treatment variable for blinding?
scramble.treat = FALSE


# Read in data  --------------------------------
setwd(prepped.data.dir)
d = read_csv("prepped_data.csv") 
expect_equal( nrow(d), 4438 )  # expected n from 2022-3-31

# long dataset
l = read_csv("prepped_data_long.csv") 
expect_equal( nrow(l), 4438 * 3 )  # because 3 times/subject


# Read in imputations  --------------------------------
# read in the imputations as a list rather than a mids object so that we can pool manually
setwd(imputed.data.dir)
to.read = list.files()[ grepl( pattern = "dataset_prepped", x = list.files() ) ]
imps <<- lapply( to.read,
                 function(x) suppressMessages(read_csv(x)) )


names(imps[[1]])
expect_equal( 0, mean( is.na(imps[[1]]$T2_TRIM) ) )  # sanity check

# read in long-format imputations for sensitivity analysis
setwd(imputed.data.dir)
to.read = list.files()[ grepl( pattern = "dataset_long_prepped", x = list.files() ) ]
impsl <<- lapply( to.read,
                  function(x) suppressMessages(read_csv(x)) )

names(impsl[[1]])


# Scramble treatment variable for blinding, if needed  --------------------------------

if ( scramble.treat == TRUE ) {
  
  d$treat = sample(d$treat, replace = TRUE)
  
  for (i in 1:M) {
    imps[[i]]$treat = sample(imps[[i]]$treat, replace = TRUE)
    
    impsl[[i]]$treat = sample(impsl[[i]]$treat, replace = TRUE)
    
  }
  
}


# Filtered datasets for figs and analyses wrt T3 ----------------------------------

# prepare to exclude the single Columbia site that didn't collect any data at T3
# as well as Ukraine-Realis for same reason


t3.keeper.ids = d$uid[ d$site_t3 == "yes" ]


l.t3.filtered = l[ l$uid %in% t3.keeper.ids, ]

impsl.t3.filtered = lapply( X = impsl,
                            FUN = function(.dat) .dat[ .dat$uid %in% t3.keeper.ids, ] )

# sanity check: dim of filtered imputed data should match filtered CC data
expect_equal( nrow( impsl.t3.filtered[[1]] ),
              nrow( l[ l$uid %in% t3.keeper.ids, ]) )

table(l.t3.filtered$site)



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
    ### Marginally wrt treatment status 
    t1.all = make_table_one(.d = d %>% filter(site == .s),
                        .include.sanity.checks = TRUE )
    # for later merging
    t1.all = t1.all %>% add_column(row = 1:nrow(t1.all), .before = 1 )

    
    ### Stratified by treatment status 
    d.temp = d %>% filter( site == .s )
    t1.treat = make_table_one(.d = d.temp %>% filter( treat == 1 ) )
    t1.cntrl = make_table_one(.d = d.temp %>% filter( treat == 0 ) )
    
    t1.treat = t1.treat %>% add_column(row = 1:nrow(t1.treat), .before = 1 )
    t1.cntrl = t1.cntrl %>% add_column(row = 1:nrow(t1.cntrl), .before = 1 )
    
    t1.strat = merge(t1.cntrl, t1.treat, all = TRUE, by = "row")
   
    
    ### Merge and organize the full table (marginal; DT group; IT group)
    t1 = merge(t1.all,
                 t1.strat,
                 all = TRUE, by = "row") %>%
      select( -c("row") )
    
    
    
    names(t1)[1] = "Characteristic (overall)"
    names(t1)[2] = paste( "Both groups", "; n=",
                          nrow(d.temp),
                          sep="" )
    
    names(t1)[3] = "Characteristic (DT)"
    names(t1)[4] = paste( "DT group (control)", "; n=",
                          sum(d.temp$treat == 0),
                          sep="" )
    names(t1)[5] = "Characteristic (IT)"
    names(t1)[6] =paste( "IT group", "; n=",
                         sum(d.temp$treat == 1),
                         sep="" )
    

    setwd(results.aux.dir)
    setwd("Marginal and site-specific Table 1's with sanity checks")
    write_csv(t1, paste(.s, "_table1_cc_sanity.csv", sep ="") )
  }
  
  
}



# TABLE 1: BASELINE DEMOGRAPHICS -----------------------------------------------------------

# ~ Across all sites ----------------------------------------

# stratify demographics by treatment group
t1.treat = make_table_one(.d = d %>% filter( treat == 1 ) )
t1.cntrl = make_table_one(.d = d %>% filter( treat == 0 ) )

# look for dimension mismatches caused by missing categories in one treatment group
dim(t1.treat); dim(t1.cntrl)
# t1.treat$Characteristic[ !t1.treat$Characteristic %in% t1.cntrl$Characteristic ]


# manually fix issues in which one table lacks a level
#  that's present in the other table
message("\n\nBe careful with this part of code; sensitive to changes in data prep and table layout\n\n")
#View( cbind(t1.treat$Characteristic, t1.cntrl$Characteristic) )

t1.cntrl = t1.cntrl %>% add_row( .after = 7,
                                 Characteristic = "Not reported",
                                 Summary = "0 (0%)" )

t1.treat = t1.treat %>% add_row( .after = 14,
                                 Characteristic = "Other",
                                 Summary = "0 (0%)" )

# combine into single table
t1 = t1.cntrl
names(t1)[2] = paste( "DT group (control)", "; n=", sum(d$treat == 0), sep="" )

newColName = paste( "IT group", "; n=", sum(d$treat == 1), sep="" )
t1[[newColName]] = t1.treat$Summary


if ( overwrite.res == TRUE ) {
  setwd(results.dir)
  setwd("Tables")
  write.xlsx(t1, "*table_1_manuscript.xlsx", row.names = FALSE)
}




# SET 1: GEE MODELS (PRIMARY AND SECONDARY OUTCOMES) -----------------------------------------------------------


# - GEE of primary and secondary Y's ~ treat + site (Bonferroni for secondaries)

# **For these analyses, Bonferroni significance is only shown for secondaries,
#  and in paper, it should only be reported for coefficient "treat" (not sites)

#missMethodsToRun = "CC"
missMethodsToRun = c("CC", "MI")

for ( .y in c(primYNames, secYNames) ) {
  
  if ( .y %in% primYNames ) bonferroni.alpha = NA
  if ( .y %in% secYNames ) bonferroni.alpha = 0.005
  
  
  .fullYName = paste("T2_", .y, sep = "")
  .formulaString = paste(.fullYName, " ~ treat + site", sep = "" )
  
  
  
  for ( .missMethod in missMethodsToRun ) {
    
    cat( paste("\n\n**********Starting outcome", .y, "; method", .missMethod) )
    
    if (.missMethod == "MI") missingString = "Multiple imputation"
    if (.missMethod == "CC") missingString = "Complete-case"
    
    .results.dir = paste( results.dir, "/Analysis set 1/", missingString, sep = "" )
    
    analyze_one_outcome( missMethod = .missMethod,
                         yName = .y,
                         formulaString = .formulaString,
                         idString = "as.factor(uid)",
                         analysisVarNames = c(.fullYName, "treat", "site"),
                         analysisLabel = paste("set1_outcome_", .y, sep = " " ),
                         bonferroni.alpha = bonferroni.alpha,
                         corstr = "exchangeable",
                         .results.dir = .results.dir )
    
    
  }
}




# ~ Sanity checks ------------------------------

if ( run.sanity == TRUE ) {

  
  ### Sanity check: Reproduce one outcome model manually
  
  # get previous results for comparison
  setwd( paste( results.dir, "/Analysis set 1/Complete-case", sep = "" ) )
  res1 = fread("set1_outcome_ TRIM_completeCase__gee_table_raw_.csv")
  
  # refit the model manually
  mod = gee( T2_TRIM ~ treat + site,
             id = as.factor(uid),  
             corstr = "exchangeable",
             data = d )
  
  summ = summary(mod)
  est = coef(mod)
  se = summ$coefficients[,"Robust S.E."]
  lo = coef(mod) - qnorm(.975) * se
  hi = coef(mod) + qnorm(.975) * se
  Z = as.numeric( summ$coefficients[,"Robust z"] )
  pval = 2 * ( c(1) - pnorm(abs(Z)) )
  
  expect_equal( res1$est, as.numeric(est), tol = 0.001 )
  expect_equal( res1$se, as.numeric(se), tol = 0.001 )
  expect_equal( res1$lo, as.numeric(lo), tol = 0.001 )
  expect_equal( res1$hi, as.numeric(hi), tol = 0.001 )
  expect_equal( res1$pval, as.numeric(pval), tol = 0.001 )
  
  
  # other conceptually similar models
  # these are here primarily to look at the effect of having both fixed effects
  #  and clustering by site on SEs
  ### Model 1: OLS
  ols = lm( T2_TRIM ~ treat + site, data = d )
  summary(ols)  # model-based SEs (might be wrong)
  # example: SE for South Africa  = 0.04 (similar for other sites)
  # for treat: 0.03
  
  ### Model 2: OLS-HC0
  # now with HC0 SEs
  res = my_ols_hc_all( dat = d, ols = ols, yName = "treat", hc.type = "HC0" )
  # **SE for South Africa nearly the same
  # for treat: 0.04
  
  
  ### Model 2b: OLS-HC1
  # this is better in finite samples
  # https://economics.mit.edu/files/7422
  
  res = my_ols_hc_all( dat = d, ols = ols, yName = "treat", hc.type = "HC1" )
  # SE for South Africa nearly the same
  # for treat: 0.03
  
  ### Model 3a: LMM with fixed AND random effects of site
  # also try LMM
  library(lme4)
  
  lmm = lmer( T2_TRIM ~ treat + site + (1|site),
              data = d )
  
  summary(lmm)
  # SE for South Africa FIXED effect: 0.27!!! (much bigger than either OLS or GEE)
  # SE for treat: 0.03
  # issue with LMM: non-normal outcomes
  
  # LMM without FEs of site; inference from empirical Bayes
  re = ranef(lmm, condVar = TRUE)
  ses = sqrt( unlist( attr(re[[1]], "postVar") ) )
  ses = as.numeric(ses)
  ses[4]  # South Africa
  
  # **Empirical Bayes SE for South Africa: 0.03
  
  
  ### Model 3b: LMM with random effects of site (but not fixed effects)
  # also try LMM
  library(lme4)
  
  lmm = lmer( T2_TRIM ~ treat + (1|site),
              data = d )
  
  summary(lmm)
  # **SE for treat: 0.03
  # potential problem for LMM: non-normal outcomes
  
  # get inference for site effects using empirical Bayes
  # LMM without FEs of site; inference from empirical Bayes
  re = ranef(lmm, condVar = TRUE)
  ses = sqrt( unlist( attr(re[[1]], "postVar") ) )
  ses = as.numeric(ses)
  ses[4]  # South Africa
  
  #**Empirical Bayes SE for South Africa: 0.03
  
  ### Model 4: GEE (prespecified) ---------
  
  mod4 = gee( T2_TRIM ~ treat + site,
              id = as.factor(site),  
              corstr = "independence",
              data = d %>% filter( !is.na(T2_TRIM) ) )
  
  summ4 = summary(mod4)
  summ4$coefficients
  
  # Naive SE (South Africa, treat): (0.04, 0.03)
  # Robust SE: (0.0008, 0.07)
  
  
  
  ### Model 4b: GEE with clustering by uid instead of site
  
  mod4b = gee( T2_TRIM ~ treat + site,
               id = as.factor(uid),  
               corstr = "independence",
               data = d %>% filter( !is.na(T2_TRIM) ) )
  
  summ4b = summary(mod4b)
  summ4b$coefficients
  
  # Naive SE (South Africa, treat): (0.04, 0.03)
  # Robust SE: (0.04, 0.03)
  # Now they agree almost exactly
  
  ### Model 4c: GEE with fake site variable
  d$fake.cluster = sample( 1:6, replace = TRUE, size = nrow(d) )
  
  mod4c = gee( T2_TRIM ~ treat + site,
               id = as.factor(fake.cluster),  
               corstr = "independence",
               data = d %>% filter( !is.na(T2_TRIM) ) )
  
  summ4c = summary(mod4c)
  summ4c$coefficients
  
  # **Now naive and robust still match!
  # This experiment suggests that it's a problem to have site as fixed effect
  #  AND as clustering variable (not having too few clusters).
}





# ~ Single table with all outcomes ---------------------------

table_all_outcomes(.results.dir = paste( results.dir,
                                         "Analysis set 1/Multiple imputation",
                                         sep = "/" ),
                   .filename = "*table_set1_manuscript.xlsx",
                   .var.name = "treat")



# SET 1B: EACH FLOURISHING DOMAIN -------------------------------


# caveat: this is a very hacky, quick & dirty analysis 


# bring back dataset from before subscales were removed
# read in intermediate dataset
# this was written by prep.R
d.interm = read_interm("prepped_data_intermediate1.csv")
expect_equal( nrow(d.interm), nrow(d) )

# temporary copy of dataset to which we'll add the flourishing subscales
d2 = d

keepers = names(d.interm)[ grepl("T2_FS", names(d.interm)) == TRUE ]

d2 = bind_cols( d2, d.interm %>% select( all_of(keepers) ) )


# can only do CC here
missMethodsToRun = "CC"


for ( .y in keepers ) {
  
  bonferroni.alpha = NA
  
  
  .fullYName = .y
  .formulaString = paste(.fullYName, " ~ treat + site", sep = "" )
  
  
  
  for ( .missMethod in missMethodsToRun ) {
    
    cat( paste("\n\n**********Starting outcome", .y, "; method", .missMethod) )
    
    if (.missMethod == "MI") missingString = "Multiple imputation"
    if (.missMethod == "CC") missingString = "Complete-case"
    
    .results.dir = paste( results.dir, "/Analysis set 1B/", missingString, sep = "" )
    
    analyze_one_outcome( dat.cc = d2,
                         missMethod = .missMethod,
                         yName = .y,
                         formulaString = .formulaString,
                         idString = "as.factor(uid)",
                         analysisVarNames = c(.fullYName, "treat", "site"),
                         analysisLabel = paste("set1B_outcome_", .y, sep = " " ),
                         bonferroni.alpha = bonferroni.alpha,
                         corstr = "exchangeable",
                         .results.dir = .results.dir )
    
    
  }
}


table_all_outcomes(.results.dir = paste( results.dir,
                                         "Analysis set 1B/Complete-case",
                                         sep = "/" ),
                   .filename = "table_set1B_posthoc.xlsx",
                   .var.name = "treat")




# SET 2: GEE MODELS (TREAT * TRAIT FORGIVENESS) -----------------------------------------------------------


# GEE of primary Y's ~ treat*T1_TrFS(binary) + site

#@NOTE: Per preregistration, the coef for T1_high_TrFS is counted in Bonferroni,
#  but not site, so should NOT report the site p-values in this model

for ( .y in primYNames ) {
  
  .fullYName = paste("T2_", .y, sep = "")
  .formulaString = paste("T2_", .y, " ~ treat*T1_high_TrFS + site", sep = "" )
  
  for ( .missMethod in missMethodsToRun ) {
    
    if (.missMethod == "MI") missingString = "Multiple imputation"
    if (.missMethod == "CC") missingString = "Complete-case"
    
    .results.dir = paste( results.dir, "/Analysis set 2/", missingString, sep = "" )
    
    analyze_one_outcome( missMethod = .missMethod,
                         yName = .y,
                         formulaString = .formulaString,
                         idString = "as.factor(uid)",
                         
                         analysisVarNames = c(.fullYName, "treat", "site", "T1_high_TrFS"),
                         analysisLabel = paste("set2_outcome_", .y, sep = " " ),
                         corstr = "exchangeable",
                         bonferroni.alpha = 0.005,
                         .results.dir = .results.dir )
    
    
  }
}


### Sanity check: Reproduce one outcome model manually
if ( run.sanity == TRUE ) {
  # get previous results for comparison
  setwd( paste( results.dir, "/Analysis set 2/Complete-case", sep = "" ) )
  res1 = fread("set2_outcome_ TRIM_completeCase__gee_table_raw_.csv")
  
  
  # refit the model manually
  mod = gee( T2_TRIM ~ treat*T1_high_TrFS + site,
             id = as.factor(uid),  
             corstr = "exchangeable",
             data = d )
  
  summ = summary(mod)
  est = coef(mod)
  se = summ$coefficients[,"Robust S.E."]
  lo = coef(mod) - qnorm(.975) * se
  hi = coef(mod) + qnorm(.975) * se
  Z = as.numeric( summ$coefficients[,"Robust z"] )
  pval = 2 * ( c(1) - pnorm(abs(Z)) )
  
  expect_equal( res1$est, as.numeric(est), tol = 0.001 )
  expect_equal( res1$se, as.numeric(se), tol = 0.001 )
  expect_equal( res1$lo, as.numeric(lo), tol = 0.001 )
  expect_equal( res1$hi, as.numeric(hi), tol = 0.001 )
  expect_equal( res1$pval, as.numeric(pval), tol = 0.001 )
}



# ~ Single table with all outcomes ---------------------------

table_all_outcomes(.results.dir = paste( results.dir,
                                         "Analysis set 2/Multiple imputation",
                                         sep = "/" ),
                   .filename = "*table_set2_manuscript.xlsx",
                   .var.name = "treat:T1_high_TrFSTRUE")



# SET 3: GEE MODELS (ANALYZE WITHIN SITE) -----------------------------------------------------------

# Per prereg, we're NOT reporting p-values for individual sites
# Instead we're doing a single global test

# As sanity check, can visually compare these to saved violin plots

# ~ GEE models within sites --------------------------------------
for ( .missMethod in missMethodsToRun ) {
  
  if (.missMethod == "MI") missingString = "Multiple imputation"
  if (.missMethod == "CC") missingString = "Complete-case"
  
  .results.dir = paste( results.dir, "/Analysis set 3/", missingString, sep = "" )
  
  
  for ( .y in primYNames ) {
    
    .fullYName = paste("T2_", .y, sep = "")
    .formulaString = paste("T2_", .y, " ~ treat", sep = "" )
    
    
    for (.site in unique(d$site) ) {
      
      # * if you want to save site-by-site results automatically as well,
      #  just change .results.dir arg below
      site.res = analyze_one_outcome( 
        missMethod = .missMethod,
        yName = .y,
        formulaString = .formulaString,
        idString = "as.factor(uid)",
        subsetString = paste( "site == '", .site, "'", sep="" ),
        
        analysisVarNames = c(.fullYName, "treat"),
        analysisLabel = paste("set3_outcome_", .y, sep = " " ),
        corstr = "exchangeable",
        .results.dir = NA )
      
      # keep only the important part and omit p-value per prereg
      site.row = site.res$res.nice %>% filter(var.name == "treat") %>% select(analysis, var.name, est)
      
      site.row = site.row %>% add_column(.before = "est",
                                         stratified.model = .site )
      
      if ( .site == unique(d$site)[1] ) res = site.row else res = rbind(res, site.row)
      
    } # end loop over sites
    
    
    
    setwd(.results.dir)
    
    string = paste( "set3", .y, missingString, "gee_table_pretty", ".csv", sep="_" )
    write_csv(res, string)
    
    
  }  # end loop over .y
  
}  # end loop over missMethod


### Sanity check: Reproduce one outcome model manually
if ( run.sanity == TRUE ) {
  # get previous results for comparison
  setwd( paste( results.dir, "/Analysis set 3/Complete-case", sep = "" ) )
  res1 = fread("set3_TRIM_Complete-case_gee_table_raw_.csv")
  
  
  # refit the model manually
  .site = "South Africa"
  res1 = res1 %>% filter(site == .site)
  
  mod = gee( T2_TRIM ~ treat,
             id = as.factor(uid),  
             corstr = "exchangeable",
             data = d[ d$site == .site, ] )
  
  summ = summary(mod)
  est = coef(mod)
  se = summ$coefficients[,"Robust S.E."]
  lo = coef(mod) - qnorm(.975) * se
  hi = coef(mod) + qnorm(.975) * se
  Z = as.numeric( summ$coefficients[,"Robust z"] )
  pval = 2 * ( c(1) - pnorm(abs(Z)) )
  
  mine = paste( round(est[2], 2),
         " [",
         round(lo[2], 2),
         ", ",
         round(hi[2], 2),
         "]",
         sep = "" )
  
  expect_equal( res1$est[2], mine )
}

# SET 6: Global test for site heterogeneity --------------------------------------


for ( .y in primYNames ) {
  
  .fullYName = paste("T2_", .y, sep = "")
  .formulaString = paste(.fullYName, " ~ treat*site", sep = "" )
  
  .missMethod = "MI"
  
  .results.dir = paste( results.dir, "/Analysis set 6/", missingString, sep = "" )
  
  mod.int = analyze_one_outcome( missMethod = .missMethod,
                                 yName = .y,
                                 formulaString = .formulaString,
                                 idString = "as.factor(uid)",
                                 analysisVarNames = c(.fullYName, "treat", "site"),
                                 analysisLabel = paste("set6_outcome_", .y, sep = " " ),
                                 corstr = "exchangeable",
                                 .results.dir = .results.dir )
  
  
  # extract interaction term p-values
  coefNames = row.names(mod.int$res.raw)
  keepers = stringsWith( pattern = "treat:", x = coefNames )
  
  pvals = mod.int$res.raw$pval[ coefNames %in% keepers ]
  
  ( pval.site.hetero = p.hmp(pvals, L = length(pvals) ) )
  # this is supposed to be compared to the Bonferroni threshold
  
  update_result_csv(name = paste( "Site heterogeneity global pval", .y, sep = " " ),
                    value = round(pval.site.hetero, 3) )
  
  update_result_csv(name = paste( "Site heterogeneity global pval passed Bonferroni", .y, sep = " " ),
                    value = pval.site.hetero < 0.005 )

}





### Sanity check: Reproduce HMP straight from tables

# given how similar the sites' estimates are for BSIdep and BSIanx,
#  is it correct that their HMPs are so different (0.008 vs. 0.26)? 

if ( run.sanity == TRUE ) {
  # for BSIanx
pvals = c(0.270383, 0.790297, 0.055420, 0.477202, 0.153326)
p.hmp(pvals, L = length(pvals) )

# for BSIdep
pvals = c(0.2836111, 0.7283142, 0.0016135, 0.6310697, 0.3590222)
p.hmp(pvals, L = length(pvals) )

# seems like this occurs because of the very small p-value for third site in BSIdep 
# i.e., driven largely by ineffectiveness in South Africa
#  corroborated by plot_effect_maintenance_by_site_all_outcomes.pdf
}


# SET 4: GEE MODELS (PRECISION COVARIATES) -----------------------------------------------------------

# GEE of primary and secondary Y's ~ treat + site + age + sex + all baseline primY

for ( .y in c(primYNames, secYNames) ) {
  
  .fullYName = paste("T2_", .y, sep = "")
  .formulaString = paste("T2_", .y, " ~ treat + site + age + gender + T1_BSIdep + T1_BSIanx + T1_TRIM", sep = "" )
  
  
  for ( .missMethod in missMethodsToRun ) {
    
    if (.missMethod == "MI") missingString = "Multiple imputation"
    if (.missMethod == "CC") missingString = "Complete-case"
    
    .results.dir = paste( results.dir, "/Analysis set 4/", missingString, sep = "" )
    
    analyze_one_outcome( missMethod = .missMethod,
                         yName = .y,
                         formulaString = .formulaString,
                         idString = "as.factor(uid)",
                         analysisVarNames = c(.fullYName, "treat", "site", "age", "gender", "T1_BSIdep", "T1_BSIanx", "T1_TRIM"),
                         analysisLabel = paste("set4_outcome_", .y, sep = " " ),
                         corstr = "exchangeable",
                         .results.dir = .results.dir )
    
    
  }
}


# @Gives warnings that SEs differed by more than 0.01
# That seems to be because gender has evil tiny category called "RECODE TROUBLE"; return to this after dataset is fixed



# ~ Single table with all outcomes ---------------------------

table_all_outcomes(.results.dir = paste( results.dir,
                                         "Analysis set 4/Multiple imputation",
                                         sep = "/" ),
                   .filename = "*table_set4_manuscript.xlsx",
                   .var.name = "treat")



# EFFECT MAINTENANCE OVER TIME (PLOT AND SIMPLE STATS) -----------------------------------------------------------

# one for each primary outcome
# points: means
# bars: +/- 1 SE (calculated by HC1, but very similar to naive; see code below)

plotList = list()


for ( i in 1:length(primYNames) ) {
  
  .y = primYNames[i]
  lp = l.t3.filtered
  lp$Y = lp[[.y]]
  
  agg = lp %>% group_by(treat.pretty, wave) %>%
    summarise( Mean = meanNA(Y),
               SE = marginal_hc_se(Y),
               # sanity check:
               SE.plain = sd(Y, na.rm = TRUE) / sqrt( length(Y[!is.na(Y)] ) ) )
  
  # for choosing axis limits
  # sort(agg$Mean)
  # sanity check: should be very similar
  #abs( agg$SE - agg$SE.plain )
  
  # simple stats for paper
  agg.treat = agg %>% filter( treat.pretty == "IT" )
  initial.change = agg.treat$Mean[ agg.treat$wave == "T2" ] - agg.treat$Mean[ agg.treat$wave == "T1" ]
  ultimate.change = agg.treat$Mean[ agg.treat$wave == "T3" ] - agg.treat$Mean[ agg.treat$wave == "T1" ]

  # note that if initial.change and ultimate.change disagree in sign, then the percentage will
  #  be negative
  update_result_csv( name = paste("Perc effect maintained at T3 for", .y, sep = " "),
                     value = round( 100 * ultimate.change/initial.change ) )
  
 
  # get pretty Y label
  if (.y == "TRIM") .ylab = "TRIM"
  if (.y == "BSIanx") .ylab = "Anxiety"
  if (.y == "BSIdep") .ylab = "Depression"
  
  # make the plot
  p <<- ggplot( data = agg,
                aes( x = wave, 
                     y = Mean,
                     color = treat.pretty ) ) +
    
    geom_hline( yintercept = 0,
                lty = 2,
                color = "gray") +
    
    geom_point(size = 1.2) + 
    geom_line( aes(group = treat.pretty) ) +
    geom_errorbar( aes(ymin = Mean - SE,
                       ymax = Mean + SE),
                   width = 0 ) +
    
    scale_y_continuous( limits = c(-0.35, 0.35),
                        breaks = round( seq(-0.35, 0.35, 0.1), 2) ) +
    scale_color_manual( values = c("black", "orange" ) ) +
    labs(color='Group')  +
    
    ylab(.ylab) +
    xlab("Time point") +
    
    theme_bw()
  
  plotList[[i]] = p
  

  setwd(results.dir)
  setwd("Figures")
  ggsave( paste("plot_effect_maintenance_outcome_", .y, ".pdf", sep="" ),
          width = 6,
          height = 4)
  
  setwd(overleaf.dir)
  ggsave( paste("plot_effect_maintenance_outcome_", .y, ".pdf", sep="" ),
          width = 6,
          height = 4)
  
} # end loop over.y


# plotList[[2]]
# plotList[[3]]

# optional: 
setwd(results.dir)
setwd("Figures")
pCombined = cowplot::plot_grid(plotlist = plotList,
                               nrow = 1)
ggsave( paste("plot_effect_maintenance_all_outcomes.pdf", sep="" ),
        width = 18,
        height = 4)



# sanity checks:
meanNA(d$T1_TRIM[ d$treat == 1] )
meanNA(d$T2_TRIM[ d$treat == 1] )
meanNA(d$T3_TRIM[ d$treat == 1] )

d %>% select(T1_TRIM, T2_TRIM, T3_TRIM, treat) %>%
  group_by(treat) %>%
  summarise_all( function(x) mean(x, na.rm = TRUE))



# ~ Sanity check: Same plot but facetted by site ----------------------------


plotList = list()


for ( i in 1:length(primYNames) ) {
  
  .y = primYNames[i]
  lp = l.t3.filtered
  lp$Y = lp[[.y]]
  
  agg2 = lp %>% group_by(site, treat.pretty, wave) %>%
    summarise( Mean = meanNA(Y),
               SE = marginal_hc_se(Y),
               # sanity check:
               SE.plain = sd(Y, na.rm = TRUE) / sqrt( length(Y[!is.na(Y)] ) ) )
  
  # get pretty Y label
  if (.y == "TRIM") .ylab = "TRIM"
  if (.y == "BSIanx") .ylab = "Anxiety"
  if (.y == "BSIdep") .ylab = "Depression"
  
  # make the plot
  p <<- ggplot( data = agg2,
                aes( x = wave, 
                     y = Mean,
                     color = treat.pretty ) ) +
    
    geom_hline( yintercept = 0,
                lty = 2,
                color = "gray") +
    
    geom_point(size = 1.2) + 
    geom_line( aes(group = treat.pretty) ) +
    geom_errorbar( aes(ymin = Mean - SE,
                       ymax = Mean + SE),
                   width = 0 ) +
    
    scale_y_continuous( limits = c(-0.7, 1.1),
                        breaks = round( seq(-0.7, 1.1, 0.2), 2) ) +
    scale_color_manual( values = c("black", "orange" ) ) +
    labs(color='Group')  +
    
    ylab(.ylab) +
    xlab("Time point") +
    
    theme_bw() +
    theme(legend.position = "none") +
    facet_wrap(~site)
  
  plotList[[i]] = p
  
} # end loop over.y


# plotList[[2]]
# plotList[[3]]

# optional: 
setwd(results.dir)
setwd("Figures")
pCombined = cowplot::plot_grid(plotlist = plotList,
                               nrow = 3)
ggsave( paste("plot_effect_maintenance_by_site_all_outcomes.pdf", sep="" ),
        width = 8,
        height = 12)


# SENSITIVITY ANALYSES -----------------------------------------------------------

# ~ 2 x 3 ANOVA --------------------------------

# https://www.r-bloggers.com/2015/08/two-way-anova-with-repeated-measures/

# https://stats.stackexchange.com/questions/15062/how-is-an-anova-calculated-for-a-repeated-measures-design-aov-vs-lm-in-r

# repeated measures structure won't fit
# "Error model is singular"
summary( aov( TRIM ~ treat * wave + Error(site / (treat * wave) ),
              data = l.t3.filtered ) )

# this does work
full = aov( TRIM ~ treat * wave, data = l.t3.filtered )
small = aov( TRIM ~ 1, data = l.t3.filtered )

# "global" p-value for ANOVA model
# this will capture secular trends as well since null model is intercept-only
res = anova(full, small)

update_result_csv( name = "ANOVA pval", 
                   value = format.pval( res$`Pr(>F)`[2], eps  = 0.0001 ) )




for ( .y in c(primYNames) ) {
  
  
  .formulaStringFull = paste(.y, " ~ treat * wave", sep = "" )
  .formulaStringSmall = paste(.y, " ~ 1", sep = "" )
  .missMethod = "CC"
  
  cat( paste("\n\n**********Starting outcome", .y ) )
  
  # this does work
  full = aov( eval( parse( text = .formulaStringFull ) ), data = l.t3.filtered )
  small = aov( eval( parse( text = .formulaStringSmall ) ), data = l.t3.filtered )
  
  # "global" p-value for ANOVA model
  # this will capture secular trends as well since null model is intercept-only
  res = anova(full, small)
  
  update_result_csv( name = paste( "ANOVA pval", .y ), 
                     value = format.pval( res$`Pr(>F)`[2], eps  = 0.0001 ) )
  
  
}






# ### DEBUGGING
# res.aov = aov( TRIM ~ treat * wave + Error(site / (treat * wave) ),
#                data = l )
# 
# small.aov = aov( TRIM ~ 1 + Error(site / (treat * wave) ),
#                  data = l )
# 
# anova(res.aov, small.aov)
# 
# 
# 
# 
# full = lm( TRIM ~ treat * wave + site, data = l )
# summary(full)
# 
# anova(full)
# 
# 
# summary( aov( TRIM ~ treat * wave + Error(site),
#                data = l ) )
# 
# 
# temp = lm( TRIM ~ treat * wave * site, data = l )
# anova(temp)






# ~ SET 5: GEE with all time points --------------------------------


# ~~ SET 5A: Main effect of treat.vary; FEs by site (prereg) ------------------

# As sanity check, can compare these to plot_effect_maintenance.pdf

missMethodsToRun = c("CC", "MI")

for ( .y in primYNames ) {
  
  .formulaString = paste(.y, " ~ treat.vary + site + wave", sep = "" )
  
  
  for ( .missMethod in missMethodsToRun ) {
    
    cat( paste("\n\n**********Starting outcome", .y, "; method", .missMethod) )
    
    if (.missMethod == "MI") missingString = "Multiple imputation"
    if (.missMethod == "CC") missingString = "Complete-case"
    
    .results.dir = paste( results.dir, "/Analysis set 5/Set 5A (prereg)/", missingString, sep = "" )
    
    analyze_one_outcome( dat.cc = l.t3.filtered,
                         dats.imp = impsl.t3.filtered,
                         
                         missMethod = .missMethod,
                         yName = .y,
                         formulaString = .formulaString,
                         idString = "as.factor(uid)",
                         
                         analysisVarNames = c(.y, "treat.vary", "site"),
                         analysisLabel = paste("set5A_geelong_outcome_", .y, sep = " " ),
                         corstr = "exchangeable",
                         .results.dir = .results.dir )
    
  }
}


# single table with all outcomes
table_all_outcomes(.results.dir = paste( results.dir,
                                         "Analysis set 5/Set 5A (prereg)/Multiple imputation",
                                         sep = "/" ),
                   .filename = "*table_set5A_manuscript.xlsx",
                   .var.name = "treat.vary")


### Sanity check: Reproduce one outcome model manually

if ( run.sanity == TRUE ) {
# get previous results for comparison
setwd( paste( results.dir, "/Analysis set 5/Set 5A (prereg)/Complete-case", sep = "" ) )
res1 = fread("set5A_geelong_outcome_ TRIM_completeCase__gee_table_raw_.csv")


# refit the model manually
mod = gee( TRIM ~ treat.vary + site + wave,
     id = as.factor(uid),  
     corstr = "exchangeable",
     data = l.t3.filtered )

summ = summary(mod)
est = coef(mod)
se = summ$coefficients[,"Robust S.E."]
lo = coef(mod) - qnorm(.975) * se
hi = coef(mod) + qnorm(.975) * se
Z = as.numeric( summ$coefficients[,"Robust z"] )
pval = 2 * ( c(1) - pnorm(abs(Z)) )

expect_equal( res1$est, as.numeric(est), tol = 0.001 )
expect_equal( res1$se, as.numeric(se), tol = 0.001 )
expect_equal( res1$lo, as.numeric(lo), tol = 0.001 )
expect_equal( res1$hi, as.numeric(hi), tol = 0.001 )
expect_equal( res1$pval, as.numeric(pval), tol = 0.001 )
}

# ~~ SET 5B: Omit FEs by site (post hoc) ------------------

missMethodsToRun = c("CC", "MI")

for ( .y in primYNames ) {
  
  .formulaString = paste(.y, " ~ treat.vary + site", sep = "" )
  
  
  for ( .missMethod in missMethodsToRun ) {
    
    cat( paste("\n\n**********Starting outcome", .y, "; method", .missMethod) )
    
    if (.missMethod == "MI") missingString = "Multiple imputation"
    if (.missMethod == "CC") missingString = "Complete-case"
    
    .results.dir = paste( results.dir, "/Analysis set 5/Set 5B (omit FEs)/", missingString, sep = "" )
    
    analyze_one_outcome( dat.cc = l.t3.filtered,
                         dats.imp = impsl.t3.filtered,
                         
                         missMethod = .missMethod,
                         yName = .y,
                         formulaString = .formulaString,
                         idString = "as.factor(uid)",
                         
                         analysisVarNames = c(.y, "treat.vary"),
                         analysisLabel = paste("set5B_geelong_outcome_", .y, sep = " " ),
                         corstr = "exchangeable",
                         .results.dir = .results.dir )
    
  }
}


# single table with all outcomes
table_all_outcomes(.results.dir = paste( results.dir,
                                         "Analysis set 5/Set 5B (omit FEs)/Multiple imputation",
                                         sep = "/" ),
                   .filename = "table_set5B_posthoc.xlsx",
                   .var.name = "treat.vary")





# ~~ SET 5C: Linear time trend (post hoc) ------------------

missMethodsToRun = c("CC", "MI")

for ( .y in primYNames ) {
  
  .formulaString = paste(.y, " ~ treat.vary + site + wave.cont", sep = "" )
  
  
  for ( .missMethod in missMethodsToRun ) {
    
    cat( paste("\n\n**********Starting outcome", .y, "; method", .missMethod) )
    
    if (.missMethod == "MI") missingString = "Multiple imputation"
    if (.missMethod == "CC") missingString = "Complete-case"
    
    .results.dir = paste( results.dir, "/Analysis set 5/Set 5C (linear time)/", missingString, sep = "" )
    
    analyze_one_outcome( dat.cc = l.t3.filtered,
                         dats.imp = impsl.t3.filtered,
                         
                         missMethod = .missMethod,
                         yName = .y,
                         formulaString = .formulaString,
                         idString = "as.factor(uid)",
                         
                         analysisVarNames = c(.y, "treat.vary"),
                         analysisLabel = paste("set5C_geelong_outcome_", .y, sep = " " ),
                         corstr = "exchangeable",
                         .results.dir = .results.dir )
    
  }
}


# single table with all outcomes
table_all_outcomes(.results.dir = paste( results.dir,
                                         "Analysis set 5/Set 5C (linear time)/Multiple imputation",
                                         sep = "/" ),
                   .filename = "table_set5C_posthoc.xlsx",
                   .var.name = "treat.vary")



# ~ GEE controlling for precision covariates --------------------------------

# already done in Analysis Set 3 :)




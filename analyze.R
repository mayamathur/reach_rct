
# NOTES -----------------------------------------------------------

# Models that need to be fit:
# - GEE of all time points (need data in long format)
# - Maybe fill in #@ things?

# Game plan:
# - Fn similar to my_ols_hc0_all, but arg is the entire model formula
# - Then should be able to modify analyze_all_outcomes to handle MI, etc.

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

# use scrambled treatment variable for blinding?
scramble.treat = FALSE


# Read in data  --------------------------------
setwd(prepped.data.dir)
d = read_csv("prepped_data.csv") 
expect_equal( nrow(d), 4571 )  # from 2022-2-1

# long dataset
l = read_csv("prepped_data_long.csv") 
expect_equal( nrow(l), 4571 * 3 )  # because 3 times/subject


# Read in imputations  --------------------------------
# read in the imputations as a list rather than a mids object so that we can pool manually
setwd(imputed.data.dir)
to.read = list.files()[ grepl( pattern = "dataset_prepped", x = list.files() ) ]
imps <<- lapply( to.read,
                 function(x) suppressMessages(read_csv(x)) )


names(imps[[1]])
mean( is.na(imps[[1]]$T2_TRIM) )

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

# stratify demographics by treatment group
t1.treat = make_table_one(.d = d %>% filter( treat == 1 ) )
t1.cntrl = make_table_one(.d = d %>% filter( treat == 0 ) )

# look for dimension mismatches caused by missing categories in one treatment group
dim(t1.treat); dim(t1.cntrl)
t1.treat$Characteristic[ !t1.treat$Characteristic %in% t1.cntrl$Characteristic ]

#@there's a lot of missing data on ethnicity


# SET 1 GEE MODELS (PRIMARY AND SECONDARY OUTCOMES) -----------------------------------------------------------


# - GEE of primary and secondary Y's ~ treat + site (Bonferroni for secondaries)

# **For these analyses, Bonferroni significance is only shown for secondaries,
#  and in paper, it should only be reported for coefficient "treat" (not sites)

missMethodsToRun = "CC"

#missMethodsToRun = c("CC", "MI")

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
                         analysisLabel = "set1",
                         bonferroni.alpha = bonferroni.alpha,
                         corstr = "exchangeable",
                         .results.dir = .results.dir )
    
    
  }
}




# ~ Sanity checks ------------------------------

# why are site coefficients so precise?
# note that Columbia is reference in coeffs above
d %>% group_by(site) %>%
  summarise( meanNA(T2_TRIM) )

d %>% group_by(treat, site) %>%
  summarise( meanNA(T2_TRIM) )



# ~~ Model 1: OLS ---------
ols = lm( T2_TRIM ~ treat + site, data = d )
summary(ols)  # model-based SEs (might be wrong)
# example: SE for South Africa  = 0.04 (similar for other sites)
# for treat: 0.03

# ~~ Model 2: OLS-HC0 ---------
# now with HC0 SEs
res = my_ols_hc_all( dat = d, ols = ols, yName = "treat", hc.type = "HC0" )
# **SE for South Africa nearly the same
# for treat: 0.04


# ~~ Model 2b: OLS-HC1 ---------
# this is better in finite samples
# https://economics.mit.edu/files/7422

res = my_ols_hc_all( dat = d, ols = ols, yName = "treat", hc.type = "HC1" )
# SE for South Africa nearly the same
# for treat: 0.03



# ~~ Model 3a: LMM with fixed AND random effects of site ---------
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


# ~~ Model 3b: LMM with random effects of site (but not fixed effects) ---------
# also try LMM
library(lme4)

lmm = lmer( T2_TRIM ~ treat + (1|site),
            data = d )

summary(lmm)
# **SE for treat: 0.03
# issue with LMM: non-normal outcomes

# get inference for site effects using empirical Bayes
# LMM without FEs of site; inference from empirical Bayes
re = ranef(lmm, condVar = TRUE)
ses = sqrt( unlist( attr(re[[1]], "postVar") ) )
ses = as.numeric(ses)
ses[4]  # South Africa

#**Empirical Bayes SE for South Africa: 0.03

# ~~ Model 4: GEE (prespecified) ---------

mod4 = gee( T2_TRIM ~ treat + site,
            id = as.factor(site),  
            corstr = "independence",
            data = d %>% filter( !is.na(T2_TRIM) ) )

summ4 = summary(mod4)
summ4$coefficients

# **Naive SE (South Africa, treat): (0.04, 0.03)
# **Robust SE: (0.0008, 0.07)



# ~~ Model 4b: GEE with clustering by uid instead of site ---------

mod4b = gee( T2_TRIM ~ treat + site,
             id = as.factor(uid),  
             corstr = "independence",
             data = d %>% filter( !is.na(T2_TRIM) ) )

summ4b = summary(mod4b)
summ4b$coefficients

# **Naive SE (South Africa, treat): (0.04, 0.03)
# **Robust SE: (0.04, 0.03)

# Now they agree almost exactly!!!

# ~~ Model 4c: GEE with fake site variable ---------

d$fake.cluster = sample( 1:6, replace = TRUE, size = nrow(d) )

mod4c = gee( T2_TRIM ~ treat + site,
             id = as.factor(fake.cluster),  
             corstr = "independence",
             data = d %>% filter( !is.na(T2_TRIM) ) )

summ4c = summary(mod4c)
summ4c$coefficients

# **Now naive and robust still match!
# This experiment suggests that the problem is having site as fixed effect
#  AND as clustering variable (not having too few clusters).





# ~~ Conclusions ---------


# The culprit is the robust SEs in GEE (not the naive ones), and only when the 
#  working correlation structure uses id = site rather than id = uid.

# on having both fixed and REs for same variable:
# https://stats.stackexchange.com/questions/263194/does-it-make-sense-to-include-a-factor-as-both-fixed-and-random-factor-in-a-line

# rationale in slide deck:
# Parzen et al. (1998). Does clustering affect the usual test statistics of no treatment effect in a randomized clinical trial?
# 
# The point of the GEE model here is to flexibly account for correlated observations between and possibly also within sites.
# Also, nonnormal outcomes




# SET 2 GEE MODELS (TREAT * TRAIT FORGIVENESS) -----------------------------------------------------------


# GEE of primary Y's ~ treat*T1_TrFS(binary) + site

#missMethodsToRun = "CC"

#@NOTE: Per preregistration, the coef for T1_high_TrFS is counted in Bonferroni,
#  but not site, so should not report the site p-values in this model

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
                         analysisLabel = "set2",
                         corstr = "exchangeable",
                         bonferroni.alpha = 0.005,
                         .results.dir = .results.dir )
    
    
  }
}




# SET 3 GEE MODELS (ANALYZE WITHIN SITE) -----------------------------------------------------------

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
      
      # * if you want to save site-by-site results automatically as well, just change .results.dir
      #  arg below
      site.res = analyze_one_outcome( 
        missMethod = .missMethod,
        yName = .y,
        formulaString = .formulaString,
        idString = "as.factor(uid)",
        subsetString = paste( "site == '", .site, "'", sep="" ),
        
        analysisVarNames = c(.fullYName, "treat"),
        analysisLabel = paste("set3", .y, .site, sep="_"),
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

#@add global test of new model with all sites vs. main one


# ~ Global test for site heterogeneity --------------------------------------


mod.small = report_gee_table( dat = d,
                              formulaString = "T2_TRIM ~ treat + site",
                              analysisVarNames = c("T2_TRIM", "treat", "site"),
                              analysisLabel = "",
                              
                              return.gee.model = TRUE,
                              write.dir = NA )

mod.full = report_gee_table( dat = d,
                              formulaString = "T2_TRIM ~ treat*site",
                              analysisVarNames = c("T2_TRIM", "treat", "site"),
                              analysisLabel = "",
                              
                              return.gee.model = TRUE,
                              write.dir = NA )

mod.full$res




# doesn't return anything useful
compCoef(mod.small$mod, mod.full$mod)

library(glmtoolbox)
anova(mod.small$mod, mod.full$mod, test="score")


# doesn't work for gee package object
geepack::QIC(mod4b)




library(MuMIn)

# doesn't work with these auto-generated models because can't parse formula string
model.sel(mod.small$mod, mod.full$mod, rank = QIC)

mod.small2 = gee( T2_TRIM ~ treat + site,
             id = as.factor(uid),  
             corstr = "exchangeable",
             data = d %>% filter( !is.na(T2_TRIM) ) )

mod.full2 = gee( T2_TRIM ~ treat*site,
             id = as.factor(uid),  
             corstr = "exchangeable",
             data = d %>% filter( !is.na(T2_TRIM) ) )

MuMIn::model.sel(mod.small2, mod.full2, rank = QIC)


# sanity check
# smaller QCI is better
mod.tiny = gee( T2_TRIM ~ 1,
                 id = as.factor(uid),  
                 corstr = "exchangeable",
                 data = d %>% filter( !is.na(T2_TRIM) ) )

# doesn't make sense because it's saying the model without treatment is better...
MuMIn::model.sel(mod.small2, mod.tiny, rank = QIC)


library(harmonicmeanp)

pvals = runif( n = 100, min=0, max=1)
p.hmp(pvals, L = length(pvals))


# extract interaction term p-values
coefNames = row.names(mod.full$res)
keepers = stringsWith( pattern = "treat:", x = coefNames )

pvals = mod.full$res$pval[ coefNames %in% keepers ]

p.hmp(pvals, L = length(pvals))





# SET 4 GEE MODELS (PRECISION COVARIATES) -----------------------------------------------------------


# - GEE of primary and secondary Y's ~ treat + site + age + sex + all baseline primY

#missMethodsToRun = "CC"

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
                         analysisLabel = "set4",
                         corstr = "exchangeable",
                         .results.dir = .results.dir )
    
    
  }
}


# @Gives warnings that SEs differed by more than 0.01
# That seems to be because gender has evil tiny category called "RECODE TROUBLE"; return to this after dataset is fixed


# PLOTS: EFFECT MAINTENANCE OVER TIME -----------------------------------------------------------

# one for each primary outcome
# points: means
# bars: +/- 1 SE (calculated by HC1, but very similar to naive; see code below)

plotList = list()

for ( i in 1:length(primYNames) ) {
  
  .y = primYNames[i]
  lp = l
  lp$Y = l[[.y]]
  
  #@later, check with TVW about type of SE to show here
  agg = lp %>% group_by(treat, wave) %>%
    summarise( Mean = meanNA(Y),
               SE = marginal_hc_se(Y),
               # sanity check:
               SE.plain = sd(Y, na.rm = TRUE) / sqrt( length(Y[!is.na(Y)] ) ) )
  
  # sanity check: should be very similar
  #abs( agg$SE - agg$SE.plain )
  
  p <<- ggplot( data = agg,
                aes( x = wave, 
                     y = Mean,
                     color = as.factor(treat) ) ) +
    
    geom_hline( yintercept = 0,
                lty = 2,
                color = "gray") +
    
    geom_point(size = 1.2) + 
    geom_line( aes(group = as.factor(treat)) ) +
    geom_errorbar( aes(ymin = Mean - SE,
                       ymax = Mean + SE),
                   width = 0 ) +
    
    scale_color_manual( values = c("black", "orange" ) ) +
    
    ylab(.y) +
    
    theme_classic()
  
  plotList[[i]] = p
  
  
} # end loop over.y


# plotList[[2]]
# plotList[[3]]

setwd(results.dir)
setwd("Figures")
ggsave("plot_effect_maintenance.pdf",
       do.call("arrangeGrob", plotList),
       width = 6,
       height = 10)


# sanity check
meanNA(d$T1_TRIM[ d$treat == 1] )
meanNA(d$T2_TRIM[ d$treat == 1] )
meanNA(d$T3_TRIM[ d$treat == 1] )



# SENSITIVITY ANALYSES -----------------------------------------------------------

# ~ 2 x 3 ANOVA --------------------------------





# ~ SET 5: GEE with all time points --------------------------------


# As sanity check, can compare these to plot_effect_maintenance.pdf

#missMethodsToRun = c("CC", "MI")
missMethodsToRun = "CC"

#@ this needs more sanity checks
for ( .y in primYNames ) {
  
  .formulaString = paste(.y, " ~ treat.vary + site", sep = "" )
  
  
  for ( .missMethod in missMethodsToRun ) {
    
    cat( paste("\n\n**********Starting outcome", .y, "; method", .missMethod) )
    
    if (.missMethod == "MI") missingString = "Multiple imputation"
    if (.missMethod == "CC") missingString = "Complete-case"
    
    .results.dir = paste( results.dir, "/Analysis set 5/", missingString, sep = "" )
    
    analyze_one_outcome( dat.cc = l,
                         dats.imp = impsl,
                         
                         missMethod = .missMethod,
                         yName = .y,
                         formulaString = .formulaString,
                         idString = "as.factor(uid)",
                         analysisVarNames = c(.y, "treat.vary", "site"),
                         analysisLabel = "set5_gee_long",
                         corstr = "exchangeable",
                         .results.dir = .results.dir )
    
    
  }
}





# ~ GEE controlling for precision covariates --------------------------------

# already done in Analysis Set 3 :)

# DEBUG GEE -----------------------------------------------------------


### Outcome BSI
# independent working structure: gee WORKS; Mancl FAILS
res = analyze_one_outcome( missMethod = "CC",
                           yName = "BSIdep",
                           formulaString = "T2_BSIdep ~ treat + site",
                           
                           #NEW
                           idString = "as.factor(uid)",
                           se.type = "mancl",
                           
                           analysisVarNames = c("T2_BSIdep", "treat", "site"),
                           analysisLabel = "set1",
                           
                           corstr = "independence",
                           
                           .results.dir = NA )
res$res.raw

# exch working structure: gee WARNS; Mancl WORKS
# "Working correlation estimate not positive definite"
res = analyze_one_outcome( missMethod = "CC",
                           yName = "BSIdep",
                           formulaString = "T2_BSIdep ~ treat + site",
                           
                           #NEW
                           idString = "as.factor(uid)",
                           se.type = "mancl",
                           
                           
                           analysisVarNames = c("T2_BSIdep", "treat", "site"),
                           analysisLabel = "set1",
                           
                           corstr = "exchangeable",
                           
                           .results.dir = NA )

res$res.raw

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
                     
                     #NEW
                     idString = "as.factor(uid)",
                     se.type = "mancl",
                     
                     .results.dir = NA )

# exch working structure: gee WARNS; Mancl FAILS
# "  Working correlation estimate not positive definite"
analyze_one_outcome( missMethod = "CC",
                     yName = "TRIM",
                     formulaString = "T2_TRIM ~ treat + site",
                     analysisVarNames = c("T2_TRIM", "treat", "site"),
                     analysisLabel = "set1",
                     
                     #NEW
                     idString = "as.factor(uid)",
                     se.type = "mancl",
                     
                     corstr = "exchangeable",
                     
                     .results.dir = NA )

# with corstr = "independence", the GEE fits but the Mancl part says "computationally singular"
# with corstr = "exchangeable", the GEE warns that the working correlation estimate isn't pos def


#**but using idString = "as.factor(uid)" means that the GEE itself never warns!


# ADDITIONAL SANITY CHECKS -----------------------------------------------------------


# ~ Boxplots of outcomes by site and treatment group --------------------------------------

# in CC dataset

dp = d

if ( run.sanity == TRUE ) {
  
  plotList = list()
  
  for ( i in 1:length(allYNames) ) {
    
    .y = allYNames[i]
    
    yName = paste( "T2_", .y, sep = "" )
    dp$Y = d[[yName]]
    
    treat0.mean = meanNA( d[[yName]][ d$treat == 0 ] )
    treat1.mean = meanNA( d[[yName]][ d$treat == 1 ] )
    
    p <<- ggplot( data = dp,
                  aes( x = site, 
                       y = Y,
                       fill = as.factor(treat),
                       color = as.factor(treat) ) ) +
      
      # overall CC means
      geom_hline( yintercept = treat0.mean,
                  lty = 2,
                  color = "black" ) + 
      
      geom_hline( yintercept = treat1.mean,
                  lty = 2,
                  color = "orange" ) + 
      
      geom_violin(draw_quantiles = TRUE,
                  alpha = 0.4,
                  position="dodge" ) + 
      
      # bars: CI limits
      stat_summary(fun.data = mean_cl_normal,
                   #fun.args = list(mult = 1),
                   #aes( color = as.factor(treat) ),
                   geom = "pointrange", 
                   position = position_dodge(width = 0.9) ) +
      
      scale_fill_manual( values = c("black", "orange" ) ) +
      scale_color_manual( values = c("black", "orange" ) ) +
      
      ylab(yName) +
      
      theme_classic()
    
    plotList[[i]] = p
    
    
  } # end loop over.y
  
  
  # plotList[[2]]
  # plotList[[3]]
  
  setwd(results.aux.dir)
  ggsave("plot_violins_by_site.pdf",
         do.call("arrangeGrob", plotList),
         width = 20,
         height = 15)
  
}















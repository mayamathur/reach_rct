
########################### UTILITY FNs FOR INTERACTING WITH THE CODE ###########################

# stands for "wipe results"
wr = function(){
  setwd(general.results.dir)
  if( "stats_for_paper.csv" %in% list.files() ) system("rm stats_for_paper.csv")
  setwd(overleaf.dir)
  if( "stats_for_paper.csv" %in% list.files() ) system("rm stats_for_paper.csv")
}

# stands for "view results"
vr = function(){
  setwd(general.results.dir)
  View( read.csv("stats_for_paper.csv") )
}

# read in everything needed for analyses 
#  facilitates re-rerunning a single section in isolation
# study: 1 or 2
prelims = function(study, overwrite.res) {
  
  ##### Packages #####
  # commented out because we're using renv
  library(dplyr)
  library(readr)
  library(tableone)
  library(ggplot2)
  library(tibble)
  library(sandwich)
  library(EValue)
  library(metafor)
  library(AER)
  library(harmonicmeanp)
  library(testthat)
  library(xtable)
  
  study <<- study
  # for dir paths
  study.string = paste( "Study", study, sep = " " )
  
  ##### Working Directories #####
  
  overleaf.dir <<- "~/Dropbox/Apps/Overleaf/EatingVeg manuscript/R_objects"
  # results dir not specific to this study (for saving results csv)
  general.results.dir <<- here("Results from R")
  
  prepped.data.dir <<- here( paste( "Data/Prepped/", study.string, sep = "" ) )
  results.dir <<- here( paste( "Results from R/", study.string, sep = "" ) )
  
  # only Studies 1 and 3 have imputations
  if ( study %in% c(1,3) ) {
    imputed.data.dir <<- here( paste( "Data/Prepped/", study.string, "/Saved imputations", sep = "" ) )
  }
  
  
  # res.raw will be a table of estimates for main outcome, secondaries, effect modifiers
  # res.overleaf will be individual stats formatted for piping into Overleaf
  if ( overwrite.res == TRUE & exists("res.raw") ) rm(res.raw)
  if ( overwrite.res == TRUE & exists("res.overleaf") ) rm(res.raw)
  
  
  ##### Dataset #####
  setwd(prepped.data.dir)
  if (study %in% c(1,3)){
    d <<- read.csv("prepped_merged_data.csv")
    
    # complete cases wrt mainY
    # might still have sporadic missing data elsewhere
    dcc <<- d %>% filter( !is.na(mainY) )
    nrow(dcc)
  }
  
  if (study == 2) {
    d <<- read.csv("prepped_data.csv")
    dcc <<- d
  }
  
  # read in imputations
  if ( study %in% c(1,3) ) {
    #setwd(imputed.data.dir)
    #load("imputed_datasets.RData")  # load "imps", the mids object
    
    # read in the imputations as a list rather than a mids object so that we can pool manually
    setwd(imputed.data.dir)
    to.read = list.files()[ grepl( pattern = "prepped", x = list.files() ) ]
    imps <<- lapply( to.read,
                     function(x) suppressMessages(read_csv(x)) )
  }
  
  ##### Lists of Variables #####
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
  
  # raw demographics, prior to collapsing categories for effect modification analyses
  demo.raw <<- c("sex",
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
                 "pDem")
  
  if (study %in% c(1,2)) {
    effect.mods <<- c("female",
                      "young",
                      "collegeGrad",
                      "cauc",  
                      "party2",
                      "pDem2")
  } else if (study == 3) {
    effect.mods <<- "targetDemoSimple"
  }
  
  
}


########################### FNs FOR STATISTICS ###########################

##### Major Fn: Analyze All Outcomes, Primary and Secondary #####

# This fn accounts for the following differences in analyses across the 3 studies:
#  - Studies 1 and 2 used marginal randomization, so each outcome is analyzed with a t-test. However, Study 2 used stratified randomization, so each outcome is analyzed with OLS + robust inference, controlling for variable, `targetDemographic`, used in randomization.
#  - For Study 3, we analyzed treatment effects on `mainY` in two ways: first among all subjects, and then only among those with `targetDemoSimple = TRUE`.
#  - Study 2 has a single binary outcome, `intentionReduce`, for which we used a log-linear model (with regular model-based inference) to estimate a risk ratio.

# uses a lot of global vars, like study
# missMethod: "MI" or "CC"
analyze_all_outcomes = function(missMethod) {
  
  if ( study %in% c(1,2,3) ) {
    ##### Analyze Each Outcome (Including Primary) #####
    
    # for Bonferroni
    n.secY = sum( length(secFoodY), length(psychY) )
    ( alpha2 = 0.05 / n.secY ) # Bonferroni-adjusted alpha
    
    # outcomes to analyze
    toAnalyze = c("mainY", secFoodY, psychY )
    if ( study == 2 ) toAnalyze = c( "intentionCont", toAnalyze )
    
    
    
    if ( exists("res.raw") ) rm(res.raw)
    
    for ( i in toAnalyze ) {
      
      # Studies 1 and 2: don't control for any covariates
      if ( study %in% c(1,2) ){
        
        if ( missMethod == "MI" ) {
          mi.res = lapply( imps, function(.d) my_ttest(yName = i,
                                                       dat = .d) )
        }
        
        if ( missMethod == "CC" ) {
          mi.res = my_ttest(yName = i,
                            dat = d)
        }
        
        
      }
      
      # Study 3: control for randomization strata
      if ( study == 3 ) {
        
        if ( missMethod == "MI" ) {
          mi.res = lapply( imps,
                           function(.d){ 
                             .d$Y = .d[[i]]
                             ols = lm( Y ~ treat + targetDemographics,
                                       data = .d)
                             my_ols_hc0( coefName = "treat",
                                         dat = .d,
                                         ols = ols,
                                         yName = i )
                           } )
        }
        
        if ( missMethod == "CC" ) {
          .d = d
          .d$Y = .d[[i]]
          ols = lm( Y ~ treat + targetDemographics,
                    data = .d)
          mi.res = my_ols_hc0( coefName = "treat",
                               dat = .d,
                               ols = ols,
                               yName = i )
          
        }
        
      }
      
      # pool the imputations
      # might have only 1 row if we're doing CC analysis
      if ( missMethod == "MI" ) {
        mi.res = do.call(what = rbind, mi.res)
        part1 = mi_pool(ests = mi.res$est, ses = mi.res$se)
        part2 = mi_pool(ests = mi.res$g, ses = mi.res$g.se)
        names(part2) = paste( "g.", names(part2), sep = "" )
        names(part2)[ names(part2) == "g.est" ] = "g"
        new.row = cbind(part1, part2)
        
      } else if ( missMethod == "CC" ) {
        # no need to pool in this case
        new.row = mi.res
      }
      
      
      # Study 3 and mainY only: also do analysis for the targetDemoSimple subset
      if ( study == 3 & i == "mainY" ) {
        
        if ( missMethod == "MI" ) {
          mi.res = lapply( imps,
                           function(.d){ 
                             # key difference from above: create the subset
                             .d = .d[ .d$targetDemoSimple == TRUE, ]
                             .d$Y = .d[[i]]
                             # note that we're still controlling for the RANDOMIZATION variable, targetDemographics, because it's not the same as targetDemoSimple
                             # in practice doesn't matter at all because so few people have targetDemographics == 1
                             ols = lm( Y ~ treat + targetDemographics,
                                       data = .d)
                             my_ols_hc0( coefName = "treat",
                                         dat = .d,
                                         ols = ols,
                                         yName = i )
                           } )
        }
        
        if ( missMethod == "CC" ) {
          # key difference from above: create the subset
          .d = d[ d$targetDemoSimple == TRUE, ]
          .d$Y = .d[[i]]
          ols = lm( Y ~ treat + targetDemographics,
                    data = .d)
          mi.res = my_ols_hc0( coefName = "treat",
                               dat = .d,
                               ols = ols,
                               yName = i )
        }
        
        
        
        # might have only 1 row if we're doing CC analysis
        if ( missMethod == "MI" ) {
          mi.res = do.call(what = rbind, mi.res)
          part1 = mi_pool(ests = mi.res$est, ses = mi.res$se)
          part2 = mi_pool(ests = mi.res$g, ses = mi.res$g.se)
          names(part2) = paste( "g.", names(part2), sep = "" )
          names(part2)[ names(part2) == "g.est" ] = "g"
          new.row2 = cbind(part1, part2)
          
        } else if ( missMethod == "CC" ) {
          # no need to pool in this case
          new.row2 = mi.res
        }
        
        # overwrite new.row to actually contain both rows
        # first row is all subjects
        # second row is targetDemoSimple = TRUE only
        new.row = bind_rows(new.row, new.row2)
        
      }  # end study == 3 & i == "mainY"
      
      
      # Study 2 only: also do analysis for the binary variable, intentionReduce
      #  This analysis actually happens during the iterate for the variable intentionCont.
      #  This is a hacky way to have this special analysis performed only once, not for every outcome in 
      #  Study 2.
      if ( study == 2 & i == "intentionCont") {
        # get inference for risk difference
        mod1 = lm( intentionReduce ~ treat,
                   data = d )
        mod1Inf = coeftest(mod1, vcov = vcovHC(mod1, type = "HC0"))["treat",]
        df = nrow(d) - 2
        tcrit = qt(p = 0.975, df = df)
        
        # get estimate and inference for RR
        mod2 = glm( intentionReduce ~ treat,
                    data = d,
                    family = binomial(link="log") )
        mod2Inf = confint(mod2)
        
        mn0 = mean( d$intentionReduce[d$treat == 0] )
        mn1 = mean( d$intentionReduce[d$treat == 1] )
        
        new.row2 = data.frame( 
          est = mn1-mn0,
          se = mod1Inf["Std. Error"],
          lo = mod1Inf["Estimate"] - mod1Inf["Std. Error"] * tcrit,
          hi = mod1Inf["Estimate"] + mod1Inf["Std. Error"] * tcrit,
          pval = mod1Inf["Pr(>|t|)"],
          g = exp( mod2$coef["treat"] ),
          g.lo = exp( mod2Inf["treat", "2.5 %"] ),
          g.hi = exp( mod2Inf["treat", "97.5 %"] ),
          pval2 = summary(mod2)$coefficients["treat","Pr(>|z|)"],
          note = "Binary outcome, so pval is from robust SEs and g's are actually risk ratios" )
        
        # overwrite new.row to actually contain both rows
        # first row is all subjects
        # second row is targetDemoSimple = TRUE only
        new.row = bind_rows(new.row, new.row2)
      }
      
      
      # Bonferroni-corrected p-value
      if( i %in% c(secFoodY, psychY) ) {
        new.row$pvalBonf = min( 1, new.row$pval * n.secY )
        new.row$group = "secY"
        
        if( i %in% secFoodY) new.row$group.specific = "secY food"
        if( i %in% psychY) new.row$group.specific = "secY psych"
        
      } else if (i %in% c("mainY") ) {
        # for primary outcome
        new.row$pvalBonf = NA
        new.row$group = i
        new.row$group.specific = i
      } else if ( i == "intentionCont" ) {
        new.row$pvalBonf = NA
        new.row$group = "intention"
        # because this outcome produces 2 rows
        new.row$group.specific = c("intentionCont", "intentionReduce")
      }
      
      # for CC analyses only, add raw means and medians
      # NOTE: FOR STUDY 3, DIFF IN MEANS WON'T EXACTLY MATCH EST BECAUSE EST CONTROLS
      #  FOR STRATIFICATION VARS
      # For Study 2, yName = "intentionCont", we have a row for intentionCont AND a row for intentionReduce,
      #   so this step will insert the same means and medians for both rows. This is overwritten in the ad hoc
      #   adjustments below. 
      if ( missMethod == "CC" ) {
        new.row = new.row %>% add_column( .before = 1,
                                          mn0 = mean( d[[i]][ d$treat == 0], na.rm = TRUE ),
                                          mn1 = mean( d[[i]][ d$treat == 1], na.rm = TRUE ),
                                          med0 = median( d[[i]][ d$treat == 0], na.rm = TRUE ),
                                          med1 = median( d[[i]][ d$treat == 1], na.rm = TRUE ) )
      }
      
      
      # add name of this analysis
      string = paste(i, missMethod, sep = " ")
      # handle the cases where a single i produces 2 rows
      if ( study == 3 & i == "mainY" ) string = c( string, paste( "mainY targetDemoSimple-subset ",
                                                                  missMethod,
                                                                  sep = "") )
      
      if ( study == 2 & i == "intentionCont" ) {
        string = c( string, paste( "intentionReduce ",
                                   missMethod,
                                   sep = "") )
        
        # also, for this outcome, need to overwrite the means and medians for the second row
        new.row$mn0[2] = mean( d[["intentionReduce"]][ d$treat == 0], na.rm = TRUE )
        new.row$mn1[2] = mean( d[["intentionReduce"]][ d$treat == 1], na.rm = TRUE )
        new.row$med0[2] = median( d[["intentionReduce"]][ d$treat == 0], na.rm = TRUE )
        new.row$med1[2] = median( d[["intentionReduce"]][ d$treat == 1], na.rm = TRUE )
 
      }
      
      new.row = add_column(new.row, analysis = string, .before = 1)
      
      if ( !exists("res.raw") ) res.raw = new.row else res.raw = bind_rows(res.raw, new.row)
    }  # end loop over all outcomes to be analyzed
    
    #res.raw
    
    
    ##### Save Both Raw and Cleaned-Up Results Tables #####
    
    # in order to have the unrounded values
    setwd(results.dir)
    if ( missMethod == "MI") write.csv(res.raw, "4_trt_effect_all_outcomes_mi.csv")
    if ( missMethod == "CC") write.csv(res.raw, "4_trt_effect_all_outcomes_cc.csv")
    
    
    # cleaned-up version
    # round it
    res.raw = res.raw %>% mutate_at( names(res.raw)[ !names(res.raw) %in% c("analysis", "analysis.1", "group", "group.specific", "note" ) ], function(x) round(x,2) )
    
    if ( missMethod == "MI") {
      res.nice = data.frame( analysis = res.raw$analysis,
                             est = stat_CI( res.raw$est, res.raw$lo, res.raw$hi),
                             g.est = stat_CI( res.raw$g, res.raw$g.lo, res.raw$g.hi),
                             pval = res.raw$pval,
                             pvalBonf = res.raw$pvalBonf )
    }
    
    if ( missMethod == "CC") {
      res.nice = data.frame( analysis = res.raw$analysis,
                             mn0 = res.raw$mn0,
                             mn1 = res.raw$mn1,
                             med0 = res.raw$med0,
                             med1 = res.raw$med1,
                             est = stat_CI( res.raw$est, res.raw$lo, res.raw$hi),
                             g.est = stat_CI( res.raw$g, res.raw$g.lo, res.raw$g.hi),
                             pval = res.raw$pval,
                             pvalBonf = res.raw$pvalBonf )
    }
    
    
    setwd(results.dir)
    if ( missMethod == "MI") write.csv(res.nice, "4_table_trt_effect_all_outcomes_mi_pretty.csv")
    if ( missMethod == "CC") write.csv(res.nice, "4_table_trt_effect_all_outcomes_cc_pretty.csv")
    
    
    ##### One-Off Stats for Paper: Main Estimates #####
    analysis.string = paste( "mainY", missMethod, sep = " ")
    
    update_result_csv( name = paste( "mainY diff", missMethod ),
                       value = round( res.raw$est[ res.raw$analysis == analysis.string], 2 ) )
    
    update_result_csv( name = paste( "mainY diff lo", missMethod ),
                       value = round( res.raw$lo[ res.raw$analysis == analysis.string], 2 ) )
    
    update_result_csv( name = paste( "mainY diff hi", missMethod ),
                       value = round( res.raw$hi[ res.raw$analysis == analysis.string], 2 ) )
    
    
    update_result_csv( name = paste( "mainY diff pval", missMethod ),
                       value = format_pval( res.raw$pval[ res.raw$analysis == analysis.string], 2 ) )
    
    update_result_csv( name = paste( "mainY diff g", missMethod ),
                       value = round( res.raw$g[ res.raw$analysis == analysis.string], 2 ) )
    
    update_result_csv( name = paste( "mainY diff g lo", missMethod ),
                       value = round( res.raw$g.lo[ res.raw$analysis == analysis.string], 2 ) )
    
    update_result_csv( name = paste( "mainY diff g hi", missMethod ),
                       value = round( res.raw$g.hi[ res.raw$analysis == analysis.string], 2 ) )
    
    ##### One-Off Stats for Study 3 Only: Main Estimate Among Target Demographic #####
    
    if ( study == 3 ) {
      analysis.string2 = paste( "mainY targetDemoSimple-subset", missMethod, sep = " ")
      
      update_result_csv( name = paste( "mainY targetDemoSimple-subset diff", missMethod ),
                         value = round( res.raw$est[ res.raw$analysis == analysis.string2], 2 ) )
      
      update_result_csv( name = paste( "mainY targetDemoSimple-subset diff lo", missMethod ),
                         value = round( res.raw$lo[ res.raw$analysis == analysis.string2], 2 ) )
      
      update_result_csv( name = paste( "mainY targetDemoSimple-subset diff hi", missMethod ),
                         value = round( res.raw$hi[ res.raw$analysis == analysis.string2], 2 ) )
      
      
      update_result_csv( name = paste( "mainY targetDemoSimple-subset diff pval", missMethod ),
                         value = format_pval( res.raw$pval[ res.raw$analysis == analysis.string2], 2 ) )
      
      update_result_csv( name = paste( "mainY targetDemoSimple-subset diff g", missMethod ),
                         value = round( res.raw$g[ res.raw$analysis == analysis.string2], 2 ) )
      
      update_result_csv( name = paste( "mainY targetDemoSimple-subset diff g lo", missMethod ),
                         value = round( res.raw$g.lo[ res.raw$analysis == analysis.string2], 2 ) )
      
      update_result_csv( name = paste( "mainY targetDemoSimple-subset diff g hi", missMethod ),
                         value = round( res.raw$g.hi[ res.raw$analysis == analysis.string2], 2 ) )
      
    }
    
    ##### One-Off Stats for Paper: Various Multiple-Testing Metrics for Secondary Outcomes #####
    update_result_csv( name = paste( "Bonferroni alpha secY", missMethod ),
                       value = round( alpha2, 4 ) )
    
    update_result_csv( name = paste( "Bonferroni number secY", missMethod ),
                       value = n.secY )
    
    update_result_csv( name = paste( "Number secY pass Bonf", missMethod ),
                       value = sum( res.raw$pvalBonf[ res.raw$group == "secY" ] < 0.05 ) )
    
    # harmonic mean p-values by subsets of effect modifiers
    update_result_csv( name = paste( "HMP all secY", missMethod ),
                       value = format_pval( p.hmp( p = res.raw$pval[ res.raw$group == "secY" ],
                                                   L = sum(res.raw$group == "secY") ), 2 ) )
    
    update_result_csv( name = paste("HMP food secY", missMethod ),
                       value = format_pval( p.hmp( p = res.raw$pval[ res.raw$group.specific == "secY food" ],
                                                   L = sum(res.raw$group.specific == "secY food") ), 2 ) )
    
    update_result_csv( name = paste( "HMP psych secY", missMethod ),
                       value = format_pval( p.hmp( p = res.raw$pval[ res.raw$group.specific == "secY psych" ],
                                                   L = sum(res.raw$group.specific == "secY psych") ), 2 ) )
    
  }  else {
    stop("not implemented for that study yet")
  }
  
  return( list(res.nice = res.nice,
               res.raw = res.raw) )
}





##### Fn: Calculate Crude RR #####

# gets the raw RR 
# assumes outcome is called "intentionReduce"
get_rr_unadj = function(condition,
                        condition.var.name = "condition",
                        control.name = "control",
                        dat) {
  
  # remove other interventions in case the study was more than 2 arms
  temp = droplevels( dat[ dat[[condition.var.name]] %in% c(condition, control.name), ] )
  
  tab = table( temp[[condition.var.name]], temp$intentionReduce )
  
  # state sample size
  print( paste( "Analyzed N:", nrow(temp) ) )
  
  library(metafor)
  es = escalc( measure = "RR",
               ai = tab[condition, 2], # X=1, Y=1
               bi = tab[condition, 1],  # X=1, Y=0
               ci = tab[control.name, 2], # X=0, Y=1
               di = tab[control.name, 1] ) # X=0, Y=0
  
  return(es)
}



##### Fn: Nicely Organize Welch t-test Results #####
my_ttest = function( yName, dat ){
  
  tres = t.test( dat[[yName]] ~ treat,
                 data = dat,
                 var.equal = FALSE )
  
  dat$Y = dat[[yName]]
  tab = suppressMessages( dat %>% group_by(treat) %>%
                            summarise( m = mean(Y, na.rm = TRUE),
                                       sd = sd(Y, na.rm = TRUE),
                                       n = n() ) )
  
  # standardized mean difference (Hedges' g)
  es = escalc( m1i = tab[2,]$m,
               sd1i = tab[2,]$sd,
               n1i = tab[2,]$n,
               
               m2i = tab[1,]$m,
               sd2i = tab[1,]$sd,
               n2i = tab[1,]$n,
               
               # Hedges' g by default
               measure = "SMD")
  summ = summary(es)
  
  return( data.frame( # documentary - control
    est = tres$estimate[2] - tres$estimate[1],
    se = tres$stderr,
    # note the reversed order because t-test calculates 
    #  control - documentary:
    lo = -tres$conf.int[2],
    hi = -tres$conf.int[1],
    pval = tres$p.value,
    
    # standardized mean difference (Hedges' g)
    g = es$yi,
    g.se = summ$sei,
    g.lo = summ$ci.lb,
    g.hi = summ$ci.ub ) )
}


##### Do OLS with Effect Modifier with Robust SEs #####


# for all coefs in regression, organize estimates and robust inference
#  into dataframe
my_ols_hc0_all = function(dat, ols, yName){
  
  coefNames = as.list( names( ols$coefficients) )
  temp = lapply( coefNames, function(.coefName) {
    my_ols_hc0(coefName = .coefName, dat = dat, ols = ols, yName = yName)
  } )
  
  # yields dataset
  do.call( what = rbind, temp )
}

# coefName: which coefficient to report
# dat: dataset (needed to calculate Hedges' g)
# ols: the OLS model with all the effect modifiers
# yName: outcome
my_ols_hc0 = function( coefName, dat, ols, yName ){
  
  dat$Y = dat[[yName]]
  
  ( se.ols = sqrt( vcov(ols)[coefName, coefName] ) )
  ( bhat.ols = coef(ols)[coefName] )
  
  # heteroskedasticity-consistent robust SEs:
  (se.hc0 = sqrt( vcovHC( ols, type="HC0")[coefName, coefName] ) )
  
  tcrit = qt(.975, df = ols$df.residual)
  t = as.numeric( abs(bhat.ols / se.hc0) )
  
  # standardized mean difference
  # **note for paper: standardizing by SD(Y|X) rather than SD(Y|X,Z) where
  #  Z is the effect modifiers because former is more directly comparable
  #  to the effect sizes in main analysis
  # note also that we need to calculate sd.pooled for each MI dataset rather than 
  #  just transforming the final pooled estimate to an SMD, because SD(Y|X) differs 
  #  in each imputed dataset
  tab = suppressMessages( dat %>% group_by(treat) %>%
                            summarise( m = mean(Y, na.rm = TRUE),
                                       sd = sd(Y, na.rm = TRUE),
                                       n = n() ) )
  num = (tab$n[1] - 1) * tab$sd[1]^2 + (tab$n[2] - 1) * tab$sd[2]^2
  denom = (tab$n[1] - 1) + (tab$n[2] - 1)
  sd.pooled = sqrt(num/denom)
  # adjustment factor for Hedges' g
  # https://www.statisticshowto.com/hedges-g/#:~:text=Hedges'%20g%20is%20a%20measure,of%20up%20to%20about%204%25.
  N = sum(tab$n)
  J = ( (N-3) / (N-2.25) ) * sqrt( (N-2) / N )
  # factor to multiply with the raw mean difference to get Hedges' g
  term = J / sd.pooled
  
  return( data.frame(
    est = bhat.ols,
    se = se.hc0,
    lo = bhat.ols - tcrit * se.hc0,
    hi = bhat.ols + tcrit * se.hc0,
    pval =  2 * ( 1 - pt(t, df = ols$df.residual ) ),
    
    # standardized mean difference (Hedges' g)
    g = bhat.ols * term,
    g.se = se.hc0 * term,
    g.lo = (bhat.ols - tcrit * se.hc0) * term,
    g.hi = (bhat.ols + tcrit * se.hc0) * term ) )
}



# OLD VERSION THAT FITS A SEPARATE MODEL FOR EACH EFFECT MODIFIER:
# # ASSUMES MODERATOR IS BINARY
# my_ols_hc0 = function( modName, dat ){
#   
#   # make sure effect modifier is binary
#   levels = unique( dat[[modName]] )
#   if( length( levels[ !is.na(levels) ] ) > 2 ) stop("Effect modifier has more than two levels -- not allowed!")
#   
#   ols = lm( mainY ~ treat*dat[[modName]], data = dat )
#   
#   # coef name of interest could be either "treat:dat[[modName]]" or "treat:dat[[modName]]TRUE"
#   #  depending on how the effect modifier is coded
#   string = names(coef(ols))[ grepl( pattern = "treat:", x = names(coef(ols)) ) ]
#   
#   ( se.ols = sqrt( vcov(ols)[string, string] ) )
#   ( bhat.ols = coef(ols)[string] )
#   
#   # heteroskedasticity-consistent robust SEs:
#   (se.hc0 = sqrt( vcovHC( ols, type="HC0")[string, string] ) )
#   
#   tcrit = qt(.975, df = ols$df.residual)
#   t = as.numeric( abs(bhat.ols / se.hc0) )
#   
#   return( data.frame( 
#     est = bhat.ols,
#     se = se.hc0,
#     lo = bhat.ols - tcrit * se.hc0,
#     hi = bhat.ols + tcrit * se.hc0,
#     pval =  2 * ( 1 - pt(t, df = ols$df.residual ) ) ) )
# }


##### Fn: Nicely Organize 2-proportion Z-test results #####
# for the sensitivity analysis with dichotomized outcome
# returns on log-RR scale
my_log_RR = function( dat ){
  
  
  es = escalc( measure = "RR",
               ai = sum( dat$treat == 1 & dat$mainYLow == 1 ), # Tx with low consumption
               bi = sum( dat$treat == 1 & dat$mainYLow == 0 ),  # Tx with high consumption
               ci = sum( dat$treat == 0 & dat$mainYLow == 1 ),  # control with low consumption
               di = sum( dat$treat == 0 & dat$mainYLow == 0 ) )  # control with high consumption
  
  
  zcrit = qnorm(.975)
  z = abs(es$yi / sqrt(es$vi))
  
  return( data.frame( 
    est = es$yi,
    se = sqrt(es$vi),
    lo = es$yi - zcrit * sqrt(es$vi),
    hi = es$yi + zcrit * sqrt(es$vi),
    pval = 2 * ( 1 - pnorm(z) ) ) )
}



##### Fn: Pool Multiple Imputations Via Rubin's Rules #####


# for all coefficients in model
# mi.res: the list with length M
mi_pool_all = function(.mi.res){
  
  coefNames = as.list( names( .mi.res$coefficients) )
  # get number of coefs from first imputation
  nCoefs = nrow(.mi.res[[1]] )
  
  # list with one element per coefficient
  # first for the coeffs on the raw scale
  temp = lapply( 1:nCoefs, function(i) {
    # to mi_pool, pass ests and SEs extracted from each imputation in the list
    raw = mi_pool( ests = unlist( lapply( .mi.res, function(j) j$est[i] ) ),
                   ses = unlist( lapply( .mi.res, function(j) j$se[i] ) ) )
    
    SMD = mi_pool( ests = unlist( lapply( .mi.res, function(j) j$g[i] ) ),
                   ses = unlist( lapply( .mi.res, function(j) j$g.se[i] ) ) )
    names(SMD) = paste( "g.", names(SMD), sep = "" )
    
    cbind(raw, SMD)
  } )
  
  # yields dataset
  .res = do.call( what = rbind, temp )
  row.names(.res) = row.names(.mi.res[[1]])
  
  
  # add Bonferroni p-values, counting only the effect modifiers 
  #  using the fact that their names have colons
  modNames = row.names(.res)[ grepl( pattern = ":", x = row.names(.res) ) ]
  nMods = length(modNames)
  .res$pvalBonf = NA
  .res$pvalBonf[ row.names(.res) %in% modNames ] = pmin( 1, .res$pval[ row.names(.res) %in% modNames ] * nMods )
  
  return(.res)
}


# for a single coefficient
# ests: ests from m imputations
# ses: ses from m imputations
mi_pool = function( ests, ses ){
  
  m = length(ests)
  
  ##### Pooled Estimate #####
  est.pool = mean(ests)
  
  ##### Pooled SE #####
  # Dong & Peng (2013), pg 5
  # within-imputation variance
  Ubar = mean( ses^2 )
  # between-imputation variance
  B = (1 / (m-1)) * sum( ( ests - mean(ests) )^2 )
  # see Marshall "Combining estimates" paper, pg 3
  se.pool = sqrt( Ubar + (1 + (1/m)) * B ) 
  
  ##### CI and P-value #####
  # Dong & Peng (2013), pg 5
  # relative increase in variance due to missing data
  r = ( ( 1 + (1/m) ) * B ) / Ubar
  # degrees of freedom without the small-sample adjustment
  vm = (m-1) * ( 1 + (1/r) )^2
  tcrit = qt(0.975, df = vm)
  
  lo.pool = est.pool - tcrit * se.pool
  hi.pool = est.pool + tcrit * se.pool
  t.pool = abs(est.pool) / se.pool
  p.pool = 2 * ( 1 - pt(t.pool, df = vm) )
  
  return( data.frame( est = est.pool,
                      se = se.pool, 
                      lo = lo.pool,
                      hi = hi.pool,
                      pval = p.pool ) )
}



##### IV Regression #####

my_ivreg = function(dat){
  
  iv = ivreg(mainY ~ treat | passCheck, data = dat)
  
  est = coef(iv)["treat"]
  summ = summary(iv, vcov = sandwich, diagnostics = TRUE)
  se = sqrt( summ$vcov["treat", "treat"] )
  t = abs(est)/se
  tcrit = qt(.975, df = iv$df.residual)
  
  # SMD
  tab = suppressMessages( dat %>% group_by(treat) %>%
                            summarise( m = mean(mainY, na.rm = TRUE),
                                       sd = sd(mainY, na.rm = TRUE),
                                       n = n() ) )
  num = (tab$n[1] - 1) * tab$sd[1]^2 + (tab$n[2] - 1) * tab$sd[2]^2
  denom = (tab$n[1] - 1) + (tab$n[2] - 1)
  sd.pooled = sqrt(num/denom)
  # adjustment factor for Hedges' g
  # https://www.statisticshowto.com/hedges-g/#:~:text=Hedges'%20g%20is%20a%20measure,of%20up%20to%20about%204%25.
  N = sum(tab$n)
  J = ( (N-3) / (N-2.25) ) * sqrt( (N-2) / N )
  # factor to multiply with the raw mean difference to get Hedges' g
  term = J / sd.pooled
  
  g = est * term
  
  
  return( data.frame( 
    est = est,
    se = se,
    lo = est - tcrit * se,
    hi = est + tcrit * se,
    pval = 2 * ( 1 - pt(t, df = iv$df.residual) ),
    
    g = g,
    g.se = se * term,
    g.lo = (est - tcrit * se) * term,
    g.hi = (est + tcrit * se) * term,
    # test for weak instruments
    # from AER package docs:
    # "an F test of the first stage regression for weak instruments"
    # so we want the stage 1 p-value to be LOW (i.e., non-weak instrument)
    stage1.pval = summ$diagnostics["Weak instruments", "p-value"] ) )
}

########################### FNs FOR FORMATTING RESULTS ###########################

# round while keeping trailing zeroes
my_round = function(x, digits) {
  formatC( round( x, digits ), format='f', digits=digits )
}


# format a p-value with scientific notation stars for cutoffs
# star.cutoffs: cutoffs for *, **, ***, etc., provided in any order
format_pval = function( p,
                        digits = 3,
                        star.cutoffs = NA ) {
  
  if (p >= 0.01) string = as.character( my_round( p, digits ) )
  if (p < 0.01 & p > 10^-5 ) string = formatC( p, format = "E", digits = 2 )
  if ( p < 10^-5 ) string = "< 1E-05"
  
  if ( ! is.na(star.cutoffs[1]) ) {
    
    # put in descending order
    star.cutoffs = sort(star.cutoffs, decreasing = TRUE)
    
    for ( i in 1 : length(star.cutoffs) ) {
      if ( p < star.cutoffs[i] ) string = paste( string, "*", sep="" )
    }
  }
  
  return(string)
}

# example
# p = seq( 0, .2, 0.001 )
# vapply( p, format_pval, "asdf" )
# vapply( p, function(x) format_pval( x, star.cutoffs = c( 0.01, 0.05) ), "asdf" )

# make a string for estimate and CI
stat_CI = function(est, lo, hi){
  paste( est, " [", lo, ", ", hi, "]", sep = "" )
}
# stat_CI( c(.5, -.1), c(.3, -.2), c(.7, .0) )

# for reproducible manuscript-writing
# adds a row to the file "stats_for_paper" with a new statistic or value for the manuscript
# optionally, "section" describes the section of code producing a given result
# expects "study" to be a global var
update_result_csv = function( name,
                              .section = section,
                              value = NA,
                              print = FALSE ) {
  setwd(general.results.dir)
  
  # attach study number to name
  name = paste("Study", study, name, sep = " ")
  
  new.rows = data.frame( name,
                         value = as.character(value),
                         section = as.character(.section) )
  
  # to avoid issues with variable types when overwriting
  new.rows$name = as.character(new.rows$name)
  new.rows$value = as.character(new.rows$value)
  new.rows$section = as.character(new.rows$section)
  
  
  if ( "stats_for_paper.csv" %in% list.files() ) {
    res.overleaf <<- read.csv( "stats_for_paper.csv",
                               stringsAsFactors = FALSE,
                               colClasses = rep("character", 3 ) )
    
    # if this entry is already in the results file, overwrite the
    #  old one
    if ( all(name %in% res.overleaf$name) ) res.overleaf[ res.overleaf$name %in% name, ] <<- new.rows
    else res.overleaf <<- rbind(res.overleaf, new.rows)
  }
  
  if ( !"stats_for_paper.csv" %in% list.files() ) {
    res.overleaf <<- new.rows
  }
  
  write.csv( res.overleaf, 
             "stats_for_paper.csv",
             row.names = FALSE,
             quote = FALSE )
  
  # also write to Overleaf
  setwd(overleaf.dir)
  write.csv( res.overleaf, 
             "stats_for_paper.csv",
             row.names = FALSE,
             quote = FALSE )
  
  if ( print == TRUE ) {
    View(res.overleaf)
  }
}


### Make my own Table 1
# x: variable to be summarized
# var.header: pretty variable name to use in table
# type: "cat", "bin01", "cont"
# num.digits: rounding
# countNA: should we count NA as its own category for cat and bin01?
# .tab1: the current table 1 (if NA, starts generating one from scratch)
# print: print the table-in-progress?
table1_add_row = function( x, # vector
                           var.header,  # variable name to use in table
                           type,
                           perc.digits = 0,
                           num.digits = 2,
                           countNA = TRUE,
                           .tab1 = NULL,
                           print = FALSE ) {
  
  useNA = ifelse( countNA == TRUE, "ifany", "no" )
  
  if ( type == "cat" ) {
    # this line will drop levels that have counts of 0, but it's surprisingly not easy to fix that
    t = table(x, useNA = useNA)
    pt = prop.table(t)
    
    row.names = names(t)
    row.names[ is.na(row.names) ] = "Not reported"
    
    stat.string = paste( t, " (", round( 100 * pt, digits = perc.digits ), "%)", sep = "" )
  }
  
  if ( type == "bin01" ) {
    # force "1" entry to be ordered first
    t = table(x == 0, useNA = useNA)
    pt = prop.table(t)
    
    row.names = names(t)
    # reverse the coding again
    row.names[ row.names == "FALSE" ] = "Yes"
    row.names[ row.names == "TRUE" ] = "No"
    row.names[ is.na(row.names) ] = "Not reported"
    
    stat.string = paste( t, " (", round( 100 * pt, digits = perc.digits ), "%)", sep = "" )
  }
  
  if ( type == "cont") {
    # assume we want the median and IQR
    if ( countNA == TRUE ) {
      
      stat.string = paste( round( median( x, na.rm = TRUE ), digits = num.digits ),
                           " (", 
                           round( quantile( x, 0.25, na.rm = TRUE ), digits = num.digits ),
                           ", ",
                           round( quantile( x, 0.75, na.rm = TRUE ), digits = num.digits ),
                           ")", 
                           sep = "" )
      
      n.NA = sum( is.na(x) )
      perc.NA = mean( is.na(x) )
      
      stat.string2 = paste( n.NA, " (", round( 100 * perc.NA, digits = perc.digits ), "%)", sep = "" )
      
      # first row is just the median, so no row name
      row.names = c("Not reported")
    }
    # haven't written the case of countNA == FALSE yet because not relevant for this paper
    
    new.row = data.frame( 
      "Characteristic" = c( var.header, row.names ),
      "Summary" = c( stat.string, stat.string2 ) )
  }
  
  if ( type %in% c("cat", "bin01") ) {
    new.row = data.frame( 
      "Characteristic" = c( var.header, row.names ),
      "Summary" = c( NA, stat.string ) )
  }
  
  # add the new row to existing Table 1, if applicable
  if ( !is.null(.tab1) ) .tab1 = rbind(.tab1, new.row)
  else .tab1 = new.row
  if ( print == TRUE ) print(.tab1)
  return(.tab1)
}



# return percent true for 0/1 variable, counting NA as own category
percTRUE_incl_NA = function(x) {
  prop.table( table(x, useNA = "ifany") )[2]
}



make_table_one = function(.d){
  t = table1_add_row( x = .d$sex,
                      var.header = "Sex",  
                      type = "cat",
                      countNA = TRUE )
  
  t = table1_add_row( x = .d$age,
                      var.header = "Age",  
                      type = "cont",
                      countNA = TRUE,
                      .tab1 = t )
  
  t = table1_add_row( x = .d$educ,
                      var.header = "Education",  
                      type = "cat",
                      countNA = TRUE,
                      .tab1 = t)
  
  t = table1_add_row( x = .d$party,
                      var.header = "Political party",  
                      type = "cat",
                      countNA = TRUE,
                      .tab1 = t)
  
  t = table1_add_row( x = .d$pDem,
                      var.header = "County liberalism",  
                      type = "cont",
                      countNA = TRUE,
                      .tab1 = t)
  
  t = table1_add_row( x = .d$cauc,
                      var.header = "Caucasian",  
                      type = "bin01",
                      countNA = TRUE,
                      .tab1 = t)
  
  t = table1_add_row( x = .d$hisp,
                      var.header = "Hispanic",  
                      type = "bin01",
                      countNA = TRUE,
                      .tab1 = t)
  
  t = table1_add_row( x = .d$black,
                      var.header = "Black/African American",  
                      type = "bin01",
                      countNA = TRUE,
                      .tab1 = t)
  
  
  t = table1_add_row( x = .d$midEast,
                      var.header = "Middle Eastern",  
                      type = "bin01",
                      countNA = TRUE,
                      .tab1 = t)
  
  
  t = table1_add_row( x = .d$pacIsl,
                      var.header = "Pacific Islander",  
                      type = "bin01",
                      countNA = TRUE,
                      .tab1 = t)
  
  
  t = table1_add_row( x = .d$natAm,
                      var.header = "Native American",  
                      type = "bin01",
                      countNA = TRUE,
                      .tab1 = t)
  
  t = table1_add_row( x = .d$SAsian,
                      var.header = "South Asian",  
                      type = "bin01",
                      countNA = TRUE,
                      .tab1 = t)
  
  t = table1_add_row( x = .d$EAsian,
                      var.header = "East Asian",  
                      type = "bin01",
                      countNA = TRUE,
                      .tab1 = t)
  
  t = table1_add_row( x = .d$SEAsian,
                      var.header = "Southeast Asian",  
                      type = "bin01",
                      countNA = TRUE,
                      .tab1 = t)
  
  return(t)
}






















# META-NOTES ------------------------------------------------

# This script writes results locally and to Overleaf

# Tables are numbered based on the section number of `analysis.R` that produces them, not the table numbers in the manuscript

# Analyzes Study 1, 2, or 3 depending on the argument to prelims()
#  specified below. 

# Meta-notes about this script:
#  - Each section is self-contained. You can start from anywhere by first running prelims() and then 
#   running just that section.
#  - You can use vr() to quickly see the stats_for_paper.csv file.


# PRELIMINARIES ------------------------------------------------

rm( list = ls() )

# set your parameters here
study = 3

# should we delete existing stats_for_paper.csv and start over?
# **NOTE: since studies all write to same results file,
#  setting to TRUE will also wipe other studies' results
overwrite.res = FALSE

run.sanity = TRUE

# other packages are loaded by prelims() below
library(here)
code.dir = here("Code (git)")
setwd(code.dir)
source("helper_analysis.R")

# restore the environment and package versions MM used to analyze
# to use more recent versions, see package list in 
# helper_analysis.R::prelims()
library(renv)
setwd(here())
restore()
# only if you want to update the renv file:
# snapshot()

# makes a bunch of global variables for different directories, lists of variables, etc.,
#  and reads in datasets
prelims(study = study,
        overwrite.res = overwrite.res)
if ( overwrite.res == TRUE ) wr()


# 1. SANITY CHECKS ------------------------------------------------

# baseline demographics
CreateTableOne( vars = c(demo.raw), strata = "treat", data = dcc )

# sanity check: treatment effects
# main and secondaries t-tests
# for Study 3, won't agree with CC results table exactly because table conditions on randomization variable

if ( study %in% c(1, 3) ) CreateTableOne( vars = c("mainY", secFoodY, psychY),
                                          strata = "treat", data = d )
if ( study == 2 ) CreateTableOne( vars = c("intentionCont", "intentionReduce", secFoodY, psychY),
                                  strata = "treat", data = d )


# 2. DESCRIPTIVE STATS & DEMOGRAPHICS TABLE ------------------------------------------------

# for keeping results csv organized
section = 2

# ~ Sample Sizes and Retention (For Study 1 & 3) ------------------------------------------------
update_result_csv( name = "N wave 1",
                   value = nrow(d) )

update_result_csv( name = "N wave 1 treat",
                   value = sum(d$treat == 1) )

update_result_csv( name = "N wave 1 cntrl",
                   value = sum(d$treat == 0) )

update_result_csv( name = "N wave 2",
                   value = nrow(dcc) )

update_result_csv( name = "N wave 2 treat",
                   value = sum(dcc$treat == 1) )

update_result_csv( name = "N wave 2 cntrl",
                   value = sum(dcc$treat == 0) )


if ( study %in% c(1,3) ){
  
  # differential attrition by treatment group and demographics
  # logit(P(missingness))
  string = paste( "is.na(mainY) ~ ", paste( "treat +", paste( demo.raw, collapse=" + "), sep = "" ) )
  missMod = glm( eval( parse( text = string ) ),
                 family = binomial(link="logit"),
                 data = d )
  setwd(results.dir)
  # write model results to text file
  sink("logit_missingness_model.txt")
  summary(missMod)
  closeAllConnections()  # needed to get output in console again after this
  
  # test entire missingness model vs. null model
  nullModel = glm( is.na(mainY) ~ 1,
                   data = d,
                   family = binomial(link = "logit") )
  # p-value for entire missingness model
  ftest = anova(missMod, nullModel, test = "Chisq" )
  update_result_csv( name = paste( "Logit missingness model global pval" ),
                     value = format.pval(ftest$`Pr(>Chi)`[2], 2 ) )
  
  # probability of being missing by treatment group
  # not conditional on covariates
  t = d %>% group_by(treat) %>%
    summarise( Pretained = mean(!is.na(mainY) ) )
  
  update_result_csv( name = paste( "Retention perc overall" ),
                     value = round( mean(!is.na(d$mainY)) * 100, 0) )
  
  update_result_csv( name = paste( "Retention perc treat", t$treat ),
                     value = round(t$Pretained * 100, 0) )
  
}


# individual follow-up times
if ( study %in% c(1,3) ) {
  update_result_csv( name = "Perc fuDays 12 to 14",
                     value = round( mean(d$fuDays <= 12, na.rm = TRUE) * 100, 0) )
  
  update_result_csv( name = "Mean fuDays",
                     value = round( mean(d$fuDays, na.rm = TRUE) ) )
  
  update_result_csv( name = "Median fuDays",
                     value = median(d$fuDays, na.rm = TRUE) )
  
  update_result_csv( name = "Min fuDays",
                     value = min(d$fuDays, na.rm = TRUE) )
  
  update_result_csv( name = "Max fuDays",
                     value = max(d$fuDays, na.rm = TRUE) )
}


# dates that data collection began and ended 
# for CONSORT checklist

if ( study %in% c(1,3) ){
  update_result_csv( name = "Earliest W1 date",
                     value = min(d$w1.date, na.rm = TRUE) )
  update_result_csv( name = "Latest W2 date",
                   value = max(d$w2.date, na.rm = TRUE) )
}
if ( study == 2 ) {
  update_result_csv( name = "Earliest W1 date",
                     value = min(d$d1.date, na.rm = TRUE) )
  update_result_csv( name = "Latest W1 date",
                     value = max(d$d1.date, na.rm = TRUE) )
}


# ~ Table 1 (Demographics Among All Wave 1 Subjects) ------------------------------------------------

# stratify demographics by treatment group
t1.treat = make_table_one(.d = d %>% filter( treat == 1 ) )
t1.cntrl = make_table_one(.d = d %>% filter( treat == 0 ) )

# if not equal, it's because a level occurs in one stratum but not the other 
nrow(t1.treat)
nrow(t1.cntrl)
# find the offending row
t1.cntrl$Characteristic[ !t1.cntrl$Characteristic %in% t1.treat$Characteristic ]

# ad hoc fix for row mismatches
if ( study ==1 ){
  t1.treat = t1.treat %>% add_row( data.frame(Characteristic = "a.subHS",
                                              Summary = "0 (0%)"),
                                   .after = 7)
}


t1 = data.frame( Characteristic = t1.treat$Characteristic,
                 Intervention = t1.treat$Summary,
                 Control = t1.cntrl$Summary )


# save it
if( overwrite.res == TRUE ){
  setwd(results.dir)
  write.csv(t1, "2_table_demographics.csv")
  
  # for Study 2, table is going in the Supplement, so also save xtable version
  write.table( print( xtable( t1,
                              include.rownames = FALSE ) ),
               file = "2_table_demographics_pretty_tex.txt"
  )
}


# ~ One-Off Stats for Paper ------------------------------------------------

# these are separate from the tables because they're specifically mentioned in the text
if ( study %in% c(1,3) ){
  
  # sex
  update_result_csv( name = "Perc female",
                     value = round( 100 * mean( d$female == 1 ), 0 ) )
  
  # age
  update_result_csv( name = "Age median",
                     value = round( median(d$age), 0 ) )
  
  # at least college-educated
  update_result_csv( name = "Perc educ at least college",
                     value = round( 100 * mean( d$educ %in% c("d.4yr", "e.post") ), 0 ) )
  
  # politics
  update_result_csv( name = "Perc Democrats",
                     value = round( 100 * mean( d$party == "Democrat" ), 0 ) )
  
  update_result_csv( name = "Perc Republicans",
                     value = round( 100 * mean( d$party == "Republican" ), 0 ) )
  
  update_result_csv( name = "Median county liberalism",
                     value = round( median( 100 * d$pDem ), 0 ) )
  
}


if ( study == 1) {
  # COVID effect on choices
  table(d$covid)
  update_result_csv( name = "Perc COVID less choice",
                     value = round( 100 * mean(d$covid[ !is.na(d$covid) ] %in% c("a.muchLess",
                                                                                 "b.somewhatLess",
                                                                                 "c.slightlyLess") ), 0 ) )
  
  update_result_csv( name = "Perc COVID more choice",
                     value = round( 100 * mean(d$covid[ !is.na(d$covid) ] %in% c("e.slightlyMore",
                                                                                 "f.somewhatMore",
                                                                                 "g.muchMore") ), 0 ) )
  
  update_result_csv( name = "Perc COVID no change",
                     value = round( 100 * mean(d$covid == "d.noChange", na.rm = TRUE), 0 ) )
}


if ( study %in% c(1,3)) {
  # attention check
  
  # Important: In Study 3, lots of subjects who were randomized didn't finish the questionnaire.
  #  Because this attention question did not require a response (because subjects could check
  #  zero boxes to indicate that the video didn't cover any of these topics), we can't just look
  #  at d$videoContent[d$treat == 0], for example: that would count everyone who stopped 
  # questionnaire before attention question as having failed it because those subjects would have 
  #  a "" response as well. 
  # So we are calculating these percentages among only subjects who finished the W1 questionnaire.
  
  # people who finished W1
  df = d[ d$w1.finishedQuestionnaire == TRUE, ]
  
  update_result_csv( name = "Perc videoContent animals treat 1",
                     value = round( 100 * mean( grepl(pattern = "animals", x = df$videoContent[df$treat == 1]) ), 0 ) )
  
  update_result_csv( name = "Perc videoContent animals treat 0",
                     value = round( 100 * mean( grepl(pattern = "animals", x = df$videoContent[df$treat == 0]) ), 0 ) )
  
  update_result_csv( name = "Perc pass check treat 1",
                     value = round( 100 * mean(df$passCheck[ df$treat == 1] == TRUE), 0 ) )
  
  update_result_csv( name = "Perc pass check treat 0",
                     value = round( 100 * mean(df$passCheck[ df$treat == 0] == TRUE), 0 ) )
  
}


#bm
# compare health motivations among control participants in Study 1 vs. 3
if ( study %in% c(1,3) ){
  
  update_result_csv( name = "cntrl importHealth",
                     value = round( mean( d$importHealth[ d$treat == 0 ], na.rm = TRUE ), 2 ) )
}

# ~ Plot Complete-Case Treatment Group Differences ------------------------------------------------

# examine skewed outcome (immaterial given sample size)
hist(dcc$mainY)

# treatment group differences...!
ggplot( data = dcc, 
        aes( x = treat.pretty,
             y = mainY ) ) +
  geom_violin(draw_quantiles = c(.25, .5, .75)) + 
  theme_bw()

setwd(results.dir)
ggsave( "descriptive_violin_mainY.pdf",
        height = 6,
        width = 6 )

# and for the intention outcome measures
if ( study == 2 ){
  # continuous intentions
  ggplot( data = dcc, 
          aes( x = treat.pretty,
               y = intentionCont ) ) +
    geom_violin(draw_quantiles = c(.25, .5, .75)) + 
    theme_bw()
  setwd(results.dir)
  ggsave( "descriptive_violin_intentionCont.pdf",
          height = 6,
          width = 6 )
}





# 4. TABLE: ALL TREATMENT EFFECTS (MI AND CC; ALL OUTCOMES)  ----------------------


section = 4

if ( exists("res.raw") ) rm(res.raw)

# this fn also write key stats to stats_for_paper.csv
( res.CC = analyze_all_outcomes(missMethod = "CC") )
# to see results
# View(res.CC$res.nice)

# study 2 doesn't have any missing data, so doesn't get MI analyses
if ( study %in% c(1,3)) {
  ( res.MI = analyze_all_outcomes(missMethod = "MI") )
  # to see results
  # View(res.MI$res.nice)
}



# xtable of CC results, for pasting into TeX supplement
setwd(results.dir)

short = res.CC$res.nice %>% select(analysis,
                                   est,
                                   g.est,
                                   pval)
write.table( print( xtable( short,
                            include.rownames = FALSE ) ),
             file = "4_table_trt_effect_all_outcomes_cc_pretty_tex.txt"
)



# ~ Sanity Check: Compare CC to MI ------------------------------------------------

if ( (study %in% c(1,3)) & run.sanity == TRUE ) {
  setwd(results.dir)
  res.cc = read.csv("4_trt_effect_all_outcomes_cc.csv")
  res.mi = read.csv("4_trt_effect_all_outcomes_mi.csv")
  expect_equal( TRUE, all( row.names(res.cc) == row.names(res.mi) ) )
  
  ggplot( data = data.frame( cc = res.cc$est, mi = res.mi$est ),
          aes( x = cc, y = mi ) ) +
    theme_bw() + 
    geom_abline( slope = 1, intercept = 0, lty = 2, color = "gray" ) +
    geom_point()
  
  setwd(results.dir)
  ggsave( "CC_vs_MI_effect_mod_ests.pdf",
          height = 6,
          width = 6 )
  
  ggplot( data = data.frame( cc = res.cc$se, mi = res.mi$se ),
          aes( x = cc, y = mi ) ) +
    theme_bw() + 
    geom_abline( slope = 1, intercept = 0, lty = 2, color = "gray" ) +
    geom_point()
  
  setwd(results.dir)
  ggsave( "CC_vs_MI_effect_mod_SEs.pdf",
          height = 6,
          width = 6 )
  
  summary( res.mi$se / res.cc$se )
}


# ~~ Sanity Checks on Main Analyses  ----------------------


# Manually reproduce est and SE in res.raw for each outcome


# reproduce MI results for Study 1
if ( run.sanity == TRUE & study == 1 ) {
  
  # names of outcomes to check
  toCheck = unlist( lapply( strsplit( res.MI$res.raw$analysis, " MI" ), function(x) x[[1]] ) )
  
  # check MI results (est and CI)
  for (yName in toCheck) {
    
    my.mi.res = lapply( imps, function(.imp) {
      
      tres = t.test( .imp[[yName]] ~ treat,
                     data = .imp,
                     var.equal = FALSE )
      
      data.frame( est = mean( .imp[[yName]][ .imp$treat == 1 ] ) - mean( .imp[[yName]][ .imp$treat == 0 ] ),
                          se = tres$stderr ) 
    }  ) 
    
    my.mi.res = do.call( rbind, my.mi.res )
    
    # pool via Rubin's Rules and confirm above results
    M = length(imps)
    my.est = mean(my.mi.res$est)
    # between-imp variance
    B = var(my.mi.res$est)
    my.se = sqrt( mean(my.mi.res$se^2) + ( 1 + (1/M) ) * B )

    expect_equal( round( my.est, 2),
                  res.MI$res.raw$est[ res.MI$res.raw$analysis == paste( yName, "MI" ) ] )
    
    expect_equal( round( my.se, 2 ),
                  res.MI$res.raw$se[ res.MI$res.raw$analysis == paste( yName, "MI" ) ] )
    
    
    
    cat( paste("\nJust checked Study 1, MI, outcome", yName) )
  }
  
  # check CC results
  for (yName in toCheck) {
    dat = dcc
    
    tres = t.test( dcc[[yName]] ~ treat,
                   data = dcc,
                   var.equal = FALSE )
    
    mn0 = mean( dcc[[yName]][ dcc$treat == 0 ] )
    mn1 = mean( dcc[[yName]][ dcc$treat == 1 ] )
    med0 = median( dcc[[yName]][ dcc$treat == 0 ] )
    med1 = median( dcc[[yName]][ dcc$treat == 1 ] )
    

    
    expect_equal( as.numeric(round( mn1, 2)),
                  res.CC$res.raw$mn1[ res.CC$res.raw$analysis == paste( yName, "CC" ) ] )
    
    expect_equal( as.numeric(round( mn0, 2)),
                  res.CC$res.raw$mn0[ res.CC$res.raw$analysis == paste( yName, "CC" ) ] )
    
    expect_equal( as.numeric(round( med1, 2)),
                  res.CC$res.raw$med1[ res.CC$res.raw$analysis == paste( yName, "CC" ) ] )
    
    expect_equal( as.numeric(round( med0, 2)),
                  res.CC$res.raw$med0[ res.CC$res.raw$analysis == paste( yName, "CC" ) ] )
    
    
    expect_equal( as.numeric(round( mn1 - mn0, 2)),
                  res.CC$res.raw$est[ res.CC$res.raw$analysis == paste( yName, "CC" ) ] )
    
    expect_equal( round( tres$stderr, 2 ),
                  res.CC$res.raw$se[ res.CC$res.raw$analysis == paste( yName, "CC" ) ] )
    
    expect_equal( round( tres$p.value, 2 ),
                  res.CC$res.raw$pval[ res.CC$res.raw$analysis == paste( yName, "CC" ) ] )
    
    cat( paste("\nJust checked Study 1, CC, outcome", yName) )
  }
}



# reproduce CC results for Study 2
if ( run.sanity == TRUE & study == 2 ) {
  
  
  # names of outcomes to check, but not the binary one
  #  that one is handled separately at the end
  toCheck = unlist( lapply( strsplit( res.CC$res.raw$analysis, " CC" ), function(x) x[[1]] ) )
  #toCheck = toCheck[ !toCheck == "intentionReduce" ]
  
  # check CC results
  for (yName in toCheck) {
    dat = dcc
    
    ### Check Raw Mean Differences ###
    
    m0 = mean( dcc[[yName]][ dcc$treat == 0], na.rm = TRUE )
    m1 = mean( dcc[[yName]][ dcc$treat == 1], na.rm = TRUE )
    
    expect_equal( round( m0, 2 ),
                  res.CC$res.raw$mn0[ res.CC$res.raw$analysis == paste( yName, "CC" ) ] )
    
    ### Check T-Test Results ###
    string = paste( yName, " ~ treat", sep = "" )
    
    # all outcomes, even the binary one, use t-test for the column "est"
    tres = t.test( dat[[yName]] ~ treat,
                   data = dcc,
                   var.equal = FALSE )
    
    # point estimate
    expect_equal( round(m1-m0, 2),
                  res.CC$res.raw$est[ res.CC$res.raw$analysis == paste( yName, "CC" ) ] )
    
    # SE
    expect_equal( round( tres$stderr, 2 ),
                  res.CC$res.raw$se[ res.CC$res.raw$analysis == paste( yName, "CC" ) ] )
    
    # CI limits
    # note that t.test contrast is coded backwards
    expect_equal( round( -tres$conf.int[2], 2 ),
                  res.CC$res.raw$lo[ res.CC$res.raw$analysis == paste( yName, "CC" ) ] )
    
    expect_equal( round( -tres$conf.int[1], 2 ),
                  res.CC$res.raw$hi[ res.CC$res.raw$analysis == paste( yName, "CC" ) ] )
    
    
    ### Check SMDs (Or RR For intentionReduce) ###      
    # now intentionReduce needs to be handled differently because it's binary, 
    #  so "g" is actually a risk ratio
    if ( yName == "intentionReduce") {
      
      mod = glm( eval( parse( text = string ) ),
                 family = binomial(link = "log"),
                 data = dat )
      CIs = exp( confint(mod) )
      
      RR = as.numeric( exp( coef(mod)["treat"] ) )
      
      expect_equal( round(RR, 2),
                    res.CC$res.raw$g[ res.CC$res.raw$analysis == paste( yName, "CC" ) ] )
      
      
      expect_equal( round( CIs["treat", "2.5 %"], 2),
                    res.CC$res.raw$g.lo[ res.CC$res.raw$analysis == paste( yName, "CC" ) ] )
      
      expect_equal( round( CIs["treat", "97.5 %"], 2),
                    res.CC$res.raw$g.hi[ res.CC$res.raw$analysis == paste( yName, "CC" ) ] )
      
    } else {
      
      # Hedges' g
      # recode to be more intuitive
      # as.factor in RHS to avoid annoying warnings
      library(effsize)
      es = cohen.d( dcc[[yName]] ~ as.factor(d$treat==0), hedges.correction = TRUE )
      
      expect_equal( round( es$estimate, 2),
                    res.CC$res.raw$g[ res.CC$res.raw$analysis == paste( yName, "CC" ) ] )
      
      expect_equal( round( sqrt(es$var), 2),
                    res.CC$res.raw$g.se[ res.CC$res.raw$analysis == paste( yName, "CC" ) ] )
      
      expect_equal( as.numeric( round( es$conf.int[1], 2) ),
                    res.CC$res.raw$g.lo[ res.CC$res.raw$analysis == paste( yName, "CC" ) ],
                    tol = 0.02 )  # because helper fn uses a different fn (escalc) whose inference is a tiny bit different
      
      expect_equal( as.numeric( round( es$conf.int[2], 2) ),
                    res.CC$res.raw$g.hi[ res.CC$res.raw$analysis == paste( yName, "CC" ) ],
                    tol = 0.02 )
    }
    
    
    cat( paste("\nJust checked Study 2, CC, outcome", yName) )
  }
  
}


# reproduce MI results for study 3
# controls for targetDemographics
if ( run.sanity == TRUE & study == 3 ) {
  
  # names of outcomes to check
  toCheck = unlist( lapply( strsplit( res.MI$res.raw$analysis, " MI" ), function(x) x[[1]] ) )
  
  # check MI results (est and CI)
  for (yName in toCheck) {
    
    my.mi.res = lapply( imps, function(.imp) {
      string = paste( yName, " ~ treat + targetDemographics", sep = "" )
      # handle the weird one
      if ( yName == "mainY targetDemoSimple-subset") {
        string = paste( "mainY ~ treat + targetDemographics", sep = "" )
        .imp = .imp[ .imp$targetDemoSimple == TRUE, ]
      }
      
      ols = lm( eval( parse( text = string ) ), data = .imp )
      est = coef(ols)["treat"]
      
      # robust SE
      se = sqrt( vcovHC( ols, type="HC0")["treat", "treat"] )
      
      t = as.numeric( abs(est / se) )
      pval = 2 * ( 1 - pt(t, df = ols$df.residual) )
      
      # only extract this one coefficient
      return( data.frame( est = est, se = se, pval = pval ) )
    }  ) 
    
    my.mi.res = do.call( rbind, my.mi.res )
    
    # pool via Rubin's Rules and confirm above results
    M = length(imps)
    my.est = mean(my.mi.res$est)
    # between-imp variance
    B = var(my.mi.res$est)
    my.se = sqrt( mean(my.mi.res$se^2) + ( 1 + (1/M) ) * B )
    
    expect_equal( round( my.est, 2),
                  res.MI$res.raw$est[ res.MI$res.raw$analysis == paste( yName, "MI" ) ] )
    expect_equal( round( my.se, 2 ),
                  res.MI$res.raw$se[ res.MI$res.raw$analysis == paste( yName, "MI" ) ] )
    
    cat( paste("\nJust checked Study 3, MI, outcome", yName) )
  }
  
  # check CC results
  for (yName in toCheck) {
    dat = dcc
    string = paste( yName, " ~ treat + targetDemographics", sep = "" )
    # handle the weird one
    if ( yName == "mainY targetDemoSimple-subset") {
      string = paste( "mainY ~ treat + targetDemographics", sep = "" )
      dat = dat[ dat$targetDemoSimple == TRUE, ]
    }  
    
    
    ols = lm( eval( parse( text = string ) ), data = dat )
    est = coef(ols)["treat"]
    
    # robust SE
    se = sqrt( vcovHC( ols, type="HC0")["treat", "treat"] )
    
    expect_equal( as.numeric(round( est, 2)),
                  res.CC$res.raw$est[ res.CC$res.raw$analysis == paste( yName, "CC" ) ] )
    expect_equal( round( se, 2 ),
                  res.CC$res.raw$se[ res.CC$res.raw$analysis == paste( yName, "CC" ) ] )
    
    cat( paste("\nJust checked Study 3, CC, outcome", yName) )
  }
}






# ~~ One-Off Stats for Study 2 ------------------------------------------------

if ( study == 2 ) {
  res.raw = res.CC$res.raw
  
  # intentions in each group
  update_result_csv( name = "intentionCont mean cntrl",
                     value = round( res.raw$mn0[ res.raw$analysis == "intentionCont CC" ], 2 ) )
  
  update_result_csv( name = "Preduce cntrl",
                     value = round( 100 * mean( d$intentionReduce[d$treat == 0] ), 0 ) )
  
  update_result_csv( name = "intentionCont mean treat",
                     value = round( res.raw$mn1[ res.raw$analysis == "intentionCont CC" ], 2 ) )
  
  update_result_csv( name = "Preduce treat",
                     value = round( 100 * mean( d$intentionReduce[d$treat == 1] ), 0 ) )
  
  
  # continuous outcomes
  toReport = c("intentionCont CC", "mainY CC", "totalMeat CC", "totalAnimProd CC" )
  
  options( scipen = 999 )
  ( pvals = format.pval( res.raw$pval[ res.raw$analysis %in% toReport ],eps =  0.0001 ) )
  
  
  update_result_csv( name = paste( res.raw$analysis[ res.raw$analysis %in% toReport ], "diff" ),
                     value = round( res.raw$est[ res.raw$analysis %in% toReport ], 2 ) )
  
  update_result_csv( name = paste( res.raw$analysis[ res.raw$analysis %in% toReport ], "lo" ),
                     value = round( res.raw$lo[ res.raw$analysis %in% toReport ], 2 ) )
  
  update_result_csv( name = paste( res.raw$analysis[ res.raw$analysis %in% toReport ], "hi" ),
                     value = round( res.raw$hi[ res.raw$analysis %in% toReport ], 2 ) )
  
  update_result_csv( name = paste( res.raw$analysis[ res.raw$analysis %in% toReport ], "pval" ),
                     value = pvals )
  
  
  
  update_result_csv( name = paste( res.raw$analysis[ res.raw$analysis %in% toReport ], "g" ),
                     value = round( res.raw$g[ res.raw$analysis %in% toReport ], 2 ) )
  
  update_result_csv( name = paste( res.raw$analysis[ res.raw$analysis %in% toReport ], "g lo" ),
                     value = round( res.raw$g.lo[ res.raw$analysis %in% toReport ], 2 ) )
  
  update_result_csv( name = paste( res.raw$analysis[ res.raw$analysis %in% toReport ], "g hi" ),
                     value = round( res.raw$g.hi[ res.raw$analysis %in% toReport ], 2 ) )
  
  # binary outcome
  # this is actually a risk ratio even though it's in the "g" column
  
  update_result_csv( name = "intentionReduce RR",
                     value = round( res.raw$g[ res.raw$analysis == "intentionReduce CC" ], 2 ) )
  
  
  update_result_csv( name = "intentionReduce RR lo",
                     value = round( res.raw$g.lo[ res.raw$analysis == "intentionReduce CC" ], 2 ) )
  
  update_result_csv( name = "intentionReduce RR hi",
                     value = round( res.raw$g.hi[ res.raw$analysis == "intentionReduce CC" ], 2 ) )
  
  update_result_csv( name = "intentionReduce RR pval",
                     value = format.pval( res.raw$pval2[ res.raw$analysis == "intentionReduce CC" ], eps = 0.0001 ) )
  
  
}


# ~~ One-Off Pledge Stats for Study 3 ------------------------------------------------


### Numeric Stats For Paper ###
if ( study == 3 ) {
  
  # proportion of subjects making pledges 
  pledgeVars = names(dcc)[ grepl(pattern = "pledge", x = names(dcc)) ]
  pledgeVars = pledgeVars[ !pledgeVars %in% c("pledgeDateGoal", "pledgeStrategies", "pledgeStrategiesFreeText")]
  
  # percent of subjects RANDOMIZED to treat=1 who made a pledge about each food
  #  if someone was randomized, then dropped out of W1, but then came back for W2,
  # we conservatively count these people as not having made a pledge
  # these people have "" for the pledge
  t = sort( dcc %>%
              filter(treat == 1) %>%
              summarise_at( .vars = pledgeVars,
                            .funs = function(x) round( 100*mean( x %in% c("Yes, I pledge to eat this food less often",
                                                                          "Yes, I pledge to stop eating this food") ) ) ),
            decreasing = TRUE )
  
  # sanity check: confirm that the denominator count for each food-specific pledge matches
  #  the number of subjects randomized to treat=1
  if ( run.sanity == TRUE ) {
    temp = dcc %>%
      filter(treat == 1) %>%
      summarise_at( .vars = pledgeVars,
                    .funs = function(x) length(x) )
    expect_equal( all.equal( unique( as.numeric(temp) ), sum(dcc$treat == 1) ),
                  TRUE )
  }
  
  # any pledge ("reduce" or "eliminate")
  t = sort(t)
  update_result_csv( name = paste( "Perc any pledge", names(t) ),
                     value = as.numeric(t) )
  
  # any "eliminate" pledge or any "reduce" pledge
  ind = (dcc$madeEliminatePledge[ dcc$treat == 1] == TRUE) | (dcc$madeReducePledge[ dcc$treat == 1] == TRUE)
  update_result_csv( name = paste( "Perc at least one pledge" ),
                     value = round( 100*mean(ind) ) )
  
  update_result_csv( name = paste( "Perc at least one eliminate pledge" ),
                     value = round( 100*mean(dcc$madeEliminatePledge[ dcc$treat == 1] == TRUE) ) )
  
  update_result_csv( name = paste( "Perc at least one reduce pledge" ),
                     value = round( 100*mean(dcc$madeReducePledge[ dcc$treat == 1] == TRUE) ) )
  
  
  # sanity checks on food-specific pledge counts
  if ( run.sanity == TRUE ) {
    for ( .var in pledgeVars ) {
      
      # randomized to treat=1
      temp = dcc[ dcc$treat == 1, ]
      denom = nrow(temp)
      num = sum( temp[[.var]] %in% c("Yes, I pledge to eat this food less often",
                                     "Yes, I pledge to stop eating this food") )
      
      
      # check percentage making any pledge for this food
      expect_equal( round( 100*(num/denom) ),
                    as.numeric(t[.var]) ) 
      
    }
    cat("\nDone checking all food-specific pledge percentages; yay")
  }
  
 
  
  ### Pledge Types Table ### 
  
  # more detailed breakdown
  # see note above about how we count droppers-out
  tEither = dcc %>% filter(treat == 1) %>%
              summarise_at( .vars = pledgeVars,
                            .funs = function(x) round( 100*mean( x %in% c("Yes, I pledge to eat this food less often",
                                                                          "Yes, I pledge to stop eating this food") ) ) )
  
  tReduce = dcc %>%
              filter(treat == 1) %>%
              summarise_at( .vars = pledgeVars,
                            .funs = function(x) round( 100*mean( x %in% c("Yes, I pledge to eat this food less often") ) ) )
  
  tElim = dcc %>%
    filter(treat == 1) %>%
    summarise_at( .vars = pledgeVars,
                  .funs = function(x) round( 100*mean( x %in% c("Yes, I pledge to stop eating this food") ) ) )
  
  pledgeTable = as.data.frame( t( rbind(tReduce, tElim, t) ) )
  names(pledgeTable) = c("PercReduce", "PercElim", "PercEither")
  
  # reorder rows to match other tables
  pledgeTable = pledgeTable[ c("pledgeChicken", "pledgeFish", "pledgePork", "pledgeBeef", 
                 "pledgeOtherMeat", "pledgeDairy", "pledgeEggs"), ]
  
  # save table
  setwd(results.dir)
  write.csv(pledgeTable, "6_pledges_cc.csv")
  
  
}  # end "if (study == 3)"





# 5. TABLE 3: EFFECT MODIFIERS ------------------------------------------------


section = 5


# look at effect modifiers as they'll be coded in analysis
CreateTableOne( vars = effect.mods, 
                data = dcc,
                includeNA = TRUE)  # last only works for NA


# ~ Multiple Imputation Effect Modification Analysis ------------------------------------------------

if ( exists("res.raw") ) rm(res.raw)

# only proceed for Studies 1 and 3 because Study 2 doesn't use MI
#  so Study 2 is handled in next section
if ( study %in% c(1,3) ) {
  
  yName = "mainY"
  
  # for each imputation, fit one model with all effect modifiers
  # returns a list of lm objects, one for each imputation
  mi.res = lapply( imps, function(.imp) {
    # fit one model with all effect modifiers
    if ( study == 1 ) string = paste( yName, " ~ ", paste( "treat*", effect.mods, collapse=" + "), sep = "" )
    
    # for Study 3, also need to control for "young", a var used in randomization that wasn't
    #  part of the simplified effect modifier
    if ( study == 3 ) string = paste( yName, " ~ ",
                                      paste( "young + treat*", effect.mods, collapse=" + "),
                                      sep = "" )
    
    ols = lm( eval( parse( text = string ) ), data = .imp )
    
    my_ols_hc0_all( dat = .imp, ols = ols, yName = yName )
    
  }  ) 
  
  
  res.raw = mi_pool_all(.mi.res = mi.res)
  
  # ~~ Save Both Raw and Cleaned-Up Results Tables ----
  # in order to have the unrounded values
  setwd(results.dir)
  write.csv(res.raw, "5_effect_mods_MI.csv")
  
  # cleaned-up version
  # round it
  # save row names before they get removed by dplyr
  rowNames = row.names(res.raw)
  # rounded version
  res.raw2 = res.raw %>% mutate_at( names(res.raw), function(x) round(x,2) )
  
  res.nice = data.frame( coef = rowNames,
                         est = stat_CI( res.raw2$est, res.raw2$lo, res.raw2$hi),
                         g.est = stat_CI( res.raw2$g.est, res.raw2$g.lo, res.raw2$g.hi),
                         pval = res.raw2$pval,
                         pvalBonf = res.raw2$pvalBonf )
  
  
  setwd(results.dir)
  write.csv(res.nice, "5_effect_mods_MI_pretty.csv")
  
  
  # ~~ Sanity Check ----
  # manually reproduce all results for a single coefficient
  if ( study == 1 & run.sanity == TRUE ) {
    # coefficient index for treat is 2 (including intercept)
    i = 2
    
    my.mi.res = lapply( imps, function(.imp) {
      string = paste( yName, " ~ ", paste( "treat*", effect.mods, collapse=" + "), sep = "" )
      
      
      ols = lm( eval( parse( text = string ) ), data = .imp )
      est = coef(ols)[i]
      
      # robust SE
      se = sqrt( vcovHC( ols, type="HC0")[i, i] )
      
      t = as.numeric( abs(est / se) )
      pval = 2 * ( 1 - pt(t, df = ols$df.residual) )
      
      # only extract this one coefficient
      return( data.frame( est = est, se = se, pval = pval ) )
    }  ) 
    
    my.mi.res = do.call( rbind, my.mi.res )
    
    # check within-imp SEs
    existing.ses = unlist( lapply( mi.res, FUN = function(table) table[i, "se"]) )
    expect_equal( existing.ses, my.mi.res$se )
    
    # check between-imp variance
    B = var(my.mi.res$est)
    existing.B = var( unlist( lapply( mi.res, FUN = function(table) table[i, "est"]) ) )
    expect_equal(existing.B, B)
    
    # pool via Rubin's Rules and confirm above results
    M = length(imps)
    my.est = mean(my.mi.res$est)
    
    my.se = sqrt( mean(my.mi.res$se^2) + ( 1 + (1/M) ) * B )
    
    expect_equal( my.est, res.raw2$est[i], tol = 0.001 )
    expect_equal( my.se, res.raw2$se[i], tol = 0.001 )
  }
  
  if ( study == 3 & run.sanity == TRUE ) {
    
    my.mi.res = lapply( imps, function(.imp) {
      
      string = "mainY ~ young + treat*targetDemoSimple"
      ols = lm( eval( parse( text = string ) ), data = .imp )
      est = coef(ols)["treat:targetDemoSimpleTRUE"]
      
      # robust SE
      se = sqrt( vcovHC( ols, type="HC0")["treat:targetDemoSimpleTRUE", "treat:targetDemoSimpleTRUE"] )
      
      t = as.numeric( abs(est / se) )
      pval = 2 * ( 1 - pt(t, df = ols$df.residual) )
      
      # only extract this one coefficient
      return( data.frame( est = est, se = se, pval = pval ) )
    }  ) 
    
    my.mi.res = do.call( rbind, my.mi.res )
    
    # pool via Rubin's Rules and confirm above results
    M = length(imps)
    my.est = mean(my.mi.res$est)
    # between-imp variance
    B = var(my.mi.res$est)
    my.se = sqrt( mean(my.mi.res$se^2) + ( 1 + (1/M) ) * B )
    
    expect_equal( my.est, res.raw["treat:targetDemoSimpleTRUE", "est"] )
    expect_equal( my.se, res.raw["treat:targetDemoSimpleTRUE", "se"] )
  }
  
  # ~ One-Off Stats for Paper ----
  
  # best combo of effect modifiers
  if ( study == 1 ){
    
    # number of moderator coefficients we estimated
    nMods = sum( !is.na(res.raw$pvalBonf) ) 
    alpha3 = 0.05 / nMods
    update_result_csv( name = "Bonferroni alpha mods",
                       value = round( alpha3, 4 ) )
    
    update_result_csv( name = "Number mods pass Bonf",
                       value = sum( res.raw$pvalBonf[ res.raw$group == "mod" ] < 0.05 ) )
    
    
    varNames = row.names(res.raw)[ grepl( x = row.names(res.raw), pattern = ":" ) ]
    # can't count coefficients for both political categories
    varNames = varNames[ !varNames == "treat:party2b.Neutral"]
    
    if ( any( res.raw$est[ row.names(res.raw) %in% varNames ] > 0 ) ) {
      stop("Calculation of best effect modifiers below won't work because some coefs were positive!")
    }
    est.best = sum( res.raw$est[ row.names(res.raw) %in% varNames ] )
    g.best = sum( res.raw$g.est[ row.names(res.raw) %in% varNames ] )
    
    # use abs value for easier reporting in paper
    update_result_csv( name = "Best effect mods absolute est",
                       value = round( abs(est.best), 2 ) )
    
    
    update_result_csv( name = "Best effect mods absolute g",
                       value = round( abs(g.best), 2 ) )
  }
  
  
  # interaction of treat:targetDemoSimple
  if ( study == 3 ) {
    
    missMethod = "MI"
    update_result_csv( name = paste( "treat:targetDemoSimpleTRUE est", missMethod ),
                       value = round( res.raw[ "treat:targetDemoSimpleTRUE", "est" ], 2 ) )
    
    update_result_csv( name = paste( "treat:targetDemoSimpleTRUE lo", missMethod ),
                       value = round( res.raw[ "treat:targetDemoSimpleTRUE", "lo" ], 2 ) )
    
    update_result_csv( name = paste( "treat:targetDemoSimpleTRUE hi", missMethod ),
                       value = round( res.raw[ "treat:targetDemoSimpleTRUE", "hi" ], 2 ) )
    
    
    update_result_csv( name = paste( "treat:targetDemoSimpleTRUE pval", missMethod ),
                       value = format_pval( res.raw["treat:targetDemoSimpleTRUE", "pval"], 2 ) )
    
    update_result_csv( name = paste( "treat:targetDemoSimpleTRUE g", missMethod ),
                       value = round( res.raw[ "treat:targetDemoSimpleTRUE", "g.est" ], 2 ) )
    
    update_result_csv( name = paste( "treat:targetDemoSimpleTRUE g lo", missMethod ),
                       value = round( res.raw[ "treat:targetDemoSimpleTRUE", "g.lo" ], 2 ) )
    
    update_result_csv( name = paste( "treat:targetDemoSimpleTRUE g hi", missMethod ),
                       value = round( res.raw[ "treat:targetDemoSimpleTRUE", "g.hi" ], 2 ) )
  }
  
  
}  # end "if (study %in% c(1,3) )"





# ~ Complete-Case Effect Modification Analysis  ------------------------------------------------


# for Study 1, this is a sensitivity analysis
# for Study 2 (no missing data), this is the only analysis

if ( study == 1 ) yName = "mainY"
if ( study == 2 ) yName = "intentionCont"


coefNames = paste("treat:", effect.mods, sep = "")

string = paste( yName, " ~ ", paste( "treat*", effect.mods, collapse=" + "), sep = "" )
ols = lm( eval( parse( text = string ) ), data = d )



res.raw = my_ols_hc0_all( dat = d, ols = ols, yName = yName )

setwd(results.dir)
write.csv(res.raw, "5_table_effect_mods_cc.csv")


# cleaned-up version
# round it
# save row names before they get removed by dplyr
rowNames = row.names(res.raw)
res.raw2 = res.raw %>% mutate_at( names(res.raw), function(x) round(x,2) )

res.nice = data.frame( coef = rowNames,
                       est = stat_CI( res.raw2$est, res.raw2$lo, res.raw2$hi),
                       g.est = stat_CI( res.raw2$g, res.raw2$g.lo, res.raw2$g.hi),
                       pval = res.raw2$pval )


setwd(results.dir)
write.csv(res.nice, "5_table_effect_mods_cc_pretty.csv")

# for pasting into Supplement
if ( study == 2 ) {
  
  setwd(results.dir)
  
  write.table( print( xtable( res.nice,
                              include.rownames = FALSE ) ),
               file = "5_effect_mods_cc_pretty_tex.txt"
  )
}



# ~ Sanity Check: Compare CC to MI ------------------------------------------------

if ( (study == 1) & run.sanity == TRUE ) {
  setwd(results.dir)
  res.cc = read.csv("5_table_effect_mods_cc.csv")
  res.mi = read.csv("5_table_effect_mods_mi.csv")
  row.names(res.cc) == row.names(res.mi)
  
  ggplot( data = data.frame( cc = res.cc$est, mi = res.mi$est ),
          aes( x = cc, y = mi ) ) +
    theme_bw() + 
    geom_abline( slope = 1, intercept = 0, lty = 2, color = "gray" ) +
    geom_point()
  
  setwd(results.dir)
  ggsave( "CC_vs_MI_effect_mod_ests.pdf",
          height = 6,
          width = 6 )
  
  ggplot( data = data.frame( cc = res.cc$se, mi = res.mi$se ),
          aes( x = cc, y = mi ) ) +
    theme_bw() + 
    geom_abline( slope = 1, intercept = 0, lty = 2, color = "gray" ) +
    geom_point()
  
  setwd(results.dir)
  ggsave( "CC_vs_MI_effect_mod_SEs.pdf",
          height = 6,
          width = 6 )
  
  summary( res.mi$se / res.cc$se )
  
  # estimates are very similar, which makes sense given
  #  how little missing data there is
  # SEs larger by about 20% with MI 
}




# SUPPLEMENT: SENSITIVITY ANALYSES ------------------------------------------------


# ~ SUBJECT AWARENESS ------------------------------------------------ 

update_result_csv( name = "Perc aware tx group",
                   value = round( 100 * mean(dcc$aware[ dcc$treat == 1 ], na.rm = TRUE ), 0 ) )

update_result_csv( name = "Perc aware cntrl group",
                   value = round( 100 * mean(dcc$aware[ dcc$treat == 0 ], na.rm = TRUE ), 0 ) )



# ~ NON-DIFFERENTIAL MEASUREMENT ERROR ------------------------------------------------

if ( study == 1 ) {
  # repeat main, MI analysis with the dichotomized outcome
  mi.res = lapply( imps, function(.d) my_ttest(yName = "mainYFreqOnly", dat = .d) )
  mi.res = do.call(what = rbind, mi.res)
  raw = mi_pool(ests = mi.res$est, ses = mi.res$se) 
  SMD = mi_pool(ests = mi.res$g, ses = mi.res$g.se) 
  
  
  update_result_csv( name = "mainYFreqOnly diff",
                     value = round( raw$est, 2 ) )
  
  update_result_csv( name = "mainYFreqOnly lo",
                     value = round( raw$lo, 2 ) )
  
  update_result_csv( name = "mainYFreqOnly hi",
                     value = round( raw$hi, 2 ) )
  
  update_result_csv( name = "mainYFreqOnly pval",
                     value = format_pval( raw$pval, 2 ) )
  
  update_result_csv( name = "mainYFreqOnly diff g",
                     value = round( SMD$est, 2 ) )
  
  update_result_csv( name = "mainYFreqOnly lo g",
                     value = round( SMD$lo, 2 ) )
  
  update_result_csv( name = "mainYFreqOnly hi g",
                     value = round( SMD$hi, 2 ) )
}


# ~ EFFECTS OF INTERVENTION NONCOMPLIANCE ------------------------------------------------ 


if ( study == 1 ) {
  # ~~ Look at CC Data ------------------------------------------------
  # look at relationship between instrument (treat) and X (video duration)
  # in CC data
  dcc %>% group_by(treat) %>%
    summarise( sum( passCheck, na.rm = TRUE) )
  table(dcc$treat, dcc$passCheck)
  # first-stage model (linear probability model; ignore the inference):
  summary( lm( passCheck ~ treat, data = dcc) )
  
  my_ivreg(dat = dcc)
  
  # ~~ IV for MI Datasets ------------------------------------------------
  mi.res = lapply( imps, function(.d) my_ivreg(dat = .d) )
  mi.res = do.call(what = rbind, mi.res)
  raw = mi_pool(ests = mi.res$est, ses = mi.res$se) 
  SMD = mi_pool(ests = mi.res$g, ses = mi.res$g.se) 
  
  # look at this manually to make sure we don't have a weak instrument
  #  (though that seems inconceivable in this case):
  mi.res$stage1.pval
  
  # sanity check
  # confirm the fact that the first-stage model has the same p-value for every imputation
  # first-stage model (linear probability model; ignore the inference):
  summary( lm( passCheck ~ treat, data = imps[[3]]) )
  # this seems to be the origin of the weak instruments p-value
  summary( ivreg(mainY ~ passCheck | treat, data = imps[[3]]), diagnostics = TRUE )
  
  
  
  update_result_csv( name = "mainY IV diff",
                     value = round( raw$est, 2 ) )
  
  update_result_csv( name = "mainY IV lo",
                     value = round( raw$lo, 2 ) )
  
  update_result_csv( name = "mainY IV hi",
                     value = round( raw$hi, 2 ) )
  
  update_result_csv( name = "mainY IV pval",
                     value = format_pval( raw$pval, 3 ) )
  
  update_result_csv( name = "mainY IV g",
                     value = round( SMD$est, 2 ) )
  
  update_result_csv( name = "mainY IV g lo",
                     value = round( SMD$lo, 2 ) )
  
  update_result_csv( name = "mainY IV g hi",
                     value = round( SMD$hi, 2 ) )
  
}





# GENERIC HELPERS  -----------------------------------------------------------

# read/write intermediate work
write_interm = function(x, filename){
  setwd(prepped.data.dir)
  #setwd("Intermediate work")
  write.csv(x, filename)
}

read_interm = function(filename){
  setwd(prepped.data.dir)
  #setwd("Intermediate work")
  read.csv(filename)
}

# like View(), but opens the extra tab if global var useView = TRUE
View2 = function(x){
  if ( useView == TRUE ) View(x) 
}

# quick length(unique) equivalent
uni = function(x){
  length(unique(x))
}

# quick mean with NAs removed
meanNA = function(x){
  mean(x, na.rm = TRUE)
}

# return strings containing anything in pattern vector
stringsWith = function(pattern, x){
  # make regex expression 
  patterns = paste(pattern, collapse="|")
  x[ grepl(pattern = patterns, x = x)]
}
# stringsWith( pattern = c("dog", "cat"),
#  x = c("dogcat", "horse", "cat", "lion") )


# return indices of strings containing anything in pattern vector
whichStrings = function(pattern, x){
  patterns = paste(pattern, collapse="|")
  grepl(pattern = pattern, x = x)
}

# stands for "wipe results"
wr = function(){
  setwd(results.dir)
  if( "stats_for_paper.csv" %in% list.files() ) system("rm stats_for_paper.csv")
  setwd(overleaf.dir)
  if( "stats_for_paper.csv" %in% list.files() ) system("rm stats_for_paper.csv")
}

# stands for "view results"
vr = function(){
  setwd(results.dir)
  View( read.csv("stats_for_paper.csv") )
}


# DATA PREP HELPERS  -----------------------------------------------------------


# read/write intermediate work
write_interm = function(x, filename){
  setwd(prepped.data.dir)
  setwd("Intermediate prepped datasets")
  write.csv(x, filename)
}

read_interm = function(filename){
  setwd(prepped.data.dir)
  setwd("Intermediate prepped datasets")
  read.csv(filename)
}




# overwrites subscales with reverse-coded versions and creates a new *standardized* composite
#  variable from the subscales' sum
#
# scale: part of scale string that appears in each subscale's variable name (e.g., "spec" for speciesism); also becomes the name of the new composite variable
# revCode: quoted names of any subscales that need to be reverse-coded
# dropSubscaleVars = should the subscale variables be removed from dataset?
recode_psych_scale = function(.d,
                              scale, 
                              revCode = NA,
                              printCorMat = TRUE,
                              dropSubscaleVars = FALSE) {
  
  # duplicate dataset just for ease of sanity-checking
  .d2 = .d
  
  subscales = names(.d2)[ grepl( x = names(.d2), pattern = scale ) ]
  
  message( paste("Subscales used: ", paste(subscales, collapse = ",") ) )
  
  # make numeric instead of factor
  .d2 = .d2 %>% mutate_at( subscales, function(x) as.numeric( as.character(x) ) )
  
  # sanity check
  # correlation matrix prior to reverse-coding
  if (printCorMat == TRUE){
    library(corrr)
    corrs = suppressWarnings( .d2 %>% select(subscales) %>%
                                correlate( use = "pairwise.complete.obs" ) )
    
    print( paste( "Reverse-coded: ", paste(revCode, collapse = ", "), sep = "" ) )
    print(corrs)
  }
  
  # just use negative values for any scales that need reverse-coding
  if ( !any( is.na(revCode) ) ) .d2[ , revCode] = -.d2[ , revCode]
  
  head( .d2 %>% select(subscales) )
  
  
  # make new variable whose name is just the root
  # new variable is mean by subject of the subscales
  .d2[[scale]] = rowMeans( .d2 %>% select(subscales) )
  
  
  # standardize the new variable
  .d2[[scale]] = ( .d2[[scale]] - mean( .d2[[scale]], na.rm = TRUE ) ) / sd( .d2[[scale]], na.rm = TRUE )
  
  # remove subscale vars
  if ( dropSubscaleVars == TRUE ) {
    .d2 = .d2 %>% select(-subscales)
  }
  
  return(.d2)
}



# ANALYSIS HELPERS  -----------------------------------------------------------


### Make my own Table 1
# x: variable to be summarized
# var.header: pretty variable name to use in table
# type: "cat", "bin01", "cont"
# num.digits: rounding
# countNA: should we count NA as its own category for cat and bin01?
# .tab1: the current table 1 (if NA, starts generating one from scratch)
# use.spacer.rows: should there be a row of NAs after each set of rows corresponding to a single variable?
# print: print the table-in-progress?
table1_add_row = function( x, # vector
                           var.header,  # variable name to use in table
                           type,
                           perc.digits = 0,
                           num.digits = 2,
                           countNA = TRUE,
                           .tab1 = NULL,
                           use.spacer.rows = TRUE,
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
  
  if ( use.spacer.rows == TRUE ) .tab1 = add_row(.tab1, Characteristic = "", Summary = "")
  
  # avoid NA's that are next to headers
  .tab1[ is.na(.tab1) ] = ""
  
  if ( print == TRUE ) print(.tab1)
  return(.tab1)
}



# return percent true for 0/1 variable, counting NA as own category
percTRUE_incl_NA = function(x) {
  prop.table( table(x, useNA = "ifany") )[2]
}



make_table_one = function(.d,
                          .include.sanity.checks = FALSE){
  t = table1_add_row( x = .d$age,
                      var.header = "Age",  
                      type = "cont",
                      countNA = TRUE )
  
  t = table1_add_row( x = .d$gender,
                      var.header = "Gender",  
                      type = "cat",
                      countNA = TRUE,
                      .tab1 = t )
  
  t = table1_add_row( x = .d$eth,
                      var.header = "Ethnicity",  
                      type = "cat",
                      countNA = TRUE,
                      .tab1 = t)
  
  t = table1_add_row( x = .d$educ,
                      var.header = "Education",  
                      type = "cat",
                      countNA = TRUE,
                      .tab1 = t)
  
  t = table1_add_row( x = .d$income,
                      var.header = "Income",  
                      type = "cat",
                      countNA = TRUE,
                      .tab1 = t)
  
  t = table1_add_row( x = .d$isReligious,
                      var.header = "Is religious",  
                      type = "bin01",
                      countNA = TRUE,
                      .tab1 = t)
  
  t = table1_add_row( x = .d$religion,
                      var.header = "Religion",  
                      type = "cat",
                      countNA = TRUE,
                      .tab1 = t)
  
  t = table1_add_row( x = .d$marstat,
                      var.header = "Marital status",  
                      type = "cat",
                      countNA = TRUE,
                      .tab1 = t)
  
  if ( .include.sanity.checks == TRUE ) {
    
    t = table1_add_row( x = .d$anyNA.T1.primY,
                        var.header = "Missing any T1 primary Y",  
                        type = "bin01",
                        countNA = TRUE,
                        .tab1 = t)
    
    t = table1_add_row( x = .d$anyNA.T2.primY,
                        var.header = "Missing any T2 primary Y",  
                        type = "bin01",
                        countNA = TRUE,
                        .tab1 = t)
    
    t = table1_add_row( x = .d$anyNA.T2.primY,
                        var.header = "Missing any T3 primary Y",  
                        type = "bin01",
                        countNA = TRUE,
                        .tab1 = t)
    
  }
  
  
  return(t)
}


# fit GEE with a given model formula and organize results nicely
report_gee_table = function(dat,
                            formulaString,
                            analysisVarNames,  # for excluding missing data
                            analysisLabel,
                            write.dir = NA){
  
  # exclude missing data
  # demonstration of how this fn works:
  # df <- tibble(x = c(1, 2, NA), y = c("a", NA, "b"))
  # df %>% drop_na(x)
  dat = dat %>% drop_na(analysisVarNames)
  
  #@add correction in Sebastien paper
  mod = geeglm( eval( parse(text = formulaString) ),
                id = uid,  #@ participant unique ID
                family = gaussian,
                corstr = "exchangeable",
                data = dat )
  
  est = coef(mod)
  se = summary(mod)$geese$mean$san.se
  lo = coef(mod) - qnorm(.975) * summary(mod)$geese$mean$san.se
  hi = coef(mod) + qnorm(.975) * summary(mod)$geese$mean$san.se
  Z = abs( coef(mod) / summary(mod)$geese$mean$san.se ) 
  
  # @consider using t-dist
  pval = 2 * ( c(1) - pnorm(as.numeric(Z)) )
  
  res = data.frame( analysis = analysisLabel,
                    variable = names(est),
                    est = est, 
                    se = se,
                    lo = lo,
                    hi = hi,
                    pval = pval,
                    n.analyzed = nrow(dat),
                    formulaString = formulaString )
  
  if ( !is.na(write.dir) ) {
    setwd(write.dir)
    write.csv( res, paste(analysisLabel, "_gee_estimates.csv") )
  }
  
  return(res)
}



# uses a lot of global vars, like study
# missMethod: "MI" or "CC"
# yName: outcomes to analyze
analyze_one_outcome = function(missMethod,
                               yName,
                               formulaString,
                               analysisVarNames, # for handling missing data in report_gee_table
                               analysisLabel,
                               .results.dir) {
  
  
  # #@TEST ONLY
  # missMethod = "MI"
  # yName = primYNames[1]
  # formulaString = "T2_BSI ~ treat + site"
  # analysisLabel = "set1_T2_BSI"
  
  # # for Bonferroni
  # n.secY = sum( length(secFoodY), length(psychY) )
  # ( alpha2 = 0.05 / n.secY ) # Bonferroni-adjusted alpha
  
  
  if ( exists("res.raw") ) suppressWarnings( rm(res.raw) )
  
  
  # ~ Fit Model(s) with MI or CC ------------------------------
  if ( missMethod == "MI" ) {
    mi.res = lapply( imps, function(.d) report_gee_table(dat = .d,
                                                         formulaString = formulaString,
                                                         analysisVarNames = analysisVarNames,
                                                         analysisLabel = analysisLabel,
                                                         write.dir = NA) )
  }
  
  if ( missMethod == "CC" ) {
    # format as list, exactly like MI, so that it can be passed to mi_pool_all
    mi.res = list( report_gee_table(dat = d,
                                    formulaString = formulaString,
                                    analysisVarNames = analysisVarNames,
                                    analysisLabel = analysisLabel,
                                    write.dir = NA) )
  }
  
  
  # ~ Pool Imputations if Applicable ------------------------------
  
  #@ EDIT MI_POOL_ALL SO THAT IT RECORDS N.ANALYZED
  
  # pool the imputations
  # might have only 1 row if we're doing CC analysis
  res.raw = mi_pool_all(mi.res)
  
  
  # ~ Prettify and Write Results Tables ------------------------------
  
  res.raw = res.raw %>% add_column(.before = 1,
                                   analysis = analysisLabel,
                                   formulaString = formulaString)
  
  #bm
  # this part breaks for CC because doesn't have pvalBonf
  digits = 2
  res.nice = data.frame( analysis = res.raw$analysis,
                         varName = row.names(res.raw),
                         est = stat_CI( round(res.raw$est, digits),
                                        round(res.raw$lo, digits),
                                        round(res.raw$hi, digits) ),
                         
                         pval = format.pval(res.raw$pval, eps  = 0.0001),
                         pvalBonf = format.pval(res.raw$pvalBonf, eps = 0.0001) )
  
  
  
  setwd(.results.dir)
  
  if (missMethod == "CC") missingString = "completeCase"
  if (missMethod == "MI") missingString = "multImp"
  
  string = paste( analysisLabel, yName, missingString, "_gee_table_raw", ".csv", sep="_" )
  write_csv(res.raw, string)
  
  string = paste( analysisLabel, yName, missingString, "_gee_table_pretty", ".csv", sep="_" )
  write_csv(res.nice, string)
  
  # # FROM EV:
  # 
  # if ( study %in% c(1,2,3) ) {
  #   ##### Analyze Each Outcome (Including Primary) #####
  #   
  #  
  #   for ( i in toAnalyze ) {
  #     
  #     # Studies 1 and 2: don't control for any covariates
  #     if ( study %in% c(1,2) ){
  #       
  #       if ( missMethod == "MI" ) {
  #         mi.res = lapply( imps, function(.d) my_ttest(yName = i,
  #                                                      dat = .d) )
  #       }
  #       
  #       if ( missMethod == "CC" ) {
  #         mi.res = my_ttest(yName = i,
  #                           dat = d)
  #       }
  #       
  #       
  #     }
  #     
  #     # Study 3: control for randomization strata
  #     if ( study == 3 ) {
  #       
  #       if ( missMethod == "MI" ) {
  #         mi.res = lapply( imps,
  #                          function(.d){ 
  #                            .d$Y = .d[[i]]
  #                            ols = lm( Y ~ treat + targetDemographics,
  #                                      data = .d)
  #                            my_ols_hc0( coefName = "treat",
  #                                        dat = .d,
  #                                        ols = ols,
  #                                        yName = i )
  #                          } )
  #       }
  #       
  #       if ( missMethod == "CC" ) {
  #         .d = d
  #         .d$Y = .d[[i]]
  #         ols = lm( Y ~ treat + targetDemographics,
  #                   data = .d)
  #         mi.res = my_ols_hc0( coefName = "treat",
  #                              dat = .d,
  #                              ols = ols,
  #                              yName = i )
  #         
  #       }
  #       
  #     }
  #     
  #     # pool the imputations
  #     # might have only 1 row if we're doing CC analysis
  #     if ( missMethod == "MI" ) {
  #       mi.res = do.call(what = rbind, mi.res)
  #       part1 = mi_pool(ests = mi.res$est, ses = mi.res$se)
  #       part2 = mi_pool(ests = mi.res$g, ses = mi.res$g.se)
  #       names(part2) = paste( "g.", names(part2), sep = "" )
  #       names(part2)[ names(part2) == "g.est" ] = "g"
  #       new.row = cbind(part1, part2)
  #       
  #     } else if ( missMethod == "CC" ) {
  #       # no need to pool in this case
  #       new.row = mi.res
  #     }
  #     
  #     
  #     # Study 3 and mainY only: also do analysis for the targetDemoSimple subset
  #     if ( study == 3 & i == "mainY" ) {
  #       
  #       if ( missMethod == "MI" ) {
  #         mi.res = lapply( imps,
  #                          function(.d){ 
  #                            # key difference from above: create the subset
  #                            .d = .d[ .d$targetDemoSimple == TRUE, ]
  #                            .d$Y = .d[[i]]
  #                            # note that we're still controlling for the RANDOMIZATION variable, targetDemographics, because it's not the same as targetDemoSimple
  #                            # in practice doesn't matter at all because so few people have targetDemographics == 1
  #                            ols = lm( Y ~ treat + targetDemographics,
  #                                      data = .d)
  #                            my_ols_hc0( coefName = "treat",
  #                                        dat = .d,
  #                                        ols = ols,
  #                                        yName = i )
  #                          } )
  #       }
  #       
  #       if ( missMethod == "CC" ) {
  #         # key difference from above: create the subset
  #         .d = d[ d$targetDemoSimple == TRUE, ]
  #         .d$Y = .d[[i]]
  #         ols = lm( Y ~ treat + targetDemographics,
  #                   data = .d)
  #         mi.res = my_ols_hc0( coefName = "treat",
  #                              dat = .d,
  #                              ols = ols,
  #                              yName = i )
  #       }
  #       
  #       
  #       
  #       # might have only 1 row if we're doing CC analysis
  #       if ( missMethod == "MI" ) {
  #         mi.res = do.call(what = rbind, mi.res)
  #         part1 = mi_pool(ests = mi.res$est, ses = mi.res$se)
  #         part2 = mi_pool(ests = mi.res$g, ses = mi.res$g.se)
  #         names(part2) = paste( "g.", names(part2), sep = "" )
  #         names(part2)[ names(part2) == "g.est" ] = "g"
  #         new.row2 = cbind(part1, part2)
  #         
  #       } else if ( missMethod == "CC" ) {
  #         # no need to pool in this case
  #         new.row2 = mi.res
  #       }
  #       
  #       # overwrite new.row to actually contain both rows
  #       # first row is all subjects
  #       # second row is targetDemoSimple = TRUE only
  #       new.row = bind_rows(new.row, new.row2)
  #       
  #     }  # end study == 3 & i == "mainY"
  #     
  #     
  #     # Study 2 only: also do analysis for the binary variable, intentionReduce
  #     #  This analysis actually happens during the iterate for the variable intentionCont.
  #     #  This is a hacky way to have this special analysis performed only once, not for every outcome in 
  #     #  Study 2.
  #     if ( study == 2 & i == "intentionCont") {
  #       # get inference for risk difference
  #       mod1 = lm( intentionReduce ~ treat,
  #                  data = d )
  #       mod1Inf = coeftest(mod1, vcov = vcovHC(mod1, type = "HC0"))["treat",]
  #       df = nrow(d) - 2
  #       tcrit = qt(p = 0.975, df = df)
  #       
  #       # get estimate and inference for RR
  #       mod2 = glm( intentionReduce ~ treat,
  #                   data = d,
  #                   family = binomial(link="log") )
  #       mod2Inf = confint(mod2)
  #       
  #       mn0 = mean( d$intentionReduce[d$treat == 0] )
  #       mn1 = mean( d$intentionReduce[d$treat == 1] )
  #       
  #       new.row2 = data.frame( 
  #         est = mn1-mn0,
  #         se = mod1Inf["Std. Error"],
  #         lo = mod1Inf["Estimate"] - mod1Inf["Std. Error"] * tcrit,
  #         hi = mod1Inf["Estimate"] + mod1Inf["Std. Error"] * tcrit,
  #         pval = mod1Inf["Pr(>|t|)"],
  #         g = exp( mod2$coef["treat"] ),
  #         g.lo = exp( mod2Inf["treat", "2.5 %"] ),
  #         g.hi = exp( mod2Inf["treat", "97.5 %"] ),
  #         pval2 = summary(mod2)$coefficients["treat","Pr(>|z|)"],
  #         note = "Binary outcome, so pval is from robust SEs and g's are actually risk ratios" )
  #       
  #       # overwrite new.row to actually contain both rows
  #       # first row is all subjects
  #       # second row is targetDemoSimple = TRUE only
  #       new.row = bind_rows(new.row, new.row2)
  #     }
  #     
  #     
  #     # Bonferroni-corrected p-value
  #     if( i %in% c(secFoodY, psychY) ) {
  #       new.row$pvalBonf = min( 1, new.row$pval * n.secY )
  #       new.row$group = "secY"
  #       
  #       if( i %in% secFoodY) new.row$group.specific = "secY food"
  #       if( i %in% psychY) new.row$group.specific = "secY psych"
  #       
  #     } else if (i %in% c("mainY") ) {
  #       # for primary outcome
  #       new.row$pvalBonf = NA
  #       new.row$group = i
  #       new.row$group.specific = i
  #     } else if ( i == "intentionCont" ) {
  #       new.row$pvalBonf = NA
  #       new.row$group = "intention"
  #       # because this outcome produces 2 rows
  #       new.row$group.specific = c("intentionCont", "intentionReduce")
  #     }
  #     
  #     # for CC analyses only, add raw means and medians
  #     # NOTE: FOR STUDY 3, DIFF IN MEANS WON'T EXACTLY MATCH EST BECAUSE EST CONTROLS
  #     #  FOR STRATIFICATION VARS
  #     # For Study 2, yName = "intentionCont", we have a row for intentionCont AND a row for intentionReduce,
  #     #   so this step will insert the same means and medians for both rows. This is overwritten in the ad hoc
  #     #   adjustments below. 
  #     if ( missMethod == "CC" ) {
  #       new.row = new.row %>% add_column( .before = 1,
  #                                         mn0 = mean( d[[i]][ d$treat == 0], na.rm = TRUE ),
  #                                         mn1 = mean( d[[i]][ d$treat == 1], na.rm = TRUE ),
  #                                         med0 = median( d[[i]][ d$treat == 0], na.rm = TRUE ),
  #                                         med1 = median( d[[i]][ d$treat == 1], na.rm = TRUE ) )
  #     }
  #     
  #     
  #     # add name of this analysis
  #     string = paste(i, missMethod, sep = " ")
  #     # handle the cases where a single i produces 2 rows
  #     if ( study == 3 & i == "mainY" ) string = c( string, paste( "mainY targetDemoSimple-subset ",
  #                                                                 missMethod,
  #                                                                 sep = "") )
  #     
  #     if ( study == 2 & i == "intentionCont" ) {
  #       string = c( string, paste( "intentionReduce ",
  #                                  missMethod,
  #                                  sep = "") )
  #       
  #       # also, for this outcome, need to overwrite the means and medians for the second row
  #       new.row$mn0[2] = mean( d[["intentionReduce"]][ d$treat == 0], na.rm = TRUE )
  #       new.row$mn1[2] = mean( d[["intentionReduce"]][ d$treat == 1], na.rm = TRUE )
  #       new.row$med0[2] = median( d[["intentionReduce"]][ d$treat == 0], na.rm = TRUE )
  #       new.row$med1[2] = median( d[["intentionReduce"]][ d$treat == 1], na.rm = TRUE )
  #       
  #     }
  #     
  #     new.row = add_column(new.row, analysis = string, .before = 1)
  #     
  #     if ( !exists("res.raw") ) res.raw = new.row else res.raw = bind_rows(res.raw, new.row)
  #   }  # end loop over all outcomes to be analyzed
  #   
  #   #res.raw
  #   
  #   
  #   ##### Save Both Raw and Cleaned-Up Results Tables #####
  #   
  #   # in order to have the unrounded values
  #   setwd(results.dir)
  #   if ( missMethod == "MI") write.csv(res.raw, "4_trt_effect_all_outcomes_mi.csv")
  #   if ( missMethod == "CC") write.csv(res.raw, "4_trt_effect_all_outcomes_cc.csv")
  #   
  #   
  #   # cleaned-up version
  #   # round it
  #   res.raw = res.raw %>% mutate_at( names(res.raw)[ !names(res.raw) %in% c("analysis", "analysis.1", "group", "group.specific", "note" ) ], function(x) round(x,2) )
  #   
  #   if ( missMethod == "MI") {
  #     res.nice = data.frame( analysis = res.raw$analysis,
  #                            est = stat_CI( res.raw$est, res.raw$lo, res.raw$hi),
  #                            g.est = stat_CI( res.raw$g, res.raw$g.lo, res.raw$g.hi),
  #                            pval = res.raw$pval,
  #                            pvalBonf = res.raw$pvalBonf )
  #   }
  #   
  #   if ( missMethod == "CC") {
  #     res.nice = data.frame( analysis = res.raw$analysis,
  #                            mn0 = res.raw$mn0,
  #                            mn1 = res.raw$mn1,
  #                            med0 = res.raw$med0,
  #                            med1 = res.raw$med1,
  #                            est = stat_CI( res.raw$est, res.raw$lo, res.raw$hi),
  #                            g.est = stat_CI( res.raw$g, res.raw$g.lo, res.raw$g.hi),
  #                            pval = res.raw$pval,
  #                            pvalBonf = res.raw$pvalBonf )
  #   }
  #   
  #   
  #   setwd(results.dir)
  #   if ( missMethod == "MI") write.csv(res.nice, "4_table_trt_effect_all_outcomes_mi_pretty.csv")
  #   if ( missMethod == "CC") write.csv(res.nice, "4_table_trt_effect_all_outcomes_cc_pretty.csv")
  #   
  #   
  #   ##### One-Off Stats for Paper: Main Estimates #####
  #   analysis.string = paste( "mainY", missMethod, sep = " ")
  #   
  #   update_result_csv( name = paste( "mainY diff", missMethod ),
  #                      value = round( res.raw$est[ res.raw$analysis == analysis.string], 2 ) )
  #   
  #   update_result_csv( name = paste( "mainY diff lo", missMethod ),
  #                      value = round( res.raw$lo[ res.raw$analysis == analysis.string], 2 ) )
  #   
  #   update_result_csv( name = paste( "mainY diff hi", missMethod ),
  #                      value = round( res.raw$hi[ res.raw$analysis == analysis.string], 2 ) )
  #   
  #   
  #   update_result_csv( name = paste( "mainY diff pval", missMethod ),
  #                      value = format_pval( res.raw$pval[ res.raw$analysis == analysis.string], 2 ) )
  #   
  #   update_result_csv( name = paste( "mainY diff g", missMethod ),
  #                      value = round( res.raw$g[ res.raw$analysis == analysis.string], 2 ) )
  #   
  #   update_result_csv( name = paste( "mainY diff g lo", missMethod ),
  #                      value = round( res.raw$g.lo[ res.raw$analysis == analysis.string], 2 ) )
  #   
  #   update_result_csv( name = paste( "mainY diff g hi", missMethod ),
  #                      value = round( res.raw$g.hi[ res.raw$analysis == analysis.string], 2 ) )
  #   
  #   ##### One-Off Stats for Study 3 Only: Main Estimate Among Target Demographic #####
  #   
  #   if ( study == 3 ) {
  #     analysis.string2 = paste( "mainY targetDemoSimple-subset", missMethod, sep = " ")
  #     
  #     update_result_csv( name = paste( "mainY targetDemoSimple-subset diff", missMethod ),
  #                        value = round( res.raw$est[ res.raw$analysis == analysis.string2], 2 ) )
  #     
  #     update_result_csv( name = paste( "mainY targetDemoSimple-subset diff lo", missMethod ),
  #                        value = round( res.raw$lo[ res.raw$analysis == analysis.string2], 2 ) )
  #     
  #     update_result_csv( name = paste( "mainY targetDemoSimple-subset diff hi", missMethod ),
  #                        value = round( res.raw$hi[ res.raw$analysis == analysis.string2], 2 ) )
  #     
  #     
  #     update_result_csv( name = paste( "mainY targetDemoSimple-subset diff pval", missMethod ),
  #                        value = format_pval( res.raw$pval[ res.raw$analysis == analysis.string2], 2 ) )
  #     
  #     update_result_csv( name = paste( "mainY targetDemoSimple-subset diff g", missMethod ),
  #                        value = round( res.raw$g[ res.raw$analysis == analysis.string2], 2 ) )
  #     
  #     update_result_csv( name = paste( "mainY targetDemoSimple-subset diff g lo", missMethod ),
  #                        value = round( res.raw$g.lo[ res.raw$analysis == analysis.string2], 2 ) )
  #     
  #     update_result_csv( name = paste( "mainY targetDemoSimple-subset diff g hi", missMethod ),
  #                        value = round( res.raw$g.hi[ res.raw$analysis == analysis.string2], 2 ) )
  #     
  #   }
  #   
  #   ##### One-Off Stats for Paper: Various Multiple-Testing Metrics for Secondary Outcomes #####
  #   update_result_csv( name = paste( "Bonferroni alpha secY", missMethod ),
  #                      value = round( alpha2, 4 ) )
  #   
  #   update_result_csv( name = paste( "Bonferroni number secY", missMethod ),
  #                      value = n.secY )
  #   
  #   update_result_csv( name = paste( "Number secY pass Bonf", missMethod ),
  #                      value = sum( res.raw$pvalBonf[ res.raw$group == "secY" ] < 0.05 ) )
  #   
  #   # harmonic mean p-values by subsets of effect modifiers
  #   update_result_csv( name = paste( "HMP all secY", missMethod ),
  #                      value = format_pval( p.hmp( p = res.raw$pval[ res.raw$group == "secY" ],
  #                                                  L = sum(res.raw$group == "secY") ), 2 ) )
  #   
  #   update_result_csv( name = paste("HMP food secY", missMethod ),
  #                      value = format_pval( p.hmp( p = res.raw$pval[ res.raw$group.specific == "secY food" ],
  #                                                  L = sum(res.raw$group.specific == "secY food") ), 2 ) )
  #   
  #   update_result_csv( name = paste( "HMP psych secY", missMethod ),
  #                      value = format_pval( p.hmp( p = res.raw$pval[ res.raw$group.specific == "secY psych" ],
  #                                                  L = sum(res.raw$group.specific == "secY psych") ), 2 ) )
  #   
  # }  else {
  #   stop("not implemented for that study yet")
  # }
  # 
  # return( list(res.nice = res.nice,
  #              res.raw = res.raw) )
}


# for a single coefficient
# ests: ests from m imputations
# ses: ses from m imputations
mi_pool = function( ests,
                    ses,
                    # below are just for the m=1 (complete-case) possibility
                    los = NA,
                    his = NA,
                    pvals = NA
){
  
  m = length(ests)
  
  if ( m == 1 ){
    return( data.frame( est = ests,
                        se = ses, 
                        lo = los,
                        hi = his,
                        pval = pvals ) )
  }
  
  if ( m > 1 ) {
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
  
  
}




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
                   ses = unlist( lapply( .mi.res, function(j) j$se[i] ) ),
                   los = unlist( lapply( .mi.res, function(j) j$lo[i] ) ),
                   his = unlist( lapply( .mi.res, function(j) j$hi[i] ) ),
                   pvals = unlist( lapply( .mi.res, function(j) j$pval[i] ) ) )
    
    raw
    
    # SMD = mi_pool( ests = unlist( lapply( .mi.res, function(j) j$g[i] ) ),
    #                ses = unlist( lapply( .mi.res, function(j) j$g.se[i] ) ) )
    # names(SMD) = paste( "g.", names(SMD), sep = "" )
    
    #cbind(raw, SMD)
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

# make a string for estimate and CI
stat_CI = function(est, lo, hi){
  paste( est, " [", lo, ", ", hi, "]", sep = "" )
}
# stat_CI( c(.5, -.1), c(.3, -.2), c(.7, .0) )

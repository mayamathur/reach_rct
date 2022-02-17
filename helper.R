

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




wrangle_post_imputation = function(.dat) {
  
  # ~ Make new variables ------------------------------------
  
  # vars that weren't useful to carry through imputation process
  
  # indicators for having any missingness on a primary outcome at each time
  message("You'll need to edit var names here after Man Yee recodes")
  .dat = .dat %>% rowwise() %>%
    mutate( anyNA.T1.primY = any( is.na(T1_BSIdep), is.na(T1_BSIanx), is.na(T1_TRIM) ),
            anyNA.T2.primY = any( is.na(T2_BSIdep), is.na(T2_BSIanx), is.na(T2_TRIM) ),
            anyNA.T3.primY = any( is.na(T3_BSIdep), is.na(T3_BSIanx), is.na(T3_TRIM) ) )
  # sanity check on this
  if ( run.sanity == TRUE ) {
    mine = is.na(.dat$T1_BSIdep) | is.na(.dat$T1_BSIanx) | is.na(.dat$T1_TRIM)
    expect_equal( .dat$anyNA.T1.primY, mine )
  }
  
  
  # median-split T1_TrFS for sensitivity analysis
  CC.median = median( d$T1_TrFS, na.rm = TRUE )  # will be fairly close to 0 because standardized
  .dat$T1_high_TrFS = .dat$T1_TrFS > CC.median
  
  return(.dat)
}

make_long_dataset = function(.dat) {
  # for effect-maintenance plots and secondary analysis
  # see "anscombe" example here: https://tidyr.tidyverse.org/reference/pivot_longer.html
  
  l = .dat %>% pivot_longer( cols = T1_BSIdep : T3_TSHS,
                             names_to = c("wave", ".value"),
                             names_sep = "_" ) 
  
  # make long-format treatment variable
  l$treat.vary = NA
  l$treat.vary[ l$wave == "T1" ] = 0
  l$treat.vary[ l$wave == "T2" & l$treat == 0 ] = 0
  l$treat.vary[ l$wave == "T2" & l$treat == 1 ] = 1
  l$treat.vary[ l$wave == "T3" ] = 1
  
  # sanity check
  l %>% group_by(wave, treat) %>%
    summarise( unique(treat.vary) )
  
  l
  
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
                            idString = "as.factor(site)",  # default is for main analysis, but change change to participant ID for long GEE sensitivity analysis
                            subsetString = NA,  # should we subset the data?
                            analysisVarNames,  # for excluding missing data
                            analysisLabel,  # will become an identifer column in dataset
                            corstr = "exchangeable",
                            se.type = "model",  # "model" or "mancl"
                            
                            write.dir = NA){
  
  # ~ Exclude missing data to please gee() --------------------------
  # demonstration of how this fn works:
  # df <- tibble(x = c(1, 2, NA), y = c("a", NA, "b"))
  # df %>% drop_na(x)
  dat = dat %>% drop_na(analysisVarNames)
  
  if ( !is.na(subsetString) ) dat = dat %>% filter( eval( parse(text = subsetString) ) )
  message( paste("\n**** For analysis ", analysisLabel, ", made subset of size ", nrow(dat), sep = "" ) )
  
  
  # ~ Fit GEE (without Mancl correction to SEs) to get coefs  --------------------------
  mod  = suppressMessages( gee( eval( parse(text = formulaString) ),
                                id = eval( parse(text = idString) ),  
                                corstr = corstr,
                                data = dat ) )
  
  est = coef(mod)
  # "error" doesn't actually trigger an error, but is basically a warning
  gee.error.code = mod$error
  corstr.final = corstr  # will be changed below if there was a warning
  
  # ~ If there was a warning, try a different corstr --------------------------
  if ( gee.error.code != 0 ) {
    
    # possible correlation structures to try
    corstrs = c("exchangeable", "independence")
    # switch to whichever we haven't tried yet
    new.corstr = corstrs[ corstrs != corstr ]
    
    mod  = suppressMessges( gee( eval( parse(text = formulaString) ),
                                 id = eval( parse(text = idString) ),  
                                 corstr = new.corstr,
                                 data = dat ) )
    
    est = coef(mod)
    gee.error.code = mod$error
    corstr.final = new.corstr
  }
  
  # ~ Get Mancl-corrected SEs --------------------------
  # critical: because of the silly way GEE.var.md handles the id variable (visible if you
  #  run it in debug mode, in the very first step), the id variable must ALSO be put in the dataframe
  #  like this, as a factor, to avoid the initial part of GEE.var.md that puts the id variable back in the dataframe
  if ( se.type == "mancl" ) {
    if ( idString == "as.factor(site)" ) {
      dat$id = as.factor(dat$site)
    } else if ( idString == "as.factor(uid)" ){
      dat$id = as.factor(dat$uid)
    } else {
      stop("idString not recognized in part of code that does Mancl SEs")
    }
    
    
    # this is prone to being computationally singular
    tryCatch({
      message("Trying Mancl SEs")
      
      # this fn ONLY returns the variance estimate, not the coeffs
      SEs.only = GEE.var.md( eval( parse(text = formulaString) ), 
                             data = dat,  
                             id = id,  # DON'T CHANGE TO ANOTHER VARIABLE NAME; SEE NOTE ABOVE
                             corstr = corstr)
      se = sqrt(SEs.only$cov.beta)
      lo = est - qnorm(.975) * se
      hi = est + qnorm(.975) * se
      Z = abs( est / se )
      # @consider using t-dist
      pval = 2 * ( c(1) - pnorm(as.numeric(Z)) )
    }, error = function(err) {
      
      warning("**There was a problem with GEE.var.md!")
      se <<- NA
      lo <<- NA
      hi <<- NA
      Z <<- NA
      pval <<- NA
    })
  }
  
  # ~ Get model-based robust SEs --------------------------
  if ( se.type == "model" ) {
    summ = summary(mod)
    est = coef(mod)
    se = summ$coefficients[,"Robust S.E."]
    lo = coef(mod) - qnorm(.975) * se
    hi = coef(mod) + qnorm(.975) * se
    Z = as.numeric( summ$coefficients[,"Robust z"] )
    pval = 2 * ( c(1) - pnorm(abs(Z)) )
  }
  
  # ~ Sanity check --------------------------
  
  # compare naive model-based SEs to the main ones (which are either Mancl or robust)
  se.naive = summ$coefficients[,"Naive S.E."]
  diff = abs( as.numeric(se) - as.numeric(se.naive) )
  
  if ( max(diff) > 0.01 ) {
    warning( paste("\n***Some SEs differed by more than 0.01 from robust ones. Biggest abs difference was ", round(max(diff), digits ) ) )
    #browser()
  }
  
  # ~ Organize results --------------------------
  res = data.frame( analysis = analysisLabel,
                    variable = names(est),
                    est = est, 
                    se = se,  # robust one
                    lo = lo,
                    hi = hi,
                    pval = pval,
                    n.analyzed = nrow(dat),
                    geeErrorCode = gee.error.code,
                    corstrFinal = corstr.final,
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
analyze_one_outcome = function( dat.cc = d,
                                dats.imp = imps,
                                
                                missMethod,
                                yName,
                                formulaString,
                                subsetString = NA,
                                
                                idString = "as.factor(site)",
                                se.type = "model",  # "model" or "mancl"
                                
                                corstr = "exchangeable",
                                
                                bonferroni.alpha = NA,
                                
                                analysisVarNames, # for handling missing data in report_gee_table
                                analysisLabel,
                                .results.dir = NA) {
  
  
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
    mi.res = lapply( dats.imp, function(.d) report_gee_table(dat = .d,
                                                             formulaString = formulaString,
                                                             idString = idString,
                                                             subsetString = subsetString,
                                                             se.type = se.type,
                                                             analysisVarNames = analysisVarNames,
                                                             analysisLabel = analysisLabel,
                                                             corstr = corstr,
                                                             write.dir = NA) )
  }
  
  if ( missMethod == "CC" ) {
    # format as list, exactly like MI, so that it can be passed to mi_pool_all
    mi.res = list( report_gee_table(dat = dat.cc,
                                    formulaString = formulaString,
                                    idString = idString,
                                    subsetString = subsetString,
                                    se.type = se.type,
                                    analysisVarNames = analysisVarNames,
                                    analysisLabel = analysisLabel,
                                    corstr = corstr,
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
                                   var.name = row.names(res.raw) ) %>%
    add_column(  formula.string = formulaString,
                 id.string = idString, 
                 se.type = se.type )
  
  digits = 2
  res.nice = data.frame( analysis = res.raw$analysis,
                         var.name = row.names(res.raw),
                         est = stat_CI( round(res.raw$est, digits),
                                        round(res.raw$lo, digits),
                                        round(res.raw$hi, digits) ),
                         
                         se = round(res.raw$se, digits),
                         
                         pval = format.pval(res.raw$pval, eps  = 0.0001),
                         bonferroni.signif = res.raw$pval < bonferroni.alpha )
  
  
  if (!is.na(.results.dir)) {
    setwd(.results.dir)
    
    if (missMethod == "CC") missingString = "completeCase"
    if (missMethod == "MI") missingString = "multImp"
    
    string = paste( analysisLabel, yName, missingString, "_gee_table_raw", ".csv", sep="_" )
    write_csv(res.raw, string)
    
    string = paste( analysisLabel, yName, missingString, "_gee_table_pretty", ".csv", sep="_" )
    write_csv(res.nice, string)
  }
  
  return(list(res.raw = res.raw, res.nice = res.nice))
  
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
  } )
  
  # yields dataset
  .res = do.call( what = rbind, temp )
  row.names(.res) = row.names(.mi.res[[1]])
  
  
  # # add Bonferroni p-values, counting only the effect modifiers 
  # #  using the fact that their names have colons
  # modNames = row.names(.res)[ grepl( pattern = ":", x = row.names(.res) ) ]
  # nMods = length(modNames)
  # .res$pvalBonf = NA
  # .res$pvalBonf[ row.names(.res) %in% modNames ] = pmin( 1, .res$pval[ row.names(.res) %in% modNames ] * nMods )
  
  # only makes sense if there's only one "imputation" (e.g., CC analysis)
  geeErrorCodes = unlist( lapply( .mi.res, function(j) j$geeErrorCode[1] ) )
  .res$geeErrorCode = paste(geeErrorCodes, collapse = " ")
  
  # similar for character string about corstr
  corstrFinals = unlist( lapply( .mi.res, function(j) j$corstrFinal[1] ) )
  .res$corstrFinal = paste(corstrFinals, collapse = " ")
  
  
  return(.res)
}

# make a string for estimate and CI
stat_CI = function(est, lo, hi){
  paste( est, " [", lo, ", ", hi, "]", sep = "" )
}
# stat_CI( c(.5, -.1), c(.3, -.2), c(.7, .0) )





# just for sanity checks

# for all coefs in regression, organize estimates and robust inference
#  into dataframe
my_ols_hc_all = function(dat, ols, yName, hc.type = "HC0"){
  
  coefNames = as.list( names( ols$coefficients) )
  temp = lapply( coefNames, function(.coefName) {
    my_ols_hc(coefName = .coefName, dat = dat, ols = ols, yName = yName, hc.type = hc.type)
  } )
  
  # yields dataset
  do.call( what = rbind, temp )
}

# coefName: which coefficient to report
# dat: dataset (needed to calculate Hedges' g)
# ols: the OLS model with all the effect modifiers
# yName: outcome
my_ols_hc = function( coefName, dat, ols, yName, hc.type = "HC0" ){
  
  dat$Y = dat[[yName]]
  
  ( se.ols = sqrt( vcov(ols)[coefName, coefName] ) )
  ( bhat.ols = coef(ols)[coefName] )
  
  # heteroskedasticity-consistent robust SEs:
  (se.hc0 = sqrt( vcovHC( ols, type=hc.type)[coefName, coefName] ) )
  
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


# marginal HC1 SE
# for plotting
marginal_hc_se = function(vec, hc.type = "HC1") {
  dat = data.frame(Y = vec)
  ols = lm(vec ~ 1, data = dat)
  se.hc = sqrt( vcovHC( ols, type=hc.type)["(Intercept)", "(Intercept)"] ) 
  return(se.hc)
}






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
  
  
  return(t)
}


# fit GEE with a given model formula and organize results nicely
report_gee_table = function(dat,
                      formulaString,
                      analysisLabel,
                      write.dir = NA){
  
  #@add correction in Sebastien paper
  mod = geeglm( eval( parse(text = formulaString) ),
                id = uid,  #@ participant unique ID
                family = gaussian,
                corstr = "exchangeable",
                data = dat )
  
  est = coef(mod)
  lo = coef(mod) - qnorm(.975) * summary(mod)$geese$mean$san.se
  hi = coef(mod) + qnorm(.975) * summary(mod)$geese$mean$san.se
  Z = abs( coef(mod) / summary(mod)$geese$mean$san.se ) 
  
  # @consider using t-dist
  pval = 2 * ( c(1) - pnorm(as.numeric(Z)) )
  
  res = data.frame( analysis = analysisLabel,
                    variable = names(est),
                    est = est, 
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




#bm


# SAPB-E
# write_geem = function(my.gee,
#                       take.exp = FALSE,
#                       name,
#                       section) {
#   print( summary(my.gee) )
#   
#   est = coef(my.gee)
#   lo = coef(my.gee) - qnorm(.975) * summary(my.gee)$se.robust
#   hi = coef(my.gee) + qnorm(.975) * summary(my.gee)$se.robust
#   Z = abs( coef(my.gee) / summary(my.gee)$se.robust ) 
#   pval = 2 * ( c(1) - pnorm(as.numeric(Z)) )
#   
#   if (take.exp == TRUE) {
#     est = exp(est)
#     lo = exp(lo)
#     hi = exp(hi)
#   }
  



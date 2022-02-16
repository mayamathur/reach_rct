
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


# Read in imputations  --------------------------------
# read in the imputations as a list rather than a mids object so that we can pool manually
setwd(imputed.data.dir)
to.read = list.files()[ grepl( pattern = "prepped", x = list.files() ) ]
imps <<- lapply( to.read,
                 function(x) suppressMessages(read_csv(x)) )


names(imps[[1]])
mean( is.na(imps[[1]]$T2_TRIM) )

# Scramble treatment variable for blinding, if needed  --------------------------------

if ( scramble.treat == TRUE ) {
  
  d$treat = sample(d$treat, replace = TRUE)
  
  for (i in 1:M) {
    imps[[i]]$treat = sample(imps[[i]]$treat, replace = TRUE)
  
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


# SET 1 GEE MODELS -----------------------------------------------------------


# - GEE of primary and secondary Y's ~ treat + site (Bonferroni for secondaries)

missMethodsToRun = "CC"

missMethodsToRun = c("CC", "MI")

#@need to add Bonferronis
for ( .y in primYNames ) {
  
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
                         analysisVarNames = c(.fullYName, "treat", "site"),
                         analysisLabel = "set1",
                         #@SETTING "INDEPENDENCE" HERE TO AVOID FITTING 2 MODELS, 
                         # BUT EVENTUALLY SHOULD USE "EXCH" TO SHOW THAT THEY NEVER FIT; 
                         # I.E., REPORT_GEE_TABLE WILL AUTOMATICALLY SWITCH TO IND
                         corstr = "independence",
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


ols = lm( T2_TRIM ~ treat + site, data = d )
summary(ols)  # model-based SEs (might be wrong)
# example: SE for South Africa  = 0.04 (similar for other sites)
# for treat: 0.03

# now with HC0 SEs
res = my_ols_hc0_all( dat = d, ols = ols, yName = "treat" )
# SE for South Africa nearly the same
# for treat: 0.04

# in main GEE model:
# SE for South Africa: 8 e-4!!!
# SE for treat: 0.04

# also try LMM
library(lme4)

lmm = lmer( T2_TRIM ~ treat + site + (1|site),
            data = d )

summary(lmm)
# SE for South Africa: 0.27!!! (much bigger than either OLS or GEE)
# SE for treat: 0.03
# issue with LMM: non-normal outcomes


#bm: choice of model doesn't seem to affect treat estimate or SE, 
#  but very much affects SEs for sites by orders of magnitude
# I think we need the FEs by site because of stratified randomization

# on having both fixed and REs for same variable:
# https://stats.stackexchange.com/questions/263194/does-it-make-sense-to-include-a-factor-as-both-fixed-and-random-factor-in-a-line

#bm: Next think about whether we need GEE in the first place. Look for my HK slide deck?

# rationale in slide deck:
# Parzen et al. (1998). Does clustering affect the usual test statistics of no treatment effect in a randomized clinical trial?
# 
# The point of the GEE model here is to flexibly account for correlated observations between and possibly also within sites.


# SET 2 GEE MODELS -----------------------------------------------------------


# GEE of primary Y's ~ treat*T1_TFS(binary) + site

#missMethodsToRun = "CC"

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
                         analysisVarNames = c(.fullYName, "treat", "site", "T1_high_TrFS"),
                         analysisLabel = "set2",
                         corstr = "independence",
                         .results.dir = .results.dir )
    
    
  }
}





# SET 3 GEE MODELS -----------------------------------------------------------


# - GEE of primary and secondary Y's ~ treat + site + age + sex + all baseline primY

#missMethodsToRun = "CC"

for ( .y in c(primYNames, secYNames) ) {
  
  .fullYName = paste("T2_", .y, sep = "")
  .formulaString = paste("T2_", .y, " ~ treat + site + age + gender + T1_BSI + T1_TRIM", sep = "" )
  
  
  for ( .missMethod in missMethodsToRun ) {
    
    if (.missMethod == "MI") missingString = "Multiple imputation"
    if (.missMethod == "CC") missingString = "Complete-case"
    
    .results.dir = paste( results.dir, "/Analysis set 3/", missingString, sep = "" )
    
    analyze_one_outcome( missMethod = .missMethod,
                         yName = .y,
                         formulaString = .formulaString,
                         analysisVarNames = c(.fullYName, "treat", "site", "age", "gender", "T1_BSI", "T1_TRIM"),
                         analysisLabel = "set3",
                         corstr = "independence",
                         .results.dir = .results.dir )
    
    
  }
}


# DEBUG GEE -----------------------------------------------------------


### Outcome BSI
# independent working structure: gee WORKS; Mancl FAILS
res = analyze_one_outcome( missMethod = "CC",
                           yName = "BSIdep",
                           formulaString = "T2_BSIdep ~ treat + site",
                           analysisVarNames = c("T2_BSIdep", "treat", "site"),
                           analysisLabel = "set1",
                           
                           corstr = "independence",
                           
                           .results.dir = NA )
res$res.raw

# exch working structure: gee WARNS; Mancl WORKS
# "Working correlation estimate not positive definite"
res = analyze_one_outcome( missMethod = "CC",
                           yName = "BSI",
                           formulaString = "T2_BSI ~ treat + site",
                           analysisVarNames = c("T2_BSI", "treat", "site"),
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















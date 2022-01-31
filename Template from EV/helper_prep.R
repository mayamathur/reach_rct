################################ MISCELLANEOUS FORMATTING AND CONVENIENCE FNS ################################

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


########################### FN: RECODE VARIABLES DURING DATA PREP ###########################

# turns a vector of Qualtrics completion times (e.g., "8/26/20 7:38") into date objects
dateify = function(x) {
  # need to split on space because there are also times in the strings
  x2 = strsplit(x, " ")
  # keep only the first part (date)
  x3 = unlist( lapply(x, function(.x) .x[[1]]) )
  as.Date(x3, "%m/%d/%y")
}
# dateify("8/26/20 7:38")
# dateify("12/26/20 7:38")

# recodes a Qualtrics checkbox question (i.e., a single column with comma-separated options)
#  into its consituent non-mutually-exclusive dummy variables
recode_checkboxes = function( .d, 
                              var ) {
  
  # split race into dummies
  # https://stackoverflow.com/questions/27630588/split-string-column-to-create-new-binary-columns/27630769#27630769
  t = mtabulate( strsplit( .d[[var]], ",") )
  
  # remove the old variable and replace with the new one
  .d = .d %>% select(-var)
  .d = cbind(.d, t)
  
  return(.d)
}
# sanity check:
# dat = data.frame( race = c("", "midEast", "natAm,SAsian,EAsian,SEAsian" ) )
# recode_checkboxes( .d = dat, var = "race" )


# also removes the stateCounty variable for deidentification
# see study1_or_3_data_prep.R for sanity checks (search name of this fn)
make_derived_vars = function(.d,
                             # variable lists
                             .meats = meats,
                             .animProds = animProds,
                             .goodPlant = goodPlant,
                             
                             printCorMat = FALSE  # print subscale correlation matrices for psych vars?
                             ){
  
  # recode food variables
  for ( i in allFoods){
    .d = recode_food_Y(.d = .d,
                       food = i)
  }
  
  # secondary outcome: total meat (total ounces over the week)
  # rowSums defaults to NOT removing NAs, so NA in ANY meat should cause 
  #  total to be NA as well
  .d$totalMeat = rowSums( .d[, .meats] )
  
  # sanity check
  # express in pounds of meat
  hist(.d$totalMeat/16)
  
  # secondary outcome: total animal products (total ounces over the week)
  .d$totalAnimProd = rowSums( .d[, .animProds] )
  
  # primary outcome: total meat + animal product consumption
  .d$mainY = .d$totalMeat + .d$totalAnimProd

  # frequency-only version for sensitivity analysis
  vars = c("chicken_Freq",
           "turkey_Freq",
           "fish_Freq",
           "pork_Freq",
           "beef_Freq",
           "otherMeat_Freq",
           "dairy_Freq",
           "eggs_Freq")
  expect_equal(TRUE, all(vars %in% names(.d)) )
  .d$mainYFreqOnly = rowSums( .d[,vars] ) 
  
  # secondary: total good plant-based foods
  .d$totalGood = rowSums( .d[, .goodPlant])
  
  hist(.d$totalGood/16)
  

  # recode secondary psych scales
  .d = recode_psych_scale( .d = .d,
                           scale = "spec",
                           revCode = "X5_spec",
                           printCorMat = printCorMat )

  .d = recode_psych_scale( .d = .d,
                           scale = "activ",
                           revCode = NA,
                           printCorMat = printCorMat)
  
  .d = recode_psych_scale( .d = .d,
                           scale = "dom",
                           revCode = c("X3_dom",
                                       "X4_dom",
                                       "X7_dom",
                                       "X8_dom"),
                           printCorMat = printCorMat )


  # recode awareness
  .d$aware = (.d$guessPurpose == "g.meatAnimProd") & (.d$guessPurpose2 == "c.decrease")
  
  
  ##### Simplify effect modifiers #####
  .d$female = NA; .d$female[ .d$sex == "a.Female" ] = 1; .d$female[ .d$sex == "b.Male" ] = 0  # NA for those marking "Other"
  
  .d$young = (.d$age <= 25)
  
  # political party: group Independents and "Other/I don't know" people under "neutral" category
  .d$party2 = NA
  .d$party2[ .d$party == "Independent" ] = "b.Neutral"
  .d$party2[ .d$party == "Other/I don't know" ] = "b.Neutral"
  .d$party2[ .d$party == "Democrat" ] = "c.Democrat"
  .d$party2[ .d$party == "Republican" ] = "a.Republican"
  
  # pDem in terms of a 10-percentage point increase in Democrats
  .d$pDem2 = .d$pDem/0.1
  
  .d$collegeGrad = .d$educ %in% c("c.2yr", "d.4yr", "e.post")
  
  # only for Study 3: the simplified joint effect modifier
  if ( study == 3 ) .d$targetDemoSimple = (.d$party == "Democrat") & (.d$highEduc == TRUE)
  
  # remove county variables to deidentify
  if ( "stateCounty" %in% names(.d) ) .d = .d %>% select(-stateCounty)
  if ( "county" %in% names(.d) ) .d = .d %>% select(-county)
  
  ##### Misc #####
  # these vars only exist for study 1 and 2
  if ( "w1.date" %in% names(.d) ) {
    # turn Qualtrics completion times into date objects
    .d$w1.date = dateify(.d$w1.date)
    .d$w2.date = dateify(.d$w2.date)
    
    # calculate follow-up time for each subject
    .d$fuDays = .d$w2.date - .d$w1.date
  }

  return(.d)
}





# yName: just the root of the string, e.g., "dairy"
# NOTE: overwrites the frequency and amount variables in the returned dataset
# see study1_or_3_data_prep.R for sanity checks (search name of this fn)
recode_food_Y = function(.d,
                         food){
  
  # # test only
  # .d = d
  # food = "legumes"
  
  freqString = paste(food, "Freq", sep = "_")
  amountString = paste(food, "Ounces", sep = "_")
  
  # duplicate data just to facilitate sanity checks
  .d2 = .d
  
  # overwrite old frequency variable
  # recode as servings per week
  .d2[[freqString]] = dplyr::recode( .d2[ , freqString ],
                                     a.Never = 0,
                                     b.1Weekly = 1,
                                     c.2Weekly = 2,
                                     d.3to4Weekly = 3.5,
                                     e.5to6Weekly = 5.5,
                                     f.1Daily = 7,
                                     g.2PlusDaily = 14 ) 
  
  # # sanity check
  table( .d2[[freqString]], .d[[freqString]], useNA = "ifany" )
  
  # recode ounces variable
  # when they answered "never" to frequency question, ounces question
  #  was not asked, so will be NA
  .d2[[ amountString ]] = as.numeric( as.character(.d2[[ amountString ]]) )
  .d2[[ amountString ]][ .d2[[freqString]] == 0 ] = 0
  
  # # sanity check
  # # when freqString is zero, amountString should always be 0
  # # and when freqString is more than 0, amountString should be more than 0
  # .d2 %>% group_by( get(freqString) == 0, get(amountString) ) %>%
  #   summarise(n())
  

  # make the ounces-per-week variable
  .d2[, food] = as.numeric( as.character(.d2[[freqString]]) ) * as.numeric( as.character(.d2[[amountString]]) )
  
  return(.d2)
}

# overwrites subscales with reverse-coded versions and creates a new *standardized* composite
#  variable from the subscales' sum
#
# scale: part of scale string that appears in each subscale's variable name (e.g., "spec" for speciesism); also becomes the name of the new composite variable
# revCode: quoted names of any subscales that need to be reverse-coded
# see study1_or_3_data_prep.R for sanity checks (search name of this fn)
recode_psych_scale = function(.d,
                              scale, 
                              revCode = NA,
                              printCorMat = TRUE) {

  # duplicate dataset just for ease of sanity-checking
  .d2 = .d
  
  subscales = names(.d2)[ grepl( x = names(.d2), pattern = scale ) ]
  
  # make numeric instead of factor
  .d2 = .d2 %>% mutate_at( subscales, function(x) as.numeric( as.character(x) ) )

  # sanity check
  # correlation matrix prior to reverse-coding
  if (printCorMat == TRUE){
    library(corrr)
    corrs = .d2 %>% select(subscales) %>%
      correlate( use = "pairwise.complete.obs" )
    
    print( paste( "Reverse-coded: ", paste(revCode, collapse = ", "), sep = "" ) )
    print(corrs)
  }
  
  # just use negative values for any scales that need reverse-coding
  if ( !any( is.na(revCode) ) ) .d2[ , revCode] = -.d2[ , revCode]
  
  head( .d2 %>% select(subscales) )
  
  # make new variable whose name is just the root
  # new variable is mean by subject of the subscales
  .d2[, scale ] = rowMeans( .d2[ , subscales] )
  
  hist( .d2[, scale ]  )
  
  # standardize the new variable
  .d2[, scale ] = ( .d2[, scale ] - mean( .d2[, scale ], na.rm = TRUE ) ) / sd( .d2[, scale ], na.rm = TRUE )
  
  return(.d2)
}

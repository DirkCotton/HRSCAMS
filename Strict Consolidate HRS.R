
# This script reads HRS data from a .csv speadsheet, "rand_0207_with nonretirees.csv".
# Next, it eliminates all households consisting of three or more HRS respondents.  
# the script them combines all two-person households into a single row, such that
# each row of HRS then identifies a single household made up of one or two HRS
# respondents. It then identifies retired households based on our model rules. 
# Next the script collects wealth and income from
# the HRS data set based on the wave of retirement previsouly identified for that household.
# For two-person households, the houshold is considered retired when the first respondent
# is determined to have retired. Lastly, a new consildated (by household) .csv file is
# created and named, "~/desktop/HRS Retired.csv".

# This script consolidates HRS data similarly to 'Consolidate HRS Household Data.R' but with a significantly
# more strict definition of "retired household." Specifically,

# 1. A household is considered retired only if one or more members of that household claim full retirement or, if none
#    claims retirement, then from labor force data 
#    designates one or more members retired.
#
# 2. Missing retirement data for the wave just prior to the wave in which retirement is established by rule #1
#    renders the age of retirement unclear. For instance, if a houshold member claims retirement in Wave 4
#    but retirement status data for wave 3 is missing, it is possible that the respondent was actually
#    retired in wave 3, throwing off the retirement date by as much as 4 years. (Some HRS data
#    has as many as three prior waves of missing data.) When retirement status data is unavailable
#    for the wave just prior to the wave retirement is identified by rule #1, that household is
#    not included in the stricter-definition data set. 
#        NOTE: Retirement claimed in wave 1 is accepted even though there is no previous wave to confirm it.
#        NOTE: Prior wave confirmation is not required for labor force status data
#
#  3. In the absence of of claimed retirement data and labor force data, Social Security retirement income
#    is considered an indication of retirement under the looser definition. In this stricter
#    regime, beginning Social Security retirment income will only be considered an indication
#    of retirement if, as in step 2, Social Security income data is not missing from the prior
#    wave AND the number of hours worked is reported and declines at least 30% from the pre-retirement wave.
#    
#

# The steps to clean the HRS data files are as follows:
# 
# 1. Load the HRS file

hrs <- read.csv("rand_0207_with nonretirees.csv",stringsAsFactors = F)

# 
# 2. Collect the following data for all individuals (hhidpn's):
# 
#   a. the earliest wave number for which the individual claims complete retirement (or zero if absent)
#   b. the claim status for the wave prior to the one found in step a.
#   c. the earliest wave number for which the individual's labor force data shows complete retirement
#   d. the labor force status data for the wave prior to the one found in step c.
#   d. the number of hours worked for each wave
#   e. Social Security income for each wave
#   f. Number of rows (persons) in each household.
#   
# 3. Remove all rows for households with more than two persons (rows). These households are rejected from our datbase.
#   
# 4. Determine the retirement status for EACH INDIVIDUAL as defined by using data collected in step 2.
# 
# 5. Consolidate 2-person household retirement status.
# 
# 6. Retrieve data for one-person households based on retirement wave determined in Step 4.
# 
# 7. Retrieve data for two-person households based on retirement wave consolidated in Step 5.
# 
# 8. Consolidate all one- and two-person households in a data frame of households
# 
# 9. Select the fields from Step 8 needed for retirement simulations.

# function waveNum returns the index of the first TRUE value in a vector or Zero if none TRUE.

waveNum <- function(logicalV) {
  waveNumber <- 0
  for (ind in 1:length(logicalV)) {
    if (logicalV[ind] == TRUE) {
      waveNumber <- ind
      break
    }
  }
  return(waveNumber)
}

hrs$hhid <- round(hrs$hhidpn/100)  # add column of hhids
cat("\nHRS data contains ",dim(hrs)[1]," indiviual hhidpns with ",length(unique(hrs$hhid))," unique households")

tabhh.df <- data.frame(table(hrs$hhid))
cat("\nNumber of households by number of members (1 to 6) in those households:\n")
print(table(tabhh.df$Freq))

# find first wave for each individual that claimed complete retirement or labor force data says completely retired
lop <- dim(hrs)[1] # rows HRS
first.M <- data.frame(hrs$hhidpn,"sayRetComp"=rep(0,lop),"sayRetPart"=rep(0,lop),"lbrfComp"=rep(0,lop),"firstSSIncome"=rep(0,lop),"workedNG10"=rep(0,lop))

# find first wave that individual claims complete retirement but reject of prior wave data is missing

sayRet.V <- grep("r.*sayret",colnames(hrs))
aTest <- data.frame(hrs[,sayRet.V])
bTest <- aTest == "1.completely retired"
bTest <- data.frame(bTest)
bTestfirst <- rep(0,dim(bTest)[1])
for (i in 1:dim(bTest)[1]) {
  lvec <- bTest[i,]
  wave <- waveNum(lvec)
  bTestfirst[i] <- wave
  if (wave > 1) {
    if (aTest[i,wave - 1] != "0.not retired" & aTest[i,wave - 1] != "2.partly retired") bTestfirst[i] <- 0
  }
}
# bTestfirst[bTestfirst == 1 & !bTest$r1sayret] <- 0
first.M$sayRetComp <- bTestfirst


# stricter regime ignores when individual claims partial retirement
first.M$sayRetPart <- NA

# first wave individual labor force data says retired

lbrf.V <- grep("r.*lbrf",colnames(hrs))
aTest <- data.frame(hrs[,lbrf.V])
bTest <- aTest == "5.retired"
bTest <- data.frame(bTest)
bTestfirst <- rep(0,dim(bTest)[1])
for (i in 1:dim(bTest)[1]) {
  lvec <- bTest[i,]
  wave <- waveNum(lvec)
  bTestfirst[i] <- wave
}

# bTestfirst[bTestfirst == 1 & !bTest$r1lbrf] <- 0
first.M$lbrfComp <- bTestfirst

# first wave with Social Security Income
wealthSSincome <- c("r1isret","r2isret","r3isret","r4isret","r5isret","r6isret","r7isret","r8isret","r9isret","r10isret","r11isret","r12isret")
wealthSSincome.V <- match(wealthSSincome,colnames(hrs))

aTest <- data.frame(hrs[,wealthSSincome.V])
aTest[is.na(aTest)] <- 0
bTest <- aTest > 0
bTest <- data.frame(bTest)
lTest <- data.frame(hrs[,lbrf.V])  # get labor force data again

bTestfirst <- rep(0,dim(bTest)[1])
for (i in 1:dim(bTest)[1]) {
  lvec <- bTest[i,]
  wave <- waveNum(lvec)
  bTestfirst[i] <- wave
  if (wave == 0) {
  } else {
    if (wave > 1 & lTest[i,wave] == "1.works ft") bTestfirst[i] <- 0  # if works full time, not retired
  }
}
  
first.M$firstSSIncome <- bTestfirst

# append first.M columns to HRS
hrs2 <- cbind(hrs,first.M)

# oneTwoPersonHHS <- head(hrs$hhidpn[hrs$hhid %in% notDupHhids.V])

# separate one-person households
notDupHhids.V <- hrs2$hhid[!(duplicated(hrs2$hhid) | duplicated(hrs2$hhid, fromLast=TRUE))]
onePerHHidpns <- hrs2[hrs2$hhid %in% notDupHhids.V,]
cat ("\nSelected ",dim(onePerHHidpns)[1]," one-person households.")

# separate two-person households

dupTab <- data.frame (table(hrs2$hhid[duplicated(hrs2$hhid) | duplicated(hrs2$hhid, fromLast=TRUE)]))
dupTab <- dupTab[dupTab$Freq == 2,]
twoPerHHidpns <- hrs2[hrs2$hhid %in% dupTab$Var1,]
cat ("\nSelected ",dim(twoPerHHidpns)[1]," two-person hhidpns representing ",dim(twoPerHHidpns)[1]/2," households.")

# split two-person household into two arrays of invidivuals with the same hhid.
firstSpouse <- twoPerHHidpns[duplicated(twoPerHHidpns$hhid),]
secondSpouse <- twoPerHHidpns[!duplicated(twoPerHHidpns$hhid),]

# find first wave worked <= 10 hours
# 
# rjhours <- firstSpouse[,match("r1jhours",colnames(firstSpouse)):match("r12jhours",colnames(firstSpouse))]
# rjhours[is.na(rjhours)] <- 0
# rjhours[rjhours > 10] <- 0
# 
# firstSpouse$workedNG10 <- rep(0,dim(firstSpouse)[1])
# for (i in 1:dim(firstSpouse)[1]) {
#   if (firstSpouse$sayRetComp[i] == 0 & firstSpouse$lbrfComp[i] == 0) {
#     firstSpouse$workedNG10[i] <- which(rjhours[i,] != 0)[1]
#     if (is.na(firstSpouse$workedNG10[i])) firstSpouse$workedNG10[i] <- 0
#   }
# }
# # for second spouse
# rjhours <- secondSpouse[,match("r1jhours",colnames(secondSpouse)):match("r12jhours",colnames(secondSpouse))]
# rjhours[is.na(rjhours)] <- 0
# rjhours[rjhours > 10] <- 0
# 
# secondSpouse$workedNG10 <- rep(0,dim(secondSpouse)[1])
# for (i in 1:dim(secondSpouse)[1]) {
#   if (secondSpouse$sayRetComp[i] == 0 & secondSpouse$lbrfComp[i] == 0) {
#     secondSpouse$workedNG10[i] <- which(rjhours[i,] != 0)[1]
#     if (is.na(secondSpouse$workedNG10[i])) secondSpouse$workedNG10[i] <- 0
#   }
# }
 
############################
# set the earliest wave for retirement for first spouse = that of second spouse if the latter is smaller
# now earliest wave number for the first spouse is the earliest of the two individuals
############################
firstSpouse$sayRetComp[firstSpouse$sayRetComp == 0] <- 13  # temporarily change zeros to a larger number
secondSpouse$sayRetComp[secondSpouse$sayRetComp == 0] <- 13
firstSpouse$sayRetComp <- pmin(firstSpouse$sayRetComp,secondSpouse$sayRetComp)  # reset say-complete wave number for first spouse if second spouse's is smaller
firstSpouse$sayRetComp[firstSpouse$sayRetComp == 13] <- 0  # change back to zero
secondSpouse$sayRetComp[secondSpouse$sayRetComp == 13] <- 0

firstSpouse$sayRetPart <- NA   # not used for strict consolidation

firstSpouse$lbrfComp[firstSpouse$lbrfComp == 0] <- 13  # temporarily change zeros to a larger number
secondSpouse$lbrfComp[secondSpouse$lbrfComp == 0] <- 13
firstSpouse$lbrfComp <- pmin(firstSpouse$lbrfComp,secondSpouse$lbrfComp)  # reset labor force-complete wave number for first spouse if second spouse's is smaller
firstSpouse$lbrfComp[firstSpouse$lbrfComp == 13] <- 0  # change back to zero
secondSpouse$lbrfComp[secondSpouse$lbrfComp == 13] <- 0

firstSpouse$firstSSIncome[firstSpouse$firstSSIncome == 0] <- 13  # temporarily change zeros to a larger number
secondSpouse$firstSSIncome[secondSpouse$firstSSIncome == 0] <- 13
firstSpouse$firstSSIncome <- pmin(firstSpouse$firstSSIncome,secondSpouse$firstSSIncome)  # reset first SS Income wave number for first spouse if second spouse's is smaller
firstSpouse$firstSSIncome[firstSpouse$firstSSIncome == 13] <- 0  # change back to zero
secondSpouse$firstSSIncome[secondSpouse$firstSSIncome == 13] <- 0

firstSpouse$workedNG10 <- NA  # reset first SS Income wave number for first spouse if second spouse's is smaller

############################
# Determine retirement date for each two-person household by selecting earliest individual to retire
############################

firstSpouse$earliestRetire <- 0    # zero both these columns
secondSpouse$earliestRetire <- rep(0,length(firstSpouse$earliestRetire))
firstSpouse$selectedBy <- secondSpouse$earliestRetire

# firstGroup <- rep("0",length(firstSpouse$earliestRetire)) # store group number
# secondGroup <- rep(0,length(firstSpouse$earliestRetire)) # store group number

firstSpouse$earliestRetire[firstSpouse$sayRetComp > 0] <- firstSpouse$sayRetComp [firstSpouse$sayRetComp > 0]
firstSpouse$selectedBy[firstSpouse$sayRetComp > 0] <- "sayComplete" # group 1 say complete

firstSpouse$earliestRetire[firstSpouse$sayRetComp == 0 & firstSpouse$lbrfComp > 0] <- firstSpouse$lbrfComp [firstSpouse$sayRetComp == 0 & firstSpouse$lbrfComp > 0]
firstSpouse$selectedBy[firstSpouse$sayRetComp == 0 & firstSpouse$lbrfComp > 0] <- "sayLbrf"    # lbrf data

firstSpouse$earliestRetire[firstSpouse$sayRetComp == 0 & firstSpouse$lbrfComp == 0 & firstSpouse$firstSSIncome > 0] <- firstSpouse$firstSSIncome [firstSpouse$sayRetComp == 0 & firstSpouse$lbrfComp == 0 & firstSpouse$firstSSIncome > 0]
firstSpouse$selectedBy[firstSpouse$sayRetComp == 0 & firstSpouse$lbrfComp == 0 & firstSpouse$firstSSIncome > 0] <- "ssIncome"  # Social Security Income

# firstSpouse$earliestRetire[firstSpouse$sayRetComp == 0 & firstSpouse$lbrfComp == 0 & firstSpouse$workedNG10 > 0] <- firstSpouse$workedNG10 [firstSpouse$sayRetComp == 0 &  firstSpouse$lbrfComp == 0 & firstSpouse$workedNG10 > 0]
# firstGroup[firstSpouse$sayRetComp == 0 & firstSpouse$lbrfComp == 0 & firstSpouse$workedNG10 > 0] <- "workedHrs" # worked no more than 10 hours
# firstSpouse$selectedBy[firstSpouse$sayRetComp == 0 & firstSpouse$lbrfComp == 0 & firstSpouse$firstSSIncome > 0] <- "0"


# Get data that requires retirement age be known
wealthArles <- c("h1arles","h2arles","h3arles","h4arles","h5arles","h6arles","h7arles","h8arles","h9arles","h10arles","h11arles","h12arles")
wealthArles.V <- match(wealthArles,colnames(hrs2))
# business owned``
wealthAbsns <- c("h1absns","h2absns","h3absns","h4absns","h5absns","h6absns","h7absns","h8absns","h9absns","h10absns","h11absns","h12absns")
wealthAbsns.V <- match(wealthAbsns,colnames(hrs2))
# IRA, Keough value
wealthAira <- c("h1aira","h2aira","h3aira","h4aira","h5aira","h6aira","h7aira","h8aira","h9aira","h10aira","h11aira","h12aira")
wealthAira.V <- match(wealthAira,colnames(hrs2))
# stock value
wealthAstck <- c("h1astck","h2astck","h3astck","h4astck","h5astck","h6astck","h7astck","h8astck","h9astck","h10astck","h11astck","h12astck")
wealthAstck.V <- match(wealthAstck,colnames(hrs2))
# checking account values
wealthAchck <- c("h1achck","h2achck","h3achck","h4achck","h5achck","h6achck","h7achck","h8achck","h9achck","h10achck","h11achck","h12achck")
wealthAchck.V <- match(wealthAchck,colnames(hrs2))
# CD values
wealthAcd <- c("h1acd","h2acd","h3acd","h4acd","h5acd","h6acd","h7acd","h8acd","h9acd","h10acd","h11acd","h12acd")
wealthAcd.V <- match(wealthAcd,colnames(hrs2))
# Bond and bond fund
wealthAbond <- c("h1abond","h2abond","h3abond","h4abond","h5abond","h6abond","h7abond","h8abond","h9abond","h10abond","h11abond","h12abond")
wealthAbond.V <- match(wealthAbond,colnames(hrs2))
# other assets
wealthAothr <- c("h1aothr","h2aothr","h3aothr","h4aothr","h5aothr","h6aothr","h7aothr","h8aothr","h9aothr","h10aothr","h11aothr","h12aothr")
wealthAothr.V <- match(wealthAothr,colnames(hrs2))
# debt
wealthAdebt <- c("h1adebt","h2adebt","h3adebt","h4adebt","h5adebt","h6adebt","h7adebt","h8adebt","h9adebt","h10adebt","h11adebt","h12adebt")
wealthAdebt.V <- match(wealthAdebt,colnames(hrs2))

# wealthhrsWorked <- c("r1jhours","r2jhours","r3jhours","r4jhours","r5jhours","r6jhours","r7jhours","r8jhours","r9jhours","r10jhours","r11jhours","r12jhours")

wealthPension <- c("r1ipena","r2ipena","r3ipena","r4ipena","r5ipena","r6ipena","r7ipena","r8ipena","r9ipena","r10ipena","r11ipena","r12ipena")
wealthPension.V <- match(wealthPension,colnames(hrs2))

wealthSSincome <- c("r1isret","r2isret","r3isret","r4isret","r5isret","r6isret","r7isret","r8isret","r9isret","r10isret","r11isret","r12isret")
wealthSSincome.V <- match(wealthSSincome,colnames(hrs2))

wealthPaycheck <- c("r1iearn","r2iearn","r3iearn","r4iearn","r5iearn","r6iearn","r7iearn","r8iearn","r9iearn","r10iearn","r11iearn","r12iearn")
wealthPaycheck.V <- match(wealthPaycheck,colnames(hrs2))

wealthHomeEquity <- c("h1atoth","h2atoth","h3atoth","h4atoth","h5atoth","h6atoth","h7atoth","h8atoth","h9atoth","h10atoth","h11atoth","h12atoth")
wealthHomeEquity.V <- match(wealthHomeEquity,colnames(hrs2))

########################################################################


wealthAgeAtRetire <- c("r1agey_b","r2agey_b","r3agey_b","r4agey_b","r5agey_b","r6agey_b","r7agey_b","r8agey_b","r9agey_b","r10agey_b","r11agey_b","r12agey_b")
wealthAgeAtRetire.V <- match(wealthAgeAtRetire,colnames(hrs2))

ssIncome <- c("r1isret","r2isret","r3isret","r4isret","r5isret","r6isret","r7isret","r8isret","r9isret","r10isret","r11isret","r12isret")
ssIncome.V <- match(ssIncome,colnames(hrs2))

#########################################
# Collect wealth data for one-person households
#########################################

# additional columns to add to both Spouses of two-income households
twoHHdata <- data.frame(matrix(0,dim(twoPerHHidpns)[1] / 2,26))
colnames(twoHHdata) <- c("hhid","hhidpn1","hhidpn2","persons","gender1","gender2","DOB1","DOB2","ssIncAtRetire1","ssIncAtRetire2","earliestRetire","ageAtRetirement1","ageAtRetirement2","realEstate","stocks","checking","CDs","business","IRAKeough","bonds","otherAssets","debt","portfolioAssetsTotal","paycheck","pensionIncome","homeEquity")
twoHHdata$persons <- 2
twoHHdata$gender1 <- firstSpouse$ragender
twoHHdata$gender2 <- secondSpouse$ragender
twoHHdata$DOB1 <- firstSpouse$rabyear
twoHHdata$DOB2 <- secondSpouse$rabyear
twoHHdata$earliestRetire <- firstSpouse$earliestRetire
twoHHdata$hhid <- firstSpouse$hhid
twoHHdata$hhidpn1 <- firstSpouse$hhidpn
twoHHdata$hhidpn2 <- secondSpouse$hhidpn
firstSpouse[is.na(firstSpouse)] <- 0

for (i in 1:dim(firstSpouse)[1]) {
  if (firstSpouse$earliestRetire[i] == 0) {
    twoHHdata$ageAtRetirement1[i] <- 0
    twoHHdata$ageAtRetirement2[i] <- 0
    twoHHdata$ssIncAtRetire1[i] <- 0
    twoHHdata$ssIncAtRetire2[i] <- 0
    twoHHdata$realEstate[i] <- 0
    twoHHdata$stocks[i] <- 0
    twoHHdata$checking[i] <- 0
    twoHHdata$CDs[i] <- 0
    twoHHdata$business[i] <- 0
    twoHHdata$IRAKeough[i] <- 0
    twoHHdata$bonds[i] <- 0
    twoHHdata$otherAssets[i] <- 0
    twoHHdata$debt[i] <- 0
    twoHHdata$portfolioAssetsTotal[i] <- 0
    twoHHdata$paycheck[i] <- 0
    twoHHdata$pensionIncome[i] <- 0
    twoHHdata$homeEquity[i] <- 0
    
  } else {
    
    twoHHdata$ageAtRetirement1[i] <-  firstSpouse[i,wealthAgeAtRetire.V[firstSpouse$earliestRetire[i]]]
    twoHHdata$ageAtRetirement2[i] <-  secondSpouse[i,wealthAgeAtRetire.V[firstSpouse$earliestRetire[i]]]
    twoHHdata$ssIncAtRetire1[i] <-  firstSpouse[i,ssIncome.V[firstSpouse$earliestRetire[i]]]
    twoHHdata$ssIncAtRetire2[i] <-  secondSpouse[i,ssIncome.V[firstSpouse$earliestRetire[i]]]
    twoHHdata$realEstate[i] <- firstSpouse [i,wealthArles.V[firstSpouse$earliestRetire[i]]]
    twoHHdata$stocks[i] <- firstSpouse [i,wealthAstck.V[firstSpouse$earliestRetire[i]]]
    twoHHdata$checking[i] <- firstSpouse [i,wealthAchck.V[firstSpouse$earliestRetire[i]]]
    twoHHdata$CDs[i] <- firstSpouse [i,wealthAcd.V[firstSpouse$earliestRetire[i]]]
    twoHHdata$business[i] <- firstSpouse [i,wealthAbsns.V[firstSpouse$earliestRetire[i]]]
    twoHHdata$IRAKeough[i] <- firstSpouse [i,wealthAira.V[firstSpouse$earliestRetire[i]]]
    twoHHdata$bonds[i] <- firstSpouse [i,wealthAbond.V[firstSpouse$earliestRetire[i]]]
    twoHHdata$otherAssets[i] <- firstSpouse [i,wealthAothr.V[firstSpouse$earliestRetire[i]]]
    twoHHdata$debt[i] <- firstSpouse [i,wealthAdebt.V[firstSpouse$earliestRetire[i]]]
    twoHHdata$portfolioAssetsTotal[i] <- twoHHdata$realEstate[i] + twoHHdata$stocks[i] + twoHHdata$checking[i] + twoHHdata$CDs[i] + twoHHdata$IRAKeough[i] + twoHHdata$bonds[i] + twoHHdata$otherAssets[i] - twoHHdata$debt[i] 
    twoHHdata$homeEquity[i] <- firstSpouse [i,wealthHomeEquity.V[firstSpouse$earliestRetire[i]]]
        
    if (secondSpouse$earliestRetire[i] > 0) {
      twoHHdata$paycheck[i] <- firstSpouse [i,wealthPaycheck.V[firstSpouse$earliestRetire[i]]] + secondSpouse [i,wealthPaycheck.V[secondSpouse$earliestRetire[i]]]
      twoHHdata$pensionIncome[i] <- firstSpouse [i,wealthPension.V[firstSpouse$earliestRetire[i]]] + secondSpouse [i,wealthPension.V[secondSpouse$earliestRetire[i]]]
    } else{
      twoHHdata$paycheck[i] <- firstSpouse [i,wealthPaycheck.V[firstSpouse$earliestRetire[i]]] 
      twoHHdata$pensionIncome[i] <- firstSpouse [i,wealthPension.V[firstSpouse$earliestRetire[i]]]
      
    }
  }
}

############################
# Determine retirement date for each one-person household by selecting earliest retirement definition
############################

onePerHHidpns$workedNG10 <- rep(NA,dim(onePerHHidpns)[1])

onePerHHidpns$earliestRetire[onePerHHidpns$sayRetComp > 0] <- onePerHHidpns$sayRetComp [onePerHHidpns$sayRetComp > 0]
onePerHHidpns$selectedBy[onePerHHidpns$sayRetComp > 0] <- "sayComplete" # group 1 say complete

onePerHHidpns$earliestRetire[onePerHHidpns$sayRetComp == 0 & onePerHHidpns$lbrfComp > 0] <- onePerHHidpns$lbrfComp [onePerHHidpns$sayRetComp == 0 & onePerHHidpns$lbrfComp > 0]
onePerHHidpns$selectedBy[onePerHHidpns$sayRetComp == 0 & onePerHHidpns$lbrfComp > 0] <- "sayLbrf"    # lbrf data

onePerHHidpns$earliestRetire[onePerHHidpns$sayRetComp == 0 & onePerHHidpns$lbrfComp == 0 & onePerHHidpns$firstSSIncome > 0] <- onePerHHidpns$firstSSIncome [onePerHHidpns$sayRetComp == 0& onePerHHidpns$lbrfComp == 0  & onePerHHidpns$firstSSIncome > 0]
onePerHHidpns$selectedBy[onePerHHidpns$sayRetComp == 0 & onePerHHidpns$lbrfComp == 0 & onePerHHidpns$firstSSIncome > 0] <- "ssIncome"  # Social Security Income

# onePerHHidpns$earliestRetire[onePerHHidpns$sayRetComp == 0 & onePerHHidpns$lbrfComp == 0 & onePerHHidpns$workedNG10 > 0] <- onePerHHidpns$workedNG10 [onePerHHidpns$sayRetComp == 0 & onePerHHidpns$lbrfComp == 0 & onePerHHidpns$workedNG10 > 0]
# group1P[onePerHHidpns$sayRetComp == 0 & onePerHHidpns$lbrfComp == 0 & onePerHHidpns$workedNG10 > 0] <- "workedHrs" # worked no more than 10 hours

onePerHHidpns$selectedBy[onePerHHidpns$sayRetComp == 0 & onePerHHidpns$lbrfComp == 0 & onePerHHidpns$firstSSIncome == 0] <- "0"

onePerHHidpns$earliestRetire[is.na(onePerHHidpns$earliestRetire)] <- 0

#########################################
# Collect wealth data for one-person households
#########################################

oneHHdata <- data.frame(matrix(0,dim(onePerHHidpns[1]),26))
colnames(oneHHdata) <- colnames(twoHHdata)
oneHHdata$hhid <- onePerHHidpns$hhid
oneHHdata$hhidpn1 <- onePerHHidpns$hhidpn
oneHHdata$hhidpn2 <- NA
oneHHdata$persons <- 1
oneHHdata$gender1 <- onePerHHidpns$ragender
oneHHdata$gender2 <- NA
oneHHdata$DOB1 <- onePerHHidpns$rabyear
oneHHdata$DOB2 <- NA
oneHHdata$earliestRetire <- onePerHHidpns$earliestRetire
oneHHdata$ageAtRetirement2 <- NA
oneHHdata$ssIncAtRetire2 <- NA

for (i in 1:dim(onePerHHidpns)[1]) {
  if (onePerHHidpns$earliestRetire[i] == 0) {
    oneHHdata$ageAtRetirement1[i] <- 0
    oneHHdata$ssIncAtRetire1[i] <- 0
    oneHHdata$ageAtRetirement2[i] <- 0
    oneHHdata$ssIncAtRetire1[i] <- 0
    oneHHdata$ssIncAtRetire2[i] <- 0
    oneHHdata$realEstate[i] <- 0
    oneHHdata$stocks[i] <- 0
    oneHHdata$checking[i] <- 0
    oneHHdata$CDs[i] <- 0
    oneHHdata$business[i] <- 0
    oneHHdata$IRAKeough[i] <- 0
    oneHHdata$bonds[i] <- 0
    oneHHdata$otherAssets[i] <- 0
    oneHHdata$debt[i] <- 0
    oneHHdata$portfolioAssetsTotal[i] <- 0
    oneHHdata$paycheck[i] <- 0
    oneHHdata$pensionIncome[i] <- 0
    oneHHdata$homeEquity[1] <- NA
    
  } else {
    
    oneHHdata$ageAtRetirement1[i] <-  onePerHHidpns[i,wealthAgeAtRetire.V[onePerHHidpns$earliestRetire[i]]]
    oneHHdata$ssIncAtRetire1[i] <-  onePerHHidpns[i,ssIncome.V[onePerHHidpns$earliestRetire[i]]]
    oneHHdata$realEstate[i] <- onePerHHidpns [i,wealthArles.V[onePerHHidpns$earliestRetire[i]]]
    oneHHdata$stocks[i] <- onePerHHidpns [i,wealthAstck.V[onePerHHidpns$earliestRetire[i]]]
    oneHHdata$checking[i] <- onePerHHidpns [i,wealthAchck.V[onePerHHidpns$earliestRetire[i]]]
    oneHHdata$CDs[i] <- onePerHHidpns [i,wealthAcd.V[onePerHHidpns$earliestRetire[i]]]
    oneHHdata$business[i] <- onePerHHidpns [i,wealthAbsns.V[onePerHHidpns$earliestRetire[i]]]
    oneHHdata$IRAKeough[i] <- onePerHHidpns [i,wealthAira.V[onePerHHidpns$earliestRetire[i]]]
    oneHHdata$bonds[i] <- onePerHHidpns [i,wealthAbond.V[onePerHHidpns$earliestRetire[i]]]
    oneHHdata$otherAssets[i] <- onePerHHidpns [i,wealthAothr.V[onePerHHidpns$earliestRetire[i]]]
    oneHHdata$debt[i] <- onePerHHidpns [i,wealthAdebt.V[onePerHHidpns$earliestRetire[i]]]
    oneHHdata$portfolioAssetsTotal[i] <- oneHHdata$realEstate[i] + oneHHdata$stocks[i] + oneHHdata$checking[i] + oneHHdata$CDs[i] +  oneHHdata$IRAKeough[i] + oneHHdata$bonds[i] + oneHHdata$otherAssets[i] - oneHHdata$debt[i] 
    oneHHdata$homeEquity[i] <- onePerHHidpns [i,wealthHomeEquity.V[onePerHHidpns$earliestRetire[i]]]
    oneHHdata$paycheck[i] <- onePerHHidpns [i,wealthPaycheck.V[onePerHHidpns$earliestRetire[i]]] 
    oneHHdata$pensionIncome[i] <- onePerHHidpns [i,wealthPension.V[onePerHHidpns$earliestRetire[i]]]
    
  }
}


twoHHdata[is.na(twoHHdata)] <- 0  # get rid of NAs
oneHHdata[is.na(oneHHdata)] <- 0  # get rid of NAs

onePerHHidpns$selectedBy[onePerHHidpns$selectedBy == 0] <- "notRetired"
firstSpouse$selectedBy[firstSpouse$selectedBy == 0] <- "notRetired"

onePerHHidpns$selectedBy[is.na(onePerHHidpns$selectedBy)] <- "notRetired"
firstSpouse$selectedBy[is.na(firstSpouse$selectedBy)] <- "notRetired"


# create Excel .csv file of retired households data

oneHHdata$selectedBy <- onePerHHidpns$selectedBy  # add column to show how retirment was defined
twoHHdata$selectedBy <- firstSpouse$selectedBy

# oneHHdata <- round(oneHHdata)
# twoHHdata <- round(twoHHdata)

bothHHdata <- rbind(oneHHdata,twoHHdata)
bothHHdata <- bothHHdata[order(bothHHdata$hhid) , ]

# if age at retirement is missing, calculate using date of birth and year of the calculated retirement wave

yrOfWave <- seq(1992,2014,2)  # wave 1 data collected in 1992; wave 12 in 2015. Biannual interviews.
for (n in 1:(dim(bothHHdata)[1])) {
  if (bothHHdata$ageAtRetirement1[n] == 0 & bothHHdata$DOB1[n] > 0 & bothHHdata$earliestRetire[n] > 0) bothHHdata$ageAtRetirement1[n] <- yrOfWave[bothHHdata$earliestRetire[n]] - bothHHdata$DOB1[n]
  if (bothHHdata$persons[n] == 2 & bothHHdata$ageAtRetirement2[n] == 0 & bothHHdata$DOB2[n] > 0 & bothHHdata$earliestRetire[n] > 0) bothHHdata$ageAtRetirement2[n] <- yrOfWave[bothHHdata$earliestRetire[n]] - bothHHdata$DOB2[n]
}

bothHHdataStrict <- bothHHdata[bothHHdata$selectedBy != "notRetired",]

write.csv(bothHHdata,"~/desktop/HRS Retired.csv")

cat ("\nHouseholds are identified as Not Retired, Claimed Complete Retirement, Labor Force Data Indicates Retired, or Social Security Income Indicates Retired.")
cat ("\nFor two-person households, the selection criteria is that of the spouse to retire first.")

print(table(bothHHdata$selectedBy))

     
     # save.image("~/Desktop/HRSData.RData") 


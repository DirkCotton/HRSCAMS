
# Run "Strict Consolidate HRS.R" first.


# The steps to clean the CAMS data files are as follows:
# 
# 1. HRS data 

cams <- read.csv("~/dropbox/Robo Research/Highlighted CAMS Data Set.csv",stringsAsFactors = F)

# 
# 2. Collect the following data for all individuals (hhidpn's):
# 
#   a. the earliest wave number for which the individual claims complete retirement (or zero if absent)
#   b. the earliest wave number for which individuals who never claimed complete retirement claim partial retirement only (or zero if absent)
#   c. the earliest wave number for which the individual's labor force data shows complete retirement
#   d. the number of hours worked for each wave
#   e. Social Security income for each wave
#   f. Number of rows (persons) in each household.
#   
# 3. Remove all rows for households with more than two persons (rows). These households are rejected form our datbase.
#   
# 4. Determine the retirement status for EACH INDIVIDUAL as defined by us using data collected in step 2.
# 
# 5. Consolidate 2-person household retirement status.
# 
# 6. Retrieve data for one-person households based on retirement wave determined in Step 4.
# 
# 7. Retrieve data for two-person households based on retirement wave consolidated in Step 5.
# 
# 8. Consolidate all one- and two-person households in a data frame of households
# 
# 9. Select the fields from Step 8 needed for retirment simulations.

cat("\nCAMS data contains ",dim(cams)[1]," individual hhidpns with ",length(unique(cams$hhid))," unique households")

tabhh.df <- data.frame(table(cams$hhid))
 
cat("\nNumber of households by number of members (1 to",max(tabhh.df$Freq),") in those households:\n")
print(table(tabhh.df$Freq))

# delete all rows of CAMS for households with more than 2 persons

tabhh2.df <- tabhh.df[tabhh.df$Freq <= 2,]
cams <- cams[cams$hhid %in% tabhh2.df$Var1,]

# Consolidate spending data for observations with duplicate HHIDs in CAMS
dupHHID <- which(duplicated(cams$hhid))
dupHHID2 <- dupHHID - 1  # point to the row that is duplicated 

cat ("\n",length(dupHHID)," CAMS households were duplicate observations (2-person households) and were consolidated.")

# consolidate data from 2-person househlds into a single HHID

if (length(dupHHID > 0)) {
  for (j in 1:length(dupHHID)) {
    cams[dupHHID2[j],match("h5cdurs",colnames(cams)):match("h11ctotc",colnames(cams))] <- colSums(cams[dupHHID2[j]:dupHHID[j],match("h5cdurs",colnames(cams)):match("h11ctotc",colnames(cams))])
  }
}

cams <- cams[!seq_len(nrow(cams)) %in% dupHHID, ]  # remove the duplicated household row

# attach column of earliest retire date from larger HRS consolidated households in bothHHdata

# create a data frame of only hhid, hwcdurs,hwcndur and hwctotc fomd CAMS data frame for join
cdurs <- colnames(cams[grepl("cdurs$",colnames(cams))])
cdurs.V <- match(cdurs,colnames(cams))
ndurs <-  colnames(cams[grepl("cndur$",colnames(cams))])
ndurs.V <- match(ndurs,colnames(cams))
consump <- colnames(cams[grepl("ctotc$",colnames(cams))])
consump.V <- match(consump,colnames(cams))
allCols <- c(cdurs.V,ndurs.V,consump.V)

camsJoin.df <- cams[,allCols]
hhid <- cams$hhid
camsJoin.df <- cbind(hhid,camsJoin.df)
camsJoin.df[is.na(camsJoin.df)] <- 0

camsJoined.df <- suppressMessages(inner_join(bothHHdata,camsJoin.df))

cat("\nFor the ",length(unique(bothHHdata$hhid)),"unique hhid's in our HRS data, ",sum(bothHHdata$selectedBy != "notRetired")," households were retired." ) 

cat("\nFor the ",sum(bothHHdata$selectedBy != "notRetired"),"retired households in our HRS data, ",length(unique(camsJoined.df$hhid[camsJoined.df$selectedBy != "notRetired"]))," households were also found in the CAMS data." ) 

cat("\nOf these ",length(unique(camsJoined.df$hhid[camsJoined.df$selectedBy != "notRetired"]))," households, ",length(unique(camsJoined.df$hhid[camsJoined.df$earliestRetire >= 5 & camsJoined.df$selectedBy != "notRetired"])) ," retired in or after wave 5, so have CAMS data.")


# find expenses (or consumption) for wave of retirement. If wave of retirement outside range
# of expense data availble (waves 6-12), set to NA.

camsJoined.df$cdursWOR <- rep(0,dim(camsJoined.df)[1]) # column for cdurs wave OF retirement
camsJoined.df$cdursWBR <- rep(0,dim(camsJoined.df)[1]) # column for cdurs wave BEFORE retirement

camsJoined.df$cndurWOR <- rep(0,dim(camsJoined.df)[1]) # column for cndur wave OF retirement
camsJoined.df$cndurWBR <- rep(0,dim(camsJoined.df)[1]) # column for cndur wave BEFORE retirement
camsJoined.df$cdurs2B4 <- rep(0,dim(camsJoined.df)[1]) # column for cndurs TWO waves BEFORE retirement

camsJoined.df$ctotcWOR <- rep(0,dim(camsJoined.df)[1]) # column for ctotc wave OF retirement
camsJoined.df$ctotcWBR <- rep(0,dim(camsJoined.df)[1]) # column for ctotcs wave BEFORE retirement


i <- match("h5cdurs",colnames(camsJoined.df)) # find first column number of cdurs
i2 <- match("h5cndur",colnames(camsJoined.df))
i3 <- match("h5ctotc",colnames(camsJoined.df))

for (k in 1:dim(camsJoined.df)[1])
  # extract expense data for the wave of retirement if available, otherwise set to NA
  if (camsJoined.df$earliestRetire[k] >= 5 & camsJoined.df$earliestRetire[k] <= 12) {
    camsJoined.df$cdursWOR[k] <- camsJoined.df[k,i + camsJoined.df$earliestRetire[k] - 5]
    camsJoined.df$cndurWOR[k] <- camsJoined.df[k,i2 + camsJoined.df$earliestRetire[k] - 5]
    camsJoined.df$ctotcWOR[k] <- camsJoined.df[k,i3 + camsJoined.df$earliestRetire[k] - 5]
    
    # if data is available for the wave BEFORE retirement, extract that, too
    if (camsJoined.df$earliestRetire[k] > 5){
      camsJoined.df$cdursWBR[k] <- camsJoined.df[k,i + camsJoined.df$earliestRetire[k] - 6]
      camsJoined.df$cndurWBR[k] <- camsJoined.df[k,i2 + camsJoined.df$earliestRetire[k] - 6]
      # if wave >6, save cndur for TWO waves before retirement
      camsJoined.df$cndur2B4[k] <- ifelse (camsJoined.df$earliestRetire[k] > 6,camsJoined.df[k,i2 + camsJoined.df$earliestRetire[k] - 7],NA)
      camsJoined.df$ctotcWBR[k] <- camsJoined.df[k,i3 + camsJoined.df$earliestRetire[k] - 6]
      
    } else {     # if no data for previous wave, set to NA
      # camsJoined.df$cdursWBR[k] <- NA
      # camsJoined.df$cndurWBR[k] <- NA
      # camsJoined.df$ctotcWBR[k] <- NA
    }
  } else {   # if no data for this wave, set columns = NA
    # camsJoined.df$cdursWOR[k] <- NA
    # camsJoined.df$cndurWOR[k] <- NA
    # camsJoined.df$ctotcWOR[k] <- NA
    # camsJoined.df$cdursWBR[k] <- NA
    # camsJoined.df$cndurWBR[k] <- NA
    # camsJoined.df$ctotcWBR[k] <- NA
  }

cdursNotNA <- camsJoined.df$cdursWOR
cdursNotNA[is.na(cdursNotNA)] <- 0
cat("\nOf these ",length(unique(camsJoined.df$hhid[camsJoined.df$earliestRetire >= 5 & camsJoined.df$selectedBy != "notRetired"]))," households, ",sum(camsJoined.df$selectedBy != "notRetired" & cdursNotNA > 0)," have CAMS consumer durables expenditures data.")

ctotcNotNA <- camsJoined.df$ctotcWOR
ctotcNotNA[is.na(ctotcNotNA)] <- 0
cat("\nOf these ",length(unique(camsJoined.df$hhid[camsJoined.df$earliestRetire >= 5 & camsJoined.df$selectedBy != "notRetired"]))," households, ",sum(camsJoined.df$selectedBy != "notRetired" & ctotcNotNA > 0)," have CAMS household consumption data.")

cndurNotNA <- camsJoined.df$cndurWOR
cndurNotNA[is.na(cndurNotNA)] <- 0
cat("\n\nOf these ",length(unique(camsJoined.df$hhid[camsJoined.df$earliestRetire >= 5 & camsJoined.df$selectedBy != "notRetired"]))," households, ",sum(camsJoined.df$selectedBy != "notRetired" & cndurNotNA > 0)," have CAMS consumer non-durables expenditures data and associated HRS data.")
cat("\nThese make up our HRS/CAMS data set.")

# remove individual wave data, leaving just data for wave of retirement
fullCAMSCols <- colnames(camsJoined.df)
omitsCols <- c(grep("h\\d",fullCAMSCols),grep("cdursWOR",fullCAMSCols),grep("cdursWBR",fullCAMSCols),grep("ctotcWOR",fullCAMSCols),grep("ctotcWBR",fullCAMSCols) )

camsJoined.df <- camsJoined.df[,-omitsCols]
write.csv(camsJoined.df,"CAMS and HRS data.csv")

save(camsJoined.df,file="CAMSdataSave.RData")
# save.image("~/Desktop/CAMSData.RData") 






  
  

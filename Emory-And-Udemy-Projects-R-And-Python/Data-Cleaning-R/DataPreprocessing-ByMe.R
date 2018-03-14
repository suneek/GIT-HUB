
########################
# DATA CLEANING WITH R #
########################

###############
# Start Clock #
###############
ptm <- proc.time()

#################################
# Installing Required libraries #
#################################
if("lubridate" %in% rownames(installed.packages())== FALSE) {install.packages("lubridate")}

#################################
#  Unpacking Required libraries #
#################################
library(lubridate)

#Setting Working Directory
setwd("C:/Users/svemu/OneDrive/Suneetha_Docs/EMORY_Big_Data/R")

# Reading Raw Data
RawData <- read.csv("RawData.csv",stringsAsFactors=FALSE)

# What if we have 2MM rows? (10,000 replica of original dataset)
RawData <- RawData[rep(seq_len(nrow(RawData)),1000),]

###################################################
# Steps for: Raw Data --> Technically Correct Data #
###################################################

# Print structure of Raw Data
str(RawData)

# 1) Checking Data Type Violations

# 1-A) Checking Identity Column (Date), detecting values that are not date
Date_I <- as.Date(RawData$Date,format="%m/%d/%Y") #Note that date format is dicated by "%m/%d/%Y"
RawData[c(is.na(Date_I)),] #Print rows if date column violation exsists

# Determining logic: If value next to date column value is date, shift each value to left
#                    If both 1st & 2nd column values are not date replace date cell with "Delete" 

TC_Data1 <- RawData
  
for (i in 1:length(RawData$Date)){   
      if(!is.na(try(as.Date(RawData[i,2],format="%m/%d/%Y"),silent=TRUE)))        
        {
        for (j in 1:(length(RawData)-1)){
        TC_Data1[i,j] <- RawData[i,j+1]
        }
      }      
      if(is.na(try(as.Date(RawData[i,1],format="%m/%d/%Y"),silent=TRUE)) 
         & is.na(try(as.Date(RawData[i,2],format="%m/%d/%Y"),silent=TRUE))){
        TC_Data1[i,1] <- "Delete"
      }
      
      else{next}       
}  

# Select rows if value is not "Delete"

TC_Data2 <- TC_Data1[TC_Data1$Date!="Delete",]

# 1-B) Checking Data Type Validation: TRUE = Validated FALSE = Violation

DTV_Impressions <- sapply(as.numeric(TC_Data2[,2]),is.finite)
DTV_Clicks <- sapply(as.numeric(TC_Data2[,3]),is.finite)
DTV_Cost <- sapply(as.numeric(TC_Data2[,4]),is.finite)
DTV_AvgPos <- sapply(as.numeric(TC_Data2[,5]),is.finite)
DTV_Conversions <- sapply(as.numeric(TC_Data2[,6]),is.finite)
DTV_Revenue <- sapply(as.numeric(TC_Data2[,7]),is.finite)

TC_Data3 <- data.frame(DTV_Impressions,DTV_Clicks,DTV_Cost,DTV_AvgPos,DTV_Conversions,DTV_Revenue,TC_Data2)

# 1-c) Checking Invalid Observation (Domain Violations): TRUE = Validated FALSE = Violation

VO_Impressions <- TC_Data3$Impressions >= 0
VO_Clicks <- TC_Data3$Clicks >= 0
VO_Cost <- TC_Data3$Cost >= 0
VO_AvgPos <- TC_Data3$AvgPos >= 1 & TC_Data3$AvgPos <= 10
VO_Conversions <- TC_Data3$Conversions >= 0
VO_Revenue <- TC_Data3$Revenue >= 0


# Creating Technically Correct Data
# No Data Type Validations No Invalid Observations

TC_Impressions <- ifelse(DTV_Impressions&VO_Impressions,TC_Data3$Impressions,NA)
TC_Clicks <- ifelse(DTV_Clicks&VO_Clicks,TC_Data3$Clicks,NA)
TC_Cost <- ifelse(DTV_Cost&VO_Cost,TC_Data3$Cost,NA)
TC_AvgPos <- ifelse(DTV_AvgPos&VO_AvgPos,TC_Data3$AvgPos,NA)
TC_Conversions <- ifelse(DTV_Conversions&VO_Conversions,TC_Data3$Conversions,NA)
TC_Revenue <- ifelse(DTV_Revenue&VO_Revenue,TC_Data3$Revenue,NA)

TechnicallyCorrectData <- data.frame(TC_Data3$Date,TC_Impressions,TC_Clicks,TC_Cost,TC_AvgPos,TC_Conversions,TC_Revenue)

#write.csv(TechnicallyCorrectData,"TechnicallyCorrectData.csv")

###########################################################
# Steps for: Technically Correct Data --> Consistent Data #
###########################################################

# Removing Duplicates (Entire row carried as duplicate not one value)

CD_1 <- unique(TechnicallyCorrectData)

# Removing Obvious inconsistencies (Impressions>Clicks is a must)

OI <- as.numeric(as.character(CD_1$TC_Impressions)) < as.numeric(as.character(CD_1$TC_Clicks))
OI[is.na(OI)] <- FALSE
CD_2 <- CD_1[!OI,]

# Removing Valid Observation Outliers (Removing in field observations that are ~ 3 standard deviations away from mean)
# IQR Interquartile Range (Q3-Q1)


remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 4 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}

CD_Impressions <- ifelse(remove_outliers(as.numeric(as.character(CD_2$TC_Impressions))) > 0,as.numeric(as.character(CD_2$TC_Impressions)),NA)
CD_Clicks <- ifelse(remove_outliers(as.numeric(as.character(CD_2$TC_Clicks))) > 0,as.numeric(as.character(CD_2$TC_Clicks)),NA)
CD_Cost <- ifelse(remove_outliers(as.numeric(as.character(CD_2$TC_Cost))) > 0,as.numeric(as.character(CD_2$TC_Cost)),NA)
CD_AvgPos <- ifelse(remove_outliers(as.numeric(as.character(CD_2$TC_AvgPos))) > 0,as.numeric(as.character(CD_2$TC_AvgPos)),NA)
CD_Conversions <- ifelse(remove_outliers(as.numeric(as.character(CD_2$TC_Conversions))) > 0,as.numeric(as.character(CD_2$TC_Conversions)),NA)
CD_Revenue <- ifelse(remove_outliers(as.numeric(as.character(CD_2$TC_Revenue))) > 0,as.numeric(as.character(CD_2$TC_Revenue)),NA)


ConsistentData <- data.frame(CD_2$TC_Data3.Date,CD_Impressions,CD_Clicks,CD_Cost,CD_AvgPos,CD_Conversions,CD_Revenue)

#write.csv(ConsistentData,"ConsistentData.csv")

##############
# Stop Clock #
##############
proc.time() - ptm
minutes_elapsed = (proc.time()[3] - ptm[3])/60
minutes_elapsed




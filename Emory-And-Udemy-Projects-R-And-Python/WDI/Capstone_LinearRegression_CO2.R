########################################
# R CAPSTONE PROJECT Based on WDI Data # 
# Author: Suneetha Vemuri - Reused code from Erinc               #
########################################
########################################################################
# Setting Working Directory                                            #
# Rule of thumb: Keep R Script & flat data files in the same directory #
########################################################################
setwd("C:/Users/svemu/OneDrive/Suneetha_Docs/EMORY_Big_Data/CAPSTONE/R_CAPSTONE")

################################################################################
# 1-Linear Regression Brush-up                                                 #
################################################################################
############################################################
# Examples based on: Elements of Econometrics - Jan Kmenta #
############################################################
# 1- Relationship Between Variables
# Characteristic Eqn: y = A + Bx

# A deterministic relationship
Price <- seq(from=1,to=50,by=1)
Quantity <- 100 - 2*Price
plot(Price,Quantity, main ="A Deterministic Relation",cex=1)

# Now consider introduction of random disturbance!
E_plus <- Quantity+5
E_minus <- Quantity-5
plot(Price,Quantity, main ="A Stochastic Relation",cex=2)
points(E_plus,col="dark red",pch=19,cex=1)
points(E_minus,col="dark red",pch=19,cex=1)

# One Possible Realization
Error <- NA
for(i in Price){
p <- sum(rbinom(n=2,size=1,prob=0.5))
Error[i] <- ifelse(p==1,0,ifelse(p==0,-5,5))
}

# Re-write the equation including random disturbance (error term)
Quantity <- 100 - 2*Price + Error
plot(Price,Quantity, main ="One Possible Realization",cex=2)

# In modeling we deal with stochastic relations. The simplest form of stochastic
# realation between two variables x & y is "Simple Linear Regression Model"
# of the form --> Y_i = A + B*X_i + E_i

# 2-Simple Linear Regression Model
# Can be time-series
# Can be cross-section (groups of individuals, objects, geographic areas)

# Basic Assumptions
# 1-Normality: E_i is normally distributed
# 2-Zero Mean: E[E_i] = 0
# 3-Homoskedasticity: Var(E_i) = Sigma^2 (Constant)
# 4-Nonautocorrelation: Cov(E_i,E_j) = 0 (for i<>j)
# 5-Nonstochastic X: X is a nonstochastic variable with values fixed in repeated
# samples

# Understanding difference between Population vs. Sample Regression Lines
Price <- seq(from=1,to=50,by=1)
Quantity_Population <- 100 - 2*Price
Quantity_Sample <- 80 - 1.5*Price
plot(Price,Quantity_Population,main="Population vs. Sample",
     type="l",ylab="Quantity")
lines(Price,Quantity_Sample,col="dark red")
points(20,90,pch=19,cex=1)
# Remove all varibles / clear Global Environment
rm(list=ls())

################################################################################
# 2-Linear Regression with R-Rattle                                            #
################################################################################
if("rattle"%in%rownames(installed.packages())==FALSE){install.packages("rattle")}
library(rattle)
rattle()
# We will cover "faithful" & "Boston" examples included in the Powerpoint

################################################################################
# 3-Getting Familiar with WDI Data                                             #
################################################################################

###################################################################
# Investigating Data before reading and assigning to a data frame #
###################################################################

# We can read top-n lines without creating the dataframe
readLines('WDI_Data.csv',n = 10)

# Checking line count
length(readLines('WDI_Data.csv'))

# Checking column names
columns <- readLines('WDI_Data.csv',n = 1)
dfColumns <- as.data.frame(strsplit(columns,","))
colnames(dfColumns) <- "WDI Columns"

###########################################################
# 3.1)Reading from csv file and assigning to a data frame #
###########################################################
# read.csv and read.csv2 are identical to read.table except for the defaults 
# They are intended for reading 'comma separated value' files ('.csv') or 
# (read.csv2) the variant used in countries that use a comma as decimal point 
# and a semicolon as field separator
?read.csv
df_csv <- read.csv(file='WDI_Data.csv',header=TRUE)

##################################################
# 3.2)Reading from excel file, reading all tabs! #
##################################################
if("readxl"%in%rownames(installed.packages())==FALSE){install.packages("readxl")}
library(readxl)
#######################################
# A UDF to read all excel sheets/tabs #
#######################################
read_excel_allsheets <- function(filename){
  sheets <- readxl::excel_sheets(filename)
  x<-lapply(sheets,function(X)readxl::read_excel(filename,
                                                 sheet=X,
                                                 col_names= TRUE))
  names(x)<-sheets
  x
}
##########################################
# Read all excel sheets/tabs of WDI.xlsx #
##########################################
tibble_xlsx <- read_excel_allsheets("WDI.xlsx")
# Notice that output object is a R tibble
# Read about R tibbles: http://tibble.tidyverse.org/
# We can create R objects for each tibble
Description_xlsx <- tibble_xlsx$Description
Data_xlsx <- tibble_xlsx$Data
Country_xlsx <- tibble_xlsx$Country

# We will use the data based on csv file (df_csv) for the rest of the session 
# let's remove all other objects, this is a good practice to free some space in
# memory
# below one-liner removes all objects with _xlsx in the objec name
rm(list=ls(pattern = "*_xlsx"))

# let also remove read_excel_allsheets function
rm(read_excel_allsheets)

#######################
# Using R WDI Package #
#######################
if("RJSONIO"%in%rownames(installed.packages())==FALSE){
  install.packages("RJSONIO")}
if("WDI"%in%rownames(installed.packages())==FALSE){install.packages("WDI")}
if("ggplot2"%in%rownames(installed.packages())==FALSE){
  install.packages("ggplot2")}
library(RJSONIO)
library(WDI)
library(ggplot2)

# Downloads the requested data by using the World Bank's API, 
# Parses the resulting JSON file, and formats it in long country-year format.

# Data frame with series code, name, description, and source for the WDI series 
# which match the given criteria
gdpcolnames <- WDIsearch(string = "gdp", field = "name", short = TRUE,
                         cache = NULL)


GDPperCapita_USD <- WDI(country = "all", indicator = "NY.GDP.PCAP.CD",
                        start = 2005, end = 2011, extra = FALSE, cache = NULL)

# Unique Country Nammes for this indicator
unique(GDPperCapita_USD$country)

# A comparison
subset(GDPperCapita_USD, country %in% c('United States','Germany'), 
       select=c(country,year,NY.GDP.PCAP.CD))

# Maybe a simple plot
dat <- WDI(indicator='NY.GDP.PCAP.CD', country=c('MX','CA','US'), 
          start=1960, end=2012)

ggplot(dat, aes(year, NY.GDP.PCAP.CD, color=country)) + geom_line() + 
  xlab('Year') + ylab('GDP per capita')

# Removing all objects except df_csv
rm(list=setdiff(ls(), c("df_csv","dfColumns")))

##################################################################
# 3.3)Preparing Data (Data Transformation) for Linear Regression #
##################################################################
#########################################################
# Returning back to dataframe created based on csv file #
#########################################################
df1 <- df_csv
# Unique Indicators in csv file
Indicators <- as.data.frame(unique(df1$Indicator.Name))
colnames(Indicators) <- "Indicator Name"
# These are potential variables for modeling
# Supervised Learning calls for selection of dependent variable(s)
# Let's try to predict CO2 emission for a given country
# Which Indicators has CO2 in Indicator.Name?
CO2_Indicators <- as.data.frame(
  Indicators[grep('CO2',Indicators$`Indicator Name`),1])
# Dependent Variable: CO2 Emissions in kilotons
df2 <- subset(df1, Indicator.Name %in% c('CO2 emissions (kt)'))
# Observe df2 visually - Up to which year we have observations? 2011
# Can we determine the 2011 programmatically? Yes - below is one way
# An example: Is all values in column-3 NA?
all(is.na(df2[,3]))
# Now let's run this in a for loop for all columns
# creating object to assign values - initiating
df3 <- NA
for(i in 1:length(df2)){df3[i] <- all(is.na(df2[,i]))}
df3 <- as.data.frame(df3)
colnames(df3) <- "All NA"
df4 <- cbind(dfColumns,df3)
subset(df4,df4[,2]==TRUE)

# Which country has High CO2 emmission as of 2011?
# Let's sort data High to Low based on 2011
df2 <- df2[with(df2,order(-df2$X2011,na.last = TRUE)),]
View(df2)
# Please observe that data has clusters of countries
# Among just countries, China has the highest CO2 emission in 2011
# Select all data for single country: China
# Please observe that we switched back to original data df1!
China <- subset(df1, Country.Name %in% c('China'))
# How much memory r session uses? 8GB dual core, ~600 MB, for my laptop
# At this point, if you are planning to work on a model just for China you can 
# delete all other objects to free up some memory

#####################################
# Deciding on Independent Variables #
#####################################
# We have 1420 variables (CO2 emissions (kt)--> Dependent)
# How to decide on independent variables?
# We can limit the variables based on number of valid observations
View(China)
# Current dataframe Years as columns and Indicators on rows
# Transpose dataframe so variables on columns and years on rows
# Years start at 5th column and 2011 is the 56th column
ChinaT <- data.frame(t(China[,5:56]))
colnames(ChinaT) <- China$Indicator.Name

# Determine NAs per variable(Indicator)
na_count <- sapply(ChinaT, function(y) sum(length(which(is.na(y)))))
na_count <- data.frame(na_count)

# How Many NAs for depedent variable?
na_count[rownames(na_count) == c("CO2 emissions (kt)"),]
#None
# Select variables with minimum NAs
ChinaRattle <- ChinaT[,which(na_count<1)]

# Saving Cleaned China data (as csv) for rattle
# Please note that all objects in this R Session is also avaialble in Rattle
# Creating a csv file is purely optional
write.csv(ChinaRattle,"China.csv")

##########################################
# Let's investigate China data in rattle #
##########################################
library(rattle)
rattle()

#########################
# Furhter Data Cleaning #
#########################

# 1-What type of variables we have? double/integer
# 2-What does CO2 emissions (kt) distribution look like? Based on box-plot there
# are 3 outliers more than 3/2 times of upper quartile
# 3- Choose variables with CO2 in name in rattle Data tab (Ignore all other)
# Was this an easy task? No - manual
# 4- Run correlation for variables with CO2 in name
#  What do you observe?
# There are versions of variables measuring the same phenomenon:
colnames(ChinaRattle)
# of GDP: Ratio to GDP
# LCU: Value in Local Currency Units instead of USD
# 2005: Normalized values based on constant 2005 US$
# of total: Ratio to Total
# etc.: Naming convention for versions of variables
# per.capita: Measure per capita

################################################################################
# Removing variables with "of.GDP", "LCU", "2005","of.total" in Indicator Name #
################################################################################
df5 <- subset(ChinaRattle,select = !grepl("of.GDP",colnames(ChinaRattle)))
df6 <- subset(df5,select = !grepl("LCU",colnames(df5)))
df7 <- subset(df6,select = !grepl("2005",colnames(df6)))
df8 <- subset(df7,select = !grepl("of.total",colnames(df7)))
df9 <- subset(df8,select = !grepl("etc",colnames(df8)))
df10 <- subset(df9,select = !grepl("capita",colnames(df9)))

##############################################################################
# Correlations with significance levels                                      #
# Observe that rattle correaltions cannot be filtered for dependent variable #
##############################################################################
ChinaCorr <- data.frame(cor(df10, use="pairwise.complete.obs", method="pearson")) 
# Which variables are highly correlated with dependent variable?
colnames(ChinaCorr)
HighCorrVariables <- rownames(ChinaCorr[abs(ChinaCorr$CO2.emissions..kt.)>0.50,])
# Creating dataframe for those variables
df11 <- df10[,colnames(df10)%in%HighCorrVariables] 

##############################################
# Let's investigate df11 dataframe in rattle #
##############################################
library(rattle)
rattle()

# Choose R Dataset df11 in Rattle


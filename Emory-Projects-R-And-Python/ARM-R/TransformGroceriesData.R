#################################
# Installing Required libraries #
#################################
if("arules" %in% rownames(installed.packages()) == FALSE) {install.packages("arules")}
if("dplyr" %in% rownames(installed.packages()) == FALSE) {install.packages("dplyr")}
if("tidyr" %in% rownames(installed.packages()) == FALSE) {install.packages("tidyr")}
if("datasets" %in% rownames(installed.packages()) == FALSE) {install.packages("datasets")}

#################################
#  Unpacking Required libraries #
#################################
library(arules)
library(tidyr)
library(dplyr)
library(datasets)

#########################
# Set Working Directory #
#########################
setwd("setwd("C:/Users/svemu/OneDrive/Suneetha_Docs/EMORY_Big_Data/R")")

###########################################################
# Preparing Groceries Data for Analysis dplyr & tidyr way #
###########################################################
data(Groceries) # Original data in arules package

ds <- as(Groceries, "matrix")             %>% # Convert to a matrix
  as.data.frame()                         %>% # Convert to a data frame
  mutate(basket=1:length(Groceries))      %>% # Add a basket ID column
  gather(item, present, frankfurter:bags) %>% # Pivot the table
  filter(present==1)                      %>% # Keep only the items present
  select(-present)                        %>% # Remove the present column
  arrange(basket)                             # Sort by basket ID

####################################################
# Preparing Groceries Data for Analysis Base R way #
####################################################
data(Groceries) # Original data in arules package

df1 <- as.matrix(Groceries@data) #Get Groceries data
df2 <- as.data.frame(Groceries@itemInfo) #Get Item Info
df3 <- as.data.frame(Groceries@itemsetInfo) #Get Item ID (Basket ID)
df3 <- seq(1,dim(df1)[2],1)
colnames(df1) <- df3 #Assign Basket ID as columns
rownames(df1) <- df2$labels #Assign Item Labels as rows

df4 <- t(df1) #Transpose matrix Columns --> Row
df5 <- as.data.frame(df4) #Store as data frame
df6 <- cbind(df3,df5) #Add a basket ID column

# df6 <- read.csv("GroceriesOriginal.csv",header=TRUE)

# df6 --> The csv file you received

if("reshape" %in% rownames(installed.packages()) == FALSE) {install.packages("reshape")}
library(reshape)

df7 <- melt(df6,id.vars = "df3") #Pivot the table
df8 <- subset(df7,df7$value==TRUE) #Keep only items present
df9 <- df8[,1:2] #Remove the present column
df10 <- df9[order(df9$df3),] #Sort by basket ID

colnames(df10) <- c("basket","item")

###########################################################################
# Now let's check if dplyr & Base R generated data frames are same or not #
###########################################################################
class(ds)
class(df10)
all.equal(ds,df10)

# Matching column names
colnames(df10) <- c("basket","item")
all.equal(ds,df10)

# Matching row names
rownames(df10) <- rownames(ds)
all.equal(ds,df10)

# Matching Attributes
attributes(df10) <- attributes(ds)
all.equal(ds,df10)

# Matching Data types
df10$basket <- as.numeric(df10$basket)
df10$item <- as.character(df10$item)
all.equal(ds,df10)

# Re-sorting df10 by basketID since it's now numeric
df11 <- df10[order(df10$basket),] #Sort by basket ID
all.equal(ds,df11)

# Matching row names (Each time we make this assignment R keeps previous data 
# frames rownames, hence the necessity)
rownames(df11) <- rownames(ds)
attributes(df11) <- attributes(ds)
all.equal(ds,df11)

# Yeay, finally a match

# Save data for Rattle (or leverage "ds" as an object directly)
write.csv(ds, file = "GroceriesModelReady.csv", row.names = F)

# Launch a Rattle session
if("rattle" %in% rownames(installed.packages()) == FALSE) {install.packages("rattle")}
library(rattle)
rattle()

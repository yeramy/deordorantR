library(ggplot2)
library(MASS)


#for my windows
setwd("/Users/Ye/Dropbox/Project/Deodorant")

#for my for Mac
setwd("/Users/yeram/Dropbox/Projects/Deodorant")

#Read and store the raw file
originaldata <- read.csv("rawdata.csv", header = TRUE)
write.csv(originaldata, file = "datafiles/oldfile.csv", row.names = FALSE, na = "")

#Cleaning up data for better visuals on what is what,and only storing what I think is important.
#Giving header names for each column.
df = originaldata[,c(3,4,6,31,43,44,47,48,51,52,53,54,55)]
colnames(df) <- c("product","opinion","strength","addictive","willbuy","preferred","after30min","overall","ethnicity","education","income","marital","working")


#grouping by product
productA = subset(df, df[,"product"] == "Deodorant B")
productB = subset(df, df[,"product"] == "Deodorant F")
productC = subset(df, df[,"product"] == "Deodorant G")
productD = subset(df, df[,"product"] == "Deodorant H")
productE = subset(df, df[,"product"] == "Deodorant J")

#this is the lines for testing out pdf generating functions


#part of analysis
summary(productA[,'overall'])

table(strength)
xtabs(~strength+after30min,productA)

productA$overall <- as.factor(productA$overall)
orderedlogit <- polr(overall ~ strength + after30min + addictive, data = productA, method = "logistic")
summary(orderedlogit)






# load required libraries
library(devtools)
#install_github("choroplethr", "trulia")
library(choroplethr)
library(maps)

# read Guardian data
# Contains gun crime statistics for all states in USA
guardian.data <- read.csv(file='US gun crime - SUMMARY 2011.csv', header=TRUE)
total.crime <- apply(guardian.data[,8:10], 1, sum)
df.guardian <- data.frame(value=total.crime, region=as.vector(guardian.data[,c('State.code')]))

# plot Guardian data on USA map
choroplethr(df.guardian, lod="state")

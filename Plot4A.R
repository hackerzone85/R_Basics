# PLOT 4
# To run this routine you must have installed the below packages
require(sqldf)
require(dplyr)

# clear the workspace to be safe
rm(list=ls())

# fetch the file from remote source, unzip and load into R
url <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
if (!file.exists("local.zip")) {
  file.create("local.zip")
}
download.file(url, destfile = "local.zip")
unzip("local.zip")

# Files "Source_Classification_Code.rds" and "summarySCC_PM25.rds".. 
# ..should now be in the wd

# read in the data files
dataFile <- tbl_df(readRDS("summarySCC_PM25.rds"))
referenceFile <- tbl_df(readRDS("Source_Classification_Code.rds"))

# prepare the data for plotting
# As with previous examples, there appear to be references labelled "All" and "Totals"
# My guess is these contain pre-aggregated numbers
# Removing these to avoid skewing the data / double counting

# For this example we also need to find all the references
# where "Combustion" is in description Level One
# and Coal is in description Level Three
# combining the required filtering into one query is trivial with SQLDF
CoalCombustion <- sqldf("SELECT SCC FROM referenceFile
                          WHERE referenceFile.[SCC.Level.One] LIKE '%Combustion%'
                          AND referenceFile.[SCC.Level.Three] LIKE '%Coal%'
                          AND referenceFile.[SCC.Level.Four] != 'All'
                          AND referenceFile.[SCC.Level.Four] != 'Total'")
# use inner_join to filter out everything that doesn't match one of the CoalCombustion references
dataFile <- inner_join(CoalCombustion, dataFile, by = "SCC")

totalsByYear <- with(dataFile, tapply(Emissions/1000, year, sum))
  
# produce the plot file
png("plot4.png")

plot(names(totalsByYear), totalsByYear, type = "l"
  , col = "red", lwd = 3
  , main = "Plot 4 - Total Emissions (Sources: Coal Combustion)"
  , xlab = "Year", ylab = "Tons (thousands)")
  text(names(totalsByYear), totalsByYear, names(totalsByYear)
  )

# close the device
dev.off()
# PLOT 5
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
# where "Vehicle" is in description Level Two
# including ALL types of motor vehicles, although there are dozens of sub-categories
# combining the required filtering into one query is trivial with SQLDF
Vehicles <- sqldf("SELECT SCC FROM referenceFile
                          WHERE referenceFile.[SCC.Level.Two] LIKE '%Vehicle%'
                          AND referenceFile.[SCC.Level.Four] != 'All'
                          AND referenceFile.[SCC.Level.Four] != 'Total'")
# use inner_join to filter out everything that doesn't match one of the CoalCombustion references
dataFile <- inner_join(Vehicles, dataFile, by = "SCC")

# as iwth other examples, this is just for Baltimore City
dataFile <- dataFile[dataFile$fips == "24510", ]

totalsByYear <- with(dataFile, tapply(Emissions/1000, year, sum))
  
# produce the plot file
png("plot5.png")

plot(names(totalsByYear), totalsByYear, type = "l"
  , col = "red", lwd = 3
  , main = "Plot 5 - Baltimore City total emissions (Sources: Vehicles)"
  , xlab = "Year", ylab = "Tons (thousands)")
  text(names(totalsByYear), totalsByYear, names(totalsByYear)
  )

# close the device
dev.off()
# PLOT 3
# To run this routine you must have installed the below packages
require(sqldf)
require(dplyr)
require(ggplot2)

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

# There appear to be references labelled "All" and "Totals"
# My guess is these contain pre-aggregated numbers
# Removing these to avoid skewing the data / double counting
RemoveTotals <- sqldf("SELECT SCC FROM referenceFile
                      WHERE referenceFile.[SCC.Level.Four] != 'All'
                      AND referenceFile.[SCC.Level.Four] != 'Total'")
# use inner_join to filter out everything that doesn't match one remaining references
dataFile <- inner_join(RemoveTotals, dataFile, by = "SCC")

# prepare the data for plotting
dataFile <- dataFile[dataFile$fips == "24510", ]
dataFile <- group_by(dataFile, year, type)
dataFile <- summarise_each(dataFile, funs(sum), Emissions)

# produce the plot file
png("plot3.png")

q <- qplot(data = dataFile, year, Emissions/1000
      , facets = .~type, col = type, geom = "line"
      , ylab = "Tons (thousands)"
      , main = "Plot 3 - Baltimore City total emissions by year by type"
      )
# make the x axis legible
q + theme(axis.text.x = element_text(angle = 90, hjust = 1))

# close the device
dev.off()
# PLOT 6
# To run this routine you must have installed the below packages
require(sqldf)
require(dplyr)
require(lattice)

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

# this time it's Baltimore City and Los Angeles County
CountySelect <- function(c) {if (c == "06037") { "Los Angeles County"} else { "Baltimore City"} }
dataFile <- dataFile[dataFile$fips == "24510" | dataFile$fips == "06037", ] %>%
group_by(fips, year) %>%
summarise_each(funs(sum), Emissions) %>%
mutate(County = CountySelect(fips) )

# produce the plot file
png("plot6.png")

xyplot(Emissions/1000~year | as.factor(County), data = dataFile
       , main = "Plot 6 - Comparison 2 counties total emissions (Sources: Vehicles)"
       , ylab = "Tons (thousands)"
       , ylim = c(0, 9)
       , layout = c(2, 1)
       , panel = function(x, y) {
            panel.xyplot(x, y)
            llines(x, y)
            ltext(x = x, y = y+0.5, label = x, cex=0.8)
       }
)

# close the device
dev.off()
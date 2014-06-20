# This function creates a bar plot for emissions due to coal combustion by year
# across USA. There are far too many counties in the data to plot them all in a
# graph, so we will be grouping them by state.
#
# To accomplish this, we have used information from
# http://www.epa.gov/envirofw/html/codes/state.html
# to match the first two digits of the fips with the corresponding state code.
# We put this information into the "StateFips.csv" file.

mkPlot4 <- function() {
  
  # We will make use of the "data.table" library, so we need
  # to load it (if it is already loaded, nothing will happen)
  library(data.table)
  
  # This time we will also need the "ggplot2" package
  library(ggplot2)
  
  # Load the data from the RDS file, and convert it to 
  # a data table
  summary   <- data.table(readRDS("summarySCC_PM25.rds"))
  sources   <- data.table(readRDS("Source_Classification_Code.rds"))
  
  # We want the fips codes in character format, to match them with the summary
  statefips <- data.table(read.csv("StateFips.csv", colClasses=c("character","factor","character")))
  
  # EI.Sector denotes all coal types (anthracite, lignite, etc) as "Coal",
  # so, it is the best chance to get all coal-related sources
  sector     <- grepl("[cC]oal", sources$EI.Sector)

  # SCC.Level.One describes the type of usage, and is a good source
  # to discriminate by coal usage (combustion)
  combustion <- grepl("[cC]omb", sources$SCC.Level.One)
  
  # Get SCCs related to coal combustion
  coalSCC <- sources[sector & combustion, SCC];
  
  # Create a new column in summary with the fips state codes
  summary$stfips <- factor(substr(summary$fips,0,2))
  
  # Now, merge with the states information, preserving all summary information
  summary <- merge(summary, statefips, by="stfips", ALL.x=T, ALL.y=F)
  
  # Subset rows from summary using coalSCC to get only 
  # coal combustion data. Also sum up emissions by year in thousands of tons.
  totals <- summary[SCC %in% coalSCC, list(CoalEmission=sum(Emissions)/1e3),
                    by=c('stCode','year')]

  # Delete summary to free memory
  rm(summary)
                    
  # Add a factor with the years for the filling and legends
  totals$yearFactor <- factor(totals$year)
  
  # Create a lots of bar plots by year, grouped in 10 columns.
  # Each plot will have a different scale, since emission ranges
  # Change much from one state to another. Also, no x tick labels
  # are written, but bars for each year have different colors,
  # shown in the legend.
  p <- ggplot(totals, aes(x=year,y=CoalEmission)) + theme(legend.title=element_blank()) + 
    facet_wrap( ~ stCode, ncol=10, scales="free_y") +
    geom_bar(aes(fill=yearFactor), stat="identity") + 
    xlab("") + ylab("thousands of tons") + 
    ggtitle("Coal Combustion related PM[2.5] 
            emissions across USA by year") +
    geom_smooth(method="lm", se = F, col="black") +
    scale_x_continuous(breaks=unique(totals$year),labels=NULL)
  
  # Save to PNG file and replot. We need a lot of space, there is much to plot
  ggsave(plot=p, filename="plot4.png", height=9, width=15, dpi = 120, units="in")  
    
  # Show a nice label saying we are done :)
  "Plotting done"  
  
  # Return totals, in case we want to do something with it
  invisible(totals)
}


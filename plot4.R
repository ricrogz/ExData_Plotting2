# This function creates a bar plot for total emissions by year

mkPlot4 <- function() {
  
  # We will make use of the "data.table" library, so we need
  # to load it (if it is already loaded, nothing will happen)
  library(data.table)
  
  # This time we will also need the "ggplot2" package
  library(ggplot2)
  
  # Load the data from the RDS file, and convert it to 
  # a data table
  summary <- data.table(readRDS("summarySCC_PM25.rds"))
  sources <- data.table(readRDS("Source_Classification_Code.rds"))
  
  # EI.Sector denotes all coal types (anthracite, lignite, etc) as "Coal",
  # so, it is the best chance to get all coal-related sources
  sector    <- grepl("[cC]oal", sources$EI.Sector)
  
  # SCC.Level.One describes the type of usage, and is a good source
  # to discriminate for coal usage (combustion)
  combustion <- grepl("[cC]omb", sources$SCC.Level.One)
  
  
  # TO DO:
  #
  # -- subset sources & combustion with (sector & combustion)
  # -- sum subset
  # -- plot
  
  
  
  
  
  # Sum the emission values in Baltimore for each year using the features
  # of the data.table. Another data table with year and
  # TotalEmission columns will be stored in totals.
  totals <- summary[fips == "24510",list(TotalEmission=sum(Emissions)/1e3),
                    by=c('year','type')]
  totals$yearFactor <- factor(totals$year)
  
  # Create a simple bar plot by year. The X axis will be drawn,
  # but the Y axis won't.
  p <- ggplot(totals, aes(x=year,y=TotalEmission)) + facet_grid(. ~ type) +
    geom_bar(aes(fill=year), stat="identity") + xlab("year") + ylab("thousands of tons") +
    ggtitle(bquote(PM[2.5] ~ " Emissions in Baltimore per type and year")) +
    geom_smooth(method="lm", se = F, col="black") +
    scale_x_continuous(breaks=unique(totals$year))
  print(p)
  
  # Open a PNG file and replot. Since no size is required by the
  # project specification, we make bigger than default to be
  # able to see data more clearly
  png(filename="plot4.png", width=800, heigh=600)
  print(p)
  dev.off()
    
  # Show a nice label saying we are done :)
  "Plotting done"  
  
  # Return totals, in case we want to do something with it
  invisible(totals)
}


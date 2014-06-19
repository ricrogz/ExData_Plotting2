# This function creates a bar plot for emissions due to coal combustion by year
# in all USA

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
  # to discriminate by coal usage (combustion)
  combustion <- grepl("[cC]omb", sources$SCC.Level.One)
  
  # Get SCCs related to coal combustion
  coalSCC <- sources[sector & combustion, SCC];
  
  # Subset rows from summay using coalSCC of the data.table to get only 
  # coal combustion data. Also sum up emissions by year.
  totals <- summary[SCC %in% coalSCC, list(CoalEmission=sum(Emissions)/1e6),
                    by='year']
    
  # Create a simple bar plot by year. The X axis will be drawn,
  # but the Y axis won't.
  p <- ggplot(totals, aes(x=year,y=CoalEmission)) +
    geom_bar(aes(fill=year), stat="identity") + xlab("year") + ylab("millions of tons") +
    ggtitle(bquote(PM[2.5] ~ " Emissions in Baltimore per type and year")) +
    geom_smooth(method="lm", se = F, col="black") + guides(fill=F) +
    scale_x_continuous(breaks=unique(totals$year))
  print(p)
  
  # Open a PNG file and replot. Since no size is required by the
  # project specification, we make bigger than default to be
  # able to see data more clearly
  png(filename="plot4.png", width=480, heigh=480)
  print(p)
  dev.off()
    
  # Show a nice label saying we are done :)
  "Plotting done"  
  
  # Return totals, in case we want to do something with it
  invisible(totals)
}


# This function creates a bar plot for total PM25 emissions in USA by year

mkPlot1 <- function() {
  
  # We will make use of the "data.table" library, so we need
  # to load it (if it is already loaded, nothing will happen)
  library(data.table)
  
  # Load the data from the RDS file, and convert it to 
  # a data table
  summary <- data.table(readRDS("summarySCC_PM25.rds"))
  
  # Sum the emission values for each year using the features
  # of the data.table. Another data table with year and
  # TotalEmission columns will be stored in totals.
  totals <- summary[,list(TotalEmission=sum(Emissions)/1e6), by='year']

  # Delete summary to free memory
  rm(summary)
  
  # Create a simple bar plot by year. The X axis will be drawn,
  # but the Y axis won't (yet).
  barplot(totals$TotalEmission, names.arg = totals$year, xlab = "year",
          ylab="millions of tons", axis.lty = 1, axes = F, ylim=c(0,8),
          main = bquote("Total " ~ PM[2.5] ~ " Emissions in USA"))
  
  # Create Y axis on the left. Put ticks at intervals
  # of 0.5 millions and let R decide where to put labels.
  axis(2, seq(0.0,7.5,0.5))
  
  # Fit data to a linear model, and plot the trendline
  trend <- lm(TotalEmission ~ year, totals)
  lines(predict(trend))
  
  # copy plot over to PNG device with specified parameters
  dev.copy(png, file = "plot1.png", width = 480, height = 480,
           units = "px")
  
  # close PNG device
  dev.off()
  
  # Show a nice label saying we are done :)
  "Plotting done"  
  
  # Return totals, in case we want to do something with it
  invisible(totals)
}
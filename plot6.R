# This function creates a bar plot for motor vehicle related emissions
#  in Baltimore vs Los Angeles by year

mkPlot6 <- function() {
  
  # We will make use of the "data.table" library, so we need
  # to load it (if it is already loaded, nothing will happen)
  library(data.table)
  
  # This time we will also need the "ggplot2" package
  library(ggplot2)
  
  # Load the data from the RDS files into data.tables
  summary <- data.table(readRDS("summarySCC_PM25.rds"))
  sources <- data.table(readRDS("Source_Classification_Code.rds"))
  
  # Here we consider "motor vehicles" as "road vehicles" (cars, trucks, motorbikes, etc),
  # which seem to be included into the "onroad" type, plus marine vessels, aircrafts and
  # railroads. It could be argued that "motor vehicles" refers only to "road vehicles",
  # and then variables "railroads", "aircrafts" and  "ships" would not be used.
  
  # build logical vector for different vehicle types
  roadVehicles <- sources$Data.Category == "Onroad"
  railroads    <- grepl("[lL]ocomotives", sources$EI.Sector)
  aircrafts    <- grepl("[aA]ircraft",    sources$EI.Sector)  
  ships        <- grepl("[vV]essels",     sources$EI.Sector)
  
  # combine logical vector to include all vehicles:
  vehicleSCC <- sources$SCC[roadVehicles | railroads | aircrafts | ships]
    
  # Subset rows from summary using vehicleSCC to get only motor vehicle data,
  # restricted to Baltimore and LA by fips number. Also sum up emissions by year 
  # scaling them to thousands of tons.  
  totals <- summary[(fips == "24510" | fips == "06037") & SCC %in% vehicleSCC, 
                    list(VehicleEmission=sum(Emissions)/1e3), by=c('year','fips')]

  # Delete summary to free memory
  rm(summary)
                    
  # Add a factor with the years for the filling and legends
  totals$yearFactor <- factor(totals$year)                    
                    
  # Add a "city" column, giving "Baltimore" as default value.
  totals$city <- "Baltimore"
  # Change city value to "Los Angeles" for the appropriate rows
  totals$city[totals$fips == "06037"] <- "Los Angeles"
  
  # Create a bar plot by year with linear trend lines both for
  # Baltimore and LA. Allow different scales from both.
  p <- ggplot(totals, aes(x=year,y=VehicleEmission)) +
    geom_bar(aes(fill=yearFactor), stat="identity") + xlab("year") +
    ylab(expression(PM[2.5] * ", thousands of tons")) +
    facet_wrap( ~ city, ncol=2, scales="free_y") +
    ggtitle("Motor Vehicle related emissions\n in Baltimore and Los Angeles by year") +
    geom_smooth(method="lm", se = F, col="black") + guides(fill=F) +
    scale_x_continuous(breaks=unique(totals$year)) +
    theme(legend.title=element_blank())
  
  # Save to PNG file. Since no specific size is required by the
  # project specification, we make bigger than default to be
  # able to see data more clearly
  ggsave(plot=p, filename="plot6.png", height=4, width=7, dpi = 120, units="in")    
    
  # Show a nice label saying we are done :)
  "Plotting done"  
  
  # Return totals, in case we want to do something with it
  invisible(totals)
}


subSelect <- function(df, string, year1) {
        subSelect.df <- subset(df, df$fips == string & df$year == year1)
        return(subSelect.df)
}

getMedian <- function(dftemp) {
        medtemp <- median(dftemp$Emissions)
        return(medtemp)
}

plot2 <- function() {
        ## Initialize variables
        dirnm <- "C:/Users/berna/Documents/R/Exploratory_Data_Analysis/Course Project 2"
        filenm1 <- "summarySCC_PM25.rds"
        filenm2 <- "Source_Classification_Code.rds"
        fips <- "24510"
        outputnm <- "plot2.png"
        med <- vector(mode = "numeric")
        
        ## Gather libraries and set diretory
        library(dplyr)
        setwd(dirnm)
        
        ## Read pm25 summary & source classification filea
        summary.df <- readRDS(filenm1)
        ## source.df <- readRDS(filenm2) -- unneeded for this plot
        
        ## Keep just fips, Emmisions and year columns
        use.df <- select(summary.df, 1, 4, 6)
        
        ## Select data by indivdiual year and fips code - 1999, 2002, 2005, 2008
        s1999.df <- subSelect(use.df, fips, 1999L)
        s2008.df <- subSelect(use.df, fips, 2008L)
        
        ## Get range of medians for all years for y-axis plotting
        med[1] <- getMedian(s1999.df)
        med[2] <- getMedian(s2008.df)
        rng <- range(med)
        
        ## Plot median for each year
        x <- png(filename = outputnm)
        plot(1999, med[1], xlab = "Year", ylab = "Emissions",
             main = "Median PM2.5 Emissions by Year - Baltimore",
             xlim = c(1998, 2009), ylim = c(0, rng[2]), pch = 19)
        points(2008, med[2], pch = 19)
        lines(c(1999, 2008), med, lwd = 2, col = "blue")
        dev.off()
}
yrSelect <- function(df, year1) {
        yrSelect.df <- subset(df, df$year == year1)
        return(yrSelect.df)
}

getMedian <- function(dftemp) {
        medtemp <- median(dftemp[, 1])
        return(medtemp)
}

plot1 <- function() {
        ## Initialize variables
        dirnm <- "C:/Users/berna/Documents/R/Exploratory_Data_Analysis/Course Project 2"
        filenm1 <- "summarySCC_PM25.rds"
        filenm2 <- "Source_Classification_Code.rds"
        outputnm <- "plot1.png"
        med <- vector(mode = "numeric")
        
        ## Gather libraries and set diretory
        library(dplyr)
        setwd(dirnm)
        
        ## Read pm25 summary & source classification filea
        summary.df <- readRDS(filenm1)
        source.df <- readRDS(filenm2)
        
        ## Keep just Emmisions and year columns
        use.df <- select(summary.df, 4, 6)
        
        ## Select data by indivdiual year - 1999, 2002, 2005, 2008
        s1999.df <- yrSelect(use.df, 1999L)
        s2002.df <- yrSelect(use.df, 2002L)
        s2005.df <- yrSelect(use.df, 2005L)
        s2008.df <- yrSelect(use.df, 2008L)
        
        ## Get range of medians for all years for y-axis plotting
        med[1] <- getMedian(s1999.df)
        med[2] <- getMedian(s2002.df)
        med[3] <- getMedian(s2005.df)
        med[4] <- getMedian(s2008.df)
        rng <- range(med)
        rngmax <- rng[2]
        
        ## Plot median for each year
        x <- png(filename = outputnm)
        plot(1999, med[1], xlab = "Year", ylab = "Emissions",
             main = "US Median PM2.5 Emissions - 1999, 2002, 2005 & 2008",
             xlim = c(1999L, 2008L), ylim = c(0, rngmax), pch = 19)
        points(2002, med[2], pch = 19)
        points(2005, med[3], pch = 19)
        points(2008, med[4], pch = 19)
        lines(c(1999, 2002, 2005, 2008), med, lwd = 2, col = "blue")
        dev.off()
}
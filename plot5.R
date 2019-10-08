subSelect <- function(df, string, year1) {
        subSelect.df <- subset(df, df$fips == string & df$year == year1)
        return(subSelect.df)
}

chgSCCtoCharacter <- function(dftemp) {
        dftemp <- as_tibble(dftemp)
        dftemp$SCC <- as.character(dftemp$SCC)
        return(dftemp)
}

getMedian <- function(dftemp) {
        medtemp <- median(dftemp$Emissions, na.rm = TRUE)
        return(medtemp)
}

plot5 <- function() {
        ## Initialize variables
        dirnm <- "C:/Users/berna/Documents/R/Exploratory_Data_Analysis/Course Project 2"
        filenm1 <- "summarySCC_PM25.rds"
        filenm2 <- "Source_Classification_Code.rds"
        fips <- "24510"
        outputnm <- "plot5.png"
        med <- vector(mode = "numeric")
        
        ## Gather libraries and set diretory
        library(dplyr)
        setwd(dirnm)
        
        ## Read pm25 summary & source classification filea
        summary.df <- readRDS(filenm1)
        source.df <- readRDS(filenm2)
        
        ## Get dataframe of Vehicle source SCCs
        source.df <- chgSCCtoCharacter(source.df)
        SCC.vehicle <- subset(source.df, grepl("Vehicle", source.df$EI.Sector), 1)
        
        ## Keep just Emmisions and year columns
        use.df <- select(summary.df, 1, 2, 4, 6)
        
        ## Select only rows where the SCC represents a Vehicle source (in EI Sector)
        vehicle.df <- subset(use.df, use.df$SCC %in% SCC.vehicle$SCC) 
        
        ## Select data by indivdiual year and fips code - 1999, 2002, 2005, 2008
        s1999.df <- subSelect(use.df, fips, 1999L)
        s2008.df <- subSelect(use.df, fips, 2008L)
        
        ## Get range of medians for both years for y-axis plotting
        med[1] <- getMedian(s1999.df)
        med[2] <- getMedian(s2008.df)
        rng <- range(med)
        rngmax <- rng[2]
        med1 <- med[1]
        med2 <- med[2]
        
        ## Plot median for each year
        x <- png(filename = outputnm)
        plot(1999L, med1, xlab = "Year", ylab = "Emissions",
             main = "Median Baltimore Vehicle Emissions - 1999 vs. 2008",
             xlim = c(1999L, 2008L), ylim = c(0, rngmax), pch = 19)
        points(2008L, med2, pch = 19)
        lines(c(1999L, 2008L), med, lwd = 2, col = "blue")
        abline(h = min(rng), col = "black")
        text(x = mean(c(1999, 2008)), y = (min(rng) - 0.01), cex = 1,
             labels = paste("Black horizontal line at level of lower point", round(min(rng), 4)))
        dev.off()
}
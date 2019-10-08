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

plot6 <- function() {
        ## Initialize variables
        dirnm <- "C:/Users/berna/Documents/R/Exploratory_Data_Analysis/Course Project 2"
        filenm1 <- "summarySCC_PM25.rds"
        filenm2 <- "Source_Classification_Code.rds"
        fips1 <- "24510"
        fips2 <- "06037"
        City <- rep(c("Baltimore", "Los Angeles"), each = 2)
        yearval <- as.integer(rep(c(1999, 2008), times = 2))
        outputnm <- "plot6.png"
        med <- vector(mode = "numeric")
        
        ## Gather libraries and set diretory
        library(dplyr)
        library(ggplot2)
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
        
        ## Select data by indivdiual year - 1999 & 2008 - and FIPS codes for Baltimore & Los Angeles
        s1999_Balt.df <- subSelect(use.df, fips1, 1999L)
        s2008_Balt.df <- subSelect(use.df, fips1, 2008L)
        s1999_LA.df <- subSelect(use.df, fips2, 1999L)
        s2008_LA.df <- subSelect(use.df, fips2, 2008L)
        
        ## Get range of medians for both years for y-axis plotting
        med[1] <- getMedian(s1999_Balt.df)
        med[2] <- getMedian(s2008_Balt.df)
        med[3] <- getMedian(s1999_LA.df)
        med[4] <- getMedian(s2008_LA.df)
        rng <- range(med)
        rngmin <- rng[1]
        rngmax <- rng[2]
        
        ## Plot median for each year
        out.df <- data.frame(yearval, med, City)
        ggplot(out.df, mapping = aes(x = yearval, y = med, group = City, color = City)) +
                labs(title = "Median Vehicle Emissions - 1999 vs. 2008 for Baltimore & Los Angeles") +
                xlab("Year") +
                ylab("Emissions") +
                coord_cartesian(xlim = c(1999L, 2008L), ylim = c(0, rngmax), clip = "off") +
                geom_point(x = yearval, y = med, shape = "o", size = 3) + 
                geom_line(linetype = 1) +
                geom_hline(color = "black", linetype = 1, yintercept = rngmin, show.legend = TRUE) +               
                annotate("text", x = 2003, y = -0.05, size = 3,
                         label = paste("black horizontal line at minimum value ",
                                       round(rngmin, 4), " for reference", sep = ""))
        ggsave(outputnm)

}
subSelect <- function(df, thistype, string, year1) {
        subSelect.df <- subset(df, df$type == thistype & df$fips == string & df$year == year1)
        return(subSelect.df)
}

getMedian <- function(dftemp) {
        medtemp <- median(dftemp$Emissions)
        return(medtemp)
}

plot3 <- function() {
        ## Initialize variables
        dirnm <- "C:/Users/berna/Documents/R/Exploratory_Data_Analysis/Course Project 2"
        filenm1 <- "summarySCC_PM25.rds"
        filenm2 <- "Source_Classification_Code.rds"
        fips <- "24510"
        outputnm <- "plot3.png"
        yearval <- as.integer(rep(c(1999, 2008), each = 4))
        types <- rep(c("POINT", "NONPOINT", "ON-ROAD", "NON-ROAD"), times = 2)
        med <- vector(mode = "numeric")
        
        ## Gather libraries and set diretory
        library(dplyr)
        library(ggplot2)
        setwd(dirnm)
        
        ## Read pm25 summary & source classification filea
        summary.df <- readRDS(filenm1)
        ## source.df <- readRDS(filenm2) -- unneeded for this plot
        
        ## Keep just fips, Emmisions and year columns
        use.df <- select(summary.df, 1, 4, 5, 6)
        
        ## Select data by indivdiual year (1999, 2002, 2005, 2008), specific fips, and type
        s1999_1.df <- subSelect(use.df, types[1], fips, yearval[1])
        s1999_2.df <- subSelect(use.df, types[2], fips, yearval[2])
        s1999_3.df <- subSelect(use.df, types[3], fips, yearval[3])
        s1999_4.df <- subSelect(use.df, types[4], fips, yearval[4])
        s2008_1.df <- subSelect(use.df, types[1], fips, yearval[5])
        s2008_2.df <- subSelect(use.df, types[2], fips, yearval[6])
        s2008_3.df <- subSelect(use.df, types[3], fips, yearval[7])
        s2008_4.df <- subSelect(use.df, types[4], fips, yearval[8])        
        
        ## Get range of medians for all years for y-axis plotting
        med[1] <- getMedian(s1999_1.df)
        med[2] <- getMedian(s1999_2.df)
        med[3] <- getMedian(s1999_3.df)
        med[4] <- getMedian(s1999_4.df)
        med[5] <- getMedian(s2008_1.df)
        med[6] <- getMedian(s2008_2.df)
        med[7] <- getMedian(s2008_3.df)
        med[8] <- getMedian(s2008_4.df)

        rng <- range(med)
        rnglow <- rng[1]
        rnghigh <- rng[2]
        
        ## Plot median for each year
        out.df <- data.frame(yearval, emissions = med, Types = types)
        ggplot(out.df, mapping = aes(x = yearval, y = emissions, group = Types, color = Types)) +
                        coord_cartesian(xlim = c(1999L, 2008L), ylim = c(-0.2, (rnghigh - 1)), clip = "off") +
                        labs(x = "Year", y = "Emissions", title = "PM2.5 Median Emissions by Year & Type") +
              geom_point(shape = "o", size = 3) +
              geom_line(linetype = 1) +
              geom_hline(color = "black", linetype = 1, yintercept = -0.1, show.legend = TRUE) +               
                        annotate("text", x = 2003, y = -0.35, size = 3.35,
                               label = "black horizontal line at -0.1 for reference")
        ggsave(outputnm, device = "png")

        
}
# Preliminary exploration of NEON zooplankton data
# December 16, 2022
# AGS

# load in libraries
pacman::p_load(tidyverse,plyr,ggplot2)

# function to count characters starting at the end of the string
substrEnd <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

#############################################
# Preliminary look at the data from each lake
#############################################

# read in all zoop data files
files <- list.files(file.path(getwd(), "Data/Neon_zooplankton"), 
                    pattern = "zoo_field.*csv$", full.names = TRUE, recursive=TRUE)

zoop_df <- ldply(files, read.csv, header=TRUE)

# add new column for site
zoop_df$site <- substrEnd(zoop_df$namedLocation,2)

as.Date(zoop_df$collectDate)

zoop_df <- zoop_df %>% # split collectDate up into year, month, and day columns
  dplyr::mutate(year = lubridate::year(collectDate), 
                month = lubridate::month(collectDate), 
                day = lubridate::day(collectDate))

zoop_df <- cbind(count = 1, zoop_df) # add a column of 1s to make the figure work
zoop_df$month <- as.factor(zoop_df$month) # make month a factor

# re-making of Heather's plots, but colored by month so we can see what months the samples are from
# quick plots to look at data availability across lakes, years, sites, and methods

ggplot(subset(zoop_df, siteID=="BARC" & samplerType %in% c("Schindler-Patalas","tow net")), aes(x = as.Date(collectDate), y = count, fill = month)) + 
  geom_bar(stat = "identity") + facet_wrap(~samplerType+site) + theme_bw() + xlab("BARC")

ggplot(subset(zoop_df, siteID=="SUGG" & samplerType %in% c("Schindler-Patalas","tow net")), aes(x = as.Date(collectDate), y = count, fill = month)) + 
  geom_bar(stat = "identity") + facet_wrap(~samplerType+site) + theme_bw() + xlab("SUGG")

ggplot(subset(zoop_df, siteID=="CRAM" & samplerType %in% c("Schindler-Patalas","tow net")), aes(x = as.Date(collectDate), y = count, fill = month)) + 
  geom_bar(stat = "identity") + facet_wrap(~samplerType+site) + theme_bw() + xlab("CRAM")

ggplot(subset(zoop_df, siteID=="LIRO" & samplerType %in% c("Schindler-Patalas","tow net")), aes(x = as.Date(collectDate), y = count, fill = month)) + 
  geom_bar(stat = "identity") + facet_wrap(~samplerType+site) + theme_bw() + xlab("LIRO")

ggplot(subset(zoop_df, siteID=="PRLA" & samplerType %in% c("Schindler-Patalas","tow net")), aes(x = as.Date(collectDate), y = count, fill = month)) + 
  geom_bar(stat = "identity") + facet_wrap(~samplerType+site) + theme_bw() + xlab("PRLA")

ggplot(subset(zoop_df, siteID=="PRPO" & samplerType %in% c("Schindler-Patalas","tow net")), aes(x = as.Date(collectDate), y = count, fill = month)) + 
  geom_bar(stat = "identity") + facet_wrap(~samplerType+site) + theme_bw() + xlab("PRPO")

ggplot(subset(zoop_df, siteID=="TOOK" & samplerType %in% c("Schindler-Patalas","tow net")), aes(x = as.Date(collectDate), y = count, fill = month)) + 
  geom_bar(stat = "identity") + facet_wrap(~samplerType+site) + theme_bw() + xlab("TOOK")

####################################################
# Preliminary look at the processed zooplankton data
####################################################

# read in all zoop data files
setwd("/Users/Anna/Desktop/GLEON/NEONrotifers/GLEON_rotifers")
files <- list.files(file.path(getwd(), "Data/Neon_zooplankton"), 
                    pattern = "zoo_taxonomyProcessed.*csv$", full.names = TRUE, recursive=TRUE)
processed_df <- ldply(files, read.csv, header=TRUE)

# need to figure out how to accurately calculate density from the columns in this data frame
# columns that we will likely use to calculate density:
#    individualCount
#    subsampleType	
#    zooVolumePerBottle
#    zooSubsampleVolume
#    adjCountPerBottle
# what about volume of water filtered??

# there is also length data for all of the taxa - amazing!

count(processed_df$scientificName) # 179 unique taxa present - can get an idea of the most frequent ones
count(processed_df$phylum) # equal amount of data for crustaceans and rotifers - amazing!

mean(subset(processed_df, phylum=="Arthropoda")$zooMeanLength, na.rm = TRUE) # mean length of crustaceans in the dataset
mean(subset(processed_df, phylum=="Rotifera")$zooMeanLength, na.rm = TRUE) # mean length of rotifers in the dataset

# to figure out: 
#     how to link this information with the information in the other spreadsheets about what sampling method was employed (Schindler or tow net) - figure out what uid column corresponds to
#     make sure that we should be using taxomonyProcessed vs. taxonomyRaw
#     figure out how to calculate density and biomass from the info in the columns provided

# Looking at which rows have width data
sub <- subset(processed_df, zooWidth!= "NA")
count(sub$scientificName)
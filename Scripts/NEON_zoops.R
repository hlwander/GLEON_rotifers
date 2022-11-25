#visualize NEON zoop data
#24Nov22 HLW

#load in libraries
pacman::p_load(tidyverse,plyr,ggplot2)

#function to count characters starting at the end of the string
substrEnd <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

#read in all zoop data files
files <- list.files(file.path(getwd(), "Data/Neon_zooplankton"), 
                      pattern = "zoo_field.*csv$", full.names = TRUE, recursive=TRUE)

zoop_df <- ldply(files, read.csv, header=TRUE)

#add new column for site
zoop_df$site <- substrEnd(zoop_df$namedLocation,2)

#quick plots to look at data availability across lakes, years, sites, and methods
ggplot(subset(zoop_df, siteID=="BARC" & samplerType %in% c("Schindler-Patalas","tow net")), aes(as.Date(collectDate))) + 
  geom_bar(fill="#008585") + facet_wrap(~samplerType+site) + theme_bw() + xlab("BARC")

ggplot(subset(zoop_df, siteID=="SUGG" & samplerType %in% c("Schindler-Patalas","tow net")), aes(as.Date(collectDate))) + 
  geom_bar(fill="#9BBAA0") + facet_wrap(~samplerType+site) + theme_bw() + xlab("SUGG")

ggplot(subset(zoop_df, siteID=="CRAM" & samplerType %in% c("Schindler-Patalas","tow net")), aes(as.Date(collectDate))) + 
  geom_bar(fill="#F2E2B0") + facet_wrap(~samplerType+site) + theme_bw() + xlab("CRAM")

ggplot(subset(zoop_df, siteID=="LIRO" & samplerType %in% c("Schindler-Patalas","tow net")), aes(as.Date(collectDate))) + 
  geom_bar(fill="#DEA868") + facet_wrap(~samplerType+site) + theme_bw() + xlab("LIRO")

ggplot(subset(zoop_df, siteID=="PRLA" & samplerType %in% c("Schindler-Patalas","tow net")), aes(as.Date(collectDate))) + 
  geom_bar(fill="#C7522B") + facet_wrap(~samplerType+site) + theme_bw() + xlab("PRLA")

ggplot(subset(zoop_df, siteID=="PRPO" & samplerType %in% c("Schindler-Patalas","tow net")), aes(as.Date(collectDate))) + 
  geom_bar(fill="#6666CC") + facet_wrap(~samplerType+site) + theme_bw() + xlab("PRPO")

ggplot(subset(zoop_df, siteID=="TOOK" & samplerType %in% c("Schindler-Patalas","tow net")), aes(as.Date(collectDate))) + 
  geom_bar(fill="#33CC99") + facet_wrap(~samplerType+site) + theme_bw() + xlab("TOOK")




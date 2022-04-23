# Evolution of AMR across time for every antimicrobial class

# Install packages
library(ggplot2)
library(ggpubr)
library(dplyr)
library(tidyverse)

# Get data
# Set working directory
getwd()
setwd("C:/Users/jadea/Documents/Applied Data Sciences/Assessment 2/AMR_Grouped_Campy")
workingdir = "."
getwd() # This is just to check the change worked. 
df <- read.csv(file = "MasterMixed_Spreadsheet_AMRGROUPED_FINAL.csv")
df <- as.data.frame(df)

# Aminoglycosides 
aminomean <- df %>%
  group_by(Year)%>%
  summarise(mean=mean(amino, exclude.NA=T)/1*100) # get mean and then percentage - 1 Abx class

aminographic <- ggplot(aminomean, aes(Year,mean)) + 
  geom_point() + 
  ggtitle("Aminoglycosides") + 
  theme(plot.title=element_text( hjust=0.5, vjust=0.5)) + 
  geom_smooth() + 
  ylim(0,10) + 
  xlab("") + 
  ylab("")

# Beta-lactams
betamean <- df %>%
  group_by(Year)%>%
  summarise(mean=mean(betalactamics, exclude.NA=T)/1*100)

betagraphic <- ggplot(betamean, aes(Year,mean)) + 
  geom_point() + 
  ggtitle("Betalactams") + 
  theme(plot.title=element_text( hjust=0.5, vjust=0.5)) + 
  labs("Betalactamics") + 
  geom_smooth() + 
  ylim(0,100) +
  xlab("") + 
  ylab("")

# Colistin
colistinmean <- df %>%
  group_by(Year)%>%
  summarise(mean=mean(Colistin, exclude.NA=T)/1*100)

colistingraphic <- ggplot(colistinmean, aes(Year,mean)) + 
  geom_point() + 
  theme(plot.title=element_text( hjust=0.5, vjust=0.5)) + 
  ggtitle("Colistin") + 
  geom_smooth() + 
  ylim(0,1) + 
  xlab("") + 
  ylab("")

# Disinfectants
disinfectantmean <- df %>%
  group_by(Year)%>%
  summarise(mean=mean(disinfectant, exclude.NA=T)/1*100)

disinfectantgraphic <- ggplot(disinfectantmean, aes(Year,mean)) + 
  geom_point() + 
  theme(plot.title=element_text( hjust=0.5, vjust=0.5)) + 
  ggtitle("Disinfectants") + 
  geom_smooth() + 
  ylim(0,70) + 
  xlab("") + 
  ylab("")

# Fosfomycin
fosfomean <- df %>%
  group_by(Year)%>%
  summarise(mean=mean(fosfomycin, exclude.NA=T)/1*100)

fosfographic <- ggplot(fosfomean, aes(Year,mean))+geom_point() + 
  theme(plot.title=element_text( hjust=0.5, vjust=0.5)) + 
  ggtitle("Fosfomycin") + 
  geom_smooth() + 
  ylim(0,10) + 
  xlab("") + 
  ylab("")

# Fusidic acid
fusidicmean <- df %>%
  group_by(Year)%>%
  summarise(mean=mean(fusidic_acid, exclude.NA=T)/1*100)

fusidicgraphic <- ggplot(fusidicmean, aes(Year,mean))+geom_point() + 
  theme(plot.title=element_text( hjust=0.5, vjust=0.5)) + 
  ggtitle("Fusidic acid") + 
  geom_smooth() + 
  ylim(0,1) + 
  xlab("") + 
  ylab("")

# Glycopeptides
glycomean <- df %>%
  group_by(Year)%>%
  summarise(mean=mean(glycopeptide, exclude.NA=T)/1*100)

glycographic <- ggplot(glycomean, aes(Year,mean)) + 
  geom_point() + 
  theme(plot.title=element_text( hjust=0.5, vjust=0.5)) + 
  geom_smooth() + 
  ggtitle("Glycopeptides") + 
  ylim(0,1) + 
  xlab("") + 
  ylab("")

# Macrolides
macromean <- df %>%
  group_by(Year)%>%
  summarise(mean=mean(macrolide, exclude.NA=T)/1*100)

macrographic <- ggplot(macromean, aes(Year,mean)) + 
  geom_point() + 
  theme(plot.title=element_text( hjust=0.5, vjust=0.5)) + 
  geom_smooth() + 
  ggtitle("Macrolides") + 
  ylim(0,20) + 
  xlab("") + 
  ylab("")

# Nitroimidazole
nitromean <- df %>%
  group_by(Year)%>%
  summarise(mean=mean(nitroimidazole, exclude.NA=T)/1*100)

nitrographic <- ggplot(nitromean, aes(Year,mean)) + 
  geom_point() + 
  theme(plot.title=element_text( hjust=0.5, vjust=0.5)) + 
  geom_smooth() + 
  ggtitle("Nitroimidazole") + 
  ylim(0,1) + 
  xlab("") + 
  ylab("")

# Oxazolidinone
oxazmean <- df %>%
  group_by(Year)%>%
  summarise(mean=mean(oxazolidinone, na.rm=T, exclude.NA=T)/1*100)

oxazgraphic <- ggplot(oxazmean, aes(Year,mean))+geom_point() + 
  theme(plot.title=element_text( hjust=0.5, vjust=0.5)) + 
  geom_smooth() + 
  ggtitle("Oxazolidinone") + 
  ylim(0,1) + 
  xlab("") + 
  ylab("")

# Phenicol
phemean <- df %>%
  group_by(Year)%>%
  summarise(mean=mean(phenicol, na.rm=T, exclude.NA=T)/1*100)

phegraphic <- ggplot(phemean, aes(Year,mean)) + 
  geom_point() + 
  theme(plot.title=element_text( hjust=0.5, vjust=0.5)) + 
  geom_smooth() + 
  ggtitle("Phenicols") + 
  ylim(0,50) + 
  xlab("") + 
  ylab("")

# Pseudomonic acid
pseudomean <- df %>%
  group_by(Year)%>%
  summarise(mean=mean(pseudomonic_acid, na.rm=T, exclude.NA=T)/1*100)

pseudographic <- ggplot(pseudomean, aes(Year,mean)) + 
  geom_point() + 
  theme(plot.title=element_text( hjust=0.5, vjust=0.5)) + 
  geom_smooth() + 
  ggtitle("Pseudomonic acid") + 
  ylim(0,1) + 
  xlab("") + 
  ylab("")

# Quinolones
quinomean <- df %>%
  group_by(Year)%>%
  summarise(mean=mean(quinolone, na.rm=T, exclude.NA=T)/1*100)

quinographic <- ggplot(quinomean, aes(Year,mean)) + 
  geom_point() + 
  theme(plot.title=element_text( hjust=0.5, vjust=0.5)) + 
  geom_smooth() + 
  ggtitle("Quinolones") + 
  ylim(0,25) + 
  xlab("") + 
  ylab("")

# Rifamycin
rifamean <- df %>%
  group_by(Year)%>%
  summarise(mean=mean(rifampicin, na.rm=T, exclude.NA=T)/1*100)

rifagraphic <- ggplot(rifamean, aes(Year,mean)) + 
  geom_point() + 
  theme(plot.title=element_text( hjust=0.5, vjust=0.5)) + 
  geom_smooth() + 
  ggtitle("Rifamycin") + 
  ylim(0,1) + 
  xlab("") + 
  ylab("")

# Sulphonamides
sulfamean <- df %>%
  group_by(Year)%>%
  summarise(mean=mean(sulfonamide, na.rm=T, exclude.NA=T)/1*100)

sulfagraphic <- ggplot(sulfamean, aes(Year,mean)) + 
  geom_point() + 
  geom_smooth() + 
  ggtitle("Sulphonamides") + 
  theme(plot.title=element_text( hjust=0.5, vjust=0.5)) + 
  ylim(0,20) + 
  xlab("") + 
  ylab("")

# Tetracyclines
tetmean <- df %>%
  group_by(Year)%>%
  summarise(mean=mean(tetracycline, na.rm=T, exclude.NA=T)/1*100)

tetgraphic <- ggplot(tetmean, aes(Year,mean)) + 
  geom_point() + 
  geom_smooth() + 
  ggtitle("Tetracyclines") + 
  theme(plot.title=element_text( hjust=0.5, vjust=0.5)) + 
  ylim(0,70) + 
  xlab("") + 
  ylab("")

# Trimethoprim
trimean <- df %>%
  group_by(Year)%>%
  summarise(mean=mean(trimethoprim, na.rm=T, exclude.NA=T)/1*100)

trigraphic <- ggplot(trimean, aes(Year,mean)) + 
  geom_point() + 
  geom_smooth() + 
  ggtitle("Trimethoprim") + 
  theme(plot.title=element_text( hjust=0.5, vjust=0.5)) + 
  ylim(0,10) + 
  xlab("") + 
  ylab("")

#Joining the graphs - excluded disinfectants as cannot fit
AMRclassestime<-ggarrange(aminographic,betagraphic,colistingraphic,fosfographic,
                          fusidicgraphic,glycographic,macrographic,nitrographic,oxazgraphic,phegraphic,
                          pseudographic,quinographic,sulfagraphic,tetgraphic,trigraphic, disinfectantgraphic)

#Adding the title and the axis
annotate_figure(AMRclassestime, 
                top=text_grob("Evolution of AMR Genes Families Across Time"), 
                bottom=text_grob("Year"), 
                left = text_grob("Level of resistance (%)",rot = 90, vjust = 1))
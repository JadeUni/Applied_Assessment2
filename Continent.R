# Set working directory
getwd()
setwd("C:/Users/jadea/Documents/Applied Data Sciences/Assessment 2/AMR_Grouped_Campy")
workingdir = "."
getwd() # This is just to check the change worked. 

#packages
library(dplyr)
library(tidyverse)
library(reshape2)
library(lessR)
library(plotly)
library(ggplot2)
library(ggpubr)


#################################################################################

# Continent by bacteria

# Campylobacter coli
df2 <- read.csv(file = "C.coli_FINAL.csv")
df2 <- as.data.frame(df2)

Campylobacter_coli_mean <- df2
Campylobacter_coli_mean <- Campylobacter_coli_mean %>%
  rowwise() %>%
  mutate(total = sum(c_across(18:34))) %>% # add column total which sums up AMR
  group_by(Continent) %>%
  summarise(mean=mean(total)) # find mean of AMR by continent

Campylobacter_coli_mean <- Campylobacter_coli_mean[-1,] # Get rid of unspecified row

Campylobacter_coli_mean <- Campylobacter_coli_mean %>% 
  mutate(percentage=mean/sum(mean)*100) # convert to percentage

Campylobacter_coli_graphic <- ggplot(Campylobacter_coli_mean, aes(x=Continent, y=percentage, fill=Continent)) +
  xlab("") +
  ylab("") +
  geom_bar(stat="identity") + # bar plot
  theme(legend.key.size = unit(0.1, "cm"), 
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.title=element_text(hjust=0.6, vjust=0.5, size=10)) +
  ggtitle("Campylobacter coli")

# Campylobacter jejuni
df3 <- read.csv(file = "C.jejuni_FINAL.csv")
df3 <- as.data.frame(df3)

Campylobacter_jejuni_mean <- df3
Campylobacter_jejuni_mean <- Campylobacter_jejuni_mean %>%
  rowwise() %>%
  mutate(total = sum(c_across(18:34))) %>% # add column total which sums up AMR
  group_by(Continent) %>%
  summarise(mean=mean(total)) # find mean of AMR by continent

Campylobacter_jejuni_mean <- Campylobacter_jejuni_mean[-1,] # Get rid of unspecified row

Campylobacter_jejuni_mean <- Campylobacter_jejuni_mean %>% 
  mutate(percentage=mean/sum(mean)*100) # convert to percentage

Campylobacter_jejuni_graphic <- ggplot(Campylobacter_jejuni_mean, aes(x=Continent, y=percentage, fill=Continent)) +
  xlab("") +
  ylab("") +
  geom_bar(stat="identity") + # bar plot
  theme(legend.key.size = unit(0.1, "cm"), 
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.title=element_text(hjust=0.6, vjust=0.5, size=10)) +
  ggtitle("Campylobacter jejuni")

# Campylobacter fetus
df4 <- read.csv(file = "C.fetus_FINAL.csv")
df4 <- as.data.frame(df4)

Campylobacter_fetus_mean <- df4
Campylobacter_fetus_mean <- Campylobacter_fetus_mean %>%
  rowwise() %>%
  mutate(total = sum(c_across(18:34))) %>% # add column total which sums up AMR
  group_by(Continent) %>%
  summarise(mean=mean(total)) # find mean of AMR by continent

Campylobacter_fetus_mean <- Campylobacter_fetus_mean[-1,] # Get rid of unspecified row

Campylobacter_fetus_mean <- Campylobacter_fetus_mean %>% 
  mutate(percentage=mean/sum(mean)*100) # convert to percentage

Campylobacter_fetus_graphic <- ggplot(Campylobacter_fetus_mean, aes(x=Continent, y=percentage, fill=Continent)) +
  xlab("") +
  ylab("") +
  geom_bar(stat="identity") + # bar plot
  theme(legend.key.size = unit(0.1, "cm"), 
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.title=element_text(hjust=0.6, vjust=0.5, size=10)) +
  ggtitle("Campylobacter fetus")

# Campylobacter heptaticus
df5 <- read.csv(file = "C.hepaticus_Final.csv")
df5 <- as.data.frame(df5)

Campylobacter_hepaticus_mean <- df5
Campylobacter_hepaticus_mean <- Campylobacter_hepaticus_mean %>%
  rowwise() %>%
  mutate(total = sum(c_across(18:34))) %>% # add column total which sums up AMR
  group_by(Continent) %>%
  summarise(mean=mean(total)) # find mean of AMR by continent

Campylobacter_hepaticus_mean <- Campylobacter_hepaticus_mean[-1,] # Get rid of unspecified row

Campylobacter_hepaticus_mean <- Campylobacter_hepaticus_mean %>% 
  mutate(percentage=mean/sum(mean)*100) # convert to percentage

Campylobacter_hepaticus_graphic <- ggplot(Campylobacter_hepaticus_mean, aes(x=Continent, y=percentage, fill=Continent)) +
  xlab("") +
  ylab("") +
  geom_bar(stat="identity") + # bar plot
  theme(legend.key.size = unit(0.1, "cm"), 
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.title=element_text(hjust=0.6, vjust=0.5, size=10)) +
  ggtitle("Campylobacter hepaticus")


# Campylobacter hyointestinalis
df6 <- read.csv(file = "C.hyointestinalis_FINAL.csv")
df6 <- as.data.frame(df6)

Campylobacter_hyointestinalis_mean <- df6
Campylobacter_hyointestinalis_mean <- Campylobacter_hyointestinalis_mean %>%
  rowwise() %>%
  mutate(total = sum(c_across(18:34))) %>% # add column total which sums up AMR
  group_by(Continent) %>%
  summarise(mean=mean(total)) # find mean of AMR by continent

Campylobacter_hyointestinalis_mean <- Campylobacter_hyointestinalis_mean[-1,] # Get rid of unspecified row

Campylobacter_hyointestinalis_mean <- Campylobacter_hyointestinalis_mean %>% 
  mutate(percentage=mean/sum(mean)*100) # convert to percentage

Campylobacter_hyointestinalis_graphic <- ggplot(Campylobacter_hyointestinalis_mean, aes(x=Continent, y=percentage, fill=Continent)) +
  xlab("") +
  ylab("") +
  geom_bar(stat="identity") + # bar plot
  theme(legend.key.size = unit(0.1, "cm"), 
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.title=element_text(hjust=0.6, vjust=0.5, size=10)) +
  ggtitle("Campylobacter hyointestinalis")

# Campylobacter lari
df7 <- read.csv(file = "C.lari_FINAL.csv")
df7 <- as.data.frame(df7)

Campylobacter_lari_mean <- df7
Campylobacter_lari_mean <- Campylobacter_lari_mean %>%
  rowwise() %>%
  mutate(total = sum(c_across(18:34))) %>% # add column total which sums up AMR
  group_by(Continent) %>%
  summarise(mean=mean(total)) # find mean of AMR by continent

Campylobacter_lari_mean <- Campylobacter_lari_mean[-1,] # Get rid of unspecified row

Campylobacter_lari_mean <- Campylobacter_lari_mean %>% 
  mutate(percentage=mean/sum(mean)*100) # convert to percentage

Campylobacter_lari_graphic <- ggplot(Campylobacter_lari_mean, aes(x=Continent, y=percentage, fill=Continent)) +
  xlab("") +
  ylab("") +
  geom_bar(stat="identity") + # bar plot
  theme(legend.key.size = unit(0.1, "cm"), 
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.title=element_text(hjust=0.6, vjust=0.5, size=10)) +
  ggtitle("Campylobacter lari")

# Campylobacter sp. 
df8 <- read.csv(file = "C.sp_FINAL.csv")
df8 <- as.data.frame(df8)

Campylobacter_sp_mean <- df8
Campylobacter_sp_mean <- Campylobacter_sp_mean %>%
  rowwise() %>%
  mutate(total = sum(c_across(18:34))) %>% # add column total which sums up AMR
  group_by(Continent) %>%
  summarise(mean=mean(total)) # find mean of AMR by continent

Campylobacter_sp_mean <- Campylobacter_sp_mean[-1,] # Get rid of unspecified row

Campylobacter_sp_mean <- Campylobacter_sp_mean %>% 
  mutate(percentage=mean/sum(mean)*100) # convert to percentage

Campylobacter_sp_graphic <- ggplot(Campylobacter_sp_mean, aes(x=Continent, y=percentage, fill=Continent)) +
  xlab("") +
  ylab("") +
  geom_bar(stat="identity") + # bar plot
  theme(legend.key.size = unit(0.1, "cm"), 
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.title=element_text(hjust=0.6, vjust=0.5, size=10)) +
  ggtitle("Campylobacter species")

# Campylobacter subantarcticus 
df9 <- read.csv(file = "C.subantarcticus_FINAL.csv")
df9 <- as.data.frame(df9)

Campylobacter_subantarcticus_mean <- df9
Campylobacter_subantarcticus_mean <- Campylobacter_subantarcticus_mean %>%
  rowwise() %>%
  mutate(total = sum(c_across(18:34))) %>% # add column total which sums up AMR
  group_by(Continent) %>%
  summarise(mean=mean(total)) # find mean of AMR by continent

Campylobacter_subantarcticus_mean <- Campylobacter_subantarcticus_mean[-1,] # Get rid of unspecified row

Campylobacter_subantarcticus_mean <- Campylobacter_subantarcticus_mean %>% 
  mutate(percentage=mean/sum(mean)*100) # convert to percentage

Campylobacter_subantarcticus_graphic <- ggplot(Campylobacter_subantarcticus_mean, aes(x=Continent, y=percentage, fill=Continent)) +
  xlab("") +
  ylab("") +
  geom_bar(stat="identity") + # bar plot
  theme(legend.key.size = unit(0.1, "cm"), 
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.title=element_text(hjust=0.6, vjust=0.5, size=10)) +
  ggtitle("Campylobacter subantarcticus")

#Joining the graphs
Continent_AMR_Species <- ggarrange(Campylobacter_coli_graphic, Campylobacter_fetus_graphic, 
                                   Campylobacter_jejuni_graphic, Campylobacter_lari_graphic,
                                   Campylobacter_sp_graphic)


#Adding the title and the axis
annotate_figure(Continent_AMR_Species, 
                top=text_grob("Campylobacter Genus Antimicrobial Resistance Percentage Per Continent"), 
                bottom=text_grob(" "), 
                left = text_grob("Level of resistance (%)",rot = 90, vjust = 1))



#################################################################################

# Continent by antimicrobial class

#Import database
df <- read.csv(file = "MasterMixed_Spreadsheet_AMRGROUPED_FINAL.csv")
df <- as.data.frame(df)

# Aminoglycosides 
aminomean <- df %>%
  group_by(Continent)%>%
  summarise(mean=mean(amino, exclude.NA=T)/1*100) # get mean and then percentage - 1 Abx class

aminomean <- aminomean[-1,] # Get rid of unspecified row

aminomean <- aminomean %>% 
  mutate(percentage=mean/sum(mean)*100) # convert to percentage

aminographic <- ggplot(aminomean, aes(x=Continent, y=percentage, fill=Continent)) +
  xlab("") +
  ylab("") +
  geom_bar(stat="identity") + # bar plot
  theme(legend.key.size = unit(0.1, "cm"), 
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.title=element_text(hjust=0.6, vjust=0.5, size=10)) +
  ggtitle("Aminoglycosides")

# Beta-lactams
betamean <- df %>%
  group_by(Continent)%>%
  summarise(mean=mean(betalactamics, exclude.NA=T)/1*100)

betamean <- betamean[-1,] # Get rid of unspecified row

betamean <- betamean %>% 
  mutate(percentage=mean/sum(mean)*100) # convert to percentage

betagraphic <- ggplot(betamean, aes(x=Continent, y=percentage, fill=Continent)) +
  xlab("") +
  ylab("") +
  geom_bar(stat="identity") + # bar plot
  theme(legend.key.size = unit(0.1, "cm"), 
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.title=element_text(hjust=0.6, vjust=0.5, size=10)) +
  ggtitle("Beta-lactams")

# Colistin
colistinmean <- df %>%
  group_by(Continent)%>%
  summarise(mean=mean(Colistin, exclude.NA=T)/1*100)

colistinmean <- colistinmean[-1,] # Get rid of unspecified row

colistinmean <- colistinmean %>% 
  mutate(percentage=mean/sum(mean)*100) # convert to percentage

colistingraphic <- ggplot(colistinmean, aes(x=Continent, y=percentage, fill=Continent)) +
  xlab("") +
  ylab("") +
  geom_bar(stat="identity") + # bar plot
  theme(legend.key.size = unit(0.1, "cm"), 
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.title=element_text(hjust=0.6, vjust=0.5, size=10)) +
  ggtitle("Colistin")

# Disinfectants
disinfectantmean <- df %>%
  group_by(Continent)%>%
  summarise(mean=mean(disinfectant, exclude.NA=T)/1*100)

disinfectantmean <- disinfectantmean[-1,] # Get rid of unspecified row

disinfectantmean <- disinfectantmean %>% 
  mutate(percentage=mean/sum(mean)*100) # convert to percentage

disinfectantgraphic <- ggplot(disinfectantmean, aes(x=Continent, y=percentage, fill=Continent)) +
  xlab("") +
  ylab("") +
  geom_bar(stat="identity") + # bar plot
  theme(legend.key.size = unit(0.1, "cm"), 
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.title=element_text(hjust=0.6, vjust=0.5, size=10)) +
  ggtitle("Disinfectants")

# Fosfomycin
fosfomean <- df %>%
  group_by(Continent)%>%
  summarise(mean=mean(fosfomycin, exclude.NA=T)/1*100)

fosfomean <- fosfomean[-1,] # Get rid of unspecified row

fosfomean <- fosfomean %>% 
  mutate(percentage=mean/sum(mean)*100) # convert to percentage

fosfographic <- ggplot(fosfomean, aes(x=Continent, y=percentage, fill=Continent)) +
  xlab("") +
  ylab("") +
  geom_bar(stat="identity") + # bar plot
  theme(legend.key.size = unit(0.1, "cm"), 
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.title=element_text(hjust=0.6, vjust=0.5, size=10)) +
  ggtitle("Fosfomycin")

# Fusidic acid
fusidicmean <- df %>%
  group_by(Continent)%>%
  summarise(mean=mean(fusidic_acid, exclude.NA=T)/1*100)

fusidicmean <- fusidicmean[-1,] # Get rid of unspecified row

fusidicmean <- fusidicmean %>% 
  mutate(percentage=mean/sum(mean)*100) # convert to percentage

fusidicgraphic <- ggplot(fusidicmean, aes(x=Continent, y=percentage, fill=Continent)) +
  xlab("") +
  ylab("") +
  geom_bar(stat="identity") + # bar plot
  theme(legend.key.size = unit(0.1, "cm"), 
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.title=element_text(hjust=0.6, vjust=0.5, size=10)) +
  ggtitle("Fusidic acid")

# Glycopeptides
glycomean <- df %>%
  group_by(Continent)%>%
  summarise(mean=mean(glycopeptide, exclude.NA=T)/1*100)

glycomean <- glycomean[-1,] # Get rid of unspecified row

glycomean <- glycomean %>% 
  mutate(percentage=mean/sum(mean)*100) # convert to percentage

glycographic <- ggplot(glycomean, aes(x=Continent, y=percentage, fill=Continent)) +
  xlab("") +
  ylab("") +
  geom_bar(stat="identity") + # bar plot
  theme(legend.key.size = unit(0.1, "cm"), 
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.title=element_text(hjust=0.6, vjust=0.5, size=10)) +
  ggtitle("Glycopeptides")

# Macrolides
macromean <- df %>%
  group_by(Continent)%>%
  summarise(mean=mean(macrolide, exclude.NA=T)/1*100)

macromean <- macromean[-1,] # Get rid of unspecified row

macromean <- macromean %>% 
  mutate(percentage=mean/sum(mean)*100) # convert to percentage

macrographic <- ggplot(macromean, aes(x=Continent, y=percentage, fill=Continent)) +
  xlab("") +
  ylab("") +
  geom_bar(stat="identity") + # bar plot
  theme(legend.key.size = unit(0.1, "cm"), 
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.title=element_text(hjust=0.6, vjust=0.5, size=10)) +
  ggtitle("Macrolides")

# Nitroimidazole
nitromean <- df %>%
  group_by(Continent)%>%
  summarise(mean=mean(nitroimidazole, exclude.NA=T)/1*100)

nitromean <- nitromean[-1,] # Get rid of unspecified row

nitromean <- nitromean %>% 
  mutate(percentage=mean/sum(mean)*100) # convert to percentage

nitrographic <- ggplot(nitromean, aes(x=Continent, y=percentage, fill=Continent)) +
  xlab("") +
  ylab("") +
  geom_bar(stat="identity") + # bar plot
  theme(legend.key.size = unit(0.1, "cm"), 
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.title=element_text(hjust=0.6, vjust=0.5, size=10)) +
  ggtitle("Nitroimidazole")

# Oxazolidinone
oxazmean <- df %>%
  group_by(Continent)%>%
  summarise(mean=mean(oxazolidinone, na.rm=T, exclude.NA=T)/1*100)

oxazmean <- oxazmean[-1,] # Get rid of unspecified row

oxazmean <- oxazmean %>% 
  mutate(percentage=mean/sum(mean)*100) # convert to percentage

oxazgraphic <- ggplot(oxazmean, aes(x=Continent, y=percentage, fill=Continent)) +
  xlab("") +
  ylab("") +
  geom_bar(stat="identity") + # bar plot
  theme(legend.key.size = unit(0.1, "cm"), 
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.title=element_text(hjust=0.6, vjust=0.5, size=10)) +
  ggtitle("Oxazolidinone")

# Phenicol
phemean <- df %>%
  group_by(Continent)%>%
  summarise(mean=mean(phenicol, na.rm=T, exclude.NA=T)/1*100)

phemean <- phemean[-1,] # Get rid of unspecified row

phemean <- phemean %>% 
  mutate(percentage=mean/sum(mean)*100) # convert to percentage

phegraphic <- ggplot(phemean, aes(x=Continent, y=percentage, fill=Continent)) +
  xlab("") +
  ylab("") +
  geom_bar(stat="identity") + # bar plot
  theme(legend.key.size = unit(0.1, "cm"), 
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.title=element_text(hjust=0.6, vjust=0.5, size=10)) +
  ggtitle("Phenicol")

# Pseudomonic acid
pseudomean <- df %>%
  group_by(Continent)%>%
  summarise(mean=mean(pseudomonic_acid, na.rm=T, exclude.NA=T)/1*100)

pseudomean <- pseudomean[-1,] # Get rid of unspecified row

pseudomean <- pseudomean %>% 
  mutate(percentage=mean/sum(mean)*100) # convert to percentage

pseudographic <- ggplot(pseudomean, aes(x=Continent, y=percentage, fill=Continent)) +
  xlab("") +
  ylab("") +
  geom_bar(stat="identity") + # bar plot
  theme(legend.key.size = unit(0.1, "cm"), 
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.title=element_text(hjust=0.6, vjust=0.5, size=10)) +
  ggtitle("Pseudomonic acid")

# Quinolones
quinomean <- df %>%
  group_by(Continent)%>%
  summarise(mean=mean(quinolone, na.rm=T, exclude.NA=T)/1*100)

quinomean <- quinomean[-1,] # Get rid of unspecified row

quinomean <- quinomean %>% 
  mutate(percentage=mean/sum(mean)*100) # convert to percentage

quinographic <- ggplot(quinomean, aes(x=Continent, y=percentage, fill=Continent)) +
  xlab("") +
  ylab("") +
  geom_bar(stat="identity") + # bar plot
  theme(legend.key.size = unit(0.1, "cm"), 
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.title=element_text(hjust=0.6, vjust=0.5, size=10)) +
  ggtitle("Quinolones")

# Rifamycin
rifamean <- df %>%
  group_by(Continent)%>%
  summarise(mean=mean(rifampicin, na.rm=T, exclude.NA=T)/1*100)

rifamean <- rifamean[-1,] # Get rid of unspecified row

rifamean <- rifamean %>% 
  mutate(percentage=mean/sum(mean)*100) # convert to percentage

rifagraphic <- ggplot(rifamean, aes(x=Continent, y=percentage, fill=Continent)) +
  xlab("") +
  ylab("") +
  geom_bar(stat="identity") + # bar plot
  theme(legend.key.size = unit(0.1, "cm"), 
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.title=element_text(hjust=0.6, vjust=0.5, size=10)) +
  ggtitle("Rifamycin")

# Sulphonamides
sulfamean <- df %>%
  group_by(Continent)%>%
  summarise(mean=mean(sulfonamide, na.rm=T, exclude.NA=T)/1*100)

sulfamean <- sulfamean[-1,] # Get rid of unspecified row

sulfamean <- sulfamean %>% 
  mutate(percentage=mean/sum(mean)*100) # convert to percentage

sulfagraphic <- ggplot(sulfamean, aes(x=Continent, y=percentage, fill=Continent)) +
  xlab("") +
  ylab("") +
  geom_bar(stat="identity") + # bar plot
  theme(legend.key.size = unit(0.1, "cm"), 
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.title=element_text(hjust=0.6, vjust=0.5, size=10)) +
  ggtitle("Sulphonamides")

# Tetracyclines
tetmean <- df %>%
  group_by(Continent)%>%
  summarise(mean=mean(tetracycline, na.rm=T, exclude.NA=T)/1*100)

tetmean <- tetmean[-1,] # Get rid of unspecified row

tetmean <- tetmean %>% 
  mutate(percentage=mean/sum(mean)*100) # convert to percentage

tetgraphic <- ggplot(tetmean, aes(x=Continent, y=percentage, fill=Continent)) +
  xlab("") +
  ylab("") +
  geom_bar(stat="identity") + # bar plot
  theme(legend.key.size = unit(0.1, "cm"), 
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.title=element_text(hjust=0.6, vjust=0.5, size=10)) +
  ggtitle("Tetracyclines")

# Trimethoprim
trimean <- df %>%
  group_by(Continent)%>%
  summarise(mean=mean(trimethoprim, na.rm=T, exclude.NA=T)/1*100)

trimean <- trimean[-1,] # Get rid of unspecified row

trimean <- trimean %>% 
  mutate(percentage=mean/sum(mean)*100) # convert to percentage

trigraphic <- ggplot(trimean, aes(x=Continent, y=percentage, fill=Continent)) +
  xlab("") +
  ylab("") +
  geom_bar(stat="identity") + # bar plot
  theme(legend.key.size = unit(0.1, "cm"), 
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.title=element_text(hjust=0.6, vjust=0.5, size=10)) +
  ggtitle("Trimethoprim")

#Joining the graphs - excluded disinfectants as cannot fit
Continent_AMR_Class<-ggarrange(aminographic,betagraphic,fosfographic,
                               macrographic,oxazgraphic,phegraphic,
                               quinographic,sulfagraphic,tetgraphic,trigraphic, disinfectantgraphic)

#Adding the title and the axis
annotate_figure(Continent_AMR_Class, 
                top=text_grob("Percentage Level of Resistance Against the Different Antibiotic Classes Per Continent"), 
                #bottom=text_grob("Continent"), 
                left = text_grob("Level of resistance %",rot = 90, vjust = 1))
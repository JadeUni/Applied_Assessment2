######## Counting from the instances of the different hosts/contintents for each Campylobacter species #####################

# Set working directory
getwd()
setwd("C:/Users/jadea/Documents/Applied Data Sciences/Assessment 2/AMR_Grouped_Campy")
workingdir = "."
getwd() # This is just to check the change worked. 

# load library
library(tm)
library(Hmisc)

#Import data - C.coli
df2 <- read.csv(file = "C.coli_FINAL.csv")
continent_data <- as.data.frame(df2$Continent)
source_data <- as.data.frame(df2$Source)

# Summary statistics of Continents for C. coli
describe(continent_data)
describe(source_data)

# Import data - C. jejuni
df3 <- read.csv(file = "C.jejuni_FINAL.csv")
continent_data_jejuni <- as.data.frame(df3$Continent)
source_data_jejuni <- as.data.frame(df3$Source)

describe(continent_data_jejuni)
describe(source_data_jejuni)

# Import data - C. fetus
df4 <- read.csv(file = "C.fetus_FINAL.csv")
continent_data_fetus <- as.data.frame(df4$Continent)
source_data_fetus <- as.data.frame(df4$Source)

describe(continent_data_fetus)
describe(source_data_fetus)

# Import data - C. hepaticus
df5 <- read.csv(file = "C.hepaticus_FINAL.csv")
continent_data_hepaticus <- as.data.frame(df5$Continent)
source_data_hepaticus <- as.data.frame(df5$Source)

describe(continent_data_hepaticus)
describe(source_data_hepaticus)

# Import data - C. hyointestinalis
df6 <- read.csv(file = "C.hyointestinalis_FINAL.csv")
continent_data_hyointestinalis <- as.data.frame(df6$Continent)
source_data_hyointestinalis <- as.data.frame(df6$Source)

describe(continent_data_hyointestinalis)
describe(source_data_hyointestinalis)

# Import data - C. lari
df7 <- read.csv(file = "C.lari_FINAL.csv")
continent_data_lari <- as.data.frame(df7$Continent)
source_data_lari <- as.data.frame(df7$Source)

describe(continent_data_lari)
describe(source_data_lari)

# Import data - C. sp.
df8 <- read.csv(file = "C.sp_FINAL.csv")
continent_data_sp <- as.data.frame(df8$Continent)
source_data_sp <- as.data.frame(df8$Source)

describe(continent_data_sp)
describe(source_data_sp)

# Import data - C. subantarcticus
df9 <- read.csv(file = "C.subantarcticus_FINAL.csv")
continent_data_subantarcticus <- as.data.frame(df9$Continent)
source_data_subantarcticus <- as.data.frame(df9$Source)

describe(continent_data_subantarcticus)
describe(source_data_subantarcticus)

# Import data - C. upsaliensis
df10 <- read.csv(file = "C.upsaliensis_FINAL.csv")
continent_data_upsaliensis <- as.data.frame(df10$Continent)
source_data_upsaliensis <- as.data.frame(df10$Source)

describe(continent_data_upsaliensis)
describe(source_data_upsaliensis)

# Load Packages
#install.packages('rsconnect')
#install.packages("shinydashboard")
#install.packages('shiny')
library(dplyr)
library(ggplot2)
library(plotly)
library(rsconnect)
library(shinydashboard)
library(shiny)
library(shinythemes)
library(DT)
library(RColorBrewer)

#Set Working Directory (local)
#setwd('C:\\RNAidbtest\\RNAidb')

# Load data frames
RNAidb <- read.csv("Data/RNAidb_out.csv", na.strings = ("NA"), check.names = FALSE)
#load('Data/RNAidb_out.Rda', na.strings = ("N/R")
col_list <- c("Author","Year", "Genus", "Species", "Gene of interest", "Knockdown (KD)")

RNAidbgene <- read.csv("Data/RNAidbgenev3_out.csv")
load('Data/RNAidbgenev3_out.Rda')
col_list_2 <- c("ID","Species")

RNAidbgenome <- read.csv("Data/RNAidbgenomev3_out.csv")
load('Data/RNAidbgenomev3_out.Rda')
col_list_3 <- c("ID","Species")

# Load Unique pubs data
UPPY = unique(subset(RNAidb, select=c("Year", "Title")))
UP <- table(UPPY$Year)
UPCS <- cumsum(UP)
UPT <- data.frame(UP, UPCS)
colnames(UPT) <- c("Year", "Var1","Var2")

# Clean and format every column of the data-frame for data vizualisation tools and protocol builder
c0 <- c(as.numeric(RNAidb$ID))
c1 <- factor(c(as.character(RNAidb$`Primary author`)), ordered = TRUE)
c2 <- c(as.numeric(as.character(RNAidb$Year)))
c3 <- factor(c(as.character(RNAidb$Title)), ordered = TRUE)
c45 <- factor(c(as.character(RNAidb$PMID)), ordered = TRUE)
c4 <- factor(c(as.character(RNAidb$Genus)), ordered = TRUE)
c5 <- factor(c(as.character(RNAidb$Species)), ordered = TRUE)
c6 <- factor(c(as.character(RNAidb$`Strain / Cell type`)), ordered = TRUE)
c7 <- factor(c(as.character(RNAidb$`RNAi trigger type`)), ordered = TRUE)
c8 <- factor(c(as.character(RNAidb$`Product company`)), ordered = TRUE)
c9 <- factor(c(as.character(RNAidb$`RNAi synthesis kit`)), ordered = TRUE)
c10 <- factor(c(as.character(RNAidb$`Synthesis template`)), ordered = TRUE)
c11 <- factor(c(as.character(RNAidb$`Production notes`)), ordered = TRUE)
c12 <- factor(c(as.character(RNAidb$`Lifestage at exposue`)), ordered = TRUE)
c47 <- c(as.numeric(as.character(RNAidb$`Adult age at exposure (days)`)))
c48 <- c(as.logical(RNAidb$`Adult age range?`))
c13 <- factor(c(as.character(RNAidb$`Lifestage notes`)), ordered = TRUE)
c14 <- factor(c(as.character(RNAidb$`Exposure route`)), ordered = TRUE)
c15 <- factor(c(as.character(RNAidb$`Injection device`)), ordered = TRUE)
c16 <- factor(c(as.character(RNAidb$`Transfection reagent`)), ordered = TRUE)
c17 <- factor(c(as.character(RNAidb$`Exposure notes`)), ordered = TRUE)
c46 <- c(as.numeric(as.character(RNAidb$`RNA Concentration (mg/mL)`)))
c18 <- c(as.numeric(as.character(RNAidb$`Volume (nL)`)))
c19 <- c(as.numeric(as.character(RNAidb$`Dose (ng)`)))
c20 <- factor(c(as.character(RNAidb$`Dose notes`)), ordered = TRUE)
c21 <- factor(c(as.character(RNAidb$`Tissue for RT-PCR`)), ordered = TRUE)
c22 <- factor(c(as.character(RNAidb$`Gene of interest`)), ordered = TRUE)
c23 <- factor(c(as.character(RNAidb$`RNAi trigger name`)), ordered = TRUE)
c24 <- factor(c(as.character(RNAidb$Accession)), ordered = TRUE)
c25 <- factor(c(as.character(RNAidb$`Vectorbase ID`)), ordered = TRUE)
c26 <- c(as.numeric(as.character(RNAidb$`Gene length`)))
c27 <- c(as.numeric(as.character(RNAidb$`CDS length`)))
c28 <- c(as.numeric(as.character(RNAidb$`RNAi trigger length`)))
c29 <- c(as.numeric(as.character(RNAidb$`Start position in CDS`)))
c30 <- c(as.numeric(as.character(RNAidb$`Stop position in CDS`)))
c31 <- c(as.numeric(as.character(RNAidb$`CDS coverage (%)`)))
c32 <- factor(c(as.character(RNAidb$`Sense primer / siRNA passenger`)), ordered = TRUE)
c33 <- factor(c(as.character(RNAidb$`Antisense primer / siRNA guide`)), ordered = TRUE)
c34 <- c(as.numeric(as.character(RNAidb$`Knockdown (KD)`)))
c35 <- factor(c(as.character(RNAidb$`KD notes`)), ordered = TRUE)
c36 <- factor(c(as.character(RNAidb$`KD range`)), ordered = TRUE)
c37 <- c(as.numeric(as.character(RNAidb$`KD hrs post-exposure`)))
c49 <- factor(c(as.character(RNAidb$`KD time notes`)), ordered = TRUE)
c38 <- c(as.numeric(as.character(RNAidb$`KD hrs post-bloodmeal`)))
c39 <- factor(c(as.character(RNAidb$Pathogens)), ordered = TRUE)
c40 <- c(as.logical(RNAidb$`Infected during RNAi?`))
c41 <- factor(c(as.character(RNAidb$Notes)), ordered = TRUE)
c42 <- factor(c(as.character(RNAidb$`Multiple entries`)), ordered = TRUE)
c43 <- factor(c(as.character(RNAidb$Error)), ordered = TRUE)
c44 <- factor(c(as.character(RNAidb$Curator)), ordered = TRUE)

#Table of ALL variables with formatting
Table <- data.frame(c0, c1, c2, c3, c45, c4, c5, c6, c7, c8, c9, c10, c11, c12, c47, c48, c13, c14, c15, c16, c17, 
                    c46, c18, c19, c20, c21, c22, c23, c24, c25, c26, c27, c28, c29, c30, c31, c32, c33, 
                    c34, c35, c36, c37, c49, c38, c39, c40, c41, c42, c43, c44)
colnames(Table) <- c("ID",
                     "Primary author",
                     "Year",
                     "Title",
                     "PMID",
                     "Genus",
                     "Species",
                     "Strain / Cell type",
                     "RNAi trigger type",
                     "Product company",
                     "RNAi synthesis kit",
                     "Synthesis template",
                     "Production notes",
                     "Lifestage at exposue",
                     "Adult age at exposure (days)",	
                     "Adult age range?",
                     "Lifestage notes",
                     "Exposure route",
                     "Injection device",
                     "Transfection reagent",
                     "Exposure notes",
                     "RNA Concentration (mg/mL)",
                     "Volume (nL)",
                     "Dose (ng)",
                     "Dose notes",
                     "Tissue for RT-PCR",
                     "Gene of interest",
                     "RNAi trigger name",
                     "Accession",
                     "Vectorbase ID",
                     "Gene length",
                     "CDS length",
                     "RNAi trigger length",
                     "Start position in CDS",
                     "Stop position in CDS",
                     "CDS coverage (%)",
                     "Sense primer / siRNA passenger",
                     "Antisense primer / siRNA guide",
                     "Knockdown (KD)",
                     "KD notes",
                     "KD range",
                     "KD hrs post-exposure",
                     "KD time notes",
                     "KD hrs post-bloodmeal",
                     "Pathogens",
                     "Infected during RNAi?",
                     "Notes",
                     "Multiple entries",
                     "Error",
                     "Curator")

# Table of variables for data Explorer tool
Explorer <- select(Table,
                           "Primary author",
                           "Year",
                           "PMID",
                           "Genus",
                           "Species",
                           "Strain / Cell type",
                           "RNAi trigger type",
                           "Product company",
                           "RNAi synthesis kit",
                           "Synthesis template",
                           "Lifestage at exposue",
                           "Adult age at exposure (days)",	
                           "Adult age range?",
                           "Exposure route",
                           "Injection device",
                           "Transfection reagent",
                           "RNA Concentration (mg/mL)",
                           "Volume (nL)",
                           "Dose (ng)",
                           "Tissue for RT-PCR",
                           "Gene of interest",
                           "RNAi trigger name",
                           "Accession",
                           "Vectorbase ID",
                           "Gene length",
                           "CDS length",
                           "RNAi trigger length",
                           "Start position in CDS",
                           "Stop position in CDS",
                           "CDS coverage (%)",
                           "Sense primer / siRNA passenger",
                           "Antisense primer / siRNA guide",
                           "Knockdown (KD)",
                           "KD range",
                           "KD hrs post-exposure",
                           "KD hrs post-bloodmeal",
                           "Pathogens",
                           "Infected during RNAi?",
                           "Multiple entries")

colnames(Explorer) <- c("Primary.Author",
                        "Year",
                        "PMID",
                        "Genus",
                        "Species",
                        "Strain.or.Cell.type",
                        "RNAi.trigger",
                        "Product.company",
                        "RNAi.synthesis.kit",
                        "Synthesis.template",
                        "Lifestage.at.exposue",
                        "Adult.age.at.exposure.days",	
                        "Adult.age.range",
                        "Exposure.route",
                        "Injection.device",
                        "Transfection.reagent",
                        "RNA.Concentration.mg-mL",
                        "Volume.nL",
                        "Dose.ng",
                        "Tissue.for.RT.PCR",
                        "Gene.of.interest",
                        "RNAi.trigger.name",
                        "Accession",
                        "Vectorbase.ID",
                        "Gene.length",
                        "CDS.length",
                        "RNAi.trigger.length",
                        "Start.position.in.CDS",
                        "Stop.position.in.CDS",
                        "CDS.coverage.Percent",
                        "Sense.primer.siRNA.passenger",
                        "Antisense.primer.siRNA.guide",
                        "Knockdown.KD",
                        "KD.range",
                        "KD.hrs.post.exposure",
                        "KD.hrs.post.bloodmeal",
                        "Pathogens",
                        "Infected.during.RNAi",
                        "Multiple.entries")
nms <- names(Explorer)
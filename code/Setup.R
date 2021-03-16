##########################################################
#                                                        #
#        Denticle Analyses                               #
#           March 16, 2021                               #
#    Elizabeth Sibert and Leah Rubin                     #
#     Supplemental Code for:                             #
#     "An Early Miocene Extinction in Pelagic Sharks"    #
#                                                        #
##########################################################

# Welcome! This script (Setup.R) is a data import and processing 
#  script that can be run in full as source if you keep the file structure
#  for this repository.
# All subsequent R Scripts can run this as source to set the R Environment
#  and run as standalone analyses and figure-generating scripts. 
#
#  NOTE: To run these scripts successfully, be sure to set the working directory.
#     On my computer this is: 
#  setwd("C:/Elizabeth_files/Personal/Research/Code/denticles/")
#     Note that all code is in a directory called 'code'
#        All data csvs are in the 'data' directory
#        All figures output to the 'figures' directory
#
#  This Setup.R file requires the following csv files in a 'data' directory in the working directory: 
#     596_accum.csv - denticle and tooth accumulation rates for DSDP Site 596, from Sibert et al 2016 Proc B
#     886_accum.csv - denticle and tooth accumulation rates for ODP Site 886, from Sibert et al 2016 Proc B
#     dent_LinGeom.csv - this study, a list of denticle types and their major grouping
#     dsdp596.csv - this study, all denticles identified at DSDP Site 596 and their ages/samples
#     odp886.csv - this study, all denticles identified at ODP Site 886 and their ages/samples
#     modern.csv - this study, literature compilation of all modern denticles classified in this study. 
#  This Setuo.R file is dependent on: 
#     functions.R - all custom functions written for these analyses are in functions.R
#  This Setup.R file supports the following scripts: 
#     stats.R - produces a text file with all denticle-related numbers used in the  main text and supplement
#     Figure1.R - produces a PDF file for Figure 1
#     supplementalFigs.R - produces PDF files for denticle-related supplemental figures
# 
#  In addition to the denticle analysis, this code package includes a standalone script: 
#     MioceneMap.R - processes the IODP data and creates a map of Miocene hiatuses, 
#        which is dependent on the modern.csv spreadsheet produced for this study

#######################################################
#                                                     #
#           Setup and Functions                       #
#     Call libraries, functions, load datasets,       #
#        save session info                            #
#                                                     #
#######################################################

#save.image('denticles.RData')
#load('denticles.RData')

##### Libraries #####
library(ichthyoliths) # for range chart fucntion
library(vegan)  # for rarefaction analyses 
library(Hmisc)  # for some of the stats
library(viridis) # for colors
# library(rfishbase) # if necessary to run taxonomy lookup for modern denticles. not necessary in the current scripts

##### Functions & Published Datasets #####
# All functions used in these analyses are stored in functions.R
source('code/functions.R')

# Westerhold 2020 smoothed dataset (for supplemental figure)
westerhold <- read.csv('data/Westerhold_2020_Oxygen_Carbon_smooth.csv')

##### Save SessionInfo to file #####
writeLines(capture.output(sessionInfo()), "sessionInfo.txt")

##################################
#                                #
#        Morphotype Datasets     #
#                                #
##################################

### Morphotype Datasets 
dat.596 <- read.csv('data/dsdp596.csv', header = TRUE, skip = 1)
dat.886 <- read.csv('data/odp886.csv', header = TRUE, skip = 1)
dat.modern <- read.csv('data/modern.csv', header = TRUE, skip = 1)

# Denticles list (used to make dent_LinGeom.csv file and double check names/types but not again afterwards):
# all.dent <- c(as.character(dat.596$Type), as.character(dat.886$Type), as.character(dat.modern$Type))
# all.dent.unique <- sort(unique(all.dent))
# all.dent.unique <- all.dent.unique #remove the blank line values
# writeClipboard(all.dent.unique) #Check that all denticle type names match the list of denticles I have

## Denticle Type Key (must be factor for rangechart function to work)
denticle_geomlin <- read.csv('data/dent_LinGeom.csv', header = TRUE)
denticle_geomlin$Classification <- as.factor(denticle_geomlin$Classification)


##################################
#                                #
#     Accumulation Datasets      #
#                                #
##################################

# 596
accum.596 <- read.csv('data/596_accum.csv', header =TRUE)
names(accum.596)[1] <- 'age'
accum.596$dentAR[39:44] <- accum.596$dentAR[39:44]/4 #normalize the accumulation rate from poor age model interval
accum.596$teethAR[39:44] <- accum.596$teethAR[39:44]/4 #normalize the accumulation rate from poor age model interval

age.breakdown.596 <- c(accum.596$age[39], accum.596$age[44])

# 886
accum.886 <- read.csv('data/886_accum.csv', header = TRUE)
names(accum.886)[1] <- 'age'

#######################################################
#                                                     #
#                 Process Datasets                    #
#     Create counts tables and R objects for plots    #
#                                                     #
#######################################################

##################################
#                                #
#     ODP 886 Denticle Types     #
#                                #
##################################

dat.886 <- dat.886[-270,] #removes row 270, which is coded as NA and is junk

# Age and type variable strings
type.886 <- dat.886$Type
age.886 <- dat.886$Age
# Subset data frame to facilitate use of "table" function 
sub.886<-data.frame(Age = dat.886$Age, Type = dat.886$Type)

# Occurrance_Table including "z" vlues
counts.886.z <- table(sub.886)  # occurrance table
taxa.886.z <- colnames(counts.886.z) # taxa list for 886 
cat.886.z <- type.cat.lookup(taxaList = taxa.886.z, typeList = denticle_geomlin) # categories vector

### Excldue the "z" values
counts.886 <- table(sub.886, exclude='z')  # occurrance table
taxa.886 <- colnames(counts.886) #
cat.886 <- type.cat.lookup(taxaList = taxa.886, typeList = denticle_geomlin)

#### Include *all* 886 samples observed, even those with no denticles
neogene.886 <- c(na.omit(accum.886$age[c(accum.886$age<19.1)])) #pull just the Neogene sample ages
    # The na.omit here is because I added a NA line to make plotting the hiatus easier

# run loop to add rows of zero values for samples with teeth but no denticles
rowname.ages <- as.numeric(rownames(counts.886)) # save the old ranges for loop
counts.886.all <- counts.886
for(i in 1:length(neogene.886)) {
    age.point <- neogene.886[i]
    if(age.point %in% rowname.ages == FALSE) {  
        newrow <- rep(0, length(counts.886.all[1,]))
        counts.886.all <- as.table(rbind(counts.886.all, newrow))
        rownames(counts.886.all)[length(rownames(counts.886.all))] <- age.point
    }
}

rm(i, age.point, newrow, rowname.ages)

# Reorder counts.596.all to be in age order
counts.886.all <- counts.886.all[ order(as.numeric(row.names(counts.886.all))), ]
# Note that taxa.886 and cat.886 remain untouched. 

age.886.all <- as.numeric(rownames(counts.886.all))


##################################
#                                #
#     DSDP 596 Denticle Types    #
#                                #
##################################

# Age and type variable strings
type.596 <- dat.596$Type
age.596 <- dat.596$Age
# Subset data frame to facilitate use of "table" function 
sub.596 <- data.frame(Age = dat.596$Age, Type = dat.596$Type)

# Occurrance_Table including "z" vlues
counts.596.z <- table(sub.596)  #make occurrance table
taxa.596.z <- colnames(counts.596.z)
cat.596.z <- type.cat.lookup(taxaList = taxa.596.z, typeList = denticle_geomlin)

# Exclude z's 
counts.596 <- table(sub.596, exclude='z')  #make occurrance table
taxa.596 <- colnames(counts.596)
cat.596 <- type.cat.lookup(taxaList = taxa.596, typeList = denticle_geomlin)

#### Include *all* 596 depths, even those with no denticles [this is ]
neogene.596 <- accum.596$age[c(accum.596$age<19.1)] #pull just the first post-extinction sample ages

# run loop to add rows of zero values for samples with teeth but no denticles
rowname.ages <- as.numeric(rownames(counts.596)) # save the old ranges for loop
counts.596.all <- counts.596
for(i in 1:length(neogene.596)) {
   age.point <- neogene.596[i]
   if(age.point %in% rowname.ages == FALSE) {  
      newrow <- rep(0, length(counts.596.all[1,]))
      counts.596.all <- as.table(rbind(counts.596.all, newrow))
      rownames(counts.596.all)[length(rownames(counts.596.all))] <- age.point
   }
}

rm(i, age.point, newrow, rowname.ages)

# Reorder counts.596.all to be in age order
counts.596.all <- counts.596.all[ order(as.numeric(row.names(counts.596.all))), ]
# Note that taxa.596 and cat.596 remain untouched. 

age.596.all <- as.numeric(rownames(counts.596.all))

##################################
#                                #
#     Combine 886 and 596        #
#        'fossil' dataset        #
#                                #
##################################

sub.fossil <- rbind(sub.596, sub.886)

counts.fossil <- table(sub.fossil, exclude = 'z')
taxa.fossil <- colnames(counts.fossil)
cat.fossil <- type.cat.lookup(taxaList = taxa.fossil, typeList = denticle_geomlin)

## For figures that combine morphotype lists from 596 and 886 onto the same x-axis labels,
#     but which then are separated to be plotted. Use this for manuscript figures so that 
#     596 and 886 x-axes are comparable directly.
master.tax.all <- sort(taxa.fossil) #used to be called fossils.master.list 
master.tax.596 <- match(taxa.596, master.tax.all) #numerical position of each type at 596 to full list
master.tax.886 <- match(taxa.886, master.tax.all) #numerical position of each type at 886 to full

##################################
#                                #
#     Add the Modern Denticles   #
#                                #
##################################
# ##### Step 0. use rfishbase to look up taxonomic classifications if necessary (NOT RUN) #####
# library(rfishbase)
# ###  6/13/2020: Note that this is no longer necessary, as I have run this for 
# #       all modern denticles and updated the google spreadsheet accordingly This code is here so 
# #       anyone who wants to re-run it can do so. 
# # Add species/genus/etc info to spreadsheet:
# dat.modern$Possible_Name <- gsub("_", " ", dat.modern$SampleID)
# #look up valid names and flag those that do not return a name for future reference
# dat.modern$Accepted_Name <- valid.names.fn(dat.modern$Possible_Name, flag = '__a__')
# 
# # Check this work: Which are the ones that had a name change from Fishabse Lookup function?
# name.change <- subset(dat.modern, dat.modern$Possible_Name != dat.modern$Accepted_Name, select = c(Possible_Name, Accepted_Name))
# ## Checking these manually, fix the errors: (only 1 error)
# # line71 (Apristurus sp) assigned a species name inappropriately, so fix Accepted_Name
# dat.modern$Accepted_Name[71] <- dat.modern$Possible_Name[71]
# 
# # Separate genus out to be just the first word of the Accepted Name (removes flags)
# dat.modern$Genus <- gsub("([A-Za-z]+).*", "\\1", dat.modern$Accepted_Name)
# # Add higher order taxonomy:
# dat.modern$Family <- fishbase[match(dat.modern$Genus, fishbase$Genus),]$Family
# dat.modern$Order <- fishbase[match(dat.modern$Genus, fishbase$Genus),]$Order
# dat.modern$Class <- fishbase[match(dat.modern$Genus, fishbase$Genus),]$Class


##### Create lists of modern denticle types & their categories, and their matches to fossils #####
# taxa.modern is just a unique list of all modern morphotypes
taxa.modern <- unique(as.character(dat.modern$Type))

# sub.modern.rarefy is for rarefaction calculation for modern types... 
sub.modern.rarefy <- data.frame('Age' = rep('modern', length(dat.modern$Type)), Type = dat.modern$Type)

# Identify the types that have matches in the fossil record: 
modern.fossil.matches <- taxa.modern[which(taxa.modern %in% taxa.fossil)] 

##### create modern denticle data frames organized by taxonomic level. #####

###   Consolidate duplicate denticles from the same species/family/order/etc
#     This is necessary because many times a single species/family/order/etc
#     had multiple skin patches/locations coded, and often
#     had the same denticle types in all of those images. 
#   To eliminate double-counting of the same denticle type
#     on the same species/family/order/etc, we consolidated the modern dataset.
#   Each of these datasets is then appended to the counts.fossil to create modern+fossil counts
#     objects for making range chart plots. Note that the counts object used in the main
#     text figure is the one for families. 

##### Species #####
# Empty data frame to fill: 
modern.species <- data.frame(NULL)

# list of taxa to loop over: 
unique.taxa <- unique(dat.modern$Accepted_Name) 

# Make consolidated list of taxa and types so that each type/species combination is counted only once: 
for(i in 1:length(unique.taxa)) {
   df.sub <- subset(dat.modern, dat.modern$Accepted_Name == unique.taxa[i], 
                    select = c('Accepted_Name', 'Genus', 'Family', 'Order', 'Class', 'Age', 'Type'))
   df.sub.unique <- unique(df.sub) 
   modern.species <- rbind(modern.species, df.sub.unique)
}

# clean up
rm(df.sub, df.sub.unique, unique.taxa,i)

### Create counts.fossil.species
sub.modern.species <- data.frame('Age' = rep(0, length(modern.species$Type)), Type = modern.species$Type)
# Note that table(sub.modern.species) now gives us a table with the number of species per denticle type.

# Make occurrence table for all modern.fossil 
sub.fossil.species <- rbind(sub.fossil, sub.modern.species)
counts.fossil.species <- table(sub.fossil.species, exclude = 'z')
taxa.fossil.species <- colnames(counts.fossil.species)
cat.fossil.species <- type.cat.lookup(taxaList = taxa.fossil.species, typeList = denticle_geomlin)

### Create counts.fossil.species.matches
# Excludes modern denticles without fossil analogs

# Only include modern rows that have fossil analog
sub.modern.species.matches <- sub.modern.species[which(sub.modern.species$Type %in% modern.fossil.matches),]

# occurrence table for modern.fossil.matches
sub.fossil.species.matches <- rbind(sub.fossil, sub.modern.species.matches)
counts.fossil.species.matches <- table(sub.fossil.species.matches, exclude = 'z')
taxa.fossil.species.matches <- colnames(counts.fossil.species.matches)
cat.fossil.species.matches <- type.cat.lookup(taxaList = taxa.fossil.species.matches, typeList = denticle_geomlin)

### 596 and 886 only matches 
# This relies on the fact that 596 and 886 have entirely unique ages from each other... . 
counts.596.species <- counts.fossil.species.matches[c(1,which(rownames(counts.fossil.species.matches) %in% unique(age.596))), which(colnames(counts.fossil.species.matches) %in% taxa.596)]

counts.886.species <- counts.fossil.species.matches[c(1,which(rownames(counts.fossil.species.matches) %in% unique(age.886))), which(colnames(counts.fossil.species.matches) %in% taxa.886)]

### Types per Species
# Make table of denticle type by shark species and count the number of types per species
species_type_table <- table(modern.species$Accepted_Name, modern.species$Type)
# write.csv(species_type_table, file = 'data/species_type_table.csv')
types_per_species <- apply(species_type_table, 1, function(x) sum(ifelse(x > 0, 1, 0)))
species_per_type <- apply(species_type_table, 2, function(x) sum(ifelse(x > 0, 1, 0)))




##### Genus #####
# Empty data frame to fill: 
modern.genus <- data.frame(NULL)

# list of taxa to loop over: 
unique.taxa <- unique(dat.modern$Genus) 

# Make consolidated list of taxa and types so that each type/species combination is counted only once: 
for(i in 1:length(unique.taxa)) {
   df.sub <- subset(dat.modern, dat.modern$Genus == unique.taxa[i], 
                    select = c('Genus', 'Family', 'Order', 'Class', 'Age', 'Type'))
   df.sub.unique <- unique(df.sub) 
   modern.genus <- rbind(modern.genus, df.sub.unique)
}

# clean up
rm(df.sub, df.sub.unique, unique.taxa,i)

### Create counts.fossil.genus
sub.modern.genus <- data.frame('Age' = rep(0, length(modern.genus$Type)), Type = modern.genus$Type)
# Note that table(sub.modern.genus) now gives us a table with the number of genus per denticle type.

# Make occurrence table for all modern.fossil 
sub.fossil.genus <- rbind(sub.fossil, sub.modern.genus)
counts.fossil.genus <- table(sub.fossil.genus, exclude = 'z')
taxa.fossil.genus <- colnames(counts.fossil.genus)
cat.fossil.genus <- type.cat.lookup(taxaList = taxa.fossil.genus, typeList = denticle_geomlin)

### Create counts.fossil.genus.matches
# Excludes modern denticles without fossil analogs

# Only include modern rows that have fossil analog
sub.modern.genus.matches <- sub.modern.genus[which(sub.modern.genus$Type %in% modern.fossil.matches),]

# occurrence table for modern.fossil.matches
sub.fossil.genus.matches <- rbind(sub.fossil, sub.modern.genus.matches)
counts.fossil.genus.matches <- table(sub.fossil.genus.matches, exclude = 'z')
taxa.fossil.genus.matches <- colnames(counts.fossil.genus.matches)
cat.fossil.genus.matches <- type.cat.lookup(taxaList = taxa.fossil.genus.matches, typeList = denticle_geomlin)

### 596 and 886 only matches 
# This relies on the fact that 596 and 886 have entirely unique ages from each other... . 
counts.596.genus <- counts.fossil.genus.matches[c(1,which(rownames(counts.fossil.genus.matches) %in% unique(age.596))), which(colnames(counts.fossil.genus.matches) %in% taxa.596)]

counts.886.genus <- counts.fossil.genus.matches[c(1,which(rownames(counts.fossil.genus.matches) %in% unique(age.886))), which(colnames(counts.fossil.genus.matches) %in% taxa.886)]


### Types per genus
genus_type_table <- table(modern.genus$Genus, modern.genus$Type)
# write.csv(genus_type_table, file = 'data/genus_type_table.csv')
types_per_genus <- apply(genus_type_table, 1, function(x) sum(ifelse(x > 0, 1, 0)))
genus_per_type <- apply(genus_type_table, 2, function(x) sum(ifelse(x > 0, 1, 0)))


##### Family #####
# Empty data frame to fill: 
modern.family <- data.frame(NULL)
# list of taxa to loop over: 
unique.taxa <- unique(dat.modern$Family) 
# Make consolidated list of taxa and types so that each type/family combination is counted only once: 
for(i in 1:length(unique.taxa)) {
   df.sub <- subset(dat.modern, dat.modern$Family == unique.taxa[i], 
                    select = c('Family', 'Order', 'Age', 'Type'))
   df.sub.unique <- unique(df.sub) 
   modern.family <- rbind(modern.family, df.sub.unique)
}
# clean up
rm(df.sub, df.sub.unique, unique.taxa,i)

### Create counts.fossil.family
sub.modern.family <- data.frame('Age' = rep(0, length(modern.family$Type)), Type = modern.family$Type)
# Note that table(sub.modern.family) now gives us a table with the number of family per denticle type.

# Make occurrence table for all modern.fossil 
sub.fossil.family <- rbind(sub.fossil, sub.modern.family)
counts.fossil.family <- table(sub.fossil.family, exclude = 'z')
taxa.fossil.family <- colnames(counts.fossil.family)
cat.fossil.family <- type.cat.lookup(taxaList = taxa.fossil.family, typeList = denticle_geomlin)

### Create counts.fossil.family.matches
# Excludes modern denticles without fossil analogs

# Only include modern rows that have fossil analog
sub.modern.family.matches <- sub.modern.family[which(sub.modern.family$Type %in% modern.fossil.matches),]

# occurrence table for modern.fossil.matches
sub.fossil.family.matches <- rbind(sub.fossil, sub.modern.family.matches)
counts.fossil.family.matches <- table(sub.fossil.family.matches, exclude = 'z')
taxa.fossil.family.matches <- colnames(counts.fossil.family.matches)
cat.fossil.family.matches <- type.cat.lookup(taxaList = taxa.fossil.family.matches, typeList = denticle_geomlin)

### 596 and 886 only matches 
# This relies on the fact that 596 and 886 have entirely unique ages from each other... . 
counts.596.family <- counts.fossil.family.matches[c(1,which(rownames(counts.fossil.family.matches) %in% unique(age.596))), which(colnames(counts.fossil.family.matches) %in% taxa.596)]

counts.886.family <- counts.fossil.family.matches[c(1,which(rownames(counts.fossil.family.matches) %in% unique(age.886))), which(colnames(counts.fossil.family.matches) %in% taxa.886)]

### Types per family
family_type_table <- table(modern.family$Family, modern.family$Type)
# write.csv(family_type_table, file = 'data/family_type_table.csv')
types_per_family <- apply(family_type_table, 1, function(x) sum(ifelse(x > 0, 1, 0)))
family_per_type <- apply(family_type_table, 2, function(x) sum(ifelse(x > 0, 1, 0)))


##### Order #####
# Empty data frame to fill: 
modern.order <- data.frame(NULL)
# list of taxa to loop over: 
unique.taxa <- unique(dat.modern$Order) 
# Make consolidated list of taxa and types so that each type/family combination is counted only once: 
for(i in 1:length(unique.taxa)) {
   df.sub <- subset(dat.modern, dat.modern$Order == unique.taxa[i], 
                    select = c('Order', 'Age', 'Type'))
   df.sub.unique <- unique(df.sub) 
   modern.order <- rbind(modern.order, df.sub.unique)
}
# clean up
rm(df.sub, df.sub.unique, unique.taxa,i)

### Create counts.fossil.order
sub.modern.order <- data.frame('Age' = rep(0, length(modern.order$Type)), Type = modern.order$Type)
# Note that table(sub.modern.order) now gives us a table with the number of order per denticle type.

# Make occurrence table for all modern.fossil 
sub.fossil.order <- rbind(sub.fossil, sub.modern.order)
counts.fossil.order <- table(sub.fossil.order, exclude = 'z')
taxa.fossil.order <- colnames(counts.fossil.order)
cat.fossil.order <- type.cat.lookup(taxaList = taxa.fossil.order, typeList = denticle_geomlin)

### Create counts.fossil.order.matches
# Excludes modern denticles without fossil analogs

# Only include modern rows that have fossil analog
sub.modern.order.matches <- sub.modern.order[which(sub.modern.order$Type %in% modern.fossil.matches),]

# occurrence table for modern.fossil.matches
sub.fossil.order.matches <- rbind(sub.fossil, sub.modern.order.matches)
counts.fossil.order.matches <- table(sub.fossil.order.matches, exclude = 'z')
taxa.fossil.order.matches <- colnames(counts.fossil.order.matches)
cat.fossil.order.matches <- type.cat.lookup(taxaList = taxa.fossil.order.matches, typeList = denticle_geomlin)

### 596 and 886 only matches 
# This relies on the fact that 596 and 886 have entirely unique ages from each other... . 
counts.596.order <- counts.fossil.order.matches[c(1,which(rownames(counts.fossil.order.matches) %in% unique(age.596))), which(colnames(counts.fossil.order.matches) %in% taxa.596)]

counts.886.order <- counts.fossil.order.matches[c(1,which(rownames(counts.fossil.order.matches) %in% unique(age.886))), which(colnames(counts.fossil.order.matches) %in% taxa.886)]

### Types per order
order_type_table <- table(modern.order$Order, modern.order$Type)
# write.csv(order_type_table, file = 'data/order_type_table.csv')
types_per_order <- apply(order_type_table, 1, function(x) sum(ifelse(x > 0, 1, 0)))
order_per_type <- apply(order_type_table, 2, function(x) sum(ifelse(x > 0, 1, 0)))


##################################
#                                #
#  Modern Family Matches - T1    #
#                                #
##################################

# This writes a text file that is the backbone for Main Text Table T1 - it identifies all modern denticle types with fossil matches, and spits out which family they may belong to. 

sink('Table1.txt')

for(i in 1:length(modern.fossil.matches)) {
   family.matches <- unique(subset(modern.family, Type == sort(modern.fossil.matches)[i])$Family)
   cat(sort(modern.fossil.matches)[i], ":", paste(sort(family.matches), collapse = ','), sep = '', collapse = '\n')
}

sink()


#######################################################
#                                                     #
#           Run additional analyses                   #
#        range extentions, rarefaction                #
#                                                     #
#######################################################
##################################
#                                #
#     Range Extensions:          #
#        Marshall 1995           #
#        Wang & Marshall 2004    #
#                                #
##################################
##### Citations & Notes #####
# This is based off of the algebra presented in 
#   Marshall, C. R. (1995). "Distinguishing between sudden and gradual extinctions in the fossil 
#       record: Predicting the position of the Cretaceous-Tertiary iridium anomaly using the 
#       ammonite fossil record on Seymour Island, Antarctica." Geology 23(8): 731-734.
# and
#   Wang, S. C. and C. R. Marshall (2004). "Improved Confidence Intervals for Estimating the 
#       Position of a Mass Extinction Boundary." Paleobiology 30(1): 5-18.

## NOTE: For the modern-matches datasets, counts.fossil.species.matches was used, however the 
#   values are the same for all counts.fossil.[taxa].matches datasets, since the only thing that 
#   changes is the number counted in the 0 ('modern') age level, which isn't a part of the calculation

##### Range Extinction calculations done on the full DSDP 596 / ODP 886 / Modern combined dataset #####
### 50% Range Extensions  
# 50% range extntions
extensions.fossil.species.matches.50 <- rangeExtensions(counts.fossil.species.matches, conf = 0.5)
extensions.fossil.species.matches.50$new.lad[extensions.fossil.species.matches.50$new.lad<0] <- 0 #zero out modern extensions... 

# Selecct only the types that went extinct that are not singletons (reduces the total nubmer of denticle types)
ext.fossils.modern.sub.50 <- subset(extensions.fossil.species.matches.50, extensions.fossil.species.matches.50$occurrences > 1 & extensions.fossil.species.matches.50$lads >= 19)

# How many denticles went extinct and are not singletons and can be used for this analysis? 
species.range.extended.50 <- dim(ext.fossils.modern.sub.50)[1]

# Calculate binomial probability distributions for these denticles: 
sp.conf.50.95 <- binom.conf(species.range.extended.50, conf = 0.5, conf.threshold = 0.95, print.output = F, print.error = F)
sp.conf.50.99 <- binom.conf(species.range.extended.50, conf = 0.5, conf.threshold = 0.99, print.output = F, print.error = F)

### 20% Range Extensions 
# 20% range extntions
extensions.fossil.species.matches.20 <- rangeExtensions(counts.fossil.species.matches, conf = 0.2)
extensions.fossil.species.matches.20$new.lad[extensions.fossil.species.matches.20$new.lad<0] <- 0 #zero out modern extensions... 

# Selecct only the types that went extinct that are not singletons (reduces the total nubmer of denticle types)
ext.fossils.modern.sub.20 <- subset(extensions.fossil.species.matches.20, extensions.fossil.species.matches.20$occurrences > 1 & extensions.fossil.species.matches.20$lads >= 19)

# How many denticles went extinct and are not singletons and can be used for this analysis? 
species.range.extended.20 <- dim(ext.fossils.modern.sub.20)[1]

# Calculate binomial probability distributions for these denticles: 
sp.conf.20.95 <- binom.conf(species.range.extended.20, conf = 0.2, conf.threshold = 0.95, print.output = F, print.error = F)
sp.conf.20.99 <- binom.conf(species.range.extended.20, conf = 0.2, conf.threshold = 0.99, print.output = F, print.error = F)


##### Range extinction calculations done only on DSDP Site 596 (with modern survivors) #####
### 50% range extntions
extensions.596.species.matches.50 <- rangeExtensions(counts.596.species, conf = 0.5)
extensions.596.species.matches.50$new.lad[extensions.596.species.matches.50$new.lad<0] <- 0 #zero out modern extensions... 

# Selecct only the types that went extinct that are not singletons (reduces the total nubmer of denticle types)
ext.596.modern.sub.50 <- subset(extensions.596.species.matches.50, extensions.596.species.matches.50$occurrences > 1 & extensions.596.species.matches.50$lads >= 19)

# How many denticles went extinct and are not singletons and can be used for this analysis? 
species.596.range.extended.50 <- dim(ext.596.modern.sub.50)[1]

# Calculate binomial probability distributions for these denticles: 
sp.conf.596.50.95 <- binom.conf(species.596.range.extended.50, conf = 0.5, conf.threshold = 0.95, print.output = F, print.error = F)
sp.conf.596.50.99 <- binom.conf(species.596.range.extended.50, conf = 0.5, conf.threshold = 0.99, print.output = F, print.error = F)

### 20% Range Extensions 
# 20% range extntions
extensions.596.species.matches.20 <- rangeExtensions(counts.596.species, conf = 0.2)
extensions.596.species.matches.20$new.lad[extensions.596.species.matches.20$new.lad<0] <- 0 #zero out modern extensions... 

# Selecct only the types that went extinct that are not singletons (reduces the total nubmer of denticle types)
ext.596.modern.sub.20 <- subset(extensions.596.species.matches.20, extensions.596.species.matches.20$occurrences > 1 & extensions.596.species.matches.20$lads >= 19)

# How many denticles went extinct and are not singletons and can be used for this analysis? 
species.596.range.extended.20 <- dim(ext.596.modern.sub.20)[1]

# Calculate binomial probability distributions for these denticles: 
sp.conf.596.20.95 <- binom.conf(species.596.range.extended.20, conf = 0.2, conf.threshold = 0.95, print.output = F, print.error = F)
sp.conf.596.20.99 <- binom.conf(species.596.range.extended.20, conf = 0.2, conf.threshold = 0.99, print.output = F, print.error = F)


##### Range Extinction calculations done on the FOSSIL DSDP 596 / ODP 886 combined dataset (no modern) #####
### 50% Range Extensions  
# 50% range extntions
extensions.fossil.50 <- rangeExtensions(counts.fossil, conf = 0.5)
extensions.fossil.50$new.lad[extensions.fossil.50$new.lad<0] <- 0 #zero out modern extensions... 

# Selecct only the types that went extinct that are not singletons (reduces the total nubmer of denticle types)
ext.fossils.sub.50 <- subset(extensions.fossil.50, extensions.fossil.50$occurrences > 1 & extensions.fossil.50$lads >= 19)

# How many denticles went extinct and are not singletons and can be used for this analysis? 
fossil.range.extended.50 <- dim(ext.fossils.sub.50)[1]

# Calculate binomial probability distributions for these denticles: 
sp.fossil.conf.50.95 <- binom.conf(fossil.range.extended.50, conf = 0.5, conf.threshold = 0.95, print.output = F, print.error = F)
sp.fossil.conf.50.99 <- binom.conf(fossil.range.extended.50, conf = 0.5, conf.threshold = 0.99, print.output = F, print.error = F)

### 20% Range Extensions 
# 20% range extntions
extensions.fossil.20 <- rangeExtensions(counts.fossil, conf = 0.2)
extensions.fossil.20$new.lad[extensions.fossil.20$new.lad<0] <- 0 #zero out modern extensions... 

# Selecct only the types that went extinct that are not singletons (reduces the total nubmer of denticle types)
ext.fossils.sub.20 <- subset(extensions.fossil.20, extensions.fossil.20$occurrences > 1 & extensions.fossil.20$lads >= 19)

# How many denticles went extinct and are not singletons and can be used for this analysis? 
fossil.range.extended.20 <- dim(ext.fossils.sub.20)[1]

# Calculate binomial probability distributions for these denticles: 
sp.fossil.conf.20.95 <- binom.conf(fossil.range.extended.20, conf = 0.2, conf.threshold = 0.95, print.output = F, print.error = F)
sp.fossil.conf.20.99 <- binom.conf(fossil.range.extended.20, conf = 0.2, conf.threshold = 0.99, print.output = F, print.error = F)


##### Range extinction calculations done only on DSDP Site 596 (no modern matches) #####
### 50% range extntions
extensions.596.50 <- rangeExtensions(counts.596.all, conf = 0.5)
extensions.596.50$new.lad[extensions.596.50$new.lad<0] <- 0 #zero out modern extensions... 

# Selecct only the types that went extinct that are not singletons (reduces the total nubmer of denticle types)
ext.596.sub.50 <- subset(extensions.596.50, extensions.596.50$occurrences > 1 & extensions.596.50$lads >= 19)

# How many denticles went extinct and are not singletons and can be used for this analysis? 
fossil.596.range.extended.50 <- dim(ext.596.sub.50)[1]

# Calculate binomial probability distributions for these denticles: 
sp.conf.596.fossil.50.95 <- binom.conf(fossil.596.range.extended.50, conf = 0.5, conf.threshold = 0.95, print.output = F, print.error = F)
sp.conf.596.fossil.50.99 <- binom.conf(fossil.596.range.extended.50, conf = 0.5, conf.threshold = 0.99, print.output = F, print.error = F)

### 20% Range Extensions 
# 20% range extntions
extensions.596.20 <- rangeExtensions(counts.596.all, conf = 0.2)
extensions.596.20$new.lad[extensions.596.20$new.lad<0] <- 0 #zero out modern extensions... 

# Selecct only the types that went extinct that are not singletons (reduces the total nubmer of denticle types)
ext.596.sub.20 <- subset(extensions.596.20, extensions.596.20$occurrences > 1 & extensions.596.20$lads >= 19)

# How many denticles went extinct and are not singletons and can be used for this analysis? 
fossil.596.range.extended.20 <- dim(ext.596.sub.20)[1]

# Calculate binomial probability distributions for these denticles: 
sp.conf.596.fossil.20.95 <- binom.conf(fossil.596.range.extended.20, conf = 0.2, conf.threshold = 0.95, print.output = F, print.error = F)
sp.conf.596.fossil.20.99 <- binom.conf(fossil.596.range.extended.20, conf = 0.2, conf.threshold = 0.99, print.output = F, print.error = F)


##################################
#                                #
#        Rarefaction Setup       #
#                                #
##################################
## Modern only
rare.modern <- sub.modern.rarefy
rare.modern.counts <- table(sub.modern.rarefy)
# rarecurve(rare.modern.counts)

## Fossil only
rare.fossil <- data.frame(Age = 'fossil', Type = sub.fossil$Type)
rare.fossil.counts <- table(rare.fossil)
# rarecurve(rare.fossil.counts)

## Fossils split by pre- and post 
rare.fossil.split <- data.frame(Age = ifelse(sub.fossil$Age >= 19, 'Pre', 'Post'), Type = sub.fossil$Type)
rare.fossil.split.counts <- table(rare.fossil.split)
# rarecurve(rare.fossil.split.counts)

## Set up Rarefaction on DSDP 596 Subset with equal sample number pre- and post- extinction 
counts.596.pre <- counts.596.all[as.numeric(rownames(counts.596.all))>= 19.36,] # all pre-extinction denticles
counts.596.post <- counts.596.all[as.numeric(rownames(counts.596.all))<= 19.36,] # all post-extinction denticles
counts.596.pre.subset <- counts.596.pre[1:dim(counts.596.post)[1],] #Pull the youngest samples pre-extinction to match the number of samples as post-extinction
# Rarefication Tables
rare.counts.596.post <- apply(counts.596.post, 2, sum)
rare.counts.596.pre <- apply(counts.596.pre, 2, sum)
rare.counts.596.pre.sub <- apply(counts.596.pre.subset, 2, sum)

# ### Number of samples used Rarefaction 
# # 886
# sum(ifelse(as.numeric(rownames(counts.886.all))<19.36, 1, 0)) # 54 samples post-extinciton, 8 with denticles
# sum(ifelse(as.numeric(rownames(counts.886))<19.36, 1, 0)) # 54 samples post-extinciton, 8 with denticles
# sum(ifelse(as.numeric(rownames(counts.886.all))>19.36, 1, 0)) # 17 samples pre-extinction
# 
# # (Note, we looked at all 886 samples post-extinction as listed in accum.886, since there were so few denticles. These were not all added to the "counts.886" matrix as was done in DSDP 596 for samples with no denticles because the addition of tickmarks would have been so dense for the 1.3 million year interval that this sample set spans, it would have become not informative. This is why to count the  total number of samples we looked for denticles in post-extinciton, we use the accumulation document. 
# 
# #596
# sum(ifelse(as.numeric(rownames(counts.596.all))<19.36, 1, 0)) # 38 samples post-extinction
# sum(ifelse(as.numeric(rownames(counts.596))<19.36, 1, 0)) # 38 samples post-extinction, 15 with denticles
# sum(ifelse(as.numeric(rownames(counts.596.all))>19.36, 1, 0)) # 65 samples pre-extinction
# 
# # This all checks out with appropriate numbers of samples and denticles for this dataset. 


##################################
#                                #
#  Statistics output text file   #
#                                #
##################################

source('code/stats.R')
##################################
#                                #
#     Save .RData file           #
#                                #
##################################
save.image('denticles.RData')
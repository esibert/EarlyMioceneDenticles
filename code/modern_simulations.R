#####################################################
#                                                   #
#       Modern_Denticle_Extinction_Simulations      #
#               Sept 28, 2021                       #
#                                                   #
#####################################################

## This code is in response to a technical comment submitted to Science by G.J.P. Naylor et al, which raises 
# the question of whether the  selection of a few individual sharks could account for the 
# >70-90% morphotype extinction observed in the deep-sea microfossil record around 19 Ma
# It relies on the original dataset compiled by Sibert and Rubin in the publication
#       "An Early Miocene Extinction in Pelagic Sharks" 
#   https://science.sciencemag.org/content/372/6546/1105
# The results of this re-analysis will be published as a technical response. 

## The question addressed in this script is: 
# What is the level of extinction required in the modern sharks database to achieve a 70% morphotype extinction? 
# The general method used is to pull in the modern shark denticle diversity database, 
#   and then selectively remove a random subset of shark species from the sample, and re-calculate the 
#   morphotype richness. This "extinction experiment" was performed 10,000 times for each possible
#   number of taxa going extinct, to calculate the range of morphotype extinction percentages for each 
#   possible level of specie-s and genus-level extiction. 
# First, though, we investigate the completeness of the modern denticle database by doing a rarefaction re-sampling
#   at the species level (e.g. how many additional morphotypes does adding one new species add to the total catalog?)
# 
# To access the Setup.R file, as well as the datasets analyzed, please navigate to either: 
#           https://zenodo.org/record/4684080
# or        https://github.com/esibert/EarlyMioceneDenticles/
#   to download and run the prior analyses and datasets that this script depends on. 
## If placed in the correct relative file tree, this file can be run as source and will create the relevant figure. 


##### Setup #####
source('code/Setup.R')

## Using the modern.species dataframe for this analysis, but just running the full, 
#  already published setup.R script for simplicity

## Total denticle richness
modern.richness <- length(unique(modern.species$Type)) #63 denticle types in the modern catalog

## List of modern species 
mod.sp.list <- unique(modern.species$Accepted_Name)

## List of modern genera 
mod.gn.list <- unique(modern.species$Genus)

#################################
#                               #
#           Rarefaction         #
#                               #
#################################

##### Rarefaction Loop for Modern denticle diversity #####
# done on dat.modern, to account for additional pictures 
# Rarefaction is based on species-level sampling. 

rare.sim.modern <- data.frame(NULL)

for(i in 1:length(mod.sp.list)) {
    
    for(j in 1:10000) {
        ## which species are being sampled?
        sp.to.sample <- sample(mod.sp.list, i) # sample i species
        
        ## Which rows of the modern-species dataset match the samples to make go extinct
        sp.to.keep <- which(dat.modern$Accepted_Name %in% sp.to.sample)
        
        ## modified modern dataset with just the species sampled 
        modern.sampled <- dat.modern[sp.to.keep,]
        
        ## re-calculate denticle morphotype richness on just the sampled subset
        modern.sampled.richness <- length(unique(modern.sampled$Type))
        
        ## Extinction counts
        rare.sim.modern[j, i] <- modern.sampled.richness
        
    }
    
}

##### Calculate summary statistics to plot from rare.sim.modern #####
sim.modern.mean <- apply(rare.sim.modern, 2, mean) 
sim.modern.sd <- apply(rare.sim.modern, 2, sd)
sim.modern.95 <- apply(rare.sim.modern, 2, function(x) quantile(x, .95)) 
sim.modern.05 <- apply(rare.sim.modern, 2, function(x) quantile(x, .05)) 
sim.modern.975 <- apply(rare.sim.modern, 2, function(x) quantile(x, .975))
sim.modern.025 <- apply(rare.sim.modern, 2, function(x) quantile(x, .025))


#########################################
#                                       #
#       Simulated extinction loops      #
#           Species-Level               #
#                                       #
#########################################
##### Extinction simulation bootstrap loop #####

## dataframe to store simulations, number of morphotypes going extinct
ext.sp.counts <- data.frame(NULL)

## dataframe to store simulations, percent of morphotypes going extinct
ext.sp.pct <- data.frame(NULL)

## Bootstrap Simulation Loop
for(i in 1:length(mod.sp.list)) {
    
    for(j in 1:10000) {
        ## which species are going extinct?
        sample.to.extinct <- sample(mod.sp.list, i) # species extinction - lose i species
        
        ## Which rows of the modern-species dataset match the samples to make go extinct
        sp.to.remove <- which(modern.species$Accepted_Name %in% sample.to.extinct)
        
        ## modified modern dataset with extinct species removed 
        modern.extinct <- modern.species[-sp.to.remove,]
        
        ## re-calculate denticle morphotype richness of the database with the extinct species removed
        modern.extinct.richness <- length(unique(modern.extinct$Type))
        
        ## Number of morphotypes extinct = total modern richness - richness from database w/ extinct sp. removed
        ext.sp.counts[j, i] <- modern.richness-modern.extinct.richness
        
        ## Percent Extinction = 1-(survivor richness/original richness) * 100%
        ext.sp.pct[j,i] <- (1-(modern.extinct.richness/modern.richness))*100
        
    }
    
}


##### Calculate summary statistics to plot from ext.sp.counts #####
# 70% of 63 morphotypes is 44.1
# 90% of 63 morphotypes is 56.7
ext.sp.counts.mean <- apply(ext.sp.counts, 2, mean) # 70% extinction is 141-142 extinct species (93%)
ext.sp.counts.sd <- apply(ext.sp.counts, 2, sd)
ext.sp.counts.95 <- apply(ext.sp.counts, 2, function(x) quantile(x, .95)) # 70% extinction is 134 extinct species (88%)
ext.sp.counts.05 <- apply(ext.sp.counts, 2, function(x) quantile(x, .05)) #70% morphotype extinction 147 extinct (97%)
ext.sp.counts.975 <- apply(ext.sp.counts, 2, function(x) quantile(x, .975)) # 70% morphotype extinction is 133 extinct (87.5%)
ext.sp.counts.025 <- apply(ext.sp.counts, 2, function(x) quantile(x, .025)) #70% morphotype extinction 148 extinct (97%)


##### Calculate summary statistics to plot from ext.sp.pct #####

ext.sp.pct.mean <- apply(ext.sp.pct, 2, mean) # 70% extinction is 141-142 extinct(93%)
ext.sp.pct.sd <- apply(ext.sp.pct, 2, sd)
ext.sp.pct.95 <- apply(ext.sp.pct, 2, function(x) quantile(x, .95)) # 70% morphotype extinction is 134 extinct (88%)
ext.sp.pct.05 <- apply(ext.sp.pct, 2, function(x) quantile(x, .05)) #70% morphotype extinction 147 extinct (97%)
ext.sp.pct.975 <- apply(ext.sp.pct, 2, function(x) quantile(x, .975)) # 70% morphotype extinction is 133 extinct (87.5%)
ext.sp.pct.025 <- apply(ext.sp.pct, 2, function(x) quantile(x, .025)) #70% morphotype extinction 148 extinct (97%)

# oh good, these match - obviously they should, but its nice to double check! 


#########################################
#                                       #
#       Simulated extinction loops      #
#             Genus-Level               #
#                                       #
#########################################

##### Extinction simulation bootstrap loop #####

## dataframe to store simulations, number of morphotypes going extinct
ext.gn.counts <- data.frame(NULL)

## dataframe to store simulations, percent of morphotypes going extinct
ext.gn.pct <- data.frame(NULL)

## Bootstrap Simulation Loop

for(i in 1:length(mod.gn.list)) {
    
    for(j in 1:10000) {
        ## which genera are going extinct?
        sample.to.extinct <- sample(mod.gn.list, i) # genus extinction - lose i genera
        
        ## Which rows of the modern-species dataset match the samples to make go extinct (selecting by Genus)
        sp.to.remove <- which(modern.species$Genus %in% sample.to.extinct)
        
        ## modified modern dataset with extinct genera removed
        modern.extinct <- modern.species[-sp.to.remove,]
        
        ## re-calculate denticle morphotype richness with extinct genera removed
        modern.extinct.richness <- length(unique(modern.extinct$Type))
        
        ## Number of morphotypes extinct = total modern richness - richness from database w/ extinct genera removed
        ext.gn.counts[j, i] <- modern.richness-modern.extinct.richness
        
        ## Percent Extinction = 1-(survivor richness/original richness) * 100%
        ext.gn.pct[j,i] <- (1-(modern.extinct.richness/modern.richness))*100
        
    }
    
}

##### Calculate summary statistics to plot from ext.gn.pct #####
# 70% of 63 morphotypes is 44.1
# 90% of 63 morphotypes is 56.7

ext.gn.pct.mean <- apply(ext.gn.pct, 2, mean) # 70% morphotype extinction is 56 genera extinct (90%)
ext.gn.pct.sd <- apply(ext.gn.pct, 2, sd)
ext.gn.pct.975 <- apply(ext.gn.pct, 2, function(x) quantile(x, .975)) # 70% morphotype extinction is 50 genera extinct (80%)
ext.gn.pct.025 <- apply(ext.gn.pct, 2, function(x) quantile(x, .025)) #70% morphotype extinction is 60 genera extinct (97%)
ext.gn.pct.95 <- apply(ext.gn.pct, 2, function(x) quantile(x, .95)) # 70% morphotype extinction is 51 genera extinct (82%)
ext.gn.pct.05 <- apply(ext.gn.pct, 2, function(x) quantile(x, .05)) #70% morphotype extinction is 60 genera extinct (96%)


##### Calculate summary statistics to plot from ext.gn.counts #####

ext.gn.counts.mean <- apply(ext.gn.counts, 2, mean) # 70% extinction is 56 genera extinct (90%)
ext.gn.counts.sd <- apply(ext.gn.counts, 2, sd)
ext.gn.counts.975 <- apply(ext.gn.counts, 2, function(x) quantile(x, .975)) # 70% morphotype extinction is 50 genera (80%)
ext.gn.counts.025 <- apply(ext.gn.counts, 2, function(x) quantile(x, .025)) #70% morphotype extinction is 60 genera (97%)
ext.gn.counts.95 <- apply(ext.gn.counts, 2, function(x) quantile(x, .95)) # 70% morphotype extinction is 51 genera (82%)
ext.gn.counts.05 <- apply(ext.gn.counts, 2, function(x) quantile(x, .05)) #70% morphotype extinction is 59-60 genera (96%)


# These also match, also good




#########################################
#                                       #
#       Figure for Submission           #
#                                       #
#########################################
##### Set up figure #####
#
writeFile <- 'pdf'
# writeFile <- 'off'

fig.dims <- c(4.5,12.5) #Set Figure-dimensions

if(writeFile == 'pdf') {
    pdf('figures/response_fig1_simulations.pdf', height = fig.dims[1], width = fig.dims[2], useDingbats = FALSE)
}

par(mfrow = c(1, 3))

##### Figure 1a: rarefaction simulation #####
x.pts <- c(1:152)

# main plot
plot(x.pts, sim.modern.mean, type = 'l', lwd = 2, xlab = '', ylab = '')
polygon(x = c(x.pts, rev(x.pts)), y = c(sim.modern.025, rev(sim.modern.975)), col = adjustcolor('gray70', alpha.f = 0.5))
mtext(side = 3, line = 0.5, at = -5, 'A. Rarefaction', font = 2, adj = 0)
mtext(side = 1, line = 2.5, "Species Sampled")
mtext(side = 2, line = 2.5, "Cumulative Denticle Morphotype Richness")

##### Figure 1b: Species Extinction Simulations #####
x.pts <- ((1:152)/1.52) # 0%-100%

# Main plot
plot(x.pts, ((ext.sp.counts.mean/63)*100), type = 'l', lwd = 2, xlab = '', ylab = '')
mtext(side = 1, line = 2.5, "Simulated % of Species going extinct")
mtext(side = 2, line = 2.5, "Simulated % morphotype extinction")
mtext(side = 3, line = 0.5, at = -5, 'B. Species-level Extinction Simulations', font = 2, adj = 0)


# 70% morphotype extinction
abline(h=70, lty = 2)

# species extinction box
rect(132.5/152*100, -5, 148/152*100, 70, col = adjustcolor('darkgreen', alpha.f = 0.1), border = NA)
segments(x0=141.5/152*100, y0=-5, x1 = 141.5/152*100, y1 = 70)
segments(x0=132.5/152*100, y0=-5, x1 = 132.5/152*100, y1 = 70, lty = 2)
segments(x0=148/152*100, y0=-5, x1 = 148/152*100, y1 = 70, lty = 2)

# error bars
polygon(x = c(x.pts, rev(x.pts)), y = (c(ext.sp.counts.975, rev(ext.sp.counts.025))/63)*100, col = adjustcolor('gray70', alpha.f = 0.5))

# error bar text
text(x = 84, y = 0, round(132.5/152*100,0))
text(x = 90.5, y = 0, round(141.5/152*100,0))
text(x = 99.2, y = 0, round(148/152*100,0))
# axis(side = 1, at = c(round(141.5/152*100,0),round(132.5/152*100,0), round(148/152*100,0)), cex.axis = 0.9, tick = F, col.axis = 'darkgreen', line = -0.7)


##### Figure 1c: Genus Extinction Simulations #####
x.pts <- ((1:62)/0.62) # 0%-100%

# Main plot
plot(x.pts, ((ext.gn.counts.mean/63)*100), type = 'l', lwd = 2, xlab = '', ylab = '')
mtext(side = 1, line = 2.5, "Simulated % of Genera going extinct")
mtext(side = 2, line = 2.5, "Simulated % morphotype extinction")
mtext(side = 3, line = 0.5, at = -5, 'C. Genus-level Extinction Simulations', font = 2, adj = 0)


# 70% morphotype extinction
abline(h=70, lty = 2)

# species extinction box
rect(50/62*100, -5, 60/62*100, 70, col = adjustcolor('darkgreen', alpha.f = 0.1), border = NA)
segments(x0=56/62*100, y0=-5, x1 = 56/62*100, y1 = 70)
segments(x0=50/62*100, y0=-5, x1 = 50/62*100, y1 = 70, lty = 2)
segments(x0=60/62*100, y0=-5, x1 = 60/62*100, y1 = 70, lty = 2)

#error bars
polygon(x = c(x.pts, rev(x.pts)), y = (c(ext.gn.counts.975, rev(ext.gn.counts.025))/63)*100, col = adjustcolor('gray70', alpha.f = 0.5))

# Error bars text
text(x = 78, y = 0, round(50/62*100,0))
text(x = 88, y = 0, round(56/62*100,0))
text(x = 99.5, y = 0, round(60/62*100,0))


##### close file #####
if(writeFile != 'off') {
    dev.off()
}



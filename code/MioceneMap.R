#############################################
#                                           #
#           Miocene Hiatus Map              #
#               3/13/2021                   #                
#                                           #
#############################################

### NOTE: This script is entirely independent of the other scripts used in this
#       manuscript. It does not rely on any post-processed denticles data
#       or any otherwise loaded libraries. It is entirely self-contained and
#       can be run as such. 
## This script can be run in its entirety as "source". It will produce a PDF figure. 
#       If you want a jpg figure, change the writeFile value to 'jpg' below, and if
#       you do not want to produce a file, and have plot show up in R, change to 'off':

writeFile <- 'pdf'
# writeFile <- 'jpg'
# writeFile <- 'off'

##### Necessary libraries #####
library(maps)

##### Call in and process data #####
hiatus <- read.csv('data/Miocene_Hiatus.csv', header = TRUE)

hiatus$Hiatus_status <- factor(hiatus$Hiatus_status) #Turn Hiatus_status value to factor for plot to work... 

## Collapse dat apoints to only have one point per site. Since I considered sites (rather than holes) this is
# the dataset to use for plotting and for the statistics later. 
hiatus.site <- hiatus[match(unique(hiatus$Site), hiatus$Site),]

hiatus.table <- table(hiatus.site$Hiatus_status)

##### Make the map plot #####

# plotting parameters and legend key
pchvec <- c(NA, 1, 16, 16, 16, 16, 16)
colvec <- c(NA, 'gray', 'gray', 'darkred', 'red3', 'blue3', 'lightblue2')
legend.order <- c(5,4, 3, 2, 6, 7) #order of hiatus values presented in the legend

# Make Plot

# pdf('figures/MioceneMap.pdf', height = 7, width = 11, useDingbats = FALSE)

fig.dims <- c(7, 11) #Set Figure-dimensions

if(writeFile == 'pdf') {
    pdf('figures/S13_MioceneMap.pdf', height = fig.dims[1], width = fig.dims[2], useDingbats = FALSE)
}

if(writeFile == 'jpg') {
    jpeg('figures/S13_MioceneMap.jpg', height = fig.dims[1], width = fig.dims[2], units = 'in', res = 300)
}

par(mar = c(3, 3, 6, 2))

plot(hiatus.site$Long, hiatus.site$Lat, pch = pchvec[hiatus.site$Hiatus_status], col = colvec[hiatus.site$Hiatus_status], cex = 1.2)
map(add=T)

legend(legend = c('E. Miocene Hiatus Confirmed', 'E. Miocene Hiatus strongly suspected', 'E. Miocene Hiatus possible, incomplete data', 'Older than E. Miocene but poor coring', 'Complete/Near Complete E. Miocene', 'E. Miocene probably complete'), 
       ncol = 3, xpd = T, x = -200, y = 115, bty = 'n', 
       col = colvec[legend.order], pch = pchvec[legend.order], pt.cex = 1.2)
mtext(side = 3, cex = 1.7, font = 2, text = "Early Miocene Unconformities", line = 3.9)
mtext(side = 3, cex = 1.2, font = 2, text = "The Missing Early Miocene", line = 2.7)

if(writeFile != 'off') {
    dev.off()
}

##### Make the output text file #####
sink("figures/MioceneMap_stats.txt") 

cat('##### Summary Statistics for the Early Miocene Hiatus analysis #####\n')
cat('This analysis is based of of a compilation of shipboard-produced age models for all DSDP, ODP, and IODP expeditions. \n')
cat('Each site was given a unique "hiatus status", which combined all of the holes that were drilled for the site. This is largely based on biostratigraphy, however where available other age constraints were included as well (e.g. paleomag).\n\n')
cat('The categories and designations were based as follows:\n')
cat('0 = Site does not reach below Miocene\n')
cat('1 = E Miocene is not recovered, but it was inferred between spot cores - usually from an DSDP expedition which did not have continuous coring and had large core gaps. Impossible to determine presence or absence of a hiatus\n')
cat('2 = E. Miocene is not recovered, and there is a relatively large age gap (e.g. Middle Miocene to Oligocene) inferred between cores with relatively small coring gaps (e.g. <10 meters between cores). E. Miocene hiatus is likely but not determinable with the available data.\n')
cat('3 = E. Miocene is not identified within a single 9.5m core, indicating a hiatus, but a hiatus is not specified in the site report.\n')
cat('4 = E. Miocene Hiatus Reognized by shipboard biostratigraphers and denoted in site report.\n')
cat('5 = Likely Complete Early Miocene / Hiatus Unlikely - shipboard biostratigraphy is relatively complete and/or estimated sedimentation rates show continuous sedimentation across the Early Miocene.\n')
cat('6 = Possibly complete Early Miocene, but relatively hard to determine based on shipboard data.\n')

cat('\n\n') ## Section break

cat(paste('Out of the ', sum(hiatus.table), ' sites in the DSDP/ODP/IODP database, ', sum(hiatus.table[2:7]), ' extend deeper than the Early Miocene. Of these:\n'))
cat(paste(hiatus.table[2], ' (', round(hiatus.table[2]/sum(hiatus.table[2:7])*100, 2) ,'%) have hiatus status = 1 (indeterminate)\n', sep = ''))
cat(paste(hiatus.table[3], ' (', round(hiatus.table[3]/sum(hiatus.table[2:7])*100, 2) ,'%) have hiatus status = 2 (possible hiatus)\n', sep = ''))
cat(paste(hiatus.table[4], ' (', round(hiatus.table[4]/sum(hiatus.table[2:7])*100, 2) ,'%) have hiatus status = 3 (likely hiatus)\n', sep = ''))
cat(paste(hiatus.table[5], ' (', round(hiatus.table[5]/sum(hiatus.table[2:7])*100, 2) ,'%) have hiatus status = 4 (identified hiatus)\n', sep = ''))
cat(paste(hiatus.table[6], ' (', round(hiatus.table[6]/sum(hiatus.table[2:7])*100, 2) ,'%) have hiatus status = 5 (Likely/complete section)\n', sep = ''))
cat(paste(hiatus.table[7], ' (', round(hiatus.table[7]/sum(hiatus.table[2:7])*100, 2) ,'%) have hiatus status = 6 (possibly complete section)\n', sep = ''))
cat(paste(round(hiatus.table[2]/sum(hiatus.table[2:7])*100, 2), '% are indeterminate, ', round(sum(hiatus.table[3:5])/sum(hiatus.table[2:7])*100, 2), '% have a likely or confirmed hiatus, and ', round(sum(hiatus.table[6:7])/sum(hiatus.table[2:7])*100, 2), '% are probably complete.', sep = ''))


cat('\n\n')

cat('If we discount the Sites where coring is too poor to make a definite determination (hiatus status = 1):\n')
cat(paste(hiatus.table[3], ' (', round(hiatus.table[3]/sum(hiatus.table[3:7])*100, 2) ,'%) have hiatus status = 2 (possible hiatus)\n', sep = ''))
cat(paste(hiatus.table[4], ' (', round(hiatus.table[4]/sum(hiatus.table[3:7])*100, 2) ,'%) have hiatus status = 3 (likely hiatus)\n', sep = ''))
cat(paste(hiatus.table[5], ' (', round(hiatus.table[5]/sum(hiatus.table[3:7])*100, 2) ,'%) have hiatus status = 4 (identified hiatus)\n', sep = ''))
cat(paste(hiatus.table[6], ' (', round(hiatus.table[6]/sum(hiatus.table[3:7])*100, 2) ,'%) have hiatus status = 5 (Likely/complete section)\n', sep = ''))
cat(paste(hiatus.table[7], ' (', round(hiatus.table[7]/sum(hiatus.table[3:7])*100, 2) ,'%) have hiatus status = 6 (possibly complete section)\n', sep = ''))

cat(paste(round(sum(hiatus.table[3:5])/sum(hiatus.table[3:7])*100, 2), '% have a likely or confirmed hiatus, and ', round(sum(hiatus.table[6:7])/sum(hiatus.table[3:7])*100, 2), '% are probably complete.', sep = ''))


cat('\n\n')

cat('If we further discount the Sites with hiatus status = 2:\n')
cat(paste(hiatus.table[4], ' (', round(hiatus.table[4]/sum(hiatus.table[4:7])*100, 2) ,'%) have hiatus status = 3 (likely hiatus)\n', sep = ''))
cat(paste(hiatus.table[5], ' (', round(hiatus.table[5]/sum(hiatus.table[4:7])*100, 2) ,'%) have hiatus status = 4 (identified hiatus)\n', sep = ''))
cat(paste(hiatus.table[6], ' (', round(hiatus.table[6]/sum(hiatus.table[4:7])*100, 2) ,'%) have hiatus status = 5 (Likely/complete section)\n', sep = ''))
cat(paste(hiatus.table[7], ' (', round(hiatus.table[7]/sum(hiatus.table[4:7])*100, 2) ,'%) have hiatus status = 6 (possibly complete section)\n', sep = ''))

cat(paste(round(sum(hiatus.table[4:5])/sum(hiatus.table[4:7])*100, 2), '% have a likely or confirmed hiatus, and ', round(sum(hiatus.table[6:7])/sum(hiatus.table[4:7])*100, 2), '% are probably complete.', sep = ''))


cat('\n\n')

cat('Finally, if we only consider sites that have definitive or near-definitive hiatus status (hiatus status = 4 or hiatus status = 5):\n')
cat(paste(hiatus.table[5], ' (', round(hiatus.table[5]/sum(hiatus.table[5:6])*100, 2) ,'%) have hiatus status = 4 (identified hiatus)\n', sep = ''))
cat(paste(hiatus.table[6], ' (', round(hiatus.table[6]/sum(hiatus.table[5:6])*100, 2) ,'%) have hiatus status = 5 (Likely/complete section)\n', sep = ''))

cat(paste(round(sum(hiatus.table[5])/sum(hiatus.table[5:6])*100, 2), '% have a likely or confirmed hiatus, and ', round(sum(hiatus.table[6])/sum(hiatus.table[5:6])*100, 2), '% are probably complete.', sep = ''))

sink()

#########################################
#                                       #
#       Supplemental Figures            #
#           8/13/2020                   #
#                                       #
#########################################

##### SETUP #####

source('code/Setup.R')  #Call in and process the datasets as done in Setup.R script

writeFile <- 'pdf'
# writeFile <- 'jpg'
# writeFile <- 'off'

#####################################
#                                   #
#       Supplemental Figures        #
#                                   #
#####################################

##### S3. Modern Denticles per taxon histograms figue #####

fig.dims <- c(6, 11) #Set Figure-dimensions

if(writeFile == 'pdf') {
    pdf('figures/S3_Denticle_type_histograms.pdf', height = fig.dims[1], width = fig.dims[2], useDingbats = FALSE)
}

if(writeFile == 'jpg') {
    jpeg('figures/S3_Denticle_type_histograms.jpg', height = fig.dims[1], width = fig.dims[2], units = 'in', res = 300)
}



# jpeg(filename = 'figures/Denticle_type_histograms.jpg', width = 10, height = 6, units = 'in', res = 150)
par(mfrow = c(2, 4))
xaxis.lab <- 'Denticle Types'

# types per species
dat_to_plot <- types_per_species
counts <- hist(dat_to_plot, breaks = c(0:max(dat_to_plot)), main = "Denticle Types per Species", xlab = xaxis.lab)$counts
sdat <- summary(dat_to_plot)
summStr <- paste(names(sdat), format(sdat, digits = 3), collapse = "\n ")
text(summStr, x = (4*max(dat_to_plot))/5, y = (4*max(counts))/5)

# types per Genus
dat_to_plot <- types_per_genus
counts <- hist(dat_to_plot, breaks = c(0:max(dat_to_plot)), main = "Denticle Types per Genus", xlab = xaxis.lab)$counts
sdat <- summary(dat_to_plot)
summStr <- paste(names(sdat), format(sdat, digits = 3), collapse = "\n ")
text(summStr, x = (4*max(dat_to_plot))/5, y = (4*max(counts))/5)


# types per Family
dat_to_plot <- types_per_family
counts <- hist(dat_to_plot, breaks = c(0:max(dat_to_plot)), main = "Denticle Types per Family", xlab = xaxis.lab)$counts
sdat <- summary(dat_to_plot)
summStr <- paste(names(sdat), format(sdat, digits = 3), collapse = "\n ")
text(summStr, x = (4*max(dat_to_plot))/5, y = (4*max(counts))/5)

# types per Order
dat_to_plot <- types_per_order
counts <- hist(dat_to_plot, breaks = c(0:max(dat_to_plot)), main = "Denticle Types per Order", xlab = xaxis.lab)$counts
sdat <- summary(dat_to_plot)
summStr <- paste(names(sdat), format(sdat, digits = 3), collapse = "\n ")
text(summStr, x = (4*max(dat_to_plot))/5, y = (4*max(counts))/5)

# Species per type
dat_to_plot <- species_per_type
counts <- hist(dat_to_plot, breaks = c(0:max(dat_to_plot)), main = "Species per denticle type", xlab = '# of Species per type')$counts
sdat <- summary(dat_to_plot)
summStr <- paste(names(sdat), format(sdat, digits = 3), collapse = "\n ")
text(summStr, x = (4*max(dat_to_plot))/5, y = (4*max(counts))/5)

# Genera per type
dat_to_plot <- genus_per_type
counts <- hist(dat_to_plot, breaks = c(0:max(dat_to_plot)), main = "Genera per denticle type", xlab = '# of Genera per type')$counts
sdat <- summary(dat_to_plot)
summStr <- paste(names(sdat), format(sdat, digits = 3), collapse = "\n ")
text(summStr, x = (4*max(dat_to_plot))/5, y = (4*max(counts))/5)

# Family per type
dat_to_plot <- family_per_type
counts <- hist(dat_to_plot, breaks = c(0:max(dat_to_plot)), main = "Families per denticle type", xlab = '# of Families per type')$counts
sdat <- summary(dat_to_plot)
summStr <- paste(names(sdat), format(sdat, digits = 3), collapse = "\n ")
text(summStr, x = (4*max(dat_to_plot))/5, y = (4*max(counts))/5)

# Order per type
dat_to_plot <- order_per_type
counts <- hist(dat_to_plot, breaks = c(0:max(dat_to_plot)), main = "Orders per denticle type", xlab = '# of Orders per type')$counts
sdat <- summary(dat_to_plot)
summStr <- paste(names(sdat), format(sdat, digits = 3), collapse = "\n ")
text(summStr, x = (4*max(dat_to_plot))/5, y = (4*max(counts))/5)


if(writeFile != 'off') {
    dev.off()
}




##### S4. Range chart including all fossil and modern denticle types, not just "matches" #####

fig.dims <- c(9,22) #Set Figure-dimensions

if(writeFile == 'pdf') {
    pdf('figures/S4_modern_full_Rangechart.pdf', height = fig.dims[1], width = fig.dims[2], useDingbats = FALSE)
}

if(writeFile == 'jpg') {
    jpeg('figures/S4_modern_full_Rangechart.jpg', height = fig.dims[1], width = fig.dims[2], units = 'in', res = 300)
}



# Range chart DSDP 596
layout(1)
par(mar = c(5.1, 4.1, 4.1, 2.1)) # reset margins
par(oma = c(0,0,0,0))

modern.fossil.legend <- rangechart(counts.fossil.family, reorder = 'lad.by.fad', normalize.counts = FALSE, 
                                   col.points = 'by.count', cols.vec = 'viridis', count.breaks = c(0, 1, 3, 5),
                                   cex.points = 'by.count', largesize = 1,
                                   tax.cat = cat.fossil.family, pch.points = 'by.category', pch.vec = c(15, 16, 17, 5), 
                                   xaxis.labels = 'alphanum', yaxis.ticks = TRUE,
                                   print.xaxis = T, main = '', ylab = '')
# add tick marks and annotations
axis(2, at = rownames(counts.596.all), labels = FALSE, tcl = -0.3) #596 ticks on left axis
# Add annotations
mtext("DSDP 596 and ODP 886 with all modern denticle types included", side = 3, line = 2, font = 2)
mtext("Age (Ma)", side = 2, line = 2.7)
mtext("Denticle Morphotype", side = 1, line = 3)
abline(h = 19, col = 'gray', lty = 2, lwd = 2)
legend('topleft', legend = c(levels(denticle_geomlin$Classification)[1:4], '1', '2-3', '4-5', '6+'),
       pch = c(15, 16, 17, 5, 16, 16, 16, 16),
       col = c(rep('black', 4), viridis(4)),
       pt.cex = c(rep(1, 5), 1.33, 1.67, 2),
       ncol = 2, title = '     Type              Count', title.adj = 0)

text("Modern denticle types with no fossil analog", x = 105, y = 2)


if(writeFile != 'off') {
    dev.off()
}


## Write legend file
sink("figures/modern_fossil_xaxis_Legend.txt") 

cat('##### Range Chart Legend for combined Fossil & ALL Modern denticles Supplemental Figure #####\n')
cat(paste(1:length(modern.fossil.legend), '. ', modern.fossil.legend, sep = '', collapse = '\n'))

sink()


##### S5. Rarefaction curves figure #####
# 1 = all
# 2 = fossils (all)
# 3 = modern
# 4 = post-extinction
# 5 = pre-extinction


fig.dims <- c(8,8) #Set Figure-dimensions

if(writeFile == 'pdf') {
    pdf('figures/S5_rarefaction.pdf', height = fig.dims[1], width = fig.dims[2], useDingbats = FALSE)
}

if(writeFile == 'jpg') {
    jpeg('figures/S5_rarefaction.jpg', height = fig.dims[1], width = fig.dims[2], units = 'in', res = 300)
}


cols <- c('red', 'orange', 'blue', 'darkgreen', 'gray30')

par(fig=c(0, 1, 0, 1), new = F, mar = c(4,4,4,4))
rarecurve(rare.fossil.modern.split.all.counts, col = cols, main = 'Rarefaction of Denticle Types')

rect(xleft = 0, ybottom = 0, xright = 50, ytop = 30, lty = 2)

mtext("a", side = 3, line = 0, font = 2, adj = 0, cex = 1.2)

legend('topleft', legend = rownames(rare.fossil.modern.split.all.counts), col = cols, lty = 1)

par(fig=c(0.5, 0.99, 0.01, 0.5), new = T)
rarecurve(rare.fossil.modern.split.all.counts, xlim = c(0, 50), col = cols, ylim = c(0, 30), axes = FALSE, xlab = '', ylab = '')
box()
axis(2)
axis(3)
mtext("Species", side = 2, line = 2.5)
mtext("Sample Size", side = 3, line = 2.5)


text("b", x = 0.5, y = 29.5, font = 2, cex = 1.2)


if(writeFile != 'off') {
    dev.off()
}
##### S6. Range chart with 50% Range Extensions #####

fig.dims <- c(7.5, 13) #Set Figure-dimensions

if(writeFile == 'pdf') {
    pdf('figures/S6_50_rangeExtensions.pdf', height = fig.dims[1], width = fig.dims[2], useDingbats = FALSE)
}

if(writeFile == 'jpg') {
    jpeg('figures/S6_50_rangeExtensions.jpg', height = fig.dims[1], width = fig.dims[2], units = 'in', res = 300)
}


# Plotting Variables needed in this script:
age.range.full <- c(max(as.numeric(rownames(counts.fossil.family.matches))), 0)

# Define multi-panel
m <- rbind(c(1, 1, 1, 1, 1, 1,1, 1, 2, 3))

layout(m)
par(oma = c(0, 0, 4, 4))
# layout.show(3) #check if layout works

## 1, Range Chart 
par(mar = c(5.1, 4.1, 4.1, 0))
rangechart(counts.fossil.family.matches, reorder = 'lad.by.fad', normalize.counts = FALSE, 
           col.points = 'by.count', cols.vec = 'viridis', count.breaks = c(0, 1, 3, 5),
           cex.points = 'by.count', largesize = 1,
           tax.cat = cat.fossil.family.matches, pch.points = 'by.category', pch.vec = c(15, 16, 17, 5), 
           xaxis.labels = 'alphanum', yaxis.ticks = TRUE,
           print.xaxis = F, main = '', ylab = '',
           rangeExtensions = extensions.fossil.species.matches.50$new.lad, elty = 1, elcol = 'firebrick')
rect(-10, sort(ext.fossils.modern.sub.50$new.lad)[sp.conf.50.95[1]], 100, sort(ext.fossils.modern.sub.50$new.lad)[sp.conf.50.95[2]], col = 'gray', border = NA)
# abline(h = quantile(ext.fossils.modern.sub.20$new.lad, 0.2), col = 'darkred', lwd = 2) # Median estimate
par(new = T)
rangechart(counts.fossil.family.matches, reorder = 'lad.by.fad', normalize.counts = FALSE, 
           col.points = 'by.count', cols.vec = 'viridis', count.breaks = c(0, 1, 3, 5), 
           cex.points = 'by.count', largesize = 1,
           tax.cat = cat.fossil.family.matches, pch.points = 'by.category', pch.vec = c(15, 16, 17, 5), 
           xaxis.labels = 'alphanum', yaxis.ticks = TRUE,
           print.xaxis = F, main = '', ylab = '',
           rangeExtensions = extensions.fossil.species.matches.50$new.lad, elty = 1, elcol = 'firebrick')
# add tick marks and annotations
axis(2, at = rownames(counts.886), labels = FALSE, tcl = -0.3) #886 ticks on left axis as well
axis(2, at = rownames(counts.596.all), labels = FALSE, tcl = -0.3) #596 ticks on left axis
# Add annotations
mtext("DSDP 596 and ODP 886 combined with Modern - 50% Range Extensions", side = 3, line = 2, font = 2)
mtext("Age (Ma)", side = 2, line = 2.7)
mtext("Denticle Morphotype", side = 1, line = 3)
# abline(h = 19, col = 'gray', lty = 2, lwd = 2)

legend('topleft', legend = c(levels(denticle_geomlin$Classification)[1:4], '1', '2-3', '4-5', '6+'),
       pch = c(15, 16, 17, 5, 16, 16, 16, 16),
       col = c(rep('black', 4), viridis(4)),
       pt.cex = c(rep(1, 5), 1.33, 1.67, 2),
       ncol = 2, title = '     Type              Count', title.adj = 0)
## Include 95% and 99% confidence intervals? Maybe not... 
# abline(h = sort(ext.fossils.modern.sub.20$new.lad)[sp.conf.50.95[1]], col = 'red', lwd = 1, lty = 3) # 95% young 
# abline(h = sort(ext.fossils.modern.sub.20$new.lad)[sp.conf.50.95[2]], col = 'red', lwd = 1, lty = 3) # 95% old 
# abline(h = sort(ext.fossils.modern.sub.20$new.lad)[sp.conf.50.99[1]], col = 'blue', lwd = 1, lty = 3) # 99% young 
# abline(h = sort(ext.fossils.modern.sub.20$new.lad)[sp.conf.50.99[2]], col = 'blue', lwd = 1, lty = 3) # 99% old 

##2.ODP 596 accumulatoin

par(mar = c(5.1, 0, 4.1, 0))

plot(y = accum.596$age, x = accum.596$dentAR, type = 'o', 
     ylim = age.range.full, xlim = c(0, 25),
     xlab = '', ylab = '', col = 'firebrick', pch = 16, axes = F, cex = 0.7)
rect(-10, sort(ext.fossils.modern.sub.50$new.lad)[sp.conf.50.95[1]], 100, sort(ext.fossils.modern.sub.50$new.lad)[sp.conf.50.95[2]], col = 'gray', border = NA)
lines(y = accum.596$age, x = accum.596$dentAR, type = 'o', col = 'firebrick', pch = 16, cex = 0.7)
box()
axis(2, labels = FALSE)
axis(1, col = 'firebrick', col.axis = 'firebrick')
# mtext('Denticles/cm2/myr', side = 1, line = 3, col = 'firebrick')
par(new=TRUE)
plot(y = accum.596$age, x = (accum.596$dent/accum.596$teeth), type = 'o', 
     ylim = age.range.full,  xlim = c(0, 0.5), 
     xlab = '', ylab = '', col = 'darkblue', pch=17, axes = F, cex = 0.7)
axis(3, col = 'darkblue', col.axis = 'darkblue')
# mtext('Denticles/Teeth ratio', side = 3, line = 2, col = 'darkblue')

# abline(h = 19, col = 'gray', lty = 2, lwd = 2)

legend('bottomright', legend = c("South Pacific", "DSDP 596"), bty = 'n')



## 3. DSDP 886 Accumulation

par(mar = c(5.1, 0, 4.1, 0))

plot(y = accum.886$age, x = accum.886$dentAR, type = 'o', 
     ylim = age.range.full, #xlim = c(0, 40), 
     xlab = '', ylab = '', col = 'firebrick', pch = 16, axes = F, cex = 0.7)
rect(-10, sort(ext.fossils.modern.sub.50$new.lad)[sp.conf.50.95[1]], 100, sort(ext.fossils.modern.sub.50$new.lad)[sp.conf.50.95[2]], col = 'gray', border = NA)
lines(y = accum.886$age, x = accum.886$dentAR, type = 'o', col = 'firebrick', pch = 16, cex = 0.7)
box()
axis(2, labels = F)
axis(1, col = 'firebrick', col.axis = 'firebrick')
# mtext('Denticles/cm2/myr', side = 1, line = 2, col = 'firebrick')
par(new=TRUE)
plot(y=accum.886$age, x=(accum.886$dent/accum.886$teeth), type = 'o', 
     ylim = age.range.full,
     xlab = '', ylab = '', col = 'darkblue', pch=17, axes = F, cex = 0.7)
axis(3, col = 'darkblue', col.axis = 'darkblue')
# mtext('Denticles/Teeth ratio', side = 3, line = 2, col = 'darkblue')
mtext('Age (Ma)', side = 4, line = 2.7)
axis(4, las = 1)

# abline(h = 19, col = 'gray', lty = 2, lwd = 2)

legend('bottomright', legend = c("North Pacific", "ODP 886"), bty = 'n')

# Add annotations to both graphs
mtext('Denticles/Teeth ratio', side = 3, line = 3, col = 'darkblue', at = 0)
# mtext('Denticles/cm2/myr', side = 1, line = 3, col = 'firebrick', at = 0)
mtext(expression("Denticles " * cm^-2 * myr^-1), side = 1, line = 3, col = 'firebrick', at = 0)


legend('topright', legend = c('Denticle absolute abundance', 'Denticle relative abundance'), lty = 1, pch = c(16, 17), col = c('firebrick', 'darkblue'), cex = 0.9)


if(writeFile != 'off') {
    dev.off()
}


##### S8. DSDP 596 and ODP 886 FOSSILS ONLY: Range chart + accumulation #####

fig.dims <- c(7.5, 13) #Set Figure-dimensions

if(writeFile == 'pdf') {
    pdf('figures/S8_596_886_Fossils_Only_Rangechart.pdf', height = fig.dims[1], width = fig.dims[2], useDingbats = FALSE)
}

if(writeFile == 'jpg') {
    jpeg('figures/S8_596_886_Fossils_Only_Rangechart.jpg', height = fig.dims[1], width = fig.dims[2], units = 'in', res = 300)
}


# Plotting Variables needed in this script:
age.range.fossils <- c(max(as.numeric(rownames(counts.fossil))), min(as.numeric(rownames(counts.fossil))))

# Define multi-panel
m <- rbind(c(1, 1, 1, 1, 1, 1,1, 1, 2, 3))

layout(m)
par(oma = c(0, 0, 0, 4))
# layout.show(3) #check if layout works

## 1, Range Chart 
par(mar = c(5.1, 4.1, 4.1, 0))
rangechart(counts.fossil, reorder = 'lad.by.fad', normalize.counts = FALSE, 
           col.points = 'by.count', cols.vec = 'viridis', count.breaks = c(0, 1, 3, 5),
           cex.points = 'by.count', largesize = 1,
           tax.cat = cat.fossil.family.matches, pch.points = 'by.category', pch.vec = c(15, 16, 17, 5), 
           xaxis.labels = 'alphanum', yaxis.ticks = TRUE,
           print.xaxis = F, main = '', ylab = '')
# add tick marks and annotations
axis(2, at = rownames(counts.886), labels = FALSE, tcl = -0.3) #886 ticks on left axis as well
axis(2, at = rownames(counts.596.all), labels = FALSE, tcl = -0.3) #596 ticks on left axis
# Add annotations
mtext("DSDP 596 and ODP 886 Fossils Only", side = 3, line = 2, font = 2)
mtext("Age (Ma)", side = 2, line = 2.7)
mtext("Denticle Morphotype", side = 1, line = 3)
abline(h = 19, col = 'gray', lty = 2, lwd = 2)
legend('topleft', legend = c(levels(denticle_geomlin$Classification)[1:4], '1', '2-3', '4-5', '6+'),
       pch = c(15, 16, 17, 5, 16, 16, 16, 16),
       col = c(rep('black', 4), viridis(4)),
       pt.cex = c(rep(1, 5), 1.33, 1.67, 2),
       ncol = 2, title = '     Type              Count', title.adj = 0)


##2.ODP 596 accumulatoin

par(mar = c(5.1, 0, 4.1, 0))

plot(y = accum.596$age, x = accum.596$dentAR, type = 'o', 
     ylim = age.range.fossils, xlim = c(0, 25),
     xlab = '', ylab = '', col = 'firebrick', pch = 16, axes = F, cex = 0.7)
box()
axis(2, labels = FALSE)
axis(1, col = 'firebrick', col.axis = 'firebrick')
# mtext('Denticles/cm2/myr', side = 1, line = 3, col = 'firebrick')
par(new=TRUE)
plot(y = accum.596$age, x = (accum.596$dent/accum.596$teeth), type = 'o', 
     ylim = age.range.fossils,  xlim = c(0, 0.5), 
     xlab = '', ylab = '', col = 'darkblue', pch=17, axes = F, cex = 0.7)
axis(3, col = 'darkblue', col.axis = 'darkblue')
# mtext('Denticles/Teeth ratio', side = 3, line = 2, col = 'darkblue')
abline(h = 19, col = 'gray', lty = 2, lwd = 2)

legend('bottomright', legend = c("South Pacific", "DSDP 596"), bty = 'n')



## 3. DSDP 886 Accumulation

par(mar = c(5.1, 0, 4.1, 0))


plot(y = accum.886$age, x = accum.886$dentAR, type = 'o', 
     ylim = age.range.fossils, #xlim = c(0, 40), 
     xlab = '', ylab = '', col = 'firebrick', pch = 16, axes = F, cex = 0.7)
box()
axis(2, labels = F)
axis(1, col = 'firebrick', col.axis = 'firebrick')
# mtext('Denticles/cm2/myr', side = 1, line = 2, col = 'firebrick')
par(new=TRUE)
plot(y=accum.886$age, x=(accum.886$dent/accum.886$teeth), type = 'o', 
     ylim = age.range.fossils,
     xlab = '', ylab = '', col = 'darkblue', pch=17, axes = F, cex = 0.7)
axis(3, col = 'darkblue', col.axis = 'darkblue')
# mtext('Denticles/Teeth ratio', side = 3, line = 2, col = 'darkblue')
mtext('Age (Ma)', side = 4, line = 2.7)
axis(4, las = 1)

legend('bottomright', legend = c("North Pacific", "ODP 886"), bty = 'n')

# Add annotations to both graphs
mtext('Denticles/Teeth ratio', side = 3, line = 2, col = 'darkblue', at = 0)
# mtext('Denticles/cm2/myr', side = 1, line = 3, col = 'firebrick', at = 0)
mtext(expression("Denticles " * cm^-2 * myr^-1), side = 1, line = 3, col = 'firebrick', at = 0)
abline(h = 19, col = 'gray', lty = 2, lwd = 2)


legend('topright', legend = c('Denticle absolute abundance', 'Denticle relative abundance'), lty = 1, pch = c(16, 17), col = c('firebrick', 'darkblue'), cex = 1)


if(writeFile != 'off') {
    dev.off()
}

##### S9. DSDP 596 FOSSILS ONLY: Range chart + accumulation #####

fig.dims <- c(6, 12.5) #Set Figure-dimensions

if(writeFile == 'pdf') {
    pdf('figures/S9_596_fossils_only_Rangechart.pdf', height = fig.dims[1], width = fig.dims[2], useDingbats = FALSE)
}

if(writeFile == 'jpg') {
    jpeg('figures/S9_596_fossils_only_Rangechart.jpg', height = fig.dims[1], width = fig.dims[2], units = 'in', res = 300)
}



m <- rbind(c(1, 1, 1, 1, 1, 2))
layout(m)
par(oma = c(0, 0, 0, 4))

age.range.596.fossils <- c(max(age.596.all), min(age.596.all))

# Range chart DSDP 596
par(mar = c(5.1, 4.1, 4.1, 0))

rangechart(counts.596.all, reorder = 'lad.by.fad', normalize.counts = FALSE, 
           tax.cat = cat.fossil.family.matches[master.tax.596], taxa = master.tax.596, 
           col.points = 'by.count', cols.vec = 'viridis', count.breaks = c(0, 1, 3, 5),
           pch.points = 'by.category', pch.vec = c(15, 16, 17, 18), 
           cex.points = 'by.count', largesize = 1,
           xaxis.labels = 'names', yaxis.ticks = TRUE, 
           print.xaxis = F, main = '', ylab = '')
# add tick marks and annotations
axis(2, at = rownames(counts.596.all), labels = FALSE, tcl = -0.3) #596 ticks on left axis
# Add annotations
mtext("DSDP 596 Fossils Only", side = 3, line = 2, font = 2)
mtext("Age (Ma)", side = 2, line = 2.7)
mtext("Denticle Morphotype", side = 1, line = 3)
abline(h = 19, col = 'gray', lty = 2, lwd = 2)
legend('topleft', legend = c(levels(denticle_geomlin$Classification)[1:4], '1', '2-3', '4-5', '6+'),
       pch = c(15, 16, 17, 5, 16, 16, 16, 16),
       col = c(rep('black', 4), viridis(4)),
       pt.cex = c(rep(1, 5), 1.33, 1.67, 2),
       ncol = 2, title = '     Type              Count', title.adj = 0)


##2.DSDP 596 accumulatoin

par(mar = c(5.1, 0, 4.1, 0))

plot(y = accum.596$age, x = accum.596$dentAR, type = 'o', 
     ylim = age.range.596.fossils, xlim = c(0, 25),
     xlab = '', ylab = '', col = 'firebrick', pch = 16, axes = F, cex = 0.7)
box()
axis(2, labels = FALSE)
axis(1, col = 'firebrick', col.axis = 'firebrick')
mtext(expression("Denticles " * cm^-2 * myr^-1), side = 1, line = 3, col = 'firebrick')
# mtext('Denticles/cm2/myr', side = 1, line = 3, col = 'firebrick')
par(new=TRUE)
plot(y = accum.596$age, x = (accum.596$dent/accum.596$teeth), type = 'o', 
     ylim = age.range.596.fossils,  xlim = c(0, 0.5), 
     xlab = '', ylab = '', col = 'darkblue', pch=17, axes = F, cex = 0.7)
axis(3, col = 'darkblue', col.axis = 'darkblue')
mtext('Denticles/Teeth ratio', side = 3, line = 2, col = 'darkblue')
abline(h = 19, col = 'gray', lty = 2, lwd = 2)

mtext('Age (Ma)', side = 4, line = 2.7)
axis(4, las = 1)

legend('topright', legend = c('Denticle absolute abundance', 'Denticle relative abundance'), lty = 1, pch = c(16, 17), col = c('firebrick', 'darkblue'), cex = 0.8)


if(writeFile != 'off') {
    dev.off()
}


##### S10. DSDP 596 and modern: Range chart + accumulation #####

fig.dims <- c(6, 12.5) #Set Figure-dimensions

if(writeFile == 'pdf') {
    pdf('figures/S10_596_modern_Rangechart.pdf', height = fig.dims[1], width = fig.dims[2], useDingbats = FALSE)
}

if(writeFile == 'jpg') {
    jpeg('figures/S10_596_modern_Rangechart.jpg', height = fig.dims[1], width = fig.dims[2], units = 'in', res = 300)
}


m <- rbind(c(1, 1, 1, 1, 1, 2))
layout(m)
par(oma = c(0, 0, 0, 4))

age.range.596 <- c(max(age.596), 0)

# Range chart DSDP 596
par(mar = c(5.1, 4.1, 4.1, 0))

rangechart(counts.596.family, reorder = 'lad.by.fad', normalize.counts = FALSE, 
           tax.cat = cat.fossil.family.matches[master.tax.596], taxa = master.tax.596, 
           col.points = 'by.count', cols.vec = 'viridis', count.breaks = c(0, 1, 3, 5),
           pch.points = 'by.category', pch.vec = c(15, 16, 17, 18), 
           cex.points = 'by.count', largesize = 1,
           xaxis.labels = 'names', yaxis.ticks = TRUE, 
           print.xaxis = F, main = '', ylab = '')
# add tick marks and annotations
axis(2, at = rownames(counts.596.all), labels = FALSE, tcl = -0.3) #596 ticks on left axis
# Add annotations
mtext("DSDP 596 combined with Modern", side = 3, line = 2, font = 2)
mtext("Age (Ma)", side = 2, line = 2.7)
mtext("Denticle Morphotype", side = 1, line = 3)
abline(h = 19, col = 'gray', lty = 2, lwd = 2)
legend('topleft', legend = c(levels(denticle_geomlin$Classification)[1:4], '1', '2-3', '4-5', '6+'),
       pch = c(15, 16, 17, 5, 16, 16, 16, 16),
       col = c(rep('black', 4), viridis(4)),
       pt.cex = c(rep(1, 5), 1.33, 1.67, 2),
       ncol = 2, title = '     Type              Count', title.adj = 0)

##2.DSDP 596 accumulatoin

par(mar = c(5.1, 0, 4.1, 0))

plot(y = accum.596$age, x = accum.596$dentAR, type = 'o', 
     ylim = age.range.596, xlim = c(0, 25),
     xlab = '', ylab = '', col = 'firebrick', pch = 16, axes = F, cex = 0.7)
box()
axis(2, labels = FALSE)
axis(1, col = 'firebrick', col.axis = 'firebrick')
mtext(expression("Denticles " * cm^-2 * myr^-1), side = 1, line = 3, col = 'firebrick')
# mtext('Denticles/cm2/myr', side = 1, line = 3, col = 'firebrick')
par(new=TRUE)
plot(y = accum.596$age, x = (accum.596$dent/accum.596$teeth), type = 'o', 
     ylim = age.range.596,  xlim = c(0, 0.5), 
     xlab = '', ylab = '', col = 'darkblue', pch=17, axes = F, cex = 0.7)
axis(3, col = 'darkblue', col.axis = 'darkblue')
mtext('Denticles/Teeth ratio', side = 3, line = 2, col = 'darkblue')
abline(h = 19, col = 'gray', lty = 2, lwd = 2)


mtext('Age (Ma)', side = 4, line = 2.7)
axis(4, las = 1)

legend('topright', legend = c('Denticle absolute abundance', 'Denticle relative abundance'), lty = 1, pch = c(16, 17), col = c('firebrick', 'darkblue'), cex = 0.8)


if(writeFile != 'off') {
    dev.off()
}


##### S11. ODP 886 FOSSILS ONLY: Range chart + accumulation #####

fig.dims <- c(6, 10) #Set Figure-dimensions

if(writeFile == 'pdf') {
    pdf('figures/S11_886_fossils_only_Rangechart.pdf', height = fig.dims[1], width = fig.dims[2], useDingbats = FALSE)
}

if(writeFile == 'jpg') {
    jpeg('figures/S11_886_fossils_only_Rangechart.jpg', height = fig.dims[1], width = fig.dims[2], units = 'in', res = 300)
}


m <- rbind(c(1, 1, 1, 1, 1, 2))
layout(m)
par(oma = c(0, 0, 0, 4))

age.range.886.fossils <- c(max(age.886),min(age.886))

# Range chart ODP 886
par(mar = c(5.1, 4.1, 4.1, 0))

rangechart(counts.886, reorder = 'lad.by.fad', normalize.counts = FALSE, 
           tax.cat = cat.fossil.family.matches[master.tax.886], taxa = master.tax.886, 
           col.points = 'by.count', cols.vec = 'viridis', count.breaks = c(0, 1, 3, 5),
           pch.points = 'by.category', pch.vec = c(15, 16, 17, 18), 
           cex.points = 'by.count', largesize = 1,
           xaxis.labels = 'names', yaxis.ticks = TRUE, 
           print.xaxis = FALSE, main = '', ylab = '')
# add tick marks and annotations
# Add annotations
mtext("ODP 886 Fossils Only", side = 3, line = 2, font = 2)
mtext("Age (Ma)", side = 2, line = 2.7)
mtext("Denticle Morphotype", side = 1, line = 3)
abline(h = 19, col = 'gray', lty = 2, lwd = 2)
legend('topleft', legend = c(levels(denticle_geomlin$Classification)[1:4], '1', '2-3', '4-5', '6+'),
       pch = c(15, 16, 17, 5, 16, 16, 16, 16),
       col = c(rep('black', 4), viridis(4)),
       pt.cex = c(rep(1, 5), 1.33, 1.67, 2),
       ncol = 2, title = '     Type              Count', title.adj = 0)


##2. ODP 886 accumulatoin

par(mar = c(5.1, 0, 4.1, 0))

plot(y = accum.886$age, x = accum.886$dentAR, type = 'o', 
     ylim = age.range.886.fossils, # xlim = c(0, 30),
     xlab = '', ylab = '', col = 'firebrick', pch = 16, axes = F, cex = 0.7)
box()
axis(2, labels = FALSE)
axis(1, col = 'firebrick', col.axis = 'firebrick')
# mtext('Denticles/cm2/myr', side = 1, line = 3, col = 'firebrick')
mtext(expression("Denticles " * cm^-2 * myr^-1), side = 1, line = 3, col = 'firebrick')
par(new=TRUE)
plot(y = accum.886$age, x = (accum.886$dent/accum.886$teeth), type = 'o', 
     ylim = age.range.886.fossils,  # xlim = c(0, 0.5), 
     xlab = '', ylab = '', col = 'darkblue', pch=17, axes = F, cex = 0.7)
axis(3, col = 'darkblue', col.axis = 'darkblue')
mtext('Denticles/Teeth ratio', side = 3, line = 2, col = 'darkblue')
abline(h = 19, col = 'gray', lty = 2, lwd = 2)

mtext('Age (Ma)', side = 4, line = 2.7)
axis(4, las = 1)

legend('topright', legend = c('Denticle absolute abundance', 'Denticle relative abundance'), lty = 1, pch = c(16, 17), col = c('firebrick', 'darkblue'), cex = 0.8)


if(writeFile != 'off') {
    dev.off()
}



##### S12. ODP 886 and modern: Range chart + accumulation #####

fig.dims <- c(6, 10) #Set Figure-dimensions

if(writeFile == 'pdf') {
    pdf('figures/S12_886_modern_Rangechart.pdf', height = fig.dims[1], width = fig.dims[2], useDingbats = FALSE)
}

if(writeFile == 'jpg') {
    jpeg('figures/S12_886_modern_Rangechart.jpg', height = fig.dims[1], width = fig.dims[2], units = 'in', res = 300)
}

m <- rbind(c(1, 1, 1, 1, 1, 2))
layout(m)
par(oma = c(0, 0, 0, 4))

age.range.886 <- c(max(age.886),0)

# Range chart ODP 886
par(mar = c(5.1, 4.1, 4.1, 0))

rangechart(counts.886.family, reorder = 'lad.by.fad', normalize.counts = FALSE, 
           tax.cat = cat.fossil.family.matches[master.tax.886], taxa = master.tax.886, 
           col.points = 'by.count', cols.vec = 'viridis', count.breaks = c(0, 1, 3, 5),
           pch.points = 'by.category', pch.vec = c(15, 16, 17, 18), 
           cex.points = 'by.count', largesize = 1,
           xaxis.labels = 'names', yaxis.ticks = TRUE, 
           print.xaxis = FALSE, main = '', ylab = '')
# add tick marks and annotations
# Add annotations
mtext("ODP 886 combined with Modern", side = 3, line = 2, font = 2)
mtext("Age (Ma)", side = 2, line = 2.7)
mtext("Denticle Morphotype", side = 1, line = 3)
abline(h = 19, col = 'gray', lty = 2, lwd = 2)
legend('topleft', legend = c(levels(denticle_geomlin$Classification)[1:4], '1', '2-3', '4-5', '6+'),
       pch = c(15, 16, 17, 5, 16, 16, 16, 16),
       col = c(rep('black', 4), viridis(4)),
       pt.cex = c(rep(1, 5), 1.33, 1.67, 2),
       ncol = 2, title = '     Type              Count', title.adj = 0)


##2. ODP 886 accumulatoin

par(mar = c(5.1, 0, 4.1, 0))

plot(y = accum.886$age, x = accum.886$dentAR, type = 'o', 
     ylim = age.range.886, # xlim = c(0, 30),
     xlab = '', ylab = '', col = 'firebrick', pch = 16, axes = F, cex = 0.7)
box()
axis(2, labels = FALSE)
axis(1, col = 'firebrick', col.axis = 'firebrick')
# mtext('Denticles/cm2/myr', side = 1, line = 3, col = 'firebrick')
mtext(expression("Denticles " * cm^-2 * myr^-1), side = 1, line = 3, col = 'firebrick')
par(new=TRUE)
plot(y = accum.886$age, x = (accum.886$dent/accum.886$teeth), type = 'o', 
     ylim = age.range.886,  # xlim = c(0, 0.5), 
     xlab = '', ylab = '', col = 'darkblue', pch=17, axes = F, cex = 0.7)
axis(3, col = 'darkblue', col.axis = 'darkblue')
mtext('Denticles/Teeth ratio', side = 3, line = 2, col = 'darkblue')
abline(h = 19, col = 'gray', lty = 2, lwd = 2)

mtext('Age (Ma)', side = 4, line = 2.7)
axis(4, las = 1)

legend('topright', legend = c('Denticle absolute abundance', 'Denticle relative abundance'), lty = 1, pch = c(16, 17), col = c('firebrick', 'darkblue'), cex = 0.8)

if(writeFile != 'off') {
    dev.off()
}



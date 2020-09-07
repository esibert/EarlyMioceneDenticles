#############################################
#                                           #
#               FIGURE 1                    #
#   Range chart w/ 596 & 886 accum          #
#               8/11/2020                   #                
#                                           #
#############################################

##### Setup (includes file output choice, pdf is default) #####

source('code/Setup.R')  # Call in and process the datasets as done in Setup.R script

writeFile <- 'pdf'
# writeFile <- 'jpg'
# writeFile <- 'off'

##### Make the figure: #####

fig.dims <- c(7.5, 13) #Set Figure-dimensions

if(writeFile == 'pdf') {
        pdf('figures/Figure1.pdf', height = fig.dims[1], width = fig.dims[2], useDingbats = FALSE)
}

if(writeFile == 'jpg') {
        jpeg('figures/Figure1.jpg', height = fig.dims[1], width = fig.dims[2], units = 'in', res = 300)
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
fig1.legend <- rangechart(counts.fossil.family.matches, reorder = 'lad.by.fad', normalize.counts = FALSE, 
           col.points = 'by.count', cols.vec = 'viridis', count.breaks = c(0, 1, 3, 5),
           cex.points = 'by.count', largesize = 1,
           tax.cat = cat.fossil.family.matches, pch.points = 'by.category', pch.vec = c(15, 16, 17, 5), 
           xaxis.labels = 'alphanum', yaxis.ticks = TRUE,
           print.xaxis = T, main = '', ylab = '',
           rangeExtensions = extensions.fossil.species.matches.20$new.lad, elty = 1, elcol = 'firebrick')
rect(-10, sort(ext.fossils.modern.sub.20$new.lad)[sp.conf.20.95[1]], 100, sort(ext.fossils.modern.sub.20$new.lad)[sp.conf.20.95[2]], col = 'gray', border = NA)
# abline(h = quantile(ext.fossils.modern.sub.20$new.lad, 0.2), col = 'darkred', lwd = 2) # Median estimate
par(new = T)
rangechart(counts.fossil.family.matches, reorder = 'lad.by.fad', normalize.counts = FALSE, 
           col.points = 'by.count', cols.vec = 'viridis', count.breaks = c(0, 1, 3, 5), # 1, 2, 3, 4-5, 6+
           cex.points = 'by.count', largesize = 1,
           tax.cat = cat.fossil.family.matches, pch.points = 'by.category', pch.vec = c(15, 16, 17, 5), 
           xaxis.labels = 'alphanum', yaxis.ticks = TRUE,
           print.xaxis = F, main = '', ylab = '',
           rangeExtensions = extensions.fossil.species.matches.20$new.lad, elty = 1, elcol = 'firebrick')
# add tick marks and annotations
axis(2, at = rownames(counts.886), labels = FALSE, tcl = -0.3) #886 ticks on left axis as well
axis(2, at = rownames(counts.596.all), labels = FALSE, tcl = -0.3) #596 ticks on left axis
# Add annotations
mtext("DSDP 596 and ODP 886 combined with Modern", side = 3, line = 2, font = 2)
mtext("Age (Ma)", side = 2, line = 2.7)
mtext("Denticle Morphotype", side = 1, line = 3)
# abline(h = 19, col = 'gray', lty = 2, lwd = 2)

# legend('topleft', legend = c(levels(denticle_geomlin$Classification)[1:4], '', '1', '2', '3', '4-5', '6+'), 
#        pch = c(15, 16, 17, 5, -1, 16, 16, 16, 16, 16), 
#        col = c(rep('black', 4), 'white', viridis(5)),
#        pt.cex = c(rep(1, 6), 1.33, 1.5, 1.67, 2), 
#        ncol = 2, title = '     Type              Count', title.adj = 0)


legend('topleft', legend = c(levels(denticle_geomlin$Classification)[1:4], '1', '2-3', '4-5', '6+'),
       pch = c(15, 16, 17, 5, 16, 16, 16, 16),
       col = c(rep('black', 4), viridis(4)),
       pt.cex = c(rep(1, 5), 1.33, 1.67, 2),
       ncol = 2, title = '     Type              Count', title.adj = 0)
## Include 95% and 99% confidence intervals? Maybe not... 
# abline(h = sort(ext.fossils.modern.sub.20$new.lad)[sp.conf.20.95[1]], col = 'red', lwd = 1, lty = 3) # 95% young 
# abline(h = sort(ext.fossils.modern.sub.20$new.lad)[sp.conf.20.95[2]], col = 'red', lwd = 1, lty = 3) # 95% old 
# abline(h = sort(ext.fossils.modern.sub.20$new.lad)[sp.conf.20.99[1]], col = 'blue', lwd = 1, lty = 3) # 99% young 
# abline(h = sort(ext.fossils.modern.sub.20$new.lad)[sp.conf.20.99[2]], col = 'blue', lwd = 1, lty = 3) # 99% old 

##2.ODP 596 accumulatoin

par(mar = c(5.1, 0, 4.1, 0))

plot(y = accum.596$age, x = accum.596$dentAR, type = 'o', 
     ylim = age.range.full, xlim = c(0, 25),
     xlab = '', ylab = '', col = 'firebrick', pch = 16, axes = F, cex = 0.7)
rect(-10, sort(ext.fossils.modern.sub.20$new.lad)[sp.conf.20.95[1]], 100, sort(ext.fossils.modern.sub.20$new.lad)[sp.conf.20.95[2]], col = 'gray', border = NA)
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
rect(-10, sort(ext.fossils.modern.sub.20$new.lad)[sp.conf.20.95[1]], 100, sort(ext.fossils.modern.sub.20$new.lad)[sp.conf.20.95[2]], col = 'gray', border = NA)
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


##### Save the RangeChart legend #####
sink("figures/Figure1_xaxis_Legend.txt") 

cat('##### Range Chart Legend for Figure 1 #####\n')
cat(paste(1:length(fig1.legend), '. ', fig1.legend, sep = '', collapse = '\n'))

sink()

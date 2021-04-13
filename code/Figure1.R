#############################################
#                                           #
#     FIGURE 1_toothMAR_included            #
#   Range chart w/ 596 & 886 accum          #
#               3/13/2021                   #                
#                                           #
#############################################

##### Setup (includes file output choice, pdf is default) #####

# source('code/Setup.R')  # Call in and process the datasets as done in Setup.R script

 writeFile <- 'pdf'
# writeFile <- 'jpg'
# writeFile <- 'off'

##### Set Up the figure (margins, files, etc): #####

fig.dims <- c(7.5, 13) #Set Figure-dimensions

if(writeFile == 'pdf') {
    pdf('figures/Figure1.pdf', height = fig.dims[1], width = fig.dims[2], useDingbats = FALSE)
}

if(writeFile == 'jpg') {
    jpeg('figures/Figure1.jpg', height = fig.dims[1], width = fig.dims[2], units = 'in', res = 300)
}

# Define multi-panel
m <- rbind(c(1, 1, 1, 1, 1, 1,1, 1, 2, 3))

layout(m)
par(oma = c(0, 0, 4, 4))
# layout.show(3) #check if layout works

##### Graphical Parameters & axis limits #####
# age range
age.range.full <- c(max(as.numeric(rownames(counts.fossil.family.matches))), 0)

## colors 
# range chart
col.rangechart <- viridis(4)
col.rangechart[1] <- 'gray70'
# Accum plots
col.dentaccum <- 'firebrick'
col.toothaccum <- 'gray70'
col.ratio <- 'darkblue'

## shapes
pch.dentaccum <- 16
pch.toothaccum <- 16
pch.ratio <- 17

## scaling factor for tooth and denticle accumulation axes
accum.scale <- 4

## Plot type
plottype <- 'o'

## axis limits
# 596
dent.accum.596.max <- 25
teeth.accum.596.max <- accum.scale*dent.accum.596.max
ratio.596.max <- 0.6

# 886
dent.accum.886.max <- 60
teeth.accum.886.max <- accum.scale*dent.accum.886.max
ratio.886.max <- 0.6

# lines
line.type = 3
line.scale = 1


##### 1, Range Chart #####
par(mar = c(5.1, 4.1, 4.1, 0))
# Make range chart
fig1.legend <- rangechart(counts.fossil.family.matches, reorder = 'lad.by.fad', normalize.counts = FALSE, 
                          col.points = 'by.count', cols.vec = col.rangechart, count.breaks = c(0, 1, 3, 5),
                          cex.points = 'by.count', largesize = 1,
                          tax.cat = cat.fossil.family.matches, pch.points = 'by.category', pch.vec = c(15, 16, 17, 5), 
                          xaxis.labels = 'alphanum', yaxis.ticks = TRUE,
                          print.xaxis = T, main = '', ylab = '',
                          rangeExtensions = extensions.fossil.species.matches.20$new.lad, elty = 1, elcol = 'firebrick')
# rectangle
rect(-10, sort(ext.fossils.modern.sub.20$new.lad)[sp.conf.20.95[1]], 100, sort(ext.fossils.modern.sub.20$new.lad)[sp.conf.20.95[2]], col = 'gray', border = NA)
par(new = T)
rangechart(counts.fossil.family.matches, reorder = 'lad.by.fad', normalize.counts = FALSE, 
           col.points = 'by.count', cols.vec = col.rangechart, count.breaks = c(0, 1, 3, 5), # 1, 2, 3, 4-5, 6+
           cex.points = 'by.count', largesize = 1,
           tax.cat = cat.fossil.family.matches, pch.points = 'by.category', pch.vec = c(15, 16, 17, 5), 
           xaxis.labels = 'alphanum', yaxis.ticks = TRUE,
           print.xaxis = F, main = '', ylab = '',
           rangeExtensions = extensions.fossil.species.matches.20$new.lad, elty = 1, elcol = 'firebrick')

# tick marks
axis(2, at = rownames(counts.886), labels = FALSE, tcl = -0.3) #886 ticks on left axis as well
axis(2, at = rownames(counts.596.all), labels = FALSE, tcl = -0.3) #596 ticks on left axis

# annotations
mtext("DSDP 596 and ODP 886 combined with Modern", side = 3, line = 2, font = 2)
mtext("Age (Ma)", side = 2, line = 2.7)
mtext("Denticle Morphotype", side = 1, line = 3)

# legend
legend('topleft', legend = c(levels(denticle_geomlin$Classification)[1:4], '1', '2-3', '4-5', '6+', '1', '2-3', '4-5', '6+'),
       pch = c(15, 16, 17, 5, 16, 16, 16, 16, 16, 16, 16, 16),
       col = c(rep('black', 4), col.rangechart, rep('firebrick', 4)),
       pt.cex = c(rep(1, 5), 1.33, 1.67, 2, 1, 1.33, 1.67, 2),
       ncol = 3, title = '     Type             Count            Families', title.adj = 0)

## Include 95% and 99% confidence intervals?  
# abline(h = sort(ext.fossils.modern.sub.20$new.lad)[sp.conf.20.95[1]], col = 'red', lwd = 1, lty = 3) # 95% young 
# abline(h = sort(ext.fossils.modern.sub.20$new.lad)[sp.conf.20.95[2]], col = 'red', lwd = 1, lty = 3) # 95% old 
# abline(h = sort(ext.fossils.modern.sub.20$new.lad)[sp.conf.20.99[1]], col = 'blue', lwd = 1, lty = 3) # 99% young 
# abline(h = sort(ext.fossils.modern.sub.20$new.lad)[sp.conf.20.99[2]], col = 'blue', lwd = 1, lty = 3) # 99% old 

##### 2.ODP 596 accumulation #####

par(mar = c(5.1, 0, 4.1, 0))

# Denticle Accumulation
plot(y = accum.596$age, x = accum.596$dentAR, type = plottype, 
     ylim = age.range.full, xlim = c(0, dent.accum.596.max),
     xlab = '', ylab = '', col = col.dentaccum, pch = pch.dentaccum, axes = F, lty = line.type, lwd = line.scale)
rect(-10, sort(ext.fossils.modern.sub.20$new.lad)[sp.conf.20.95[1]], 100, sort(ext.fossils.modern.sub.20$new.lad)[sp.conf.20.95[2]], col = 'gray', border = NA)
lines(y = accum.596$age, x = accum.596$dentAR, type = plottype, col = col.dentaccum, pch = pch.dentaccum, lty = line.type, lwd = line.scale)
box()
axis(2, labels = FALSE)
dent.axis.596 <- axis(3, col = col.dentaccum, col.axis = col.dentaccum)

# Tooth accumulation (4x scale) 
par(new = TRUE) 
plot(y = accum.596$age, x = accum.596$teethAR, type = plottype, lty = line.type, lwd = line.scale,
     ylim = age.range.full, xlim = c(0, teeth.accum.596.max),
     xlab = '', ylab = '', col = col.toothaccum, pch = pch.toothaccum, axes = F)
axis(3, col = col.toothaccum, col.axis = col.toothaccum, line = 3.5, at = dent.axis.596*accum.scale)


# Tooth to Denticle Ratio
par(new=TRUE)
plot(y = accum.596$age, x = (accum.596$dent/accum.596$teeth), type = plottype, 
     ylim = age.range.full,  xlim = c(0, ratio.596.max), 
     xlab = '', ylab = '', col = col.ratio, pch=pch.ratio, axes = F, lty = line.type, lwd = line.scale)
axis(1, col = col.ratio, col.axis = col.ratio)


legend('bottomright', legend = c("South Pacific", "DSDP 596"), bty = 'n')



##### 3. DSDP 886 Accumulation #####

par(mar = c(5.1, 0, 4.1, 0))

# Denticle accumulation
plot(y = accum.886$age, x = accum.886$dentAR, type = plottype, 
     ylim = age.range.full, xlim = c(0, dent.accum.886.max), 
     xlab = '', ylab = '', col = col.dentaccum, pch = 16, axes = F, lty = line.type, lwd = line.scale)
rect(-10, sort(ext.fossils.modern.sub.20$new.lad)[sp.conf.20.95[1]], dent.accum.886.max*2, sort(ext.fossils.modern.sub.20$new.lad)[sp.conf.20.95[2]], col = 'gray', border = NA)
lines(y = accum.886$age, x = accum.886$dentAR, type = plottype, col = col.dentaccum, pch = pch.dentaccum, lty = line.type, lwd = line.scale)
box()
axis(2, labels = F)
dent.axis.886 <- axis(3, col = col.dentaccum, col.axis = col.dentaccum)

# Tooth accumulation (4x scale) 
par(new = TRUE) 
plot(y = accum.886$age, x = accum.886$teethAR, type = plottype, lty = line.type, lwd = line.scale,
     ylim = age.range.full, xlim = c(0, teeth.accum.886.max),
     xlab = '', ylab = '', col = col.toothaccum, pch = pch.toothaccum, axes = F)
axis(3, col = col.toothaccum, col.axis = col.toothaccum, line = 3.5, at = dent.axis.886*accum.scale)

# Tooth:Denticle ratio
par(new=TRUE)
plot(y=accum.886$age, x=(accum.886$dent/accum.886$teeth), type = plottype, 
     ylim = age.range.full, xlim = c(0, ratio.886.max), 
     xlab = '', ylab = '', col = col.ratio, pch=pch.ratio, axes = F, lty = line.type, lwd = line.scale)
axis(1, col = col.ratio, col.axis = col.ratio)
mtext('Age (Ma)', side = 4, line = 2.7)
axis(4, las = 1)

# annotations
legend('bottomright', legend = c("North Pacific", "ODP 886"), bty = 'n')
text("Hiatus", x = ratio.886.max/2, y = 16)

##### Add annotations to both graphs #####
# axis labels
mtext('Denticle:Tooth ratio', side = 1, line = 3, col = col.ratio, at = 0)
mtext(expression("Denticles " * cm^-2 * myr^-1), side = 3, line = 1.7, col = col.dentaccum, at = 0, cex = 0.9)
mtext(expression("Teeth " * cm^-2 * myr^-1), side = 3, line = 6.2, col = col.toothaccum, at = 0, cex = 0.9)
mtext(expression("(4x denticle axis)"), line = 5.4, col = col.toothaccum, at = 0, cex = 0.75)

# legend
legend('topright', legend = c('Denticle accumulation', 'Tooth accumulation', 'Denticle:Tooth Ratio'), 
       lty = line.type, lwd = line.scale, pch = c(pch.dentaccum, pch.toothaccum, pch.ratio), 
       col = c(col.dentaccum, col.toothaccum, col.ratio), cex = 0.9)

# close file
if(writeFile != 'off') {
    dev.off()
}


##### Save the RangeChart legend #####
sink("figures/Figure1_xaxis_Legend.txt") 

cat('##### Range Chart Legend for Figure 1 #####\n')
cat(paste(1:length(fig1.legend), '. ', fig1.legend, sep = '', collapse = '\n'))

sink()

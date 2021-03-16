#########################################
#                                       #
#       Supplemental Figures            #
#           3/16/2021                   #
#                                       #
#########################################

##### SETUP #####

source('code/Setup.R')  #Call in and process the datasets as done in Setup.R script

writeFile <- 'pdf'
# writeFile <- 'jpg'
# writeFile <- 'off'

##### Graphical Parameters & axis limits for range chart & accum plots #####
age.range.full <- c(max(as.numeric(rownames(counts.fossil.family.matches))), 0) # For figures with modern matches
age.range.fossils <- c(max(as.numeric(rownames(counts.fossil))), min(as.numeric(rownames(counts.fossil)))) # For figures with both sites fossils
age.range.596.fossils <- c(max(age.596.all), min(age.596.all)) # For figures with 596 only 
age.range.596 <- c(max(age.596), 0) #596+modern figure
age.range.886 <- c(max(age.886),0) #886+modern figure
age.range.886.fossils <- c(max(age.886),min(age.886)) #886 fossils only figure

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


#####################################
#                                   #
#       Supplemental Figures        #
#                                   #
#####################################

##### S3. Modern Denticles per taxon histograms figue #####

## Start file
fig.dims <- c(6, 11) #Set Figure-dimensions

if(writeFile == 'pdf') {
    pdf('figures/S3_Denticle_type_histograms.pdf', height = fig.dims[1], width = fig.dims[2], useDingbats = FALSE)
}

if(writeFile == 'jpg') {
    jpeg('figures/S3_Denticle_type_histograms.jpg', height = fig.dims[1], width = fig.dims[2], units = 'in', res = 300)
}

## Set up plot space
layout(1)
par(mfrow = c(2, 4))
xaxis.lab <- 'Denticle Types'

## calculate histograms
# 1. types per species
dat_to_plot <- types_per_species
counts <- hist(dat_to_plot, breaks = c(0:max(dat_to_plot)), main = "Denticle Types per Species", xlab = xaxis.lab)$counts
sdat <- summary(dat_to_plot)
summStr <- paste(names(sdat), format(sdat, digits = 3), collapse = "\n ")
text(summStr, x = (4*max(dat_to_plot))/5, y = (4*max(counts))/5)

# 2. types per Genus
dat_to_plot <- types_per_genus
counts <- hist(dat_to_plot, breaks = c(0:max(dat_to_plot)), main = "Denticle Types per Genus", xlab = xaxis.lab)$counts
sdat <- summary(dat_to_plot)
summStr <- paste(names(sdat), format(sdat, digits = 3), collapse = "\n ")
text(summStr, x = (4*max(dat_to_plot))/5, y = (4*max(counts))/5)


# 3. types per Family
dat_to_plot <- types_per_family
counts <- hist(dat_to_plot, breaks = c(0:max(dat_to_plot)), main = "Denticle Types per Family", xlab = xaxis.lab)$counts
sdat <- summary(dat_to_plot)
summStr <- paste(names(sdat), format(sdat, digits = 3), collapse = "\n ")
text(summStr, x = (4*max(dat_to_plot))/5, y = (4*max(counts))/5)

# 4. types per Order
dat_to_plot <- types_per_order
counts <- hist(dat_to_plot, breaks = c(0:max(dat_to_plot)), main = "Denticle Types per Order", xlab = xaxis.lab)$counts
sdat <- summary(dat_to_plot)
summStr <- paste(names(sdat), format(sdat, digits = 3), collapse = "\n ")
text(summStr, x = (4*max(dat_to_plot))/5, y = (4*max(counts))/5)

# 5. Species per type
dat_to_plot <- species_per_type
counts <- hist(dat_to_plot, breaks = c(0:max(dat_to_plot)), main = "Species per denticle type", xlab = '# of Species per type')$counts
sdat <- summary(dat_to_plot)
summStr <- paste(names(sdat), format(sdat, digits = 3), collapse = "\n ")
text(summStr, x = (4*max(dat_to_plot))/5, y = (4*max(counts))/5)

# 5. Genera per type
dat_to_plot <- genus_per_type
counts <- hist(dat_to_plot, breaks = c(0:max(dat_to_plot)), main = "Genera per denticle type", xlab = '# of Genera per type')$counts
sdat <- summary(dat_to_plot)
summStr <- paste(names(sdat), format(sdat, digits = 3), collapse = "\n ")
text(summStr, x = (4*max(dat_to_plot))/5, y = (4*max(counts))/5)

# 7. Family per type
dat_to_plot <- family_per_type
counts <- hist(dat_to_plot, breaks = c(0:max(dat_to_plot)), main = "Families per denticle type", xlab = '# of Families per type')$counts
sdat <- summary(dat_to_plot)
summStr <- paste(names(sdat), format(sdat, digits = 3), collapse = "\n ")
text(summStr, x = (4*max(dat_to_plot))/5, y = (4*max(counts))/5)

# 8. Order per type
dat_to_plot <- order_per_type
counts <- hist(dat_to_plot, breaks = c(0:max(dat_to_plot)), main = "Orders per denticle type", xlab = '# of Orders per type')$counts
sdat <- summary(dat_to_plot)
summStr <- paste(names(sdat), format(sdat, digits = 3), collapse = "\n ")
text(summStr, x = (4*max(dat_to_plot))/5, y = (4*max(counts))/5)

## close file
if(writeFile != 'off') {
    dev.off()
}




##### S4. Environmental Parameters #####
fig.dims <- c(7, 8) #Set Figure-dimensions

if(writeFile == 'pdf') {
    pdf('figures/S4_Environmental_Parameters.pdf', height = fig.dims[1], width = fig.dims[2], useDingbats = FALSE)
}

if(writeFile == 'jpg') {
    jpeg('figures/S4_Environmental_Parameters.jpg', height = fig.dims[1], width = fig.dims[2], units = 'in', res = 300)
}


### 0. Set up the page
# figure parameters
par(oma = c(5.1, 0.5, 3, 0.5))
par(mfrow = c(4,1))
par(mar = c(0, 8.1, 0, 4.1))

# relevant values
age.range.full <- c(max(as.numeric(rownames(counts.fossil.family.matches))), 0)

# Graphical parameters


### 1. Oxygen
plot(x = westerhold$ï..Age_Ma, y = westerhold$d18O_longsmooth, 
     xlim = age.range.full, ylim = c(5, 0), 
     type = 'l', lwd = 2, xlab = '', ylab = '', axes = F)
rect(sort(ext.fossils.modern.sub.20$new.lad)[sp.conf.20.95[1]], -10, sort(ext.fossils.modern.sub.20$new.lad)[sp.conf.20.95[2]], 100, col = 'gray', border = NA)
lines(x = westerhold$ï..Age_Ma, y = westerhold$d18O_longsmooth, type = 'l', lwd = 2)
box()
axis(2, las = 1)
axis(1, labels = FALSE)
mtext(expression(delta^18*"O"), side = 2, line = 2.5)
legend("bottomleft",  legend = expression(delta^18*"O (Westerhold et al. 2020)"), bty = 'n')


### 2. Carbon
plot(x = westerhold$ï..Age_Ma, y = westerhold$d13C_longsmooth, 
     xlim = age.range.full, ylim = c(-1, 2), 
     type = 'l', lwd = 2, xlab = "", ylab = '', axes = F)
rect(sort(ext.fossils.modern.sub.20$new.lad)[sp.conf.20.95[1]], -10, sort(ext.fossils.modern.sub.20$new.lad)[sp.conf.20.95[2]], 100, col = 'gray', border = NA)
lines(x = westerhold$ï..Age_Ma, y = westerhold$d13C_longsmooth, type = 'l', lwd = 2)
box()
axis(2, las = 1)
axis(1, labels = FALSE)
mtext(expression(delta^13*"C"), side = 2, line = 2.5)
legend("bottomleft",  legend = expression(delta^13*"C (Westerhold et al. 2020)"), bty = 'n')


### 3.ODP 596 accumulation

# Denticle Accumulation
plot(x = accum.596$age, y = accum.596$dentAR, type = plottype, 
     xlim = age.range.full, ylim = c(0, dent.accum.596.max),
     xlab = '', ylab = '', col = col.dentaccum, pch = pch.dentaccum, axes = F, lty = line.type, lwd = line.scale)
rect(sort(ext.fossils.modern.sub.20$new.lad)[sp.conf.20.95[1]], -10, sort(ext.fossils.modern.sub.20$new.lad)[sp.conf.20.95[2]], 100, col = 'gray', border = NA)
lines(x = accum.596$age, y = accum.596$dentAR, type = plottype, col = col.dentaccum, pch = pch.dentaccum, lty = line.type, lwd = line.scale)
box()
#axes
axis(1, labels = FALSE)
dent.axis.596 <- axis(2, col = col.dentaccum, col.axis = col.dentaccum, las = 1)

# Tooth accumulation (4x scale) 
par(new = TRUE) 
plot(x = accum.596$age, y = accum.596$teethAR, type = plottype, lty = line.type, lwd = line.scale,
     xlim = age.range.full, ylim = c(0, teeth.accum.596.max),
     xlab = '', ylab = '', col = col.toothaccum, pch = pch.toothaccum, axes = F)
axis(2, col = col.toothaccum, col.axis = col.toothaccum, line = 3.5, at = dent.axis.596*accum.scale)


# Tooth to Denticle Ratio
par(new=TRUE)
plot(x = accum.596$age, y = (accum.596$dent/accum.596$teeth), type = plottype, 
     xlim = age.range.full,  ylim = c(0, ratio.596.max), 
     xlab = '', ylab = '', col = col.ratio, pch=pch.ratio, axes = F, lty = line.type, lwd = line.scale)
axis(4, col = col.ratio, col.axis = col.ratio, las = 1)


legend('topleft', legend = c("South Pacific", "DSDP 596"), bty = 'n')

#axis labels (for both accumulation plots combined)
mtext(expression("Denticles " * cm^-2 * myr^-1), side = 2, line = 1.7, col = col.dentaccum, at = 0, cex = 0.9)
mtext(expression("Teeth " * cm^-2 * myr^-1), side = 2, line = 6.2, col = col.toothaccum, at = 0, cex = 0.9)
mtext(expression("(4x denticle axis)"), side = 2, line = 5.4, col = col.toothaccum, at = 0, cex = 0.75)
mtext('Denticle:Tooth ratio', side = 4, line = 3, col = col.ratio, at = 0)


### 4. DSDP 886 Accumulation

# Denticle accumulation
plot(x = accum.886$age, y = accum.886$dentAR, type = plottype, 
     xlim = age.range.full, ylim = c(0, dent.accum.886.max), 
     xlab = '', ylab = '', col = col.dentaccum, pch = 16, axes = F, lty = line.type, lwd = line.scale)
rect(sort(ext.fossils.modern.sub.20$new.lad)[sp.conf.20.95[1]], -10, sort(ext.fossils.modern.sub.20$new.lad)[sp.conf.20.95[2]], dent.accum.886.max*2, col = 'gray', border = NA)
lines(x = accum.886$age, y = accum.886$dentAR, type = plottype, col = col.dentaccum, pch = pch.dentaccum, lty = line.type, lwd = line.scale)
box()
axis(1)
dent.axis.886 <- axis(2, col = col.dentaccum, col.axis = col.dentaccum, las = 1)

# Tooth accumulation (4x scale) 
par(new = TRUE) 
plot(x = accum.886$age, y = accum.886$teethAR, type = plottype, lty = line.type, lwd = line.scale,
     xlim = age.range.full, ylim = c(0, teeth.accum.886.max),
     xlab = '', ylab = '', col = col.toothaccum, pch = pch.toothaccum, axes = F)
axis(2, col = col.toothaccum, col.axis = col.toothaccum, line = 3.5, at = dent.axis.886*accum.scale)

# Tooth:Denticle ratio
par(new=TRUE)
plot(x = accum.886$age, y = (accum.886$dent/accum.886$teeth), type = plottype, 
     xlim = age.range.full, ylim = c(0, ratio.886.max), 
     xlab = '', ylab = '', col = col.ratio, pch=pch.ratio, axes = F, lty = line.type, lwd = line.scale)
axis(4, col = col.ratio, col.axis = col.ratio, las = 1)
mtext('Age (Ma)', side = 1, line = 2.7)

# annotations
legend('topleft', legend = c("North Pacific", "ODP 886"), bty = 'n')
text("Hiatus", y = ratio.886.max/2, x = 17)

# add legend
legend('topright', legend = c('Denticle accumulation', 'Tooth accumulation', 'Denticle:Tooth Ratio'), 
       lty = line.type, lwd = line.scale, pch = c(pch.dentaccum, pch.toothaccum, pch.ratio), 
       col = c(col.dentaccum, col.toothaccum, col.ratio), cex = 0.9)

if(writeFile != 'off') {
    dev.off()
}


##### S5. Range chart including all fossil and modern denticle types, not just "fossil matches" #####

## Set up file
fig.dims <- c(9,22) #Set Figure-dimensions

if(writeFile == 'pdf') {
    pdf('figures/S5_modern_full_Rangechart.pdf', height = fig.dims[1], width = fig.dims[2], useDingbats = FALSE)
}

if(writeFile == 'jpg') {
    jpeg('figures/S5_modern_full_Rangechart.jpg', height = fig.dims[1], width = fig.dims[2], units = 'in', res = 300)
}


## Reset margins
layout(1) # reset layout 
par(mar = c(5.1, 4.1, 4.1, 2.1)) # reset margins
par(oma = c(0,0,0,0))

## Range chart Full modern + fossil dataset
modern.fossil.legend <- rangechart(counts.fossil.family, reorder = 'lad.by.fad', normalize.counts = FALSE, 
                                   col.points = 'by.count', cols.vec = col.rangechart, count.breaks = c(0, 1, 3, 5),
                                   cex.points = 'by.count', largesize = 1,
                                   tax.cat = cat.fossil.family, pch.points = 'by.category', pch.vec = c(15, 16, 17, 5), 
                                   xaxis.labels = 'alphanum', yaxis.ticks = TRUE,
                                   print.xaxis = T, main = '', ylab = '')

# add rectangle & overlay the plot again to get over rectangle
rect(-10, sort(ext.fossils.modern.sub.20$new.lad)[sp.conf.20.95[1]], 200, sort(ext.fossils.modern.sub.20$new.lad)[sp.conf.20.95[2]], col = 'gray', border = NA)
par(new = T)
modern.fossil.legend <- rangechart(counts.fossil.family, reorder = 'lad.by.fad', normalize.counts = FALSE, 
                                   col.points = 'by.count', cols.vec = col.rangechart, count.breaks = c(0, 1, 3, 5),
                                   cex.points = 'by.count', largesize = 1,
                                   tax.cat = cat.fossil.family, pch.points = 'by.category', pch.vec = c(15, 16, 17, 5), 
                                   xaxis.labels = 'alphanum', yaxis.ticks = TRUE,
                                   print.xaxis = T, main = '', ylab = '')

# Axis tick marks 
axis(2, at = rownames(counts.596.all), labels = FALSE, tcl = -0.3) #596 ticks on left axis

# annotations
mtext("DSDP 596 and ODP 886 with all modern denticle types included", side = 3, line = 2, font = 2)
mtext("Age (Ma)", side = 2, line = 2.7)
mtext("Denticle Morphotype", side = 1, line = 3)
text("Modern denticle types with no fossil analog", x = 105, y = 2)

# Rangechart Legend
legend('topleft', legend = c(levels(denticle_geomlin$Classification)[1:4], '1', '2-3', '4-5', '6+', '1', '2-3', '4-5', '6+'),
       pch = c(15, 16, 17, 5, 16, 16, 16, 16, 16, 16, 16, 16),
       col = c(rep('black', 4), col.rangechart, rep('firebrick', 4)),
       pt.cex = c(rep(1, 5), 1.33, 1.67, 2, 1, 1.33, 1.67, 2),
       ncol = 3, title = '     Type             Count            Families', title.adj = 0)


if(writeFile != 'off') {
    dev.off()
}


## Write legend file
sink("figures/modern_fossil_xaxis_Legend.txt") 

cat('##### Range Chart Legend for combined Fossil & ALL Modern denticles Supplemental Figure #####\n')
cat(paste(1:length(modern.fossil.legend), '. ', modern.fossil.legend, sep = '', collapse = '\n'))

sink()


##### S6a. Rarefaction curves figure (full dataset) #####
fig.dims <- c(8,8) #Set Figure-dimensions

if(writeFile == 'pdf') {
    pdf('figures/S6a_rarefaction.pdf', height = fig.dims[1], width = fig.dims[2], useDingbats = FALSE)
}

if(writeFile == 'jpg') {
    jpeg('figures/S6a_rarefaction.jpg', height = fig.dims[1], width = fig.dims[2], units = 'in', res = 300)
}

# Order for colors (6a): 
# 1. Pre-extinctoin
# 2. Post-extinction
# 3. Modern 

rare.cols <- c('darkblue', 'darkgreen', 'goldenrod')

par(fig=c(0, 1, 0, 1), new = F, mar = c(4,4,4,4))

## Main Figure
# Fossils - pre-extinction
rarefaction.plot(rare.fossil.split.counts[2,], ci.type = 'polygon', lcol = rare.cols[1], lwd = 2)
# Fossils - post-extinction
rarefaction.plot(rare.fossil.split.counts[1,], ci.type = 'polygon', lcol = rare.cols[2], lwd = 2, add.line = TRUE)
# Modern - all
rarefaction.plot(rare.modern.counts, ci.type = 'polygon', lcol = rare.cols[3], lwd = 2, add.line = TRUE)

## Annotations
# Title
mtext('(a) Rarefaction of Denticle Types (Full Dataset)', side = 3, cex = 1.5, line = 1.5, font = 2)
# Rectangle
rect(xleft = 0, ybottom = 0, xright = 50, ytop = 30, lty = 2)
# # annotation of (a)
# mtext("a", side = 3, line = 0, font = 2, adj = 0, cex = 1.2)

## Legend
legend('topleft', legend = c('Pre-Extinction', 'Post-Extinction', 'Modern Catalog'), 
       col = adjustcolor(rare.cols[1:3], alpha.f = 0.5), lty = 1, lwd = 15)
legend('topleft', legend = c('', '', ''), 
       col = (rare.cols[1:3]), lty = 1, lwd = 3, bg = 'transparent', bty = 'n')


### Inset Plot
par(fig=c(0.4, 0.99, 0.01, 0.6), new = T)

## Main plot
# Fossils - pre-extinction
rarefaction.plot(rare.fossil.split.counts[2,], ci.type = 'polygon', lcol = rare.cols[1], lwd = 2, xlim = c(0, 50), ylim = c(0, 30), axes = F, xlab = '', ylab = '')
# Fossils - post-extinction
rarefaction.plot(rare.fossil.split.counts[1,], ci.type = 'polygon', lcol = rare.cols[2], lwd = 2, add.line = TRUE)
# Modern - all
rarefaction.plot(rare.modern.counts, ci.type = 'polygon', lcol = rare.cols[3], lwd = 2, add.line = TRUE)

## annotations
box()
axis(2)
axis(3)
mtext("Morphotypes", side = 2, line = 2.5)
mtext("Sample Size", side = 3, line = 2.5)

# text("b", x = 0.5, y = 29.5, font = 2, cex = 1.2)

## Close out file 
if(writeFile != 'off') {
    dev.off()
}


##### S6b. Rarefaction curves figure (DSDP 596 ONLY Dataset) #####

fig.dims <- c(8,8) #Set Figure-dimensions

if(writeFile == 'pdf') {
    pdf('figures/S6b_rarefaction.pdf', height = fig.dims[1], width = fig.dims[2], useDingbats = FALSE)
}

if(writeFile == 'jpg') {
    jpeg('figures/S6b_rarefaction.jpg', height = fig.dims[1], width = fig.dims[2], units = 'in', res = 300)
}

# Order for colors (6a): 
# 1. Pre-extinction - 596 full dataset
# 2. Pre-extinction - 596 youngest subset
# 3. Pre-extinction - 596 re-sampling
# 4. Post-extinction - 596

rare.cols <- c('gray50', 'skyblue2', 'firebrick', 'forestgreen')

## Set up Big figure
par(fig=c(0, 1, 0, 1), new = F, mar = c(4,4,4,4))

### Main Figure
# 596 full dataset
rarefaction.plot(rare.counts.596.pre, ci.type = 'polygon', lcol = rare.cols[1], lwd = 2) # 596 pre (full)
# 596 - youngest pre-extinction subset
rarefaction.plot(rare.counts.596.pre.sub, ci.type = 'polygon', lcol = rare.cols[2], lwd = 2, add.line = T) # 596 Young subset
# 596 - resampling lines
for (i in 1:20) {
    counts.596.pre.sample <- counts.596.pre[sample(1:dim(counts.596.pre)[1], dim(counts.596.post)),] #random sample
    rare.counts.596.pre.sample <- apply(counts.596.pre.sample, 2, sum)
    rarefaction.plot(rare.counts.596.pre.sample, ci.type = 'off', lcol = rare.cols[3], add.line = T, lwd = 1)
}
# 596 - post-extinction
rarefaction.plot(rare.counts.596.post, ci.type = 'polygon', lcol = rare.cols[4], lwd = 2, add.line = T) # 596 post 

## Annotations
# Title
mtext('(b) Rarefaction of Denticle Types (DSDP 596 only)', side = 3, cex = 1.5, line = 1.5, font = 2)
# Rectangle
rect(xleft = 0, ybottom = 0, xright = 50, ytop = 30, lty = 2)
# # annotation of (a)
# mtext("a", side = 3, line = 0, font = 2, adj = 0, cex = 1.2)

legend('topleft', legend = c('Pre-Extinction', 'Pre-extinction Young subset', 'Pre-extinction Resampling', 'Post-Extinction'), 
       col = adjustcolor(rare.cols[1:4], alpha.f = 0.5), lty = 1, lwd = c(15, 15, 3, 15))
legend('topleft', legend = c('', '', '', ''), 
       col = (rare.cols[1:4]), lty = 1, lwd = 3, bg = 'transparent', bty = 'n')

### Inset Plot
par(fig=c(0.4, 0.99, 0.01, 0.6), new = T)

## Main plot
# 596 - pre-extinction all
rarefaction.plot(rare.counts.596.pre, ci.type = 'polygon', lcol = rare.cols[1], lwd = 2, xlim = c(0, 30), ylim = c(0, 10), axes = F, xlab = '', ylab = '') # 596 pre
# 596 - youngest pre-extinction subset
rarefaction.plot(rare.counts.596.pre.sub, ci.type = 'polygon', lcol = rare.cols[2], lwd = 2, add.line = T) # 596 Young subset
# 596 - resampling lines
for (i in 1:20) {
    counts.596.pre.sample <- counts.596.pre[sample(1:dim(counts.596.pre)[1], dim(counts.596.post)),] #random sample
    rare.counts.596.pre.sample <- apply(counts.596.pre.sample, 2, sum)
    rarefaction.plot(rare.counts.596.pre.sample, ci.type = 'off', lcol = rare.cols[3], add.line = T, lwd = 1)
}
# 596 - post-extinction
rarefaction.plot(rare.counts.596.post, ci.type = 'polygon', lcol = rare.cols[4], lwd = 2, add.line = T) # 596 post 

## annotations
box()
axis(2)
axis(3)
mtext("Morphotypes", side = 2, line = 2.5)
mtext("Sample Size", side = 3, line = 2.5)

# text("b", x = 0.5, y = 29.5, font = 2, cex = 1.2)

## Close out file
if(writeFile != 'off') {
    dev.off()
}
##### S7. Range chart with 50% Range Extensions #####

## 0. Set up figure
fig.dims <- c(7.5, 13) #Set Figure-dimensions

if(writeFile == 'pdf') {
    pdf('figures/S7_50_rangeExtensions.pdf', height = fig.dims[1], width = fig.dims[2], useDingbats = FALSE)
}

if(writeFile == 'jpg') {
    jpeg('figures/S7_50_rangeExtensions.jpg', height = fig.dims[1], width = fig.dims[2], units = 'in', res = 300)
}

# Define multi-panel
m <- rbind(c(1, 1, 1, 1, 1, 1,1, 1, 2, 3))
layout(m)
par(oma = c(0, 0, 4, 4))

## 1, Range Chart 
par(mar = c(5.1, 4.1, 4.1, 0))
# make range chart
rangechart(counts.fossil.family.matches, reorder = 'lad.by.fad', normalize.counts = FALSE, 
           col.points = 'by.count', cols.vec = col.rangechart, count.breaks = c(0, 1, 3, 5),
           cex.points = 'by.count', largesize = 1,
           tax.cat = cat.fossil.family.matches, pch.points = 'by.category', pch.vec = c(15, 16, 17, 5), 
           xaxis.labels = 'alphanum', yaxis.ticks = TRUE,
           print.xaxis = F, main = '', ylab = '',
           rangeExtensions = extensions.fossil.species.matches.50$new.lad, elty = 1, elcol = 'firebrick')

# Rectangle
rect(-10, sort(ext.fossils.modern.sub.50$new.lad)[sp.conf.50.95[1]], 100, sort(ext.fossils.modern.sub.50$new.lad)[sp.conf.50.95[2]], col = 'gray', border = NA)
par(new = T)
rangechart(counts.fossil.family.matches, reorder = 'lad.by.fad', normalize.counts = FALSE, 
           col.points = 'by.count', cols.vec = col.rangechart, count.breaks = c(0, 1, 3, 5), 
           cex.points = 'by.count', largesize = 1,
           tax.cat = cat.fossil.family.matches, pch.points = 'by.category', pch.vec = c(15, 16, 17, 5), 
           xaxis.labels = 'alphanum', yaxis.ticks = TRUE,
           print.xaxis = F, main = '', ylab = '',
           rangeExtensions = extensions.fossil.species.matches.50$new.lad, elty = 1, elcol = 'firebrick')

# tick marks and annotations
axis(2, at = rownames(counts.886), labels = FALSE, tcl = -0.3) #886 ticks on left axis as well
axis(2, at = rownames(counts.596.all), labels = FALSE, tcl = -0.3) #596 ticks on left axis

# annotations
mtext("DSDP 596 and ODP 886 combined with Modern - 50% Range Extensions", side = 3, line = 2, font = 2)
mtext("Age (Ma)", side = 2, line = 2.7)
mtext("Denticle Morphotype", side = 1, line = 3)

# Rangechart Legend
legend('topleft', legend = c(levels(denticle_geomlin$Classification)[1:4], '1', '2-3', '4-5', '6+', '1', '2-3', '4-5', '6+'),
                            pch = c(15, 16, 17, 5, 16, 16, 16, 16, 16, 16, 16, 16),
                            col = c(rep('black', 4), col.rangechart, rep('firebrick', 4)),
                            pt.cex = c(rep(1, 5), 1.33, 1.67, 2, 1, 1.33, 1.67, 2),
                            ncol = 3, title = '     Type             Count            Families', title.adj = 0)

## 2.ODP 596 accumulation 
par(mar = c(5.1, 0, 4.1, 0))

# Denticle Accumulation
plot(y = accum.596$age, x = accum.596$dentAR, type = plottype, 
     ylim = age.range.full, xlim = c(0, dent.accum.596.max),
     xlab = '', ylab = '', col = col.dentaccum, pch = pch.dentaccum, axes = F, lty = line.type, lwd = line.scale)
rect(-10, sort(ext.fossils.modern.sub.50$new.lad)[sp.conf.50.95[1]], 2*dent.accum.596.max, sort(ext.fossils.modern.sub.50$new.lad)[sp.conf.50.95[2]], col = 'gray', border = NA)
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


## 3. DSDP 886 Accumulation
par(mar = c(5.1, 0, 4.1, 0))

# Denticle accumulation
plot(y = accum.886$age, x = accum.886$dentAR, type = plottype, 
     ylim = age.range.full, xlim = c(0, dent.accum.886.max), 
     xlab = '', ylab = '', col = col.dentaccum, pch = 16, axes = F, lty = line.type, lwd = line.scale)
rect(-10, sort(ext.fossils.modern.sub.50$new.lad)[sp.conf.50.95[1]], 2*dent.accum.886.max, sort(ext.fossils.modern.sub.50$new.lad)[sp.conf.50.95[2]], col = 'gray', border = NA)
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

## 4. Add annotations to both graphs 

# axis labels
mtext('Denticle:Tooth ratio', side = 1, line = 3, col = col.ratio, at = 0)
mtext(expression("Denticles " * cm^-2 * myr^-1), side = 3, line = 1.7, col = col.dentaccum, at = 0, cex = 0.9)
mtext(expression("Teeth " * cm^-2 * myr^-1), side = 3, line = 6.2, col = col.toothaccum, at = 0, cex = 0.9)
mtext(expression("(4x denticle axis)"), line = 5.4, col = col.toothaccum, at = 0, cex = 0.75)

# legend
legend('topright', legend = c('Denticle accumulation', 'Tooth accumulation', 'Denticle:Tooth Ratio'), 
       lty = line.type, lwd = line.scale, pch = c(pch.dentaccum, pch.toothaccum, pch.ratio), 
       col = c(col.dentaccum, col.toothaccum, col.ratio), cex = 0.9)

if(writeFile != 'off') {
    dev.off()
}


##### S8. DSDP 596 and ODP 886 FOSSILS ONLY: Range chart + accumulation #####

## 0. Set up file
fig.dims <- c(7.5, 13) #Set Figure-dimensions

if(writeFile == 'pdf') {
    pdf('figures/S8_596_886_Fossils_Only_Rangechart.pdf', height = fig.dims[1], width = fig.dims[2], useDingbats = FALSE)
}

if(writeFile == 'jpg') {
    jpeg('figures/S8_596_886_Fossils_Only_Rangechart.jpg', height = fig.dims[1], width = fig.dims[2], units = 'in', res = 300)
}

# Define multi-panel
m <- rbind(c(1, 1, 1, 1, 1, 1,1, 1, 2, 3))
layout(m)
par(oma = c(0, 0, 4, 4))
# layout.show(3) #check if layout works

## 1, Range Chart 
par(mar = c(5.1, 4.1, 4.1, 0))
# make range chart
rangechart(counts.fossil, reorder = 'lad.by.fad', normalize.counts = FALSE, 
           col.points = 'by.count', cols.vec = col.rangechart, count.breaks = c(0, 1, 3, 5),
           cex.points = 'by.count', largesize = 1,
           tax.cat = cat.fossil.family.matches, pch.points = 'by.category', pch.vec = c(15, 16, 17, 5), 
           xaxis.labels = 'alphanum', yaxis.ticks = TRUE,
           print.xaxis = F, main = '', ylab = '',
           rangeExtensions = extensions.fossil.20$new.lad, elty = 1, elcol = 'firebrick')
# Rectangle
rect(-10, sort(ext.fossils.sub.20$new.lad)[sp.fossil.conf.20.95[1]], 100, sort(ext.fossils.sub.20$new.lad)[sp.fossil.conf.20.95[2]], col = 'gray', border = NA)
par(new = T)
rangechart(counts.fossil, reorder = 'lad.by.fad', normalize.counts = FALSE, 
           col.points = 'by.count', cols.vec = col.rangechart, count.breaks = c(0, 1, 3, 5),
           cex.points = 'by.count', largesize = 1,
           tax.cat = cat.fossil.family.matches, pch.points = 'by.category', pch.vec = c(15, 16, 17, 5), 
           xaxis.labels = 'alphanum', yaxis.ticks = TRUE,
           print.xaxis = F, main = '', ylab = '',
           rangeExtensions = extensions.fossil.20$new.lad, elty = 1, elcol = 'firebrick')


# tick marks
axis(2, at = rownames(counts.886), labels = FALSE, tcl = -0.3) #886 ticks on left axis as well
axis(2, at = rownames(counts.596.all), labels = FALSE, tcl = -0.3) #596 ticks on left axis

# annotations
mtext("DSDP 596 and ODP 886 Fossils Only", side = 3, line = 2, font = 2)
mtext("Age (Ma)", side = 2, line = 2.7)
mtext("Denticle Morphotype", side = 1, line = 3)

# legend
legend('topleft', legend = c(levels(denticle_geomlin$Classification)[1:4], '1', '2-3', '4-5', '6+'),
       pch = c(15, 16, 17, 5, 16, 16, 16, 16),
       col = c(rep('black', 4), col.rangechart),
       pt.cex = c(rep(1, 5), 1.33, 1.67, 2),
       ncol = 2, title = '     Type              Count', title.adj = 0)

##2.ODP 596 accumulatoin
par(mar = c(5.1, 0, 4.1, 0))

# Denticle Accumulation
plot(y = accum.596$age, x = accum.596$dentAR, type = plottype, 
     ylim = age.range.fossils, xlim = c(0, dent.accum.596.max),
     xlab = '', ylab = '', col = col.dentaccum, pch = pch.dentaccum, axes = F, lty = line.type, lwd = line.scale)
rect(-10, sort(ext.fossils.sub.20$new.lad)[sp.fossil.conf.20.95[1]], 2*dent.accum.596.max, sort(ext.fossils.sub.20$new.lad)[sp.fossil.conf.20.95[2]], col = 'gray', border = NA)
lines(y = accum.596$age, x = accum.596$dentAR, type = plottype, col = col.dentaccum, pch = pch.dentaccum, lty = line.type, lwd = line.scale)
box()
axis(2, labels = FALSE)
dent.axis.596 <- axis(3, col = col.dentaccum, col.axis = col.dentaccum)

# Tooth accumulation (4x scale) 
par(new = TRUE) 
plot(y = accum.596$age, x = accum.596$teethAR, type = plottype, lty = line.type, lwd = line.scale,
     ylim = age.range.fossils, xlim = c(0, teeth.accum.596.max),
     xlab = '', ylab = '', col = col.toothaccum, pch = pch.toothaccum, axes = F)
axis(3, col = col.toothaccum, col.axis = col.toothaccum, line = 3.5, at = dent.axis.596*accum.scale)

# Tooth to Denticle Ratio
par(new=TRUE)
plot(y = accum.596$age, x = (accum.596$dent/accum.596$teeth), type = plottype, 
     ylim = age.range.fossils,  xlim = c(0, ratio.596.max), 
     xlab = '', ylab = '', col = col.ratio, pch=pch.ratio, axes = F, lty = line.type, lwd = line.scale)
axis(1, col = col.ratio, col.axis = col.ratio)

# annotations
legend('bottomright', legend = c("South Pacific", "DSDP 596"), bty = 'n')


## 3. DSDP 886 Accumulation
par(mar = c(5.1, 0, 4.1, 0))

# Denticle accumulation
plot(y = accum.886$age, x = accum.886$dentAR, type = plottype, 
     ylim = age.range.fossils, xlim = c(0, dent.accum.886.max), 
     xlab = '', ylab = '', col = col.dentaccum, pch = 16, axes = F, lty = line.type, lwd = line.scale)
rect(-10, sort(ext.fossils.sub.20$new.lad)[sp.fossil.conf.20.95[1]], 2*dent.accum.886.max, sort(ext.fossils.sub.20$new.lad)[sp.fossil.conf.20.95[2]], col = 'gray', border = NA)
lines(y = accum.886$age, x = accum.886$dentAR, type = plottype, col = col.dentaccum, pch = pch.dentaccum, lty = line.type, lwd = line.scale)
box()
axis(2, labels = F)
dent.axis.886 <- axis(3, col = col.dentaccum, col.axis = col.dentaccum)

# Tooth accumulation (4x scale) 
par(new = TRUE) 
plot(y = accum.886$age, x = accum.886$teethAR, type = plottype, lty = line.type, lwd = line.scale,
     ylim = age.range.fossils, xlim = c(0, teeth.accum.886.max),
     xlab = '', ylab = '', col = col.toothaccum, pch = pch.toothaccum, axes = F)
axis(3, col = col.toothaccum, col.axis = col.toothaccum, line = 3.5, at = dent.axis.886*accum.scale)

# Tooth:Denticle ratio
par(new=TRUE)
plot(y=accum.886$age, x=(accum.886$dent/accum.886$teeth), type = plottype, 
     ylim = age.range.fossils, xlim = c(0, ratio.886.max), 
     xlab = '', ylab = '', col = col.ratio, pch=pch.ratio, axes = F, lty = line.type, lwd = line.scale)
axis(1, col = col.ratio, col.axis = col.ratio)
mtext('Age (Ma)', side = 4, line = 2.7)
axis(4, las = 1)

# annotations
legend('bottomright', legend = c("North Pacific", "ODP 886"), bty = 'n')
text("Hiatus", x = ratio.886.max/2, y = 16)

## Add annotations to both graphs 
# axis labels
mtext('Denticle:Tooth ratio', side = 1, line = 3, col = col.ratio, at = 0)
mtext(expression("Denticles " * cm^-2 * myr^-1), side = 3, line = 1.7, col = col.dentaccum, at = 0, cex = 0.9)
mtext(expression("Teeth " * cm^-2 * myr^-1), side = 3, line = 6.2, col = col.toothaccum, at = 0, cex = 0.9)
mtext(expression("(4x denticle axis)"), line = 5.4, col = col.toothaccum, at = 0, cex = 0.75)

# legend
legend('topright', legend = c('Denticle accumulation', 'Tooth accumulation', 'Denticle:Tooth Ratio'), 
       lty = line.type, lwd = line.scale, pch = c(pch.dentaccum, pch.toothaccum, pch.ratio), 
       col = c(col.dentaccum, col.toothaccum, col.ratio), cex = 0.9)

if(writeFile != 'off') {
    dev.off()
}

##### S9. DSDP 596 FOSSILS ONLY: Range chart + accumulation #####

## 0. set up plot
fig.dims <- c(6, 12.5) #Set Figure-dimensions

if(writeFile == 'pdf') {
    pdf('figures/S9_596_fossils_only_Rangechart.pdf', height = fig.dims[1], width = fig.dims[2], useDingbats = FALSE)
}

if(writeFile == 'jpg') {
    jpeg('figures/S8_596_fossils_only_Rangechart.jpg', height = fig.dims[1], width = fig.dims[2], units = 'in', res = 300)
}

# multi-panel
m <- rbind(c(1, 1, 1, 1, 1, 2))
layout(m)
par(oma = c(0, 0, 4, 4))


## Range chart DSDP 596
par(mar = c(5.1, 4.1, 4.1, 0))
# make range chart
rangechart(counts.596.all, reorder = 'lad.by.fad', normalize.counts = FALSE, 
           tax.cat = cat.fossil.family.matches[master.tax.596], taxa = master.tax.596, 
           col.points = 'by.count', cols.vec = col.rangechart, count.breaks = c(0, 1, 3, 5),
           pch.points = 'by.category', pch.vec = c(15, 16, 17, 18), 
           cex.points = 'by.count', largesize = 1,
           xaxis.labels = 'names', yaxis.ticks = TRUE, 
           print.xaxis = F, main = '', ylab = '',
           rangeExtensions = extensions.596.20$new.lad, elty = 1, elcol = 'firebrick')
# rectangle
rect(-10, sort(ext.596.sub.20$new.lad)[sp.conf.596.fossil.20.95[1]], 100, sort(ext.596.sub.20$new.lad)[sp.conf.596.fossil.20.95[2]], col = 'gray', border = NA)
par(new=T)
rangechart(counts.596.all, reorder = 'lad.by.fad', normalize.counts = FALSE, 
           tax.cat = cat.fossil.family.matches[master.tax.596], taxa = master.tax.596, 
           col.points = 'by.count', cols.vec = col.rangechart, count.breaks = c(0, 1, 3, 5),
           pch.points = 'by.category', pch.vec = c(15, 16, 17, 18), 
           cex.points = 'by.count', largesize = 1,
           xaxis.labels = 'names', yaxis.ticks = TRUE, 
           print.xaxis = F, main = '', ylab = '',
           rangeExtensions = extensions.596.20$new.lad, elty = 1, elcol = 'firebrick')

# tick marks
axis(2, at = rownames(counts.596.all), labels = FALSE, tcl = -0.3) #596 ticks on left axis

# annotations
mtext("DSDP 596 Fossils Only", side = 3, line = 2, font = 2)
mtext("Age (Ma)", side = 2, line = 2.7)
mtext("Denticle Morphotype", side = 1, line = 3)

# legend
legend('topleft', legend = c(levels(denticle_geomlin$Classification)[1:4], '1', '2-3', '4-5', '6+'),
       pch = c(15, 16, 17, 5, 16, 16, 16, 16),
       col = c(rep('black', 4), col.rangechart),
       pt.cex = c(rep(1, 5), 1.33, 1.67, 2),
       ncol = 2, title = '     Type              Count', title.adj = 0)


## 2.ODP 596 accumulation

par(mar = c(5.1, 0, 4.1, 0))

# Denticle Accumulation
plot(y = accum.596$age, x = accum.596$dentAR, type = plottype, 
     ylim = age.range.596.fossils, xlim = c(0, dent.accum.596.max),
     xlab = '', ylab = '', col = col.dentaccum, pch = pch.dentaccum, axes = F, lty = line.type, lwd = line.scale)
rect(-10, sort(ext.596.sub.20$new.lad)[sp.conf.596.fossil.20.95[1]], 2*dent.accum.596.max, sort(ext.596.sub.20$new.lad)[sp.conf.596.fossil.20.95[2]], col = 'gray', border = NA)
lines(y = accum.596$age, x = accum.596$dentAR, type = plottype, col = col.dentaccum, pch = pch.dentaccum, lty = line.type, lwd = line.scale)
box()
axis(2, labels = FALSE)
axis(4, las = 1)
mtext('Age (Ma)', side = 4, line = 2.7)
dent.axis.596 <- axis(3, col = col.dentaccum, col.axis = col.dentaccum)

# Tooth accumulation (4x scale) 
par(new = TRUE) 
plot(y = accum.596$age, x = accum.596$teethAR, type = plottype, lty = line.type, lwd = line.scale,
     ylim = age.range.596.fossils, xlim = c(0, teeth.accum.596.max),
     xlab = '', ylab = '', col = col.toothaccum, pch = pch.toothaccum, axes = F)
axis(3, col = col.toothaccum, col.axis = col.toothaccum, line = 3.5, at = dent.axis.596*accum.scale)


# Tooth to Denticle Ratio
par(new=TRUE)
plot(y = accum.596$age, x = (accum.596$dent/accum.596$teeth), type = plottype, 
     ylim = age.range.596.fossils,  xlim = c(0, ratio.596.max), 
     xlab = '', ylab = '', col = col.ratio, pch=pch.ratio, axes = F, lty = line.type, lwd = line.scale)
axis(1, col = col.ratio, col.axis = col.ratio)


legend('bottomright', legend = c("South Pacific", "DSDP 596"), bty = 'n')


## Add annotations to both graphs 
# axis labels
mtext('Denticle:Tooth ratio', side = 1, line = 3, col = col.ratio)
mtext(expression("Denticles " * cm^-2 * myr^-1), side = 3, line = 1.7, col = col.dentaccum, cex = 0.9)
mtext(expression("Teeth " * cm^-2 * myr^-1), side = 3, line = 6.2, col = col.toothaccum, cex = 0.9)
mtext(expression("(4x denticle axis)"), line = 5.4, col = col.toothaccum, cex = 0.75)

# legend
legend('topright', legend = c('Denticle accumulation', 'Tooth accumulation', 'Denticle:Tooth Ratio'), 
       lty = line.type, lwd = line.scale, pch = c(pch.dentaccum, pch.toothaccum, pch.ratio), 
       col = c(col.dentaccum, col.toothaccum, col.ratio), cex = 0.9)


if(writeFile != 'off') {
    dev.off()
}


##### S10. DSDP 596 and modern: Range chart + accumulation #####
## 0. set up file
fig.dims <- c(6, 12.5) #Set Figure-dimensions

if(writeFile == 'pdf') {
    pdf('figures/S10_596_modern_Rangechart.pdf', height = fig.dims[1], width = fig.dims[2], useDingbats = FALSE)
}

if(writeFile == 'jpg') {
    jpeg('figures/S10_596_modern_Rangechart.jpg', height = fig.dims[1], width = fig.dims[2], units = 'in', res = 300)
}

# set up multipanel
m <- rbind(c(1, 1, 1, 1, 1, 2))
layout(m)
par(oma = c(0, 0, 4, 4))

## 1. Range chart DSDP 596
par(mar = c(5.1, 4.1, 4.1, 0))
# make range chart
rangechart(counts.596.family, reorder = 'lad.by.fad', normalize.counts = FALSE, 
           tax.cat = cat.fossil.family.matches[master.tax.596], taxa = master.tax.596, 
           col.points = 'by.count', cols.vec = col.rangechart, count.breaks = c(0, 1, 3, 5),
           pch.points = 'by.category', pch.vec = c(15, 16, 17, 18), 
           cex.points = 'by.count', largesize = 1,
           xaxis.labels = 'names', yaxis.ticks = TRUE, 
           print.xaxis = F, main = '', ylab = '', 
           rangeExtensions = extensions.596.species.matches.20$new.lad, elty = 1, elcol = 'firebrick')
# rectangle
rect(-10, sort(ext.596.modern.sub.20$new.lad)[sp.conf.596.20.95[1]], 100, sort(ext.596.modern.sub.20$new.lad)[sp.conf.596.20.95[2]], col = 'gray', border = NA)
par(new = T)
rangechart(counts.596.family, reorder = 'lad.by.fad', normalize.counts = FALSE, 
           tax.cat = cat.fossil.family.matches[master.tax.596], taxa = master.tax.596, 
           col.points = 'by.count', cols.vec = col.rangechart, count.breaks = c(0, 1, 3, 5),
           pch.points = 'by.category', pch.vec = c(15, 16, 17, 18), 
           cex.points = 'by.count', largesize = 1,
           xaxis.labels = 'names', yaxis.ticks = TRUE, 
           print.xaxis = F, main = '', ylab = '', 
           rangeExtensions = extensions.596.species.matches.20$new.lad, elty = 1, elcol = 'firebrick')

# tick marks
axis(2, at = rownames(counts.596.all), labels = FALSE, tcl = -0.3) #596 ticks on left axis
# annotations
mtext("DSDP 596 combined with Modern", side = 3, line = 2, font = 2)
mtext("Age (Ma)", side = 2, line = 2.7)
mtext("Denticle Morphotype", side = 1, line = 3)

# Legend
legend('topleft', legend = c(levels(denticle_geomlin$Classification)[1:4], '1', '2-3', '4-5', '6+', '1', '2-3', '4-5', '6+'),
       pch = c(15, 16, 17, 5, 16, 16, 16, 16, 16, 16, 16, 16),
       col = c(rep('black', 4), col.rangechart, rep('firebrick', 4)),
       pt.cex = c(rep(1, 5), 1.33, 1.67, 2, 1, 1.33, 1.67, 2),
       ncol = 3, title = '     Type             Count            Families', title.adj = 0)

## 2.ODP 596 accumulation
par(mar = c(5.1, 0, 4.1, 0))

# Denticle Accumulation
plot(y = accum.596$age, x = accum.596$dentAR, type = plottype, 
     ylim = age.range.596, xlim = c(0, dent.accum.596.max),
     xlab = '', ylab = '', col = col.dentaccum, pch = pch.dentaccum, axes = F, lty = line.type, lwd = line.scale)
rect(-10, sort(ext.596.modern.sub.20$new.lad)[sp.conf.596.20.95[1]], 2*dent.accum.596.max, sort(ext.596.modern.sub.20$new.lad)[sp.conf.596.20.95[2]], col = 'gray', border = NA)
lines(y = accum.596$age, x = accum.596$dentAR, type = plottype, col = col.dentaccum, pch = pch.dentaccum, lty = line.type, lwd = line.scale)
box()
axis(2, labels = FALSE)
axis(4, las = 1)
mtext('Age (Ma)', side = 4, line = 2.7)
dent.axis.596 <- axis(3, col = col.dentaccum, col.axis = col.dentaccum)

# Tooth accumulation (4x scale) 
par(new = TRUE) 
plot(y = accum.596$age, x = accum.596$teethAR, type = plottype, lty = line.type, lwd = line.scale,
     ylim = age.range.596, xlim = c(0, teeth.accum.596.max),
     xlab = '', ylab = '', col = col.toothaccum, pch = pch.toothaccum, axes = F)
axis(3, col = col.toothaccum, col.axis = col.toothaccum, line = 3.5, at = dent.axis.596*accum.scale)


# Tooth to Denticle Ratio
par(new=TRUE)
plot(y = accum.596$age, x = (accum.596$dent/accum.596$teeth), type = plottype, 
     ylim = age.range.596,  xlim = c(0, ratio.596.max), 
     xlab = '', ylab = '', col = col.ratio, pch=pch.ratio, axes = F, lty = line.type, lwd = line.scale)
axis(1, col = col.ratio, col.axis = col.ratio)

# annotation
legend('bottomright', legend = c("South Pacific", "DSDP 596"), bty = 'n')

## Add annotations to both graphs 
# axis labels
mtext('Denticle:Tooth ratio', side = 1, line = 3, col = col.ratio)
mtext(expression("Denticles " * cm^-2 * myr^-1), side = 3, line = 1.7, col = col.dentaccum, cex = 0.9)
mtext(expression("Teeth " * cm^-2 * myr^-1), side = 3, line = 6.2, col = col.toothaccum, cex = 0.9)
mtext(expression("(4x denticle axis)"), line = 5.4, col = col.toothaccum, cex = 0.75)

# legend
legend('topright', legend = c('Denticle accumulation', 'Tooth accumulation', 'Denticle:Tooth Ratio'), 
       lty = line.type, lwd = line.scale, pch = c(pch.dentaccum, pch.toothaccum, pch.ratio), 
       col = c(col.dentaccum, col.toothaccum, col.ratio), cex = 0.9)

if(writeFile != 'off') {
    dev.off()
}


##### S11. ODP 886 FOSSILS ONLY: Range chart + accumulation #####
## 0. Set up file 
fig.dims <- c(6, 10) #Set Figure-dimensions

if(writeFile == 'pdf') {
    pdf('figures/S11_886_fossils_only_Rangechart.pdf', height = fig.dims[1], width = fig.dims[2], useDingbats = FALSE)
}

if(writeFile == 'jpg') {
    jpeg('figures/S11_886_fossils_only_Rangechart.jpg', height = fig.dims[1], width = fig.dims[2], units = 'in', res = 300)
}

# multi-panel
m <- rbind(c(1, 1, 1, 1, 1, 2))
layout(m)
par(oma = c(0, 0, 4, 4))

# Range chart ODP 886
par(mar = c(5.1, 4.1, 4.1, 0))

rangechart(counts.886, reorder = 'lad.by.fad', normalize.counts = FALSE, 
           tax.cat = cat.fossil.family.matches[master.tax.886], taxa = master.tax.886, 
           col.points = 'by.count', cols.vec = 'viridis', count.breaks = c(0, 1, 3, 5),
           pch.points = 'by.category', pch.vec = c(15, 16, 17, 18), 
           cex.points = 'by.count', largesize = 1,
           xaxis.labels = 'names', yaxis.ticks = TRUE, 
           print.xaxis = FALSE, main = '', ylab = '')
# rectangle
rect(-10, sort(ext.fossils.modern.sub.20$new.lad)[sp.conf.20.95[1]], 100, sort(ext.fossils.modern.sub.20$new.lad)[sp.conf.20.95[2]], col = 'gray', border = NA)
par(new = T)
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
legend('topleft', legend = c(levels(denticle_geomlin$Classification)[1:4], '1', '2-3', '4-5', '6+'),
       pch = c(15, 16, 17, 5, 16, 16, 16, 16),
       col = c(rep('black', 4), viridis(4)),
       pt.cex = c(rep(1, 5), 1.33, 1.67, 2),
       ncol = 2, title = '     Type              Count', title.adj = 0)


## 2. DSDP 886 Accumulation 

par(mar = c(5.1, 0, 4.1, 0))

# Denticle accumulation
plot(y = accum.886$age, x = accum.886$dentAR, type = plottype, 
     ylim = age.range.886.fossils, xlim = c(0, dent.accum.886.max), 
     xlab = '', ylab = '', col = col.dentaccum, pch = 16, axes = F, lty = line.type, lwd = line.scale)
rect(-10, sort(ext.fossils.modern.sub.20$new.lad)[sp.conf.20.95[1]], dent.accum.886.max*2, sort(ext.fossils.modern.sub.20$new.lad)[sp.conf.20.95[2]], col = 'gray', border = NA)
lines(y = accum.886$age, x = accum.886$dentAR, type = plottype, col = col.dentaccum, pch = pch.dentaccum, lty = line.type, lwd = line.scale)
box()
axis(2, labels = F)
dent.axis.886 <- axis(3, col = col.dentaccum, col.axis = col.dentaccum)

# Tooth accumulation (4x scale) 
par(new = TRUE) 
plot(y = accum.886$age, x = accum.886$teethAR, type = plottype, lty = line.type, lwd = line.scale,
     ylim = age.range.886.fossils, xlim = c(0, teeth.accum.886.max),
     xlab = '', ylab = '', col = col.toothaccum, pch = pch.toothaccum, axes = F)
axis(3, col = col.toothaccum, col.axis = col.toothaccum, line = 3.5, at = dent.axis.886*accum.scale)

# Tooth:Denticle ratio
par(new=TRUE)
plot(y=accum.886$age, x=(accum.886$dent/accum.886$teeth), type = plottype, 
     ylim = age.range.886.fossils, xlim = c(0, ratio.886.max), 
     xlab = '', ylab = '', col = col.ratio, pch=pch.ratio, axes = F, lty = line.type, lwd = line.scale)
axis(1, col = col.ratio, col.axis = col.ratio)
mtext('Age (Ma)', side = 4, line = 2.7)
axis(4, las = 1)

# annotations
legend('bottomright', legend = c("North Pacific", "ODP 886"), bty = 'n')
text("Hiatus", x = ratio.886.max/2, y = 17)

## Add annotations to both graphs 
# axis labels
mtext('Denticle:Tooth ratio', side = 1, line = 3, col = col.ratio)
mtext(expression("Denticles " * cm^-2 * myr^-1), side = 3, line = 1.7, col = col.dentaccum, cex = 0.9)
mtext(expression("Teeth " * cm^-2 * myr^-1), side = 3, line = 6.2, col = col.toothaccum, cex = 0.9)
mtext(expression("(4x denticle axis)"), line = 5.4, col = col.toothaccum, cex = 0.75)

# legend
legend(x=0.1, y=13, legend = c('Denticle accumulation', 'Tooth accumulation', 'Denticle:Tooth Ratio'), 
       lty = line.type, lwd = line.scale, pch = c(pch.dentaccum, pch.toothaccum, pch.ratio), 
       col = c(col.dentaccum, col.toothaccum, col.ratio), cex = 0.9)


if(writeFile != 'off') {
    dev.off()
}


##### S12. ODP 886 and modern: Range chart + accumulation #####
## 0. set up figure
fig.dims <- c(6, 10) #Set Figure-dimensions

if(writeFile == 'pdf') {
    pdf('figures/S12_886_modern_Rangechart.pdf', height = fig.dims[1], width = fig.dims[2], useDingbats = FALSE)
}

if(writeFile == 'jpg') {
    jpeg('figures/S12_886_modern_Rangechart.jpg', height = fig.dims[1], width = fig.dims[2], units = 'in', res = 300)
}

# multipanel
m <- rbind(c(1, 1, 1, 1, 1, 2))
layout(m)
par(oma = c(0, 0, 4, 4))

## 1. Range chart ODP 886
par(mar = c(5.1, 4.1, 4.1, 0))

# make range chart
rangechart(counts.886.family, reorder = 'lad.by.fad', normalize.counts = FALSE, 
           tax.cat = cat.fossil.family.matches[master.tax.886], taxa = master.tax.886, 
           col.points = 'by.count', cols.vec = col.rangechart, count.breaks = c(0, 1, 3, 5),
           pch.points = 'by.category', pch.vec = c(15, 16, 17, 18), 
           cex.points = 'by.count', largesize = 1,
           xaxis.labels = 'names', yaxis.ticks = TRUE, 
           print.xaxis = FALSE, main = '', ylab = '')
# rectangle
rect(-10, sort(ext.fossils.modern.sub.20$new.lad)[sp.conf.20.95[1]], 100, sort(ext.fossils.modern.sub.20$new.lad)[sp.conf.20.95[2]], col = 'gray', border = NA)
par(new = T)
rangechart(counts.886.family, reorder = 'lad.by.fad', normalize.counts = FALSE, 
           tax.cat = cat.fossil.family.matches[master.tax.886], taxa = master.tax.886, 
           col.points = 'by.count', cols.vec = col.rangechart, count.breaks = c(0, 1, 3, 5),
           pch.points = 'by.category', pch.vec = c(15, 16, 17, 18), 
           cex.points = 'by.count', largesize = 1,
           xaxis.labels = 'names', yaxis.ticks = TRUE, 
           print.xaxis = FALSE, main = '', ylab = '')

# Add annotations
mtext("ODP 886 combined with Modern", side = 3, line = 2, font = 2)
mtext("Age (Ma)", side = 2, line = 2.7)
mtext("Denticle Morphotype", side = 1, line = 3)

# Rangechart Legend
legend('topleft', legend = c(levels(denticle_geomlin$Classification)[1:4], '1', '2-3', '4-5', '6+', '1', '2-3', '4-5', '6+'),
       pch = c(15, 16, 17, 5, 16, 16, 16, 16, 16, 16, 16, 16),
       col = c(rep('black', 4), col.rangechart, rep('firebrick', 4)),
       pt.cex = c(rep(1, 5), 1.33, 1.67, 2, 1, 1.33, 1.67, 2),
       ncol = 3, title = '     Type             Count            Families', title.adj = 0)


## 2. DSDP 886 Accumulation 

par(mar = c(5.1, 0, 4.1, 0))

# Denticle accumulation
plot(y = accum.886$age, x = accum.886$dentAR, type = plottype, 
     ylim = age.range.886, xlim = c(0, dent.accum.886.max), 
     xlab = '', ylab = '', col = col.dentaccum, pch = 16, axes = F, lty = line.type, lwd = line.scale)
rect(-10, sort(ext.fossils.modern.sub.20$new.lad)[sp.conf.20.95[1]], dent.accum.886.max*2, sort(ext.fossils.modern.sub.20$new.lad)[sp.conf.20.95[2]], col = 'gray', border = NA)
lines(y = accum.886$age, x = accum.886$dentAR, type = plottype, col = col.dentaccum, pch = pch.dentaccum, lty = line.type, lwd = line.scale)
box()
axis(2, labels = F)
dent.axis.886 <- axis(3, col = col.dentaccum, col.axis = col.dentaccum)

# Tooth accumulation (4x scale) 
par(new = TRUE) 
plot(y = accum.886$age, x = accum.886$teethAR, type = plottype, lty = line.type, lwd = line.scale,
     ylim = age.range.886, xlim = c(0, teeth.accum.886.max),
     xlab = '', ylab = '', col = col.toothaccum, pch = pch.toothaccum, axes = F)
axis(3, col = col.toothaccum, col.axis = col.toothaccum, line = 3.5, at = dent.axis.886*accum.scale)

# Tooth:Denticle ratio
par(new=TRUE)
plot(y=accum.886$age, x=(accum.886$dent/accum.886$teeth), type = plottype, 
     ylim = age.range.886, xlim = c(0, ratio.886.max), 
     xlab = '', ylab = '', col = col.ratio, pch=pch.ratio, axes = F, lty = line.type, lwd = line.scale)
axis(1, col = col.ratio, col.axis = col.ratio)
mtext('Age (Ma)', side = 4, line = 2.7)
axis(4, las = 1)

# annotations
legend('bottomright', legend = c("North Pacific", "ODP 886"), bty = 'n')
text("Hiatus", x = ratio.886.max/2, y = 16)

## Add annotations to both graphs 
# axis labels
mtext('Denticle:Tooth ratio', side = 1, line = 3, col = col.ratio)
mtext(expression("Denticles " * cm^-2 * myr^-1), side = 3, line = 1.7, col = col.dentaccum, cex = 0.9)
mtext(expression("Teeth " * cm^-2 * myr^-1), side = 3, line = 6.2, col = col.toothaccum, cex = 0.9)
mtext(expression("(4x denticle axis)"), line = 5.4, col = col.toothaccum, cex = 0.75)

# legend
legend('topright', legend = c('Denticle accumulation', 'Tooth accumulation', 'Denticle:Tooth Ratio'), 
       lty = line.type, lwd = line.scale, pch = c(pch.dentaccum, pch.toothaccum, pch.ratio), 
       col = c(col.dentaccum, col.toothaccum, col.ratio), cex = 0.9)


if(writeFile != 'off') {
    dev.off()
}



##### S13. Miocene Hiatus Map #####
# The Miocene Map figure and analysis are made fully in the stand-alone MioceneMap.R script
source('code/MioceneMap.R')


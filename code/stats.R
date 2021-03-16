#########################################################
#                                                       #
#       Statistics and Percentages Calculated           #
#       Last updated 3/15/2020 by ECS                   #
#                                                       #
#########################################################

# This file is automatically run as source when Setup.R is run. 
#   It relies on Setup.R having been run first to create the environment necessary
#   to calculate these metrics. 

######### Step 1: Open file #####

sink("Output_Stats.txt") 

######### Step 2: Annotate file #####

cat('This file contains all output statisitcs referenced in the manuscript\n')
cat('It is generated automatically when the Setup.R file is sourced\n\n')
cat(paste('This file was generated: ', Sys.time(), '\n\n', sep = ''))


######### Step 3: Calculate various statistics #####

##### Modern shark diversity #####
cat('##### Modern shark diversity ######\n\n')

cat(paste('There are ', length(unique(dat.modern$Accepted_Name)), ' species of modern shark included in this analysis.\n', sep = ''))
cat(paste('There are ', length(unique(dat.modern$Genus)), ' genera of modern shark included in this analysis.\n', sep = ''))
cat(paste('There are ', length(unique(dat.modern$Family)), ' families of modern shark included in this analysis.\n', sep = ''))
cat(paste('There are ', length(unique(dat.modern$Order)), ' orders of modern shark included in this analysis.\n', sep = ''))
cat(paste('There are ', length(unique(dat.modern$Type)), ' unique types of denticles identified across ', length(dat.modern$Type), ' denticle images.\n', sep = ''))
cat(paste('Of these, ', length(modern.fossil.matches), ' modern morphotypes have representation among the ', length(taxa.fossil), ' morphotypes identified in the fossil record.\n', sep = ''))
cat(paste('This represents a ', round((1-(length(modern.fossil.matches)/length(taxa.fossil)))*100, 3), '% Extinction.\n', sep = ''))



cat('\n\n') #section break

##### Denticles per species #####
cat('##### Denticles per taxon ######\n\n')

cat(paste('There are ', round(mean(types_per_species), 3), ' denticle types per species on average, with a median of ', round(median(types_per_species), 3), ' type per species in the dataset.\n', sep = ''))
cat(paste('There are ', round(mean(types_per_genus), 3), ' denticle types per genus on average, with a median of ', round(median(types_per_genus), 3), ' type per genus in the dataset.\n', sep = ''))
cat(paste('There are ', round(mean(types_per_family), 3), ' denticle types per family on average, with a median of ', round(median(types_per_family), 3), ' type per family in the dataset.\n', sep = ''))
cat(paste('There are ', round(mean(types_per_order), 3), ' denticle types per order on average, with a median of ', round(median(types_per_order), 3), ' type per order in the dataset.\n', sep = ''))

cat('\n\n') #section break


##### Range Extension Calculation Values #####
cat('##### Range Extension Values (Supplemental Table T5) ######\n\n')
cat('## For the combined fossil dataset, including DSDP 596 and ODP 886, with modern matches:\n')
cat('50% Range Extensions:\n')
cat(paste('\t The estimated extinction age is ', round(median(ext.fossils.modern.sub.50$new.lad), 3), ' Ma\n', sep = ''))
cat(paste('\t The 95% confidence for age of extinction is ', round(sort(ext.fossils.modern.sub.50$new.lad)[sp.conf.50.95[1]], 3), ' to ', round(sort(ext.fossils.modern.sub.50$new.lad)[sp.conf.50.95[2]], 3), ' Ma\n', sep = ''))
cat(paste('\t The 99% confidence for age of extinction is ', round(sort(ext.fossils.modern.sub.50$new.lad)[sp.conf.50.99[1]], 3), ' to ', round(sort(ext.fossils.modern.sub.50$new.lad)[sp.conf.50.99[2]], 3), ' Ma\n', sep = ''))

cat('20% Range Extensions:\n')
cat(paste('\t The estimated extinction age is ', round(quantile(ext.fossils.modern.sub.20$new.lad, 0.2), 3), ' Ma\n', sep = ''))
cat(paste('\t The 95% confidence for age of extinction is ', round(sort(ext.fossils.modern.sub.20$new.lad)[sp.conf.20.95[1]],3), ' to ', round(sort(ext.fossils.modern.sub.20$new.lad)[sp.conf.20.95[2]],3), ' Ma\n', sep = ''))
cat(paste('\t The 99% confidence for age of extinction is ', round(sort(ext.fossils.modern.sub.20$new.lad)[sp.conf.20.99[1]],3), ' to ', round(sort(ext.fossils.modern.sub.20$new.lad)[sp.conf.20.99[2]],3), ' Ma\n', sep = ''))

cat('\n')
cat('## For only DSDP Site 596, with modern matches:\n')
cat('50% Range Extensions:\n')
cat(paste('\t The estimated extinction age is ', round(median(ext.596.modern.sub.50$new.lad), 3), ' Ma\n', sep = ''))
cat(paste('\t The 95% confidence for age of extinction is ', round(sort(ext.596.modern.sub.50$new.lad)[sp.conf.596.50.95[1]], 3), ' to ', round(sort(ext.596.modern.sub.50$new.lad)[sp.conf.596.50.95[2]], 3), ' Ma\n', sep = ''))
cat(paste('\t The 99% confidence for age of extinction is ', round(sort(ext.596.modern.sub.50$new.lad)[sp.conf.596.50.99[1]], 3), ' to ', round(sort(ext.596.modern.sub.50$new.lad)[sp.conf.596.50.99[2]], 3), ' Ma\n', sep = ''))

cat('20% Range Extensions:\n')
cat(paste('\t The estimated extinction age is ', round(quantile(ext.596.modern.sub.20$new.lad, 0.2), 3), ' Ma\n', sep = ''))
cat(paste('\t The 95% confidence for age of extinction is ', round(sort(ext.596.modern.sub.20$new.lad)[sp.conf.596.20.95[1]],3), ' to ', round(sort(ext.596.modern.sub.20$new.lad)[sp.conf.596.20.95[2]],3), ' Ma\n', sep = ''))
cat(paste('\t The 99% confidence for age of extinction is ', round(sort(ext.596.modern.sub.20$new.lad)[sp.conf.596.20.99[1]],3), ' to ', round(sort(ext.596.modern.sub.20$new.lad)[sp.conf.596.20.99[2]],3), ' Ma\n', sep = ''))

cat('\n')
cat('## For combined fossil dataset with no modern matches (DSDP 596 + ODP 886):\n')
cat('50% Range Extensions:\n')
cat(paste('\t The estimated extinction age is ', round(median(ext.fossils.sub.50$new.lad), 3), ' Ma\n', sep = ''))
cat(paste('\t The 95% confidence for age of extinction is ', round(sort(ext.fossils.sub.50$new.lad)[sp.fossil.conf.50.95[1]], 3), ' to ', round(sort(ext.fossils.sub.50$new.lad)[sp.fossil.conf.50.95[2]], 3), ' Ma\n', sep = ''))
cat(paste('\t The 99% confidence for age of extinction is ', round(sort(ext.fossils.sub.50$new.lad)[sp.fossil.conf.50.99[1]], 3), ' to ', round(sort(ext.fossils.sub.50$new.lad)[sp.fossil.conf.50.99[2]], 3), ' Ma\n', sep = ''))

cat('20% Range Extensions:\n')
cat(paste('\t The estimated extinction age is ', round(quantile(ext.fossils.sub.20$new.lad, 0.2), 3), ' Ma\n', sep = ''))
cat(paste('\t The 95% confidence for age of extinction is ', round(sort(ext.fossils.sub.20$new.lad)[sp.fossil.conf.20.95[1]],3), ' to ', round(sort(ext.fossils.sub.20$new.lad)[sp.fossil.conf.20.95[2]],3), ' Ma\n', sep = ''))
cat(paste('\t The 99% confidence for age of extinction is ', round(sort(ext.fossils.sub.20$new.lad)[sp.fossil.conf.20.99[1]],3), ' to ', round(sort(ext.fossils.sub.20$new.lad)[sp.fossil.conf.20.99[2]],3), ' Ma\n', sep = ''))

cat('\n')
cat('## For DSDP 596 dataset only with no modern matches:\n')
cat('50% Range Extensions:\n')
cat(paste('\t The estimated extinction age is ', round(median(ext.596.sub.50$new.lad), 3), ' Ma\n', sep = ''))
cat(paste('\t The 95% confidence for age of extinction is ', round(sort(ext.596.sub.50$new.lad)[sp.conf.596.fossil.50.95[1]], 3), ' to ', round(sort(ext.596.sub.50$new.lad)[sp.conf.596.fossil.50.95[2]], 3), ' Ma\n', sep = ''))
cat(paste('\t The 99% confidence for age of extinction is ', round(sort(ext.596.sub.50$new.lad)[sp.conf.596.fossil.50.99[1]], 3), ' to ', round(sort(ext.596.sub.50$new.lad)[sp.conf.596.fossil.50.99[2]], 3), ' Ma\n', sep = ''))

cat('20% Range Extensions:\n')
cat(paste('\t The estimated extinction age is ', round(quantile(ext.596.sub.20$new.lad, 0.2), 3), ' Ma\n', sep = ''))
cat(paste('\t The 95% confidence for age of extinction is ', round(sort(ext.596.sub.20$new.lad)[sp.conf.596.fossil.20.95[1]],3), ' to ', round(sort(ext.596.sub.20$new.lad)[sp.conf.596.fossil.20.95[2]],3), ' Ma\n', sep = ''))
cat(paste('\t The 99% confidence for age of extinction is ', round(sort(ext.596.sub.20$new.lad)[sp.conf.596.fossil.20.99[1]],3), ' to ', round(sort(ext.596.sub.20$new.lad)[sp.conf.596.fossil.20.99[2]],3), ' Ma\n', sep = ''))

cat('\n\n') #section break


##### Community Composition Percentages #####

# Create lists of type-categories per fossil datasets. This is only used here, doesn't need to go to setup.R
type.cat.886 <- type.cat.lookup(taxaList = as.character(type.886), typeList = denticle_geomlin)
type.cat.596 <- type.cat.lookup(taxaList = as.character(type.596), typeList = denticle_geomlin)
type.age.596 <- data.frame(age = as.numeric(age.596), taxa = as.character(type.596), type = type.cat.596)
type.age.886 <- data.frame(age = as.numeric(age.886), taxa = as.character(type.886), type = type.cat.886)
# Final data frame: 
type.cat.fossil <- rbind (type.age.596, type.age.886)

## Type Key:
# 1 = geometric
# 2 = linear
# 3 = smooth
# 4 = spine [modern only]
# 5 = z (unclassifiable, but some sort of denticle fragment; used for fossils)

cat('##### Community Composition Percentages ######\n\n')

## Fossils only
cat('For the FOSSIL ONLY dataset (combines DSDP 596 and ODP 886):\n')
cat(paste('There are ', length(type.cat.fossil[,1]), ' denticles in this dataset, of which ', sum(counts.596, counts.886), ' are identifiable to a Morphotype or category:\n', sum(counts.596), ' from DSDP Site 596\n', sum(counts.886), ' from ODP Site 886.\n', sep = ''))

cat('\n')

## Broken down by type (full dataset): 
table.fossil.types <- table(factor(type.cat.fossil$type, levels = 1:5))

cat('The full fossil dataset consists of:\n')
cat(paste(table.fossil.types[which(names(table.fossil.types) == 1)], ' Geometric (', round(table.fossil.types[which(names(table.fossil.types) == 1)]/sum(table.fossil.types)*100, 2), ' %)\n', sep = ''))
cat(paste(table.fossil.types[which(names(table.fossil.types) == 2)], ' Linear (', round(table.fossil.types[which(names(table.fossil.types) == 2)]/sum(table.fossil.types)*100, 2), ' %)\n', sep = ''))
cat(paste(table.fossil.types[which(names(table.fossil.types) == 3)], ' Smooth (', round(table.fossil.types[which(names(table.fossil.types) == 3)]/sum(table.fossil.types)*100, 2), ' %)\n', sep = ''))
cat(paste(table.fossil.types[which(names(table.fossil.types) == 4)], ' Spines (', round(table.fossil.types[which(names(table.fossil.types) == 4)]/sum(table.fossil.types)*100, 2), ' %)\n', sep = ''))
cat(paste(table.fossil.types[which(names(table.fossil.types) == 5)], ' Unclassifiable Denticles (', round(table.fossil.types[which(names(table.fossil.types) == 5)]/sum(table.fossil.types)*100, 2), ' %)\n', sep = ''))

cat('\n')
## Pre-Extinction Stats: 
fossils.pre <- subset(type.cat.fossil, age >= 19)
table.fossil.types.pre <- table(factor(fossils.pre$type, levels = 1:5))

cat('The pre-extinction fossil dataset consists of:\n')
cat(paste(table.fossil.types.pre[which(names(table.fossil.types.pre) == 1)], ' Geometric (', round(table.fossil.types.pre[which(names(table.fossil.types.pre) == 1)]/sum(table.fossil.types.pre)*100, 2), ' %)\n', sep = ''))
cat(paste(table.fossil.types.pre[which(names(table.fossil.types.pre) == 2)], ' Linear (', round(table.fossil.types.pre[which(names(table.fossil.types.pre) == 2)]/sum(table.fossil.types.pre)*100, 2), ' %)\n', sep = ''))
cat(paste(table.fossil.types.pre[which(names(table.fossil.types.pre) == 3)], ' Smooth (', round(table.fossil.types.pre[which(names(table.fossil.types.pre) == 3)]/sum(table.fossil.types.pre)*100, 2), ' %)\n', sep = ''))
cat(paste(table.fossil.types.pre[which(names(table.fossil.types.pre) == 4)], ' Spines (', round(table.fossil.types.pre[which(names(table.fossil.types.pre) == 4)]/sum(table.fossil.types.pre)*100, 2), ' %)\n', sep = ''))
cat(paste(table.fossil.types.pre[which(names(table.fossil.types.pre) == 5)], ' Unclassifiable Denticles (', round(table.fossil.types.pre[which(names(table.fossil.types.pre) == 5)]/sum(table.fossil.types.pre)*100, 2), ' %)\n', sep = ''))
cat(paste('For a total of ', sum(table.fossil.types.pre), ' denticles pre-extinction.\n', sep = ''))

cat('\n')

## Post-Extinction Stats: 
fossils.post <- subset(type.cat.fossil, age <= 19)
table.fossil.types.post <- table(factor(fossils.post$type, levels = 1:5))

cat('The post-extinction fossil dataset consists of:\n')
cat(paste(table.fossil.types.post[which(names(table.fossil.types.post) == 1)], ' Geometric (', round(table.fossil.types.post[which(names(table.fossil.types.post) == 1)]/sum(table.fossil.types.post)*100, 2), ' %)\n', sep = ''))
cat(paste(table.fossil.types.post[which(names(table.fossil.types.post) == 2)], ' Linear (', round(table.fossil.types.post[which(names(table.fossil.types.post) == 2)]/sum(table.fossil.types.post)*100, 2), ' %)\n', sep = ''))
cat(paste(table.fossil.types.post[which(names(table.fossil.types.post) == 3)], ' Smooth (', round(table.fossil.types.post[which(names(table.fossil.types.post) == 3)]/sum(table.fossil.types.post)*100, 2), ' %)\n', sep = ''))
cat(paste(table.fossil.types.post[which(names(table.fossil.types.post) == 4)], ' Spines (', round(table.fossil.types.post[which(names(table.fossil.types.post) == 4)]/sum(table.fossil.types.post)*100, 2), ' %)\n', sep = ''))
cat(paste(table.fossil.types.post[which(names(table.fossil.types.post) == 5)], ' Unclassifiable Denticles (', round(table.fossil.types.post[which(names(table.fossil.types.post) == 5)]/sum(table.fossil.types.post)*100, 2), ' %)\n', sep = ''))
cat(paste('For a total of ', sum(table.fossil.types.post), ' denticles post-extinction.\n', sep = ''))

cat('\n')

## Chi-squared test
chisq.tbl <- as.matrix(cbind(table.fossil.types.pre, table.fossil.types.post))[-4,] #remove the zero-values for spines here
cat('Using a Chi-square test of independence, we calculate that the pre-extinction denticle assemblage is significantly different from the post-extinction denticle assemblage:\n')
cat(capture.output(chisq.test(chisq.tbl)))
cat('\n\n\n')

### Excluding the unclassifiable denticles 

##### Fossils ONLY: Morphotype Extinction calculations #####
cat('##### Fossils ONLY: Morphotype Extinction Calculations #####\n\n')

cat(paste('There are ', length(unique(type.cat.fossil$taxa)), ' fossil morphotypes, ', length(unique(dat.596$Type)), ' observed at DSDP Site 596 and ', length(unique(dat.886$Type)), ' observed at ODP Site 886.\n'))
cat(paste(dim(unique(subset(type.cat.fossil, type == 1, select = taxa)))[1], ' geometric morphotypes, including "GenGeom"\n', sep = ''))
cat(paste(dim(unique(subset(type.cat.fossil, type == 2, select = taxa)))[1], ' linear morphotypes, including "GenLin"\n', sep = ''))
cat(paste(dim(unique(subset(type.cat.fossil, type == 3, select = taxa)))[1], ' smooth morphotypes\n', sep = ''))
cat(paste(dim(unique(subset(type.cat.fossil, type == 4, select = taxa)))[1], ' spine morphotypes\n', sep = ''))
cat(paste(dim(unique(subset(type.cat.fossil, type == 5, select = taxa)))[1], ' unclassifiable catch-all type\n', sep = ''))

cat('\n')

cat(paste('Of these, ', length(unique(fossils.pre$taxa)), ' morphotypes are present prior to 19 Ma, while only ', length(unique(fossils.post$taxa)), ' are present in the fossil record after the extinction event, a ', round((1-(length(unique(fossils.post$taxa))/length(unique(fossils.pre$taxa))))*100, 3), '% decline in morphotype diversity in open ocean sediments\n'))

cat(paste('Of the ', dim(unique(subset(fossils.pre, type == 1, select = taxa)))[1], ' geometric morphotypes, ', dim(unique(subset(fossils.post, type == 1, select = taxa)))[1], ' are found after the extinction (', round((1-(dim(unique(subset(fossils.post, type == 1, select = taxa)))[1]/dim(unique(subset(fossils.pre, type == 1, select = taxa)))[1]))*100, 3), '% observed extinction)\n', sep = ''))


cat(paste('Of the ', dim(unique(subset(fossils.pre, type == 2, select = taxa)))[1], ' linear morphotypes, ', dim(unique(subset(fossils.post, type == 2, select = taxa)))[1], ' are found after the extinction (', round((1-(dim(unique(subset(fossils.post, type == 2, select = taxa)))[1]/dim(unique(subset(fossils.pre, type == 2, select = taxa)))[1]))*100, 3), '% observed extinction)\n', sep = ''))

cat('\n')

### Exclude the 'z' morphotype

cat('If we exclude the unclassiiable morphotype, these numbers become:\n')
fossils.pre <- subset(fossils.pre, fossils.pre$type != 5)
fossils.post <- subset(fossils.post, fossils.post$type != 5)

cat(paste('Of the ', length(unique(fossils.pre$taxa)), ' morphotypes present prior to 19 Ma,  only ', length(unique(fossils.post$taxa)), ' are present in the fossil record after the extinction event, a ', round((1-(length(unique(fossils.post$taxa))/length(unique(fossils.pre$taxa))))*100, 3), '% decline in morphotype diversity in open ocean sediments\n', sep = ''))

cat(paste('Of the ', dim(unique(subset(fossils.pre, type == 1, select = taxa)))[1], ' geometric morphotypes, ', dim(unique(subset(fossils.post, type == 1, select = taxa)))[1], ' are found after the extinction (', round((1-(dim(unique(subset(fossils.post, type == 1, select = taxa)))[1]/dim(unique(subset(fossils.pre, type == 1, select = taxa)))[1]))*100, 3), '% observed extinction)\n', sep = ''))


cat(paste('Of the ', dim(unique(subset(fossils.pre, type == 2, select = taxa)))[1], ' linear morphotypes, ', dim(unique(subset(fossils.post, type == 2, select = taxa)))[1], ' are found after the extinction (', round((1-(dim(unique(subset(fossils.post, type == 2, select = taxa)))[1]/dim(unique(subset(fossils.pre, type == 2, select = taxa)))[1]))*100, 3), '% observed extinction)\n', sep = ''))

cat('\n')
cat(paste('The ', length(unique(fossils.post$taxa)), ' morphotypes which are present in the <19 Ma fossil record (e.g. the survivors) are:\n', sep = ''))
cat(paste(sort(unique(fossils.post$taxa)), collapse = '\n'))

cat('\n\n')
cat(paste('There are ', length(unique(subset(sub.596, sub.596$Age <= 19)$Type))-1, ' unique surviving morphotypes at DSDP Site 596, and ', length(unique(subset(sub.886, sub.886$Age <= 19)$Type)), ' at ODP Site 886' ))

# # overlap between 596 and 886 survivors
# which(unique(subset(sub.596, sub.596$Age <= 19)$Type) %in% unique(subset(sub.886, sub.886$Age <= 19)$Type))

cat('\n\n')


### Exclude the 'genlin' and 'gengeo' morphotypes

cat('If we *also* exclude the generic "genlin" and "gengeo" morphotypes, these numbers become:\n')
fossils.pre.gen <- subset(fossils.pre, fossils.pre$taxa != 'GenLin' & fossils.pre$taxa != 'GenGeo')
fossils.post.gen <- subset(fossils.post, fossils.post$taxa != 'GenLin' & fossils.post$taxa != 'GenGeo')

cat(paste('Of the, ', length(unique(fossils.pre.gen$taxa)), ' morphotypes present prior to 19 Ma,  only ', length(unique(fossils.post.gen$taxa)), ' are present in the fossil record after the extinction event, a ', round((1-(length(unique(fossils.post.gen$taxa))/length(unique(fossils.pre.gen$taxa))))*100, 3), '% decline in morphotype diversity in open ocean sediments\n'))

cat(paste('Of the ', dim(unique(subset(fossils.pre.gen, type == 1, select = taxa)))[1], ' geometric morphotypes, ', dim(unique(subset(fossils.post.gen, type == 1, select = taxa)))[1], ' are found after the extinction (', round((1-(dim(unique(subset(fossils.post.gen, type == 1, select = taxa)))[1]/dim(unique(subset(fossils.pre.gen, type == 1, select = taxa)))[1]))*100, 3), '% observed extinction)\n', sep = ''))


cat(paste('Of the ', dim(unique(subset(fossils.pre.gen, type == 2, select = taxa)))[1], ' linear morphotypes, ', dim(unique(subset(fossils.post.gen, type == 2, select = taxa)))[1], ' are found after the extinction (', round((1-(dim(unique(subset(fossils.post.gen, type == 2, select = taxa)))[1]/dim(unique(subset(fossils.pre.gen, type == 2, select = taxa)))[1]))*100, 3), '% observed extinction)\n', sep = ''))


cat('\n\n') #section break


##### Including modern denticles: Morphotype Extinction calculations #####
cat('##### Including modern denticles: Morphotype Extinction Calculations #####\n\n')

modern.fossil.matches.types <- type.cat.lookup(taxaList = as.character(modern.fossil.matches), typeList = denticle_geomlin)
table.modern.fossil.matches.types <- table(factor(modern.fossil.matches.types, levels = 1:5))

# Which ones are in modern *and* fossil? 
cat(paste('There are ', length(modern.fossil.matches), ' fossil denticle morphotypes that have a modern analog:\n', sep = ''))
cat(paste(sort(modern.fossil.matches), collapse = '\n'))

cat('\n\n')

cat(paste('Of the, ', length(unique(fossils.pre$taxa)), ' morphotypes present prior to 19 Ma,  only ', length(modern.fossil.matches), ' are present in the fossil record and/or our modern database after the extinction event, a ', round((1-(length(modern.fossil.matches)/length(unique(fossils.pre$taxa))))*100, 3), '% extinction in morphotype diversity.\n'))

cat(paste('Of the ', dim(unique(subset(fossils.pre, type == 1, select = taxa)))[1], ' geometric morphotypes, ', table.modern.fossil.matches.types[which(names(table.modern.fossil.matches.types) == 1)], ' are occur after the extinction, either in the fossils or by a modern analog (', round((1-(table.modern.fossil.matches.types[which(names(table.modern.fossil.matches.types) == 1)]/dim(unique(subset(fossils.pre, type == 1, select = taxa)))[1]))*100, 3), '% observed extinction)\n', sep = ''))

cat(paste('Of the ', dim(unique(subset(fossils.pre, type == 2, select = taxa)))[1], ' linear morphotypes, ', table.modern.fossil.matches.types[which(names(table.modern.fossil.matches.types) == 2)], ' are found after the extinction, either in the fossils or by a modern analog (', round((1-(table.modern.fossil.matches.types[which(names(table.modern.fossil.matches.types) == 2)]/dim(unique(subset(fossils.pre, type == 2, select = taxa)))[1]))*100, 3), '% observed extinction)\n', sep = ''))


cat('\n\n') #section break

##### Number of investigated Neogene and Paleogene Samples wih denticles #####
cat('##### Number of investigated Neogene and Paleogene Samples wih denticles #####\n\n')

## 596 stats
cat(paste('Of the ', dim(subset(accum.596, accum.596$age <= 19.1))[1], ' samples at DSDP Site 596 post-extinction (<19 Ma), only ', dim(subset(accum.596, accum.596$age <= 19.1 & accum.596$dent >= 1))[1], ' have denticles present.\n', sep = ''))
cat(paste('Compared to ', dim(subset(accum.596, accum.596$age >= 19.1))[1], ' samples at DSDP Site 596 pre-extinction (19-41 Ma), ', dim(subset(accum.596, accum.596$age >= 19.1 & accum.596$dent >= 1))[1], ' have denticles present.\n', sep = ''))

cat(paste('At DSDP Site 596 post-extinction, there is an average of ', round(mean(subset(accum.596, accum.596$age <= 19.1)$dent), 2), ' denticles per sample and ', round(mean(subset(accum.596, accum.596$age <= 19.1)$teeth),2), ' teeth per sample.\n', sep = ''))
cat(paste('Before the extinction, there is an average of ', round(mean(subset(accum.596, accum.596$age >= 19.1)$dent),2), ' denticles per sample and ', round(mean(subset(accum.596, accum.596$age >= 19.1)$teeth),2), ' teeth per sample.\n', sep = ''))

cat('\n')
## 886 stats
cat(paste('Of the ', dim(subset(accum.886, accum.886$age <= 19.1))[1], ' samples at ODP Site 886 post-extinction (<19 Ma), only ', dim(subset(accum.886, accum.886$age <= 19.1 & accum.886$dent >= 1))[1], ' have denticles present.\n', sep = ''))
cat(paste('Compared to ', dim(subset(accum.886, accum.886$age >= 19.1))[1], ' samples at ODP Site 886 pre-extinction (22-35 Ma), ', dim(subset(accum.886, accum.886$age >= 19.1 & accum.886$dent >= 1))[1], ' have denticles present.\n', sep = ''))

cat(paste('At ODP Site 886 post-extinction, there is an average of ', round(mean(subset(accum.886, accum.886$age <= 19.1)$dent), 2), ' denticles per sample and ', round(mean(subset(accum.886, accum.886$age <= 19.1)$teeth),2), ' teeth per sample.\n', sep = ''))
cat(paste('Before the extinction, there is an average of ', round(mean(subset(accum.886, accum.886$age >= 19.1)$dent),2), ' denticles per sample and ', round(mean(subset(accum.886, accum.886$age >= 19.1)$teeth),2), ' teeth per sample.\n', sep = ''))

# combine sites
cat('\n') 
cat(paste('In total, there were ', dim(subset(accum.886, accum.886$age <= 19.1))[1] + dim(subset(accum.596, accum.596$age <= 19.1))[1], ' samples investigated from after the extinction event (<19 Ma). Of these only ', dim(subset(accum.886, accum.886$age <= 19.1 & accum.886$dent >= 1))[1] + dim(subset(accum.596, accum.596$age <= 19.1 & accum.596$dent >= 1))[1], ' have any denticles at all (', round((dim(subset(accum.886, accum.886$age <= 19.1 & accum.886$dent >= 1))[1] + dim(subset(accum.596, accum.596$age <= 19.1 & accum.596$dent >= 1))[1])/(dim(subset(accum.886, accum.886$age <= 19.1))[1] + dim(subset(accum.596, accum.596$age <= 19.1))[1])*100, 2) ,'%)\n', sep = ''))


cat('\n\n') #section break


######### Step 4: Close file #####

sink()

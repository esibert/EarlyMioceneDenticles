#################################################
#                                               #
#   in-house functions for denticles analysis   #
#        Updated 7/15/20                        #
#                                               #
#################################################

##### 0. Table of Contents #####
# 1. type.cat.lookup matches the denticle name to the category it is found in.
# 2. valid.names.fn for validating species names within RFishbase. 
# 3. lad.fad.fn for producing first/last occurrence datum dataframe for use in range extension calculatoins
# 4. rconf uses the Strauss & Sadler 1989 method to calculate confidence intervals on range charts
# 5. rangeExtensions uses methods described in Marshall 1995; Wang & Marshall 2004 to calculate range extensions on all morphotypes in a dataset simultaneously
# 6. binom.conf calculates the binomial probability ranges for the range chart dataset of interest, after Wang & Marshall 2004. 

##################################
#                                #
#     Functions used             #
#                                #
##################################
##### 1. type.cat.lookup takes master type list (dent_lingeom) and classifies each denticle #####
type.cat.lookup <- function(taxaList, typeList) {
   type.cat <- c()
   for(i in 1:length(taxaList)) {
      type.cat[i] <- typeList[match(taxaList[i], typeList[,1]),2]
   }
   return(type.cat)
}


##### 2. valid.names.fn uses rfishbase functions to validate names for later taxonomic classification #####

valid.names.fn <- function(species.list, flag = '..a..') {
   valid.names <- c()
   for(i in 1:length(species.list)) {
      
      # If there is a name result returned:
      if(length(validate_names(species.list[i])) != 0) {
         valid.names[i] <- validate_names(species.list[i])
      }
      
      # If there's not a name result returned, return original name with asterisk
      else {
         valid.names[i] <- paste(species.list[i], flag, sep = '')
      }
   }
   return(valid.names)
}


##### 3. lad.fad.fn takes a counts (occurrence) table with ages as rows and morphotypes as columns, and produces a FAD & LAD data frame for range extensions #####

lad.fad.fn <- function(counts.table, ages.vector) {
   
   if(missing(ages.vector)) {ages.vector <- as.numeric(rownames(counts.table))}
   
   df <- data.frame(taxon = colnames(counts.table), fads = NA, lads = NA)
   
   # Iterate across each species [could probably also do this with apply, but this is easier]
   for(i in 1:dim(counts.table)[2]) {
      occur <- ages.vector[which(counts.table[,i] != 0)] # What age values are non-zero? 
      df$fads[i] <- max(occur) #first occurrence = oldest
      df$lads[i] <- min(occur) #last occurrence = youngest
   }
   
   return(df)
   
}


##### 4. rconf (Strauss & Sadler 1989) #####
# rconf calculates the range extensions based on R (total range duration), H (number of horizons detected), and conf (probability value) based on Strauss & Sadler 1989
rconf <- function(R, H, conf) {
   rconf_calc <- R * ((1-conf)^ (-1/(H-1)) -1)
   rconf_calc[is.nan(rconf_calc)] <- 0
   return(rconf_calc)
}
# R is range length
# H is fossil horizons (not a percentage?)
# conf is probability/range extension value

##### 5. rangeExtensions (Marshall 1995; Wang & Marshall 2004) #####
# counts.table is counts table calculated in Setup.R
# conf is the range extension probability, which feeds to rconf calculation

rangeExtensions <- function(counts.table, conf) {
   ranges <- lad.fad.fn(counts.table)
   ranges$occurrences <- apply(counts.table, 2, function(x) sum(ifelse(x>0, 1, 0)))
   ranges$duration <- ranges$fads - ranges$lads
   ranges$extension <- rconf(R = ranges$duration, H = ranges$occurrences, conf = conf)
   ranges$new.lad <- ranges$lads - ranges$extension
   return(ranges)
}

##### 6. binom.conf (Marshall 1995; Wang & Marshall 2004)  #####
# bionm.conf calculates binomial probability values for the dataset of interest. These were written to match with the figures in Wang & Marshall 2004
binom.conf <- function(species, conf, conf.threshold = NULL, print.output = TRUE, print.error = TRUE) {
   species.conf <- ceiling(species*conf)
   if(print.error == TRUE) {
      if(species %% (1/conf) != 0) cat(paste('species not divisible by confidence interval, rounding up \n'))
   }
   
   start.val <- 1
   odd.correction <- ifelse((species %% 2) == 0, 0, 1) #I think I need this regardless
   probs.df <- data.frame(NULL)
   # Calculate probabilities
   for(i in start.val:(species.conf+1)) {
      #for(i in start.val:(species-species.conf)) {
      spmin <- species.conf-i + start.val
      spmin <- ifelse(spmin < 1, 1, spmin)
      spmax <- species.conf+i
      probs <- sum(dbinom(spmin:(spmax-1), species, conf))
      if(print.output == TRUE) {
         cat(paste('conf level of', probs, 'between species', spmin, 'and', spmax - odd.correction, '\n'))
      }
      probs.df <- rbind(probs.df, c(spmin, spmax - odd.correction, probs))
   }
   
   names(probs.df) <- c('spmin', 'spmax', 'probs')
   
   # Grab the row that designates which species are the designated ones
   if(is.null(conf.threshold) == FALSE) {
      prob.row <- min(which(probs.df$probs > conf.threshold))
      prob.species <- probs.df[prob.row,]
      prob.species <- c(prob.species)[1:2]
      prob.species <- unlist(prob.species)
      return(prob.species)
   }
}


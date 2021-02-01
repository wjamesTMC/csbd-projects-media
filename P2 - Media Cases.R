##############################################################################
#
# Media Analysis - CCC Vocabulary extractioN
# Bill James / jamesw@csps.com
#
# Files:  https://github.com/wjamesTMC/csbd-projects-media.git
#
##############################################################################

#-----------------------------------------------------------------------------
#
# Library setups
#
#-----------------------------------------------------------------------------

# Import libraries
library(tidyverse)
library(tidyr)
library(plyr)
library(dplyr)
library(caret)
library(ggplot2)
library(ggthemes)
library(extrafont)
library(scales)
library(reshape2)
library(stringi)
library(stringr)
library(expss)
library(grid)
library(gridExtra)
library(lattice)
library(janitor)
library(tokenizers)
library(googlesheets)

#--------------------------------------------------------------------
#
# File opens and cleanup
#
#--------------------------------------------------------------------

#
# Open up the media word list
#
vocab_filename <- "D2 - CCC_media_word_list.csv"
mwl_dat        <- read.csv(vocab_filename, stringsAsFactors = FALSE)

#
# Open up the product list
# 
prod_filename <- "D3 - CCC_prod_list.csv"
prods         <- read.csv(prod_filename, stringsAsFactors = FALSE)
prods         <- prods %>% filter(prods$Include == "X") %>% mutate("Count" = 0)
prod_groups   <- unique(prods$Group)

# Determine the number of columns we will need for products (one per group)
num_prod_cols <- length(prod_groups)

#
# Open up the PNN word list
#
vocab_filename <- "D4 - PNN_Vocabulary.csv"
comms          <- read.csv(vocab_filename, stringsAsFactors = FALSE)

# Split the PNNs into the positive, negative, and neutral groups
pos_vocab      <- comms %>% filter(Tone == "Positive") 
neg_vocab      <- comms %>% filter(Tone == "Negative")
neu_vocab      <- comms %>% filter(Tone == "Neutral")

# Open the set of cases and read in; just keep text as plain text
inp_dat <- read.csv("D5 - CCC_results.csv", stringsAsFactors = FALSE)

wkg_dat <- inp_dat

# Add columns to collect counts and name them according to the product list
dffc  <- data.frame(matrix(NA, nrow = nrow(wkg_dat), ncol = num_prod_cols))
for(i in 1:num_prod_cols) {
     names(dffc)[i] <- prod_groups[i]
}

# Add columns to collect totals as well as P-N-N counts
dfft <- data.frame(matrix(NA, nrow = nrow(wkg_dat), ncol = 5))
names(dfft)[1] <- "Total"
names(dfft)[2] <- "Pos"  # Number of positive vocabulary words
names(dfft)[3] <- "Neg"  # Number of negative
names(dfft)[4] <- "Neu"  # Number of neutral
names(dfft)[5] <- "MWC"  # Media word count

# Add these data frames as additional columns to the data file
pen_dat <- cbind(wkg_dat, dffc)
pro_dat <- cbind(pen_dat, dfft)
pro_dat <- pro_dat %>% select(!Case.Number) 
pro_dat <- pro_dat %>% select(!Subject)
pro_dat <- pro_dat %>% select(!Description) 
pro_dat <- pro_dat %>% select(!Opened.Date) 
pro_dat <- pro_dat %>% select(!Type)
pro_dat[is.na(pro_dat)] = 0

#
# Go throgh the cases. Check for an incidence of each product name and add up
#

grp_count = 0
for(i in 1:nrow(pro_dat)) {                                                  # Each case
     for(k in 1:num_prod_cols) {                                             # Each prod group
          grp_name  <- prod_groups[k]
          grp_set   <- prods %>% filter(prods$Group == grp_name)
          for(j in 1:nrow(grp_set)) {                                        # Each product
               if(str_detect(wkg_dat$Description[i], grp_set$Product[j]) |   # Test for match
                  str_detect(wkg_dat$Subject[i],     grp_set$Product[j])) {  # Test for match
                    grp_count <- grp_count + 1
               }
          }
          pro_dat[i,k+1] <- grp_count
          grp_count = 0
     }
}

# Write the totals for product counts into 4th column from the end [ ,-4]
for(i in 1:nrow(pro_dat)) { 
     pro_dat[i, (ncol(pro_dat) - 4)] <- sum(pro_dat[i,2:(ncol(pro_dat) - 5)])
}

#
# Go through the cases again and log positive word counts
#
for(i in 1:nrow(pro_dat)) {                                             # Each row
     for(j in 1:nrow(pos_vocab)) {                                      # Each word
          if(str_detect(wkg_dat$Description[i], pos_vocab$Term[j]) |
             str_detect(wkg_dat$Subject[i],     pos_vocab$Term[j])) {   # Test for match
               pro_dat[i, (ncol(pro_dat) - 3)] <- 
                    pro_dat[i, (ncol(pro_dat) - 3)] + 1                 # Total
          }
          
     }
}

#
# Go through the cases again and log negative word counts
#
for(i in 1:nrow(pro_dat)) {                                             # Each row
     for(j in 1:nrow(neg_vocab)) {                                      # Each word
          if(str_detect(wkg_dat$Description[i], neg_vocab$Term[j]) |
             str_detect(wkg_dat$Subject[i],     neg_vocab$Term[j])) {   # Test for match
               pro_dat[i, (ncol(pro_dat) - 2)] <- 
                    pro_dat[i, (ncol(pro_dat) - 2)] + 1                 # Total
          }
          
     }
}

#
# Go through the cases again and log neutral word counts
#
for(i in 1:nrow(pro_dat)) {                                             # Each row
     for(j in 1:nrow(neu_vocab)) {                                      # Each word
          if(str_detect(wkg_dat$Description[i], neu_vocab$Term[j]) |
             str_detect(wkg_dat$Subject[i],     neu_vocab$Term[j])) {   # Test for match
               pro_dat[i, (ncol(pro_dat) - 1)] <- 
                    pro_dat[i, (ncol(pro_dat) - 1)] + 1                 # Total
          }
          
     }
}

#
# Go through the cases a final time and log the media words count
#
for(i in 1:nrow(pro_dat)) {                                                 # Each row
     for(j in 1:nrow(mwl_dat)) {                                            # Each word
          if(str_detect(wkg_dat$Description[i], mwl_dat$Media_Word[j]) |
             str_detect(wkg_dat$Subject[i],     mwl_dat$Media_Word[j])) {   # Test for match
               pro_dat[i, (ncol(pro_dat))] <- 
                    pro_dat[i, (ncol(pro_dat))] + 1                         # Total
          }
          
     }
}

#
# Create final table by joining the cases with the word counts
#
fin_dat <- left_join(wkg_dat, pro_dat, by = "seq_id")
fin_dat <- fin_dat %>% mutate("Notes" = " ")

# Write out the overall data file
write.csv(fin_dat, "R1 - CCC_MWL_Cases.csv", row.names= FALSE)

#
# Develop cutoffs to filter out cases with GT average counts
#
ave_mwc <- round(mean(fin_dat$MWC), digits = 0) + 1   # GT average media words
ave_pos <- round(mean(fin_dat$Pos), digits = 0) + 1   # GT average positive words
ave_neg <- round(mean(fin_dat$Neg), digits = 0) + 1   # GT average negative words

#
# Apply the filters to get the cases where things are GT averages
#
mwc_dat <- fin_dat %>% filter(MWC >  ave_mwc)   # More than average media words
pos_dat <- fin_dat %>% filter(Pos >  ave_pos)   # Within that, GT ave positives
neg_dat <- fin_dat %>% filter(Neg >= ave_neg)   # Cases with > 1 negative word
prod_ct <- fin_dat %>% filter(Total > 1)        # Instances of 2 or more product references

mwc_dat <- arrange(mwc_dat, desc(mwc_dat$MWC))
pos_dat <- arrange(pos_dat, desc(pos_dat$Pos))
neg_dat <- arrange(neg_dat, desc(neg_dat$Neg))
prod_ct <- arrange(prod_ct, desc(prod_ct$Total))

# Write out the files for the record
write.csv(mwc_dat, "R2 - CCC_HMW_Cases.csv", row.names = FALSE)
write.csv(pos_dat, "R2 - CCC_HPW_Cases.csv", row.names = FALSE)
write.csv(neg_dat, "R2 - CCC_HNW_Cases.csv", row.names = FALSE)
write.csv(prod_ct, "R2 - CCC_PCT_Cases.csv", row.names = FALSE)

#
# Do joins, sort, and write out the files
#

# First, strip off everything except the seq_id
pos_dat <- pos_dat %>% select(seq_id)
neg_dat <- neg_dat %>% select(seq_id)
prod_ct <- prod_ct %>% select(seq_id)

# Next, do the join 
mpw <- semi_join(mwc_dat, pos_dat, by = "seq_id", type = "left", match = "first")
mnw <- semi_join(mwc_dat, neg_dat, by = "seq_id", type = "left", match = "first")
pcm <- semi_join(mwc_dat, prod_ct, by = "seq_id", type = "left", match = "first")

# Sort the results by media word, then by pos or neg or pord count
mpw <- arrange(mpw, desc(MWC), desc(Pos))
mnw <- arrange(mnw, desc(MWC), desc(Neg))
pcm <- arrange(pcm, desc(MWC), desc(Total))

# Write out the files
write.csv(mpw, "R3 - CCC_PosMed_Cases.csv", row.names= FALSE)
write.csv(mnw, "R3 - CCC_NegMed_Cases.csv", row.names= FALSE)
write.csv(pcm, "R3 - CCC_ProdCt-Med_Cases.csv", row.names = FALSE)

#
# Take random sample of vocab_df to hunt for additional terms to remove cases
#
case_index  <- createDataPartition(y = mwc_dat$MWC, times = 1, p = 0.25, list = FALSE)
rev_dat <- mwc_dat[case_index, ]
write.csv(rev_dat,"R9 - CCC_check.csv", row.names = FALSE)

################################################################################
# End
################################################################################


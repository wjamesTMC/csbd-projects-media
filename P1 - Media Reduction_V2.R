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
# Open and process the CCC File
#

# Open the set of cases and read in; just keep text as plain text
inp_dat <- read.csv("D1 - Cases.csv", stringsAsFactors = FALSE)
# inp_dat <- read.csv("Cases with Emails.csv", stringsAsFactors = FALSE)
# inp_dat <- read.csv("Cases with Knowledge Articles.csv", stringsAsFactors = FALSE)

# Remove unneeded fields
wkg_dat <- inp_dat %>% 
     select(Case.Number, Opened.Date, Subject, Description, Type) %>% 
     mutate(seq_id = 1, ind = "x")

total_cases <- nrow(wkg_dat)

# Remove cases with Type as shown
wkg_dat <- wkg_dat %>% filter(Type != "Junk")
wkg_dat <- wkg_dat %>% filter(Type != "Duplicate")
wkg_dat <- wkg_dat %>% filter(Type != "Out of office")
wkg_dat <- wkg_dat %>% filter(Type != "No Answer")
wkg_dat <- wkg_dat %>% filter(Type != "Misdirected")
wkg_dat <- wkg_dat %>% filter(Type != "Broken Link")
wkg_dat <- wkg_dat %>% filter(Type != "Prepaid Sub Card")
wkg_dat <- wkg_dat %>% filter(Type != "Returns")
wkg_dat <- wkg_dat %>% filter(Type != "House Order")
wkg_dat <- wkg_dat %>% filter(Type != "Replacement")
wkg_dat <- wkg_dat %>% filter(Type != "Policy Q/Manual Interpretation")
wkg_dat <- wkg_dat %>% filter(Type != "Membership Number")
wkg_dat <- wkg_dat %>% filter(Type != "Account Modification")
wkg_dat <- wkg_dat %>% filter(Type != "Blast")
wkg_dat <- wkg_dat %>% filter(Type != "Increase")
wkg_dat <- wkg_dat %>% filter(Type != "Journal Directory")
wkg_dat <- wkg_dat %>% filter(Type != "Technical/Bug/System Issue")
wkg_dat <- wkg_dat %>% filter(Type != "Technical/Device")
wkg_dat <- wkg_dat %>% filter(Type != "Damaged")
wkg_dat <- wkg_dat %>% filter(Type != "Advertising")
wkg_dat <- wkg_dat %>% filter(Type != "Refund")
wkg_dat <- wkg_dat %>% filter(Description != "")

ol_vocab <- nrow(wkg_dat)

#
# Open up the media word list
#
vocab_filename <- "D2 - CCC_media_word_list.csv"
mwl_dat        <- read.csv(vocab_filename, stringsAsFactors = FALSE)

#
# Go through the cases and log where there are media words 
#
for(i in 1:nrow(wkg_dat)) {                                          
     for(j in 1:nrow(mwl_dat)) {                                     
          if(str_detect(wkg_dat$Description[i], mwl_dat$Media_Word[j]) |
             str_detect(wkg_dat$Subject[i],     mwl_dat$Media_Word[j])) {   
               wkg_dat[i, 6]   <- i
               wkg_dat[i, 7]   <- "M"                  
          }
     }
}

# Take only those cases with media words
wkg_dat <- wkg_dat %>% filter(ind == "M")

ml_vocab <- nrow(wkg_dat)

# 
# Detect cases we want to throw out - open junk word list
#

junk_filename  <- "DX - CCC_junk_list.csv"
jwl_dat        <- read.csv(junk_filename, stringsAsFactors = FALSE)

#
# Go through the cases and log where there are junk words 
#
for(i in 1:nrow(wkg_dat)) {                                          
     for(j in 1:98) {                                     
          if(str_detect(wkg_dat$Description[i], jwl_dat$Junk_Word[j]) |
             str_detect(wkg_dat$Subject[i],     jwl_dat$Junk_Word[j])) {   
               wkg_dat[i, 7] <- "X"                  
          }
     }
}

# Remove rows that have been marked with an X
wkg_dat <- wkg_dat %>% filter(ind != "X")

fl_vocab <- nrow(wkg_dat)

# Take a look at our stats
cat("Results Summary")
cat("---------------------------")
cat("Original number of cases  :", total_cases)
cat("After category reductions :", ol_vocab)
cat("Cases with media words    :", ml_vocab)
cat("Cases removed by searching:", (ml_vocab - fl_vocab))
cat("Number of cases to examine:", fl_vocab)
cat("Reduction percentage      :", 1 - (fl_vocab / ol_vocab))

# Strip out indicator / add a field for notes
wkg_dat <- wkg_dat %>% select(!ind)

# Write out a file to examine it
write.csv(wkg_dat,"D5 - CCC_results.csv", row.names = FALSE)

################################################################################
# End
################################################################################


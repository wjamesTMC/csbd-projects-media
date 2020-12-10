##############################################################################
#
# Media Analysis - CCC Vocabulary extractioN
# Bill James / jamesw@csps.com
#
# Files:  https://github.com/wjamesTMC/tsg-projects-media.git
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
# CCC File
#

inp_dat <- read.csv("Cases.csv", stringsAsFactors = FALSE)
str(inp_data)

# Keep selected fields
#

wkg_dat <- inp_dat %>% select(Parent.Case.ID, Case.Origin, Description, Subject, Subject...Description, Case.Answer)
str(wkg_dat)

unq_case_origins <- unique(wkg_dat$Case.Origin) # 56 unique case origins
unq_subjects <- unique(wkg_dat$Subject)         # 59084 unique subjects

# 
# Now we know that we have to mine words from the subject and description fields
# We need to build a dataframe that has the following fields:
#     word
#     Number of occurrences in the subject fields
#     number of occureences in the description fields
#     total number of occurrences
#     media word Y / N
#     occurrence ranking
#     media word occurrence ranking
#

#
# Detect instances where a case has a media term
#
detect_media <- c("audio",
                  "digital",
                  "CD",
                  "telephone",
                  "iTune")

# Check each of the words against the Description fields
Pattern = paste(detect_media, collapse = "|")
result <- grepl(Pattern, wkg_dat$Description)

# Count the number of TRUE results
ntrues <- table(result)["TRUE"]

# Build dataframe
vocab_df <- data.frame("wkg_dat_id" = ntrues,
                       "seq_id"     = ntrues,
                       "desc"       = ntrues,
                       "subj"       = ntrues)

# If a word does show up, mark it with a "1" and save off the result
for(i in 1:nrow(wkg_dat)) {
     if(result[i] == TRUE) {
          vocab_df[i, 1]   <- wkg_dat$Parent.Case.ID[i]
          vocab_df[i, 2]   <- i
          vocab_df[i, 3]   <- wkg_dat$Description[i]
          vocab_df[i, 4]   <- wkg_dat$Subject[i]
     }
}
# Loop through and create the list of word occurrences

#
# Now we need to select the words that are related to media and go back 
# through the list and tag them, and then run the totals for occurrences
# and media word occurrences
#

# <code>

# 
# Now we create a list of the case IDs that have media related words in them
# and narrow down the list of cases to those that are truly media related and
# not just containing incidental media words
#

# <code>

#
# Depending on what we find at this point, we can somehow categorize the cases 
# into a number of subject catgories

# <code>


# Remove duplicates and blank descriptions, if any
e_dat <- e_dat %>% filter(C_Type != "Duplicate", C_Type != "")
e_dat <- e_dat %>% filter(Desc != "<None>")
e_dat <- as.data.frame(e_dat)

# Pull in Trillion Word list
base_url <- "https://programminghistorian.org/assets/basic-text-processing-in-r"
url <- sprintf("%s/sotu_text/236.txt", base_url)
wf <- read_csv(sprintf("%s/%s", base_url, "word_frequency.csv"))

#--------------------------------------------------------------------
#
# Build Sets of Vocabularly by Category
#
#--------------------------------------------------------------------

# Establish the category list
cat_list <- unique(e_dat$C_Type)

# Outer loop to build a vocab list for each category
for(x in 1:length(cat_list)) {
     
     # Establish the category for the inner loop
     cat_name <- cat_list[x]
     
     # Pull all the rows for that category from the data file
     category <- e_dat %>% filter(C_Type == cat_name)
     cat_len <- nrow(category)
     
     # Create a dataframe to hold the results
     cat_df <- data.frame(cat_word = 1:cat_len, cat_name = cat_name, cat_count = 1:cat_len)
     
     # Loop through the category and build a dataframe of the results
     new_df_line <- data.frame(cat_word = 1, cat_name = cat_name, cat_count = 1)
     
     # Now the inner loop to generate the results
     for(i in 1:cat_len) {
          # Grab the email text
          text  <- category[i,3]
          
          # Break the text down into words
          words <- tokenize_words(text)
          
          # Build a table of the words and frequency and turn into a dataframe
          tab <- table(words)
          tab <- data_frame(word = names(tab), count = as.numeric(tab))
          
          # Sort the dataframe by largest number of occurrences first
          tab <- arrange(tab, desc(count))
          
          # Compare with master list and select out only significant words
          tab <- inner_join(tab, wf)
          wds <- filter(tab, frequency < 0.001)
          
          # For the given category, build a dataframe of the results
          if(nrow(wds) > 0) {
               
               # This loop builds a row and adds it the overall dataframe
               for(j in 1:nrow(wds)) {
                    new_df_line[1,1] <- wds[j,1]
                    new_df_line[1,3] <- wds[j,2]
                    cat_df <- rbind(cat_df, new_df_line)
               }
          }
     }
     
     # Now remove unused rows in the dataframe and resort
     cat_df <- cat_df %>% filter(cat_word != cat_count)
     cat_df <- arrange(cat_df, desc(cat_count))
     
     # Remove duplicates (some emails can appear twice)
     cat_df <- unique(cat_df)
     
     # Remove infrequent words - Google Sheets limits to 1000 lines
     if(nrow(cat_df) > 1000) {
          cat_df <- cat_df %>% filter(cat_count > 2)
     }
     
     # 
     # Create the category Google Sheet and Write out the results
     #
     
     # Create file name
     output_file_name <- paste(x, "_", cat_name)
     str_replace_all(output_file_name, space(), "")
     
     # Create output file and write out the contents of the dataframe
     output_file <- gs_new(output_file_name)
     gs_edit_cells(output_file, ws = 1, input = cat_df, anchor = "A1", byrow = FALSE,
                   col_names = NULL, trim = FALSE, verbose = TRUE)
}

#--------------------------------------------------------------------
#
# Identify vocabulary unique to each category
#
#--------------------------------------------------------------------

inp_file_name_1 <- paste("1_", cat_list[1])
inp_file_name_1 <- gsub(" ", "", inp_file_name_1, fixed = TRUE)

inp_file_1      <- gs_title(inp_file_name_1)
inp_data_1      <- gs_read(inp_file_1, stringsAsFactors = FALSE)

inp_file_name_2 <- paste("2_", cat_list[2])
inp_file_name_2 <- gsub(" ", "", inp_file_name_2, fixed = TRUE)

inp_file_2      <- gs_title(inp_file_name_2)
inp_data_2      <- gs_read(inp_file_2, stringsAsFactors = FALSE)

unq_words <- anti_join(inp_data_1, inp_data_2, by = "cat_word")
print(unq_words)

#
# Vocabulary file - the list of terms we are using to predict classifications
#

# Open vocabulary file 
v_fln <- gs_title("CCC Email Vocabulary")
v_dat <- gs_read(v_fln, stringsAsFactors = FALSE)

#--------------------------------------------------------------------
#
# Testing for various classifications
#
#--------------------------------------------------------------------

#
# Classification "Junk"
#

# Determine what the CCC staff classified as Junk
match_term  <- "Junk or No Answer Required"
act_result  <- e_dat %>% filter(e_dat$C_Type == match_term) 
act_num     <- nrow(act_result)

# Establish the Junk vocablulary
v_dat  <- as.data.frame(v_dat %>% filter(Junk == "X"))

# Predict match term based on input vocabulary
for(i in 1:nrow(e_dat)) {
     if(is.na(e_dat$Desc[i]) == TRUE) {
          e_dat$Desc[i] <- "None"
     }
        
     for(j in 1:nrow(v_dat)) {
          if(str_detect(e_dat$Desc[i], v_dat$Word[j]) == TRUE) {
               e_dat$N_Type[i]  <- match_term
               e_dat$M_Count[i] <- e_dat$M_Count[i] + 1
          }
     }
}

# Write out the results
e_ofl <- e_dat %>% filter(C_Type == match_term | N_Type == match_term)

# Write out the results (res_df)
output_file <- gs_title("CCC Junk Match Results")
gs_edit_cells(output_file, ws = 1, input = e_ofl, anchor = "A2", byrow = FALSE,
              col_names = NULL, trim = FALSE, verbose = TRUE)




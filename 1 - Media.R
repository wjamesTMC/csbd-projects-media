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
# Open and process the CCC File
#

# Open the set of cases and read iut in
inp_dat <- read.csv("Cases.csv", stringsAsFactors = FALSE)

# Keep just the fields we need
wkg_dat <- inp_dat %>% select(Parent.Case.ID, Case.Origin, Description, Subject)

# Reduce the file size to speed up work (randomly select 25% of the cases)
case_index  <- createDataPartition(y = wkg_dat$Case.Origin, times = 1, p = 0.25, list = FALSE)
wkg_dat <- wkg_dat[case_index, ]

#
# Detect instances where a case has a vocabulary term we want
#

# Set up a list of words to search for in the cases
detect_media <- c("audio",
                  "digital",
                  "CD",
                  "elephone",
                  "iTunes",
                  "media",
                  "Spotify",
                  "laylist",
                  "Alexa",
                  "mart speaker",
                  "available in",
                  "format",
                  "share",
                  "idea")

# Check each of the words against the Description field
pattern_m = paste(detect_media, collapse = "|")
result_md <- grepl(pattern_m, wkg_dat$Description)

# Now check the same list of words against the Subject field
result_ms <- grepl(pattern_m, wkg_dat$Subject)

# Count the number of TRUE results
n_md_trues <- table(result_md)["TRUE"]
n_ms_trues <- table(result_ms)["TRUE"]

# find the longer of the two results so we can build a dataframe the right size
if (n_md_trues > n_ms_trues) {
     len_trues <- n_md_trues
} else {
     len_trues <- n_ms_trues
}

# Build dataframe
vocab_df <- data.frame("wkg_dat_id"    = len_trues,
                       "seq_id"        = len_trues,
                       "desc"          = len_trues,
                       "subj"          = len_trues,
                       "media_case"    = len_trues)

# If a word does show up, save off the result and mark it
for(i in 1:nrow(wkg_dat)) {
     if(result_md[i] == TRUE) {
          vocab_df[i, 1]   <- wkg_dat$Parent.Case.ID[i]
          vocab_df[i, 2]   <- i
          vocab_df[i, 3]   <- wkg_dat$Description[i]
          vocab_df[i, 4]   <- wkg_dat$Subject[i]
          vocab_df[i, 5]   <- "M"
     } 
}

# Repeat for the subject field
for(i in 1:nrow(wkg_dat)) {
     if(result_ms[i] == TRUE) {
          vocab_df[i, 1]   <- wkg_dat$Parent.Case.ID[i]
          vocab_df[i, 2]   <- i
          vocab_df[i, 3]   <- wkg_dat$Description[i]
          vocab_df[i, 4]   <- wkg_dat$Subject[i]
          vocab_df[i, 5]   <- "M"
     } 
}

#
# Detect cases we want to throw out
#

# Create the list of words and phrases like we did before
detect_custs <- c("Thank you for your support of Longyear!",
                  "Hudson in the News:",
                  "Important Information Regarding Your Account",
                  "Evolution of Yahoo Groups",
                  "or more information contact",
                  "CNBC",
                  "Text or Call",
                  "text or call",
                  "oronavirus",
                  "Covid",
                  "COVID",
                  "contagion",
                  "isinfectant",
                  "2020 Season",
                  "TELEMUNDO",
                  "telemundo",
                  "Royal United Services Institute",
                  "thealtmanbrothers.com",
                  "Auto Reply",
                  "Zogby Report",
                  "For Immediate Release",
                  "Crossroads Career",
                  "pply now",
                  "South Tulsa Charmer",
                  "yours in Christ",
                  "Yours in Christ",
                  "Delivery Status Notification (Failure)",
                  "RN Public Relations Group",
                  "Author Solutions Inc",
                  "Green Party",
                  "Republican", 
                  "Democratic",
                  "pdate credit card",
                  "Automatic Renewal Reminder",
                  "graduation gift",
                  "Release Completed:",
                  "Press release",
                  "news release",
                  "For immediate release",
                  "management@rmfriedland.com",
                  "robly.com",
                  "catalystresourcegroup.com",
                  "pdated member information",
                  "ditorial comments and corrections",
                  "Department of Mental Health",
                  ".gov",
                  "utexas.edu",
                  "MassDOT",
                  "Associated Press",
                  "CBS Evening News",
                  "Reuters",
                  "PR Newswire",
                  "UPDATE:",
                  "Follow us on Facebook",
                  "Log In",
                  "log in",
                  "log out",
                  "Log Out",
                  "purchased in advance",
                  "iew this email in your browser",
                  "urvey request",
                  "now open",
                  ".co.uk",
                  "FOR IMMEDIATE PUBLIC RELEASE",
                  "ABCD.agency",
                  "404: EMAIL not unique",
                  "Warning",
                  "warning",
                  "I am away",
                  "Better Business Bureau",
                  "BBB",
                  "Nova Scotia Defence and Security",
                  "latticepublishing.com",
                  "did not include any attachments",
                  "Christian Television",
                  ".com.tw",
                  ".ru",
                  "armenianchurch",
                  "======",
                  "Halloween",
                  "Press call",
                  "Automatic Renewal Reminder",
                  "cspsnews@csps.com",
                  "Editorial comments and corrections: Headlines",
                  "POLITICO",
                  "momentum for growth",
                  "Clock Collector's Paradise",
                  "Microsoft 2020 Training",
                  "Kozhikode's",
                  "BREAKING:",
                  "Mental Health",
                  "Blackwoods",
                  "Faith-Based",
                  "estatesalesplus.com",
                  "Strategic Relations",
                  "rmfriedland.com",
                  "Give the Gift of Christmas",
                  "NEWS RELEASE",
                  "Services for Artists",
                  "PRWorkzone",
                  "Updating Credit details",
                  "Netflix",
                  "ounninaneang@gmail.com",
                  "Planned Parenthood",
                  "pdates from",
                  "Supreme Court",
                  "Ascot Media Group",
                  "@lung.org",
                  "email.kingdom.com",
                  "ievapor.com",
                  "Dear Beneficiary",
                  "your congregation",
                  "challengerwealth.com",
                  "SMITH PUBLICITY",
                  "Security Vulnerability Notification",
                  "NEWS STORY:",
                  ".itsliquid.com",
                  "million",
                  "auto-btob.com",
                  "Purchase Order",
                  ".cobracanine.com",
                  "passed on",
                  "To: Undisclosed",
                  "Annual Meeting",
                  "CMM Enewsletter",
                  "Working remotely",	
                  "Google Business",	
                  "Google business",
                  "Easter",
                  "Change of Address",	
                  "Application for Permission",	
                  "echo chamber",	
                  "Biden",	
                  "not unique",	
                  "nhancement of",	
                  "NEW STUDY",	
                  "new study",
                  "Phone Number Update",	
                  "onsters",	
                  "Show Guests",	
                  "show guests",
                  "Away from the office",	
                  "Automatic Response",	
                  "Ecumenical",	
                  "ecumenical",
                  "Account Access",	
                  "vangelizers",	
                  "Broken Arrow Beauty",	
                  "eBulletin",	
                  "irculation",	
                  "Pasadena",	
                  "ouble billing",	
                  "PC Culture",	
                  "culture",
                  "Order Confirmation",	
                  "order confirmation",
                  "Tax Time",	
                  "tax time",
                  "decline notice",	
                  "ebsite sales",	
                  "India's",
                  "Spectacular",	
                  "spectacular",
                  "Editas",
                  "medicine",
                  "Latest Print-At-Home",	
                  "Reformation",	
                  "reformation",
                  "Inpirational Show",	
                  "inspirational show",
                  "failed credit card",	
                  "US-Iran",	
                  "Ziopharm",	
                  "Seabras",	
                  "Desk & Chair",	
                  "desk and chair",
                  "Wine",
                  "wine",
                  "Congregation",	
                  "congregation",
                  "The National Funding",	
                  "Grant",
                  "THL Credit",	
                  "Writing A Book",	
                  "writing a book",
                  "Future Horizons",
                  "Nicotine",	
                  "nicotine",
                  "GRAMMY",	
                  "Grammy",
                  "vrier",	
                  "EMI",	
                  "Password",	
                  "Permissions",	
                  "permissions",
                  "Undeliverable",	
                  "Expieration Notice",	
                  "Que d'amour",	
                  "Staples",	
                  "SEO Marketing",	
                  "Charley Pride",	
                  "enter to win",	
                  "Enter to Win",
                  "blues",	
                  "besity",	
                  "Climate change",	
                  "climate change",
                  "Acknowlegement Receipt",	
                  "not unique",	
                  "manufacturer",	
                  "xporter",
                  "Mueller",
                  "St. Vincent",	
                  "E&P",	
                  "Out of Office",	
                  "Brown Brothers",	
                  "Credit Card",	
                  "credit card",
                  "Supercharge",	
                  "supercharge",
                  "USAGM",	
                  "View this email in your browser",
                  "COH_Primary",
                  "Fenway Alliance",	
                  "Secretary of State",	
                  "mnesia",
                  "vocuspr.com",
                  "the Heights",	
                  "rganizations",
                  "Out of the office",	
                  "Editors",	
                  "cancel",	
                  "Medical",	
                  "mediacal",
                  "New report",	
                  "new report",
                  "Review",
                  "review",
                  "PRESIDENTIAL",	
                  "Donor",	
                  "donor",
                  "guest",	
                  "your order",	
                  "SPAN",	
                  "ive at the",	
                  "new design",	
                  "extension program",	
                  "Bible verses",	
                  "Bible Verses",
                  "Last Chance",	
                  "last chance",
                  "Online shop",	
                  "Change of address",	
                  "membership",	
                  "pdate from",
                  "ookies")

# Check each of the words against the Subject field
pattern_c = paste(detect_custs, collapse = "|")
result_cs <- grepl(pattern_c, vocab_df$subj)

# Check each of the words against the description field
result_cd <- grepl(pattern_c, vocab_df$desc)

# Loop to narrow to eliminate non-customer originated cases (subjs)
for(i in 1:nrow(vocab_df)) {
     if(result_cs[i] == TRUE) {
          vocab_df[i, 5]   <- "X"
     } 
}

# Loop to narrow to eliminate non-customer originated cases (descs)
for(i in 1:nrow(vocab_df)) {
     if(result_cd[i] == TRUE) {
          vocab_df[i, 5]   <- "X"
     } 
}

# Remove instances where there are not complete data, if any
vocab_df <- vocab_df[complete.cases(vocab_df), ]

# Write out a file to examine it
write.csv(vocab_df,"CCC_results.csv", row.names = FALSE)

#######################################################################s

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




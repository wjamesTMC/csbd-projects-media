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

# Remove unneeded fields
wkg_dat <- inp_dat %>% select(Parent.Case.ID, Case.Origin, Description, Subject, Type)

# unique(wkg_dat$Type)

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


#
# Detect instances where a case has a vocabulary term we want
#

# mwl <- read.csv("D2 - media_word_list.csv", stringsAsFactors = FALSE)
# detect_media <- as.data.frame(mwl, stringsAsFactors = default.stringsAsFactors())
detect_media <- c("30 seconds",
                  "a copy of",
                  "accent",
                  "access",
                  "Alexa",
                  "article",
                  "audio",
                  "Audio Chat",
                  "audio-lesson",
                  "available in",
                  "blind",
                  "bookmark",
                  "braille",
                  "broadcasts",
                  "cassette",
                  "CD",
                  "cellular",
                  "content",
                  "continuous",
                  "DeafBlind",
                  "deaf-blind",
                  "deaf",
                  "dialect",
                  "diction",
                  "digital",
                  "Digital",
                  "download",
                  "downloaded",
                  "format",
                  "forward",
                  "hear",
                  "hearing",
                  "hymn",
                  "hymn number",
                  "idea",
                  "impaired",
                  "impairment",
                  "internet",
                  "in print",
                  "iTunes",
                  "laylist",
                  "listen",
                  "listened",
                  "listening",
                  "loop",
                  "media",
                  "mp3",
                  "music",
                  "navigation",
                  "number of the hymn",
                  "online",
                  "on line",
                  "phone",
                  "podcast",
                  "podcasts",
                  "print",
                  "printing",
                  "program",
                  "Radio",
                  "radio",
                  "read",
                  "readability",
                  "reader",
                  "reading",
                  "record",
                  "recording",
                  "replay",
                  "replayed",
                  "rewind",
                  "RSS",
                  "shortwave",
                  "siri",
                  "Siri",
                  "share",
                  "skip ahead",
                  "smart speaker",
                  "song",
                  "sound",
                  "speak",
                  "speaker",
                  "Spotify",
                  "streaming",
                  "talk",
                  "telephone",
                  "transcript",
                  "transcriptions",
                  "video",
                  "videoconference",
                  "videoconferencing",
                  "vision",
                  "voice",
                  "voices",
                  "watch",
                  "wifi",
                  "WiFi",
                  "widget",
                  "Zoom")

# Using the words on the list, create a pattern of OR'd terms to check
pattern_m <- paste(detect_media, collapse = "|")

# Check each of the words against the Description field and collect TRUE's
result_md <- grepl(pattern_m, wkg_dat$Description)

# Check the same list of words against the Subject field and collect TRUE's
result_ms <- grepl(pattern_m, wkg_dat$Subject)

# Build a dataframe to hold our results
vocab_df <- data.frame("wkg_dat_id"    = nrow(wkg_dat),
                       "seq_id"        = nrow(wkg_dat),
                       "desc"          = nrow(wkg_dat),
                       "subj"          = nrow(wkg_dat),
                       "media_case"    = nrow(wkg_dat),
                       "media_ind"     = nrow(wkg_dat))

# Loop through the data file. Save the result where we have a TRUE 
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

# Create the list of words & phrases that will signal it is NOT a customer case
detect_custs <- c("!!!!!",
                  "!!!!",
                  "!!!",
                  ".co.uk",
                  ".cobracanine.com",
                  ".com.tw",
                  ".gov",
                  ".itsliquid.com",
                  ".ru",
                  "@lung.org",
                  "2020 Season",
                  "404: EMAIL not unique",
                  "a problem with my",
                  "ABCD.agency",
                  "account 30890",               # Board Officee account
                  "Account Access",
                  "Acknowledgement Receipt",
                  "Address Change",
                  "address change",
                  "address has changed",
                  "annot access my account",
                  "announces",
                  "Annual Meeting",
                  "annual meeting",
                  "Application for Permission",
                  "armenianchurch",
                  "Ascot Media Group",
                  "Associated Press",
                  "Author Solutions Inc",
                  "Auto Reply",
                  "auto-btob.com",
                  "Automatic Renewal Reminder",
                  "Automatic Response",
                  "Away from the office",
                  "bank card",
                  "bank statement",
                  "bathroom",
                  "Bathroom",
                  "BBB",
                  "besity",
                  "Better Business Bureau",
                  "Bible verses",
                  "Bible Verses",
                  "Biden",
                  "BitBounce",
                  "Blackwoods",
                  "blues",
                  "BNP MEDIA",
                  "booklet",
                  "BREAKING:",
                  "Broken Arrow Beauty",
                  "Brown Brothers",
                  "cancel subscription",
                  "cancel",
                  "can't get into",
                  "catalystresourcegroup.com",
                  "CBS Evening News",
                  "CEO",
                  "challengerwealth.com",
                  "Change of Address",
                  "Change of address",
                  "Change of Email Address",
                  "change of email address",
                  "change of Email address",
                  "charge",
                  "charges",
                  "Charley Pride",
                  "Christian Television",
                  "Chritian Faith Publishing",
                  "Clerk",
                  "Climate change",
                  "climate change",
                  "Clock Collector's Paradise",
                  "CMM Enewsletter",
                  "CNBC",
                  "COH_Primary",
                  "Congregation",
                  "congregation",
                  "CONTACT:",
                  "contagion",
                  "Covid",
                  "COVID",
                  "crashed",
                  "crashing",
                  "Credit Card",
                  "credit card",
                  "credit card",
                  "Crossroads Career",
                  "cspsnews@csps.com",
                  "culture",
                  "Customer's email address:",
                  "Daily Offers",
                  "Day Translations, Inc.",
                  "Dear Beneficiary",
                  "deceased",
                  "decline notice",
                  "dejifisho20@gmail.com",
                  "delay in responding to",
                  "Delivery Status Notification (Failure)",
                  "Democratic",
                  "Department of Mental Health",
                  "Des Moines Register",
                  "Desk & Chair",
                  "desk and chair",
                  "Destiny Chaser",
                  "did not include any attachments",
                  "disease",
                  "ditorial comments and corrections",
                  "Donor",
                  "donor",
                  "E&P",
                  "Easter",
                  "ebsite sales",
                  "eBulletin",
                  "echo chamber",
                  "Ecumenical",
                  "ecumenical",
                  "edit folders",
                  "Editas",
                  "Editorial comments and corrections",
                  "Editorial comments and corrections: Headlines",
                  "Editors",
                  "EMAIL not unique",
                  "email.kingdom.com",
                  "EMI",
                  "enter to win",
                  "Enter to Win",
                  "Errors/Bugs",
                  "estatesalesplus.com",
                  "Events Branding Solutions",
                  "Evergreen Design Center",
                  "Evolution of Yahoo Groups",
                  "Expieration Notice",
                  "Expiration Notice:",
                  "extend my subscription",
                  "extension program",
                  "failed credit card",
                  "Faith-Based",
                  "Fenway Alliance",
                  "Follow us on Facebook",
                  "FOR IMMEDIATE PUBLIC RELEASE",
                  "For Immediate Release",
                  "For immediate release",
                  "forward all emails",
                  "fraud",
                  "Fraud",
                  "fraud",
                  "Future Horizons",
                  "gift card",
                  "Gift Guide",
                  "gift sub",
                  "gift subscription",
                  "Give the Gift of Christmas",
                  "God bless",
                  "goodreads.com",
                  "Google Business",
                  "Google business",
                  "graduation gift",
                  "GRAMMY",
                  "Grammy",
                  "GrandPad",
                  "Grant",
                  "Green Party",
                  "guest",
                  "Halloween",
                  "hange of address",
                  "hange of name",
                  "have not received",
                  "have not received",
                  "have to share this with you",
                  "haven't received",
                  "HowToWatch.com",
                  "Hudson in the News:",
                  "I am away",
                  "ievapor.com",
                  "iew this email in your browser",
                  "Important Information Regarding Your Account",
                  "India's",
                  "Inpirational Show",
                  "inspirational show",
                  "invalid address",
                  "invoice",
                  "irculation",
                  "isinfectant",
                  "ive at the",
                  "Kozhikode's",
                  "Last Chance",
                  "last chance",
                  "Latest Print-At-Home",
                  "latticepublishing.com",
                  "Log In",
                  "log in",
                  "log onto",
                  "log out",
                  "Log Out",
                  "Lyme",
                  "mailing address",
                  "management@rmfriedland.com",
                  "manufacturer",
                  "marketing",
                  "MassDOT",
                  "medical",
                  "Medical",
                  "medicine",
                  "member",
                  "Membership",
                  "membership",
                  "memberships",
                  "Mental Health",
                  "Message not delivered",
                  "Microsoft 2020 Training",
                  "million",
                  "Mindset",
                  "ministry",
                  "mnesia",
                  "momentum for growth",
                  "Mueller",
                  "my account",
                  "My Bible Lesson",
                  "My email address",
                  "my email address",
                  "myBibleLesson",
                  "MyBibleLesson",
                  "nable to receive",
                  "Netflix",
                  "New address",
                  "new address",
                  "new card info",
                  "new card information",
                  "new design",
                  "New report",
                  "new report",
                  "NEW STUDY",
                  "new study",
                  "news release",
                  "NEWS RELEASE",
                  "NEWS STORY:",
                  "nhancement of",
                  "Nicotine",
                  "nicotine",
                  "no longer getting",
                  "not unique",
                  "not unique",
                  "Nova Scotia Defence and Security",
                  "now open",
                  "obituary",
                  "Office of the Treasurer",
                  "Official Release",
                  "online registration",
                  "Online Shop Feedback",
                  "Online shop",
                  "onsters",
                  "ookies",
                  "or more information contact",
                  "Order Confirmation",
                  "order confirmation",
                  "oronavirus",
                  "ouble billing",
                  "ounninaneang@gmail.com",
                  "Out of Office",
                  "Out of the office",
                  "out of the office",
                  "Pasadena",
                  "passed on",
                  "password",
                  "Password",
                  "payment change",
                  "payment",
                  "payments our latest offers",
                  "PC Culture",
                  "pdate credit card",
                  "pdate from",
                  "pdated member information",
                  "pdates from",
                  "PER CAPITA TAX",
                  "per captia tax",
                  "permission",
                  "Permissions",
                  "permissions",
                  "personal information",
                  "phone number change",
                  "Phone Number Change",
                  "Phone Number change",
                  "Phone Number Update",
                  "Planned Parenthood",
                  "please cancel",
                  "please respond to customer",
                  "POLITICO",
                  "pply now",
                  "poster",
                  "PR Newswire",
                  "PRESIDENTIAL",
                  "Press call",
                  "Press release",
                  "PRWorkzone",
                  "publicityforgood.com",
                  "Purchase Order",
                  "purchased in advance",
                  "Que d'amour",
                  "readerscomments@csmonitor.com",
                  "Reading Room",
                  "received your correspondence",
                  "Reformation",
                  "reformation",
                  "register for access",
                  "Release Completed",
                  "Release Completed:",
                  "remove me from your email list",
                  "renewal notice",
                  "Republican",
                  "re-subscribing",
                  "Reuters",
                  "Review",
                  "review",
                  "rganizations",
                  "rmfriedland.com",
                  "RN Public Relations Group",
                  "robly.com",
                  "Royal United Services Institute",
                  "RR invoices",
                  "sales",
                  "scam",
                  "scammer",
                  "Seabras",
                  "Secretary of State",
                  "Security Vulnerability Notification",
                  "SEO Marketing",
                  "SERV",
                  "Services for Artists",
                  "sexist",
                  "Share your comments with",
                  "Show Guests",
                  "show guests",
                  "shower",
                  "Shower",
                  "SMITH PUBLICITY",
                  "solo",
                  "South Tulsa Charmer",
                  "SPAN",
                  "Spectacular",
                  "spectacular",
                  "St. Vincent",
                  "Staples",
                  "Strategic Relations",
                  "Subscription Options",
                  "Subscription question",
                  "subscription will end",
                  "Supercharge",
                  "supercharge",
                  "Supreme Court",
                  "Survey",
                  "survey",
                  "Synup",
                  "Tax Time",
                  "tax time",
                  "TELEMUNDO",
                  "telemundo",
                  "terminate my contributions",
                  "Text or Call",
                  "text or call",
                  "Thank you for your support of Longyear!",
                  "thanked her for her feedback",
                  "the Heights",
                  "The National Funding",
                  "thealtmanbrothers.com",
                  "this account",
                  "THL Credit",
                  "to start receiving",
                  "To: Undisclosed",
                  "transaction",
                  "Undeliverable",
                  "UPDATE:",
                  "Updating Credit details",
                  "urvey request",
                  "USAGM",
                  "User registration approved",
                  "US-Iran",
                  "utexas.edu",
                  "vangelizers",
                  "View this email in your browser",
                  "vocuspr.com",
                  "vrier",
                  "Warning",
                  "warning",
                  "Watchfire Music",
                  "Website sales",
                  "what is my account number",
                  "What is my account number",
                  "wholesale",
                  "widget",
                  "Wine",
                  "wine",
                  "wish to subscribe",
                  "Working remotely",
                  "Writing A Book",
                  "writing a book",
                  "xporter",
                  "your congregation",
                  "Your Monitor Weekly PDF is waiting for you",
                  "your order",
                  "yours in Christ",
                  "Yours in Christ",
                  "Ziopharm",
                  "Zogby Report",
                  "you cannot view this email properly",
                  "account number",
                  "treasurer",
                  "Treasurer",
                  "donation",
                  "pause my subscription",
                  "private equity",
                  "discontinue subscription",
                  "article submission",
                  "embargo",
                  "Subject: subscription",
                  "plan not to renew",
                  "Adnimation",
                  "Demand Blue",
                  "writing for the periodicals",
                  "per capita tax",
                  "Kim Crooks Korinek",
                  "publish my novel",
                  "s.schwarcz@gcomworks.com",
                  "upgrade my subscription",
                  "submitting a cartoon",
                  "like to renew",
                  "Christian Life",
                  "Christian life",
                  "mybiblelesson",
                  "KidRex",
                  "reinstate my subscription",
                  "Where Magazine",
                  "Sales & Marketing",
                  "renew subscription",
                  "bibliography",
                  "Computer Deals",
                  "The Facebook Team",
                  "My Bible",
                  "MEDIPAL HOLDINGS CORPORATION",
                  "For Further information",
                  "place subscription",
                  "AUTO-RENEWAL",
                  "auto-renewal",
                  "do I subscribe to",
                  "Please unsubscribe me",
                  "genealogy research",
                  "York Revolution",
                  "Please see the online error!",
                  "Executive Offices",
                  "debit card",
                  "billing",
                  "solicitations",
                  "PhD",
                  "bonus",
                  "Marble Edition",
                  "S&H",
                  "READER'S BOOKS",
                  "reader's books",
                  "Domain Nerdz",
                  "on sale",
                  "advertising opportunities",
                  "Per Capita Tax",
                  "Publicist",
                  "publicist",
                  "renewed",
                  "Renewed",
                  "Vining")

# 
# Create the pattern to check against
pattern_c = paste(detect_custs, collapse = "|")

# Check the words in this pattern against the description field
result_cd <- grepl(pattern_c, vocab_df$desc)

# Check the pattern against the Subject field
result_cs <- grepl(pattern_c, vocab_df$subj)

# Mark junk / non-customer oriented cases in the description field with an X
for(i in 1:nrow(vocab_df)) {
     if(result_cd[i] == TRUE) {
          vocab_df[i, 5]   <- "X"
     } 
}

# xx Repeat for unwanted cases showing up in the subject field
for(i in 1:nrow(vocab_df)) {
     if(result_cs[i] == TRUE) {
          vocab_df[i, 5]   <- "X"
     } 
}

ol_vocab_df <- nrow(wkg_dat)

# Remove instances where there are not complete data, if any
# vocab_df <- vocab_df[complete.cases(vocab_df), ]

# Remove rows that have been marked with an X
vocab_df <- vocab_df %>% filter(media_case != "X")

fl_vocab_df <- nrow(vocab_df)

# Take a look at our stats
cat("Results Summary")
cat("---------------------------")
cat("Original number of cases  :", ol_vocab_df)
cat("Cases removed by searching:", (ol_vocab_df - fl_vocab_df))
cat("Number of cases to examine:", fl_vocab_df)
cat("Reduction percentage      :", 1 - (fl_vocab_df / ol_vocab_df))

# Write out a file to examine it
write.csv(vocab_df,"D5 - CCC_results.csv", row.names = FALSE)

#
# Take random sample of vocab_df to hunt for additional terms to remove cases
#
case_index  <- createDataPartition(y = vocab_df$seq_id, times = 1, p = 0.05, list = FALSE)
chk_dat <- vocab_df[case_index, ]
write.csv(chk_dat,"R9 - CCC_check.csv", row.names = FALSE)

################################################################################
# End
################################################################################


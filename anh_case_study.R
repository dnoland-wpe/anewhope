# Lead Author/Developer/Analyst: David T. Noland
# Date: 2019-01-21
# Description: Case Study on using functions and loops to reduce code clutter and excessive code repetition while 
#              promoting collaboration between multiple departments with different data acceptance requirements using 
#              the same initial dataset
# Data Requirements: 
#   1. Engineering:
#     a. Listing of all unique environs listed by phase
#     b. Provide addendum of summary count of  accounts and environs by phase
#   2. Communications:
#     a. Provide concatted list of all environs requiring upgrade, by phase, grouped by account
#     b. Only one user email per account by date
#     c. Each account will be upgraded on a single day
# Acceptance criteria: 
#   1. Accounts only found in one phase, regardless of number of users on account
#   2. User only receives one email per account they are authorized on.

library(tidyverse)
library(tm)
library(stringr)

# Set working directory - add project folder
setwd("~/Documents/Github/anewhope")

# Import initial dataset
df <- read.csv("anh_environs.csv", stringsAsFactors = FALSE)
df$Phase <- factor(df$Phase, levels = seq(max(df$Phase), min(df$Phase), by = -1))
str(df)

# Identify anomalous email addresses
clean_emails <- function(x) {
    # Common email anomalies
    x <- as.character(x)
    x <- lapply(x, tolower)
    x <- lapply(x, trimws)
    x <- lapply(x, stripWhitespace)
    x <- gsub("[[:space:]]", "", x)
    x <- gsub("\\.\\.+", ".", x)
    x <- gsub("^\\.", "", x)
    x <- gsub("\\.$", "", x)
    x <- gsub("@-", "@", x)
    
    # substitute email address with invalid characters with NA
    x <- sub("*\\*+", NA, x)
    return(x)
}

# Validate emails following clean
email_pattern <- "^\\<[A-Za-z0-9.-]+[^.]([\\.])[^.][A-Za-z0-9.-]+@[A-Za-z0-9.-]+[^.]([\\.])[^.][A-Za-z]{2,}$"

isValidEmail <- function(x) {
    grepl(email_pattern, as.character(x), ignore.case = TRUE)
}

df['comlink_address'] <- as.data.frame(sapply(df$comlink_address, clean_emails))
check_emails <- lapply(df$comlink_address, isValidEmail)
invalid_emails <- subset(df, !grepl(email_pattern, comlink_address))
#write.csv(df, "testoutput.csv", row.names = FALSE)

# split out concatted install column into individual rows per install
df_concat <- df %>%
               group_by(Phase, upgrade_date, comlink_address, first_name, account, plan, planet) %>%
               summarise(environments_concat = paste(environs, collapse = ","))
colnames(df_concat)[3] <- "comlink_address"

# Function to create engineering phase sets
create_engineering_phase_group <- function(p){
  df_concat %>% 
    filter(Phase == p) %>%
    separate_rows(environments_concat, sep = ",") %>%
    rename(environs = environments_concat) %>%
    select(environs) %>% 
    unique()
}

# Function to create communications phase sets
create_comms_phase_group <- function(p){
  df_concat %>%
    filter(Phase == p)
}

# Loop to create individual phase sets by phase number
phases <- c(unique(df$Phase))
for (i in phases){
  i_group <- create_engineering_phase_group(i)
  assign(paste("Enginnering_phase_", i, sep = ""), i_group)
}

# Loop to create phased comms groups
for (c in phases){
  c_group <- create_comms_phase_group(c)
  assign(paste("Comms_phase_", c, sep = ""), c_group)
}

# Write output files to csv
# Engineering
write.csv(Engineering_phase_1, "Engineering_phase_1.csv", row.names = FALSE)
# Communications
write.csv(Comms_phase_1, "Comms_phase_1.csv", row.names = FALSE)

#summary analysis
df %>%
    select(Phase, comlink_address) %>%
    arrange(Phase) %>%
    unique() %>%
    ggplot(aes(Phase)) +
        geom_bar(fill = "red") +
        ggtitle("Contacts by Phase") +
        theme(legend.position = "none") +
        coord_flip()

df %>%
    select(Phase, account) %>%
    arrange(Phase) %>%
    unique() %>%
    ggplot(aes(Phase)) +
        geom_bar(fill = "blue") +
        ggtitle("Accounts by Phase") +
        theme(legend.position = "none") +
        coord_flip()

df %>%
    select(Phase, environs) %>%
    arrange(Phase) %>%
    unique() %>%
    ggplot(aes(Phase)) +
    geom_bar(fill = "purple") +
    ggtitle("Environs by Phase") +
    theme(legend.position = "none") +
    coord_flip()

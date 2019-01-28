library(tidyverse)
library(tm)

# Set working directory - add project folder
setwd("~/Documents/Github/anewhope")

# Import initial dataset
df <- read.csv("anh_environs.csv", stringsAsFactors = FALSE)
df$Phase <- factor(df$Phase, levels = seq(max(df$Phase), min(df$Phase), by = -1))
str(df)

# Identify anomalous email addresses
clean_emails <- function(x) {
    x <- as.character(x)
    x <- lapply(x, tolower)
    x <- lapply(x, trimws)
    x <- lapply(x, stripWhitespace)
    x <- sub(" ", "", x)
    x <- sub("\\.\\.+", ".", x)
    x <- sub("*\\*+", NA, x)
    x <- sub("^\\.", "", x)
    x <- sub("\\.$", "", x)
    x <- sub("@-", "@", x)
    return(x)
}

isValidEmail <- function(x) {
    grepl("^\\<[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}\\>$", as.character(x), ignore.case = TRUE)
}

# Email format cleaning post EDA (Exploratory Data Analysis) for data format anomalies
df['comlink_address'] <- as.data.frame(sapply(df$comlink_address, clean_emails))
check_emails <- lapply(df$comlink_address, isValidEmail)


print(valid_emails)
write.csv(df, "testoutput.csv", row.names = FALSE)
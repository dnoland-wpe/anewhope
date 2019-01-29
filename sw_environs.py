"""
Lead Author/Developer/Analyst: David T. Noland
Contributors:
Date: 2019-01-25
Description: Case Study on using functions and loops to reduce code clutter and excessive code repetition while 
#              promoting collaboration between multiple departments with different data acceptance requirements using 
#              the same initial dataset
Stakeholders and roles: 
Data Requirements: 1. Engineering:
     a. Listing of all unique environs listed by phase
     b. Provide addendum of summary count of  accounts and environs by phase
   2. Communications:
     a. Provide concatted list of all environs requiring upgrade, by phase, grouped by account
     b. Only one user email per account by date
     c. Each account will be upgraded on a single day
Acceptance criteria:    1. Accounts only found in one phase, regardless of number of users on account
   2. User only receives one email per account they are authorized on.
"""

import os
import re

import numpy as np
import pandas as pd


# set current working directory
os.chdir("/Users/david.noland/Documents/GitHub/anewhope")

# Import initial dataset and save as a dataframe and print first 5 rows
filename = "anh_environs.csv"
df = pd.read_csv(filename)
    
# Exploratory Data Analysis and data cleaning
print(df.head(10))

# Check datatypes for each column
for col in df:
    print(col, ':',  df[col].apply(type).iat[0])

# Check for null values    
print(df.isnull().any(axis = 1))

def clean_emails(frame, column):
    """
    Function to clean common email errors from human entered data
    Remove whitespace, remove multiple simultaneous dots
    """
    frame_cleaned = frame.apply(lambda x: x.str.replace(" ", "") if x.dtype == "object" else x)
    frame_cleaned[column] = frame_cleaned[column].str.replace("\.\.+", ".") 
    return frame_cleaned

df_cleaned = clean_emails(df, 'comlink_address')
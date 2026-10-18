## DATA CLEANING PROCESS FOR DAILY INDONESIA BONDS YIELD DATA
# The main objective of this R file is to perform data cleansing processes
# for Indonesia Bonds Yield Data, covering daily frequency from January 2020 until September 2026

## Load all Library
library(lubridate)
library(dplyr)
library(tidyr)
library(readr)
library(janitor)
library(writexl)

## Import CSV data for all yields
bonds_1year <- read_csv("Raw Data/Bonds Yield 1-Year.csv")
bonds_5year <- read_csv("Raw Data/Bonds Yield 5-Year.csv")
bonds_10year <- read_csv("Raw Data/Bonds Yield 10-Year.csv")
bonds_30year <- read_csv("Raw Data/Bonds Yield 30-Year.csv")

## Perform initial check, including:
# 1) Data Structure; 2) Number of missing values
str(bonds_1year)
names(bonds_1year)
colSums(is.na(bonds_1year))

## Begin the data cleansing process
# Data cleansing for 1-Year Bonds Yield data
bonds_1year <- bonds_1year %>%
  mutate(
    Date = mdy(Date), # Convert Text-formatted date into Standard date format within R
    Change = parse_number(`Change %`) # Create new column to reflect the change of Bonds yield
  ) %>%
  # rename all columns in order to represent each column appropriately
  rename(
    day_date = Date,
    yield = Price,
    open_yield = Open,
    high_yield = High,
    low_yield = Low,
    yield_diff = Change
  ) %>%
  # Re-arrange the date so it begins from January 2nd, 2020
  arrange(day_date) %>%
  # Remove 'Change %' column from the data
  select(-'Change %')

# View the cleansed data for 1-year bonds yield
bonds_1year
str(bonds_1year)

## Continue the cleansing process for all remaining datasets
bonds_5year <- bonds_5year %>%
  mutate(
    Date = mdy(Date),
    Change = parse_number(`Change %`)
  ) %>%
  rename(
    day_date = Date,
    yield = Price,
    open_yield = Open,
    high_yield = High,
    low_yield = Low,
    yield_diff = Change
  ) %>%
  arrange(day_date) %>%
  select(-'Change %')

# View the cleansed data for 5-year bonds yield 
bonds_5year
str(bonds_5year)

# Data cleansing for 10-Year Bonds Yield data
bonds_10year <- bonds_10year %>%
  mutate(
    Date = mdy(Date),
    Change = parse_number(`Change %`)
  ) %>%
  rename(
    day_date = Date,
    yield = Price,
    open_yield = Open,
    high_yield = High,
    low_yield = Low,
    yield_diff = Change
  ) %>%
  arrange(day_date) %>%
  select(-'Change %')

# View the cleansed data for 10-year bonds yield
bonds_10year
str(bonds_10year)

# Data cleansing for 30-Year Bonds Yield data
bonds_30year <- bonds_30year %>%
  mutate(
    Date = mdy(Date),
    Change = parse_number(`Change %`)
  ) %>%
  rename(
    day_date = Date,
    yield = Price,
    open_yield = Open,
    high_yield = High,
    low_yield = Low,
    yield_diff = Change
  ) %>%
  arrange(day_date) %>%
  select(-'Change %')

# Viw the cleansed data for 30-year bonds yield
bonds_30year
str(bonds_30year)

# Export all Bonds Yield datasets into .xlsx format
write_xlsx(bonds_1year, "Clean Data/Bonds Yield 1-Year.xlsx")
write_xlsx(bonds_5year, "Clean Data/Bonds Yield 5-Year.xlsx")
write_xlsx(bonds_10year, "Clean Data/Bonds Yield 10-Year.xlsx")
write_xlsx(bonds_30year, "Clean Data/Bonds Yield 30-Year.xlsx")

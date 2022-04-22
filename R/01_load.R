# Load libraries ----------------------------------------------------------
library("tidyverse")
library("readxl")


# Define functions --------------------------------------------------------
#source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------
read_excel(path = "./data/_raw/SD1_excel.xlsx", skip = 1) %>% 
  write_csv2(file = "./data/SD1_converted.csv") 

SD1_raw <- as_tibble(read_csv2(file = "./data/SD1_converted.csv"))

read_excel(path = "./data/_raw/SD2_excel.xlsx", skip = 1) %>% 
  write_csv2(file = "./data/SD2_converted.csv") 

SD2_raw <- as_tibble(read_csv2(file = "./data/SD2_converted.csv"))

# Wrangle data ------------------------------------------------------------
SD1_clean <-
  SD1_raw %>%
  separate(`Taxonomic classification`, into = c("k", "domain", "p", "phylum", "c", "className", "o", "order", "f", "family", "g", "genus")) %>%
  select(-k, -p, -c, -o, -g, -f)

rm(SD_cle)


# Write data --------------------------------------------------------------
write_tsv(x = my_data,
          file = "data/01_my_data.tsv")












# Load libraries ----------------------------------------------------------
library("tidyverse")


# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------

SD1_raw <- 
  read_excel(path = "./data/_raw/SD1_excel.xlsx", skip = 1, na = c(""," ","NA")) %>% 
  write_csv2(file = "./data/SD1_converted.csv") %>% 
  as_tibble()


SD2_raw <-
  read_excel(path = "./data/_raw/SD2_excel.xlsx", skip = 1,na = c(""," ","NA")) %>% 
  write_csv2(file = "./data/SD2_converted.csv") %>% 
  as_tibble()


# Wrangle data ------------------------------------------------------------



# Write data --------------------------------------------------------------
write_tsv(x = my_data_clean,
          file = "data/02_my_data_clean.tsv")
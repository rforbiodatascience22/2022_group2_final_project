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


# data wrangling and visualization supplementary data 2
SD2_clean <- SD2_raw %>% 
  mutate (COLA1 = sqrt(COLA1/sum(COLA1)),
          COLB1 = sqrt(COLB1/sum(COLB1)),
          N2A = sqrt(N2A/sum(N2A)),
          N2B = sqrt(N2B/sum(N2B)),
          N3C1 = sqrt(N3C1/sum(N3C1)),
          N1C = sqrt(N1C/sum(N1C)))

#data for figure 4: supplementary data 2
SD2_table <- SD2_clean %>% 
  pivot_longer(names_to = "ISSlocation", values_to = "ValueL", COLA1:N1C) %>% 
  arrange(desc(ValueL)) %>% 
  top_n(100)




# Write data --------------------------------------------------------------
write_tsv(x = my_data_clean,
          file = "data/02_my_data_clean.tsv")
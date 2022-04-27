#trial of all codes of main project:

# Load libraries ----------------------------------------------------------
library(tidyverse)
library(readxl)
library(ggplot2)
library(reshape2)
library(patchwork)


# Load data ---------------------------------------------------------------
SD1_raw <- 
  read_excel(path = "./data/_raw/SD1_excel.xlsx", skip = 1, na = c(""," ","NA")) %>% 
  write_csv2(file = "./data/SD1_converted.csv") %>% 
  as_tibble()


SD2_raw <-
  read_excel(path = "./data/_raw/SD2_excel.xlsx", skip = 1, na = c(""," ","NA"))%>% 
  write_csv2(file = "./data/SD2_converted.csv") %>% 
  as_tibble()

# Define functions --------------------------------------------------------
#source(file = "R/99_project_functions.R")


#plots for supplementary data 2----------------------------
#day 4 : 

#fig 4 Shotgun metagenome-based taxonomic information from ISS locations

#create data for fig 4 

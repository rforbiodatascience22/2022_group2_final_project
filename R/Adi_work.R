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



#day 4 :

#plots for supplementary data 2----------------------------
#fig4: Shotgun metagenome-based taxonomic information from ISS locations

#create data for fig 4 
SD2_data <- SD2_clean %>% 
  pivot_longer(names_to = "ISSlocation", values_to = "ValueL", COLA1:N1C)

#plot1 (have to figure out the y axis scale )
plot1 <- SD2_data %>%
  filter(domain == "Archaea" | 
           domain == "Bacteria" | 
           domain == "Eukaryota" | 
           domain == "Viruses")  %>%
  ggplot(mapping = aes(x = ISSlocation,
                       y = ValueL, 
                       fill = domain)) + 
  geom_bar(position = "fill",
           stat="identity") +
  #ylim(0, 15) +
  scale_y_continuous(labels = scales::percent) +
  labs(x= "ISS Locations",
       y = "Abundance (TSS)") +
  theme(axis.text.x = element_blank())
#scale_fill_manual(values = c("", "", "", ""))  #how to identify color names to use here?
plot1

#note : have to figure out how to filter all phylum and genus and the y axis scale range

#plot2   
plot2 <- SD2_data %>%
  ggplot(mapping = aes(x = ISSlocation,
                       y = ValueL, 
                       fill = phylum)) + 
  geom_bar(position = "fill",
           stat="identity") +
  scale_y_continuous(labels = scales::percent) +
  labs(x= "ISS Locations",
       y = "Abundance (TSS)") +
  theme(axis.text.x = element_blank())

plot2


#plot 3 

plot3 <- SD2_data %>%
  filter(Genus == c("Streptococcus", "Corynebacterium", "Lactobacillus", "Acinetobacter", "Staphylococcus", "NA")) %>%
  ggplot(mapping = aes(x = ISSlocation,
                       y = ValueL, 
                       fill = genus)) + 
  geom_bar(position = "fill",
           stat="identity") +
  scale_y_continuous(labels = scales::percent) +
  labs(x= "ISS Locations",
       y = "Abundance (TSS)") +
  theme(axis.text.x = element_blank())
#scale_fill_manual(values = c("", "", "", "")) 

plot3






# Load libraries ----------------------------------------------------------
library("tidyverse")
library("readxl")
library(reshape2)
install.packages("patchwork")
library(patchwork)

# Define functions --------------------------------------------------------
#source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------

SD1_raw <- 
  read_excel(path = "./data/_raw/SD1_excel.xlsx", skip = 1)write_csv2(file = "./data/SD1_converted.csv") %>% 
  as_tibble()


SD2_raw <-
  read_excel(path = "./data/_raw/SD2_excel.xlsx", skip = 1) %>% 
  write_csv2(file = "./data/SD2_converted.csv") %>% 
  as_tibble()

# Wrangle data ------------------------------------------------------------

#Wrangling Supplementary data1

SD1_clean <- 
  SD1_raw %>% 
  mutate(`Taxonomic classification` = str_replace_all(`Taxonomic classification`, pattern = "[:alpha:]__", "")) %>% 
  separate(col = `Taxonomic classification`, into = c("Domain", "Phylum", "Class", "Order", "Family", "Genus", "Species"), sep = ';') %>% 
  na_if("")
SD1_clean

#Creating data1 and plot 1 for ISS Session A,  Stratified with domain

data1<- 
  SD1_clean %>%
  gather(key = ISSCapoA, value= ValueA, ISSCapoA1:ISSCapoA9) 
data1
pl1 <- ggplot(data1, aes(fill=domain, y=ValueA, x=ISSCapoA)) + 
  geom_bar(position = "fill", stat="identity") +
  scale_y_continuous(labels = scales::percent)+
  labs(x= "ISS session A",
       y = "Abundance (TSS)") +
  theme(axis.text.x = element_blank())+
  scale_fill_manual(values = c("lightblue", "deepskyblue4"))
pl1


#Creating data2 and plot 2 for ISS Session B,  Stratified with domain

data2 <- SD1_clean %>%
  gather(key = ISSCapoB, value= ValueB, ISSCapoB1:ISSCapoB8)
data2
pl2 <- ggplot(data2, aes(fill=domain, y=ValueB, x=ISSCapoB)) + 
  geom_bar(position = "fill", stat="identity")+
  scale_y_continuous(labels = scales::percent)+
  labs(x= "ISS session B",
       y = "Abundance (TSS)") +
  theme(axis.text.x = element_blank())+
  scale_fill_manual(values = c("lightblue", "deepskyblue4"))
pl2


#Creating data3 and plot 3 for ISS Session C,  Stratified with domain

data3 <- SD1_clean %>%
  gather(key = ISSCapoC, value= ValueC, ISSCapoC1:ISSCapoC7)
data3
pl3 <- ggplot(data3, aes(fill=domain, y=ValueC, x=ISSCapoC)) + 
  geom_bar(position = "fill", stat="identity") +
  scale_y_continuous(labels = scales::percent)+
  labs(x= "ISS session C",
       y = "Abundance (TSS)") +
  theme(axis.text.x = element_blank())+
  scale_fill_manual(values = c("lightblue", "deepskyblue4"))
pl3



#combining 3 plots together

pl1+pl2+pl3


# Creating  plot 4 for ISS Session A,  Stratified with phylum

pl4 <- data1 %>%
  filter(phylum == "Actinobacteria" | 
           phylum == "Bacteroidetes" | 
           phylum == "Firmicutes" | 
           phylum == "Proteobacteria") %>%
   ggplot(mapping = aes(fill=phylum, 
                       y=ValueA, 
                       x=ISSCapoA)) + 
  geom_bar(position = "fill",
           stat="identity") +
  scale_y_continuous(labels = scales::percent) +
  labs(x= "ISS session A",
       y = "Abundance (TSS)") +
  theme(axis.text.x = element_blank()) +
  scale_fill_manual(values = c("beige", "coral1", "darkorange", "blueviolet"))
pl4


# Creating  plot 5 for ISS Session B,  Stratified with phylum

pl5 <- data2 %>%
  filter(phylum == "Actinobacteria" | 
           phylum == "Bacteroidetes" | 
           phylum == "Firmicutes" | 
           phylum == "Proteobacteria") %>%
  ggplot(mapping = aes(x=ISSCapoB,
                       y=ValueB, 
                       fill=phylum)) + 
  geom_bar(position = "fill",
           stat="identity") +
  scale_y_continuous(labels = scales::percent) +
  labs(x= "ISS session B",
       y = "Abundance (TSS)") +
  theme(axis.text.x = element_blank()) +
  scale_fill_manual(values = c("beige", "coral1", "darkorange", "blueviolet"))
pl5


# Creating  plot 6 for ISS Session C,  Stratified with phylum

pl6 <- data3 %>%
  filter(phylum == "Actinobacteria" | 
           phylum == "Bacteroidetes" | 
           phylum == "Firmicutes" | 
           phylum == "Proteobacteria")  %>%
  ggplot(mapping = aes(x=ISSCapoC,
                       y=ValueC, 
                       fill=phylum)) + 
  geom_bar(position = "fill",
           stat="identity") +
  scale_y_continuous(labels = scales::percent) +
  labs(x= "ISS session C",
       y = "Abundance (TSS)") +
  theme(axis.text.x = element_blank()) +
  scale_fill_manual(values = c("beige", "coral1", "darkorange", "blueviolet"))
pl6


#Combining pl4+pl5+pl6

pl4+pl5+pl6







SD2_clean <- SD2_raw


# Write data --------------------------------------------------------------
write_tsv(x = my_data,
          file = "data/01_my_data.tsv")












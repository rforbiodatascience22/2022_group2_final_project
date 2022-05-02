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
  read_excel(path = "./data/_raw/SD1_excel.xlsx", skip = 1)  %>% 
write_csv2(file = "./data/SD1_converted.csv") %>% 
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

#1.creating 2 columns ISSCapoA and ValueA
data1 <- 
  SD1_clean %>%
  gather(key = ISSCapoA, 
         value= ValueA, 
         ISSCapoA1:ISSCapoA9) %>%
  select(ISSCapoA, ValueA, everything())
data1


#2.renaming all the ISSCapoA rowname
#Creating plot1

pl1 <-
  data1 %>%
  mutate(ISSCapoA = str_replace_all(ISSCapoA, pattern = c('ISSCapoA1' = "5.Columbus RGSH", 
                                                          'ISSCapoA2' = "3.Columbus SCC laptop",
                                                          'ISSCapoA3' = "4.Columbus handrails", 
                                                          'ISSCapoA4' = "2.Columbus light cover", 
                                                          'ISSCapoA5' = "1.Columbus air", 
                                                          'ISSCapoA6' = "6.Node2 sleeping unit",
                                                          'ISSCapoA7' = "7.Node2 panel outside",
                                                          'ISSCapoA8' = "8.Node2 ATU",
                                                          'ISSCapoA9' = "9.Node2 RGSH"))) %>%
  ggplot(mapping = aes(x = ISSCapoA,
                       y = ValueA,
                       fill = Domain)) + 
  geom_bar(position = "fill", 
           stat = "identity") +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "ISS session A",
       y = "Abundance (TSS)") +
  theme(axis.text.x = element_blank()) +
  scale_fill_manual(values = c("lightblue", 
                               "deepskyblue4"))
pl1




#Creating data2 and plot 2 for ISS Session B,  Stratified with domain

#1.creating 2 columns ISSCapoB and ValueB

data2 <- 
  SD1_clean %>%
  gather(key = ISSCapoB, 
         value = ValueB, 
         ISSCapoB1:ISSCapoB8) %>%
  select(ISSCapoB, ValueB, everything())
data2


#2.renaming all the ISSCapoA rowname
#Creating plot2

pl2 <- data2 %>%
  mutate(ISSCapoB = str_replace_all(ISSCapoB, pattern = c('ISSCapoB1' = "5.Columbus RGSH", 
                                                          'ISSCapoB2' = "3.Columbus SCC laptop",
                                                          'ISSCapoB3' = "4.Columbus handrails", 
                                                          'ISSCapoB4' = "2.Columbus light cover", 
                                                          'ISSCapoB5' = "1.Columbus air", 
                                                          'ISSCapoB6' = "6.Node2 sleeping unit",
                                                          'ISSCapoB7' = "7.Node2 panel outside",
                                                          'ISSCapoB8' = "8.Node2 ATU"))) %>%

  ggplot(mapping = aes(x = ISSCapoB,
                       y = ValueB,
                       fill = Domain)) + 
  geom_bar(position = "fill", 
           stat = "identity") +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "ISS session B",
       y = "Abundance (TSS)") +
  theme(axis.text.x = element_blank()) +
  scale_fill_manual(values = c("lightblue", 
                               "deepskyblue4"))
pl2




#Creating data3 and plot 3 for ISS Session C,  Stratified with domain

#1.creating 2 columns ISSCapoC and Valuec

data3 <- 
  SD1_clean %>%
  gather(key = ISSCapoC, 
         value= ValueC, 
         ISSCapoC1:ISSCapoC7) %>%
  select(ISSCapoC, ValueC, everything())
data3

#2.renaming all the ISSCapoA rowname
#Creating plot3

pl3 <- data3 %>%
  mutate(ISSCapoC = str_replace_all(ISSCapoC, pattern = c('ISSCapoC1' = "1.Columbus RGSH", 
                                                          'ISSCapoC2' = "3.Columbus SCC laptop",
                                                          'ISSCapoC3' = "4.Columbus handrails", 
                                                          'ISSCapoC4' = "2.Columbus light cover" , 
                                                          'ISSCapoC5' = "5.Columbus air", 
                                                          'ISSCapoC6' = "6.Node2 sleeping unit",
                                                          'ISSCapoC7' = "7.Node2 panel outside"))) %>%
  ggplot(mapping = aes(x = ISSCapoC,
                       y = ValueC,
                       fill = Domain)) + 
  geom_bar(position = "fill", 
           stat = "identity") +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "ISS session C",
       y = "Abundance (TSS)") +
  theme(axis.text.x = element_blank()) +
  scale_fill_manual(values = c("lightblue", 
                               "deepskyblue4"))
pl3


#combining 3 plots together

pl1+pl2+pl3




# Creating  plot 4 for ISS Session A,  Stratified with Phylum

pl4 <- data1 %>%
  filter(Phylum == "Actinobacteria"| 
         Phylum == "Bacteroidetes"| 
         Phylum == "Firmicutes"| 
         Phylum == "Proteobacteria") %>%
  mutate(ISSCapoA = str_replace_all(ISSCapoA, pattern = c('ISSCapoA1' = "5.Columbus RGSH", 
                                                          'ISSCapoA2' = "3.Columbus SCC laptop",
                                                          'ISSCapoA3' = "4.Columbus handrails", 
                                                          'ISSCapoA4' = "2.Columbus light cover", 
                                                          'ISSCapoA5' = "1.Columbus air", 
                                                          'ISSCapoA6' = "6.Node2 sleeping unit",
                                                          'ISSCapoA7' = "7.Node2 panel outside",
                                                          'ISSCapoA8' = "8.Node2 ATU",
                                                          'ISSCapoA9' = "9.Node2 RGSH"))) %>%
  
  ggplot(mapping = aes(x = ISSCapoA,
                       y = ValueA,
                       fill = Phylum)) + 
  geom_bar(position = "fill", 
           stat = "identity") +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "ISS session A",
       y = "Abundance (TSS)") +
  theme(axis.text.x = element_blank()) +
  scale_fill_manual(values = c("khaki3", "coral1", "orange1", "darkorchid4"))
pl4


# Creating  plot 5 for ISS Session B,  Stratified with phylum

pl5 <- data2 %>%
  filter(Phylum == "Actinobacteria"| 
         Phylum == "Bacteroidetes"| 
         Phylum == "Firmicutes"| 
         Phylum == "Proteobacteria") %>%
  mutate(ISSCapoB = str_replace_all(ISSCapoB, pattern = c('ISSCapoB1' = "5.Columbus RGSH", 
                                                          'ISSCapoB2' = "3.Columbus SCC laptop",
                                                          'ISSCapoB3' = "4.Columbus handrails", 
                                                          'ISSCapoB4' = "2.Columbus light cover", 
                                                          'ISSCapoB5' = "1.Columbus air", 
                                                          'ISSCapoB6' = "6.Node2 sleeping unit",
                                                          'ISSCapoB7' = "7.Node2 panel outside",
                                                          'ISSCapoB8' = "8.Node2 ATU"))) %>%
  
  ggplot(mapping = aes(x = ISSCapoB,
                       y = ValueB,
                       fill = Phylum)) + 
  geom_bar(position = "fill", 
           stat = "identity") +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "ISS session B",
       y = "Abundance (TSS)") +
  theme(axis.text.x = element_blank()) +
  scale_fill_manual(values = c("khaki3", "coral1", "orange1", "darkorchid4"))
pl5


# Creating  plot 7 for ISS Session C,  Stratified with phylum

pl6 <- data3 %>%
  filter(Phylum == "Actinobacteria"| 
         Phylum == "Bacteroidetes"| 
         Phylum == "Firmicutes"| 
         Phylum == "Proteobacteria") %>%
  mutate(ISSCapoC = str_replace_all(ISSCapoC, pattern = c('ISSCapoC1' = "1.Columbus RGSH", 
                                                          'ISSCapoC2' = "3.Columbus SCC laptop",
                                                          'ISSCapoC3' = "4.Columbus handrails", 
                                                          'ISSCapoC4' = "2.Columbus light cover" , 
                                                          'ISSCapoC5' = "5.Columbus air", 
                                                          'ISSCapoC6' = "6.Node2 sleeping unit",
                                                          'ISSCapoC7' = "7.Node2 panel outside"))) %>%

  ggplot(mapping = aes(x = ISSCapoC,
                       y = ValueC,
                       fill = Phylum)) + 
  geom_bar(position = "fill", 
           stat = "identity") +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "ISS session C",
       y = "Abundance (TSS)") +
  theme(axis.text.x = element_blank()) +
  scale_fill_manual(values = c("khaki3", "coral1", "orange1", "darkorchid4"))
pl6


#Combining pl4+pl5+pl6

pl4+pl5+pl6


## Creating  plot 7 for ISS Session A,  Stratified with GENUS
pl7 <- data1 %>%
  replace_na(list(Genus = 'Unknown')) %>%
  filter(Genus == "Streptococcus" | 
         Genus == "Corynebacterium" | 
         Genus == "Lactobacillus" | 
         Genus == "Acinetobacter" | 
         Genus == "Staphylococcus" | 
         Genus == "Unknown") %>%
  mutate(ISSCapoA = str_replace_all(ISSCapoA, pattern = c('ISSCapoA1' = "5.Columbus RGSH", 
                                                          'ISSCapoA2' = "3.Columbus SCC laptop",
                                                          'ISSCapoA3' = "4.Columbus handrails", 
                                                          'ISSCapoA4' = "2.Columbus light cover", 
                                                          'ISSCapoA5' = "1.Columbus air", 
                                                          'ISSCapoA6' = "6.Node2 sleeping unit",
                                                          'ISSCapoA7' = "7.Node2 panel outside",
                                                          'ISSCapoA8' = "8.Node2 ATU",
                                                          'ISSCapoA9' = "9.Node2 RGSH"))) %>%
  select(Genus, ISSCapoA, ValueA) %>% 
  ggplot(mapping = aes(x = ISSCapoA,
                       y = ValueA, 
                       fill = Genus)) + 
  geom_bar(position = "stack",
           stat = "identity") +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "ISS session A",
       y = "Abundance (TSS)") +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_fill_manual(values = c("khaki2", "palevioletred3", "skyblue", "orange", "violet", "turquoise4"))
pl7




## Creating  plot 8 for ISS Session B,  Stratified with Genus
pl8 <- data2 %>%
  replace_na(list(Genus = 'Unknown')) %>%
  filter(Genus == "Streptococcus" | 
         Genus == "Corynebacterium" | 
         Genus == "Lactobacillus" | 
         Genus == "Acinetobacter" | 
         Genus == "Staphylococcus" | 
         Genus == "Unknown") %>%
  mutate(ISSCapoB = str_replace_all(ISSCapoB, pattern = c('ISSCapoB1' = "5.Columbus RGSH", 
                                                          'ISSCapoB2' = "3.Columbus SCC laptop",
                                                          'ISSCapoB3' = "4.Columbus handrails", 
                                                          'ISSCapoB4' = "2.Columbus light cover", 
                                                          'ISSCapoB5' = "1.Columbus air", 
                                                          'ISSCapoB6' = "6.Node2 sleeping unit",
                                                          'ISSCapoB7' = "7.Node2 panel outside",
                                                          'ISSCapoB8' = "8.Node2 ATU"))) %>% 
  select(Genus, ISSCapoB, ValueB) %>% 
  ggplot(mapping = aes(x = ISSCapoB,
                       y = ValueB, 
                       fill = Genus)) + 
  geom_bar(position = "stack",
           stat = "identity") +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "ISS session B",
       y = "Abundance (TSS)") +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_fill_manual(values = c("khaki2", "palevioletred3", "skyblue", "orange", "violet", "turquoise4"))
pl8




#counting how many are there
data3 %>%
  replace_na(list(Genus = 'Unknown')) %>%
  filter(Genus == "Streptococcus" | 
           Genus == "Corynebacterium" | 
           Genus == "Lactobacillus" | 
           Genus == "Acinetobacter" | 
           Genus == "Staphylococcus" | 
           Genus == "Unknown") %>%
  count(Genus)




## Creating  plot 9 for ISS Session C,  Stratified with Genus
pl9 <- data3 %>%
  replace_na(list(Genus = 'Unknown')) %>%
  filter(Genus == "Streptococcus" | 
         Genus == "Corynebacterium" | 
         Genus == "Lactobacillus" | 
         Genus == "Acinetobacter" | 
         Genus == "Staphylococcus" | 
         Genus == "Unknown") %>%
  mutate(ISSCapoC = str_replace_all(ISSCapoC, pattern = c('ISSCapoC1' = "1.Columbus RGSH", 
                                                          'ISSCapoC2' = "3.Columbus SCC laptop",
                                                          'ISSCapoC3' = "4.Columbus handrails", 
                                                          'ISSCapoC4' = "2.Columbus light cover" , 
                                                          'ISSCapoC5' = "5.Columbus air", 
                                                          'ISSCapoC6' = "6.Node2 sleeping unit",
                                                          'ISSCapoC7' = "7.Node2 panel outside"))) %>%
  select(Genus, ISSCapoC, ValueC) %>% 
  ggplot(mapping = aes(x = ISSCapoC,
                       y = ValueC, 
                       fill = Genus)) + 
  geom_bar(position = "stack",
           stat = "identity") +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "ISS session C",
       y = "Abundance (TSS)") +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_fill_manual(values = c("khaki2", "palevioletred3", "skyblue", "orange", "violet", "turquoise4"))
pl9






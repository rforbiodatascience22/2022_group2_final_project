#figure 4 : supplementary data 2 updated version

# Load libraries ----------------------------------------------------------
library(tidyverse)
library(readxl)
library(ggplot2)
library(reshape2)
library(patchwork)
library(hilldiv)

# Load data ---------------------------------------------------------------
SD1_raw <- 
  read_excel(path = "./data/_raw/SD1_excel.xlsx", skip = 1, na = c(""," ","NA")) %>% 
  write_csv2(file = "./data/SD1_converted.csv") %>% 
  as_tibble()


SD2_raw <-
  read_excel(path = "./data/_raw/SD2_excel.xlsx", skip = 1, na = c(""," ","NA"))%>% 
  write_csv2(file = "./data/SD2_converted.csv") %>% 
  as_tibble()

#updated version as per the 02_clean
# data wrangling and visualization supplementary data 2
SD2_clean <- SD2_raw %>% 
  mutate (COLA1 = sqrt(COLA1/sum(COLA1)),
          COLB1 = sqrt(COLB1/sum(COLB1)),
          N2A = sqrt(N2A/sum(N2A)),
          N2B = sqrt(N2B/sum(N2B)),
          N3C1 = sqrt(N3C1/sum(N3C1)),
          N1C = sqrt(N1C/sum(N1C)))


#data for figure 4: supplementary data 2
SD2_data <- SD2_clean %>% 
  pivot_longer(names_to = "ISSlocation", values_to = "ValueL", COLA1:N1C) 

#plot 1 Abundance sqrt(TSS) vs domain
#plot1 <- 
SD2_data %>%
  filter(domain == "Archaea" | 
           domain == "Bacteria" | 
           domain == "Eukaryota" |
           domain == "other sequences" |
           domain == "Viruses")  %>%
  ggplot(mapping = aes(x = ISSlocation,
                       y = ValueL, 
                       fill = domain)) + 
  geom_bar(position = "fill",
           stat="identity") +
  labs(x= "ISS Locations",
       y = "Abundance sqrt(TSS)") +
  theme(axis.text.x = element_text(angle = 90)) +
 scale_fill_manual(values = c("lightgreen", "salmon", "violet", "darkgreen", "skyblue")) 
  
# plot1




#data for plot 2 and plot 3
SD2_top_100 <- SD2_clean %>% 
  pivot_longer(names_to = "ISSlocation", values_to = "ValueL", COLA1:N1C) %>% 
  arrange(desc(ValueL)) %>% 
  top_n(100)

#plot 2: Abundance sqrt(TSS) vs phylum
SD2_top_100 %>%
  ggplot(mapping = aes(x = ISSlocation,
                       y = ValueL, 
                       fill = phylum)) + 
  geom_bar(position = "fill",
           stat="identity") +
labs(x= "ISS Locations",
     y = "Abundance sqrt(TSS)")


#plot 3: Abundance sqrt(TSS) vs genus(top 40)
SD2_top_100 %>%
  filter(domain == "Bacteria") %>% 
  ggplot(mapping = aes(x = ISSlocation,
                       y = ValueL, 
                       fill = genus)) + 
  geom_bar(position = "fill",
           stat="identity") +
  labs(x= "ISS Locations",
       y = "Abundance sqrt(TSS)")

-------------------------------------




#trial

#method 3 using mutate function


SD2_table <- SD2_raw %>% 
  mutate (COLA1 = sqrt(COLA1/sum(COLA1)),
          COLB1 = sqrt(COLB1/sum(COLB1)),
          N2A = sqrt(N2A/sum(N2A)),
          N2B = sqrt(N2B/sum(N2B)),
          N3C1 = sqrt(N3C1/sum(N3C1)),
          N1C = sqrt(N1C/sum(N1C)))
View(SD2_table)


#data for plot
SD2_data <- SD2_table %>% 
  pivot_longer(names_to = "ISSlocation", values_to = "ValueL", COLA1:N1C) 


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
  #scale_y_continuous(labels = scales::percent) +
  #scale_y_continuous(breaks = seq(0, 15, 5), 
  #limits=c(0, 15))
  
  labs(x= "ISS Locations",
       y = "Abundance (TSS)") +
  #names.arg = c("COLA1", "COLB1", "N2A", "N2B", "N3C1", "N1C")) 
  theme(axis.text.x = element_blank())
#scale_fill_manual(values = c("", "", "", ""))

plot1

#Prince 
SD2_raw <-
  read_excel(path = "./data/SD2_excel.xlsx", skip = 1, na = c(""," ","NA"))%>% 
  write_csv2(file = "./data/SD2_converted.csv") %>% 
  as_tibble()

SD2_table <- SD2_raw %>% 
  mutate (COLA1 = sqrt(COLA1/sum(COLA1)),
          COLB1 = sqrt(COLB1/sum(COLB1)),
          N2A = sqrt(N2A/sum(N2A)),
          N2B = sqrt(N2B/sum(N2B)),
          N3C1 = sqrt(N3C1/sum(N3C1)),
          N1C = sqrt(N1C/sum(N1C)))

SD2_top_40 <- SD2_table %>% 
  pivot_longer(names_to = "ISSlocation", values_to = "ValueL", COLA1:N1C) %>% 
  arrange(desc(ValueL)) %>% 
  top_n(500)


SD2_top_40 %>%
  ggplot(mapping = aes(x = ISSlocation,
                       y = ValueL, 
                       fill = phylum)) + 
  geom_bar(position = "stack",
           stat="identity") 
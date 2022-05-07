# Wrangle data ------------------------------------------------------------

# data wrangling supplementary data 1
SD1_clean <- 
  SD1_raw %>% 
  mutate(`Taxonomic classification` = str_replace_all(`Taxonomic classification`, pattern = "[:alpha:]__", "")) %>% 
  separate(col = `Taxonomic classification`, into = c("Domain", "Phylum", "Class", "Order", "Family", "Genus", "Species"), sep = ';') %>% 
  na_if("") 

SD1_clean %>% write.csv2(file = "./data/SD1_clean.csv")

SD1_TSS <- 
  SD1_clean %>%
  mutate(ISSCapoA1 = ISSCapoA1/sum(ISSCapoA1),
         ISSCapoA2 = ISSCapoA2/sum(ISSCapoA2),
         ISSCapoA3 = ISSCapoA3/sum(ISSCapoA3),
         ISSCapoA4 = ISSCapoA4/sum(ISSCapoA4),
         ISSCapoA5 = ISSCapoA5/sum(ISSCapoA5),
         ISSCapoA6 = ISSCapoA6/sum(ISSCapoA6),
         ISSCapoA7 = ISSCapoA7/sum(ISSCapoA7),
         ISSCapoA8 = ISSCapoA8/sum(ISSCapoA8),
         ISSCapoA9 = ISSCapoA9/sum(ISSCapoA9),
         ISSCapoB1 = ISSCapoB1/sum(ISSCapoB1),
         ISSCapoB2 = ISSCapoB2/sum(ISSCapoB2),
         ISSCapoB3 = ISSCapoB3/sum(ISSCapoB3),
         ISSCapoB4 = ISSCapoB4/sum(ISSCapoB4),
         ISSCapoB5 = ISSCapoB5/sum(ISSCapoB5),
         ISSCapoB6 = ISSCapoB6/sum(ISSCapoB6),
         ISSCapoB7 = ISSCapoB7/sum(ISSCapoB7),
         ISSCapoB8 = ISSCapoB8/sum(ISSCapoB8),
         ISSCapoC1 = ISSCapoC1/sum(ISSCapoC1),
         ISSCapoC2 = ISSCapoC2/sum(ISSCapoC2),
         ISSCapoC3 = ISSCapoC3/sum(ISSCapoC3),
         ISSCapoC4 = ISSCapoC4/sum(ISSCapoC4),
         ISSCapoC5 = ISSCapoC5/sum(ISSCapoC5),
         ISSCapoC6 = ISSCapoC6/sum(ISSCapoC6),
         ISSCapoC7 = ISSCapoC7/sum(ISSCapoC7))

SD1_TSS %>% write.csv2(file = "./data/SD1_clean_TSS.csv")

SD1_data_pivot_longer <- SD1_clean %>% 
  pivot_longer(names_to = "ISSCapoA", values_to = "ValueA", ISSCapoA1 : ISSCapoA9) %>%
  pivot_longer(names_to = "ISSCapoB", values_to = "ValueB", ISSCapoB1 : ISSCapoB8) %>%
  pivot_longer(names_to = "ISSCapoC", values_to = "ValueC", ISSCapoC1 : ISSCapoC7) %>% 
  mutate(ISSCapoA = str_replace_all(ISSCapoA, pattern = c('ISSCapoA1' = "5.Columbus RGSH", 
                                                          'ISSCapoA2' = "3.Columbus SCC laptop",
                                                          'ISSCapoA3' = "4.Columbus handrails", 
                                                          'ISSCapoA4' = "2.Columbus light cover", 
                                                          'ISSCapoA5' = "1.Columbus air", 
                                                          'ISSCapoA6' = "6.Node2 sleeping unit",
                                                          'ISSCapoA7' = "7.Node2 panel outside",
                                                          'ISSCapoA8' = "8.Node2 ATU",
                                                          'ISSCapoA9' = "9.Node2 RGSH")),
         ISSCapoB = str_replace_all(ISSCapoB, pattern = c('ISSCapoB1' = "1.Columbus air", 
                                                          'ISSCapoB2' = "2.Columbus light cover",
                                                          'ISSCapoB3' = "3.Columbus SSC laptop", 
                                                          'ISSCapoB4' = "4.Columbus handrails", 
                                                          'ISSCapoB5' = "5.Columbus RGSH", 
                                                          'ISSCapoB6' = "6.Node2 sleeping unit",
                                                          'ISSCapoB7' = "7.Node2 panel outside",
                                                          'ISSCapoB8' = "8.Node2 ATU")),
         ISSCapoC = str_replace_all(ISSCapoC, pattern = c('ISSCapoC1' = "1.Cupola air", 
                                                          'ISSCapoC2' = "2.Cupola surface",
                                                          'ISSCapoC3' = "3.Node3 ARED ", 
                                                          'ISSCapoC4' = "4.Node3 treadmill" , 
                                                          'ISSCapoC5' = "5.Node3 WHC", 
                                                          'ISSCapoC6' = "6.Node1 Panel inside",
                                                          'ISSCapoC7' = "7.Node1 dinning table")))

# data wrangling supplementary data 2
SD2_clean_TSS <- SD2_raw %>% 
  mutate (COLA1 = sqrt(COLA1/sum(COLA1)),
          COLB1 = sqrt(COLB1/sum(COLB1)),
          N2A = sqrt(N2A/sum(N2A)),
          N2B = sqrt(N2B/sum(N2B)),
          N3C1 = sqrt(N3C1/sum(N3C1)),
          N1C = sqrt(N1C/sum(N1C)))

SD2_clean_TSS %>% write.csv2(file = "./data/SD2_clean_TSS.csv")

#data for plot 10 and plot 11
SD2_data_pivot_longer <- SD2_clean_TSS %>% 
  pivot_longer(names_to = "ISSlocation", values_to = "ValueL", COLA1:N1C) 

#data for plot 12
SD2_top_200 <- SD2_clean_TSS %>% 
  pivot_longer(names_to = "ISSlocation", values_to = "ValueL", COLA1:N1C) %>% 
  arrange(desc(ValueL)) %>% 
  top_n(200)

#Tidy Table 1
table1_tidy <- 
  table1 %>% 
  mutate(`Wipe` = str_replace_all(`Wipe`, pattern = "-", "")) %>% 
  separate(col = Wipe, into = c("Wipe1", "Wipe2"), sep = ",") %>% 
  separate(col = Session, into = c("Session1", "Session2"), sep = ",") %>% 
  pivot_longer(cols = c("Wipe1", "Wipe2"), values_to = "Wipe") %>% 
  select(-name) %>% 
  pivot_longer(cols = c("Session1", "Session2"), values_to = "Session") %>% 
  select(-name) %>% 
  drop_na()

table1_tidy %>% write.csv2(file = "./data/table1_tidy.csv")

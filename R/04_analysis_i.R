# Visualize data ----------------------------------------------------------

#SD1 Plots
pl1 <- SD1_data_pivot_longer %>%
  ggplot(mapping = aes(x = ISSCapoA,
                       y = ValueA,
                       fill = Domain)) + 
  geom_bar(position = "fill", 
           stat = "identity") +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "ISS session A",
       y = "Abundance (TSS)") +
  theme(axis.text.x = element_text(angle = 90),
        legend.position = "none") +
  scale_fill_manual(values = c("lightblue", 
                               "deepskyblue4"))


ggsave(pl1, file = "./results/pl1.jpg")

pl2 <- SD1_data_pivot_longer %>%
  ggplot(mapping = aes(x = ISSCapoB,
                       y = ValueB,
                       fill = Domain)) + 
  geom_bar(position = "fill", 
           stat = "identity") +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "ISS session B",
       y = "Abundance (TSS)") +
  theme(axis.text.x = element_text(angle = 90),
        axis.title.y = element_blank(),
              legend.position = "none") +
  scale_fill_manual(values = c("lightblue", 
                               "deepskyblue4"))
pl2
  
pl3 <- SD1_data_pivot_longer %>%
  ggplot(mapping = aes(x = ISSCapoC,
                       y = ValueC,
                       fill = Domain)) + 
  geom_bar(position = "fill", 
           stat = "identity") +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "ISS session C",
       y = "Abundance (TSS)") +
  theme(axis.text.x = element_text(angle = 90),
        axis.title.y = element_blank()) +
  scale_fill_manual(values = c("lightblue", 
                               "deepskyblue4"))
pl3

pl1 + pl2 + pl3

fig1_Domain <- pl1 + pl2 + pl3
fig1_Domain

ggsave("./doc/images/fig1_Domain.png",
       width = 3000,
       height = 1200,
       units = "px")

pl4 <- SD1_data_pivot_longer %>%
    filter(Phylum == "Actinobacteria"| 
           Phylum == "Bacteroidetes"| 
           Phylum == "Firmicutes"| 
           Phylum == "Proteobacteria"|
           Phylum == "Woesearchaeota_(DHVEG-6)"|
           Phylum == "Thaumarchaeota"|
           Phylum == "Euryarchaeota") %>%
  ggplot(mapping = aes(x = ISSCapoA,
                       y = ValueA,
                       fill = Phylum)) + 
  geom_bar(position = "fill", 
           stat = "identity") +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "ISS session A",
       y = "Abundance (TSS)") +
  theme(axis.text.x = element_text(angle = 90),
        legend.position = "none") +
  scale_fill_manual(values = c("khaki3", "coral1", "skyblue", "orange1", "darkorchid4", "skyblue", "skyblue"))
pl4
pl5 <- SD1_data_pivot_longer %>%
    filter(Phylum == "Actinobacteria"| 
           Phylum == "Bacteroidetes"| 
           Phylum == "Firmicutes"| 
           Phylum == "Proteobacteria"|
           Phylum == "Woesearchaeota_(DHVEG-6)"|
           Phylum == "Thaumarchaeota"|
           Phylum == "Euryarchaeota") %>%
  ggplot(mapping = aes(x = ISSCapoB,
                       y = ValueB,
                       fill = Phylum)) + 
  geom_bar(position = "fill", 
           stat = "identity") +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "ISS session B",
       y = "Abundance (TSS)") +
  theme(axis.text.x = element_text(angle = 90),
        legend.position = "none") +
  scale_fill_manual(values = c("khaki3", "coral1", "skyblue", "orange1", "darkorchid4", "skyblue", "skyblue"))

pl6 <- SD1_data_pivot_longer %>%
    filter(Phylum == "Actinobacteria"| 
           Phylum == "Bacteroidetes"| 
           Phylum == "Firmicutes"| 
           Phylum == "Proteobacteria"|
           Phylum == "Woesearchaeota_(DHVEG-6)"|
           Phylum == "Thaumarchaeota"|
           Phylum == "Euryarchaeota") %>%
  ggplot(mapping = aes(x = ISSCapoC,
                       y = ValueC,
                       fill = Phylum)) + 
  geom_bar(position = "fill", 
           stat = "identity") +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "ISS session C",
       y = "Abundance (TSS)") +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_fill_manual(values = c("khaki3", "coral1", "skyblue", "orange1", "darkorchid4", "skyblue", "skyblue"))
pl6

pl4 + pl5 + pl6

fig1_Phylum <- pl4 + pl5 + pl6
fig1_Phylum

ggsave("./doc/images/fig1_Phylum.png",
       width = 3000,
       height = 1200,
       units = "px")

pl7 <- SD1_data_pivot_longer %>%
  replace_na(list(Genus = 'Unknown')) %>%
    filter(Genus == "Streptococcus" | 
           Genus == "Corynebacterium" | 
           Genus == "Lactobacillus" | 
           Genus == "Acinetobacter" | 
           Genus == "Staphylococcus" | 
           Genus == "Unknown" ) %>%
  select(Genus, ISSCapoA, ValueA) %>% 
  ggplot(mapping = aes(x = ISSCapoA,
                       y = ValueA, 
                       fill = Genus)) + 
  geom_bar(position = "fill",
           stat = "identity") +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "ISS session A",
       y = "Abundance (TSS)") +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_fill_manual(values = c("khaki2", "palevioletred3", "skyblue", "orange", "violet", "turquoise4"))
pl7
pl8 <- SD1_data_pivot_longer %>%
  replace_na(list(Genus = 'Unknown')) %>%
    filter(Genus == "Streptococcus" | 
           Genus == "Corynebacterium" | 
           Genus == "Lactobacillus" | 
           Genus == "Acinetobacter" | 
           Genus == "Staphylococcus" | 
           Genus == "Unknown") %>%
  select(Genus, ISSCapoB, ValueB) %>% 
  ggplot(mapping = aes(x = ISSCapoB,
                       y = ValueB, 
                       fill = Genus)) + 
  geom_bar(position = "fill",
           stat = "identity") +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "ISS session B",
       y = "Abundance (TSS)") +
  theme(axis.text.x = element_text(angle = 90),
        axis.title.y = element_blank()) +
  theme() +
  scale_fill_manual(values = c("khaki2", "palevioletred3", "skyblue", "orange", "violet", "turquoise4")) 
pl8
pl9 <- SD1_data_pivot_longer %>%
  replace_na(list(Genus = 'Unknown')) %>%
    filter(Genus == "Streptococcus" | 
           Genus == "Corynebacterium" | 
           Genus == "Lactobacillus" | 
           Genus == "Acinetobacter" | 
           Genus == "Staphylococcus" | 
           Genus == "Unknown") %>%
  select(Genus, ISSCapoC, ValueC) %>% 
  ggplot(mapping = aes(x = ISSCapoC,
                       y = ValueC, 
                       fill = Genus)) + 
  geom_bar(position = "fill",
           stat = "identity") +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "ISS session C",
       y = "Abundance (TSS)") +
  theme(axis.text.x = element_text(angle = 90),
        axis.title.y = element_blank()) +
  scale_fill_manual(values = c("khaki2", "palevioletred3", "skyblue", "orange", "violet", "turquoise4"))
pl9
pl7 + pl8 + pl9

fig1_Genus <- pl7 + pl8 + pl9
fig1_Genus

ggsave("./doc/images/fig1_Genus.png",
       width = 3000,
       height = 1200,
       units = "px")


# SD2 Plots
#plot 10 Abundance sqrt(TSS) vs Domain
pl10 <- SD2_data_pivot_longer %>%
    filter(domain == "Archaea" | 
           domain == "Bacteria" | 
           domain == "Eukaryota" |
           domain == "other sequences" |
           domain == "Viruses")  %>%
  ggplot(mapping = aes(x = ISSlocation,
                       y = ValueL, 
                       fill = domain)) + 
  geom_bar(position = "fill",
           stat = "identity") +
  labs(x = "ISS Locations",
       y = "Abundance sqrt(TSS)") +
  theme(axis.text.x = element_text(),
        legend.title = element_text( size = 12), 
        legend.text = element_text(size = 10)) +
  scale_fill_manual(values = c("green","salmon","violet","darkgreen","skyblue")) 

pl10
ggsave("./doc/images/fig4_Domain.png",
       width = 3000,
       height = 1200,
       units = "px")

#plot 11: Abundance sqrt(TSS) vs Phylum
pl11 <- SD2_data_pivot_longer %>%
  filter(domain == "Archaea" | 
           domain == "Bacteria") %>% 
  ggplot(mapping = aes(x = ISSlocation,
                       y = ValueL, 
                       fill = phylum)) + 
  geom_bar(position = "fill",
           stat = "identity") +
  labs(x = "ISS Locations",
       y = "Abundance sqrt(TSS)") +
  theme(legend.title = element_text( size = 12), 
        legend.text = element_text(size = 10))

pl11
ggsave("./doc/images/fig4_Phylum.png",
       width = 3000,
       height = 1300,
       units = "px")

#plot 12: Abundance sqrt(TSS) vs genus(top 40)
pl12 <- SD2_top_200 %>%
  filter(domain == "Bacteria") %>% 
  ggplot(mapping = aes(x = ISSlocation,
                       y = ValueL, 
                       fill = genus)) + 
  geom_bar(position = "fill",
           stat = "identity") +
  labs(x = "ISS Locations",
       y = "Abundance sqrt(TSS)") +
  theme(legend.title = element_text(size = 12), 
        legend.text = element_text(size = 8))

pl12
ggsave("./doc/images/fig4_Genus.png",
       width = 3000,
       height = 1600,
       units = "px")

# Write data --------------------------------------------------------------

# Visualise data ----------------------------------------------------------
pl1 <- SD1_data_pivot_longer %>%
  ggplot(mapping = aes(x = ISSCapoA,
                       y = ValueA,
                       fill = Domain)) + 
  geom_bar(position = "fill", 
           stat = "identity") +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "ISS session A",
       y = "Abundance (TSS)") +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_fill_manual(values = c("lightblue", 
                               "deepskyblue4"))

pl2 <- SD1_data_pivot_longer %>%
  ggplot(mapping = aes(x = ISSCapoB,
                       y = ValueB,
                       fill = Domain)) + 
  geom_bar(position = "fill", 
           stat = "identity") +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "ISS session B",
       y = "Abundance (TSS)") +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_fill_manual(values = c("lightblue", 
                               "deepskyblue4"))


pl3 <- SD1_data_pivot_longer %>%
  ggplot(mapping = aes(x = ISSCapoC,
                       y = ValueC,
                       fill = Domain)) + 
  geom_bar(position = "fill", 
           stat = "identity") +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "ISS session C",
       y = "Abundance (TSS)") +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_fill_manual(values = c("lightblue", 
                               "deepskyblue4"))

pl1 + pl2 + pl3

pl4 <- SD1_data_pivot_longer %>%
  filter(Phylum == "Actinobacteria"| 
           Phylum == "Bacteroidetes"| 
           Phylum == "Firmicutes"| 
           Phylum == "Proteobacteria") %>%
  ggplot(mapping = aes(x = ISSCapoA,
                       y = ValueA,
                       fill = Phylum)) + 
  geom_bar(position = "fill", 
           stat = "identity") +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "ISS session A",
       y = "Abundance (TSS)") +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_fill_manual(values = c("khaki3", "coral1", "orange1", "darkorchid4"))

pl5 <- SD1_data_pivot_longer %>%
  filter(Phylum == "Actinobacteria"| 
           Phylum == "Bacteroidetes"| 
           Phylum == "Firmicutes"| 
           Phylum == "Proteobacteria") %>%
  ggplot(mapping = aes(x = ISSCapoB,
                       y = ValueB,
                       fill = Phylum)) + 
  geom_bar(position = "fill", 
           stat = "identity") +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "ISS session B",
       y = "Abundance (TSS)") +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_fill_manual(values = c("khaki3", "coral1", "orange1", "darkorchid4"))

pl6 <- SD1_data_pivot_longer %>%
  filter(Phylum == "Actinobacteria"| 
           Phylum == "Bacteroidetes"| 
           Phylum == "Firmicutes"| 
           Phylum == "Proteobacteria") %>%
  ggplot(mapping = aes(x = ISSCapoC,
                       y = ValueC,
                       fill = Phylum)) + 
  geom_bar(position = "fill", 
           stat = "identity") +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "ISS session C",
       y = "Abundance (TSS)") +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_fill_manual(values = c("khaki3", "coral1", "orange1", "darkorchid4"))

pl4 + pl5 + pl6

pl7 <- SD1_data_pivot_longer %>%
  replace_na(list(Genus = 'Unknown')) %>%
  filter(Genus == "Streptococcus" | 
           Genus == "Corynebacterium" | 
           Genus == "Lactobacillus" | 
           Genus == "Acinetobacter" | 
           Genus == "Staphylococcus" | 
           Genus == "Unknown") %>%
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
  theme(axis.text.x = element_text(angle = 90)) +
  scale_fill_manual(values = c("khaki2", "palevioletred3", "skyblue", "orange", "violet", "turquoise4"))

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
  theme(axis.text.x = element_text(angle = 90)) +
  scale_fill_manual(values = c("khaki2", "palevioletred3", "skyblue", "orange", "violet", "turquoise4"))


pl7 + pl8 + pl9




# Write data --------------------------------------------------------------
write_tsv(...)
ggsave(...)
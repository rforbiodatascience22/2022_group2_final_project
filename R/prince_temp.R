#Perform TSS via mutate call
SD_2_TSS <-
read_excel(path = "./data/_raw/SD2_excel.xlsx", 
           skip = 1, 
           na = c(""," ","NA")) %>% 
  as_tibble() %>% 
  mutate(COLA1 = COLA1/sum(COLA1),
         COLB1 = COLB1/sum(COLB1),
         N2A = N2A/sum(N2A),
         N2B = N2B/sum(N2B),
         N3C1 = N3C1/sum(N3C1),
         N1C = N1C/sum(N1C))

#Each column sums to 1
sum(SD_2_TSS$COLA1)
sum(SD_2_TSS$COLB1)
sum(SD_2_TSS$N2A)
# Load libraries ----------------------------------------------------------
library("tidyverse")
library("readxl")
library("patchwork")

# Load data ---------------------------------------------------------------

SD1_raw <- 
  read_excel(path = "./data/_raw/SD1_excel.xlsx", skip = 1) %>% 
  write_csv2(file = "./data/SD1_converted.csv") %>% 
  as_tibble()


SD2_raw <-
  read_excel(path = "./data/_raw/SD2_excel.xlsx", skip = 1) %>% 
  write_csv2(file = "./data/SD2_converted.csv") %>% 
  as_tibble()

table1 <- tribble(
  ~Wipe, ~Sampled_surface, ~ISS_module, ~Session,
  c("A-5,B-1"), "Ambient air (field blank, FB)", "Columbus", c("A,B"),
  c("A-4,B-2"), "Light covers", "Columbus", c("A,B"),
  c("A-2,B-3"), "SCC laptop", "Columbus", c("A,B"),
  c("A-3,B-4"), "Hand grips", "Columbus", c("A,B"),
  c("A-1,B-5"), "Return Grid Sensor Housing (RGSH)", "Columbus", c("A,B"),
  c("A-6,B-6"), "Sleeping unit", "Node 2", c("A,B"),
  c("A-7,B-7"), "Panels (outer surface, close to the Portable Fire Extinguisher (PFA) and Portable Breathing", "Node 2", c("A,B"),
  c("A-8,B-8"), "Audio Terminal Unit (ATU)", "Node2", c("A,B"),
  "A-9", "Return Grid Sensor Housing (RGSH)", "Node 2", "A",
  "C-1", "Ambient air (field blank, FB)", "Cupola", "C",
  "C-2", "Surface facing a window", "Cupola", "C",
  "C-3", "Advanced Resistive Exercise Device (ARED)", "Node 3", "C",
  "C-4", "Treadmill", "Node 3", "C", 
  "C-5", "Waste and Hygiene Compartment (WHC): surfaces", "Node 3", "C",
  "C-6", "Cover of the PBA, inside", "Node 1", "C", 
  "C-7", "Dining table", "Node 1", "C") 

table1 %>% write.csv2(file = "./data/_raw/table1_raw.csv")



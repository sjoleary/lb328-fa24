

## Get set up ----

# read libraries
library(readr)
library(janitor)
library(lubridate)
library(tidyr)
library(dplyr)
library(glue)
library(ggplot2)
library(patchwork)
library(knitr)

# read in sample information
sampleinfo <- read_delim("data/RAND_eDNA-samples.csv", ",") %>%
  clean_names()

# read in qPCR results
results <- read_delim("data/RAND_eDNA-concentrations.txt", delim = "\t") %>%
  left_join(sampleinfo) %>%
  mutate(rel_conc = rel_conc*1000)



## Assess brook trout occupancy ----




## Assess environmental factors ----




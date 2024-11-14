

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


# read data sets
rand_2210 <- read_delim("data/macros_RAND_2022-10-19.tsv", delim = "\t") %>%
  clean_names()

rand_2305 <- read_delim("data/macros_RAND_2023-05-30.tsv", delim = "\t") %>%
  clean_names()

rand_2307 <- read_delim("data/macros_RAND_2023-07-07.tsv", delim = "\t") %>%
  clean_names()

rand_2310 <- read_delim("data/macros_RAND_2023-10-18.tsv", delim = "\t") %>%
  clean_names()

rand_2407 <- read_delim("data/macros_RAND_2024-07-09.tsv", delim = "\t") %>%
  clean_names()

rand_2410 <- read_delim("data/macros_RAND_2024-10-02.tsv", delim = "\t") %>%
  clean_names()

schb_2210 <- read_delim("data/macros_SCHB_2022-11-02.tsv", delim = "\t") %>%
  clean_names()

schb_2305 <- read_delim("data/macros_SCHB_2023-05-30.tsv", delim = "\t") %>%
  clean_names()

schb_2410 <- read_delim("data/macros_SCHB_2024-10-23.tsv", delim = "\t") %>%
  clean_names()


## Compare species richness and diversity for selected headwater streams across spring, summer, and fall ----

# Rand October 2024 (Fall) ----

# table with number of individuals in each order
kable(
  rand_2410 %>%
    group_by(order) %>%
    count()
)






# Visualize results ----



## Compare stream habitat quality for selected headwater streams across spring, summer, and fall based on macroinvertebrate as bioindicators indices ----

# combine data sets
macros <- bind_rows(rand_2210,
                    rand_2305,
                    rand_2307,
                    rand_2310,
                    rand_2407,
                    rand_2410,
                    schb_2210,
                    schb_2305,
                    schb_2410)

# Table with EPT index 
kable( 
  macros %>%
    mutate(EPT = ifelse(order %in% c("Ephemeroptera", "Plecoptera", "Trichoptera"), "EPT", "non-EPT")) %>%
    group_by(site_id, sample_date, EPT) %>%
    count() %>%
    ungroup() %>%
    group_by(site_id, sample_date) %>%
    mutate(EPT_index = n/(sum(n))) %>%
    filter(EPT == "EPT"),
  digits = 3
)

# read tolerance data
tolerance <- read_delim("data/macros_family-tolerance.csv", delim = ",")

# table with Hilsenhoff Biotic Index
kable(
  macros %>%
    left_join(tolerance) %>%
    filter(!is.na(tolerance)) %>%
    group_by(site_id, sample_date, family, tolerance) %>%
    count() %>%
    mutate(weighted_abund = n*tolerance) %>%
    ungroup() %>%
    group_by(site_id, sample_date) %>%
    summarize(HSI = sum(weighted_abund)/sum(n)),
  digits = 2)




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


## Read in data sets ----

# plate set up
setup <- read_delim("data/qPCR_brk_2024-11-08_samples.csv", delim = ",", skip = 7) %>%
  clean_names() %>%
  select(well, sample_name, target_name, task, quantity) %>%
  filter(!is.na(task))

# raw fluorescence
FAM <- read_delim("data/qPCR_brk_2024-11-08_multicomponent.csv", skip = 7, delim = ",") %>%
  clean_names() %>%
  filter(!is.na(fam)) %>%
  left_join(setup)

# normalized fluorescence
Rn <- read_delim("data/qPCR_brk_2024-11-08_amplification.csv", delim = ",", skip = 7) %>%
  clean_names() %>%
  rename(delta_rn = rn_2) %>%
  filter(!is.na(target_name)) %>%
  select(-target_name) %>%
  left_join(setup)


# extract threshold of detection
threshold <- read_delim("data/qPCR_brk_2024-11-08_results.csv", skip = 7, delim = ",") %>%
  clean_names() %>%
  select(ct_threshold) %>%
  slice_max(ct_threshold, n = 1, with_ties = FALSE) %>%
  pull()

# extract baseline cycle values
baseline <- read_delim("data/qPCR_brk_2024-11-08_results.csv", skip = 7, delim = ",") %>%
  clean_names() %>%
  filter(!is.na(target_name)) %>%
  select(well, baseline_start, baseline_end) %>%
  left_join(setup)

# Cycle threshold values
Ct <- read_delim("data/qPCR_brk_2024-11-08_results.csv", skip = 7, delim = ",") %>%  
  clean_names() %>%                                             
  select(well, c) %>% 
  mutate(c = as.numeric(c)) %>%
  filter(!is.na(c)) %>%           
  left_join(setup)  


## Process data ----

# detected fluorescence
FAM %>%
  pivot_longer(names_to = "dye", values_to = "fluorescence", 3:4) %>%
  ggplot(aes(x = cycle, y = fluorescence, color = well)) +
  geom_line() +
  facet_grid(target_name ~ dye) +
  scale_y_log10() +
  theme_classic() +
  theme(legend.position = "blank")

# normalize fluorescence, baseline & threshold of detection
p1 <- ggplot(Rn, aes(x = cycle, y = delta_rn, color = well)) +
  geom_line() +
  geom_hline(yintercept = threshold, linetype = "dotted", color = "red") + 
  geom_vline(xintercept = c(min(baseline$baseline_end)), linetype = "dotted", color = "blue") +
  geom_vline(xintercept = c(max(baseline$baseline_end)), linetype = "dotted", color = "blue") +
  facet_grid(. ~ target_name) +
  theme_classic() +
  theme(legend.position = "none")

p2 <- ggplot(Rn, aes(x = cycle, y = delta_rn, color = well)) +
  geom_line() +
  geom_hline(yintercept = threshold, linetype = "dotted", color = "red") + 
  geom_vline(xintercept = c(min(baseline$baseline_end)), linetype = "dotted", color = "blue") +
  geom_vline(xintercept = c(max(baseline$baseline_end)), linetype = "dotted", color = "blue") +
  facet_grid(. ~ target_name) +
  scale_y_log10() +
  theme_classic() +
  theme(legend.position = "none")

p1 / p2

# negative controls
Rn %>%
  filter(task == "NTC") %>%
  ggplot(aes(x = cycle, y = delta_rn, color = well)) +
  geom_line() +
  geom_hline(yintercept = threshold, linetype = "dotted", color = "red") + 
  geom_vline(xintercept = c(min(baseline$baseline_end)), linetype = "dotted", color = "blue") +
  geom_vline(xintercept = c(max(baseline$baseline_end)), linetype = "dotted", color = "blue") +
  scale_y_log10() +
  theme_classic() +
  theme(legend.position = "bottom")


## Fit standard curve ----

# filter standards
Ct_standard <- Ct %>%
  filter(task == "STANDARD") 

# plot calibration
ggplot(Ct_standard, aes(x = log10(quantity), y = c)) +
  geom_hline(yintercept = c(8, 35), linetype = "dotted", color = "red") +
  geom_smooth(method = "lm", color = "black", alpha = .2) +
  geom_point(shape = 21, fill = "darkorange", size = 2) +
  facet_grid(. ~ target_name) +
  labs(x = "log(quantity)", y = "Cycle threshold (Ct)") +
  theme_classic()

# compare mean & std
kable(
  Ct %>%
    filter(task == "STANDARD") %>%
    group_by(target_name, quantity) %>%
    summarize(mean_Ct = mean(c),
              sd_Ct = sd(c)) %>%
    arrange(desc(quantity))
)




## Calculate relative concentrations for each sample ----

# calculate concentration
Ct <- Ct %>%
  mutate(rel_conc = 10^((c-score_model$coefficients[["(Intercept)"]])/score_model$coefficients[["log10(quantity)"]]))

# compare values
kable(
  Ct %>%
    filter(task == "UNKNOWN") %>%
    mutate(rel_conc = rel_conc*1000) %>%
    select(-task, -quantity) %>%
    arrange(sample_name),
  digits = 2
)





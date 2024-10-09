

# read libraries
library(readr)
library(janitor)
library(lubridate)
library(tidyr)
library(dplyr)
library(ggplot2)
library(patchwork)
library(knitr)

## WADEABLE STREAMS ASSESSMENT -------------------------------------------------

# read in wadeable streams data set
wadeable <- read_delim("data/epa_water-chemistry_wadeable-streams.txt",
                       delim = "\t") %>%
  clean_names()

# check data set
head(wadeable) %>%
  kable()

# Nitrogen ----

# summarize Nitrogen by epa region
nitrogen <- wadeable %>%
  group_by(eparegion) %>%
  summarize(mean_N = mean(ntl),
            median_N = median(ntl),
            sd_N = sd(ntl))

# create formatted table
kable(
  nitrogen,
  digits = 2
)

# compare distributions (logarithmic scale on y-axis)
ggplot(wadeable, aes(x = eparegion, y = ntl)) +
  geom_boxplot() +
  scale_y_log10() +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


# Phosphorus ----



# macro data set comparison ----

# add column values converted to logarithm
nutrients <- wadeable %>%
  mutate(log_N = log10(ntl),
         log_P = log10(ptl))


# Plot relationship macros and Nitrogen
ggplot(nutrients, aes(x = log_N, y = mmi_wsabest)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_classic()

# Plot relationship macros and phosphorus



## CHEMICAL/PHYSICAL PARAMETERS HEADWATERS -------------------------------------

# read data
headwaters <- read_delim("data/environmental-param.tsv",
                         delim = "\t") %>%
  clean_names()

# check data
head(headwaters)


# calculate summary statistics and plot to formatted table
headwaters %>%
  group_by(stream_name) %>%
  summarize(mean_TDS = mean(tds_ppm, na.rm = TRUE),
            sd_TDS = sd(tds_ppm, na.rm = TRUE)) %>%
  arrange(desc(mean_TDS)) %>%
  kable(digits = 2)

# plot distribution of total dissolved solids (tds)
ggplot(headwaters, aes(x = stream_name, y = tds_ppm)) +
  geom_boxplot(outlier.shape = NULL) +
  geom_jitter() +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# comparisons of other values


## STREAMS AND RIERS ASSESSMENT 2019 -------------------------------------------

# read data
rivers <- read_delim("data/epa_water-chemistry_streams-rivers_2019.csv",
                     delim = ",") %>%
  clean_names()

# column names
colnames(rivers)

# compare distributions to help interpret our headwater streams


# load libraries
library(readr)
library(janitor)
library(lubridate)
library(tidyr)
library(dplyr)
library(ggplot2)
library(patchwork)
library(knitr)


# Discharge & Precipitation Piscataquog ---------------------------------------

## Piscataquog discharge ----

# read data set
discharge <- read_delim("data/piscat_365.txt",
                        skip = 27,
                        delim = "\t",
                        col_names = c("agency", "site_no", "date", "discharge_cfs", "flag"))

# check data
head(discharge) %>%
  kable()


# plot as line graph
ggplot(discharge, aes(x = date, y = discharge_cfs)) +
  geom_line() +
  geom_point() +
  labs(x = "Date", y = "Daily Mean Discarge (cubic feet per second)") +
  theme_classic()


## Precipitation data ----

# read data
precip <- read_delim("data/man_precipitation.csv",
                     delim = ",") %>%
  clean_names()

# check data
head(precip) %>%
  kable()

ggplot(precip, aes(x = date, y = prcp)) +
  geom_bar(stat = "identity", color = "darkblue") +
  labs(x = "date", y = "daily total precipitation") +
  theme_classic()


## Create combined precipitation & discharge plot using patchwork

# create plots as objects
p1 <- ggplot(precip, aes(x = date, y = prcp)) +
  geom_bar(stat = "identity", color = "darkblue") +
  labs(x = "", y = "daily total precipitation") +
  theme_classic()

p2 <- ggplot(discharge, aes(x = date, y = discharge_cfs)) +
  geom_line() +
  geom_point() +
  labs(x = "Date", y = "daily mean discarge [cfs]") +
  theme_classic()

# combine figures under each other
p1 / p2


# Water level Headwaters streams -----------------------------------------------

## Precipitation data 2021-2024 ----

# read data set 
precip_2021 <- read_delim("data/precipitat_2021-2024.csv", delim = ",") %>%
  clean_names()

# plot precipitation
ggplot(precip_2021, aes(x = date, y = prcp)) +
  geom_bar(stat = "identity", color = "#3E2F5B", fill = "#3E2F5B") +
  labs(x = "date", y = "mean daily precipitation") +
  theme_classic()


## Rand Brook ----

# wrangle data set
rab <- read_delim("data/RAB_water-level.tsv", delim = "\t") %>% # read data set
  mutate(date = as_date(date_time)) %>%                         # extract date
  group_by(date) %>%                                            # group by date
  summarize(mean_daily_level_cm = mean(water_level_cm)) %>%     # calculate daily mean
  mutate(yday = yday(date),                                     # determine day of year
         year = as.character(year(date)))                       # make year qualitative


# plot water level changes for each year
ggplot(rab, aes(x = yday, y = mean_daily_level_cm, color = year)) +
  geom_line() +
  geom_point(size = 0.5) +
  scale_color_manual(values = c("#3E2F5B", "#136F63", "#E0CA3C", "#F34213")) +
  labs(x = "day of the year", y = "daily mean water level [cm]") +
  theme_classic() + 
  theme(legend.position = "bottom")

# plot water level changes (each year in individual panel)
ggplot(rab, aes(x = yday, y = mean_daily_level_cm)) +
  geom_line(color = "#136F63") +
  geom_point(size = 0.5, color = "#136F63") +
  facet_grid(. ~ year) +
  labs(x = "date", y = "daily mean water level [cm]") +
  theme_classic() + 
  theme(legend.position = "bottom")

# plot water level across entire time period
ggplot(rab, aes(x = date, y = mean_daily_level_cm)) +
  geom_line(color = "#136F63") +
  geom_point(size = 0.5, color = "#136F63") + 
  labs(x = "date", y = "daily mean water level [cm]") +
  theme_classic() + 
  theme(legend.position = "bottom")


## Whiting Brook





## Brennan Brook




## Schoolhouse Brook




## Avery Brook




# Changes in discharge over time ----

## Determine patterns of monthly discharge ----

# read monthly discharge
monthly <- read_delim("data/Piscat_monthly.txt",
                      skip = 37,
                      col_names = c("agency", "site_no", "parameter", "ts_id",
                                    "year", "month", "mean_monthly")) %>%
  select(year, month, mean_monthly) %>%
  mutate(month = month(month, label = TRUE))

# check data
head(monthly) %>%
  kable()

# plot monthly discharge by month
ggplot(monthly, aes(x = year, y = mean_monthly, color = month)) +
  stat_smooth(method = "lm", se = FALSE) +
  geom_point() +
  labs(x = "month", y = "mean monthly discharge") +
  theme_classic() +
  theme(legend.position = "bottom")


## Comparison of "winter discharge" (Feb) ----

# subset February
feb <- monthly %>%
  filter(month == "Feb")

# plot Feb data
ggplot(feb, aes(x = year, y = mean_monthly)) +
  stat_smooth(method = "lm") +
  geom_point() +
  labs(x = "month", y = "mean discharge for February") +
  theme_classic() +
  theme(legend.position = "bottom")


# subset August
aug <- monthly %>%
  filter(month == "Aug")

# plot August Data
ggplot(aug, aes(x = year, y = mean_monthly)) +
  stat_smooth(method = "lm") +
  geom_point() +
  labs(x = "month", y = "mean discharge for August") +
  theme_classic() +
  theme(legend.position = "bottom")


# linear regression Feb
lm(mean_monthly ~ year, data = feb) %>%
  summary()

# linear regression Aug
lm(mean_monthly ~ year, data = aug) %>%
  summary()
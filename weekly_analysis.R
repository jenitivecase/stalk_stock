#Sourcing a standard set of options I maintain to make my life easier.
source("https://raw.githubusercontent.com/jenitivecase/Settings/master/options.R")

sapply(c("gsheet", "tidyverse", "DT", "knitr"), load_packages)

stalks <- as.data.frame(gsheet2tbl("docs.google.com/spreadsheets/d/1SVxFEGjCGhj3q6NQB1cJNf5CLeFTo9giG79HdOqJXgo/",
                                   sheetid = "selling")) %>%
  filter(!is.na(Date))


stalks <- stalks %>%
  rename("AMPM" = `AM/PM`) %>%
  mutate(AMPM = factor(AMPM, levels = c("AM", "PM"))) %>%
  mutate(Date = as.Date(Date, format = "%m/%d/%Y")) %>%
  mutate(Day = lubridate::wday(Date, label = TRUE, abbr = TRUE), 
         Week = lubridate::year(Date) + lubridate::isoweek(Date) - 2037)

# saveRDS(stalks, "./stalk_stock/data_through_20200515.rds")

##### GENERAL PROJECT STAGES ###################################################
#
# 1. MVP: hand-coded algorithm to identify pattern. >= 3 data points required.
# 2. loess regression lines?
# 3. time series analysis
# 4. ML time series analysis
#
##### QUEsTIONS ################################################################
#
# - how to handle missing data?
# - are there really four patterns?
# - are the four pattern labels put forth by the internet really real?
#
################################################################################


day_label_set <- stalks %>%
  filter(Week == 1) %>%
  mutate(Date = as.Date(Date, format = "%m/%d/%Y")) %>%
  gather(key = "person", value = "sell_price", Brianna, Jack, Jen, Kelly) %>%
  group_by(person, Week) %>%
  arrange(Week, person, Date, AMPM) %>%
  mutate(obs_index = paste0(Day, "_", AMPM)) %>%
  pull(obs_index) %>%
  unique()

long_stalks <- stalks %>%
  mutate(Date = as.Date(Date, format = "%m/%d/%Y")) %>%
  mutate(time = case_when(AMPM == "AM" ~ "09:00",
                          AMPM == "PM"~ "15:00", 
                          TRUE ~ as.character(NA))) %>%
  mutate(datetime = as.POSIXct(paste0(Date, " ", time))) %>%
  select(-time) %>%
  mutate(Week = paste0("Week ", Week)) %>%
  gather(key = "person", value = "sell_price", Brianna, Jack, Jen, Kelly) %>%
  group_by(person, Week) %>%
  arrange(Week, person, Date, AMPM) %>%
  # mutate(obs_index = paste0("obs_", sprintf("%02d", row_number()))) %>%
  mutate(obs_index = paste0(Day, "_", AMPM)) %>%
  mutate(obs_index = factor(obs_index, 
                            levels = day_label_set,
                            labels = day_label_set))

wide_stalks <- long_stalks %>%
  select(-Date, -Day, -AMPM, -datetime) %>%
  spread(key = obs_index, value = sell_price)
  


ggplot(long_stalks) +
  facet_grid(Week ~ ., switch = "y") + 
  geom_point(aes(x = factor(obs_index, labels = gsub("_", "\n", day_label_set)), 
                y = sell_price, group = person, color = person), size = 1) +
  geom_line(aes(x = factor(obs_index, labels = gsub("_", "\n", day_label_set)), 
                y = sell_price, group = person, color = person), size = 1) +
  labs(title = "Stalk Market Patterns", 
       x = "Time",
       y = "Sell Price",
       color = "Person")

##### AN ATTEMPT AT WRITING DECISION RULES #####################################
# - if first value > 100, not spike pattern
# - if first two values decrease but the decrease <= 10, small spike
# ? if first two values decrease but the decrease >= 10, random
# - if value >= 150 at observation 5 or 6 (Wed), large spike
#
##### WHEN TO SELL #############################################################
# - either spike: 
# - random: ???
# - decreasing: ASAP
# - decrease then spike?: obs 1 or on first large increase (obs 10/Fri PM?)
#
################################################################################
library(dplyr)
library(readr)
library(stringr)
library(lubridate)

raw.data <- read.csv('data/gpu_raw_data.csv')

# Chipset Manufacturer
raw.data <- raw.data %>% filter(Chipset.Manufacturer == "AMD" | Chipset.Manufacturer == "ATI" | 
                                  Chipset.Manufacturer == "NVIDIA" | is.na(Chipset.Manufacturer)) # 356 -> 355

# Clock Speeds
raw.data <- raw.data %>% mutate(tmp=regmatches(Core.Clock, gregexpr("[[:digit:]]+", Core.Clock)),
                                Core.Clock=as.numeric(lapply(tmp, min)),
                                OC.Core.Clock=as.numeric(lapply(tmp, function(x) ifelse(length(x) > 1, max(x), NA)))) %>% 
  select(-tmp) %>% select(Page:Core.Clock, OC.Core.Clock, Boost.Clock:NumberOfRatings)


raw.data <- raw.data %>% mutate(tmp=regmatches(Boost.Clock, gregexpr("[[:digit:]]+", Boost.Clock)),
                                Boost.Clock=as.numeric(lapply(tmp, min)),
                                OC.Boost.Clock=as.numeric(lapply(tmp, function(x) ifelse(length(x) > 1, max(x), NA)))) %>% 
  select(-tmp) %>% select(Page:Boost.Clock, OC.Boost.Clock, Memory.Size:NumberOfRatings)

# Memory
raw.data <- raw.data %>% filter(grepl('GB', Memory.Size) | is.na(Memory.Size)) %>% 
  mutate(Memory.Size=as.numeric(gsub('GB','', Memory.Size))) # 355 -> 351

# Month/Year First Available
raw.data <- raw.data %>% mutate(Date.First.Available=as.Date(raw.data$Date.First.Available, format = "%B %d, %Y"),
                                Month=month(Date.First.Available),
                                Year=year(Date.First.Available)) %>% select(-Date.First.Available) %>%
  select(Page:Memory.Type, Month, Year, Price:NumberOfRatings)

# Price
raw.data <- raw.data %>% mutate(Price=as.numeric(lapply(Price, function(x) gsub("\\$", "", 
                                                                                unlist(str_split(x, '\\s+'))[1]))))
# Current Savings
raw.data <- raw.data %>% mutate(tmp=gsub("%", "", regmatches(CurrentSavings, gregexpr("[0-9]*%", CurrentSavings))),
                                CurrentSavings=as.numeric(lapply(tmp, function(x) ifelse(rlang::is_empty(x), NA, x)))) %>% select(-tmp)

# Shipping
raw.data <- raw.data %>% mutate(Shipping=recode(Shipping, "Free Shipping" = "0"),
                                tmp=ifelse(Shipping != "0", regmatches(Shipping, gregexpr("\\$[0-9]*.[0-9]*", Shipping)), Shipping),
                                Shipping=as.numeric(gsub("\\$", "", tmp))) %>% select(-tmp)

# Average Ratings
raw.data <- raw.data %>% mutate(AvgRating=as.numeric(lapply(AvgRating, function(x) unlist(regmatches(x, gregexpr("[0-9]", x)))[1])))

# Number of Ratings
raw.data <- raw.data %>% mutate(NumberOfRatings=as.numeric(gsub("\\(|\\)", "", NumberOfRatings)))

# Keep observations with less than 30% of data missing
raw.data <- raw.data %>% mutate(na.percent=round(rowSums(is.na(.))/ncol(.), 4)*100) %>% 
  filter(na.percent < 30) %>% select(-na.percent) # 351 -> 288

# Save final data set
write.csv(raw.data, 'data\\gpu_cleaned_data.csv', row.names = FALSE)

                      
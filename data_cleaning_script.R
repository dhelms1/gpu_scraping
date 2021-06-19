library(dplyr)
library(readr)
library(stringr)
library(lubridate)

raw.data <- read.csv('data/gpu_raw_data.csv')

raw.data <- raw.data %>% mutate(na.percent=round(rowSums(is.na(.))/ncol(.), 4)*100) %>% 
  filter(na.percent < 40) %>% select(-na.percent) # 356 -> 314

# Chipset Manufacturer
raw.data <- raw.data %>% filter(Chipset.Manufacturer == "AMD" | Chipset.Manufacturer == "ATI" | 
                                  Chipset.Manufacturer == "NVIDIA" | is.na(Chipset.Manufacturer))

# Clock Speeds (FIX THIS, ISSUE WITH MIN/MAX FILLING BOTH REG AND OC CLOCKS)
raw.data <- raw.data %>% mutate(tmp=regmatches(Core.Clock, gregexpr("[[:digit:]]+", Core.Clock)),
                                Core.Clock=as.numeric(lapply(tmp, min)),
                                OC.Core.Clock=as.numeric(lapply(tmp, max))) %>% 
  select(-tmp) %>% select(Page:Core.Clock, OC.Core.Clock, Boost.Clock:NumberOfRatings)


raw.data <- raw.data %>% mutate(tmp=regmatches(Boost.Clock, gregexpr("[[:digit:]]+", Boost.Clock)),
                                Boost.Clock=as.numeric(lapply(tmp, min)),
                                OC.Boost.Clock=as.numeric(lapply(tmp, max))) %>% select(-tmp) %>%
  select(Page:Boost.Clock, OC.Boost.Clock, Memory.Size:NumberOfRatings)

# Memory
raw.data <- raw.data %>% filter(grepl('GB', Memory.Size)) %>% 
  mutate(Memory.Size=as.numeric(gsub('GB','', Memory.Size)))

# Month/Year First Available




                      
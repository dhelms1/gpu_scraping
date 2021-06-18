library(dplyr)

raw.data <- read.csv('data/gpu_raw_data.csv')

raw.data <- raw.data %>% mutate(na.percent=round(rowSums(is.na(.))/ncol(.), 4)*100) %>% filter(na.percent < 40) # 356 -> 314

# Core Clock
tmp <- data.frame(Core.Clock = raw.data$Core.Clock)

library(ggplot2)
library(dplyr)
library(sjPlot)

set_theme(
  base = theme_sjplot(),
  axis.title.size = .9,
  axis.textsize = .9,
  legend.size = .7,
  legend.title.size = .8,
  geom.label.size = 4
)

data <- read.csv('data/gpu_cleaned_data.csv')

######################################################## BASIC EXPLORATION ########################################################

# Brand (P1a)
plot_frq(data$Brand, sort.frq="desc", show.prc=FALSE, geom.colors='#09C589', title='Brands') + xlab('') + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
ggsave("figures/Brand.png", width=5, height=5, units="in")

# Chipset (P1b)
plot_frq(data$Chipset.Manufacturer, sort.frq="desc", show.prc=TRUE, geom.colors='#C50909', title='Chipset Manufacturers') + xlab('')
ggsave("figures/Chipset.png", width=4, height=5, units="in")

# Looking at the most common GPU Brands we can see that MSI, ASUS, GIGABYTE, and EVGA are the clear leaders. This is to be expected
# if you've ever shopped for GPUs before. There are some other brands with small values, but this could also be due to the scripts
# reading in the incorrect data (i.e. NVIDIA is the most common manufacturer but isn't exactly a "brand"). Now looking at the Chipset
# Manufacturing graph we can clearly see that NVIDIA is dominant in the data set, producing around 80% of the chipsets for these GPUs. 
# AMD also seems to be producing a good amount, and ATI seems to be extremely rare.

# > *NOTE*: ATI was acquired by AMD in 2006 so we could technically recode ATI back into AMD, but I'll leave it how it is. 



# GPU Series (P2a)
series <- unlist(data %>% count(GPU.Series, sort=TRUE) %>% filter(n > 1) %>% select(GPU.Series))
plot_frq(subset(data, GPU.Series %in% series)$GPU.Series, sort.frq="desc", show.prc=FALSE, geom.colors='#0995C5', title='GPU Series (> 1 Occurance)') + 
  xlab('') + theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
rm(series)
ggsave("figures/GPUSeries.png", width=5, height=5, units="in")

# GPU (P2b)
gpu <- unlist(data %>% count(GPU, sort=TRUE) %>% select(GPU, n) %>% slice_max(n, n=10) %>% select(GPU))
plot_frq(subset(data, GPU %in% gpu)$GPU, sort.frq="desc", show.prc=FALSE, geom.colors='#A909C5', title='GPUs (Top 11 Occuring)') + 
  xlab('') + theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
rm(gpu)
ggsave("figures/GPUs.png", width=5, height=5, units="in")

# Looking at the GPU Series, to no surprise we can see that NVIDIA has the most common series. The GTX 10 Series seems to be dominant, 
# followed by the GTX 16 Series, and followed again by the RTX 20 Series. The AMD Radeon RX 6000 and 500 Series are the most popular AMD
# GPUs (shout out to the RX 580 that's what I currently have), but besides that there are no clear occurrences in any other GPU Series.
# Now, looking at the most common GPUs in the data set we can see that it follows what our Series graph had found. The GTX 1650/1060 are
# the most common, followed by the RTX 2060, and again followed by the Radeon RX 6700 XT. These are only the top 11 of the total 55
# unique GPUs in the data set. It seems NVIDIA is dominant, and with its new releases I can understand why.



# Core Clock (P3a)
data <- data %>% filter(Core.Clock > 50 | is.na(Core.Clock)) # removes 2 observation (288 -> 286)
plot_frq(data$Core.Clock, type='histogram', show.mean=TRUE, xlim=c(0,2700), geom.colors='#09BFC5', title='Core Clock (MHz)') + xlab('')
ggsave("figures/CoreClock.png", width=5, height=5, units="in")

# OC Core Clock (P3b)
plot_frq(data$OC.Core.Clock, type='histogram', show.mean=TRUE, xlim=c(800,1900), geom.colors='#094BC5', title='OC Core Clock (MHz)') + xlab('')
ggsave("figures/OCCoreClock.png", width=5, height=5, units="in")

# 



# Boost Clock (P4a)
plot_frq(data$Boost.Clock, type='histogram', show.mean=TRUE, xlim=c(1000,5100), geom.colors='#C5096D', title='Boost Clock (MHz)') + xlab('')
ggsave("figures/BoostClock.png", width=5, height=5, units="in")

# OC Boost Clock (P4b)
plot_frq(data$OC.Boost.Clock, type='histogram', show.mean=TRUE, xlim=c(1000,2800), geom.colors='#C509B7', title='OC Boost Clock (MHz)') + xlab('')
ggsave("figures/OCBoostClock.png", width=5, height=5, units="in")

# I know what you're thinking, I probably messed up with that 5000 MHz boost clock speed right? Nope. I went to the URL and checked, and it 
# indeed does go from a 1250 MHz Core Clock up to a 5000 MHz Boost Clock (not even overclocked). The trade-off is 2 GB of memory and Windows 7
# is the only compatible operating system. 



# Memory Size (P5a)


# Memory Interface (P5b)


# Memory Type (P5c)

#



# Price (P6a)


# Shipping (P6b)

#



# Average Rating (P7a)


# Number of Ratings (P7b)

# 





###################################################### ADVANCE EXPLORATION ######################################################







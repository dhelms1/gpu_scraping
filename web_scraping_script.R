library(rvest)
library(dplyr)
library(janitor)
library(tibble)

`%notin%` <- Negate(`%in%`) # create negation function to test if column not in data frame

get_model_info <- function(gpu_link) {
  # Pull information from the "Specs" tab
  cols <- c('Brand', 'Model', 'Chipset Manufacturer', 'GPU Series', 'GPU', 'Core Clock', 'Boost Clock', 
            'Memory Size', 'Memory Interface', 'Memory Type', 'Date First Available')
  model_info <- read_html(gpu_link) %>% html_nodes("table , #product-mini-feature .tab-nav:nth-child(2)") %>% html_table(fill=TRUE)
  info_df <- data.frame(tmp=t(do.call(rbind, model_info))) %>% row_to_names(row_number = 1)
  for (cname in cols) {
    if (cname %notin% colnames(info_df)) {
      info_df[cname] <- rep(NA, nrow(info_df))
    }
  }
  info_df[cols]
}


get_gpu_ratings <- function(gpu_link) {
  data.frame('AvgRating' = substring(unlist(strsplit(as.character(read_html(gpu_link) %>% html_nodes(".product-rating")),
                                                     "rating rating"))[2], 10, 30),
             'NumberOfRatings' = read_html(gpu_link) %>% html_nodes(".product-rating .item-rating-num") %>% html_text() %>% .[1])
}


get_gpu_pricing <- function(page_link) {
  page <- read_html(page_link)
  tmp_df <- data.frame('OriginalPrice' = page %>% html_nodes('.price-current') %>% html_text())
  tmp_df$Savings <- page %>% html_nodes('.price-save') %>% html_text()
  sale_prices <- page %>% html_nodes('.price-was-data') %>% html_text()
  price_idx <- 1
  tmp_df$CurrentPrice <- rep(NA, nrow(tmp_df))
  for (i in 1:nrow(tmp_df)) {
    if (tmp_df$Savings[i] == "") {
      tmp_df$CurrentPrice[i] <- tmp_df$OriginalPrice[i]
    }
    else {
      tmp_df$CurrentPrice[i] <- sale_prices[price_idx]
      price_idx <- price_idx + 1
    }
  }
  tmp_df$Shipping <- page %>% html_nodes('.price-ship') %>% html_text()
  tmp_df
}

# Create empty data frame for GPU model information
gpu_df <- data.frame('Brand' = character(),
                     'Model' = character(),
                     'Chipset Manufacturer' = character(),
                     'GPU Series' = character(),
                     'GPU' = character(),
                     'Core Clock' = character(),
                     'Boost Clock' = character(),
                     'Memory Size' = character(),
                     'Memory Interface' = character(),
                     'Memory Type' = character(),
                     'Date First Available' = character())

# Create empty data frame for GPU pricing information
rating_df <- data.frame('AvgRating' = character(),
                        'NumberOfRatings' = character())

# Create empty data frame for GPU pricing information
pricing_df <- data.frame('OriginalPrice' = character(),
                         'Savings' = character(),
                         'CurrentPrice' = character(),
                         'Shipping' = character())

# Run scraper over given number of pages (36 results per page for 30 pages = 1080 observations)
# I broke up the page into groups of 5 which I will run every few hours so that I'm not 
# creating too many requests at once (I think this should be enough spacing)
page_1_to_5 <- 1:5
page_6_to_10 <- 6:10
page_11_to_15 <- 11:15
page_16_to_20 <- 16:20
page_21_to_25 <- 21:25
page_26_to_30 <- 26:30

for (page_num in page_1_to_5) {
  link <- paste0("https://www.newegg.com/Desktop-Graphics-Cards/SubCategory/ID-48/Page-", page_num, "?Tid=7709")
  gpu_links <- read_html(link) %>% html_nodes("a.item-title") %>% html_attr("href")
  pricing_df <- rbind(get_gpu_pricing(link), pricing_df)
  gpu_df <- rbind(t(sapply(gpu_links, FUN = get_model_info, USE.NAMES = FALSE)), gpu_df)
  rating_df <- rbind(t(sapply(gpu_links, FUN = get_gpu_ratings, USE.NAMES = FALSE)), rating_df)
}


final_df <- cbind(gpu_df, pricing_df, rating_df) # combine all into final DF with 17 features and 1080 observations

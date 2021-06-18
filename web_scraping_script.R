library(rvest)
library(dplyr)
library(janitor)
library(tibble)
library(naniar)

`%notin%` <- Negate(`%in%`) # create negation function to test if column not in data frame

get_model_info <- function(gpu_link) {
  # Pull information from the "Specs" tab
  print(gpu_link)
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
  tmp_df <- data.frame('Price' = page %>% html_nodes('.price-current') %>% html_text())
  tmp_df$CurrentSavings <- page %>% html_nodes('.price-save') %>% html_text()
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
pricing_df <- data.frame('Price' = character(),
                         'CurrentSavings' = character(),
                         'Shipping' = character())

# Run scraper over given page number
page_num <- 10
link <- paste0("https://www.newegg.com/Desktop-Graphics-Cards/SubCategory/ID-48/Page-", page_num, "?Tid=7709")
gpu_links <- read_html(link) %>% html_nodes("a.item-title") %>% html_attr("href")
pricing_df <- rbind(get_gpu_pricing(link), pricing_df)
gpu_df <- rbind(t(sapply(gpu_links, FUN = get_model_info, USE.NAMES = FALSE)), gpu_df)
rating_df <- rbind(t(sapply(gpu_links, FUN = get_gpu_ratings, USE.NAMES = FALSE)), rating_df)
page_df <- data.frame(Page=rep(page_num, length(gpu_links))) # create page number column
link_df <- data.frame(URL=gpu_links) # create column for original gpu links

# Combine all into final DF with 18 features. Change all columns to character (issue with writing since
# some columns were "list" class). NA's were replaced with "NA", so reverse this with naniar function
current_final_df <- read.csv('data\\gpu_raw_data.csv')
new_data_df <- cbind(page_df, link_df, gpu_df, pricing_df, rating_df)
new_data_df <- new_data_df %>% mutate(across(everything(), as.character)) %>% replace_with_na_all(condition = ~.x == 'NA')
if (exists('current_final_df')) {
  colnames(new_data_df) <- colnames(current_final_df)
  final_df <- rbind(current_final_df, new_data_df)
}

# Make sure data is in correct format before overwriting previous final data
write.csv(final_df, 'data\\gpu_raw_data.csv', row.names = FALSE)

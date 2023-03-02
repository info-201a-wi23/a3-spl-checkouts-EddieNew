# # load necessary libraries
library(dplyr)
library(stringr)
library(tidyverse)

# # load dataframe
spl_data <- read.csv("~/Documents/Info201Code/a3-spl-checkouts-EddieNew/Checkouts_by_Title.csv", stringsAsFactors = FALSE, na.strings=c("","NA"))
View(spl_data)

creator_missing_data <- spl_data %>%
  filter(is.na(Creator))

unlabled_books <- creator_missing_data %>%
  filter(MaterialType == "BOOK")

unlabled_ebooks <- creator_missing_data %>%
  filter(MaterialType == "EBOOK")

unlabled_audiobooks <- creator_missing_data %>%
  filter(MaterialType == "AUDIOBOOK")

total_unlabled <- nrow(creator_missing_data)
book_unlabled_count <- nrow(unlabled_books)
ebook_unlabled_count <- nrow(unlabled_ebooks)
audiobook_unlabled_count <- nrow(unlabled_audiobooks)

total_credited <- nrow(spl_data) - total_unlabled

most_popular <- spl_data %>%
  filter(MaterialType == "BOOK" |
           MaterialType == "EBOOK" |
           MaterialType == "AUDIOBOOK") %>%
  group_by(Title) %>%
  summarize(Checkouts = sum(Checkouts)) %>%  
  filter(Title != "<Unknown Title>") %>%
  filter(!grepl("Uncataloged", Title, fixed = TRUE))

most_popular_book <- most_popular %>%
  arrange(-Checkouts) %>%
  slice(1) %>%
  pull(Title)

most_popular_author <- spl_data %>%
  filter(Title == most_popular_book) %>%
  na.omit() %>%
  slice(1) %>%
  pull(Creator)

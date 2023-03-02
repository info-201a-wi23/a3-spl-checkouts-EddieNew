# # load necessary libraries
library(dplyr)
library(ggplot2)
library("scales")
library(stringr)
library(tidyverse)
library(RColorBrewer)

# # load dataframe
spl_data <- read.csv("~/Documents/Info201Code/a3-spl-checkouts-EddieNew/Checkouts_by_Title.csv", stringsAsFactors = FALSE)

alt_book_data <- spl_data %>%
  filter(MaterialType == "EBOOK" |
           MaterialType == "AUDIOBOOK")

alt_author_data <- alt_book_data %>%
  group_by(Creator) %>%
  summarize(total_checkouts = sum(Checkouts)) %>%
  na.omit()

top_five_alt_authors <- alt_author_data %>%
  arrange(desc(total_checkouts)) %>% 
  slice(1:5)

top_five_over_time_alt <- spl_data %>%
  filter(Creator %in% top_five_alt_authors$Creator) %>%
  group_by(CheckoutYear, Creator) %>%
  summarize(Checkouts = sum(Checkouts)) %>% 
  filter(CheckoutYear != 2023)

ggplot(top_five_over_time_alt) +
  geom_line(aes(x = CheckoutYear, y = Checkouts, color = Creator)) +
  scale_color_brewer(palette = "Set2") +
  labs(title = "The Five Most Checked-Out eBook and Audiobook Creators (SPL 2005 - 2022)", x = "Year", y = "Number of Times Their Work Was Checked Out") +
  scale_x_continuous(breaks = seq(2004, 2022, 2))

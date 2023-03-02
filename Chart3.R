# # load necessary libraries
library(dplyr)
library(ggplot2)
library("scales")
library(stringr)
library(tidyverse)
library(RColorBrewer)

# # load dataframe
spl_data <- read.csv("~/Documents/Info201Code/a3-spl-checkouts-EddieNew/Checkouts_by_Title.csv", stringsAsFactors = FALSE)

book_data <- spl_data %>%
  filter(MaterialType == "BOOK")

author_data <- book_data %>%
  group_by(Creator) %>%
  summarize(total_checkouts = sum(Checkouts)) %>%
  na.omit()

top_five_authors <- author_data %>%
  arrange(desc(total_checkouts)) %>% 
  slice(2:6)

top_five_authors <- top_five_authors %>%
  na.omit()

top_five_authors_books <- spl_data %>%
  filter(Creator %in% top_five_authors$Creator) %>%
  select(c(Title, Creator, Subjects))

top_five_authors_books$Title <- gsub("\\s*\\([^\\)]+\\)", "", top_five_authors_books$Title)

top_five_authors_books$Creator <- gsub("1.*", "", top_five_authors_books$Creator)
top_five_authors_books$Creator <- gsub("([^ ]*),\\s([^ ]*)( \\w+)?", "\\2\\3 \\1", top_five_authors_books$Creator)
top_five_authors_books$Creator <- gsub(",", "", top_five_authors_books$Creator)

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

top_five_alt_authors_works <- spl_data %>%
  filter(Creator %in% top_five_alt_authors$Creator) %>%
  select(c(Title, Creator, Subjects))

top_five_alt_authors_works$Title <- gsub("\\s*\\([^\\)]+\\)", "", top_five_alt_authors_works$Title)

ten_greats <- rbind(top_five_authors_books, top_five_alt_authors_works)

ten_greats <- ten_greats %>%
  mutate(broadSubject = gsub(",.*$", "", Subjects)) %>%
  distinct(Title, broadSubject, .keep_all = TRUE)

#
# PattersonBookCount <- sum(ten_greats$Creator == "James Patterson")
# SeussBookCount <- sum(ten_greats$Creator == "Dr. Seuss")
# WillemsBookCount <- sum(ten_greats$Creator == "Mo Willems")
# OsborneBookCount <- sum(ten_greats$Creator == "Mary Pope Osborne")
# ChildBookCount <- sum(ten_greats$Creator == "Lee Child")
# MeadowsBookCount <- sum(ten_greats$Creator == "Daisy Meadows")
# ConnellyBookCount <- sum(ten_greats$Creator == "Michael Connelly")
# GrishamBookCount <- sum(ten_greats$Creator == "John Grisham")
# ChristieBookCount <- sum(ten_greats$Creator == "Agatha Christie")
# DavisBookCount <- sum(ten_greats$Creator == "Jim Davis ")
# 
# ten_greats <- ten_greats %>%
#   mutate(bookCount = case_when(
#     Creator == "James Patterson" ~ PattersonBookCount,
#     Creator == "Dr. Seuss" ~ SeussBookCount,
#     Creator == "Mo Willems" ~ WillemsBookCount,
#     Creator == "Mary Pope Osborne" ~ OsborneBookCount,
#     Creator == "Lee Child" ~ ChildBookCount,
#     Creator == "Daisy Meadows" ~ MeadowsBookCount,
#     Creator == "Michael Connelly" ~ ConnellyBookCount,
#     Creator == "John Grisham" ~ GrishamBookCount,
#     Creator == "Agatha Christie" ~ ChristieBookCount,
#     Creator == "Jim Davis " ~ DavisBookCount
#   ))
#

common_genres <- c("Fantasy", "Mystery", "Fairy Tales", "Romance", "True Crime", "Young Adult Fiction", "Science Fiction", "History", "Law", "Classic Literature", "Fiction", "Nature", "Nonfiction", "African American Fiction")

ten_greats$broadSubject <- sapply(strsplit(ten_greats$broadSubject, ',\\s*'), function(x) {
  x[!x %in% common_genres] <- "Other"
  toString(x)
})

ten_greats <- ten_greats %>%
  na.omit()

ggplot(data = ten_greats, aes(x=Creator, y=1, fill = broadSubject)) +
  geom_bar(stat="identity", width=0.75) +
  labs(title = "Subject Classification Breakdown of the Nine Most Checked-Out SPL Creators", x = "Creator", y = "Amount of Unique Works in SPL Database", fill = "Popular Genre Classifications") +
  scale_y_continuous(labels = label_number_si()) + 
  coord_flip()

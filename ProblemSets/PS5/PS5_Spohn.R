library(rvest)
library(dplyr)
library(fredr)

# web scraping

url <- "https://www.baseball-reference.com/leagues/majors/2023-standard-pitching.shtml"


webpage <- read_html(url)


css_selectors <- c(
  "#teams_standard_pitching tr+ tr .left",
  "#teams_standard_pitching a",
  "#teams_standard_pitching tbody .right:nth-child(23)",
  "#teams_standard_pitching .right.poptip",
  "#teams_standard_pitching tbody .right:nth-child(19)"
)

# Turning web scraping results into a data frame

# Use CSS selectors to extract information
data_list <- lapply(css_selectors, function(selector) {
  webpage %>% html_nodes(selector) %>% html_text()
})

# Find the maximum length of the lists
max_length <- max(sapply(data_list, length))

# Pad the shorter lists with NAs to make them equal in length
data_list_padded <- lapply(data_list, function(x) c(x, rep(NA, max_length - length(x))))


df <- data.frame(data_list_padded, stringsAsFactors = FALSE)

# some of the colomns had too much data in the colomn, they all had the wrong
# colomn title, and one colomn was twice as long, with every odd number row
# having data that I do not know where it came from. This code fixes that.

df <- df[, -1]
df[31, 2] <- NA

df[, 3] <- df[c(FALSE, TRUE), 3]

# Remove empty rows
df <- df[complete.cases(df), ]

# Reset row names
rownames(df) <- NULL

colnames(df) <- c("Team Names", "SO", "FIP", "ER")

df

# using API for data download

# I am downloading unemployment data for san jose area in California

api_key <- "29e545bf1ccf0314e30a636068b85afb"
fredr_set_key(api_key)


series_code <- "SANJ906URN"
data <- fredr(series_code)

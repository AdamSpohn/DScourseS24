# I used the same data as PS5, so most of the data collection and cleaning is
# no different from PS5, except for small touches and changing API key usage.

install.packages("usethis")
usethis::edit_r_environ()

library(rvest)
library(dplyr)
library(fredr)
library(ggplot2)

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

# Use CSS selectors to extract information
data_list <- lapply(css_selectors, function(selector) {
  webpage %>% html_nodes(selector) %>% html_text()
})

# Find the maximum length of the lists
max_length <- max(sapply(data_list, length))

# Pad the shorter lists with NAs to make them equal in length
data_list_padded <- lapply(data_list, function(x) c(x, rep(NA, max_length - 
                                                             length(x))))


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

#renaming columns
colnames(df) <- c("TeamNames", "SO", "FIP", "ER")

# Adding in a team abbreviation for easier displays on plot
df$TeamAbbreviation <- c(
  "ARI", "ATL", "BAL", "BOS", "CHC", "CWS", "CIN", "CLE",
  "COL", "DET", "HOU", "KC", "LAA", "LAD", "MIA", "MIL",
  "MIN", "NYM", "NYY", "OAK", "PHI", "PIT", "SD", "SF",
  "SEA", "STL", "TB", "TEX", "TOR", "WSH"
)


# Changing data to numerics
df$SO <- as.numeric(as.character(df$SO))
df$ER <- as.numeric(as.character(df$ER))
df$FIP <- as.numeric(as.character(df$FIP))


df

# using API for data download

# I am downloading unemployment data for san jose area in California

# Setting API Key
fredr_set_key(Sys.getenv("FRED_KEY"))


series_code <- "SANJ906URN"
data <- fredr(series_code)


# PROBLEM SET 6 ACTUAL PLOTTING BEGINS HERE

# Line Plot of San Jose CA Unemployment with the included date where chatgpt 
# released
ggplot(data, aes(x = date, y = value)) +
  geom_line() +
  labs(title = "Unemployment Rates in San Jose, CA",
       x = "Year",
       y = "Percent Unemployment Rate") +
  scale_x_date(
    breaks = seq(as.Date("1990-01-01"), max(data$date), by = "5 years"),
    date_labels = "%Y"
  ) +
  scale_y_continuous(limits = c(0, 15), breaks = seq(0, 15, by = 2.5)) +
  theme_minimal() +
  geom_vline(xintercept = as.Date("2022-11-30"), linetype = "dashed", 
             color = "red") +
  geom_text(x = as.Date("2022-11-30"), y = 14, label = "ChatGPT Release Date",
            vjust = 1, hjust = 1.1, color = "red")


# Scatter plots of Strike outs and Earned Runs by team in 2023
 ggplot(df, aes(x = SO, y = ER, color = 
                 ifelse(df$SO >= 1480 & df$ER < 670, "High Performance", 
  ifelse(df$ER > 700 & df$SO < 1350, "Low Performance", "Not Specified")))) +
  geom_point() +
  labs(title = "2023 High and Low Pitching Performance by Team",
       x = "Strikeouts",
       y = "Earned Runs") +
  geom_text(aes(label = ifelse(df$SO >= 1480 & df$ER < 670 | 
                      df$ER > 700 & df$SO < 1350, df$TeamAbbreviation, "")),
            hjust = 1.2, vjust = 0.5, size = 3) +
  scale_x_continuous(breaks = seq(0, max(df$SO), by = 100)) +
  scale_y_continuous(breaks = seq(0, max(df$ER), by = 20)) +
  scale_color_manual(values = c("High Performance" = "red", 
                                "Low Performance" = "blue")) +
  guides(color = guide_legend(title = "Performance"))

# Means of FIP and ER for reference in the plot 
mean_fip <- mean(df$FIP)
mean_er <- mean(df$ER)

# subsets of data for labeling purposes
strong_defense_teams <- df[df$FIP < 4.0 & df$ER < 650, ]
strong_defense_teams$category <- "Strong"
weaker_defense_teams <- df[df$FIP > 4.6 & df$ER > 750, ]
weaker_defense_teams$category <- "Weak"
outshine_teams <- df[df$FIP > mean_fip & df$ER < mean_er, ]
outshine_teams$category <- "Outshine"

# Combine the subsets
combined_subset <- rbind(strong_defense_teams, weaker_defense_teams,
                         outshine_teams)


# Scatter Plot of FIP and ER, where the hope is to measure defensive ability
# of different MLB teams

ggplot(df, aes(x = ER, y = FIP, label = TeamAbbreviation)) +
  geom_point(aes(color = TeamType), size = 3) +
  scale_color_manual(values = c("black", "purple", "red", "blue"),
                     name = "Team Type",
                     labels = c("Average Defense", "Fielding Outshines Pitching", 
                                "Strong Defense", "Weak Defense")) +
  geom_hline(yintercept = mean_fip, color = "black", linetype = "dashed") +
  geom_vline(xintercept = mean_er, color = "black", linetype = "dashed") +
  annotate("text", x = mean_er + 2, y = mean_fip + 0.75,
           label = paste("Mean ER:", round(mean_er, 2)),
           color = "black", size = 3, hjust = 0) +
  annotate("text", x = mean_er + 80, y = mean_fip - 0.02,
           label = paste("Mean FIP:", round(mean_fip, 2)),
           color = "black", size = 3, hjust = 0, angle = -30) +
  labs(title = "2023 Defensive Measures of MLB Teams",
       x = "Earned Runs",
       y = "Fielding Independent Pitching (FIP)") +
  theme_minimal() +
  geom_text(data = subset_strong_defense, aes(label = TeamAbbreviation),
            color = "red", size = 3, hjust = -0.5, vjust = 0.5) +
  geom_text(data = subset_weak_teams, aes(label = TeamAbbreviation), 
            color = "blue", size = 3, hjust = -0.5, vjust = 0.5) +
  geom_text(data = subset_outshine, aes(label = TeamAbbreviation), 
            color = "purple", size = 3, hjust = -0.5, vjust = 0.5) +
  geom_point(data = df[df$TeamAbbreviation == "KC", ], color = "blue", 
             size = 3) +
  geom_text(data = df[df$TeamAbbreviation == "KC", ], aes(label = 
        TeamAbbreviation), color = "blue", size = 3, hjust = -0.5, vjust = 0.5)

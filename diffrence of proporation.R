library(readr)
nobel_data <- read_csv("./nobel_prize_by_winner.csv")

# rename dataset to make it easier to reference
df <- nobel_data

# filter out entries with missing borncountry or category
df <- subset(df, !is.na(borncountry) & !is.na(category))

# identify Top 10 countries by count
country_counts <- sort(table(df$borncountry), decreasing = TRUE)
top_countries <- names(country_counts)[1:10]

# Load necessary libraries
# The course uses readr for CSV reading
if (!require("readr")) install.packages("readr", repos = "http://cran.us.r-project.org")
library(readr)

# Set output directory
output_dir <- "analysis_output"
if (!dir.exists(output_dir)) {
  dir.create(output_dir)
}

# Load data
# In RStudio, ensure your Working Directory is set to the folder containing this script and the CSV file.
if (file.exists("nobel_prize_by_winner.csv")) {
  df <- read_csv("nobel_prize_by_winner.csv")
} else if (file.exists("data/nobel_prize_by_winner.csv")) {
  df <- read_csv("data/nobel_prize_by_winner.csv")
} else {
  stop("Could not find 'nobel_prize_by_winner.csv'. Please make sure the file is in your working directory.")
}

# Standardize column names to lowercase
names(df) <- tolower(names(df))

head(df, 5)
 


# --- Data Cleaning & Preprocessing (Base R style) ---
# Filter for individuals where born country and category are known
df_clean <- subset(df, !is.na(borncountry) & !is.na(category))

# (Removed cleaning of country names and derivation of is_migrant as per instructions)
# We will analyze the existing 'borncountry' and 'category' columns directly.

# --- Analysis: Comparison of Proportions ---

# Create a table of counts: Born Country vs Category
# Since there are many countries, we focus on the top 10 countries by number of laureates for the visualization
top_countries <- names(sort(table(df_clean$borncountry), decreasing = TRUE))[1:10]
df_subset <- subset(df_clean, borncountry %in% top_countries)

pt <- table(df_subset$borncountry, df_subset$category)

print("Contingency Table (Counts) for Top 10 Countries:")
print(pt)

# Chi-Square Test
# Research Question: Is there a difference in the proportions of Laureate winners bornplace between prize categories?
# H0 (Null Hypothesis): There is NO difference in the proportions of birth countries between prize categories (Variables are independent).
# H1 (Alternative Hypothesis): There IS a difference in the proportions of birth countries between prize categories (Variables are dependent).

chi_test <- chisq.test(pt)
print("Chi-Square Test Results:")
print(chi_test)

# Interpretation
alpha <- 0.05
if(chi_test$p.value < alpha) {
  print(paste("Result: Reject the Null Hypothesis (p-value =", format.pval(chi_test$p.value, digits=4), ")"))
  print("Conclusion: There is a statistically significant difference in the proportions of Laureate winners' birth countries between prize categories.")
} else {
  print(paste("Result: Fail to reject the Null Hypothesis (p-value =", format.pval(chi_test$p.value, digits=4), ")"))
  print("Conclusion: There is NO statistically significant difference in the proportions of Laureate winners' birth countries between prize categories.")
}

# --- Visualization (Base R barplot) ---

# Proportions Chart
# Calculate proportions of countries within each category (column proportions)
percentages <- prop.table(pt, margin = 2) * 100

print("Percentages Table (Column Proportions):")
print(percentages)

png(file.path(output_dir, "born_country_proportion_category.png"), width = 1000, height = 600)
barplot(percentages, 
        col = rainbow(nrow(percentages)), 
        xlab = "Prize Category", 
        ylab = "Percentage", 
        main = "Proportion of Nobel Laureates by Born Country (Top 10) and Category",
        ylim = c(0, 100),
        legend.text = rownames(percentages), 
        args.legend = list(x = "topright", bty = "n", cex = 0.8))
dev.off()

# Display in RStudio
barplot(percentages, 
        col = rainbow(nrow(percentages)), 
        xlab = "Prize Category", 
        ylab = "Percentage", 
        main = "Proportion of Nobel Laureates by Born Country (Top 10) and Category",
        ylim = c(0, 100),
        legend.text = rownames(percentages), 
        args.legend = list(x = "topright", bty = "n", cex = 0.8))

print("Analysis complete. Plots saved using base R.")

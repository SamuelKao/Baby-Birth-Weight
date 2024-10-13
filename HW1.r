# Load the dataset and display the first few rows
df <- read.table(file = "babies.txt", header = TRUE)
head(df)

# Display the structure of the data (variable types)
type <- str(df)

# Summarize the dataset (provides statistics like mean, median, min, max for numerical variables)
summary(df)

# Manually describe each variable's distribution
bwt_description <- "Numerical, Min:55.0, Median 120.0, Max: 176.0, Mean: 119.6"
gestation_description <- "Numerical, Min: 148.0, Median: 280.0, Max: 999.0, Mean: 286.9"
parity_description <- "Categorical (ordinal), Min: 0, Median:0, Max: 1.0000, Mean: 0.2549"
age_description <- "Numerical, Min: 15.00, Median: 26.00, Max: 99.00, Mean: 27.37"
height_description <- "Numerical, Min: 53.00, Median: 64.00, Max: 99.00, Mean: 64.67"
weight_description <- "Numerical, Min: 87, Median: 126, Max:999, Mean: 154"
smoke_description <- "Categorical, Min: 0, Median: 0, Max: 9.0, Mean: 0.4644"

# Plot histograms for the variables to visualize their distributions
hist(df$bwt, main = "Histogram of bwt", xlab = "weight", freq = FALSE)
hist(df$gestation, main = "Histogram of gestation", xlab = "gestation", breaks = 15, freq = FALSE)
hist(df$parity, main = "Parity", freq = FALSE)
hist(df$age, main = "Age", freq = FALSE, xlab = "age")
hist(df$height, main = "Height", freq = FALSE, xlab = "Inches")
hist(df$weight, main = "Weight", freq = FALSE, xlab = "Pounds")
hist(df$smoke, main = "smoke", freq = FALSE, xlab = "0(no smoke) 1(smoke)", breaks = 20)

# Define a function to find outliers based on the Interquartile Range (IQR)
find_outliers_IQR <- function(column) {
    Q1 <- quantile(column, 0.25) # First quartile (25%)
    Q3 <- quantile(column, 0.75) # Third quartile (75%)
    IQR_value <- IQR(column) # Calculate the IQR

    # Calculate the lower and upper bounds for identifying outliers
    lower_bound <- Q1 - 1.5 * IQR_value
    upper_bound <- Q3 + 1.5 * IQR_value

    # Return the values that are outside of the bounds
    return(column[column < lower_bound | column > upper_bound])
}

# Apply the outlier detection function to all columns in the dataframe
outliers <- lapply(df, find_outliers_IQR)

# Remove rows with missing values (NA) from the dataset
df_clean <- na.omit(df)

# Data cleaning steps to remove outliers and invalid data
clean_age <- df[df$age < 50, ] # Remove records with age > 50
clean_smoke <- clean_age[clean_age$smoke == 0 | clean_age$smoke == 1, ] # Keep only valid smoking values
clean_weight <- clean_smoke[clean_smoke$weight < 300, ] # Remove mothers weighing more than 300 pounds
clean_height <- clean_weight[clean_weight$height < 90, ] # Remove mothers taller than 90 inches
df_clean <- clean_height[clean_height$gestation < 365, ] # Remove gestation periods greater than 365 days

# Display the cleaned summary
summary(df_clean)

# Aggregate summary statistics (min, max, mean, median, quartiles, standard deviation) for birth weight by smoking status
aggregate(bwt ~ smoke, df_clean, min)
aggregate(bwt ~ smoke, df_clean, max)

# Manually assign the min and max birth weights for smokers and non-smokers
smoker_min_bwt <- 58
smoker_max_bwt <- 163
nonsmoker_min_bwt <- 55
nonsmoker_max_bwt <- 176

# Calculate the mean birth weight for smokers and non-smokers
aggregate(bwt ~ smoke, df_clean, mean)
nonsmoker_mean_bwt <- 123.0853
smoker_mean_bwt <- 113.8192

# Calculate the median birth weight for smokers and non-smokers
aggregate(bwt ~ smoke, df_clean, median)
nonsmoker_median_bwt <- 123
smoker_median_bwt <- 115

# Calculate the first, second (median), and third quartiles for birth weight by smoking status
aggregate(bwt ~ smoke, df_clean, function(x) quantile(x, 0.25))
smoker_q1_bwt <- 101
nonsmoker_q1_bwt <- 113

aggregate(bwt ~ smoke, df_clean, function(x) quantile(x, 0.5))
smoker_q2_bwt <- 115
nonsmoker_q2_bwt <- 123

aggregate(bwt ~ smoke, df_clean, function(x) quantile(x, 0.75))
smoker_q3_bwt <- 126
nonsmoker_q3_bwt <- 134

# Calculate the standard deviation of birth weights for smokers and non-smokers
aggregate(bwt ~ smoke, df_clean, sd)
smoker_std_bwt <- 17.42370
nonsmoker_std_bwt <- 18.29501

# Boxplot to compare birth weights between smokers and non-smokers
boxplot(bwt ~ smoke,
    data = df_clean,
    main = "Birth Weight Distribution by Smoking Status",
    xlab = "Smoking Status (0 = Non-Smoker, 1 = Smoker)",
    ylab = "Birth Weight",
    col = c("lightblue", "lightpink"),
    border = "darkblue"
)

# Density plot to visualize the distribution of birth weights by smoking status
ggplot(df_clean, aes(x = bwt, fill = as.factor(smoke))) +
    geom_density(alpha = 0.5) +
    labs(
        title = "Density Plot of Birth Weight by Smoking Status",
        x = "Birth Weight",
        fill = "Smoking Status (0 = Non-Smoker, 1 = Smoker)"
    ) +
    scale_fill_manual(values = c("lightblue", "lightpink"))

# Calculate the percentage of babies with low birth weight (< 100 oz) for smokers and non-smokers
low_bwt_smoker <- sum(df_clean$bwt[df_clean$smoke == 1] < 100) / sum(df_clean$smoke == 1)
low_bwt_smoker <- 0.1835749

low_bwt_nonsmoker <- sum(df_clean$bwt[df_clean$smoke == 0] < 100) / sum(df_clean$smoke == 0)
low_bwt_nonsmoker <- 0.05320814
